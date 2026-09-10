# =============================================================================
# Trial replay — animated participant flow for the Overview tab
# =============================================================================
# Turns the raw REDCap export into a compact per-participant timeline that
# www/trial_replay.js animates on the Overview tab:
#
#   • Living CONSORT — each participant is a dot moving through the pathway
#     (randomised → operation → discharge → Day 30 → Day 90), with
#     change-of-status exits dropping into a "Discontinued" lane
#   • Site recruitment race, pace against the protocol target schedule and a
#     feed of recent events, all driven by one timeline you can play or drag
#
# Stages come from the trial config: the operation / discharge stages only
# appear when those fields are mapped, and follow-up milestones are read from
# `day_<n>` entries in redcap_events (Day 30 / Day 90 for TONIC), counted from
# the operation date (or randomisation when the trial has no operation). They
# show time in follow-up, not form returns.
#
# Change of status: codes whose label mentions "part" (TONIC code 3, part
# withdrawal) keep the participant in follow-up with a marker; every other
# code moves them into the Discontinued lane on cos_dt.
#
# Public functions:
#   trial_replay_payload(raw, sites, cfg, trial_target, today)  →  list | NULL
#   trial_replay_card(payload, id)                              →  tags$section
# =============================================================================

.TR_DAY <- 86400

# Parse REDCap date / datetime strings to POSIXct (UTC wall-clock). Date-only
# values are placed at midday so they sort after a same-day randomisation.
.tr_parse_time <- function(x) {
  x  <- trimws(as.character(x))
  ok <- !is.na(x) & nzchar(x) & x != "NA"
  out <- rep(as.POSIXct(NA, tz = "UTC"), length(x))
  if (!any(ok)) return(out)
  out[ok] <- suppressWarnings(lubridate::parse_date_time(
    x[ok], orders = c("Ymd HMS", "Ymd HM", "Ymd", "dmY HMS", "dmY HM", "dmY"),
    tz = "UTC", quiet = TRUE))
  date_only <- ok & !grepl(":", x, fixed = TRUE)
  out[date_only] <- out[date_only] + 12 * 3600
  out
}

# Earliest parsed time (epoch seconds) per participant for one column — the
# value can sit on any event row of a longitudinal export.
.tr_first_time <- function(raw, ids, col) {
  if (is.null(col) || !nzchar(col) || !col %in% names(raw))
    return(rep(NA_real_, length(ids)))
  v  <- as.numeric(.tr_parse_time(raw[[col]]))
  ok <- !is.na(v)
  if (!any(ok)) return(rep(NA_real_, length(ids)))
  first <- tapply(v[ok], raw$record_id[ok], min)
  unname(as.numeric(first[ids]))
}

.tr_field <- function(cfg, name) {
  v <- cfg$redcap_fields[[name]]
  if (is.character(v) && length(v) == 1 && nzchar(v)) v else NULL
}

.tr_exit_colour <- function(label) {
  l <- tolower(label)
  if (grepl("death|died|deceased", l)) return("#4A4A4A")
  if (grepl("no op|operation", l))      return("#F07F3C")
  if (grepl("lost", l))                 return("#2581C4")
  if (grepl("withdr", l))               return("#E30513")
  "#8A8A8C"
}

# Pathway stages for this trial. `now` describes a participant currently in the
# stage (shown under each box); `status` is the tooltip wording.
.tr_stages <- function(has_op, has_dis, fu_days) {
  st <- list(list(key = "rand", label = "Randomised", short = "Randomisation"))
  if (has_op)  st[[length(st) + 1]] <- list(key = "op",  label = "Received surgery", short = "Surgery")
  if (has_dis) st[[length(st) + 1]] <- list(key = "dis", label = "Discharged",       short = "Discharge")
  for (d in fu_days)
    st[[length(st) + 1]] <- list(key = paste0("d", d), label = paste("Reached Day", d),
                                 short = paste("Day", d))
  n <- length(st)
  for (k in seq_len(n)) {
    nxt <- if (k < n) st[[k + 1]] else NULL
    st[[k]]$now <- switch(st[[k]]$key,
      rand = if (has_op) "awaiting surgery"
             else if (!is.null(nxt)) paste("before", nxt$short) else "randomised",
      op   = if (has_dis) "in hospital"
             else if (!is.null(nxt)) paste("before", nxt$short) else "after surgery",
      dis  = if (!is.null(nxt)) paste("before", nxt$short) else "discharged",
      if (!is.null(nxt)) sprintf("in %s–%s window", st[[k]]$short, sub("^Day ", "", nxt$short))
      else "follow-up complete")
    st[[k]]$status <- switch(st[[k]]$key,
      rand = if (has_op) "Awaiting surgery" else "Randomised",
      op   = "In hospital after surgery",
      dis  = if (!is.null(nxt)) paste0("Discharged, before ", nxt$short) else "Discharged",
      if (!is.null(nxt)) sprintf("Between %s and %s", st[[k]]$short, nxt$short)
      else paste("Reached", st[[k]]$short))
  }
  st[[n]]$done <- paste("reached", st[[n]]$short)
  st
}

# =============================================================================
# 1. Payload (JSON-ready list) from the processed REDCap export
# =============================================================================
# raw          rv$raw_redcap (or the WP-scoped redcap_wp()) — one row per event
# sites        rv$sites — supplies open dates and monthly targets when set
# trial_target effective recruitment target for the current view (WP-aware)
trial_replay_payload <- function(raw, sites = NULL, cfg = current_trial_config(),
                                 trial_target = NULL, today = Sys.Date()) {
  if (is.null(cfg) || is.null(raw) || !is.data.frame(raw) || !nrow(raw) ||
      !"record_id" %in% names(raw)) return(NULL)

  raw$record_id <- trimws(as.character(raw$record_id))
  raw <- raw[!is.na(raw$record_id) & nzchar(raw$record_id) & raw$record_id != "NA", ,
             drop = FALSE]
  ids <- sort(unique(raw$record_id))

  # ── Randomisation — participants without a date can't be placed in time ──
  rand   <- .tr_first_time(raw, ids, .tr_field(cfg, "randomisation_datetime") %||% "rand_dttm_s")
  placed <- !is.na(rand)
  if (!any(placed)) return(NULL)
  n_undated <- sum(!placed)
  ids  <- ids[placed]
  rand <- rand[placed]
  n    <- length(ids)

  # ── Site (DAG) ─────────────────────────────────────────────────────────────
  site_col <- if ("site_dag" %in% names(raw)) "site_dag" else .tr_field(cfg, "site_name")
  site <- rep("Unknown site", n)
  if (!is.null(site_col) && site_col %in% names(raw)) {
    s  <- trimws(as.character(raw[[site_col]]))
    ok <- !is.na(s) & nzchar(s) & s != "NA"
    if (any(ok)) {
      hit <- unname(tapply(s[ok], raw$record_id[ok], function(z) z[1])[ids])
      site[!is.na(hit)] <- hit[!is.na(hit)]
    }
  }

  # ── Operation / discharge ──────────────────────────────────────────────────
  op_dttm_col <- .tr_field(cfg, "operation_datetime")
  op_dt_col   <- .tr_field(cfg, "operation_date")
  has_op <- any(c(op_dttm_col, op_dt_col) %in% names(raw))
  op <- .tr_first_time(raw, ids, op_dttm_col)
  miss <- is.na(op)
  if (any(miss)) op[miss] <- .tr_first_time(raw, ids, op_dt_col)[miss]
  op <- pmax(op, rand)

  dis_col <- .tr_field(cfg, "discharge_date")
  has_dis <- !is.null(dis_col) && dis_col %in% names(raw)
  dis <- pmax(.tr_first_time(raw, ids, dis_col), rand)
  has <- !is.na(op) & !is.na(dis)
  dis[has] <- pmax(dis[has], op[has])

  # ── Follow-up milestones (day_30 / day_90 events → 30, 90) ────────────────
  evt_names <- names(cfg$redcap_events %||% list())
  fu_days <- suppressWarnings(as.integer(sub("^day_?", "",
                                             grep("^day_?\\d+$", evt_names, value = TRUE))))
  fu_days <- sort(unique(fu_days[!is.na(fu_days) & fu_days > 0]))
  anchor  <- if (has_op) op else rand

  stages <- .tr_stages(has_op, has_dis, fu_days)
  cols <- list(rand)
  if (has_op)  cols[[length(cols) + 1]] <- op
  if (has_dis) cols[[length(cols) + 1]] <- dis
  for (d in fu_days) cols[[length(cols) + 1]] <- anchor + d * .TR_DAY
  M <- do.call(cbind, cols)

  today_s <- as.numeric(as.POSIXct(format(as.Date(today)), tz = "UTC")) + .TR_DAY - 60

  # ── Change of status ───────────────────────────────────────────────────────
  labs    <- cfg$cos_type_labels
  cos_col <- .tr_field(cfg, "cos_type") %||% "cos_type"
  exits   <- data.frame(code = character(), label = character(), col = character(),
                        stringsAsFactors = FALSE)
  x_idx <- rep(NA_integer_, n); x_at <- rep(NA_real_, n); part_at <- rep(NA_real_, n)
  n_cos_undated <- 0L
  if (cos_col %in% names(raw)) {
    part_codes <- if (length(labs)) names(labs)[grepl("part", labs, ignore.case = TRUE)] else character()
    cv <- trimws(as.character(raw[[cos_col]]))
    ok <- !is.na(cv) & nzchar(cv) & !cv %in% c("NA", "0") & raw$record_id %in% ids
    # Every labelled exit gets a lane (zero counts included); unlabelled codes
    # found in the data are added so nothing silently disappears.
    exit_codes <- union(setdiff(names(labs), part_codes), setdiff(sort(unique(cv[ok])), part_codes))
    exits <- data.frame(
      code  = exit_codes,
      label = vapply(exit_codes, function(cd)
                if (cd %in% names(labs)) as.character(labs[[cd]]) else paste("Status", cd),
                character(1), USE.NAMES = FALSE),
      stringsAsFactors = FALSE)
    exits$col <- vapply(exits$label, .tr_exit_colour, character(1), USE.NAMES = FALSE)

    if (any(ok)) {
      date_col <- .tr_field(cfg, "cos_date")
      at <- if (!is.null(date_col) && date_col %in% names(raw))
              as.numeric(.tr_parse_time(raw[[date_col]]))[ok] else rep(NA_real_, sum(ok))
      ev <- data.frame(i = match(raw$record_id[ok], ids), code = cv[ok], at = at,
                       stringsAsFactors = FALSE)
      # Undated changes of status: place them just after the participant's last
      # recorded event so they still leave the pathway.
      undated <- is.na(ev$at)
      if (any(undated)) {
        last_known <- pmin(pmax(rand, op, dis, na.rm = TRUE), today_s)
        n_cos_undated <- length(unique(ev$i[undated]))
        ev$at[undated] <- last_known[ev$i[undated]] + 3600
      }
      ev$at <- pmax(ev$at, rand[ev$i])
      ev <- ev[order(ev$at), , drop = FALSE]

      is_part <- ev$code %in% part_codes
      px <- ev[is_part, , drop = FALSE];  px <- px[!duplicated(px$i), , drop = FALSE]
      part_at[px$i] <- px$at
      xx <- ev[!is_part, , drop = FALSE]; xx <- xx[!duplicated(xx$i), , drop = FALSE]
      x_idx[xx$i] <- match(xx$code, exits$code) - 1L
      x_at[xx$i]  <- xx$at
    }
  }

  # ── Sites: open date from the Sites tab, else first randomisation ─────────
  first_rand <- tapply(rand, site, min)
  sd <- data.frame(name = unique(site), stringsAsFactors = FALSE)
  sd$open <- as.numeric(first_rand[sd$name])
  sd$monthly_target <- 2
  if (!is.null(sites) && is.data.frame(sites) && nrow(sites) && "site_name" %in% names(sites)) {
    od <- if ("site_open_date" %in% names(sites))
            suppressWarnings(as.Date(sites$site_open_date)) else as.Date(rep(NA, nrow(sites)))
    od_s <- as.numeric(as.POSIXct(format(od), tz = "UTC"))
    mt <- if ("monthly_target" %in% names(sites))
            suppressWarnings(as.numeric(sites$monthly_target)) else rep(NA_real_, nrow(sites))
    m <- match(sd$name, sites$site_name)
    hit <- !is.na(m)
    sd$open[hit] <- pmin(sd$open[hit], od_s[m[hit]], na.rm = TRUE)
    good_mt <- hit & !is.na(mt[m]) & mt[m] > 0
    sd$monthly_target[good_mt] <- mt[m[good_mt]]
    # Sites with an open date but no participants yet (0 so far, or opening soon)
    extra <- which(!sites$site_name %in% sd$name & !is.na(od_s) &
                   !is.na(sites$site_name) & nzchar(sites$site_name))
    if (length(extra))
      sd <- rbind(sd, data.frame(
        name = sites$site_name[extra], open = od_s[extra],
        monthly_target = ifelse(!is.na(mt[extra]) & mt[extra] > 0, mt[extra], 2),
        stringsAsFactors = FALSE))
  }

  # ── Protocol target schedule (scaled when a work package is active) ───────
  ts <- th_target_schedule(cfg)
  has_ts <- !is.null(ts)
  ts_s <- if (has_ts) as.numeric(as.POSIXct(format(as.Date(ts$month_date)), tz = "UTC")) else numeric()
  trial_target <- suppressWarnings(as.numeric(trial_target %||% cfg$trial_target %||% 0))
  base_target  <- suppressWarnings(as.numeric(cfg$trial_target %||% 0))
  scale <- if (has_ts && isTRUE(trial_target > 0) && isTRUE(base_target > 0) &&
               trial_target != base_target) trial_target / base_target else 1

  # ── Time origin: first of the month of the earliest event ─────────────────
  start_s <- min(c(rand, sd$open, ts_s), na.rm = TRUE)
  start_d <- lubridate::floor_date(as.Date(as.POSIXct(start_s, origin = "1970-01-01", tz = "UTC")),
                                   "month")
  origin  <- as.numeric(as.POSIXct(format(start_d), tz = "UTC"))
  off     <- function(x) round((x - origin) / .TR_DAY, 4)

  target <- if (has_ts) list(t = I(off(ts_s)),
                             v = I(round(as.numeric(ts$cumulative_target) * scale, 1))) else NULL

  Moff <- off(M)
  p <- data.frame(id = ids, s = match(site, sd$name) - 1L, stringsAsFactors = FALSE)
  p$m  <- lapply(seq_len(n), function(i) I(unname(Moff[i, ])))
  p$x  <- x_idx
  p$xa <- off(x_at)
  p$pa <- off(part_at)
  sd$open <- off(sd$open)

  # ── Footnote ───────────────────────────────────────────────────────────────
  notes <- character()
  if (length(fu_days))
    notes <- c(notes, sprintf(
      "%s are counted from the %s, so they show time in follow-up rather than form returns.",
      paste(paste("Day", fu_days), collapse = " and "),
      if (has_op) "operation date" else "randomisation date"))
  if (n_undated)
    notes <- c(notes, sprintf("%d participant%s without a randomisation date %s not shown.",
                              n_undated, if (n_undated == 1) "" else "s",
                              if (n_undated == 1) "is" else "are"))
  if (n_cos_undated)
    notes <- c(notes, sprintf(
      "%d change-of-status record%s had no date and %s placed just after the participant's last recorded event.",
      n_cos_undated, if (n_cos_undated == 1) "" else "s", if (n_cos_undated == 1) "is" else "are"))
  if (scale != 1) notes <- c(notes, "Target schedule scaled to this work package's target.")

  list(
    start         = format(start_d),
    end           = off(today_s),
    stages        = stages,
    exits         = exits,
    sites         = sd,
    p             = p,
    target        = target,
    trial_target  = trial_target,
    planned_sites = cfg$projection_defaults$target_sites,
    pilot         = cfg$pilot,
    note          = paste(notes, collapse = " ")
  )
}

# =============================================================================
# 2. Overview card — the JS mounts on .tr-root and reads the sibling JSON
# =============================================================================
trial_replay_card <- function(payload, id = "trial-replay") {
  head <- div(class = "pov-card-head",
    div(tags$h3("Trial replay"),
        span(class = "pov-card-sub",
             "Watch the trial unfold: participants move through the CONSORT pathway as recruitment, follow-up and withdrawals happen. Press play or drag the timeline.")))

  if (is.null(payload))
    return(tags$section(class = "pov-card tr-card", head,
      div(class = "tr-empty",
          "Load a REDCap export with randomisation dates to replay the trial.")))

  json <- jsonlite::toJSON(payload, auto_unbox = TRUE, dataframe = "rows",
                           na = "null", null = "null", digits = 4)
  # Record IDs / site names come from the export — never let them close the tag.
  json <- gsub("</", "<\\/", json, fixed = TRUE)

  tags$section(class = "pov-card tr-card", head,
    div(id = id, class = "tr-root"),
    tags$script(type = "application/json", id = paste0(id, "-data"), HTML(json)))
}
