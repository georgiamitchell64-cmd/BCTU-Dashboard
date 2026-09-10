# =============================================================================
# Trial health — site, participant, data and recruitment analytics
# =============================================================================
# One engine behind the health features on the Overview, Data, Sites and
# Randomisations tabs. Built from the processed REDCap export (rv$raw_redcap /
# redcap_wp()), the Sites table and the trial config:
#
#   th_settings(cfg)                         monitoring settings + defaults
#   th_target_schedule(cfg)                  protocol target schedule as a df
#   th_participants(raw, cfg, today)         one row per randomised participant
#   th_crf_schedule(cfg, raw)                the forms each participant needs
#   th_crf_status(raw, P, sched, cfg, today) participant x form: done / due / overdue
#   th_site_health(P, crf, sites, cfg, today) per-site scores, funnel z, RAG + reasons
#   th_participant_issues(P, crf, cfg, today) the "needs attention" worklist
#   th_rand_patterns(P, cfg, today)          weekday x hour, in / out of hours, gaps
#   th_projection(P, sites, cfg, today, target) trend-based trajectories + fan chart
#   th_build(raw, sites, cfg, today, trial_target)  all of the above in one list
#
# Everything is driven by the trial config so it works for any trial. Forms,
# timings, change-of-status codes, working hours and thresholds come from
# Settings (cfg$crf_schedule, cfg$monitoring) with sensible defaults. Times are
# epoch seconds on the wall clock (parsed as UTC, never converted), via the
# parsing helpers in functions/trial_replay.R.
# =============================================================================

.TH_DAY <- 86400

.TH_DEFAULTS <- list(
  working_hours = list(start = "08:00", end = "18:00",
                       days = c("Mon", "Tue", "Wed", "Thu", "Fri"),
                       bank_holidays = TRUE, extra_holidays = character()),
  crf = list(grace_days = 14,              # days after a visit before its CRF is overdue
             op_expected_days = 7,         # flag a missing operation date after this
             discharge_expected_days = 60),# flag a missing discharge date after this
  flags = list(cos_exclude = "death",      # change-of-status labels not held against a site
               z_warn = 1.96, z_alarm = 3.09,
               overdue_amber = 10, overdue_red = 25,   # % of expected CRFs overdue
               quiet_amber = 30, quiet_red = 60,       # days since last randomisation
               expected_attrition = 15,                # % change of status the trial allows for
               min_n = 5),                             # participants before rates are judged
  weights = list(recruitment = 30, retention = 30, data = 30, activity = 10),
  projection = list(window_weeks = 12, sims = 1000)
)

# England & Wales bank holidays (gov.uk), used to split out-of-hours recruitment.
# Extra dates (other nations, local holidays) come from Settings.
.TH_UK_BANK_HOLIDAYS <- as.Date(c(
  "2024-01-01", "2024-03-29", "2024-04-01", "2024-05-06", "2024-05-27", "2024-08-26", "2024-12-25", "2024-12-26",
  "2025-01-01", "2025-04-18", "2025-04-21", "2025-05-05", "2025-05-26", "2025-08-25", "2025-12-25", "2025-12-26",
  "2026-01-01", "2026-04-03", "2026-04-06", "2026-05-04", "2026-05-25", "2026-08-31", "2026-12-25", "2026-12-28",
  "2027-01-01", "2027-03-26", "2027-03-29", "2027-05-03", "2027-05-31", "2027-08-30", "2027-12-27", "2027-12-28",
  "2028-01-03", "2028-04-14", "2028-04-17", "2028-05-01", "2028-05-29", "2028-08-28", "2028-12-25", "2028-12-26",
  "2029-01-01", "2029-03-30", "2029-04-02", "2029-05-07", "2029-05-28", "2029-08-27", "2029-12-25", "2029-12-26"))

.TH_WDAYS <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

# ── Small helpers ──────────────────────────────────────────────────────────
.th_date_s  <- function(d) as.numeric(as.POSIXct(format(as.Date(d)), tz = "UTC"))
.th_today_s <- function(today) .th_date_s(today) + .TH_DAY - 1
.th_as_date <- function(x) as.Date(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
.th_pretty  <- function(role) tools::toTitleCase(gsub("_", " ", role))
.th_clamp   <- function(x, lo = 0, hi = 100) pmin(hi, pmax(lo, x))
# One value from JSON-ish input (NULL, list() and NA fall back to the default)
.th_num1    <- function(x, d) { x <- suppressWarnings(as.numeric(unlist(x))); if (length(x) == 1 && !is.na(x)) x else d }
.th_chr1    <- function(x, d = "") { x <- as.character(unlist(x)); if (length(x) && !is.na(x[1])) x[1] else d }
.th_hm      <- function(s) {
  p <- suppressWarnings(as.integer(strsplit(as.character(s %||% "0:0"), ":", fixed = TRUE)[[1]]))
  p[is.na(p)] <- 0L
  p[1] * 60 + (if (length(p) > 1) p[2] else 0)
}
.th_has_op  <- function(cfg) !is.null(.tr_field(cfg, "operation_date")) ||
                             !is.null(.tr_field(cfg, "operation_datetime"))
.th_has_dis <- function(cfg) !is.null(.tr_field(cfg, "discharge_date"))

# Monitoring settings: defaults overlaid with cfg$monitoring (one level deep).
th_settings <- function(cfg) {
  m   <- cfg$monitoring %||% list()
  out <- .TH_DEFAULTS
  for (k in names(out)) if (is.list(m[[k]]))
    for (kk in names(m[[k]])) out[[k]][[kk]] <- m[[k]][[kk]]
  out$working_hours$days <- as.character(unlist(out$working_hours$days))
  out$working_hours$extra_holidays <- as.character(unlist(out$working_hours$extra_holidays))
  out
}

# Protocol target schedule as a data frame (month_date, cumulative_target).
# config.R stores a data frame; overrides.json stores the columns as lists.
th_target_schedule <- function(cfg) {
  ts <- cfg$target_schedule
  if (is.null(ts) || !length(ts)) return(NULL)
  if (!is.data.frame(ts)) {
    if (!all(c("month_date", "cumulative_target") %in% names(ts))) return(NULL)
    ts <- data.frame(month_date = unlist(ts$month_date),
                     cumulative_target = unlist(ts$cumulative_target),
                     stringsAsFactors = FALSE)
  }
  ts$month_date <- suppressWarnings(as.Date(ts$month_date))
  ts$cumulative_target <- suppressWarnings(as.numeric(ts$cumulative_target))
  ts <- ts[!is.na(ts$month_date) & !is.na(ts$cumulative_target), , drop = FALSE]
  if (!nrow(ts)) return(NULL)
  ts[order(ts$month_date), , drop = FALSE]
}

.th_target_at <- function(ts, date) {
  if (is.null(ts)) return(NA_real_)
  x <- as.numeric(ts$month_date); y <- ts$cumulative_target; d <- as.numeric(as.Date(date))
  if (d <= x[1]) return(y[1])
  if (d >= x[length(x)]) return(y[length(y)])
  stats::approx(x, y, xout = d)$y
}

# ── Change-of-status labels ────────────────────────────────────────────────
.th_cos_label <- function(code, labs) {
  vapply(code, function(cd)
    if (!is.na(cd) && cd %in% names(labs)) as.character(unlist(labs[[cd]]))[1]
    else paste("Status", cd), character(1), USE.NAMES = FALSE)
}
.th_part_codes <- function(cfg) {
  labs <- cfg$cos_type_labels
  if (!length(labs)) return(character())
  names(labs)[grepl("part", unlist(labs), ignore.case = TRUE)]
}

# =============================================================================
# 1. Participants — one row per randomised participant
# =============================================================================
th_participants <- function(raw, cfg, today = Sys.Date()) {
  empty <- data.frame(id = character(), site = character(), rand = numeric(),
                      has_time = logical(), op = numeric(), dis = numeric(),
                      exit_code = character(), exit_label = character(),
                      exit_at = numeric(), part_at = numeric(),
                      cos_undated = logical(), stringsAsFactors = FALSE)
  if (is.null(cfg) || is.null(raw) || !is.data.frame(raw) || !nrow(raw) ||
      !"record_id" %in% names(raw)) return(empty)

  raw$record_id <- trimws(as.character(raw$record_id))
  raw <- raw[!is.na(raw$record_id) & nzchar(raw$record_id) & raw$record_id != "NA", ,
             drop = FALSE]
  ids <- sort(unique(raw$record_id))

  rand_col <- .tr_field(cfg, "randomisation_datetime") %||% "rand_dttm_s"
  rand <- .tr_first_time(raw, ids, rand_col)
  keep <- !is.na(rand)
  if (!any(keep)) return(empty)
  ids <- ids[keep]; rand <- rand[keep]; n <- length(ids)

  # Time of day is only meaningful when the export carries one
  timed <- if (rand_col %in% names(raw))
    unique(raw$record_id[grepl(":", as.character(raw[[rand_col]]), fixed = TRUE)]) else character()

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

  op <- .tr_first_time(raw, ids, .tr_field(cfg, "operation_datetime"))
  miss <- is.na(op)
  if (any(miss)) op[miss] <- .tr_first_time(raw, ids, .tr_field(cfg, "operation_date"))[miss]
  op  <- pmax(op, rand)
  dis <- pmax(.tr_first_time(raw, ids, .tr_field(cfg, "discharge_date")), rand)
  has <- !is.na(op) & !is.na(dis)
  dis[has] <- pmax(dis[has], op[has])

  P <- data.frame(id = ids, site = site, rand = rand, has_time = ids %in% timed,
                  op = op, dis = dis, exit_code = NA_character_,
                  exit_label = NA_character_, exit_at = NA_real_, part_at = NA_real_,
                  cos_undated = FALSE, stringsAsFactors = FALSE)
  .th_add_status_changes(P, raw, cfg, today)
}

# Change of status: the earliest non-"part" code is the exit; the earliest
# "part" code is recorded separately (participant stays in follow-up).
# Undated records are placed just after the participant's last known event.
.th_add_status_changes <- function(P, raw, cfg, today) {
  cos_col <- .tr_field(cfg, "cos_type") %||% "cos_type"
  if (!nrow(P) || !cos_col %in% names(raw)) return(P)
  cv <- trimws(as.character(raw[[cos_col]]))
  ok <- !is.na(cv) & nzchar(cv) & !cv %in% c("NA", "0") & raw$record_id %in% P$id
  if (!any(ok)) return(P)

  date_col <- .tr_field(cfg, "cos_date")
  at <- if (!is.null(date_col) && date_col %in% names(raw))
          as.numeric(.tr_parse_time(raw[[date_col]]))[ok] else rep(NA_real_, sum(ok))
  ev <- data.frame(i = match(raw$record_id[ok], P$id), code = cv[ok], at = at,
                   stringsAsFactors = FALSE)
  und <- is.na(ev$at)
  if (any(und)) {
    last_known <- pmin(pmax(P$rand, P$op, P$dis, na.rm = TRUE), .th_today_s(today))
    ev$at[und] <- last_known[ev$i[und]] + 3600
    P$cos_undated[unique(ev$i[und])] <- TRUE
  }
  ev$at <- pmax(ev$at, P$rand[ev$i])
  ev <- ev[order(ev$at), , drop = FALSE]

  is_part <- ev$code %in% .th_part_codes(cfg)
  px <- ev[is_part, , drop = FALSE];  px <- px[!duplicated(px$i), , drop = FALSE]
  P$part_at[px$i] <- px$at
  xx <- ev[!is_part, , drop = FALSE]; xx <- xx[!duplicated(xx$i), , drop = FALSE]
  P$exit_code[xx$i]  <- xx$code
  P$exit_label[xx$i] <- .th_cos_label(xx$code, cfg$cos_type_labels)
  P$exit_at[xx$i]    <- xx$at
  P
}

# Where each participant is on `today` (worklist / drill-down wording).
.th_stage <- function(P, cfg, today) {
  if (!nrow(P)) return(character())
  today_s <- .th_today_s(today)
  anchor  <- if (.th_has_op(cfg)) P$op else P$rand
  fu <- .th_fu_days(cfg)
  out <- rep("Randomised", nrow(P))
  if (.th_has_op(cfg)) out[!is.na(P$op) & P$op <= today_s] <- "Had surgery"
  if (.th_has_dis(cfg)) out[!is.na(P$dis) & P$dis <= today_s] <- "Discharged"
  for (d in fu) out[!is.na(anchor) & anchor + d * .TH_DAY <= today_s] <- paste("Past Day", d)
  gone <- !is.na(P$exit_at) & P$exit_at <= today_s
  out[gone] <- P$exit_label[gone]
  out
}

.th_fu_days <- function(cfg) {
  ev <- names(cfg$redcap_events %||% list())
  d  <- suppressWarnings(as.integer(sub("^day_?", "", grep("^day_?[0-9]+$", ev, value = TRUE))))
  sort(unique(d[!is.na(d) & d > 0]))
}

# =============================================================================
# 2. CRF schedule — which forms each participant should have, and when
# =============================================================================
# Each row: timepoint, form, field (a REDCap *_complete variable), event (the
# redcap_event_name it lives on), anchor ("rand" | "op" | "discharge"), offset
# (days after the anchor), grace (days before overdue; NA = default) and kind
# ("CRF" = site-completed, "PROM" = participant questionnaire).
.th_empty_schedule <- function() {
  data.frame(timepoint = character(), form = character(), field = character(),
             event = character(), anchor = character(), offset = numeric(),
             grace = numeric(), kind = character(), present = logical(),
             fid = character(), stringsAsFactors = FALSE)
}

.th_timing <- function(role, has_op, has_dis) {
  if (identical(role, "baseline")) return(list(anchor = "rand", offset = 0))
  if (identical(role, "discharge"))
    return(list(anchor = if (has_dis) "discharge" else if (has_op) "op" else "rand", offset = 0))
  m <- regmatches(role, regexec("^(day|week|month|year)s?_?([0-9]+)$", role))[[1]]
  if (length(m) == 3) {
    mult <- c(day = 1, week = 7, month = 30.44, year = 365.25)[[m[2]]]
    return(list(anchor = if (has_op) "op" else "rand", offset = round(as.numeric(m[3]) * mult)))
  }
  NULL
}

# Default schedule derived from the config: the randomisation / consent forms,
# one completion form per follow-up event, and the participant questionnaires
# listed in participant_table_layout.
.th_default_schedule <- function(cfg) {
  ev <- cfg$redcap_events %||% list()
  has_op <- .th_has_op(cfg); has_dis <- .th_has_dis(cfg)
  rows <- list()
  add <- function(role, form, field, kind) {
    tm <- .th_timing(role, has_op, has_dis)
    if (is.null(tm) || is.null(field) || !nzchar(field)) return(invisible())
    rows[[length(rows) + 1]] <<- data.frame(
      timepoint = .th_pretty(role), form = form, field = field,
      event = if (is.null(ev[[role]])) "" else as.character(unlist(ev[[role]]))[1],
      anchor = tm$anchor, offset = tm$offset, grace = NA_real_, kind = kind,
      stringsAsFactors = FALSE)
  }
  add("baseline", "Randomisation", .tr_field(cfg, "randomisation_complete"), "CRF")
  add("baseline", "Consent & eligibility", .tr_field(cfg, "consent_complete"), "CRF")
  known <- list(discharge = "discharge_complete", day_30 = "day30_complete", day_90 = "day90_complete")
  roles <- setdiff(names(ev), c("baseline", "sub_forms"))
  for (role in roles)
    add(role, paste(.th_pretty(role), "form"), .tr_field(cfg, known[[role]] %||% paste0(role, "_complete")), "CRF")
  for (tp in cfg$participant_table_layout$timepoints %||% list())
    for (ins in tp$instruments %||% list())
      add(tp$event %||% "", ins$label %||% ins$field, ins$field, "PROM")

  if (!length(rows)) return(NULL)
  s <- do.call(rbind, rows)
  s <- s[!duplicated(paste(s$field, s$event)), , drop = FALSE]
  tp_order <- match(s$timepoint, unique(c("Baseline", .th_pretty(roles), s$timepoint)))
  s[order(tp_order, s$kind != "CRF"), , drop = FALSE]
}

th_crf_schedule <- function(cfg, raw = NULL) {
  custom <- cfg$crf_schedule
  s <- if (length(custom)) {
    do.call(rbind, lapply(custom, function(r) data.frame(
      timepoint = .th_chr1(r$timepoint),
      form      = .th_chr1(r$form, .th_chr1(r$field)),
      field     = .th_chr1(r$field),
      event     = .th_chr1(r$event),
      anchor    = .th_chr1(r$anchor, "rand"),
      offset    = .th_num1(r$offset, 0),
      grace     = .th_num1(r$grace, NA_real_),
      kind      = .th_chr1(r$kind, "CRF"),
      stringsAsFactors = FALSE)))
  } else .th_default_schedule(cfg)
  if (is.null(s) || !nrow(s)) return(.th_empty_schedule())
  s <- s[nzchar(s$field), , drop = FALSE]
  s$offset[is.na(s$offset)] <- 0
  s$anchor[!s$anchor %in% c("rand", "op", "discharge")] <- "rand"
  s$present <- if (is.null(raw)) TRUE else s$field %in% names(raw)
  s$fid <- paste0("f", seq_len(nrow(s)))
  rownames(s) <- NULL
  s
}

# =============================================================================
# 3. CRF status — every participant x scheduled form
# =============================================================================
# status: complete | overdue (past visit + grace) | due (in the grace window) |
#         not_due (visit still ahead) | awaiting (anchor date not recorded yet) |
#         not_required (participant left the trial before the visit)
.th_empty_crf <- function() {
  data.frame(id = character(), site = character(), fid = character(),
             timepoint = character(), form = character(), kind = character(),
             event_at = numeric(), over_at = numeric(), estimated = logical(),
             status = character(), days_overdue = numeric(), stringsAsFactors = FALSE)
}

th_crf_status <- function(raw, P, sched, cfg, today = Sys.Date(), st = th_settings(cfg)) {
  sched <- sched[sched$present %in% TRUE, , drop = FALSE]
  if (!nrow(P) || !nrow(sched) || is.null(raw) || !nrow(raw)) return(.th_empty_crf())
  today_s <- .th_today_s(today)
  rid <- trimws(as.character(raw$record_id))
  evv <- if ("redcap_event_name" %in% names(raw)) trimws(as.character(raw$redcap_event_name)) else NULL
  grace_default <- suppressWarnings(as.numeric(st$crf$grace_days %||% 14))
  # A missing operation / discharge date mustn't hide a form from tracking (in
  # REDCap the date usually arrives with the form itself), so estimate it from
  # the expected timings and let the form fall due anyway.
  op_est  <- ifelse(is.na(P$op), P$rand + st$crf$op_expected_days * .TH_DAY, P$op)
  dis_est <- ifelse(is.na(P$dis), op_est + st$crf$discharge_expected_days * .TH_DAY, P$dis)

  out <- lapply(seq_len(nrow(sched)), function(k) {
    f <- sched[k, ]
    v <- trimws(as.character(raw[[f$field]])) %in% c("2", "Complete", "complete")
    if (nzchar(f$event) && !is.null(evv)) v <- v & evv == f$event
    done <- P$id %in% unique(rid[v])
    anchor   <- switch(f$anchor, op = P$op, discharge = P$dis, P$rand)
    est      <- switch(f$anchor, op = op_est, discharge = dis_est, P$rand)
    event_at <- ifelse(is.na(anchor), est, anchor) + f$offset * .TH_DAY
    grace    <- if (is.na(f$grace)) grace_default else f$grace
    over_at  <- event_at + grace * .TH_DAY
    left     <- !is.na(P$exit_at) & (is.na(event_at) | P$exit_at < event_at)
    status <- ifelse(done, "complete",
              ifelse(left, "not_required",
              ifelse(is.na(event_at), "awaiting",
              ifelse(today_s < event_at, "not_due",
              ifelse(today_s < over_at, "due", "overdue")))))
    data.frame(id = P$id, site = P$site, fid = f$fid, timepoint = f$timepoint,
               form = f$form, kind = f$kind, event_at = event_at, over_at = over_at,
               estimated = is.na(anchor), status = status,
               days_overdue = ifelse(status == "overdue", floor((today_s - over_at) / .TH_DAY), NA_real_),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# =============================================================================
# 4. Site health — scores, funnel-plot z and plain-English reasons
# =============================================================================
# Rates are compared with the trial-wide rate using funnel-plot control limits
# (binomial, z = 1.96 / 3.09 by default), so a small site with 1 of 3
# withdrawn isn't flagged the way a large site with 10 of 30 is.
.th_z <- function(x, n, p) {
  ifelse(n > 0 & p > 0 & p < 1, (x / n - p) / sqrt(p * (1 - p) / n), NA_real_)
}

.th_cos_flag <- function(P, st) {
  excl <- trimws(strsplit(as.character(st$flags$cos_exclude %||% ""), ",", fixed = TRUE)[[1]])
  excl <- excl[nzchar(excl)]
  counted <- !is.na(P$exit_code)
  if (length(excl)) counted <- counted &
    !grepl(paste(excl, collapse = "|"), P$exit_label, ignore.case = TRUE)
  counted | !is.na(P$part_at)
}

th_site_health <- function(P, crf, sites, cfg, today = Sys.Date(), st = th_settings(cfg)) {
  today_s <- .th_today_s(today)
  fl <- st$flags; w <- st$weights
  num <- function(x) suppressWarnings(as.numeric(x))

  s_name <- s_open <- s_mt <- s_status <- NULL
  if (!is.null(sites) && is.data.frame(sites) && nrow(sites) && "site_name" %in% names(sites)) {
    s_name   <- as.character(sites$site_name)
    s_open   <- if ("site_open_date" %in% names(sites))
                  .th_date_s(suppressWarnings(as.Date(sites$site_open_date))) else rep(NA_real_, nrow(sites))
    s_mt     <- if ("monthly_target" %in% names(sites)) num(sites$monthly_target) else rep(NA_real_, nrow(sites))
    s_status <- if ("status" %in% names(sites)) as.character(sites$status) else rep(NA_character_, nrow(sites))
  }
  names_all <- unique(c(P$site, s_name[!is.na(s_open) & s_open <= today_s]))
  names_all <- names_all[!is.na(names_all) & nzchar(names_all)]
  if (!length(names_all)) return(NULL)

  P$flag <- .th_cos_flag(P, st)
  ck <- crf[crf$kind == "CRF", , drop = FALSE]
  ck$expected <- !is.na(ck$over_at) & ck$over_at <= today_s & ck$status %in% c("complete", "overdue")

  rows <- lapply(names_all, function(s) {
    idx <- which(P$site == s)
    n   <- length(idx)
    m   <- if (is.null(s_name)) NA_integer_ else match(s, s_name)
    first <- if (n) min(P$rand[idx]) else NA_real_
    last  <- if (n) max(P$rand[idx]) else NA_real_
    open  <- suppressWarnings(min(c(if (!is.na(m)) s_open[m], first), na.rm = TRUE))
    if (!is.finite(open)) open <- NA_real_
    months <- if (is.na(open)) 0 else max(0, (today_s - open) / (30.44 * .TH_DAY))
    tgt    <- if (!is.na(m) && !is.na(s_mt[m]) && s_mt[m] > 0) s_mt[m] else 2
    cs <- ck[ck$site == s, , drop = FALSE]
    gaps <- if (n > 1) diff(sort(P$rand[idx])) / .TH_DAY else numeric()
    data.frame(
      site = s, status = if (!is.na(m)) s_status[m] else NA_character_,
      n = n, open = open, first = first, last = last, months = months,
      rate = if (months >= 0.5) n / months else NA_real_, target_rate = tgt,
      cos_n = sum(P$flag[idx]), exits_n = sum(!is.na(P$exit_code[idx])),
      deaths_n = sum(grepl("death|died", P$exit_label[idx], ignore.case = TRUE)),
      expected = sum(cs$expected), done = sum(cs$expected & cs$status == "complete"),
      overdue = sum(cs$status == "overdue"), due_soon = sum(cs$status == "due"),
      max_gap = if (length(gaps)) max(gaps) else NA_real_,
      median_gap = if (length(gaps)) stats::median(gaps) else NA_real_,
      latency = if (n && !is.na(open)) (first - open) / .TH_DAY else NA_real_,
      stringsAsFactors = FALSE)
  })
  S <- do.call(rbind, rows)

  p_cos  <- if (sum(S$n)) sum(S$cos_n) / sum(S$n) else NA_real_
  p_over <- if (sum(S$expected)) sum(S$overdue) / sum(S$expected) else NA_real_
  S$cos_rate    <- ifelse(S$n > 0, S$cos_n / S$n, NA_real_)
  S$z_cos       <- .th_z(S$cos_n, S$n, p_cos)
  S$overdue_pct <- ifelse(S$expected > 0, 100 * S$overdue / S$expected, NA_real_)
  S$z_over      <- .th_z(S$overdue, S$expected, p_over)
  S$complete_pct <- ifelse(S$expected > 0, 100 * S$done / S$expected, NA_real_)
  S$days_quiet  <- ifelse(is.na(S$last), ifelse(is.na(S$open), NA_real_, (today_s - S$open) / .TH_DAY),
                          (today_s - S$last) / .TH_DAY)
  inactive <- S$status %in% c("Closed", "Paused")
  judged   <- S$n >= fl$min_n

  # Component scores, 0-100 (NA = not enough to judge)
  S$sc_recruit  <- ifelse(S$months >= 1 & !is.na(S$rate), .th_clamp(100 * S$rate / S$target_rate), NA_real_)
  S$sc_retain   <- ifelse(judged & !is.na(S$z_cos), .th_clamp(100 - 25 * pmax(0, S$z_cos)), NA_real_)
  S$sc_data     <- ifelse(S$expected > 0, S$complete_pct, NA_real_)
  qa <- fl$quiet_amber; qr <- fl$quiet_red
  S$sc_activity <- ifelse(inactive | S$months < 1, NA_real_,
                          .th_clamp(100 * (1 - (S$days_quiet - qa) / (2 * qr - qa))))
  comp <- cbind(S$sc_recruit, S$sc_retain, S$sc_data, S$sc_activity)
  wts  <- c(w$recruitment, w$retention, w$data, w$activity)
  S$score <- apply(comp, 1, function(r) {
    ok <- !is.na(r) & wts > 0
    if (!any(ok)) NA_real_ else round(sum(r[ok] * wts[ok]) / sum(wts[ok]))
  })

  pct <- function(x) sprintf("%.0f%%", 100 * x)
  S$rag <- "green"; S$reasons <- vector("list", nrow(S))
  for (i in seq_len(nrow(S))) {
    r <- S[i, ]; why <- character(); lvl <- 0
    bump <- function(l, txt) { why <<- c(why, txt); lvl <<- max(lvl, l) }
    if (judged[i] && !is.na(r$z_cos) && r$z_cos >= fl$z_warn)
      bump(if (r$z_cos >= fl$z_alarm) 2 else 1,
           sprintf("%d of %d participants had a change of status (%s vs %s trial-wide)",
                   r$cos_n, r$n, pct(r$cos_rate), pct(p_cos)))
    if (!is.na(r$overdue_pct) && r$overdue_pct >= fl$overdue_amber)
      bump(if (r$overdue_pct >= fl$overdue_red) 2 else 1,
           sprintf("%d CRF%s overdue (%.0f%% of those expected)", r$overdue,
                   if (r$overdue == 1) "" else "s", r$overdue_pct))
    if (!inactive[i] && r$months >= 1 && !is.na(r$days_quiet) && r$days_quiet >= qa)
      bump(if (r$days_quiet >= qr) 2 else 1,
           if (r$n) sprintf("No randomisations for %.0f days", r$days_quiet)
           else sprintf("Open %.0f days with no randomisations", r$days_quiet))
    if (!is.na(r$sc_recruit) && r$sc_recruit < 50 && r$months >= 2)
      bump(1, sprintf("Recruiting %.1f a month against a target of %s", r$rate,
                      format(r$target_rate, drop0trailing = TRUE)))
    if (!is.na(r$score) && r$score < 50) lvl <- max(lvl, 2)
    else if (!is.na(r$score) && r$score < 75) lvl <- max(lvl, 1)
    S$rag[i] <- c("green", "amber", "red")[lvl + 1]
    if (!judged[i] && lvl < 2 && r$months < 3) {
      S$rag[i] <- "new"
      why <- c(sprintf("Early days: %d participant%s so far", r$n, if (r$n == 1) "" else "s"), why)
    }
    S$reasons[[i]] <- why
  }
  S <- S[order(match(S$rag, c("red", "amber", "new", "green")), S$score), , drop = FALSE]
  rownames(S) <- NULL
  attr(S, "p_cos")  <- p_cos       # trial-wide rates for the funnel plot
  attr(S, "p_over") <- p_over
  S
}

# =============================================================================
# 5. Participants needing attention
# =============================================================================
th_participant_issues <- function(P, crf, cfg, today = Sys.Date(), st = th_settings(cfg)) {
  if (!nrow(P)) return(NULL)
  today_s <- .th_today_s(today)
  alive <- is.na(P$exit_at) | P$exit_at > today_s
  od <- crf[crf$status == "overdue", , drop = FALSE]
  od <- od[order(-od$days_overdue), , drop = FALSE]

  n_crf  <- as.integer(table(factor(od$id[od$kind == "CRF"], levels = P$id)))
  n_prom <- as.integer(table(factor(od$id[od$kind != "CRF"], levels = P$id)))
  max_d  <- tapply(od$days_overdue, factor(od$id, levels = P$id), max)
  max_d  <- ifelse(is.na(max_d), 0, max_d)
  odc    <- od[od$kind == "CRF", , drop = FALSE]
  max_c  <- tapply(odc$days_overdue, factor(odc$id, levels = P$id), max)
  max_c  <- ifelse(is.na(max_c), 0, max_c)
  forms  <- tapply(sprintf("%s %s (%dd)", od$timepoint, od$form, as.integer(od$days_overdue)),
                   factor(od$id, levels = P$id), function(z) paste(z, collapse = "; "))

  op_days  <- (today_s - P$rand) / .TH_DAY
  dis_days <- (today_s - P$op) / .TH_DAY
  miss_op  <- .th_has_op(cfg) & is.na(P$op) & alive & op_days > st$crf$op_expected_days
  miss_dis <- .th_has_dis(cfg) & !is.na(P$op) & is.na(P$dis) & alive &
              dis_days > st$crf$discharge_expected_days
  undated  <- P$cos_undated %in% TRUE

  # Site CRFs and missing key dates drive priority; overdue questionnaires add a little
  sev <- 3 * n_crf + pmin(2, 0.5 * n_prom) + pmin(3, max_c / 30) + 4 * miss_op + 3 * miss_dis + undated
  any_issue <- n_crf + n_prom > 0 | miss_op | miss_dis | undated
  if (!any(any_issue)) return(NULL)

  issue_txt <- vapply(seq_len(nrow(P)), function(i) {
    x <- c(if (n_crf[i])  sprintf("%d overdue CRF%s", n_crf[i], if (n_crf[i] == 1) "" else "s"),
           if (n_prom[i]) sprintf("%d overdue questionnaire%s", n_prom[i], if (n_prom[i] == 1) "" else "s"),
           if (miss_op[i])  sprintf("No operation date %d days after randomisation", as.integer(op_days[i])),
           if (miss_dis[i]) sprintf("No discharge date %d days after surgery", as.integer(dis_days[i])),
           if (undated[i])  "Change of status has no date")
    paste(x, collapse = " · ")
  }, character(1))

  out <- data.frame(
    id = P$id, site = P$site, rand_date = .th_as_date(P$rand),
    stage = .th_stage(P, cfg, today),
    n_overdue_crf = n_crf, n_overdue_prom = n_prom, max_days_overdue = as.integer(max_d),
    missing_op = miss_op, missing_dis = miss_dis, cos_undated = undated,
    issues = issue_txt, overdue_forms = ifelse(is.na(forms), "", forms),
    severity = round(sev, 1),
    priority = ifelse(sev >= 6, "High", ifelse(sev >= 3, "Medium", "Low")),
    stringsAsFactors = FALSE)[any_issue, , drop = FALSE]
  rownames(out) <- NULL
  out[order(-out$severity, out$id), , drop = FALSE]
}

# =============================================================================
# 6. Randomisation patterns — when does recruitment happen?
# =============================================================================
th_rand_patterns <- function(P, cfg, today = Sys.Date(), st = th_settings(cfg)) {
  if (!nrow(P)) return(NULL)
  wh <- st$working_hours
  t  <- as.POSIXct(P$rand, origin = "1970-01-01", tz = "UTC")
  d  <- as.Date(t)
  wd <- as.integer(format(t, "%u"))
  hr <- as.integer(format(t, "%H"))
  mn <- hr * 60L + as.integer(format(t, "%M"))
  work_days <- match(wh$days, .TH_WDAYS)
  hol <- c(if (isTRUE(wh$bank_holidays)) .TH_UK_BANK_HOLIDAYS,
           suppressWarnings(as.Date(wh$extra_holidays)))
  is_hol <- d %in% hol[!is.na(hol)]
  workday <- wd %in% work_days & !is_hol
  in_hrs  <- workday & mn >= .th_hm(wh$start) & mn < .th_hm(wh$end)
  cat <- ifelse(!P$has_time, "Time not recorded",
         ifelse(is_hol, "Bank holiday",
         ifelse(!wd %in% work_days, "Weekend",
         ifelse(in_hrs, "In hours", "Weekday out of hours"))))
  cats <- c("In hours", "Weekday out of hours", "Weekend", "Bank holiday", "Time not recorded")

  timed <- P$has_time
  heat  <- matrix(0L, 7, 24, dimnames = list(.TH_WDAYS, sprintf("%02d", 0:23)))
  if (any(timed)) {
    tb <- table(factor(wd[timed], 1:7), factor(hr[timed], 0:23))
    heat[] <- as.integer(tb)
  }

  # Monthly counts per site (for sparklines)
  mon <- format(d, "%Y-%m")
  months <- format(seq(as.Date(paste0(min(mon), "-01")), as.Date(format(as.Date(today), "%Y-%m-01")),
                       by = "month"), "%Y-%m")
  by_site <- lapply(split(seq_along(d), P$site), function(ix) {
    ooh <- cat[ix] %in% c("Weekday out of hours", "Weekend", "Bank holiday")
    tm  <- sum(timed[ix])
    list(n = length(ix),
         in_hours_pct = if (tm) 100 * sum(cat[ix] == "In hours") / tm else NA_real_,
         ooh_pct      = if (tm) 100 * sum(ooh) / tm else NA_real_,
         weekend_pct  = 100 * mean(cat[ix] %in% c("Weekend", "Bank holiday")),
         monthly      = as.integer(table(factor(mon[ix], months))))
  })

  today_d <- as.Date(today)
  recent  <- sum(d > today_d - 90)
  prior   <- sum(d > today_d - 180 & d <= today_d - 90)
  list(
    n = nrow(P), n_timed = sum(timed),
    category = setNames(as.integer(table(factor(cat, cats))), cats),
    heat = heat,
    by_wday = setNames(as.integer(table(factor(wd, 1:7))), .TH_WDAYS),
    by_hour = setNames(as.integer(table(factor(hr[timed], 0:23))), sprintf("%02d", 0:23)),
    months = months,
    monthly = as.integer(table(factor(mon, months))),
    monthly_by_cat = lapply(setNames(cats, cats), function(k)
      as.integer(table(factor(mon[cat == k], months)))),
    by_site = by_site,
    recent_90 = recent, prior_90 = prior,
    trend_pct = if (prior > 0) 100 * (recent - prior) / prior else NA_real_,
    working_hours = wh
  )
}

# =============================================================================
# 7. Projection — where will recruitment land on current trends?
# =============================================================================
# Three scenarios from the last `window_weeks` complete weeks:
#   pace  — carry on at the recent weekly rate
#   trend — the recent rate's log-linear trend (Poisson GLM), growth capped at ±2%/week
#   sites — each open site at its own (shrunk) recent rate, plus the sites still to open
# plus a Monte Carlo fan (gamma-Poisson on the pace) giving the completion-date
# range and the chance of reaching the target by the end of the target schedule.
th_projection <- function(P, sites, cfg, today = Sys.Date(), st = th_settings(cfg),
                          target = cfg$trial_target) {
  target <- suppressWarnings(as.numeric(target))
  if (nrow(P) < 3 || is.na(target) || target <= 0) return(NULL)
  today_d <- as.Date(today)
  d   <- sort(.th_as_date(P$rand))
  mon <- function(x) x - (as.integer(format(x, "%u")) - 1L)
  wk0 <- mon(d[1]); cur <- mon(today_d)
  weeks  <- seq(wk0, cur, by = "week")
  counts <- tabulate(as.integer(d - wk0) %/% 7L + 1L, nbins = length(weeks))
  done_w <- length(weeks) - 1L                      # the current week is partial
  if (done_w < 2) return(NULL)
  W   <- max(2L, min(as.integer(st$projection$window_weeks %||% 12), done_w))
  win <- counts[(done_w - W + 1):done_w]
  lam <- sum(win) / W
  n_now <- length(d)
  remaining <- max(0, target - n_now)

  g <- 1
  if (W >= 6 && sum(win) >= 5) {
    b <- tryCatch(stats::coef(stats::glm(win ~ seq_len(W), family = stats::poisson()))[2],
                  error = function(e) NA_real_)
    if (is.finite(b)) g <- exp(max(min(b, log(1.02)), log(0.98)))
  }

  # Site capacity: each recruiting site's recent weekly rate, shrunk towards the
  # average site so one lucky week doesn't dominate.
  s_rate <- vapply(split(d, P$site), function(x) {
    span <- max(1, min(13, as.numeric(today_d - min(x)) / 7))
    sum(x > today_d - span * 7) / span
  }, numeric(1))
  prior_m <- if (length(s_rate)) mean(s_rate) else lam
  s_rate  <- (s_rate * 4 + prior_m * 2) / 6
  med_site <- if (length(s_rate)) stats::median(s_rate) else lam
  pd <- cfg$projection_defaults %||% list()
  target_sites <- suppressWarnings(as.numeric(pd$target_sites %||% NA))
  per_month    <- suppressWarnings(as.numeric(pd$sites_central %||% 1))
  future_open <- numeric()
  if (!is.null(sites) && is.data.frame(sites) && nrow(sites) && "site_open_date" %in% names(sites)) {
    od <- suppressWarnings(as.Date(sites$site_open_date))
    future_open <- as.numeric(od[!is.na(od) & od > today_d & !sites$site_name %in% names(s_rate)] - cur) / 7
  }
  extra_n <- if (is.na(target_sites)) 0 else max(0, target_sites - length(s_rate) - length(future_open))
  if (extra_n > 0 && per_month > 0) {
    start_w <- if (length(future_open)) max(future_open) else 0
    future_open <- c(future_open, start_w + seq_len(extra_n) * 4.345 / per_month)
  }

  # Horizon: long enough for the slowest central scenario, capped at 10 years
  ts <- th_target_schedule(cfg)
  end_date <- if (!is.null(ts)) max(ts$month_date) else NA
  slow <- max(0.05, min(lam, lam * g^52, sum(s_rate)))
  H <- as.integer(min(520, max(26, ceiling(1.3 * remaining / slow),
                                 if (!is.na(end_date)) as.numeric(end_date - cur) / 7 + 8 else 0)))
  k <- seq_len(H)
  pace  <- n_now + lam * k
  trend <- n_now + cumsum(lam * g^k)
  site_w <- sum(s_rate) + vapply(k, function(j) {
    on <- future_open[future_open < j]
    sum(med_site * pmin(1, (j - on) / 4))           # new sites ramp up over a month
  }, numeric(1))
  sites_cum <- n_now + cumsum(site_w)

  # Monte Carlo on the pace with rate uncertainty (reproducible, global RNG untouched)
  S <- as.integer(st$projection$sims %||% 1000)
  had_seed <- exists(".Random.seed", envir = globalenv())
  if (had_seed) old_seed <- get(".Random.seed", envir = globalenv())
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = globalenv())
          else if (exists(".Random.seed", envir = globalenv())) rm(".Random.seed", envir = globalenv()),
          add = TRUE)
  set.seed(898)
  lam_s <- stats::rgamma(S, shape = sum(win) + 0.5, rate = W)
  sims  <- n_now + t(apply(matrix(stats::rpois(S * H, lam_s), nrow = S), 1, cumsum))
  band  <- apply(sims, 2, stats::quantile, probs = c(.1, .25, .5, .75, .9), names = FALSE)
  # Quantiles of whole-number counts move in steps (very visibly when recruitment
  # is slow). Joining the middle of each step draws the range as a smooth corridor.
  smooth_q <- function(x) {
    r <- rle(cummax(x))
    if (length(r$values) < 2) return(x)
    m <- cumsum(r$lengths) - r$lengths / 2 + 0.5
    stats::approx(m, r$values, xout = seq_along(x), rule = 2)$y
  }
  band <- t(apply(band, 1, smooth_q))
  hit_w <- apply(sims >= target, 1, function(r) if (any(r)) which(r)[1] else NA_integer_)
  wk_date <- function(j) if (is.na(j)) as.Date(NA) else cur + 7 * j
  first_hit <- function(x) { j <- which(x >= target)[1]; wk_date(j) }
  prob_end <- if (!is.na(end_date)) {
    j <- as.integer(ceiling(as.numeric(end_date - cur) / 7))
    if (j >= 1 && j <= H) mean(sims[, j] >= target) else if (j < 1) as.numeric(n_now >= target) else NA_real_
  } else NA_real_

  list(
    weeks = weeks, actual = cumsum(counts), weekly = counts,
    proj_weeks = cur + 7 * k,
    pace = pace, trend = trend, sites = sites_cum,
    band = list(p10 = band[1, ], p25 = band[2, ], p50 = band[3, ], p75 = band[4, ], p90 = band[5, ]),
    finish = list(pace = first_hit(pace), trend = first_hit(trend), sites = first_hit(sites_cum),
                  p10 = wk_date(stats::quantile(hit_w, .1, na.rm = TRUE, type = 1)),
                  p50 = wk_date(stats::quantile(hit_w, .5, na.rm = TRUE, type = 1)),
                  p90 = wk_date(stats::quantile(hit_w, .9, na.rm = TRUE, type = 1))),
    lambda_week = lam, per_month = lam * 52 / 12, growth_month = (g^(52 / 12) - 1) * 100,
    sites_per_month = sum(s_rate) * 52 / 12, open_sites = length(s_rate),
    to_open = length(future_open), prob_by_end = prob_end, end_date = end_date,
    target = target, n_now = n_now, window_weeks = W, schedule = ts, last_rand = max(d)
  )
}

# =============================================================================
# 8. Trial-level summary + one-call builder
# =============================================================================
th_summary <- function(P, crf, S, issues, proj, cfg, today, st, target) {
  today_s <- .th_today_s(today)
  ts  <- th_target_schedule(cfg)
  tbn <- .th_target_at(ts, today)
  n   <- nrow(P)
  ck  <- crf[crf$kind == "CRF", , drop = FALSE]
  expected <- sum(!is.na(ck$over_at) & ck$over_at <= today_s & ck$status %in% c("complete", "overdue"))
  done     <- sum(!is.na(ck$over_at) & ck$over_at <= today_s & ck$status == "complete")
  flagged  <- if (n) sum(.th_cos_flag(P, st)) else 0
  open_s   <- if (!is.null(S)) S[!S$status %in% c("Closed", "Paused") & S$months >= 1, , drop = FALSE] else NULL
  active   <- if (!is.null(open_s) && nrow(open_s)) mean(open_s$days_quiet <= st$flags$quiet_amber, na.rm = TRUE) else NA

  comp <- c(
    recruitment = if (!is.na(tbn) && tbn > 0) .th_clamp(100 * n / tbn) else
                  if (!is.null(S) && any(!is.na(S$sc_recruit))) mean(S$sc_recruit, na.rm = TRUE) else NA,
    # Within the expected attrition the score stays 80-100; beyond it, it falls to 0 at double
    retention   = if (n >= st$flags$min_n) {
      pct <- 100 * flagged / n; ea <- max(1, as.numeric(st$flags$expected_attrition %||% 15))
      if (pct <= ea) 100 - 20 * pct / ea else .th_clamp(80 - 80 * (pct - ea) / ea)
    } else NA,
    data        = if (expected) 100 * done / expected else NA,
    activity    = if (!is.na(active)) 100 * active else NA)
  wts <- unlist(st$weights)[c("recruitment", "retention", "data", "activity")]
  ok  <- !is.na(comp) & wts > 0
  list(
    n = n, target = target, target_by_now = tbn,
    ahead = if (!is.na(tbn)) n - round(tbn) else NA,
    expected_crfs = expected, complete_crfs = done,
    overdue_crfs = sum(ck$status == "overdue"),
    overdue_proms = sum(crf$kind != "CRF" & crf$status == "overdue"),
    completeness = if (expected) 100 * done / expected else NA,
    flagged = flagged, flagged_pct = if (n) 100 * flagged / n else NA,
    attention = if (is.null(issues)) c(High = 0, Medium = 0, Low = 0)
                else table(factor(issues$priority, c("High", "Medium", "Low"))),
    rag = if (is.null(S)) NULL else table(factor(S$rag, c("red", "amber", "green", "new"))),
    components = comp,
    score = if (any(ok)) round(sum(comp[ok] * wts[ok]) / sum(wts[ok])) else NA,
    finish = proj$finish, prob_by_end = proj$prob_by_end, end_date = proj$end_date
  )
}

th_build <- function(raw, sites, cfg, today = Sys.Date(), trial_target = NULL) {
  if (is.null(cfg)) return(NULL)
  st <- th_settings(cfg)
  P  <- th_participants(raw, cfg, today)
  P$stage <- .th_stage(P, cfg, today)
  sched <- th_crf_schedule(cfg, raw)
  crf   <- th_crf_status(raw, P, sched, cfg, today, st)
  S     <- if (nrow(P)) th_site_health(P, crf, sites, cfg, today, st) else NULL
  iss   <- th_participant_issues(P, crf, cfg, today, st)
  pat   <- th_rand_patterns(P, cfg, today, st)
  target <- suppressWarnings(as.numeric(trial_target %||% cfg$trial_target))
  proj  <- tryCatch(th_projection(P, sites, cfg, today, st, target),
                    error = function(e) { message("th_projection: ", e$message); NULL })
  list(settings = st, participants = P, schedule = sched, crf = crf, sites = S,
       issues = iss, patterns = pat, projection = proj,
       summary = th_summary(P, crf, S, iss, proj, cfg, today, st, target),
       today = as.Date(today))
}
