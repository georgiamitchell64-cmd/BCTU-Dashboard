# =============================================================================
# Change of status — withdrawals, loss to follow-up, deaths and any other
# status change, summarised for the Data tab's "Withdrawals & change of status"
# panel and its safety tile.
# =============================================================================
# Builds on withdrawal_events() (functions/safety_events.R), so every column is
# resolved through the trial config and any trial with a change-of-status form
# gets the same panel. The denominator is the number randomised, the same basis
# as the recruitment charts and the timepoint donuts.

# Status types that take a participant out of follow-up. A trial can list its
# own codes in cfg$cos_ends_followup; otherwise the type's label decides.
.COS_ENDS_PATTERN <- "(?i)death|died|deceased|complete withdrawal|withdrawn completely|lost to follow|ltfu"

cos_ends_followup <- function(codes, labels, cfg = NULL) {
  own <- as.character(unlist(cfg$cos_ends_followup %||% character(0)))
  if (length(own)) return(as.character(codes) %in% own)
  grepl(.COS_ENDS_PATTERN, as.character(labels), perl = TRUE)
}

# Participant ids that count as randomised: the trial's recruitment definition
# when it has one, else anyone with a randomisation date, else everyone.
.cos_randomised_ids <- function(raw, cfg) {
  if (is.null(raw) || !nrow(raw) || !"record_id" %in% names(raw)) return(character(0))
  rec <- tryCatch(recruited_ids(raw, cfg), error = function(e) NULL)
  if (!is.null(rec)) return(unique(as.character(rec)))
  rc <- fld("randomisation_datetime", "rand_dttm_s")
  if (!rc %in% names(raw)) return(unique(as.character(raw$record_id)))
  v <- trimws(as.character(raw[[rc]]))
  unique(as.character(raw$record_id[!is.na(v) & nzchar(v) & v != "NA"]))
}

# The withdrawal reason, when the trial records one: the first mapped detail
# field whose header mentions a reason, else the configured reason field.
# Returns list(field = header or NULL, table = data.frame(reason, n)).
.cos_reasons <- function(ev, cfg) {
  fields <- tryCatch(detail_fields_for("withdrawal"), error = function(e) list())
  is_reason <- function(f) {
    h <- as.character(f$header %||% f$col %||% "")
    grepl("(?i)reason|why", h, perl = TRUE) && !grepl("(?i)other", h, perl = TRUE)
  }
  tally <- function(v) {
    v <- trimws(as.character(v)); v <- v[!is.na(v) & nzchar(v) & v != "NA"]
    if (!length(v)) return(data.frame(reason = character(0), n = integer(0)))
    tab <- sort(table(v), decreasing = TRUE)
    data.frame(reason = names(tab), n = as.integer(tab), stringsAsFactors = FALSE)
  }
  for (f in Filter(is_reason, fields)) {
    xc <- paste0("x__", f$header)
    if (!xc %in% names(ev)) next
    v <- as.character(ev[[xc]])
    keep <- !is.na(v) & nzchar(trimws(v))
    if (any(keep)) v[keep] <- .resolve_value_labels(v[keep], f$col, cfg)
    return(list(field = f$header, table = tally(v)))
  }
  if ("narrative" %in% names(ev) && any(!is.na(ev$narrative) & nzchar(trimws(ev$narrative))))
    return(list(field = "Reason", table = tally(ev$narrative)))
  list(field = if (length(Filter(is_reason, fields))) Filter(is_reason, fields)[[1]]$header else NULL,
       table = data.frame(reason = character(0), n = integer(0)))
}

# Changes per month, from the month of the first randomisation (or the last 12
# months for an older trial) to the current month.
.cos_months <- function(dates, rand_dates, today) {
  m_now   <- as.Date(format(today, "%Y-%m-01"))
  first   <- suppressWarnings(min(c(rand_dates, dates), na.rm = TRUE))
  m_first <- if (is.finite(first)) as.Date(format(first, "%Y-%m-01")) else m_now
  start   <- max(min(m_first, m_now), seq(m_now, by = "-11 months", length.out = 2)[2])
  months  <- seq(start, m_now, by = "month")
  key     <- format(dates[!is.na(dates)], "%Y-%m")
  data.frame(month = months,
             n = vapply(format(months, "%Y-%m"), function(k) sum(key == k), integer(1),
                        USE.NAMES = FALSE))
}

#' Everything the change-of-status panel shows, in one list.
change_of_status_summary <- function(raw, cfg = current_trial_config(),
                                     wd = withdrawal_events(raw), today = Sys.Date()) {
  rand_ids <- .cos_randomised_ids(raw, cfg)
  n_rand   <- length(rand_ids)
  labels   <- cfg$cos_type_labels %||% (if (exists("cos_type_labels")) cos_type_labels else NULL)
  descs    <- cfg$cos_type_descriptions %||%
              (if (exists("cos_type_descriptions")) cos_type_descriptions else NULL)
  ev       <- if (is.null(wd)) data.frame() else as.data.frame(wd, stringsAsFactors = FALSE)
  has_form <- nrow(ev) > 0 || (!is.null(raw) && fld("cos_type", "cos_type") %in% names(raw))

  # Per-participant randomisation date and site
  rd_raw    <- .rec_values(raw, fld("randomisation_datetime", "rand_dttm_s"))
  rand_date <- stats::setNames(suppressWarnings(as.Date(substr(rd_raw, 1, 10))), names(rd_raw))
  site_of   <- .rec_values(raw, "site_dag")

  if (nrow(ev)) {
    ev$record_id <- as.character(ev$record_id)
    ev$rand_date <- unname(rand_date[ev$record_id])
    ev$days_in   <- as.integer(ev$onset_date - ev$rand_date)
    ev$ends      <- cos_ends_followup(ev$severity, ev$term, cfg)
    s <- unname(site_of[ev$record_id])
    ev$site <- ifelse(is.na(ev$site) | !nzchar(ev$site), s, ev$site)
  }

  codes       <- as.character(ev$severity %||% character(0))
  changed_ids <- unique(as.character(ev$record_id %||% character(0)))
  ended_ids   <- if (nrow(ev)) unique(ev$record_id[ev$ends %in% TRUE]) else character(0)
  in_rand     <- function(ids) if (n_rand) intersect(ids, rand_ids) else ids
  n_changed   <- length(in_rand(changed_ids))
  n_ended     <- length(in_rand(ended_ids))

  # Every configured type (zero counts included), then any code the export
  # carries that the config does not name.
  all_codes <- unique(c(names(labels), codes))
  all_codes <- all_codes[!is.na(all_codes) & nzchar(all_codes)]
  lab_of  <- function(k) if (!is.null(labels) && k %in% names(labels)) as.character(labels[[k]]) else paste("Code", k)
  desc_of <- function(k) if (!is.null(descs) && k %in% names(descs)) as.character(descs[[k]]) else ""
  types <- data.frame(code = all_codes, stringsAsFactors = FALSE)
  if (length(all_codes)) {
    types$label       <- vapply(all_codes, lab_of,  character(1), USE.NAMES = FALSE)
    types$description <- vapply(all_codes, desc_of, character(1), USE.NAMES = FALSE)
    types$events      <- vapply(all_codes, function(k) sum(codes == k), integer(1), USE.NAMES = FALSE)
    types$people      <- vapply(all_codes, function(k)
                           length(unique(ev$record_id[codes == k])), integer(1), USE.NAMES = FALSE)
    types$pct         <- if (n_rand) types$people / n_rand else 0
    types$ends        <- cos_ends_followup(types$code, types$label, cfg)
    types <- types[order(-types$people, match(types$code, all_codes)), , drop = FALSE]
  }

  # By site: share of each site's randomised participants with a change
  sites <- NULL
  if (n_rand) {
    site_name <- function(ids) { s <- unname(site_of[ids]); s[is.na(s) | !nzchar(s)] <- "Unknown site"; s }
    rs <- site_name(rand_ids); cs <- site_name(in_rand(changed_ids)); es <- site_name(in_rand(ended_ids))
    tab <- table(rs)
    sites <- data.frame(site = names(tab), n_rand = as.integer(tab), stringsAsFactors = FALSE)
    sites$n_changed <- vapply(sites$site, function(s) sum(cs == s), integer(1), USE.NAMES = FALSE)
    sites$n_ended   <- vapply(sites$site, function(s) sum(es == s), integer(1), USE.NAMES = FALSE)
    sites$rate      <- sites$n_changed / sites$n_rand
    sites <- sites[order(-sites$rate, -sites$n_changed, sites$site), , drop = FALSE]
  }
  overall <- if (n_rand) n_changed / n_rand else 0
  if (!is.null(sites)) sites$high <- sites$rate > overall & sites$n_changed >= 2

  dates <- if (nrow(ev)) ev$onset_date else as.Date(character(0))
  dated <- dates[!is.na(dates) & dates <= today]
  list(
    has_form   = has_form,
    n_rand     = n_rand,
    n_events   = nrow(ev),
    n_changed  = n_changed,
    n_ended    = n_ended,
    n_active   = max(0L, n_rand - n_ended),
    overall    = overall,
    ending_labels = if (nrow(types)) types$label[types$ends] else character(0),
    types      = types,
    sites      = sites,
    months     = .cos_months(dates, unname(rand_date[rand_ids]), today),
    n_last30   = sum(dated > today - 30),
    n_prev30   = sum(dated > today - 60 & dated <= today - 30),
    latest     = if (length(dated)) max(dated) else as.Date(NA),
    n_undated  = sum(is.na(dates)),
    median_days = if (nrow(ev) && any(!is.na(ev$days_in)))
                    as.integer(stats::median(ev$days_in, na.rm = TRUE)) else NA_integer_,
    reasons    = .cos_reasons(ev, cfg),
    events     = ev
  )
}
