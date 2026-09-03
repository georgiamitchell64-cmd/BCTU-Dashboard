# =============================================================================
# Recruitment model — screening, eligibility and what counts as recruited
# =============================================================================
# Trials differ in what "recruited" means. A randomised trial counts a
# participant from the moment they are randomised. A cohort or registry counts
# them from consent or registration, and many of those screen a much larger
# group first: screened → eligible → approached → consented, with only the last
# step counting towards the target.
#
# The active trial describes this in its config under `recruitment`:
#
#   recruitment = list(
#     model  = "randomised" | "registration",
#     basis  = "consent_field" | "date_field" | "event_present",
#     event      = "screening_arm_1",   # event the registration record sits in
#     date_field = "screen_created_date",
#     consent_field = "valid_consent", consent_value = "1",
#     screening = list(
#       enabled          = TRUE,
#       event            = "screening_arm_1",
#       screened_field   = "screening_calc",   # any non-blank value = screened
#       eligible_field   = "screening_calc", eligible_value  = "4",
#       approached_field = "approached_yn",  approached_value = "1"
#     )
#   )
#
# Nothing here is required: a config without a `recruitment` block falls back to
# the legacy behaviour (randomisation datetime + baseline event), so trials
# configured before this existed keep counting exactly as they did.
# =============================================================================

#' Resolved recruitment spec for a trial, with legacy fallbacks filled in.
recruitment_spec <- function(cfg = current_trial_config()) {
  cfg <- cfg %||% list()
  r   <- cfg$recruitment %||% list()
  f   <- cfg$redcap_fields %||% list()

  model <- r$model %||% (if (is_randomised_trial(cfg)) "randomised" else "registration")
  basis <- r$basis
  if (is.null(basis) || !nzchar(basis)) {
    basis <- if (!mapping_is_blank(r$consent_field)) "consent_field"
             else if (!mapping_is_blank(f$randomisation_datetime)) "date_field"
             else "event_present"
  }

  list(
    model         = model,
    basis         = basis,
    event         = mapping_first(r$event %||% cfg$redcap_events$baseline, NA_character_),
    date_field    = mapping_first(r$date_field %||% f$randomisation_datetime, NA_character_),
    consent_field = mapping_first(r$consent_field %||% f$valid_consent, NA_character_),
    consent_value = as.character(r$consent_value %||% "1"),
    screening     = screening_spec(cfg))
}

#' Resolved screening spec, or a disabled one when the trial doesn't screen.
screening_spec <- function(cfg = current_trial_config()) {
  cfg <- cfg %||% list()
  s   <- (cfg$recruitment %||% list())$screening %||% list()
  f   <- cfg$redcap_fields %||% list()

  screened   <- mapping_first(s$screened_field   %||% f$screening_calc, NA_character_)
  eligible   <- mapping_first(s$eligible_field   %||% f$screening_calc, NA_character_)
  approached <- mapping_first(s$approached_field %||% f$approached,     NA_character_)

  enabled <- if (!is.null(s$enabled)) isTRUE(s$enabled) else
    (!is.na(screened) || !is.na(eligible) || !is.na(approached))

  list(
    enabled          = enabled,
    event            = mapping_first(s$event %||% cfg$redcap_events$screening, NA_character_),
    screened_field   = screened,
    eligible_field   = eligible,
    eligible_value   = as.character(s$eligible_value %||% "4"),
    approached_field = approached,
    approached_value = as.character(s$approached_value %||% "1"),
    date_field       = mapping_first(s$date_field %||% f$screen_created_date, NA_character_))
}

# First non-blank value per participant, optionally within one event.
.rec_values <- function(raw, field, event = NA_character_, id_col = "record_id",
                        event_col = "redcap_event_name") {
  if (is.null(raw) || !nrow(raw) || is.na(field) || !field %in% names(raw) ||
      !id_col %in% names(raw)) return(character(0))
  d <- raw
  if (!is.na(event) && nzchar(event) && event_col %in% names(d))
    d <- d[as.character(d[[event_col]]) %in% event, , drop = FALSE]
  if (!nrow(d)) return(character(0))
  v <- as.character(d[[field]]); r <- as.character(d[[id_col]])
  keep <- !is.na(v) & nzchar(trimws(v)) & !is.na(r) & nzchar(r)
  v <- v[keep]; r <- r[keep]
  if (!length(v)) return(character(0))
  first <- !duplicated(r)
  stats::setNames(v[first], r[first])
}

#' Screening funnel and recruited count for a raw REDCap export.
#' Returns counts plus the participant ids behind each, so callers can filter.
#' `recruited` is whichever definition the trial's spec says counts towards the
#' target; ids that never got that far are still reported as screened.
recruitment_counts <- function(raw, cfg = current_trial_config()) {
  spec <- recruitment_spec(cfg)
  scr  <- spec$screening
  id_col <- mapping_first((cfg %||% list())$redcap_fields$record_id, "record_id")
  if (is.null(raw) || !is.data.frame(raw) || !nrow(raw) || !id_col %in% names(raw))
    return(list(spec = spec, all_ids = character(0), screened = character(0),
                eligible = character(0), approached = character(0),
                recruited = character(0), n_screened = 0L, n_eligible = 0L,
                n_approached = 0L, n_recruited = 0L))

  all_ids <- unique(as.character(raw[[id_col]]))
  all_ids <- all_ids[!is.na(all_ids) & nzchar(all_ids)]
  val <- function(field, event = NA_character_)
    .rec_values(raw, field, event, id_col = id_col)

  screened <- if (isTRUE(scr$enabled) && !is.na(scr$screened_field))
    names(val(scr$screened_field, scr$event)) else all_ids

  eligible <- if (isTRUE(scr$enabled) && !is.na(scr$eligible_field)) {
    v <- val(scr$eligible_field, scr$event)
    names(v)[trimws(v) == scr$eligible_value]
  } else screened

  approached <- if (isTRUE(scr$enabled) && !is.na(scr$approached_field)) {
    v <- val(scr$approached_field, scr$event)
    names(v)[trimws(v) == scr$approached_value]
  } else eligible

  recruited <- switch(spec$basis,
    consent_field = {
      v <- val(spec$consent_field, spec$event)
      names(v)[trimws(v) == spec$consent_value]
    },
    date_field    = names(val(spec$date_field, NA_character_)),
    event_present = {
      b <- baseline_rows(raw, cfg)
      unique(as.character(b[[id_col]]))
    },
    all_ids)
  recruited <- intersect(recruited, all_ids)

  list(spec = spec, all_ids = all_ids,
       screened = screened, eligible = eligible,
       approached = approached, recruited = recruited,
       n_screened = length(screened), n_eligible = length(eligible),
       n_approached = length(approached), n_recruited = length(recruited))
}

# =============================================================================
# Timepoints — what is due, and what has not happened yet
# =============================================================================
# A trial that opened last month has no 3-month follow-ups; its export carries
# no rows for that event at all. Showing 0% complete for it is wrong — the
# window has not opened. Each timepoint can declare when it falls due:
#
#   timepoints = list(
#     day_30 = list(label = "Day 30", offset_days = 30, window_days = 3,
#                   anchor = "recruitment")   # or "discharge"
#   )
#
# With no `timepoints` block a timepoint is simply "due for everyone", which is
# the behaviour trials had before this existed.
# =============================================================================

#' Resolved timepoint metadata for every follow-up role in the config.
timepoint_spec <- function(cfg = current_trial_config()) {
  cfg <- cfg %||% list()
  ev  <- cfg$redcap_events %||% list()
  tp  <- cfg$timepoints %||% list()
  roles <- setdiff(names(ev), c("baseline", "screening", "sub_forms"))
  out <- lapply(roles, function(r) {
    m <- tp[[r]] %||% list()
    list(role        = r,
         label       = m$label %||% tools::toTitleCase(gsub("_", " ", r)),
         event       = mapping_first(ev[[r]], NA_character_),
         offset_days = suppressWarnings(as.numeric(m$offset_days %||% NA)),
         window_days = suppressWarnings(as.numeric(m$window_days %||% 0)),
         anchor      = m$anchor %||% "recruitment")
  })
  stats::setNames(out, roles)
}

#' Which participants a timepoint has actually fallen due for.
#' `anchor_dates` is a named vector (participant id -> Date) of the recruitment
#' or discharge date the offset counts from. Returns every id when the timepoint
#' declares no offset, so unconfigured trials behave as before.
timepoint_due_ids <- function(ids, anchor_dates, offset_days, window_days = 0,
                              today = Sys.Date()) {
  if (!length(ids)) return(character(0))
  if (is.null(offset_days) || is.na(offset_days)) return(ids)
  d <- suppressWarnings(as.Date(anchor_dates[ids]))
  w <- if (is.null(window_days) || is.na(window_days)) 0 else window_days
  ids[!is.na(d) & today >= (d + offset_days + w)]
}
