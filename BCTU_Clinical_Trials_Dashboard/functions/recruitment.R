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
#       screened_value   = NULL,               # or a value the field must equal
#       eligible_field   = "screening_calc", eligible_value  = "4",
#       approached_field = "approached_yn",  approached_value = "1"
#     )
#   )
#
# Some trials need more than one field to agree before a participant counts.
# PANORAMA screens and consents inside REDCap, so recruitment is the two form
# completion flags together: `screening_complete = 2` AND `consent_complete = 2`.
# List them under `conditions` and every one must hold:
#
#   recruitment = list(
#     basis = "all_conditions",
#     event = "baseline_arm_1",
#     conditions = list(
#       list(field = "screening_complete", value = "2"),
#       list(field = "consent_complete",   value = "2")))
#
# A participant who meets the screening definition but not every recruitment
# condition is reported as screened only — see `screened_only` below.
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
  event <- mapping_first(r$event %||% cfg$redcap_events$baseline, NA_character_)

  # Every condition that must hold for a participant to count as recruited.
  # Each inherits the recruitment event unless it names its own.
  conds <- lapply(r$conditions %||% list(), function(x) list(
    field = mapping_first(x$field, NA_character_),
    value = trimws(as.character(x$value %||% "")),
    event = mapping_first(x$event %||% r$event %||% cfg$redcap_events$baseline,
                          NA_character_)))
  conds <- Filter(function(x) !is.na(x$field) && nzchar(x$field), conds)

  basis <- r$basis
  if (is.null(basis) || !nzchar(basis)) {
    basis <- if (length(conds)) "all_conditions"
             else if (!mapping_is_blank(r$consent_field)) "consent_field"
             else if (!mapping_is_blank(f$randomisation_datetime)) "date_field"
             else "event_present"
  }
  # A config that asks for all_conditions but lists none would count everyone;
  # fall back to the single consent field rather than inflate the total.
  if (identical(basis, "all_conditions") && !length(conds)) basis <- "consent_field"

  list(
    model         = model,
    basis         = basis,
    event         = event,
    date_field    = mapping_first(r$date_field %||% f$randomisation_datetime, NA_character_),
    consent_field = mapping_first(r$consent_field %||% f$valid_consent, NA_character_),
    consent_value = as.character(r$consent_value %||% "1"),
    conditions    = conds,
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
    # NA means "any non-blank value counts as screened"; a value means the
    # field has to equal it (PANORAMA: screening_complete = 2).
    screened_value   = if (is.null(s$screened_value)) NA_character_
                       else trimws(as.character(s$screened_value)),
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
  if (!is.na(event) && nzchar(event) && event_col %in% names(d)) {
    in_event <- d[as.character(d[[event_col]]) %in% event, , drop = FALSE]
    # A mapped event that matches nothing means the config names an event the
    # export does not have — renamed in REDCap, or never created. Reading zero
    # rows there silently collapses the whole funnel (and the recruited count)
    # to 0 while the participants are sitting in the export, so fall back to
    # the unfiltered frame, as baseline_rows() already does.
    if (nrow(in_event)) d <- in_event
  }
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
#' target; ids that never got that far are still reported as screened, and
#' `screened_only` is exactly those — screened but not (yet) recruited.
recruitment_counts <- function(raw, cfg = current_trial_config()) {
  spec <- recruitment_spec(cfg)
  scr  <- spec$screening
  id_col <- mapping_first((cfg %||% list())$redcap_fields$record_id, "record_id")
  if (is.null(raw) || !is.data.frame(raw) || !nrow(raw) || !id_col %in% names(raw))
    return(list(spec = spec, all_ids = character(0), screened = character(0),
                eligible = character(0), approached = character(0),
                recruited = character(0), screened_only = character(0),
                n_screened = 0L, n_eligible = 0L, n_approached = 0L,
                n_recruited = 0L, n_screened_only = 0L))

  all_ids <- unique(as.character(raw[[id_col]]))
  all_ids <- all_ids[!is.na(all_ids) & nzchar(all_ids)]
  val <- function(field, event = NA_character_)
    .rec_values(raw, field, event, id_col = id_col)

  screened <- if (isTRUE(scr$enabled) && !is.na(scr$screened_field)) {
    v <- val(scr$screened_field, scr$event)
    if (is.na(scr$screened_value) || !nzchar(scr$screened_value)) names(v)
    else names(v)[trimws(v) == scr$screened_value]
  } else all_ids

  eligible <- if (isTRUE(scr$enabled) && !is.na(scr$eligible_field)) {
    v <- val(scr$eligible_field, scr$event)
    names(v)[trimws(v) == scr$eligible_value]
  } else screened

  approached <- if (isTRUE(scr$enabled) && !is.na(scr$approached_field)) {
    v <- val(scr$approached_field, scr$event)
    names(v)[trimws(v) == scr$approached_value]
  } else eligible

  recruited <- switch(spec$basis,
    # Every condition must hold. PANORAMA is the case this exists for:
    # screening_complete = 2 AND consent_complete = 2. A participant with the
    # screening form complete but consent blank or 0 fails here and stays in
    # `screened` only.
    all_conditions = {
      # Every condition has to hold, so every one has to be testable. A field
      # the export does not carry means the config and the export disagree (a
      # per-work-package export without the consent form, a renamed variable),
      # and neither way of guessing is safe: dropping the condition counts
      # everyone who met the rest, applying it counts nobody. Both are silently
      # wrong. Say so and return NULL, which tells recruited_ids() the
      # definition cannot be applied here and leaves the count to the caller's
      # own rule.
      missing <- Filter(function(f) !f %in% names(raw),
                        vapply(spec$conditions, function(x) x$field, character(1)))
      if (length(missing)) {
        message("Recruitment: the export has no ", paste(missing, collapse = ", "),
                " column, so this trial's recruitment definition cannot be ",
                "applied to it. Counting as configured elsewhere instead. ",
                "Check the field mapping in Trial Settings, or whether the ",
                "export you loaded is the right one.")
        NULL
      } else {
        ok <- all_ids
        for (cnd in spec$conditions) {
          v  <- val(cnd$field, cnd$event)
          ok <- intersect(ok, names(v)[trimws(v) == cnd$value])
        }
        as.character(ok)
      }
    },
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
  # NULL means "this export cannot be judged against the trial's definition";
  # character(0) means "judged, and nobody qualifies". Callers act differently
  # on the two, so the difference must survive this point.
  recruited     <- if (is.null(recruited)) NULL else
    as.character(intersect(recruited, all_ids))
  screened_only <- setdiff(screened, recruited %||% character(0))

  list(spec = spec, all_ids = all_ids,
       screened = screened, eligible = eligible,
       approached = approached, recruited = recruited,
       screened_only = screened_only,
       n_screened = length(screened), n_eligible = length(eligible),
       n_approached = length(approached), n_recruited = length(recruited),
       n_screened_only = length(screened_only),
       # TRUE when the trial's definition could actually be applied here.
       recruited_known = !is.null(recruited))
}

#' Participant ids that count towards the recruitment target, or NULL.
#' NULL means "this trial has no explicit recruitment definition" — callers
#' then count as they did before the spec existed, so nothing changes for a
#' trial configured without a `recruitment` block.
recruited_ids <- function(raw, cfg = current_trial_config()) {
  if (is.null(cfg) || is.null(cfg$recruitment)) return(NULL)
  out <- tryCatch(recruitment_counts(raw, cfg)$recruited, error = function(e) NULL)
  # NULL travels through unchanged: the definition could not be applied to this
  # export, so the caller counts as it did before. character(0) is a real
  # answer — nobody is recruited yet — and must not be turned into NULL.
  if (is.null(out)) NULL else as.character(out)
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
