# =============================================================================
# Recruitment definition — regression test
# =============================================================================
# PANORAMA screens and consents inside REDCap, so a participant only counts as
# recruited when screening_complete = 2 AND consent_complete = 2. Screened with
# consent 0 or blank is screened only. This test pins that, plus the two things
# that made the count read 0 while participants sat in the export:
#   * the recruitment/screening events named an event the project does not have
#   * only one field (valid_consent) decided recruitment
#
# Base R only — run from the app directory:  Rscript tests/recruitment_panorama.R
# Exits non-zero on the first failed assertion.
# =============================================================================

setwd(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE),
                                                value = TRUE))), ".."))

`%||%` <- function(a, b) if (is.null(a)) b else a
mapping_is_blank <- function(x)
  is.null(x) || length(x) == 0 || (is.character(x) && !any(nzchar(trimws(x))))
mapping_first <- function(x, default = NA_character_)
  if (mapping_is_blank(x)) default else as.character(unlist(x))[1]
is_randomised_trial <- function(cfg)
  !identical(cfg$recruitment_model, "registration")
baseline_rows <- function(raw, cfg = NULL, event_col = "redcap_event_name",
                          id_col = NULL) {
  out <- raw[as.character(raw[[event_col]]) %in%
               as.character(unlist(cfg$redcap_events$baseline)), , drop = FALSE]
  if (nrow(out)) out else raw[!duplicated(raw$record_id), , drop = FALSE]
}

source("trials/panorama/config.R")
cfg <- trial_config
current_trial_config <- function() cfg
source("functions/recruitment.R")

ok <- function(cond, what) {
  if (!isTRUE(cond)) { cat("FAIL:", what, "\n"); quit(status = 1L) }
  cat("  ✓", what, "\n")
}

# Records 1 and 4 are recruited; 2 (consent 0) and 3 (consent blank) are
# screened only; 5 never finished screening.
raw <- data.frame(stringsAsFactors = FALSE,
  record_id           = c("1", "1", "2", "3", "4", "5"),
  redcap_event_name   = c("baseline_arm_1", "fu_7_day_arm_1", "baseline_arm_1",
                          "baseline_arm_1", "baseline_arm_1", "baseline_arm_1"),
  rc_site_name        = c("Site A", "Site A", "Site A", "Site B", "Site B", "Site B"),
  screening_calc      = c("4", "", "4", "4", "4", "2"),
  approached_yn       = c("1", "", "1", "1", "1", ""),
  valid_consent       = c("1", "", "", "", "", ""),
  screening_complete  = c("2", "", "2", "2", "2", "0"),
  consent_complete    = c("2", "", "0", "", "2", "0"),
  screen_created_date = c("2026-08-01", "", "2026-08-02", "2026-08-03",
                          "2026-08-04", "2026-08-05"))

rc <- recruitment_counts(raw, cfg)

ok(identical(rc$spec$basis, "all_conditions"),
   "PANORAMA recruits on all conditions, not a single consent field")
ok(identical(rc$spec$event, "baseline_arm_1"),
   "recruitment event is an event the project actually has")
ok(identical(rc$spec$screening$event, "baseline_arm_1"),
   "screening event is an event the project actually has")

ok(identical(sort(rc$recruited), c("1", "4")),
   "recruited = screening_complete 2 AND consent_complete 2")
ok(rc$n_recruited == 2L, "recruited count is 2, not 0")
ok(identical(sort(rc$screened_only), c("2", "3")),
   "consent 0 or blank counts as screened only, never recruited")
ok(rc$n_screened == 4L, "screened = screening_complete 2")
ok(!any(rc$recruited %in% rc$screened_only),
   "no participant is both recruited and screened only")

# valid_consent alone would have found only record 1: the old rule undercounted
# every participant consented through the form flags.
ok(sum(trimws(raw$valid_consent) == "1") == 1L,
   "the old valid_consent rule would have counted 1 of 2")

# An event named in the config but absent from the export must not zero the
# funnel — that is what made every stage read 0.
bad <- cfg
bad$recruitment$event            <- "screening_arm_1"
bad$recruitment$screening$event  <- "screening_arm_1"
bad$recruitment$conditions <- lapply(bad$recruitment$conditions, function(x) {
  x$event <- "screening_arm_1"; x
})
rc_bad <- recruitment_counts(raw, bad)
ok(rc_bad$n_recruited == 2L,
   "a configured event missing from the export falls back to the whole export")

# recruited_ids() is what the dashboard counts with; NULL only for a trial that
# declares no recruitment model.
ok(identical(sort(recruited_ids(raw, cfg)), c("1", "4")),
   "recruited_ids returns the recruited set")
no_model <- cfg; no_model$recruitment <- NULL
ok(is.null(recruited_ids(raw, no_model)),
   "recruited_ids is NULL for a trial with no recruitment model")

# A condition whose field the export does not carry cannot be tested. Applying
# it anyway excluded everyone and reported 0 recruited against a full export —
# the definition is reported as inapplicable instead, and recruited_ids() hands
# the count back to the caller's own rule.
no_consent_col <- raw[, setdiff(names(raw), "consent_complete"), drop = FALSE]
rc_nc <- suppressMessages(recruitment_counts(no_consent_col, cfg))
ok(!isTRUE(rc_nc$recruited_known),
   "a missing condition column makes the definition inapplicable, not 0")
ok(is.null(suppressMessages(recruited_ids(no_consent_col, cfg))),
   "recruited_ids is NULL when the export cannot be judged")

# Judged, and nobody qualifies, is a real answer and must stay distinct from
# the above — it is what a trial that is still screening should report.
none_yet <- raw
none_yet$consent_complete <- "0"
rc_none <- recruitment_counts(none_yet, cfg)
ok(isTRUE(rc_none$recruited_known),
   "an export with the columns present is judged")
ok(identical(rc_none$recruited, character(0)) && rc_none$n_recruited == 0L,
   "nobody consented yet reports 0 recruited")
ok(identical(recruited_ids(none_yet, cfg), character(0)),
   "recruited_ids is character(0), not NULL, when nobody qualifies")
ok(rc_none$n_screened_only == 4L,
   "everyone screened is screened-only when nobody has consented")

cat("\nAll recruitment assertions passed.\n")
