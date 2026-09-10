# =============================================================================
# prepare_report_data — baseline event mismatch
# =============================================================================
# An export whose participants are not under the mapped baseline event used to
# leave every participant frame empty, and the first length-1 default assigned
# into that zero-row frame failed with base R's
#
#   Error: replacement has 1 row, data has 0
#
# which says nothing about the cause. The baseline role now falls back to one
# row per participant (as baseline_rows() does everywhere else) and reports the
# mismatch, and the zero-row case is survivable rather than fatal.
#
# Needs the app's runtime packages; skips cleanly without them.
#   Rscript tests/report_data_events.R
# =============================================================================

setwd(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE),
                                                value = TRUE))), ".."))

need <- c("dplyr", "tidyr", "stringr", "lubridate", "tibble", "rlang")
absent <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(absent)) {
  cat("SKIP: needs", paste(absent, collapse = ", "), "\n"); quit(status = 0L)
}
suppressPackageStartupMessages(for (p in need) library(p, character.only = TRUE))

for (f in c("globals/constants.R", "globals/datasets.R", "globals/trial_config.R",
            "functions/helpers.R", "functions/trial_overrides.R",
            "functions/recruitment.R", "functions/prepare_report_data.R"))
  source(f)

cfg <- discover_trials()[["panorama"]]
if (is.null(cfg)) { cat("SKIP: no panorama trial config\n"); quit(status = 0L) }
apply_trial_globals(cfg)

ok <- function(cond, what) {
  if (!isTRUE(cond)) { cat("FAIL:", what, "\n"); quit(status = 1L) }
  cat("  ✓", what, "\n")
}

mk <- function(event) data.frame(stringsAsFactors = FALSE,
  record_id           = c("1", "2"),
  redcap_event_name   = c(event, event),
  rc_site_name        = c("Site A", "Site B"),
  screening_calc      = c("4", "4"),
  approached_yn       = c("1", "1"),
  screening_complete  = c("2", "2"),
  consent_complete    = c("2", "0"),
  screen_created_date = c("2026-08-01", "2026-08-02"),
  index_discharge_date = c("2026-08-03", ""))

# The mapped baseline event is baseline_arm_1; this export has none of it.
res <- suppressMessages(tryCatch(prepare_report_data(mk("screening_arm_1")),
                                 error = function(e) conditionMessage(e)))
ok(!is.character(res),
   "an export with no rows at the mapped baseline event still builds")
ok(is.list(res) && res$kpis$total_randomised == 1L,
   "the fallback finds the one recruited participant")

# The mapping matching normally must be unaffected.
res2 <- suppressMessages(prepare_report_data(mk("baseline_arm_1")))
ok(res2$kpis$total_randomised == 1L,
   "a correctly mapped export counts the same")

# Nobody recruited yet is a report showing 0, not an error.
none <- mk("baseline_arm_1"); none$consent_complete <- c("0", "0")
res3 <- suppressMessages(tryCatch(prepare_report_data(none),
                                  error = function(e) conditionMessage(e)))
ok(!is.character(res3), "a trial still screening builds a report")
ok(res3$kpis$total_randomised == 0L, "and reports 0 recruited")

cat("\nAll report-data assertions passed.\n")
