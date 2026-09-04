# =============================================================================
# PANORAMA TMG report — screening funnel regression test
# =============================================================================
# The report template derives its own funnel from the raw export (it renders in
# a separate R session and cannot call the app's recruitment functions), so the
# same rule has to hold there:
#   screening_complete = 2                           -> screened
#   screening_complete = 2 AND consent_complete = 2  -> recruited
#   screening_complete = 2, consent_complete 0/blank -> screened only
#
# Evaluates the template's setup chunk against a fixture export, up to the
# funnel counts. Needs knitr only — run from the app directory:
#   Rscript tests/panorama_report_funnel.R
# Exits non-zero on the first failed assertion.
# =============================================================================

setwd(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE),
                                                value = TRUE))), ".."))

rmd <- "trials/panorama/reports/tmg_report.Rmd"
p   <- knitr::purl(rmd, output = tempfile(fileext = ".R"), quiet = TRUE,
                   documentation = 0L)
l <- readLines(p)
# purl emits the YAML defaults as `params <- list(...)`; skip it so the fixture
# params below are the ones the chunk reads.
start   <- grep("^knitr::opts_chunk", l)[1]
stop_at <- grep("^n_active\\s*<- length\\(active_ids\\)", l)[1]
if (is.na(start) || is.na(stop_at))
  stop("could not locate the setup chunk in ", rmd)
code <- parse(text = paste(l[start:stop_at], collapse = "\n"))

# 1 and 4 are recruited; 2 (consent 0) and 3 (consent blank) are screened only;
# 5 never finished screening.
raw <- data.frame(stringsAsFactors = FALSE,
  record_id            = c("1", "1", "2", "3", "4", "5"),
  redcap_event_name    = c("baseline_arm_1", "fu_7_day_arm_1", "baseline_arm_1",
                           "baseline_arm_1", "baseline_arm_1", "baseline_arm_1"),
  rc_site_name         = c("Site A", "Site A", "Site A", "Site B", "Site B", "Site B"),
  screening_calc       = c("4", "", "4", "4", "4", "2"),
  approached_yn        = c("1", "", "1", "1", "1", ""),
  valid_consent        = c("1", "", "", "", "", ""),
  screening_complete   = c("2", "", "2", "2", "2", "0"),
  consent_complete     = c("2", "", "0", "", "2", "0"),
  screen_created_date  = c("2026-08-01", "", "2026-08-02", "2026-08-03",
                           "2026-08-04", "2026-08-05"),
  index_discharge_date = c("2026-08-02", "", "", "", "2026-08-05", ""),
  withdraw_level       = c("", "", "", "", "", ""),
  age                  = c("55", "", "60", "61", "62", "63"),
  sex                  = c("1", "", "2", "1", "2", "1"),
  ethnicity            = c("1", "", "1", "1", "1", "1"),
  index_panc_aetio     = c("1", "", "2", "1", "2", "1"))

env <- new.env(parent = globalenv())
env$params <- list(
  report_data    = list(raw_df = raw),
  report_content = list(),
  work_package   = list(code = "WP4", label = "Cohort study", target = 1017L),
  column_labels  = list(),
  report_date    = format(Sys.Date(), "%d %B %Y"),
  selected_sites = "All sites",
  report_type    = "TMG")
eval(code, envir = env)

ok <- function(cond, what) {
  if (!isTRUE(cond)) { cat("FAIL:", what, "\n"); quit(status = 1L) }
  cat("  ✓", what, "\n")
}

ok(isTRUE(env$has_form_flags), "the report reads the form-completion flags")
ok(env$n_screened == 4L, "screened = screening_complete 2")
ok(identical(sort(env$cons_ids), c("1", "4")),
   "recruited = screening_complete 2 AND consent_complete 2")
ok(env$n_cons == 2L, "recruited count is 2, not 0")
ok(identical(sort(env$scr_only_ids), c("2", "3")),
   "consent 0 or blank counts as screened only")
ok(env$n_scr_only == 2L, "screened-only count is 2")
# n_cons is the "Expected" column of the CRF completion table and the
# denominator of every participant-level table on pages 3-5.
ok(env$n_cons > 0, "the CRF table's Expected column is non-zero")

cat("\nAll report funnel assertions passed.\n")
