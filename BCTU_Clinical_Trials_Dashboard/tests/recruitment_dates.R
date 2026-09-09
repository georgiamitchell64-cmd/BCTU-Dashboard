# =============================================================================
# Recruitment dates — regression test
# =============================================================================
# The PANORAMA export gained two dates: screen_date (screening) and
# pat_sig_date (consent signature). Recruitment is consent, so the recruitment
# date is pat_sig_date; screen_created_date and screen_date stay as fallbacks
# for an export taken before it was collected. This pins:
#   * a role mapped to several candidate names resolves to whichever the
#     export has, rather than always the first
#   * dd/mm/yyyy parses as dd/mm/yyyy, not as the year 20
#
# Base R only — run from the app directory:  Rscript tests/recruitment_dates.R
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
                          id_col = NULL) raw

source("trials/panorama/config.R")
cfg <- trial_config
current_trial_config <- function() cfg
source("functions/recruitment.R")

# fld_present() and parse_redcap_date() come from the app's own globals.
source("globals/trial_config.R")

ok <- function(cond, what) {
  if (!isTRUE(cond)) { cat("FAIL:", what, "\n"); quit(status = 1L) }
  cat("  ✓", what, "\n")
}

spec <- recruitment_spec(cfg)
ok("pat_sig_date" %in% spec$date_candidates,
   "the consent signature date is a recruitment-date candidate")
ok(length(spec$date_candidates) > 1,
   "the older screening dates stay as fallbacks")

new_export <- data.frame(stringsAsFactors = FALSE,
  record_id    = c("1", "2"),
  screen_date  = c("2026-08-01", "2026-08-02"),
  pat_sig_date = c("2026-08-03", ""))
old_export <- data.frame(stringsAsFactors = FALSE,
  record_id           = c("1", "2"),
  screen_created_date = c("2026-08-01", "2026-08-02"))

ok(identical(fld_present("randomisation_datetime", new_export, cfg = cfg),
             "pat_sig_date"),
   "an export with pat_sig_date dates recruitment by the consent signature")
ok(identical(fld_present("randomisation_datetime", old_export, cfg = cfg),
             "screen_created_date"),
   "an export without it falls back to the date it does carry")
ok(is.null(fld_present("randomisation_datetime",
                       data.frame(record_id = "1"), cfg = cfg)),
   "an export with no recruitment date at all resolves to nothing")

v <- .rec_values(new_export, spec$date_candidates)
ok(identical(unname(v["1"]), "2026-08-03"),
   "the recruitment date is read from the candidate the export has")

d <- parse_redcap_date(c("2026-08-01", "20/04/2026", "", NA))
ok(identical(format(d[1], "%Y-%m-%d"), "2026-08-01"), "ISO dates parse")
ok(identical(format(d[2], "%Y-%m-%d"), "2026-04-20"),
   "dd/mm/yyyy parses as 20 April 2026, not the year 20")
ok(all(is.na(d[3:4])), "blank and NA stay NA")

cat("\nAll recruitment-date assertions passed.\n")
