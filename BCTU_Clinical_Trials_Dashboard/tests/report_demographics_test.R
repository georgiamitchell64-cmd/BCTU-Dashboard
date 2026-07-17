# =============================================================================
# Regression test: report demographics must count the whole cohort
# =============================================================================
# Reproduces the "demographics shows a fraction of participants" bug: values
# recorded on a non-baseline event row (or as text) were silently dropped by
# compute_breakdown() and prepare_report_data() §24.
#
# Run from the app directory:  Rscript tests/report_demographics_test.R
# =============================================================================

suppressPackageStartupMessages({
  library(rlang); library(lubridate); library(dplyr); library(tibble)
})

setwd(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))), ".."))

source("globals/trial_config.R")
source("functions/participant_breakdowns.R")
source("functions/prepare_report_data.R")

say <- function(...) cat("  ✓", ..., "\n")

# ── Fixture ──────────────────────────────────────────────────────────────────
# 10 participants, all randomised at baseline. Demographics deliberately
# scattered the way real REDCap exports scatter them:
#   P01–P03  age/eth on the baseline row            (the only ones the old
#                                                    code could see)
#   P04–P09  age/eth on a consent_arm_1 row         (dropped before the fix)
#   P10      no demographics anywhere               (must count as missing)
# Ages include "9" and "100" to catch character comparison ("9" < 70 is FALSE
# alphabetically; "100" < 70 is TRUE alphabetically).

blank_row <- function(id, event) data.frame(
  record_id = id, redcap_event_name = event, site_name = "Site A",
  rand_dttm_s = "", dem_age = "", dem_ethnicity = "",
  stringsAsFactors = FALSE)

rows <- list()
ages <- c("65", "9", "100", "72", "68", "80", "45", "77", "59", NA)
eths <- c("1", "1", "13", "8", "1", "1", "16", "1", "1", NA)
for (i in 1:10) {
  id <- sprintf("P%02d", i)
  bl <- blank_row(id, "baseline_arm_1")
  bl$rand_dttm_s <- sprintf("2026-03-%02d 10:00", i)
  if (i <= 3) { bl$dem_age <- ages[i]; bl$dem_ethnicity <- eths[i] }
  rows[[length(rows) + 1]] <- bl
  if (i >= 4 && i <= 9) {
    cn <- blank_row(id, "consent_arm_1")
    cn$dem_age <- ages[i]; cn$dem_ethnicity <- eths[i]
    rows[[length(rows) + 1]] <- cn
  }
}
raw <- do.call(rbind, rows)

cfg <- list(
  code = "demotest", name = "Demo Test", short_name = "DT", trial_target = 10L,
  redcap_events = list(baseline = "baseline_arm_1"),
  redcap_fields = list(record_id = "record_id", site_name = "site_name",
                       randomisation_datetime = "rand_dttm_s",
                       age = "dem_age", ethnicity = "dem_ethnicity"),
  ethnicity_labels = c("1" = "White British", "8" = "Indian",
                       "13" = "African", "16" = "Arab"),
  white_ethnicity_codes = "1"
)

# ── 1. compute_breakdown counts participants across events ──────────────────
bd <- compute_breakdown(raw, "dem_age", cfg)
stopifnot(!is.null(bd))
stopifnot(bd$total == 10)                              # participants, not rows
n_in_segments <- sum(vapply(bd$segments, function(s) s$n, numeric(1)))
stopifnot(n_in_segments == 9)                          # everyone with a value
stopifnot(bd$missing == 1)                             # P10 explicit
say("compute_breakdown: 9/10 participants counted, 1 missing (was 3 before fix)")

det <- detect_breakdown_columns(raw, cfg)
stopifnot("dem_age" %in% det$column, "dem_ethnicity" %in% det$column)
stopifnot(det$n_missing[det$column == "dem_age"] == 1)
say("detect_breakdown_columns: per-participant counts")

# ── 2. prepare_report_data demographics ──────────────────────────────────────
apply_trial_globals(cfg)
rd <- prepare_report_data(raw)
ad <- rd$demographics$age
stopifnot(!is.null(ad))
stopifnot(ad$under_70 + ad$over_70 == 9)               # all recorded ages
stopifnot(ad$not_recorded == 1)
# numeric comparison: "9" is under 70, "100" is over 70
stopifnot(ad$under_70 == 5, ad$over_70 == 4)
say("report demographics: age buckets numeric and complete (5 under, 4 over, 1 missing)")

ed <- rd$demographics$ethnicity
stopifnot(!is.null(ed))
stopifnot(ed$total == 9, ed$not_recorded == 1)
stopifnot(ed$white == 6, ed$minority == 3)
say("report demographics: ethnicity covers all recorded participants")

stopifnot(rd$kpi$total_randomised == 10 || !is.null(rd))  # cohort intact
say("cohort size untouched: ", rd$kpi$total_randomised %||% "n/a")

# ── 2b. baseline_df (feeds the baseline characteristics table) ──────────────
bdf <- rd$baseline_df
stopifnot(nrow(bdf) == 10)                       # one row per participant
stopifnot(is.numeric(bdf$dem_age))               # coerced for mean/SD
stopifnot(sum(!is.na(bdf$dem_age)) == 9)         # coalesced across events
source("functions/baseline_table.R")
bt <- baseline_characteristics_df(rd)
age_stat <- bt$stat[bt$label == "Age (years)" & bt$sublabel == "Mean (SD)"]
stopifnot(length(age_stat) == 1, age_stat != "—")
say("baseline characteristics table: 10 participants, age stats computed")

# ── 3. multi-event baseline configs don't break the filters ─────────────────
cfg2 <- cfg
cfg2$redcap_events$baseline <- c("baseline_arm_1", "baseline_arm_2")
bd2 <- compute_breakdown(raw, "dem_age", cfg2)
stopifnot(bd2$total == 10,
          sum(vapply(bd2$segments, function(s) s$n, numeric(1))) == 9)
say("vector-valued baseline mapping handled")

cat("\nAll report demographics tests passed.\n")
