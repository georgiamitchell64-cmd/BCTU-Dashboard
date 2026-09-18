# =============================================================================
# TSC report sections — the data behind the Word tables
# =============================================================================
# Four faults the TSC report showed against a real TONIC export, each caused
# by the report reading something prepare_report_data() does not produce:
#
#   * Baseline characteristics vanished: baseline_table.R is sourced on its
#     own into the rendering session, where the app's fld() does not exist, so
#     every column lookup threw and the caller's tryCatch() turned the whole
#     table into nothing.
#   * Protocol deviations read 0: the template looked for rd$deviations, while
#     the extractor fills rd$deviation_log / rd$deviation_count — and the
#     deviation type stayed a REDCap code instead of its codebook label.
#   * Recruitment per site listed every site in the register with no figures.
#   * Open centres counted sites typed into the Sites tab during set-up, which
#     the export has never heard of.
#
# Needs the app's runtime packages; skips cleanly without them.
#   Rscript tests/tsc_report_sections.R
# =============================================================================

# Rscript passes spaces in the script's path as "~+~" on Windows.
.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
.this <- gsub("~+~", " ", .this, fixed = TRUE)
setwd(file.path(dirname(.this), ".."))

need <- c("dplyr", "tidyr", "stringr", "lubridate", "tibble", "rlang")
absent <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(absent)) {
  cat("SKIP: needs", paste(absent, collapse = ", "), "\n"); quit(status = 0L)
}
suppressPackageStartupMessages(for (p in need) library(p, character.only = TRUE))

for (f in c("globals/paths.R", "globals/constants.R", "globals/datasets.R",
            "globals/trial_config.R", "functions/helpers.R",
            "functions/trial_overrides.R", "functions/recruitment.R",
            "functions/participant_breakdowns.R", "functions/safety_events.R",
            "functions/prepare_report_data.R", "functions/consort_flow.R",
            "functions/baseline_table.R", "functions/tsc_charts.R"))
  source(f)

cfg <- discover_trials()[["tonic"]]
if (is.null(cfg)) { cat("SKIP: no tonic trial config\n"); quit(status = 0L) }
apply_trial_globals(cfg)

ok <- function(cond, what) {
  if (!isTRUE(cond)) { cat("FAIL:", what, "\n"); quit(status = 1L) }
  cat("  ✓", what, "\n")
}

# ── A TONIC-shaped export: 12 consented records, 9 of them randomised, three
#    deviations on the ad hoc event, across three sites. ───────────────────
sites <- c("Site A", "Site B", "Site C")
n     <- 12L
ids   <- as.character(seq_len(n))
rand  <- ifelse(seq_len(n) <= 9,
                format(as.Date("2026-05-01") + seq_len(n) * 3, "%Y-%m-%d 09:30"), "")
base <- data.frame(stringsAsFactors = FALSE,
  record_id = ids, redcap_event_name = "baseline_arm_1",
  site_name = sites[c(1, 1, 1, 2, 2, 2, 2, 3, 1, 2, 1, 3)],
  rand_dttm_s = rand,
  cae_age = as.character(rep(c(58, 72, 81, 64), 3)),
  base_sex = as.character(rep(c(1, 2), 6)),
  base_ethnic_gp = as.character(rep(c(13, 1, 7, 19), 3)),
  base_nela_score_mort = as.character(rep(c(2.5, 7.1, 12.4, 4.9), 3)),
  base_residence = as.character(rep(c(1, 1, 3, 2), 3)),
  nut_b_nrs_group = rep(c("0-3 Low risk", "4 At risk", "5-7 High risk", "0-3 Low risk"), 3),
  nut_b_must_score = as.character(rep(c(1, 2, 3, 1), 3)),
  consent_eligibility_complete = "2",
  randomisation_complete = ifelse(nzchar(rand), "2", "0"),
  iop_op_end_dt = ifelse(nzchar(rand),
                         format(as.Date("2026-05-05") + seq_len(n) * 3, "%Y-%m-%d"), ""))
dev <- base[0, ]
dev[1:3, ] <- NA
# The third sits on a participant who consented but was never randomised —
# the case a window-scoped lookup used to drop.
dev$record_id <- c("1", "2", "11")
dev$redcap_event_name <- "ad_hoc_arm_1"
dev$site_name <- sites[c(1, 1, 1)]
for (col in setdiff(names(dev), c("record_id", "redcap_event_name", "site_name")))
  dev[[col]] <- ""
export <- rbind(base, dev)
export$dev_ref  <- c(rep("", n), "DEV-001", "DEV-002", "DEV-003")
export$dev_dt   <- c(rep("", n), "2026-05-20", "2026-06-02", "2026-06-18")
export$dev_summary <- c(rep("", n), "Visit window missed", "Consent version",
                        "Late SAE report")
export$dev_type <- c(rep("", n), "1", "5", "1")
export$dev_capa <- c(rep("", n), "Retraining", "Re-consented", "Reminder issued")
export$deviation_complete <- c(rep("", n), "2", "2", "2")

# The Sites register: three sites synced from the export, two typed in by hand
# during set-up that the export has never heard of.
register <- data.frame(stringsAsFactors = FALSE,
  site_name      = c(sites, "Set-up Only A", "Set-up Only B"),
  status         = c("Recruiting", "Recruiting", "Open", "Set-up", "Set-up"),
  site_open_date = c("2026-04-01", "2026-04-15", "2026-05-01", "2026-06-22", "2026-06-22"),
  monthly_target = 2L, target = 42L, randomised = 0L,
  source         = c("auto", "auto", "auto", "manual", "manual"))

rd <- suppressMessages(prepare_report_data(export, pipeline_df = register))

# ── 1. Baseline characteristics, built the way the Rmd builds them ─────────
# Sourced on its own in a session that has none of the app loaded — the case
# that used to throw "could not find function fld".
rds <- tempfile(fileext = ".rds"); saveRDS(rd, rds)
probe <- tempfile(fileext = ".R")
writeLines(c(
  'args <- commandArgs(TRUE)',
  'setwd(args[2])',
  'source("functions/baseline_table.R")',
  'bl <- baseline_characteristics_df(readRDS(args[1]))',
  'cat(nrow(bl), sum(grepl("Female", bl$sublabel)), "\n")'), probe)
out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                c(shQuote(probe), shQuote(rds), shQuote(getwd())),
                                stdout = TRUE, stderr = TRUE))
parsed <- suppressWarnings(as.integer(strsplit(trimws(tail(out, 1)), "\\s+")[[1]]))
ok(length(parsed) == 2 && !any(is.na(parsed)) && parsed[1] > 10,
   paste0("baseline table builds without the app loaded (",
          paste(out, collapse = " | "), ")"))
ok(parsed[2] == 1, "it reaches the demographic rows, not just the minimisation block")

bl <- baseline_characteristics_df(rd)
ok(any(bl$sublabel == "Male") && any(bl$sublabel == "Female"),
   "coded sex reads as words")
ok(sum(bl$section == "Minimisation variables") > 0,
   "minimisation variables are present")

# ── 2. Protocol deviations ────────────────────────────────────────────────
ok(identical(as.integer(rd$deviation_count), 3L),
   "the three deviations in the export are counted")
ok(isTRUE(rd$deviation_available), "the export is recognised as carrying the form")
ok("category" %in% names(rd$deviation_log),
   "the deviation log carries the deviation type")
ok(all(rd$deviation_log$category %in%
       c("Non-compliance with the trial protocol", "Data Management")),
   "deviation types read as codebook labels, not REDCap codes")
ok(all(nzchar(rd$deviation_log$action)),
   "the corrective action comes through")
ok("11" %in% rd$deviation_log$record_id,
   "a deviation on a consented-but-not-randomised record is still counted")

rd_site_b <- suppressMessages(prepare_report_data(export, pipeline_df = register,
                                                  selected_sites = "Site B"))
ok(identical(as.integer(rd_site_b$deviation_count), 0L),
   "a report scoped to one site does not show another site's deviations")

# ── 3. Recruitment per site ───────────────────────────────────────────────
os <- rd$open_sites
ok(nrow(os) == 3, "only the three recruiting sites from the export are listed")
ok(!any(grepl("Set-up Only", os$site_name)),
   "sites typed into the register but absent from the export are left out")
ok("first_rand_date" %in% names(os) && all(!is.na(os$first_rand_date)),
   "each site carries the date of its first randomisation")
ok(sum(os$randomisations) == rd$kpis$total_randomised,
   "the per-site counts add up to the trial total")

# ── 4. Open centres over time ─────────────────────────────────────────────
st <- rd$site_status
ok("in_redcap" %in% names(st), "the site table says which sites the export knows")
ok(all(st$in_redcap[st$site_name %in% sites]),
   "every site in the export is flagged as imported")
ok(!any(st$in_redcap[grepl("Set-up Only", st$site_name)]),
   "hand-typed set-up sites are not")
ok(nrow(.tsc_open_sites_rows(st)) == 3,
   "the open-centres chart counts only the imported sites")

# ── 5. CONSORT: consented but not randomised ──────────────────────────────
ok(identical(as.integer(rd$kpis$total_consented), 12L),
   "every consented record is counted")
ok(identical(as.integer(rd$kpis$consented_not_randomised), 3L),
   "the three consented-but-not-randomised participants are counted")
counts <- consort_counts(rd)
ok(counts$consented_not_rand == 3L && counts$consented == 12L,
   "the CONSORT stage picks those counts up")
ok(counts$screened == counts$excluded + counts$consented,
   "the enrolment total is consistent with the new stage")
if (requireNamespace("consort", quietly = TRUE)) {
  png <- tempfile(fileext = ".png")
  res <- tryCatch({ consort_png(counts, filepath = png, width = 6.5, height = 8); TRUE },
                  error = function(e) conditionMessage(e))
  ok(isTRUE(res) && file.exists(png) && file.info(png)$size > 0,
     paste("the diagram renders with the extra stage", if (!isTRUE(res)) res else ""))
}

cat("PASS: tests/tsc_report_sections.R\n")
