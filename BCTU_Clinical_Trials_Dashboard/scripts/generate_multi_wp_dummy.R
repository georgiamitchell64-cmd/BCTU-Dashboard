# =============================================================================
# Generate a multi-work-package dummy trial for testing
# =============================================================================
# Usage (from the BCTU_Clinical_Trials_Dashboard root):
#   Rscript scripts/generate_multi_wp_dummy.R
#
# Creates:
#   trials/multiwp/config.R     — trial config with 3 work packages
#   trials/multiwp/data/MULTIWP_synthetic_<date>.csv
#   trials/multiwp/www/         — empty (no logo)
#   trials/multiwp/reports/     — empty (will be seeded on first selection)
#
# Re-run anytime for a fresh CSV. Each run writes a new dated file; the
# dashboard's find_latest_csv() picks the newest.
# =============================================================================

set.seed(42)

# ── Knobs ────────────────────────────────────────────────────────────────────
TRIAL_CODE <- "multiwp"
TRIAL_NAME <- "MULTIWP — Multi-work-package Test Trial"
TRIAL_SHORT <- "MULTIWP"
N_BY_WP <- c("WKP1: Surgery cohort"      = 40,
             "WKP2: Recovery cohort"     = 30,
             "WKP3: Long-term follow-up" = 22)   # 92 participants total
SITES <- c("QE Birmingham", "Manchester Royal Infirmary",
           "Leeds Teaching Hospitals", "Bristol Royal Infirmary",
           "Oxford University Hospitals", "Royal London", "Edinburgh Royal")
# Realistic-looking start: ramp begins April 2025, runs ~14 months
START_DATE <- as.Date("2025-04-01")
RAMP_MONTHS <- 14L

# ── Layout helpers ───────────────────────────────────────────────────────────
TRIAL_DIR <- file.path(getwd(), "trials", TRIAL_CODE)
for (sub in c("data", "www", "reports"))
  dir.create(file.path(TRIAL_DIR, sub), recursive = TRUE, showWarnings = FALSE)

# ── 1. Per-participant baseline metadata ─────────────────────────────────────
n_total  <- sum(N_BY_WP)
wp_codes <- rep(seq_along(N_BY_WP), N_BY_WP)
wp_names <- rep(names(N_BY_WP),     N_BY_WP)

# Add N months to a Date — small helper since lubridate may not be loaded
# in this script's session.
add_months <- function(date, m) {
  out <- as.POSIXlt(date)
  out$mon <- out$mon + m
  as.Date(out)
}

# Spread randomisation dates so each WP has a believable distribution.
rand_dates <- do.call(c, lapply(seq_along(N_BY_WP), function(i) {
  n <- N_BY_WP[[i]]
  # WP1 ramps fastest, WP3 starts later
  offset_months <- c(0, 1, 4)[i]
  duration <- c(11, 9, 8)[i]
  sample(seq.Date(
    from = add_months(START_DATE, offset_months),
    by   = "day",
    length.out = duration * 30L
  ), n, replace = TRUE)
}))

age_pool <- 40:88
sex_pool <- c(1L, 2L)              # 1 = Male, 2 = Female
eth_pool <- c(rep(1L, 18), 8L, 13L, 16L, 17L, 18L)  # mostly white-british, some variety
nela_pool <- 0:9

participants <- data.frame(
  record_id    = sprintf("MULTI-%03d", seq_len(n_total)),
  work_package = wp_codes,
  wp_name      = wp_names,
  site_name    = sample(SITES, n_total, replace = TRUE),
  rand_dttm_s  = format(rand_dates, "%Y-%m-%d %H:%M"),
  surgery_date = format(rand_dates + sample(1:5, n_total, replace = TRUE), "%Y-%m-%d"),
  discharge_dt = format(rand_dates + sample(6:18, n_total, replace = TRUE), "%Y-%m-%d"),
  dem_age      = sample(age_pool, n_total, replace = TRUE),
  dem_sex      = sample(sex_pool, n_total, replace = TRUE, prob = c(.55, .45)),
  dem_ethnicity = sample(eth_pool, n_total, replace = TRUE),
  nela_score   = sample(nela_pool, n_total, replace = TRUE),
  stringsAsFactors = FALSE
)

# 6% withdraw / lose to follow-up
withdrawn  <- sample(seq_len(n_total), round(n_total * 0.06))
cos_type   <- rep(NA_integer_, n_total)
cos_type[withdrawn] <- sample(1:5, length(withdrawn), replace = TRUE)
participants$cos_type <- cos_type

# Follow-up completion (1 = incomplete, 2 = complete, NA = not started)
# 80% complete each instrument, 12% started, 8% missing
rand_complete <- function(n) sample(c(2L, 1L, NA_integer_),
                                    n, replace = TRUE,
                                    prob = c(.80, .12, .08))
participants$cci_complete     <- rand_complete(n_total)
participants$eq5d_complete    <- rand_complete(n_total)
participants$clavien_complete <- rand_complete(n_total)

# ── 2. Build long-format REDCap-style rows ───────────────────────────────────
# Each participant gets a baseline row + a discharge row, plus a sub_forms row
# for any withdrawn participants (carries cos_type).

mk_blank_row <- function(record_id, event, site, wp, wp_label) {
  data.frame(
    record_id        = record_id,
    redcap_event_name = event,
    site_name        = site,
    work_package     = wp,
    work_package_name = wp_label,
    rand_dttm_s      = NA_character_,
    surgery_date     = NA_character_,
    discharge_dt     = NA_character_,
    dem_age          = NA_integer_,
    dem_sex          = NA_integer_,
    dem_ethnicity    = NA_integer_,
    nela_score       = NA_integer_,
    cci_complete     = NA_integer_,
    eq5d_complete    = NA_integer_,
    clavien_complete = NA_integer_,
    cos_type         = NA_integer_,
    stringsAsFactors = FALSE
  )
}

rows <- list()
for (i in seq_len(n_total)) {
  p <- participants[i, ]

  # — Baseline event: identity + demographics + randomisation date
  base <- mk_blank_row(p$record_id, "baseline_arm_1",
                       p$site_name, p$work_package, p$wp_name)
  base$rand_dttm_s   <- p$rand_dttm_s
  base$dem_age       <- p$dem_age
  base$dem_sex       <- p$dem_sex
  base$dem_ethnicity <- p$dem_ethnicity
  base$nela_score    <- p$nela_score
  rows[[length(rows) + 1]] <- base

  # — Discharge event: surgery / discharge dates + follow-up completion fields
  disc <- mk_blank_row(p$record_id, "discharge_arm_1",
                       p$site_name, p$work_package, p$wp_name)
  disc$surgery_date     <- p$surgery_date
  disc$discharge_dt     <- p$discharge_dt
  disc$cci_complete     <- p$cci_complete
  disc$eq5d_complete    <- p$eq5d_complete
  disc$clavien_complete <- p$clavien_complete
  rows[[length(rows) + 1]] <- disc

  # — Sub-forms event (only when cos_type is set)
  if (!is.na(p$cos_type)) {
    sub <- mk_blank_row(p$record_id, "sub_forms_arm_1",
                        p$site_name, p$work_package, p$wp_name)
    sub$cos_type <- p$cos_type
    rows[[length(rows) + 1]] <- sub
  }
}
csv_df <- do.call(rbind, rows)

# ── 3. Write CSV ─────────────────────────────────────────────────────────────
csv_path <- file.path(TRIAL_DIR, "data",
                      sprintf("MULTIWP_synthetic_%s.csv",
                              format(Sys.Date(), "%Y-%m-%d")))
write.csv(csv_df, csv_path, row.names = FALSE, na = "")

# ── 4. Write config.R ────────────────────────────────────────────────────────
cfg_text <- sprintf('# ===========================================================================
# Trial Configuration: %s
# ===========================================================================
# Auto-generated by scripts/generate_multi_wp_dummy.R
# Re-run that script anytime to regenerate the dummy CSV.
# ===========================================================================

trial_config <- list(

  # -- Identity --
  code         = "%s",
  name         = "%s",
  short_name   = "%s",
  trial_target = %dL,
  category     = "Surgery",

  # -- Design --
  trial_type    = "platform",   # randomised / single_blind / double_blind / observational / single_arm / platform / other
  work_packages = c(%s),

  # -- Branding --
  logo_file = NULL,
  colors = list(
    primary   = "#312E81",
    secondary = "#8B5CF6",
    accent    = "#F59E0B"
  ),

  # -- Data paths --
  data_dir = NULL,    # uses trials/<code>/data/
  return_rates_dir = NULL,

  # -- REDCap events --
  redcap_events = list(
    baseline  = "baseline_arm_1",
    discharge = "discharge_arm_1",
    day_30    = NULL,
    day_90    = NULL,
    sub_forms = c("sub_forms_arm_1")
  ),

  # -- REDCap field mappings --
  redcap_fields = list(
    record_id               = "record_id",
    site_name               = "site_name",
    randomisation_datetime  = "rand_dttm_s",

    operation_date          = "surgery_date",
    operation_datetime      = NULL,
    discharge_date          = "discharge_dt",
    age                     = "dem_age",
    sex                     = "dem_sex",
    ethnicity               = "dem_ethnicity",

    follow_up_instruments = list(
      cci     = "cci_complete",
      eq5d    = "eq5d_complete",
      clavien = "clavien_complete"
    ),
    cos_type              = "cos_type"
  ),

  # -- COS type labels (standard defaults) --
  cos_type_labels = c(
    "1" = "Death", "2" = "No Operation", "3" = "Part withdrawal",
    "4" = "Complete withdrawal", "5" = "Lost to follow-up"
  ),

  ethnicity_labels       = NULL,
  white_ethnicity_codes  = NULL,
  target_schedule        = NULL,
  participant_table_layout = NULL,

  projection_defaults = list(
    rate_central = 3.0, rate_optimistic = 4.0, rate_pessimistic = 2.0,
    sites_central = 2.0, sites_optimistic = 3.0, sites_pessimistic = 1.0,
    target_sites = 24
  ),

  report_defaults = list(
    ci      = "Test CI",
    sponsor = "University of Birmingham"
  ),

  # -- Feature flags --
  features = list(
    postal_tracking            = FALSE,
    return_rates               = FALSE,
    projections                = TRUE,
    pilot_criteria             = FALSE,
    consort_flow               = FALSE,
    baseline_table             = TRUE,
    participant_questionnaires = TRUE
  )
)
',
  toupper(TRIAL_CODE),
  TRIAL_CODE,
  gsub('"', '\\\\"', TRIAL_NAME),
  TRIAL_SHORT,
  sum(N_BY_WP),
  paste(sprintf('"%s"', names(N_BY_WP)), collapse = ", ")
)

writeLines(cfg_text, file.path(TRIAL_DIR, "config.R"))

# ── 5. Optional sample portfolio_review override (matches a 14-month ramp) ───
# Pre-fills the Portfolio review chart with rough projections so you can see
# the chart populated immediately without importing an Excel.
overrides <- list(
  portfolio_review = list(
    n_months    = 18L,
    start_month = format(START_DATE, "%Y-%m-%d"),
    excel_path  = "",
    series      = list(
      proj_pts_monthly = c(2L, 4L, 6L, 7L, 8L, 9L, 9L, 9L, 9L, 9L, 8L, 6L, 4L, 2L,
                            0L, 0L, 0L, 0L),
      proj_pts_cum     = cumsum(c(2, 4, 6, 7, 8, 9, 9, 9, 9, 9, 8, 6, 4, 2, 0, 0, 0, 0)),
      proj_sites_cum   = c(1L, 2L, 3L, 4L, 5L, 5L, 6L, 6L, 7L, 7L, 7L, 7L, 7L, 7L,
                            7L, 7L, 7L, 7L)
    )
  )
)
jsonlite::write_json(overrides, file.path(TRIAL_DIR, "overrides.json"),
                     auto_unbox = TRUE, pretty = TRUE)

# ── 6. Summary ───────────────────────────────────────────────────────────────
cat("──────────────────────────────────────────────────────────────────\n")
cat(sprintf("Created multi-WP test trial '%s'\n", TRIAL_CODE))
cat("──────────────────────────────────────────────────────────────────\n")
cat(sprintf("  Folder:           %s\n", TRIAL_DIR))
cat(sprintf("  Config:           %s\n", file.path(TRIAL_DIR, "config.R")))
cat(sprintf("  CSV:              %s\n", csv_path))
cat(sprintf("  Participants:     %d total\n", n_total))
for (i in seq_along(N_BY_WP))
  cat(sprintf("                    WP%d %-30s %d\n", i, names(N_BY_WP)[i], N_BY_WP[[i]]))
cat(sprintf("  Sites used:       %d\n", length(SITES)))
cat(sprintf("  Date range:       %s → %s\n",
            min(format(rand_dates, "%b %Y")),
            max(format(rand_dates, "%b %Y"))))
cat(sprintf("  Withdrawals:      %d (cos_type set)\n", length(withdrawn)))
cat(sprintf("  CSV rows:         %d\n", nrow(csv_df)))
cat("──────────────────────────────────────────────────────────────────\n")
cat("Restart the dashboard. The 'multiwp' trial will appear on the\n")
cat("home selector with its WP picker bar at the top of the dashboard.\n")
