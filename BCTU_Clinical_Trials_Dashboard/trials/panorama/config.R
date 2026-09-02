# =============================================================================
# Trial Configuration: PANORAMA (Work Package 4 — cohort study)
# =============================================================================
# oPtimising post-dischArge care pathways after acute paNcreatitis:
# evaluatiOn of health seRvice utilisAtion, outcoMes
#
# WP4 is an observational cohort: participants are screened, approached and
# consented before discharge, then followed up remotely with PROMs at day of
# discharge, day 7, day 28, 3 months and 6 months. There is no randomisation,
# so `randomisation_datetime` is mapped to the REDCap record-creation date and
# read throughout the dashboard as the registration date.
# =============================================================================

# Folder for this trial; discover_trials() sources this file with the app root
# as the working directory.
.panorama_dir <- file.path(getwd(), "trials", "panorama")

trial_config <- list(

  code         = "panorama",
  name         = "PANORAMA — post-discharge care pathways after acute pancreatitis",
  short_name   = "PANORAMA",
  work_package = "WP4",
  trial_target = 1017L,

  logo_file = file.path(.panorama_dir, "www", "logo.png"),
  colors = list(
    primary   = "#5D4A9C",
    secondary = "#A88BD4",
    accent    = "#C9A227"
  ),

  data_dir = NULL,

  # ── REDCap events ─────────────────────────────────────────────────────────
  # Baseline lists the candidate names for the registration/index event: the
  # dashboard matches with %in%, so trim this to the real event name once the
  # first export is loaded.
  redcap_events = list(
    baseline  = c("screening_arm_1", "baseline_arm_1", "index_admission_arm_1",
                  "registration_arm_1", "enrolment_arm_1"),
    discharge = c("discharge_arm_1", "day_of_discharge_arm_1"),
    day_7     = "day_7_arm_1",
    day_30    = "fu_30_day_arm_1",
    month_3   = "fu_3_month_arm_1",
    month_6   = "fu_6_month_arm_1",
    sub_forms = c("withdrawal_arm_1", "ad_hoc_arm_1")
  ),

  # ── REDCap field mappings ─────────────────────────────────────────────────
  redcap_fields = list(
    record_id              = "record_id",
    site_name              = "redcap_data_access_group",
    randomisation_datetime = "screen_created_date",   # registration date
    discharge_date         = "index_discharge_date",

    age                    = "age",

    # Screening / eligibility / consent
    screening_calc         = "screening_calc",        # 4 = eligible
    approached             = "approached_yn",
    valid_consent          = "valid_consent",         # 1 = consented
    screen_created_date    = "screen_created_date",
    aetiology              = "index_panc_aetio",

    # Form completion flags
    screening_complete     = "screening_complete",
    consent_complete       = "consent_complete",
    demographics_complete  = "demographics_past_history_complete",
    index_admission_complete = "index_admission_complete",

    # PROM completion flags
    follow_up_instruments = list(
      discharge_proms = "day_of_discharge_proms_complete",
      day7_proms      = "day7_proms_complete",
      month_proms     = "month_proms_complete"
    ),

    # Withdrawal — any non-blank value means withdrawn
    cos_type                  = "withdraw_level",
    change_of_status_complete = NULL,
    change_of_status_date     = NULL
  ),

  # ── Withdrawal levels (protocol section 15) ───────────────────────────────
  cos_type_labels = c(
    "1" = "No study-related follow-up",
    "2" = "No further data collection"
  ),

  ethnicity_labels = NULL,
  target_schedule  = NULL,
  participant_table_layout = NULL,

  # ── Report defaults ───────────────────────────────────────────────────────
  report_defaults = list(
    ci          = "Mr Matthew Lee",
    sponsor     = "University of Birmingham",
    sponsor_ref = "RG_25-105",
    funder      = "NIHR Health Services & Delivery Research",
    funder_ref  = "NIHR165210",
    isrctn      = "",
    iras        = "349680"
  ),

  # ── Report content (cover panel + TMG template defaults) ──────────────────
  report_content = list(
    short_name        = "PANORAMA",
    trial_title       = "oPtimising post-dischArge care pathways after acute paNcreatitis: evaluatiOn of health seRvice utilisAtion, outcoMes",
    trial_subtitle    = "Work Package 4 — prospective cohort study",
    registration_line = "IRAS 349680 · CPMS 64883 · Sponsor RG_25-105"
  ),

  # ── Report templates ──────────────────────────────────────────────────────
  # Pin the TMG/iTMG template to this trial's lilac WP4 layout so a newer
  # canonical tonic_report.Rmd at the project root can never take precedence.
  report_template_paths = list(
    tonic = file.path(.panorama_dir, "reports", "tonic_report.Rmd")
  ),

  # ── Feature flags ─────────────────────────────────────────────────────────
  features = list(
    postal_tracking  = TRUE,
    return_rates     = FALSE,
    projections      = TRUE,
    pilot_criteria   = FALSE,
    consort_flow     = TRUE,
    baseline_table   = TRUE
  )
)
