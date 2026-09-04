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
  trial_type   = "observational",
  # No randomisation: participants are screened, consented and then followed
  # up. The dashboard reads this to label recruitment "registered", not
  # "randomised".
  recruitment_model = "registration",
  trial_target = 1017L,          # WP4 — the work package this dashboard tracks

  # ── Work packages ─────────────────────────────────────────────────────────
  # Each WP has its own design, target and outcome measures; the WP picker and
  # the TMG report read them from here (see wp_report_context()).
  work_packages = c("WKP1: Conceptual framework",
                    "WKP2: Patient experience interviews",
                    "WKP3: Clinician interviews",
                    "WKP4: Cohort study",
                    "WKP5: Implementation workshop"),
  work_package_targets = c(80L, 30L, 30L, 1017L, 30L),
  work_package_meta = list(
    list(label = "Conceptual framework",
         design = "Consensus development",
         interventions = "None — consensus exercise",
         outcomes = c("Conceptual framework for a gold-standard post-discharge pathway")),
    list(label = "Patient experience interviews",
         design = "Qualitative interview study",
         interventions = "None — interviews",
         outcomes = c("Patient experience of post-discharge care",
                      "Groups at highest risk of health service use (with WP4)")),
    list(label = "Clinician interviews",
         design = "Qualitative interview study",
         interventions = "None — interviews",
         outcomes = c("Clinician approaches to post-discharge care")),
    list(label = "Cohort study",
         design = "Prospective observational cohort",
         interventions = "None — standard care; no clinical intervention",
         follow_up_months = 6,
         outcomes = c(
           "Health resource utilisation in the 6 months after discharge",
           "Onset of anxiety or depression (GAD-7, PHQ-9)",
           "Healthcare costs related to acute pancreatitis",
           "Quality of life (EQ-5D-5L)",
           "Pancreatic exocrine insufficiency (PEI-Q)",
           "Return to work and normal activities")),
    list(label = "Implementation workshop",
         design = "Co-production workshop",
         interventions = "None — workshop",
         outcomes = c("Co-produced interventions for post-discharge care pathways"))),

  logo_file = file.path(.panorama_dir, "www", "logo.png"),
  colors = list(
    primary   = "#5D4A9C",
    secondary = "#A88BD4",
    accent    = "#C9A227"
  ),

  data_dir = NULL,

  # ── REDCap events ─────────────────────────────────────────────────────────
  # From the project's data dictionary codebook (PID 554). Screening, consent,
  # demographics, index admission and withdrawal all sit in baseline_arm_1;
  # each PROM timepoint has its own event. site_records_arm_2 holds the site
  # and investigator forms, not participants.
  redcap_events = list(
    baseline  = "baseline_arm_1",
    discharge = "day_of_discharge_arm_1",
    day_7     = "fu_7_day_arm_1",
    day_30    = "fu_30_day_arm_1",
    month_3   = "fu_3_month_arm_1",
    month_6   = "fu_6_month_arm_1",
    sub_forms = "site_records_arm_2"
  ),

  # ── REDCap field mappings ─────────────────────────────────────────────────
  redcap_fields = list(
    record_id              = "record_id",
    site_name              = "rc_site_name",          # DAG name; rc_site_id is its code
    randomisation_datetime = "screen_created_date",   # registration date
    discharge_date         = "index_discharge_date",

    age                    = "age",
    sex                    = "sex",
    ethnicity              = "ethnicity",             # eth_white / eth_asian etc. hold the detail

    # Screening / eligibility / consent
    # screening_calc = sum(screening_1..4); all four answered "eligible" = 4.
    screening_calc         = "screening_calc",        # 4 = eligible
    screening_date         = "screen_date",           # date the patient was screened
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

  # ── Recruitment model ─────────────────────────────────────────────────────
  # Participants are screened AND consented inside REDCap, so recruitment is
  # defined by the two form-completion flags together:
  #
  #   screening_complete = 2 AND consent_complete = 2  → recruited
  #   screening_complete = 2, consent_complete 0/blank → screened only
  #
  # WP4 screens far more patients than it consents; only the recruited count
  # goes towards the 1017 target, the rest are reported in the funnel.
  #
  # Both the recruitment and the screening event are `baseline_arm_1`. They
  # previously named `screening_arm_1`, which does not exist in this project —
  # every stage of the funnel filtered to zero rows and read 0.
  recruitment = list(
    model         = "registration",
    basis         = "all_conditions",
    event         = "baseline_arm_1",
    date_field    = "screen_created_date",
    # Kept for the views that still show a single consent flag.
    consent_field = "consent_complete",
    consent_value = "2",
    conditions = list(
      list(field = "screening_complete", value = "2"),
      list(field = "consent_complete",   value = "2")
    ),
    screening = list(
      enabled          = TRUE,
      event            = "baseline_arm_1",
      screened_field   = "screening_complete",
      screened_value   = "2",
      eligible_field   = "screening_calc",
      eligible_value   = "4",
      approached_field = "approached_yn",
      approached_value = "1"
    )
  ),

  # ── Follow-up schedule (days from discharge; protocol section 13.8) ───────
  # Windows are the protocol's: day 7 and 28 +/-3, 3 and 6 months +/-7. A
  # timepoint is only counted once its window has closed, so early months do
  # not read as 0% complete.
  timepoints = list(
    discharge = list(label = "Day of discharge", offset_days = 0,   window_days = 0,  anchor = "discharge"),
    day_7     = list(label = "Day 7",            offset_days = 7,   window_days = 3,  anchor = "discharge"),
    day_30    = list(label = "Day 28",           offset_days = 28,  window_days = 3,  anchor = "discharge"),
    month_3   = list(label = "3 months",         offset_days = 91,  window_days = 7,  anchor = "discharge"),
    month_6   = list(label = "6 months",         offset_days = 182, window_days = 7,  anchor = "discharge")
  ),

  # ── Withdrawal levels (withdraw_level, from the REDCap codebook) ──────────
  # The codebook carries three levels, not the two the protocol text describes.
  cos_type_labels = c(
    "1" = "No study intervention",
    "2" = "No study-related follow-up",
    "3" = "No further data collection"
  ),

  # Coded values come from the project codebook in trials/panorama/codebook.csv
  # and are applied through overrides.json (column_labels), so every view and
  # report reads labels rather than numbers. Re-import the codebook from Trial
  # Settings after a REDCap change.
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
    tonic = file.path(.panorama_dir, "reports", "tmg_report.Rmd")
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
