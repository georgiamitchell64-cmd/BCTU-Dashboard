# =============================================================================
# Trial Configuration: TONIC
# =============================================================================
# A randomised trial comparing early parenteral nutrition vs standard
# nutritional care in adults undergoing emergency laparotomy.
# ISRCTN59200516  |  NIHR HTA ref NIHR155875  |  CI: Mr Matthew Lee
# =============================================================================

trial_config <- list(

  # ── Identity ───────────────────────────────────────────────────────────────
  code         = "tonic",
  name         = "TONIC — Early Parenteral Nutrition vs Standard Care",
  short_name   = "TONIC",
  trial_target = 898L,
  category     = "Surgery",

  # ── Branding ───────────────────────────────────────────────────────────────
  logo_file = "trials/tonic/www/logo.png",
  colors = list(
    primary   = "#1B4F6B",
    secondary = "#2EC4A5",
    accent    = "#F59E0B"
  ),

  # ── Data source ────────────────────────────────────────────────────────────
  # Network drive path — set to NULL to use trials/tonic/data/ instead
  data_dir = NULL,
  # Uncomment below for your BCTU K: drive path:
  # data_dir = "K:/BCTU/BCTU/Teams/Coloproctology/CURRENT TRIALS/TONIC/Data",

  # ── REDCap events ──────────────────────────────────────────────────────────
  redcap_events = list(
    baseline  = "baseline_arm_1",
    discharge = "discharge_arm_1",
    day_30    = "day_30_arm_1",
    day_90    = "day_90_arm_1",
    sub_forms = c("sub_forms_arm_1", "ad_hoc_arm_1")
  ),

  # ── REDCap field mappings ──────────────────────────────────────────────────
  redcap_fields = list(
    record_id               = "record_id",
    site_name               = "site_name",
    randomisation_datetime  = "rand_dttm_s",

    operation_date          = "iop_op_end_dt",
    operation_datetime      = "iop_op_end_dttm",
    discharge_date          = "dis_discharge_day",

    age                     = "cae_age",
    sex                     = "base_sex",
    ethnicity               = "base_ethnic_gp",
    nela_score              = "base_nela_score_mort",
    residence               = "base_residence",
    nrs_group               = "nut_b_nrs_group",
    must_score              = "nut_b_must_score",

    follow_up_instruments = list(
      prodigi              = "prodigi_complete",
      eq5d                 = "eq5d_complete",
      qor15                = "qor15_complete",
      patient_satisfaction = "patient_satisfaction_complete",
      hruq                 = "hruq_complete"
    ),

    cos_type                = "cos_type",
    cos_date                = "cos_dt",        # Date of change in trial status (withdrawal drill-down)

    pn_start_datetime       = "nut_o_pn_start_dttm",
    pn_late_reason          = "nut_o_pn_late_rsn",
    pn_no_line_reason       = "nut_o_no_line_rsn",
    pn_early_reason         = "nut_o_pn_early_rsn",

    contact_preference      = "cntct_questionnaires_pref",

    # Completion status fields (used in upload/data loading)
    randomisation_complete  = "randomisation_complete",
    consent_complete        = "consent_eligibility_complete",
    discharge_complete      = "discharge_complete",

    # Timepoint form-completion fields driving the Data-tab donut NUMERATORS.
    # The donut shows "<forms complete> / <number randomised>".
    # The post-operation form is LONGITUDINAL — the same `post_operation_complete`
    # variable is recorded at both the day_30_arm_1 and day_90_arm_1 events — so
    # participants_server counts it per redcap_event_name (see redcap_events
    # above) to keep Day 30 and Day 90 separate. discharge_complete (above)
    # drives the Discharge donut.
    day30_complete          = "post_operation_complete",
    day90_complete          = "post_operation_complete",

    # Sub-form completion / safety fields
    sae_complete                    = "sae_complete",
    deviation_complete              = "deviation_complete",
    pregnancy_notification_complete = "pregnancy_notification_complete",
    pregnancy_outcome_complete      = "pregnancy_outcome_complete",

    # SAE drill-down detail (Data tab → SAEs). Without these the count is right
    # but every detail cell is an em-dash.
    sae_term         = "sae_diagnosis",      # Diagnosis           → "Event"
    sae_severity     = "sae_severity",       # 1 Mild/2 Mod/3 Sev  → "Severity"
    sae_relatedness  = "sae_causality_site", # 1-5 causality
    sae_status       = "sae_status",         # 1 Ongoing/2 Resolved/3 Fatal → "Status"
    sae_onset_date   = "sae_onset_dt",       # Date of Onset       → "Date occurred"
    sae_report_date  = "sae_reported_dt",    # Date Reported       → "Date submitted"
    sae_narrative    = "sae_signssymptoms",  # Signs & symptoms    → "Reason / notes"

    # Deviation drill-down detail (Data tab → Deviations). These populate the
    # "Event / Date occurred / Date submitted / Reason" columns; without them
    # the count is right but every detail cell shows an em-dash.
    deviation_term          = "dev_ref",      # Deviation number  → "Event"
    deviation_date          = "dev_dt",       # Date of deviation → "Date occurred"
    deviation_report_date   = "dev_aware",    # Date team aware   → "Date submitted"
    deviation_narrative     = "dev_summary",  # Summary           → "Reason / notes"

    # REDCap structural fields
    redcap_event_name = "redcap_event_name"
  ),

  # Optional override for the CRF return-rates CSV folder. NULL = look in
  # `<trial_dir>/return rates/` or whatever upload module is wired to.
  crf_csv_default_path = "K:/BCTU/BCTU/Teams/Coloproctology/CURRENT TRIALS/TONIC/TONIC Meeting Organiser/TONIC TMG Report/TONIC_app/return rates",

  # ── Coded value → label maps (keyed by REDCap column name) ────────────────
  # Used by the demographic breakdowns and the safety drill-down to show words
  # instead of raw codes. Add more columns here as needed.
  column_labels = list(
    sae_severity = list("1" = "Mild",    "2" = "Moderate", "3" = "Severe"),
    sae_status   = list("1" = "Ongoing", "2" = "Resolved", "3" = "Fatal")
  ),

  # ── COS type labels ───────────────────────────────────────────────────────
  cos_type_labels = c(
    "1" = "Death",
    "2" = "No Operation",
    "3" = "Part withdrawal",
    "4" = "Complete withdrawal",
    "5" = "Lost to follow-up"
  ),

  cos_type_descriptions = c(
    "1" = "Death - Notification that participant has died",
    "2" = "No Operation - Participant did not have TONIC index operation as planned",
    "3" = "Part withdrawal - Participant wishes to withdraw from certain aspects of the trial",
    "4" = "Complete withdrawal - Participant wishes to withdraw completely from the trial",
    "5" = "Lost to follow-up - After repeated attempts it has not been possible to contact the participant"
  ),

  # ── Ethnicity labels (19-code REDCap scheme) ──────────────────────────────
  ethnicity_labels = c(
    "1"  = "Asian or Asian British - Indian",
    "2"  = "Asian or Asian British - Pakistani",
    "3"  = "Asian or Asian British - Bangladeshi",
    "4"  = "Asian or Asian British - Chinese",
    "5"  = "Asian or Asian British - Any other Asian background",
    "6"  = "Black, Black British, Caribbean or African - Caribbean",
    "7"  = "Black, Black British, Caribbean or African - African",
    "8"  = "Black, Black British, Caribbean or African - Any other Black background",
    "9"  = "Mixed or multiple ethnic groups - White and Black Caribbean",
    "10" = "Mixed or multiple ethnic groups - White and Black African",
    "11" = "Mixed or multiple ethnic groups - White and Asian",
    "12" = "Mixed or multiple ethnic groups - Any other Mixed background",
    "13" = "White - English, Welsh, Scottish, Northern Irish or British",
    "14" = "White - Irish",
    "15" = "White - Gypsy or Irish Traveller",
    "16" = "White - Roma",
    "17" = "White - Any other White background",
    "18" = "Other ethnic group - Arab",
    "19" = "Other ethnic group - Any other ethnic group"
  ),

  # White ethnicity codes (13-17) — used for minority/non-white headline
  white_ethnicity_codes = as.character(13:17),

  # ── Target schedule (protocol plan) ────────────────────────────────────────
  target_schedule = data.frame(
    month_date = as.Date(c(
      "2026-03-01","2026-04-01","2026-05-01","2026-06-01",
      "2026-07-01","2026-08-01","2026-09-01","2026-10-01","2026-11-01","2026-12-01",
      "2027-01-01","2027-02-01","2027-03-01","2027-04-01","2027-05-01","2027-06-01",
      "2027-07-01","2027-08-01","2027-09-01","2027-10-01","2027-11-01","2027-12-01",
      "2028-01-01","2028-02-01","2028-03-01","2028-04-01","2028-05-01","2028-06-01"
    )),
    cumulative_target = c(
      0,4,12,24,36,48,60,76,96,120,148,180,216,256,300,348,398,
      448,498,548,598,648,698,748,798,848,873,898
    ),
    stringsAsFactors = FALSE
  ),

  # ── Participant table layout ───────────────────────────────────────────────
  participant_table_layout = list(
    timepoints = list(
      list(name = "Baseline",  event = "baseline",  color = "#7C3AED",
           instruments = list(list(label = "EQ-5D", field = "eq5d_complete"))),
      list(name = "Discharge", event = "discharge", color = "#3B82F6",
           instruments = list(list(label = "PRODIGI", field = "prodigi_complete"),
                              list(label = "EQ-5D",   field = "eq5d_complete"),
                              list(label = "QoR-15",  field = "qor15_complete"))),
      list(name = "Day 30",    event = "day_30",    color = "#2EC4A5",
           instruments = list(list(label = "EQ-5D",     field = "eq5d_complete"),
                              list(label = "Pat. Sat.", field = "patient_satisfaction_complete"),
                              list(label = "HRUQ",      field = "hruq_complete"))),
      list(name = "Day 90",    event = "day_90",    color = "#059669",
           instruments = list(list(label = "EQ-5D", field = "eq5d_complete"),
                              list(label = "HRUQ",  field = "hruq_complete")))
    )
  ),

  # ── Projection defaults ───────────────────────────────────────────────────
  projection_defaults = list(
    rate_central       = 3.0,
    rate_optimistic    = 4.0,
    rate_pessimistic   = 2.0,
    sites_central      = 2.0,
    sites_optimistic   = 3.0,
    sites_pessimistic  = 1.0,
    target_sites       = 24
  ),

  # ── Report defaults ───────────────────────────────────────────────────────
  report_defaults = list(
    ci              = "Mr Matthew Lee",
    sponsor         = "University of Birmingham",
    sponsor_ref     = "RG_25-059",
    funder          = "National Institute for Health and Care Research, Health Technology Assessment Programme (NIHR \u2013 HTA)",
    funder_ref      = "NIHR155875",
    isrctn          = "59200516",
    iras            = "328678",

    trial_title = "A randomised trial comparing early parenteral nutrition vs standard nutritional care in adults undergoing emergency laparotomy.",

    ts_objectives = paste(
      "Primary clinical objective: to determine whether early parenteral nutrition (PN) in patients undergoing emergency laparotomy/laparoscopy reduces in-hospital post-operative complications assessed at hospital discharge as compared to usual nutritional care, measured using the Comprehensive Complication Index (CCI).",
      "",
      "Secondary objectives: to assess the impact of early PN on post-operative complications, activities of daily living, muscle function (sit-to-stand test), patient-reported outcomes (PRO-diGI, QoR-15, EQ-5D), SAE rates, unplanned readmissions, hospital length of stay, discharge destination, participant satisfaction, and to report PN use (duration and calories) and pathway metrics (randomisation, line insertion, operation, time to starting PN) up to 90 days post-operation.",
      sep = "\n"
    ),

    ts_design = "Multi-centre, two-arm, parallel-group, superiority, individual participant randomised controlled trial with 1:1 allocation to early parenteral nutrition (PN) or standard nutritional care. Includes an internal pilot (first 6 months of recruitment) and a full economic evaluation.",

    ts_eligibility = "Adults undergoing National Emergency Laparotomy Audit (NELA) eligible emergency laparotomy or laparoscopy.",

    ts_interventions = paste(
      "Intervention arm: early parenteral nutrition (PN) started within 48 hours of emergency laparotomy/laparoscopy.",
      "Control arm: standard nutritional care.",
      sep = "\n"
    ),

    ts_primary_outcome = "In-hospital post-operative complications assessed at hospital discharge, measured using the Comprehensive Complication Index (CCI).",

    ts_secondary_outcomes = paste(
      "Post-operative complications up to 90 days post-operation",
      "Activities of daily living up to 90 days post-operation (Barthel Index)",
      "Muscle function (sit-to-stand test) at hospital discharge",
      "Patient-reported outcomes (PRO-diGI, QoR-15, EQ-5D) up to 90 days post-operation",
      "SAE rate related to early PN up to 89 days post-operation",
      "Unplanned readmissions following discharge up to 90 days post-operation",
      "Hospital length of stay during index admission",
      "Discharge destination following index admission",
      "Participant satisfaction with treatment",
      "PN use in both arms (duration and calories administered)",
      "Use of any nutritional intervention in either trial arm",
      "Pathway metrics: randomisation, line insertion, operation, and time to starting PN",
      sep = "\n"
    ),

    pilot_info = paste(
      "Internal pilot: first 6 months of recruitment, aiming for 60 participants (7% of total sample) from 6 sites.",
      "Assessed on: recruitment rate, data completeness for primary outcome, 90-day questionnaire return rate, intervention delivery within 48 hours, and cross-over rates between arms.",
      "Continuation guided by pre-defined stop-go criteria.",
      sep = " "
    )
  ),

  # ── Feature flags ──────────────────────────────────────────────────────────
  features = list(
    postal_tracking  = TRUE,
    return_rates     = TRUE,
    projections      = TRUE,
    pilot_criteria   = TRUE,
    consort_flow     = TRUE,
    baseline_table   = TRUE
  )
)
