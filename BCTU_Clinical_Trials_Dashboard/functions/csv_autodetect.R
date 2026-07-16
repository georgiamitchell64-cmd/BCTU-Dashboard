# =============================================================================
# REDCap CSV Auto-Detection
# =============================================================================
# Scans a CSV export and detects events, instruments, fields, and structure.
# Returns a proposed config that the user can confirm/adjust.
# =============================================================================

autodetect_redcap <- function(filepath) {
  df <- tryCatch(
    read.csv(filepath, stringsAsFactors = FALSE, check.names = FALSE, nrows = 5000),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0) return(NULL)

  # Clean column names
  names(df) <- gsub("^\xef\xbb\xbf", "", names(df))
  names(df) <- trimws(names(df))
  cols <- names(df)

  result <- list()

  # ── 1. Record ID ──────────────────────────────────────────────────────────
  result$record_id <- if ("record_id" %in% cols) "record_id"
                      else if ("participant_id" %in% cols) "participant_id"
                      else cols[1]  # REDCap always puts record ID first

  # ── 2. Events ─────────────────────────────────────────────────────────────
  result$events <- list()
  if ("redcap_event_name" %in% cols) {
    all_events <- unique(df$redcap_event_name)
    all_events <- all_events[!is.na(all_events) & nzchar(all_events)]

    # Classify events by name patterns
    classify <- function(patterns) {
      for (ev in all_events) {
        ev_lower <- tolower(ev)
        for (pat in patterns) {
          if (grepl(pat, ev_lower)) return(ev)
        }
      }
      NULL
    }

    result$events$baseline  <- classify(c("baseline", "enrol", "screen", "randomi"))
    result$events$discharge <- classify(c("discharge", "day_0", "post_op", "postop"))
    result$events$day_30    <- classify(c("day.?30", "month.?1[^0-9]", "4.?week", "30.?day", "follow.?up.?1"))
    result$events$day_90    <- classify(c("day.?90", "month.?3[^0-9]", "12.?week", "90.?day", "follow.?up.?2"))

    # Sub-forms: anything with sae, adverse, safety, ad_hoc, sub_form
    sf_patterns <- c("sae", "adverse", "safety", "ad.?hoc", "sub.?form", "protocol.?dev",
                     "change.?of.?status", "withdrawal", "serious")
    result$events$sub_forms <- all_events[sapply(all_events, function(ev) {
      any(sapply(sf_patterns, function(p) grepl(p, tolower(ev))))
    })]

    # Any unclassified events
    classified <- c(result$events$baseline, result$events$discharge,
                    result$events$day_30, result$events$day_90,
                    result$events$sub_forms)
    result$events$unclassified <- setdiff(all_events, classified)
    result$events$all <- all_events
  }

  # ── 3. Instruments (columns ending in _complete) ──────────────────────────
  complete_cols <- grep("_complete$", cols, value = TRUE)
  result$instruments <- gsub("_complete$", "", complete_cols)
  result$instrument_fields <- setNames(complete_cols, result$instruments)

  # ── 4. Key field detection ────────────────────────────────────────────────
  result$fields <- list()

  # Site name
  site_candidates <- c("site_name", "site", "redcap_data_access_group",
                       "centre", "center", "hospital", "site_id")
  result$fields$site_name <- detect_field(cols, site_candidates)

  # Randomisation datetime
  rand_candidates <- c("rand_dttm_s", "rand_dttm", "randomisation_date",
                       "randomization_date", "rand_date", "date_randomised",
                       "date_randomized", "consent_date")
  result$fields$randomisation_datetime <- detect_field(cols, rand_candidates)

  # Operation date
  op_candidates <- c("iop_op_end_dt", "op_date", "operation_date", "surgery_date",
                     "procedure_date", "iop_op_end_dttm")
  result$fields$operation_date <- detect_field(cols, op_candidates)

  # Discharge date
  dc_candidates <- c("dis_discharge_day", "discharge_date", "date_discharge",
                     "discharge_dt", "date_of_discharge")
  result$fields$discharge_date <- detect_field(cols, dc_candidates)

  # Demographics
  age_candidates <- c("cae_age", "age", "patient_age", "age_at_consent",
                      "age_years", "dem_age")
  result$fields$age <- detect_field(cols, age_candidates)

  sex_candidates <- c("base_sex", "sex", "gender", "dem_sex", "dem_gender",
                      "patient_sex")
  result$fields$sex <- detect_field(cols, sex_candidates)

  eth_candidates <- c("base_ethnic_gp", "ethnicity", "ethnic_group",
                      "race_ethnicity", "dem_ethnicity", "ethnic")
  result$fields$ethnicity <- detect_field(cols, eth_candidates)

  # COS / withdrawal
  cos_candidates <- c("cos_type", "change_of_status", "withdrawal_type",
                      "withdrawal_reason", "status_change")
  result$fields$cos_type <- detect_field(cols, cos_candidates)

  # NELA score (surgery-specific)
  nela_candidates <- c("base_nela_score_mort", "nela_score", "nela_mortality",
                       "predicted_mortality")
  result$fields$nela_score <- detect_field(cols, nela_candidates)

  # ── 4b. Safety / regulatory event-detail fields ───────────────────────────
  # The Data tab drill-down needs per-event detail (onset date, severity,
  # narrative, etc.) for SAEs / deviations / withdrawals / pregnancies. These
  # column names are not standardised across trials, so we sniff using a
  # prefix + suffix pattern: for each event family, look for a column whose
  # name starts with the form prefix (sae, dev, preg, etc.) and ends with a
  # role suffix (severity, onset, narrative, etc.). First match wins.
  sniff <- function(prefixes, suffixes) {
    cl <- tolower(cols)
    for (p in prefixes) for (s in suffixes) {
      hit <- grep(paste0("^", p, ".*", s), cl, perl = TRUE)
      if (length(hit) > 0) return(cols[hit[1]])
    }
    NULL
  }

  # SAEs
  result$fields$sae_term         <- sniff(c("sae_"), c("term$", "name$", "desc$", "description$", "event$"))
  result$fields$sae_severity     <- sniff(c("sae_"), c("severity$", "grade$", "intensity$"))
  result$fields$sae_relatedness  <- sniff(c("sae_"), c("relat", "causal"))
  result$fields$sae_status       <- sniff(c("sae_"), c("status$", "outcome$"))
  result$fields$sae_onset_date   <- sniff(c("sae_"), c("onset", "occur", "_date$", "_dt$", "start"))
  result$fields$sae_report_date  <- sniff(c("sae_"), c("report", "submitted", "submit", "notif"))
  result$fields$sae_narrative    <- sniff(c("sae_"), c("narrative", "details", "comment", "free"))

  # Deviations
  result$fields$deviation_term        <- sniff(c("dev_", "deviation_", "pd_"), c("term$", "type$", "category$", "desc"))
  result$fields$deviation_severity    <- sniff(c("dev_", "deviation_", "pd_"), c("severity$", "grade$", "impact$"))
  result$fields$deviation_status      <- sniff(c("dev_", "deviation_", "pd_"), c("status$", "resolved$", "outcome$"))
  result$fields$deviation_date        <- sniff(c("dev_", "deviation_", "pd_"), c("_date$", "_dt$", "occur"))
  result$fields$deviation_report_date <- sniff(c("dev_", "deviation_", "pd_"), c("report", "submitted", "notif"))
  result$fields$deviation_narrative   <- sniff(c("dev_", "deviation_", "pd_"), c("narrative", "details", "comment"))

  # Pregnancy notification
  result$fields$preg_notif_term        <- sniff(c("preg_n", "pn_", "pregnancy_n"), c("term$", "type$", "desc"))
  result$fields$preg_notif_status      <- sniff(c("preg_n", "pn_", "pregnancy_n"), c("status$", "outcome$"))
  result$fields$preg_notif_date        <- sniff(c("preg_n", "pn_", "pregnancy_n"), c("_date$", "_dt$", "lmp"))
  result$fields$preg_notif_report_date <- sniff(c("preg_n", "pn_", "pregnancy_n"), c("report", "submitted", "notif"))
  result$fields$preg_notif_narrative   <- sniff(c("preg_n", "pn_", "pregnancy_n"), c("narrative", "details", "comment"))

  # Pregnancy outcome
  result$fields$preg_out_outcome     <- sniff(c("preg_o", "po_", "pregnancy_o"), c("outcome$", "result$", "term$"))
  result$fields$preg_out_status      <- sniff(c("preg_o", "po_", "pregnancy_o"), c("status$"))
  result$fields$preg_out_date        <- sniff(c("preg_o", "po_", "pregnancy_o"), c("_date$", "_dt$", "delivery"))
  result$fields$preg_out_report_date <- sniff(c("preg_o", "po_", "pregnancy_o"), c("report", "submitted"))
  result$fields$preg_out_narrative   <- sniff(c("preg_o", "po_", "pregnancy_o"), c("narrative", "details", "comment"))

  # Withdrawal (cos_*)
  result$fields$cos_date   <- sniff(c("cos_", "withdraw"), c("_date$", "_dt$", "occur"))
  result$fields$cos_reason <- sniff(c("cos_", "withdraw"), c("reason", "narrative", "comment", "free"))

  # ── 5. Date fields (for reference) ────────────────────────────────────────
  date_cols <- cols[grepl("(date|_dt$|_dttm$|_dttm_|_day$)", cols, ignore.case = TRUE)]
  result$date_fields <- date_cols

  # ── 6. Statistics ─────────────────────────────────────────────────────────
  result$n_rows <- nrow(df)
  result$n_cols <- ncol(df)
  result$n_records <- length(unique(df[[result$record_id]]))

  result
}


# Helper: find the first matching column name
detect_field <- function(cols, candidates) {
  # Exact match first
  for (c in candidates) {
    if (c %in% cols) return(c)
  }
  # Partial match (case-insensitive)
  cols_lower <- tolower(cols)
  for (c in candidates) {
    matches <- grep(tolower(c), cols_lower, fixed = TRUE)
    if (length(matches) > 0) return(cols[matches[1]])
  }
  NULL
}


# ── Build a trial config from auto-detected results ──────────────────────────

build_config_from_detection <- function(detected, trial_code, trial_name, trial_target,
                                        template = NULL) {

  flds <- detected$fields

  # Build follow-up instruments list from detected instruments
  # Common instrument patterns for surgery trials
  fu_patterns <- list(
    prodigi              = c("prodigi", "pro_digi"),
    eq5d                 = c("eq5d", "eq_5d", "euroqol"),
    qor15                = c("qor15", "qor_15", "quality_of_recovery"),
    patient_satisfaction = c("patient_satisfaction", "satisfaction", "pat_sat"),
    hruq                 = c("hruq", "health_resource", "resource_use"),
    barthel              = c("barthel"),
    cci                  = c("cci", "comprehensive_complication"),
    clavien_dindo        = c("clavien", "dindo"),
    promis               = c("promis"),
    sf36                 = c("sf36", "sf_36"),
    vas                  = c("vas", "visual_analogue")
  )

  fu_instruments <- list()
  for (inst_name in names(fu_patterns)) {
    patterns <- fu_patterns[[inst_name]]
    for (detected_inst in names(detected$instrument_fields)) {
      if (any(sapply(patterns, function(p) grepl(p, tolower(detected_inst))))) {
        fu_instruments[[inst_name]] <- detected$instrument_fields[[detected_inst]]
        break
      }
    }
  }

  # Build events
  events <- list(
    baseline  = detected$events$baseline %||% "baseline_arm_1",
    discharge = detected$events$discharge
  )
  if (!is.null(detected$events$day_30))    events$day_30    <- detected$events$day_30
  if (!is.null(detected$events$day_90))    events$day_90    <- detected$events$day_90
  if (length(detected$events$sub_forms) > 0) events$sub_forms <- detected$events$sub_forms

  list(
    code         = trial_code,
    name         = trial_name,
    short_name   = toupper(trial_code),
    trial_target = as.integer(trial_target),
    colors       = if (!is.null(template)) template$colors
                   else list(primary = "#1B4F6B", secondary = "#2EC4A5", accent = "#F59E0B"),
    data_dir     = NULL,
    redcap_events = events,
    redcap_fields = list(
      record_id              = detected$record_id,
      site_name              = flds$site_name %||% "site_name",
      randomisation_datetime = flds$randomisation_datetime %||% "rand_dttm_s",
      operation_date         = flds$operation_date,
      discharge_date         = flds$discharge_date,
      age                    = flds$age,
      sex                    = flds$sex,
      ethnicity              = flds$ethnicity,
      nela_score             = flds$nela_score,
      follow_up_instruments  = fu_instruments,
      cos_type               = flds$cos_type,

      # Safety / regulatory event-detail columns (auto-sniffed; NULL if
      # no match — the Data tab drill-down renders em-dashes for missing
      # columns rather than failing).
      sae_term         = flds$sae_term,
      sae_severity     = flds$sae_severity,
      sae_relatedness  = flds$sae_relatedness,
      sae_status       = flds$sae_status,
      sae_onset_date   = flds$sae_onset_date,
      sae_report_date  = flds$sae_report_date,
      sae_narrative    = flds$sae_narrative,

      deviation_term        = flds$deviation_term,
      deviation_severity    = flds$deviation_severity,
      deviation_status      = flds$deviation_status,
      deviation_date        = flds$deviation_date,
      deviation_report_date = flds$deviation_report_date,
      deviation_narrative   = flds$deviation_narrative,

      preg_notif_term        = flds$preg_notif_term,
      preg_notif_status      = flds$preg_notif_status,
      preg_notif_date        = flds$preg_notif_date,
      preg_notif_report_date = flds$preg_notif_report_date,
      preg_notif_narrative   = flds$preg_notif_narrative,

      preg_out_outcome     = flds$preg_out_outcome,
      preg_out_status      = flds$preg_out_status,
      preg_out_date        = flds$preg_out_date,
      preg_out_report_date = flds$preg_out_report_date,
      preg_out_narrative   = flds$preg_out_narrative,

      cos_date   = flds$cos_date,
      cos_reason = flds$cos_reason
    ),
    cos_type_labels = c("1"="Death","2"="No Operation","3"="Part withdrawal",
                        "4"="Complete withdrawal","5"="Lost to follow-up"),
    features = if (!is.null(template)) template$features
               else list(postal_tracking = FALSE, return_rates = FALSE,
                         projections = TRUE, pilot_criteria = FALSE,
                         consort_flow = FALSE, baseline_table = FALSE)
  )
}
