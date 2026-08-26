# =============================================================================
# Shared Report Data Preparation
# =============================================================================
# Trial-agnostic. Reads REDCap field/event mappings from the active trial
# config (via fld() / evt() / current_trial_config()). Trial-specific clinical
# logic (e.g. PN intervention/contamination for TONIC) only runs when the
# corresponding fields are mapped.
# =============================================================================

prepare_report_data <- function(df,
                                selected_sites    = NULL,
                                date_from         = NULL,
                                date_to           = NULL,
                                include_withdrawn = FALSE,
                                pipeline_df       = NULL,
                                crf_csv_path      = NULL) {

  cfg <- current_trial_config()

  # Guard against missing / empty input. Without this, `names(df) <- ...` below
  # blows up with "attempt to set an attribute on NULL" when the trial has no
  # data loaded yet.
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0)
    stop("No data loaded for this trial. Upload a REDCap CSV from the Data tab before generating a report.")

  # ── 1. Clean column names ──────────────────────────────────────────────────
  names(df) <- gsub("^\xef\xbb\xbf", "", names(df))
  names(df) <- trimws(names(df))

  # ── 2. Alias trial-specific REDCap columns to canonical short names ────────
  # Every downstream reference uses these canonicals so the function works for
  # any trial whose config maps the same logical roles.
  alias_map <- list(
    rand_dttm        = "randomisation_datetime",
    op_dttm          = "operation_datetime",
    op_dt            = "operation_date",
    dis_day          = "discharge_date",
    pn_start         = "pn_start_datetime",
    pn_late          = "pn_late_reason",
    pn_noline        = "pn_no_line_reason",
    pn_early         = "pn_early_reason",
    age_v            = "age",
    sex_v            = "sex",
    eth_v            = "ethnicity",
    nela_v           = "nela_score",
    residence_v      = "residence",
    nrs_v            = "nrs_group",
    must_v           = "must_score",
    cos_v            = "cos_type",
    cos_done         = "change_of_status_complete",
    cos_dt           = "change_of_status_date",
    cos_rsn          = "change_of_status_reason",
    sae_done         = "sae_complete",
    rand_done        = "randomisation_complete",
    consent_done     = "consent_complete",
    disc_done        = "discharge_complete",
    site_v           = "site_name",
    record_v         = "record_id",
    event_v          = "redcap_event_name"
  )
  for (canon in names(alias_map)) {
    src <- fld(alias_map[[canon]], default = NULL, cfg = cfg)
    if (!is.null(src) && src %in% names(df) && !(canon %in% names(df)))
      df[[canon]] <- df[[src]]
  }

  # PN timing reasons (TONIC: nut_o_pn_late_rsn / nut_o_pn_early_rsn) —
  # autodetect when the config doesn't map pn_late_reason / pn_early_reason.
  if (!"pn_late" %in% names(df)) {
    cand <- grep("pn_late.*(rsn|reason)|pn_late_rsn", names(df),
                 ignore.case = TRUE, value = TRUE)
    if (length(cand) > 0) df$pn_late <- df[[cand[1]]]
  }
  if (!"pn_early" %in% names(df)) {
    cand <- grep("pn_early.*(rsn|reason)|pn_early_rsn", names(df),
                 ignore.case = TRUE, value = TRUE)
    if (length(cand) > 0) df$pn_early <- df[[cand[1]]]
  }

  # REDCap CSV exports encode missing as "" (not NA) — a field only counts
  # as completed when it holds something.
  filled <- function(x) !is.na(x) & nzchar(trimws(as.character(x)))

  # ── 3. Parse datetime / date columns ───────────────────────────────────────
  parse_dt <- function(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M", tz = "Europe/London")
  parse_d  <- function(x) as.Date(x, format = "%Y-%m-%d")
  for (c in c("op_dttm", "rand_dttm", "pn_start"))
    if (c %in% names(df)) df[[c]] <- parse_dt(df[[c]])
  for (c in c("dis_day", "op_dt"))
    if (c %in% names(df)) df[[c]] <- parse_d(df[[c]])

  # ── 4. Split by event ──────────────────────────────────────────────────────
  evcol <- if ("event_v" %in% names(df)) "event_v" else "redcap_event_name"
  ev    <- function(name) {
    actual <- evt(name, default = NULL, cfg = cfg)
    if (is.null(actual) || !(evcol %in% names(df))) return(df[0, ])
    df[df[[evcol]] %in% actual, ]
  }
  baseline  <- ev("baseline")
  discharge <- ev("discharge")
  day30     <- ev("day_30")
  day90     <- ev("day_90")
  sub_forms <- ev("sub_forms")

  # ── 5. Follow-up instrument fields (from cfg) ──────────────────────────────
  fu_inst <- cfg$redcap_fields$follow_up_instruments %||% list()
  fu_cols <- unname(unlist(fu_inst))

  # ── 6. One-row-per-participant ─────────────────────────────────────────────
  bl_keep <- intersect(c("record_v","site_v","rand_dttm","age_v","sex_v","eth_v",
                         "nela_v","residence_v","nrs_v","must_v","op_dt",
                         "rand_done","consent_done"), names(baseline))
  base_wide <- baseline[, bl_keep, drop = FALSE]
  if ("record_v" %in% names(base_wide)) names(base_wide)[names(base_wide)=="record_v"] <- "record_id"

  dc_keep <- intersect(c("record_v","op_dttm","dis_day","pn_start","pn_noline",
                         "pn_late","pn_early","disc_done"), names(discharge))
  dc_wide <- discharge[, dc_keep, drop = FALSE]
  if ("record_v" %in% names(dc_wide)) names(dc_wide)[names(dc_wide)=="record_v"] <- "record_id"

  prefix_fu <- function(src_df, prefix) {
    keep <- intersect(c("record_v", fu_cols), names(src_df))
    out  <- src_df[, keep, drop = FALSE]
    if ("record_v" %in% names(out)) names(out)[names(out)=="record_v"] <- "record_id"
    other <- setdiff(names(out), "record_id")
    if (length(other) > 0) names(out)[names(out) %in% other] <- paste0(prefix, other)
    out
  }
  fu30_wide <- prefix_fu(day30, "d30_")
  fu90_wide <- prefix_fu(day90, "d90_")

  ptcp <- merge(base_wide, dc_wide, by = "record_id", all.x = TRUE)
  ptcp <- merge(ptcp, fu30_wide, by = "record_id", all.x = TRUE)
  ptcp <- merge(ptcp, fu90_wide, by = "record_id", all.x = TRUE)

  complete_cols <- grep("_complete$", names(ptcp), value = TRUE)
  for (cc in complete_cols) ptcp[[cc]] <- as.numeric(ptcp[[cc]])

  # ── 7. Trial arm (only meaningful when PN fields are mapped) ───────────────
  has_late   <- if ("pn_late"  %in% names(ptcp)) filled(ptcp$pn_late)   else rep(FALSE, nrow(ptcp))
  has_noline <- if ("pn_noline" %in% names(ptcp)) filled(ptcp$pn_noline) else rep(FALSE, nrow(ptcp))
  has_early  <- if ("pn_early" %in% names(ptcp)) filled(ptcp$pn_early)  else rep(FALSE, nrow(ptcp))
  if (all(c("pn_start","op_dttm") %in% names(ptcp))) {
    hd   <- as.numeric(difftime(ptcp$pn_start, ptcp$op_dttm, units = "hours"))
    pn48 <- !is.na(hd) & hd >= 0 & hd <= 48
  } else pn48 <- rep(FALSE, nrow(ptcp))
  ptcp$trial_arm <- ifelse((has_late | pn48 | has_noline) & !has_early,
                           "Intervention", "Standard care")

  # ── 8. Dates (formatted to avoid UTC shift) ────────────────────────────────
  ptcp$op_date <- if ("op_dt" %in% names(ptcp)) ptcp$op_dt else
    if ("op_dttm" %in% names(ptcp)) as.Date(format(ptcp$op_dttm, "%Y-%m-%d")) else NA_Date_
  ptcp$rand_date <- if ("rand_dttm" %in% names(ptcp))
    as.Date(format(ptcp$rand_dttm, "%Y-%m-%d")) else NA_Date_

  # ── 9. Follow-up flags ────────────────────────────────────────────────────
  d30q <- intersect(paste0("d30_", fu_cols), names(ptcp))
  d90q <- intersect(paste0("d90_", fu_cols), names(ptcp))
  ptcp$fu_30_complete <- if (length(d30q) > 0)
    as.integer(rowSums(ptcp[, d30q, drop = FALSE] == 2, na.rm = FALSE) == length(d30q))
    else NA_integer_
  ptcp$fu_90_complete <- if (length(d90q) > 0)
    as.integer(rowSums(ptcp[, d90q, drop = FALSE] == 2, na.rm = FALSE) == length(d90q))
    else NA_integer_
  ptcp$fu_30_any <- if (length(d30q) > 0)
    as.integer(rowSums(ptcp[, d30q, drop = FALSE] == 2, na.rm = TRUE) >= 1)
    else NA_integer_
  ptcp$fu_90_any <- if (length(d90q) > 0)
    as.integer(rowSums(ptcp[, d90q, drop = FALSE] == 2, na.rm = TRUE) >= 1)
    else NA_integer_

  # ── 10. COS / Withdrawals ─────────────────────────────────────────────────
  cos_label <- cfg$cos_type_labels %||% c(
    "1"="Death","2"="No operation","3"="Part withdrawal",
    "4"="Complete withdrawal","5"="Lost to follow-up")
  withdrawal_events <- data.frame()
  # The count of change-of-status events is driven by the instrument's REDCap
  # completion flag (<instrument>_complete == 2), not by cos_type alone:
  # a completed form whose type code is blank or non-standard (e.g. "WDR")
  # must still be counted. cos_type then supplies the type/label.
  if (!"cos_done" %in% names(sub_forms)) {
    cand <- grep("change_of_status_complete$", names(sub_forms), value = TRUE)
    if (length(cand) > 0) sub_forms$cos_done <- sub_forms[[cand[1]]]
  }
  # Date the change of status was recorded (withdrawal date). Config role
  # change_of_status_date first; otherwise autodetect a cos_* date column.
  if (!"cos_dt" %in% names(sub_forms)) {
    cand <- grep("^cos_.*(date|dt)$|change_of_status.*(date|dt)$",
                 names(sub_forms), ignore.case = TRUE, value = TRUE)
    cand <- setdiff(cand, "cos_done")
    if (length(cand) > 0) sub_forms$cos_dt <- sub_forms[[cand[1]]]
  }
  # Withdrawal reason. REDCap pairs a coded reason (TONIC:
  # cos_withdraw_rsn_pt, where 99 = "Other") with a free-text field
  # (cos_withdraw_rsn_oth) completed when the code is 99. Per row: the free
  # text when present, otherwise the coded value mapped through
  # cfg$cos_reason_labels (99 → "Other" by default). Columns resolve via the
  # config roles change_of_status_reason / change_of_status_reason_code,
  # falling back to cos_* reason column autodetection.
  if (!"cos_rsn" %in% names(sub_forms)) {
    cand  <- grep("^cos_.*(rsn|reason)|change_of_status.*(rsn|reason)",
                  names(sub_forms), ignore.case = TRUE, value = TRUE)
    texty <- grep("oth|text|spec", cand, ignore.case = TRUE, value = TRUE)
    txt_col <- fld("change_of_status_reason", default = NULL, cfg = cfg)
    if (is.null(txt_col) || !(txt_col %in% names(sub_forms)))
      txt_col <- if (length(texty) > 0) texty[1] else NULL
    code_col <- fld("change_of_status_reason_code", default = NULL, cfg = cfg)
    if (is.null(code_col) || !(code_col %in% names(sub_forms))) {
      coded    <- setdiff(cand, c(texty, txt_col))
      code_col <- if (length(coded) > 0) coded[1] else NULL
    }
    if (!is.null(txt_col) || !is.null(code_col)) {
      rsn_labels <- cfg$cos_reason_labels %||% c("99" = "Other")
      val <- rep(NA_character_, nrow(sub_forms))
      if (!is.null(code_col)) {
        code <- trimws(as.character(sub_forms[[code_col]]))
        lab  <- unname(rsn_labels[code])
        has  <- !is.na(code) & nzchar(code)
        val[has] <- ifelse(is.na(lab[has]), code[has], lab[has])
      }
      if (!is.null(txt_col)) {
        txt <- trimws(as.character(sub_forms[[txt_col]]))
        has <- !is.na(txt) & nzchar(txt)
        val[has] <- txt[has]
      }
      sub_forms$cos_rsn <- val
    }
  }
  if (nrow(sub_forms) > 0 && any(c("cos_v", "cos_done") %in% names(sub_forms))) {
    typed <- if ("cos_v" %in% names(sub_forms))
      !is.na(sub_forms$cos_v) & nzchar(trimws(as.character(sub_forms$cos_v)))
      else rep(FALSE, nrow(sub_forms))
    done <- if ("cos_done" %in% names(sub_forms))
      !is.na(suppressWarnings(as.integer(sub_forms$cos_done))) &
        suppressWarnings(as.integer(sub_forms$cos_done)) == 2
      else rep(FALSE, nrow(sub_forms))
    sub_cos <- sub_forms[typed | done, ]
    if (nrow(sub_cos) > 0) {
      if (!"cos_v" %in% names(sub_cos)) sub_cos$cos_v <- NA_character_
      raw <- trimws(as.character(sub_cos$cos_v))
      sub_cos$cos_label <- unname(cos_label[raw])
      no_lab <- is.na(sub_cos$cos_label)
      sub_cos$cos_label[no_lab] <- ifelse(!is.na(raw[no_lab]) & nzchar(raw[no_lab]),
                                          raw[no_lab], "Type not recorded")
      # Non-numeric / blank cos codes coerce to NA (they sort last). Wrap so a
      # legitimately mixed column doesn't spam the log on every report build.
      sub_cos <- sub_cos[order(sub_cos$record_v,
                               -suppressWarnings(as.numeric(sub_cos$cos_v))), ]
      sub_cos <- sub_cos[!duplicated(sub_cos$record_v), ]
      withdrawal_events <- data.frame(
        record_id = sub_cos$record_v,
        cos_type  = sub_cos$cos_v,
        cos_label = sub_cos$cos_label,
        cos_date  = if ("cos_dt" %in% names(sub_cos))
          as.character(sub_cos$cos_dt) else NA_character_,
        cos_reason = if ("cos_rsn" %in% names(sub_cos))
          as.character(sub_cos$cos_rsn) else NA_character_,
        has_cos   = TRUE,
        stringsAsFactors = FALSE)
    }
  }
  if (nrow(withdrawal_events) > 0)
    ptcp <- merge(ptcp, withdrawal_events, by = "record_id", all.x = TRUE)
  else { ptcp$cos_type <- NA_integer_; ptcp$cos_label <- NA_character_; ptcp$has_cos <- FALSE }
  ptcp$has_cos         <- !is.na(ptcp$has_cos) & ptcp$has_cos
  ptcp$is_death        <- !is.na(ptcp$cos_type) & ptcp$cos_type == 1
  ptcp$is_no_operation <- !is.na(ptcp$cos_type) & ptcp$cos_type == 2
  ptcp$is_withdrawn    <- ptcp$has_cos & !ptcp$is_death & !ptcp$is_no_operation
  ptcp$participant_status <- dplyr::case_when(
    ptcp$is_death        ~ "Death",
    ptcp$is_no_operation ~ "No operation",
    ptcp$is_withdrawn    ~ ptcp$cos_label,
    TRUE                 ~ "Active")

  # ── 11. SAEs (unfiltered) ─────────────────────────────────────────────────
  sae_rows <- data.frame()
  if ("sae_done" %in% names(sub_forms)) {
    sae_rows <- sub_forms[!is.na(sub_forms$sae_done) & sub_forms$sae_done == 2, ]
    if (nrow(sae_rows) > 0 && "site_v" %in% names(base_wide)) {
      lk <- data.frame(record_id = base_wide$record_id,
                       site_name = base_wide$site_v, stringsAsFactors = FALSE)
      sae_rows$record_id <- sae_rows$record_v
      sae_rows <- merge(sae_rows, lk, by = "record_id", all.x = TRUE)
    }
  }
  sae_count <- nrow(sae_rows)

  # SAE detail log for the report (from the TMG REDCap export). Each detail
  # column resolves via a config role first, then autodetects a sae_* column.
  sae_log <- NULL
  if (sae_count > 0) {
    sae_roles <- list(
      reported  = list(role = "sae_reported_date", pat = "^sae_.*(report|onset|start).*(dt|date)"),
      diagnosis = list(role = "sae_diagnosis",     pat = "^sae_.*(diag|term|desc|title)"),
      soc       = list(role = "sae_soc",           pat = "^sae_.*soc"),
      category  = list(role = "sae_category",      pat = "^sae_.*categ"),
      severity  = list(role = "sae_severity",      pat = "^sae_.*(sever|grade)"),
      outcome   = list(role = "sae_outcome",       pat = "^sae_.*outcome"),
      related   = list(role = "sae_causality",     pat = "^sae_.*(caus|relat)"),
      expected  = list(role = "sae_expectedness",  pat = "^sae_.*expect"),
      death_yn  = list(role = "sae_death",         pat = "^sae_.*death.*yn"),
      death     = list(role = "sae_death_date",    pat = "^sae_.*death.*(dt|date)"))
    # Coded values render as their labels. Defaults are the TONIC code
    # lists; a trial config can override via sae_*_labels.
    sae_label_sets <- list(
      soc = cfg$sae_soc_labels %||% c(
        "1"="Infections and infestations","2"="Blood and lymphatic system disorders",
        "3"="Endocrine disorders","4"="Psychiatric disorders","5"="Eye disorders",
        "6"="Cardiac disorders","7"="Respiratory, thoracic and mediastinal disorders",
        "8"="Hepatobiliary disorders","9"="Musculoskeletal and connective tissue disorders",
        "10"="Pregnancy, puerperium and perinatal conditions",
        "11"="Congenital, familial and genetic disorders","12"="Investigations",
        "13"="Surgical and medical procedures",
        "14"="Neoplasms benign, malignant and unspecified","15"="Immune system disorders",
        "16"="Metabolism and nutrition disorders","17"="Nervous system disorders",
        "18"="Ear and labyrinth disorders","19"="Vascular disorders",
        "20"="Gastrointestinal disorders","21"="Skin and subcutaneous tissue disorders",
        "22"="Renal and urinary disorders","23"="Reproductive system and breast disorders",
        "24"="General disorders and administration site conditions",
        "25"="Injury, poisoning and procedural complications","26"="Social circumstances",
        "99"="Other"),
      category = cfg$sae_category_labels %||% c(
        "1"="Expedited SAE — related and unexpected","2"="Expedited SAE",
        "3"="Non-expedited SAE","4"="Non-SAE (downgraded to AE)",
        "5"="Non-SAE (reported in error)","6"="SAE exempt from reporting"),
      severity = cfg$sae_severity_labels %||% c(
        "1"="Mild","2"="Moderate","3"="Severe"),
      death_yn = c("1"="Yes","0"="No"))
    sae_log <- data.frame(
      record_id = sae_rows$record_id %||% sae_rows$record_v,
      site_name = if ("site_name" %in% names(sae_rows))
        as.character(sae_rows$site_name) else NA_character_,
      stringsAsFactors = FALSE)
    # Only columns actually present in the export make it into the log.
    for (nm in names(sae_roles)) {
      src <- fld(sae_roles[[nm]]$role, default = NULL, cfg = cfg)
      if (is.null(src) || !src %in% names(sae_rows)) {
        cand <- grep(sae_roles[[nm]]$pat, names(sae_rows),
                     ignore.case = TRUE, value = TRUE)
        src <- if (length(cand) > 0) cand[1] else NULL
      }
      if (!is.null(src)) {
        v <- trimws(as.character(sae_rows[[src]]))
        labs <- sae_label_sets[[nm]]
        if (!is.null(labs)) {
          m <- unname(labs[v])
          v <- ifelse(is.na(m), v, m)
        }
        sae_log[[nm]] <- v
      }
    }
    if ("reported" %in% names(sae_log))
      sae_log <- sae_log[order(suppressWarnings(as.Date(sae_log$reported)),
                               na.last = TRUE), ]
  }

  # site_name canonicalisation in ptcp
  if ("site_v" %in% names(ptcp) && !"site_name" %in% names(ptcp))
    ptcp$site_name <- ptcp$site_v

  # ── 12. Total randomised (unfiltered) ─────────────────────────────────────
  ptcp_randomised <- ptcp[!is.na(ptcp$rand_dttm), ]
  if (is.null(ptcp_randomised) || nrow(ptcp_randomised) == 0)
    stop("No randomised data available after preprocessing")
  total_randomised <- nrow(ptcp_randomised)
  n_sites_active   <- if ("site_name" %in% names(ptcp_randomised))
    length(unique(ptcp_randomised$site_name)) else NA

  # ── 13. Protocol target schedule (from cfg) ───────────────────────────────
  target_schedule <- cfg$target_schedule %||% data.frame(
    month_date = as.Date(character(0)),
    cumulative_target = integer(0),
    stringsAsFactors = FALSE)
  trial_target <- cfg$trial_target %||% if (nrow(target_schedule) > 0)
    max(target_schedule$cumulative_target, na.rm = TRUE) else NA_integer_
  today <- Sys.Date()

  past_targets <- target_schedule[target_schedule$month_date <= today, , drop = FALSE]
  # cumulative_target is the figure to reach by the END of its month, so a
  # report pulled mid-month must not demand the whole month's target. Expected
  # to date = last completed month's cumulative target + a pro-rata share of
  # the current month's increment (days elapsed ÷ days in the month).
  expected_to_date <- if (nrow(past_targets) > 0) {
    cur_cum   <- past_targets$cumulative_target[nrow(past_targets)]
    prev_cum  <- if (nrow(past_targets) > 1)
      past_targets$cumulative_target[nrow(past_targets) - 1] else 0
    m_start   <- past_targets$month_date[nrow(past_targets)]
    m_days    <- as.numeric(seq(m_start, by = "1 month", length.out = 2)[2] - m_start)
    frac      <- min(1, max(0, (as.numeric(today - m_start) + 1) / m_days))
    round(prev_cum + frac * (cur_cum - prev_cum))
  } else 0
  recruitment_start <- if (nrow(target_schedule) > 0) min(target_schedule$month_date) else today
  months_elapsed <- max(0, as.numeric(difftime(today, recruitment_start, units = "weeks")) / 4.33)
  recruitment_pct <- if (expected_to_date > 0)
    round((total_randomised / expected_to_date) * 100, 1) else NA

  # ── 13b. Build full schedule (targets + actuals) ──────────────────────────
  actuals <- if ("rand_date" %in% names(ptcp_randomised) && nrow(ptcp_randomised) > 0) {
    tmp <- ptcp_randomised[!is.na(ptcp_randomised$rand_date), ]
    if (nrow(tmp) > 0) {
      tmp$month_date <- as.Date(format(tmp$rand_date, "%Y-%m-01"))
      agg <- aggregate(list(n = rep(1, nrow(tmp))),
                       by  = list(month_date = tmp$month_date), FUN = sum)
      agg <- agg[order(agg$month_date), ]
      agg$cumulative_actual <- cumsum(agg$n)
      agg
    } else data.frame(month_date = as.Date(character(0)), n = integer(0),
                      cumulative_actual = integer(0))
  } else data.frame(month_date = as.Date(character(0)), n = integer(0),
                    cumulative_actual = integer(0))

  schedule_full <- if (nrow(target_schedule) > 0) {
    sf <- merge(target_schedule, actuals[, c("month_date","n","cumulative_actual")],
                by = "month_date", all.x = TRUE)
    sf$n[is.na(sf$n)] <- 0
    sf$monthly_actual <- sf$n
    ca <- sf$cumulative_actual; ca[is.na(ca)] <- 0
    sf$cumulative_actual <- cummax(ca)
    sf$monthly_target    <- c(sf$cumulative_target[1], diff(sf$cumulative_target))
    sf$month_label       <- format(sf$month_date, "%b %Y")
    sf
  } else target_schedule

  # ── 14. Apply filters ─────────────────────────────────────────────────────
  filtered <- ptcp_randomised
  if (!is.null(selected_sites) && length(selected_sites) > 0 &&
      !("All sites" %in% selected_sites) && "site_name" %in% names(filtered))
    filtered <- filtered[filtered$site_name %in% selected_sites, ]
  if (!is.null(date_from) && "rand_date" %in% names(filtered))
    filtered <- filtered[!is.na(filtered$rand_date) & filtered$rand_date >= as.Date(date_from), ]
  if (!is.null(date_to) && "rand_date" %in% names(filtered))
    filtered <- filtered[!is.na(filtered$rand_date) & filtered$rand_date <= as.Date(date_to), ]
  withdrawn_df <- ptcp_randomised[ptcp_randomised$is_withdrawn, ]

  # ── 15. Monthly recruitment ────────────────────────────────────────────────
  monthly_recruit <- if ("rand_date" %in% names(filtered) && nrow(filtered) > 0 &&
                         any(!is.na(filtered$rand_date))) {
    fr <- filtered[!is.na(filtered$rand_date), ]
    if (nrow(fr) == 0)
      data.frame(month=character(0), n=integer(0),
                 month_date=as.Date(character(0)), cumulative=integer(0))
    else {
      fr$month_label <- format(fr$rand_date, "%Y-%m")
      m <- aggregate(list(n = rep(1, nrow(fr))), by = list(month = fr$month_label), FUN = sum)
      m <- m[order(m$month), ]
      m$month_date <- as.Date(paste0(m$month, "-01"))
      m$cumulative <- cumsum(m$n)
      m
    }
  } else data.frame(month=character(0), n=integer(0),
                    month_date=as.Date(character(0)), cumulative=integer(0))

  # ── 16. Follow-up completion ──────────────────────────────────────────────
  op_dates <- if ("op_date" %in% names(filtered)) filtered$op_date else rep(NA_Date_, nrow(filtered))
  reached_po <- !is.na(op_dates) & (op_dates + 7) <= today
  elig_po <- sum(reached_po)
  comp_po <- if ("disc_done" %in% names(filtered) && elig_po > 0)
    sum(filtered$disc_done[reached_po] == 2, na.rm = TRUE) else 0
  pct_po  <- if (elig_po > 0) round(comp_po/elig_po*100, 1) else NA_real_
  primary_outcome <- list(eligible = elig_po, complete = comp_po, pct = pct_po, threshold_days = 7)

  elig_dc <- nrow(filtered)
  comp_dc <- if ("disc_done" %in% names(filtered))
    sum(filtered$disc_done == 2, na.rm = TRUE) else 0
  reached_30 <- !is.na(op_dates) & (op_dates + 30) <= today
  elig_30 <- sum(reached_30)
  comp_30 <- if ("fu_30_complete" %in% names(filtered) && elig_30 > 0)
    sum(filtered$fu_30_complete[reached_30] == 1, na.rm = TRUE) else NA
  reached_90 <- !is.na(op_dates) & (op_dates + 90) <= today
  elig_90 <- sum(reached_90)
  comp_90 <- if ("fu_90_complete" %in% names(filtered) && elig_90 > 0)
    sum(filtered$fu_90_complete[reached_90] == 1, na.rm = TRUE) else NA
  fu_90_rate <- if (!is.na(comp_90) && elig_90 > 0) round(comp_90/elig_90*100, 1) else NA
  fu_90_status <- if (elig_90 == 0) "not_yet_due" else if (elig_90 < 10) "small_n"
    else if (is.na(fu_90_rate)) "not_yet_due" else if (fu_90_rate >= 100) "green"
    else if (fu_90_rate >= 70) "amber" else "red"

  fu_summary <- data.frame(
    timepoint = c("Discharge","30-day","90-day"),
    denominator_note = c("all randomised participants",
      paste0("op date + 30 days ≤ ", format(today, "%d %b %Y")),
      paste0("op date + 90 days ≤ ", format(today, "%d %b %Y"))),
    eligible   = c(elig_dc, elig_30, elig_90),
    complete   = c(comp_dc, comp_30, comp_90),
    assessable = c(TRUE, elig_30 > 0, elig_90 > 0),
    stringsAsFactors = FALSE)
  fu_summary$pct <- ifelse(fu_summary$eligible > 0 & fu_summary$assessable,
                           round(fu_summary$complete / fu_summary$eligible * 100, 1),
                           NA_real_)

  fu_30_kpi <- if ("fu_30_any" %in% names(filtered)) sum(filtered$fu_30_any == 1, na.rm = TRUE) else 0
  fu_90_kpi <- if ("fu_90_any" %in% names(filtered)) sum(filtered$fu_90_any == 1, na.rm = TRUE) else 0

  # ── 17. Safety ─────────────────────────────────────────────────────────────
  safety_summary <- if (nrow(sae_rows) > 0)
    sae_rows[, intersect(c("site_name"), names(sae_rows)), drop = FALSE] else NULL

  # ── 18. Site performance ──────────────────────────────────────────────────
  site_summary <- if ("site_name" %in% names(filtered) && nrow(filtered) > 0) {
    s <- aggregate(list(randomisations = rep(1, nrow(filtered))),
                   by = list(site_name = filtered$site_name), FUN = sum)
    safe_min_date <- function(x) { x <- x[!is.na(x)]; if (length(x) == 0) as.Date(NA) else min(x) }
    if ("rand_date" %in% names(filtered)) {
      frd <- filtered %>% dplyr::filter(!is.na(rand_date)) %>%
        dplyr::group_by(site_name) %>%
        dplyr::summarise(first_rand_date = safe_min_date(rand_date), .groups = "drop") %>%
        as.data.frame()
      s <- merge(s, frd, by = "site_name", all.x = TRUE)
    } else s$first_rand_date <- as.Date(NA)
    s[order(-s$randomisations), ]
  } else data.frame(site_name=character(0), randomisations=integer(0),
                    first_rand_date=as.Date(character(0)))

  # ── 19. Pipeline + site status ────────────────────────────────────────────
  if (!is.null(pipeline_df) && nrow(pipeline_df) > 0) {
    pn <- names(pipeline_df)
    rmap <- c("Trust name"="site_name","trust name"="site_name","Status"="stage","status"="stage",
      "Open date"="open_date","open date"="open_date","Overall target"="target","overall target"="target",
      "Monthly target"="monthly_target","monthly target"="monthly_target",
      "Site ID"="site_id","site id"="site_id","Identified"="identified",
      "Already randomised"="already_randomised")
    for (on in names(rmap)) {
      idx <- which(pn == on)
      if (length(idx) > 0) names(pipeline_df)[idx[1]] <- rmap[on]
    }
    if (!"site_name" %in% names(pipeline_df) && "site_id" %in% names(pipeline_df))
      pipeline_df$site_name <- pipeline_df$site_id
  }

  # Site performance is a lifetime view: the Target column and the progress bar
  # beside this count are all-time figures, so the count has to be all-time too.
  # `filtered` is narrowed by the report's date range, which left Rand. frozen
  # at whatever happened to fall inside the reporting window rather than
  # tracking the site's real total — a period numerator over a lifetime
  # denominator. Count from the unfiltered randomised set instead, still
  # honouring an explicit site selection.
  rand_src <- ptcp_randomised
  if (!is.null(selected_sites) && length(selected_sites) > 0 &&
      !("All sites" %in% selected_sites) && "site_name" %in% names(rand_src))
    rand_src <- rand_src[rand_src$site_name %in% selected_sites, ]

  recruiting_sites <- if ("site_name" %in% names(rand_src) && nrow(rand_src) > 0) {
    sites <- unique(rand_src$site_name)
    data.frame(site_name = sites, stage = "Open — Recruiting",
      randomisations = sapply(sites, function(s) sum(rand_src$site_name == s)),
      source = "redcap", stringsAsFactors = FALSE)
  } else data.frame(site_name=character(0), stage=character(0),
                    randomisations=integer(0), source=character(0),
                    stringsAsFactors=FALSE)

  if (!is.null(pipeline_df) && "site_name" %in% names(pipeline_df)) {
    lk_cols <- intersect(c("site_name","open_date","target"), names(pipeline_df))
    if (length(lk_cols) > 1) {
      lk <- pipeline_df[, lk_cols, drop = FALSE]
      recruiting_sites <- merge(recruiting_sites, lk, by = "site_name", all.x = TRUE)
    }
  }
  if (!"target"          %in% names(recruiting_sites)) recruiting_sites$target          <- 42
  if (!"monthly_target"  %in% names(recruiting_sites)) recruiting_sites$monthly_target  <- 2
  if (!"open_date"       %in% names(recruiting_sites)) recruiting_sites$open_date       <- NA_character_
  recruiting_sites$target[is.na(recruiting_sites$target)]                 <- 42
  recruiting_sites$monthly_target[is.na(recruiting_sites$monthly_target)] <- 2

  pre_recruit <- if (!is.null(pipeline_df) && nrow(pipeline_df) > 0 &&
                     "site_name" %in% names(pipeline_df)) {
    p <- pipeline_df[!pipeline_df$site_name %in% recruiting_sites$site_name, ]
    if (nrow(p) > 0) data.frame(
      site_name      = p$site_name,
      stage          = if ("stage"          %in% names(p)) p$stage          else "In setup",
      randomisations = 0L, source = "manual",
      target         = if ("target"         %in% names(p)) p$target         else 42L,
      monthly_target = if ("monthly_target" %in% names(p)) p$monthly_target else 2L,
      open_date      = if ("open_date"      %in% names(p)) as.character(p$open_date) else NA_character_,
      stringsAsFactors = FALSE) else recruiting_sites[0, ]
  } else recruiting_sites[0, ]

  pipeline_combined <- rbind(recruiting_sites, pre_recruit)
  site_status_table <- if (nrow(pipeline_combined) > 0) {
    st <- pipeline_combined[, c("site_name","stage","target","monthly_target","randomisations"), drop=FALSE]
    st$randomisations[is.na(st$randomisations)] <- 0
    st$progress_pct <- round(st$randomisations / st$target * 100, 1)
    st[order(-st$randomisations, st$site_name), ]
  } else data.frame(site_name=character(0), stage=character(0),
                    target=integer(0), monthly_target=integer(0),
                    randomisations=integer(0), progress_pct=numeric(0))

  # ── 20. Monthly target achievement ────────────────────────────────────────
  current_month <- format(today, "%Y-%m")
  rec_names <- if ("site_name" %in% names(recruiting_sites)) recruiting_sites$site_name else character(0)
  monthly_achievement <- if (length(rec_names) > 0 && "rand_date" %in% names(filtered)) {
    tm <- if (nrow(filtered) == 0) filtered[0, ] else
      filtered[!is.na(filtered$rand_date) &
               format(filtered$rand_date, "%Y-%m") == current_month, ]
    ac <- if (nrow(tm) > 0 && "site_name" %in% names(tm))
      aggregate(list(actual = rep(1, nrow(tm))), by = list(site_name = tm$site_name), FUN = sum)
      else data.frame(site_name = character(0), actual = integer(0))
    ma <- data.frame(site_name = rec_names, stringsAsFactors = FALSE)
    ma <- merge(ma, ac, by = "site_name", all.x = TRUE); ma$actual[is.na(ma$actual)] <- 0
    mt_df <- recruiting_sites[, c("site_name","monthly_target"), drop = FALSE]
    ma <- merge(ma, mt_df, by = "site_name", all.x = TRUE)
    ma$monthly_target[is.na(ma$monthly_target)] <- 2
    ma$month_label <- format(today, "%B %Y")
    ma[order(-ma$actual, ma$site_name), ]
  } else data.frame(site_name=character(0), actual=integer(0),
                    monthly_target=integer(0), month_label=character(0))

  # ── 21. Open sites table ──────────────────────────────────────────────────
  open_sites_table <- if (nrow(recruiting_sites) > 0 && "rand_date" %in% names(filtered)) {
    ot <- data.frame(site_name = recruiting_sites$site_name, stringsAsFactors = FALSE)
    od <- recruiting_sites[, c("site_name","open_date"), drop = FALSE]
    ot <- merge(ot, od, by = "site_name", all.x = TRUE)
    ot <- merge(ot, site_summary[, c("site_name","randomisations","first_rand_date"), drop=FALSE],
                by = "site_name", all.x = TRUE)
    ot$randomisations[is.na(ot$randomisations)] <- 0
    ot$open_date_parsed <- tryCatch(as.Date(ot$open_date), error = function(e) rep(NA_Date_, nrow(ot)))
    ot$days_to_first <- as.integer(difftime(ot$first_rand_date, ot$open_date_parsed, units = "days"))
    ot[order(-ot$randomisations, ot$site_name),
       c("site_name","open_date","randomisations","days_to_first")]
  } else data.frame(site_name=character(0), open_date=character(0),
                    randomisations=integer(0), days_to_first=integer(0))

  # ── 22. Per-site per-month heatmap ────────────────────────────────────────
  site_month_heatmap <- if (nrow(recruiting_sites) > 0 && "rand_date" %in% names(filtered)) {
    rec <- filtered[filtered$site_name %in% rec_names & !is.na(filtered$rand_date), ]
    if (nrow(rec) > 0) {
      rec$month  <- format(rec$rand_date, "%Y-%m")
      all_months <- sort(unique(rec$month))
      hm <- expand.grid(site_name = rec_names, month = all_months, stringsAsFactors = FALSE)
      act <- aggregate(list(actual = rep(1, nrow(rec))),
                       by = list(site_name = rec$site_name, month = rec$month), FUN = sum)
      hm <- merge(hm, act, by = c("site_name","month"), all.x = TRUE)
      hm$actual[is.na(hm$actual)] <- 0
      mt <- recruiting_sites[, c("site_name","monthly_target"), drop = FALSE]
      hm <- merge(hm, mt, by = "site_name", all.x = TRUE)
      hm$monthly_target[is.na(hm$monthly_target)] <- 2
      hm$month_label <- format(as.Date(paste0(hm$month, "-01")), "%b %Y")
      hm[order(hm$site_name, hm$month), ]
    } else NULL
  } else NULL

  # ── 23. CRF return rates (from CSV) ───────────────────────────────────────
  default_crf_folder <- cfg$crf_csv_default_path
  if (is.null(crf_csv_path) && !is.null(default_crf_folder)) {
    if (dir.exists(default_crf_folder)) {
      candidates <- list.files(default_crf_folder, pattern = "\\.csv$",
                               ignore.case = TRUE, full.names = TRUE)
      if (length(candidates) > 0) {
        ages <- file.info(candidates)$mtime
        crf_csv_path <- candidates[which.max(ages)]
        message("CRF: auto-detected ", basename(crf_csv_path),
                " (most recent of ", length(candidates), " files)")
      } else message("CRF: no .csv files found in ", default_crf_folder)
    } else message("CRF: default folder not accessible: ", default_crf_folder)
  }

  crf_data <- if (!is.null(crf_csv_path) && file.exists(crf_csv_path)) {
    tryCatch({
      crf <- read.csv(crf_csv_path, stringsAsFactors = FALSE, check.names = FALSE)
      names(crf) <- gsub("^\xef\xbb\xbf", "", names(crf))
      names(crf) <- trimws(names(crf))
      if (!"Site" %in% names(crf)) {
        message("CRF: loaded ", basename(crf_csv_path),
                " but no 'Site' column found. Columns: ",
                paste(names(crf), collapse = ", "))
        NULL
      } else {
        overall <- crf[trimws(crf$Site) == ".Overall", ]
        if (nrow(overall) > 0) {
          message("CRF: loaded ", nrow(overall), " overall rows from ", basename(crf_csv_path))
          overall
        } else {
          message("CRF: loaded ", basename(crf_csv_path),
                  " but found 0 rows with Site == '.Overall'.")
          NULL
        }
      }
    }, error = function(e) { message("CRF: error reading ", crf_csv_path, ": ", e$message); NULL })
  } else NULL

  # Primary-outcome completeness follows the same logic as the CRF return
  # rates by timepoint & form — entered ÷ due for the Discharge form, capped
  # at 100% — whenever a return-rate CSV is loaded. The REDCap disc_done
  # approximation above (which saturates at 100% once every entered form is
  # complete) is only the fallback when no CSV is available.
  if (!is.null(crf_data) && nrow(crf_data) > 0) {
    gcol <- function(pat) {
      m <- grep(pat, names(crf_data), ignore.case = TRUE, value = TRUE)
      if (length(m)) crf_data[[m[1]]] else NULL
    }
    ev_col   <- gcol("^(event|timepoint|time.?point|visit)")
    form_col <- gcol("^(stage|form|instrument)")
    due_v    <- suppressWarnings(as.numeric(gcol("^due")))
    ent_v    <- suppressWarnings(as.numeric(gcol("entered|received")))
    if (!is.null(form_col) && !is.null(due_v) && !is.null(ent_v)) {
      sel_form <- grepl("discharge", as.character(form_col), ignore.case = TRUE)
      sel_ev   <- if (!is.null(ev_col))
        grepl("^discharge", trimws(as.character(ev_col)), ignore.case = TRUE)
        else rep(FALSE, length(form_col))
      sel <- if (any(sel_form & sel_ev)) sel_form & sel_ev
             else if (any(sel_form)) sel_form else sel_ev
      if (any(sel)) {
        d_due <- sum(due_v[sel], na.rm = TRUE)
        d_ent <- sum(ent_v[sel], na.rm = TRUE)
        primary_outcome <- list(
          eligible = d_due, complete = d_ent,
          pct = if (d_due > 0) round(min(d_ent, d_due) / d_due * 100, 1)
                else NA_real_,
          source = "crf_due")
      }
    }
  }

  # ── 24. Demographics ──────────────────────────────────────────────────────
  dem_df   <- baseline[baseline$record_v %in% filtered$record_id, , drop = FALSE]
  age_data <- if ("age_v" %in% names(dem_df)) list(
    under_70 = sum(dem_df$age_v < 70, na.rm = TRUE),
    over_70  = sum(dem_df$age_v >= 70, na.rm = TRUE)) else NULL
  nela_data <- if ("nela_v" %in% names(dem_df)) list(
    under_5  = sum(dem_df$nela_v < 5, na.rm = TRUE),
    over_5   = sum(dem_df$nela_v >= 5, na.rm = TRUE)) else NULL

  eth_labels <- cfg$ethnicity_labels
  white_codes <- cfg$white_ethnicity_codes %||% as.character(13:17)
  eth_data <- if ("eth_v" %in% names(dem_df) && !is.null(eth_labels)) {
    valid <- dem_df[!is.na(dem_df$eth_v), ]
    if (nrow(valid) == 0) NULL else {
      valid$eth_label <- eth_labels[as.character(valid$eth_v)]
      valid$eth_label[is.na(valid$eth_label)] <-
        paste0("Unknown (", valid$eth_v[is.na(valid$eth_label)], ")")
      tbl <- as.data.frame(table(valid$eth_label), stringsAsFactors = FALSE)
      names(tbl) <- c("ethnicity","n")
      tbl$pct <- round(tbl$n / sum(tbl$n) * 100, 1)
      tbl <- tbl[order(-tbl$n), ]
      iw <- as.character(valid$eth_v) %in% white_codes
      list(table = tbl, white = sum(iw), minority = sum(!iw), total = nrow(valid))
    }
  } else NULL

  # ── 25. Withdrawals ───────────────────────────────────────────────────────
  w <- withdrawn_df
  if (!is.null(selected_sites) && length(selected_sites) > 0 &&
      !("All sites" %in% selected_sites) && "site_name" %in% names(w))
    w <- w[w$site_name %in% selected_sites, ]
  withdrawal_summary <- if (nrow(w) > 0) {
    if ("cos_date" %in% names(w))
      w <- w[order(suppressWarnings(as.Date(w$cos_date)), na.last = TRUE), ]
    log_tbl <- w[, intersect(c("cos_date","cos_label","cos_reason","site_name"), names(w)), drop = FALSE]
    list(n = nrow(w), rate = round(nrow(w)/total_randomised*100, 1), log = log_tbl)
  } else list(n = 0, rate = 0, log = NULL)

  # ── 26. Pilot criteria (PN-specific metrics only when fields are mapped) ──
  recruit_rate_pct <- if (expected_to_date > 0)
    round(total_randomised/expected_to_date*100, 1) else NA

  has_pn_fields  <- all(c("pn_start","op_dttm") %in% names(filtered))
  has_pn_late    <- "pn_late"  %in% names(filtered)
  has_pn_early   <- "pn_early" %in% names(filtered)

  interv_df  <- filtered[!is.na(filtered$trial_arm) & filtered$trial_arm == "Intervention", ]
  interv_rate <- if (nrow(interv_df) > 0 && has_pn_fields) {
    hrs <- as.numeric(difftime(interv_df$pn_start, interv_df$op_dttm, units = "hours"))
    hl  <- if (has_pn_late) !is.na(interv_df$pn_late) else rep(FALSE, nrow(interv_df))
    w48 <- !is.na(hrs) & hrs >= 0 & hrs <= 48 & !hl
    round(mean(w48) * 100, 1)
  } else NA

  # Contamination cannot be derived from allocation (the export carries no
  # randomisation-arm field). Per TMG guidance a participant counts as
  # contaminated when either PN-timing reason is completed
  # (nut_o_pn_late_rsn / nut_o_pn_early_rsn), out of all randomised.
  # NA — "not determinable" — when neither field is in the export.
  sc_df <- filtered[!is.na(filtered$trial_arm) & filtered$trial_arm == "Standard care", ]
  contam_rate <- if ((has_pn_late || has_pn_early) && nrow(filtered) > 0) {
    late  <- if (has_pn_late)  filled(filtered$pn_late)  else rep(FALSE, nrow(filtered))
    early <- if (has_pn_early) filled(filtered$pn_early) else rep(FALSE, nrow(filtered))
    round(mean(late | early) * 100, 1)
  } else NA

  crossover_rate <- if (nrow(sc_df) > 0 && has_pn_fields) {
    hsc <- as.numeric(difftime(sc_df$pn_start, sc_df$op_dttm, units = "hours"))
    round(mean(!is.na(hsc) & hsc >= 0 & hsc <= 48) * 100, 1)
  } else NA_real_

  pilot <- list(recruit_rate = recruit_rate_pct, n_sites = n_sites_active,
    fu_90_rate = fu_90_rate, fu_90_status = fu_90_status,
    fu_90_elig = elig_90, fu_90_comp = comp_90,
    interv_rate = interv_rate, contam_rate = contam_rate, crossover_rate = crossover_rate)

  # ── 27. Protocol deviations ───────────────────────────────────────────────
  # Reuses the Data-tab extractor so the report and the drill-down never
  # disagree. NULL when the trial maps no deviation form or none is recorded —
  # the report then omits the section rather than printing an empty table.
  deviation_log <- NULL
  if (exists("deviation_events", mode = "function")) {
    deviation_log <- tryCatch({
      d <- deviation_events(df)
      if (is.null(d) || nrow(d) == 0) NULL else d
    }, error = function(e) NULL)
  }
  if (!is.null(deviation_log)) {
    # Scope to the participants this report covers, and label the site from the
    # resolved participant record rather than the raw DAG column.
    deviation_log <- deviation_log[
      as.character(deviation_log$record_id) %in% as.character(filtered$record_id), ,
      drop = FALSE]
    if (nrow(deviation_log) == 0) {
      deviation_log <- NULL
    } else if ("site_name" %in% names(filtered)) {
      lk <- data.frame(record_id = as.character(filtered$record_id),
                       .site     = as.character(filtered$site_name),
                       stringsAsFactors = FALSE)
      lk <- lk[!duplicated(lk$record_id), , drop = FALSE]
      deviation_log$record_id <- as.character(deviation_log$record_id)
      deviation_log <- merge(deviation_log, lk, by = "record_id", all.x = TRUE)
      has_site <- !is.na(deviation_log$.site) & nzchar(deviation_log$.site)
      deviation_log$site[has_site] <- deviation_log$.site[has_site]
      deviation_log$.site <- NULL
      deviation_log <- deviation_log[order(deviation_log$onset_date,
                                           deviation_log$record_id,
                                           na.last = TRUE), , drop = FALSE]
    }
  }
  deviation_count <- if (is.null(deviation_log)) 0L else nrow(deviation_log)
  # Distinguishes "no deviations reported" from "this export carries no
  # deviation form", so the report can say which.
  deviation_available <- {
    dc <- fld("deviation_complete", default = "deviation_complete", cfg = cfg)
    !is.null(dc) && dc %in% names(df)
  }

  # ── Return ────────────────────────────────────────────────────────────────
  list(
    filtered_df = filtered,
    kpis = list(total_randomised = total_randomised, trial_target = trial_target,
      n_sites_active = n_sites_active, expected_to_date = expected_to_date,
      recruitment_pct = recruitment_pct, months_elapsed = round(months_elapsed, 1),
      sae_count = sae_count, report_date = format(Sys.Date(), "%d %B %Y")),
    monthly_recruit = monthly_recruit, target_schedule = target_schedule,
    fu_summary = fu_summary,
    fu_kpi = list(fu_dc_count = comp_dc, fu_dc_elig = elig_dc,
                  fu_30_count = fu_30_kpi, fu_30_elig = elig_30,
                  fu_90_count = fu_90_kpi, fu_90_elig = elig_90),
    safety_summary = safety_summary, site_summary = site_summary,
    sae_log = sae_log,
    deviation_log = deviation_log, deviation_count = deviation_count,
    deviation_available = deviation_available,
    site_status = site_status_table, monthly_achievement = monthly_achievement,
    open_sites = open_sites_table, site_month_heatmap = site_month_heatmap,
    crf_data = crf_data, pipeline = pipeline_combined,
    demographics = list(age = age_data, nela = nela_data, ethnicity = eth_data),
    baseline_df = dem_df,
    withdrawals = withdrawal_summary, pilot = pilot,
    primary_outcome = primary_outcome,
    schedule_full = schedule_full)
}
