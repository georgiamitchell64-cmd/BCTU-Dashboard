# =============================================================================
# Concept registry
# =============================================================================
# The single platform-owned list of every semantic concept the dashboard can
# consume (see docs/ARCHITECTURE.md §6.1). Mapping UIs, the suggestion engine
# and the transformer all key off this registry — adding dashboard capability
# means adding registry entries plus a module that consumes them.
#
# Each concept:
#   id         dotted identifier, stable forever (stored in trial configs)
#   label      human-readable name shown in mapping UIs
#   target     "<canonical_table>.<column>" the mapped value lands in
#   required   "platform" (dashboard cannot run without it) | "module" | "optional"
#   group      concepts that stand or fall together (e.g. safety.sae)
#   types      acceptable normalised source types (see .normalise_field_type)
#   unique     TRUE if values must be unique per participant (identifiers)
#   synonyms   lowercase names/labels seen in the wild (seeds the learned
#              synonym library; grown from confirmed mappings via
#              record_confirmed_mapping())
#   form_hints lowercase substrings of form names that boost confidence
#   legacy_role the trials/<code>/config.R redcap_fields (or redcap_events)
#              key this concept corresponds to, for config migration
# =============================================================================

.concept <- function(id, label, target, required = "optional", group = NA_character_,
                     types = "any", unique = FALSE, synonyms = character(0),
                     form_hints = character(0), legacy_role = NA_character_) {
  list(id = id, label = label, target = target, required = required,
       group = group, types = types, unique = unique,
       synonyms = tolower(synonyms), form_hints = tolower(form_hints),
       legacy_role = legacy_role)
}

#' The platform concept registry.
#' Synonym seeds consolidate the candidate lists that previously lived in
#' functions/csv_autodetect.R.
concept_registry <- function() {
  cs <- list(

    # ── Identity / structure ─────────────────────────────────────────────────
    .concept("participant.id", "Participant ID", "participants.participant_id",
             required = "platform", types = c("id", "text", "integer"), unique = TRUE,
             synonyms = c("record_id", "participant_id", "study_id", "patid",
                          "subject", "subject_id", "patient_identifier",
                          "participant id", "study id", "record id",
                          "unique participant identifier", "screening number",
                          "trial number", "trial no"),
             legacy_role = "record_id"),

    .concept("site.name", "Site name", "sites.site_name",
             required = "platform", types = c("text", "categorical"),
             synonyms = c("site_name", "site", "redcap_data_access_group",
                          "centre", "center", "hospital", "site_id",
                          "recruiting site", "centre name", "dag"),
             legacy_role = "site_name"),

    .concept("event.name", "Visit / event name", "visits.visit_id",
             required = "optional", types = c("text", "categorical"),
             synonyms = c("redcap_event_name", "event_name", "visit", "event",
                          "timepoint", "visit name"),
             legacy_role = "redcap_event_name"),

    # ── Recruitment / randomisation ─────────────────────────────────────────
    .concept("randomisation.datetime", "Randomisation date/time",
             "participants.randomised_at", required = "module",
             types = c("date", "datetime"),
             synonyms = c("rand_dttm_s", "rand_dttm", "rand_dt", "rand_date",
                          "randomisation_date", "randomization_date",
                          "date_randomised", "date_randomized",
                          "randomisation date", "date and time of randomisation",
                          "date of randomisation"),
             form_hints = c("randomisation", "randomization", "rando",
                            "treatment allocation"),
             legacy_role = "randomisation_datetime"),

    .concept("randomisation.arm", "Allocated arm / group",
             "participants.arm", types = c("categorical", "text", "integer"),
             synonyms = c("arm", "allocation", "treatment_arm", "rand_arm",
                          "treatment group", "allocated arm", "trt", "group"),
             form_hints = c("randomisation", "randomization")),

    .concept("consent.date", "Consent date", "observations",
             types = c("date", "datetime"),
             synonyms = c("consent_date", "consent_dt", "date_of_consent",
                          "date of consent", "icf_date"),
             form_hints = c("consent", "eligibility")),

    # ── Key dates ────────────────────────────────────────────────────────────
    .concept("operation.date", "Operation / intervention date", "observations",
             types = c("date", "datetime"),
             synonyms = c("iop_op_end_dt", "iop_op_end_dttm", "op_date",
                          "operation_date", "surgery_date", "procedure_date",
                          "operation date", "date of operation", "date of surgery"),
             legacy_role = "operation_date"),

    .concept("discharge.date", "Discharge date", "observations",
             types = c("date", "datetime"),
             synonyms = c("dis_discharge_day", "discharge_date", "date_discharge",
                          "discharge_dt", "date_of_discharge", "discharge date",
                          "date of discharge"),
             form_hints = c("discharge"),
             legacy_role = "discharge_date"),

    # ── Demographics ─────────────────────────────────────────────────────────
    .concept("demographics.age", "Age", "observations",
             types = c("integer", "numeric", "calc"),
             synonyms = c("age", "cae_age", "patient_age", "age_at_consent",
                          "age_years", "dem_age", "age at consent", "age (years)"),
             legacy_role = "age"),

    .concept("demographics.sex", "Sex", "observations",
             types = c("categorical", "text"),
             synonyms = c("sex", "gender", "base_sex", "dem_sex", "dem_gender",
                          "patient_sex", "sex at birth"),
             legacy_role = "sex"),

    .concept("demographics.ethnicity", "Ethnicity", "observations",
             types = c("categorical", "text"),
             synonyms = c("ethnicity", "ethnic_group", "base_ethnic_gp",
                          "race_ethnicity", "dem_ethnicity", "ethnic",
                          "ethnic group"),
             legacy_role = "ethnicity"),

    # ── Status changes / withdrawals (~SDTM DS) ──────────────────────────────
    .concept("status_change.type", "Change-of-status type",
             "status_changes.type", group = "status_change",
             types = c("categorical", "integer", "text"),
             synonyms = c("cos_type", "change_of_status", "withdrawal_type",
                          "status_change", "disposition", "withdrawal type",
                          "change of status"),
             form_hints = c("change of status", "withdrawal", "trial exit",
                            "disposition"),
             legacy_role = "cos_type"),

    .concept("status_change.date", "Change-of-status date",
             "status_changes.date", group = "status_change",
             types = c("date", "datetime"),
             synonyms = c("cos_dt", "cos_date", "withdrawal_date",
                          "date_of_withdrawal", "date of withdrawal",
                          "date of change in trial status"),
             form_hints = c("change of status", "withdrawal", "trial exit"),
             legacy_role = "cos_date"),

    .concept("status_change.reason", "Change-of-status reason",
             "status_changes.reason", group = "status_change",
             types = c("text", "notes", "categorical"),
             synonyms = c("cos_withdraw_rsn_pt", "cos_withdraw_rsn_oth",
                          "withdrawal_reason", "reason_for_withdrawal",
                          "reason for withdrawal"),
             legacy_role = "change_of_status_reason"),

    # ── Safety: SAEs (~SDTM AE) ──────────────────────────────────────────────
    .concept("safety.sae.term", "SAE term / diagnosis",
             "safety_events.term", group = "safety.sae",
             types = c("text", "categorical", "notes"),
             synonyms = c("sae_diagnosis", "sae_term", "ae_term", "diagnosis",
                          "event term", "adverse event term"),
             form_hints = c("sae", "serious adverse", "adverse event", "safety"),
             legacy_role = "sae_term"),

    .concept("safety.sae.onset_date", "SAE onset date",
             "safety_events.onset_date", group = "safety.sae",
             types = c("date", "datetime"),
             synonyms = c("sae_onset_dt", "sae_onset_date", "ae_onset_date",
                          "onset date", "date of onset"),
             form_hints = c("sae", "serious adverse", "adverse event"),
             legacy_role = "sae_onset_date"),

    .concept("safety.sae.report_date", "SAE report date",
             "safety_events.report_date", group = "safety.sae",
             types = c("date", "datetime"),
             synonyms = c("sae_reported_dt", "sae_report_date", "date_reported",
                          "date reported", "date sae reported"),
             form_hints = c("sae", "serious adverse"),
             legacy_role = "sae_report_date"),

    .concept("safety.sae.severity", "SAE severity",
             "safety_events.severity", group = "safety.sae",
             types = c("categorical", "integer"),
             synonyms = c("sae_severity", "ae_severity", "severity", "grade",
                          "ctcae_grade"),
             form_hints = c("sae", "serious adverse"),
             legacy_role = "sae_severity"),

    .concept("safety.sae.relatedness", "SAE causality / relatedness",
             "safety_events.relatedness", group = "safety.sae",
             types = c("categorical", "integer"),
             synonyms = c("sae_causality_site", "sae_causality_cent",
                          "sae_causality", "causality", "relatedness",
                          "related to treatment"),
             form_hints = c("sae", "serious adverse"),
             legacy_role = "sae_relatedness"),

    .concept("safety.sae.status", "SAE outcome / status",
             "safety_events.outcome", group = "safety.sae",
             types = c("categorical", "integer"),
             synonyms = c("sae_status", "sae_outcome", "ae_outcome", "outcome",
                          "event status"),
             form_hints = c("sae", "serious adverse"),
             legacy_role = "sae_status"),

    .concept("safety.sae.narrative", "SAE narrative",
             "safety_events.narrative", group = "safety.sae",
             types = c("notes", "text"),
             synonyms = c("sae_signssymptoms", "sae_narrative", "narrative",
                          "signs and symptoms", "description of event"),
             form_hints = c("sae", "serious adverse"),
             legacy_role = "sae_narrative"),

    # ── Protocol deviations (~SDTM DV) ───────────────────────────────────────
    .concept("deviation.term", "Deviation reference / term",
             "deviations.term", group = "deviation",
             types = c("text", "categorical", "id"),
             synonyms = c("dev_ref", "deviation_ref", "deviation_number",
                          "deviation number", "deviation type"),
             form_hints = c("deviation", "protocol dev"),
             legacy_role = "deviation_term"),

    .concept("deviation.date", "Deviation date",
             "deviations.date", group = "deviation",
             types = c("date", "datetime"),
             synonyms = c("dev_dt", "deviation_date", "date_of_deviation",
                          "date of deviation"),
             form_hints = c("deviation", "protocol dev"),
             legacy_role = "deviation_date"),

    .concept("deviation.report_date", "Deviation report date",
             "deviations.report_date", group = "deviation",
             types = c("date", "datetime"),
             synonyms = c("dev_aware", "deviation_reported", "date_team_aware",
                          "date team aware"),
             form_hints = c("deviation", "protocol dev"),
             legacy_role = "deviation_report_date"),

    .concept("deviation.narrative", "Deviation summary",
             "deviations.narrative", group = "deviation",
             types = c("notes", "text"),
             synonyms = c("dev_summary", "deviation_summary", "summary",
                          "deviation description"),
             form_hints = c("deviation", "protocol dev"),
             legacy_role = "deviation_narrative")
  )
  names(cs) <- vapply(cs, `[[`, character(1), "id")
  cs
}

#' Registry as a flat tibble (one row per concept) for UIs and joins.
concept_table <- function(registry = concept_registry()) {
  dplyr::bind_rows(lapply(registry, function(cc) {
    tibble::tibble(
      id = cc$id, label = cc$label, target = cc$target,
      required = cc$required, group = cc$group %||% NA_character_,
      unique = isTRUE(cc$unique),
      types = paste(cc$types, collapse = "|"),
      legacy_role = cc$legacy_role %||% NA_character_
    )
  }))
}

# =============================================================================
# Learned synonym library
# =============================================================================
# Confirmed mappings are recorded centrally (shared.sqlite) so that once one
# trial maps e.g. "centre_name" → site.name, later trials get that suggestion
# at high confidence. Storage is a plain table: concept_id, field_name, label,
# trial_code, confirmed_at.

synonyms_db_init <- function(db_path = file.path("data", "shared.sqlite")) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS mapping_synonyms (
      concept_id   TEXT NOT NULL,
      field_name   TEXT NOT NULL,
      field_label  TEXT,
      trial_code   TEXT,
      confirmed_at TEXT NOT NULL,
      UNIQUE(concept_id, field_name, trial_code)
    )")
  invisible(TRUE)
}

#' Record a user-confirmed concept→field mapping so future trials benefit.
record_confirmed_mapping <- function(concept_id, field_name, field_label = NULL,
                                     trial_code = NULL,
                                     db_path = file.path("data", "shared.sqlite")) {
  tryCatch({
    synonyms_db_init(db_path)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbExecute(con, "
      INSERT OR REPLACE INTO mapping_synonyms
        (concept_id, field_name, field_label, trial_code, confirmed_at)
      VALUES (?, ?, ?, ?, ?)",
      params = list(concept_id, tolower(field_name),
                    tolower(field_label %||% ""), trial_code %||% "",
                    format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    invisible(TRUE)
  }, error = function(e) {
    message("synonym record error: ", e$message)
    invisible(FALSE)
  })
}

#' Learned synonyms per concept: named list concept_id → character vector of
#' field names/labels confirmed on previous trials.
learned_synonyms <- function(db_path = file.path("data", "shared.sqlite")) {
  if (!file.exists(db_path)) return(list())
  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    if (!"mapping_synonyms" %in% DBI::dbListTables(con)) return(list())
    rows <- DBI::dbGetQuery(con, "
      SELECT concept_id, field_name, field_label FROM mapping_synonyms")
    if (!nrow(rows)) return(list())
    out <- list()
    for (i in seq_len(nrow(rows))) {
      cid <- rows$concept_id[i]
      vals <- c(rows$field_name[i],
                if (nzchar(rows$field_label[i])) rows$field_label[i])
      out[[cid]] <- unique(c(out[[cid]], vals))
    }
    out
  }, error = function(e) list())
}
