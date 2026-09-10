# =============================================================================
# Pipeline smoke test
# =============================================================================
# Exercises the platform pipeline end-to-end without Shiny:
#   adapter detection → read → suggestions → transform → validate → store →
#   module availability → trial.json round-trip → generic CSV adapter.
#
# Run from the app directory:  Rscript tests/pipeline_smoke.R
# Exits non-zero on the first failed assertion.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(stringr)
  library(DBI); library(RSQLite); library(jsonlite)
  library(digest); library(readxl); library(rlang)
})

setwd(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))), ".."))

source("functions/adapters/adapter_api.R")
source("functions/adapters/redcap_csv.R")
source("functions/adapters/generic_tabular.R")
source("functions/pipeline/concepts.R")
source("functions/pipeline/suggest.R")
source("functions/pipeline/transform.R")
source("functions/pipeline/validate.R")
source("functions/pipeline/canonical_store.R")
source("functions/pipeline/trial_config_json.R")

tdir <- file.path(tempdir(), "pipeline_smoke")
dir.create(tdir, recursive = TRUE, showWarnings = FALSE)
say <- function(...) cat("  ✓", ..., "\n")

# ── Fixtures: a REDCap-ish trial that uses NON-TONIC names throughout ────────
# Deliberately different variable names to prove nothing is hardcoded.

dict <- data.frame(check.names = FALSE, stringsAsFactors = FALSE,
  `Variable / Field Name` = c("patid", "centre_name", "rnd_date", "rnd_group",
                              "pt_age", "pt_gender", "exit_type", "exit_date",
                              "sev_event_diag", "sev_event_onset", "sev_event_sev"),
  `Form Name`  = c("enrolment", "enrolment", "randomisation", "randomisation",
                   "enrolment", "enrolment", "trial_exit", "trial_exit",
                   "serious_adverse_event", "serious_adverse_event",
                   "serious_adverse_event"),
  `Section Header` = "",
  `Field Type` = c("text", "text", "text", "radio", "text", "radio", "radio",
                   "text", "text", "text", "radio"),
  `Field Label` = c("Unique participant identifier", "Recruiting centre",
                    "Date of randomisation", "Allocated arm", "Age at consent",
                    "Sex at birth", "Change of status type",
                    "Date of change in trial status", "Diagnosis of event",
                    "Date of onset", "Severity"),
  `Choices, Calculations, OR Slider Labels` =
    c("", "", "", "1, Intervention | 2, Control", "", "1, Male | 2, Female",
      "1, Death | 2, Complete withdrawal | 3, Lost to follow-up", "",
      "", "", "1, Mild | 2, Moderate | 3, Severe"),
  `Field Note` = "",
  `Text Validation Type OR Show Slider Number` =
    c("", "", "date_dmy", "", "integer", "", "", "date_dmy", "", "date_dmy", ""))
dict_path <- file.path(tdir, "MYTRIAL_DataDictionary.csv")
write.csv(dict, dict_path, row.names = FALSE)

export <- data.frame(check.names = FALSE, stringsAsFactors = FALSE,
  patid = c("P001", "P001", "P002", "P002", "P003", "P003", "P004"),
  redcap_event_name = c("enrolment_arm_1", "followup_1_arm_1",
                        "enrolment_arm_1", "unscheduled_arm_1",
                        "enrolment_arm_1", "followup_1_arm_1",
                        "enrolment_arm_1"),
  centre_name = c("City Hospital", "", "County General", "", "City Hospital", "", ""),
  rnd_date  = c("14/03/2026", "", "2026-04-02", "", "20/05/2026", "", ""),
  rnd_group = c("1", "", "2", "", "1", "", ""),
  pt_age    = c("64", "", "71", "", "58", "", "49"),
  pt_gender = c("2", "", "1", "", "2", "", "1"),
  exit_type = c("", "", "", "2", "", "", ""),
  exit_date = c("", "", "", "15/06/2026", "", "", ""),
  sev_event_diag  = c("", "", "", "", "", "Pneumonia", ""),
  sev_event_onset = c("", "", "", "", "", "2026-06-30", ""),
  sev_event_sev   = c("", "", "", "", "", "2", ""),
  enrolment_complete      = c("2", "", "2", "", "2", "", "0"),
  randomisation_complete  = c("2", "", "2", "", "2", "", ""),
  serious_adverse_event_complete = c("", "", "", "", "", "2", ""))
export_path <- file.path(tdir, "MYTRIAL_export.csv")
write.csv(export, export_path, row.names = FALSE)

# ── 1. Detection ─────────────────────────────────────────────────────────────
det <- detect_source(c(export_path, dict_path))
stopifnot(det$adapter[1] == "redcap_csv", det$confidence[1] >= 0.9)
stopifnot("generic_tabular" %in% det$adapter)
say("detect_source ranks redcap_csv first (", det$confidence[1], ")")

# ── 2. Adapter read: schema merges dictionary + structural columns ──────────
pkg <- adapter_read(get_adapter("redcap_csv"), c(export_path, dict_path))
stopifnot(nrow(pkg$data$records) == 7)
sch <- pkg$schema
stopifnot(all(c("patid", "rnd_date", "redcap_event_name") %in% sch$field_name))
stopifnot(sch$type[sch$field_name == "rnd_date"] == "date")
stopifnot(sch$label[sch$field_name == "patid"] == "Unique participant identifier")
stopifnot(isFALSE(sch$inferred[sch$field_name == "patid"]))
stopifnot(isTRUE(sch$inferred[sch$field_name == "redcap_event_name"]))
ch <- sch$choices[[which(sch$field_name == "sev_event_sev")]]
stopifnot(nrow(ch) == 3, ch$label[2] == "Moderate")
stopifnot(pkg$conventions$event_field == "redcap_event_name")
stopifnot("enrolment" %in% names(pkg$conventions$completion_fields))
say("redcap adapter: schema from dictionary, choices parsed, conventions set")

# ── 3. Suggestions: right fields win despite non-standard names ─────────────
sugg <- suggest_mappings(pkg)
top  <- top_suggestions(sugg)
pick <- function(cid) top$field_name[top$concept_id == cid]
stopifnot(pick("participant.id") == "patid")
stopifnot(pick("site.name") == "centre_name")
stopifnot(pick("randomisation.datetime") == "rnd_date")
stopifnot(pick("demographics.age") == "pt_age")
stopifnot(pick("demographics.sex") == "pt_gender")
stopifnot(pick("status_change.type") == "exit_type")
stopifnot(pick("safety.sae.onset_date") == "sev_event_onset")
stopifnot(all(nzchar(top$reasons)))
say("suggestions correct for", nrow(top), "concepts, all with reasons")

# ── 4. Transform via a mapping-style config ──────────────────────────────────
cfg <- list(
  code = "mytrial", name = "My Trial", short_name = "MYTRIAL",
  mapping = list(
    concepts = list(
      "participant.id"         = list(field = "patid"),
      "site.name"              = list(field = "centre_name"),
      "randomisation.datetime" = list(field = "rnd_date"),
      "randomisation.arm"      = list(field = "rnd_group"),
      "demographics.age"       = list(field = "pt_age"),
      "demographics.sex"       = list(field = "pt_gender"),
      "status_change.type"     = list(field = "exit_type"),
      "status_change.date"     = list(field = "exit_date"),
      "safety.sae.term"        = list(field = "sev_event_diag"),
      "safety.sae.onset_date"  = list(field = "sev_event_onset"),
      "safety.sae.severity"    = list(field = "sev_event_sev")
    ),
    events = list(
      baseline = list(source_events = "enrolment_arm_1"),
      day_30   = list(source_events = "followup_1_arm_1")
    )
  )
)
built <- build_canonical(pkg, cfg)
ds <- built$dataset
stopifnot(nrow(ds$participants) == 4)
p1 <- ds$participants[ds$participants$participant_id == "P001", ]
stopifnot(p1$site_name == "City Hospital",
          p1$randomised_at == as.Date("2026-03-14"),   # UK date parsed
          p1$arm == "Intervention",                     # decoded via choices
          p1$status == "randomised")
p2 <- ds$participants[ds$participants$participant_id == "P002", ]
stopifnot(p2$status == "withdrawal",                    # decoded from label
          p2$status_date == as.Date("2026-06-15"))
p4 <- ds$participants[ds$participants$participant_id == "P004", ]
stopifnot(p4$status == "screened", is.na(p4$randomised_at))
stopifnot(nrow(ds$safety_events) == 1,
          ds$safety_events$severity == "Moderate",
          ds$safety_events$onset_date == as.Date("2026-06-30"))
stopifnot(nrow(ds$status_changes) == 1,
          ds$status_changes$type == "withdrawal",
          ds$status_changes$subtype_label == "Complete withdrawal")
fs <- ds$form_status
stopifnot(sum(fs$form_concept == "enrolment" & fs$status == "complete") == 3)
stopifnot(sum(fs$form_concept == "enrolment" & fs$status == "partial") == 1)
stopifnot(all(fs$visit_id[fs$form_concept == "enrolment"] == "baseline"))
obs <- ds$observations
stopifnot("demographics.age" %in% obs$concept)
stopifnot(obs$value_label[obs$concept == "demographics.sex" &
                          obs$participant_id == "P001"] == "Female")
say("canonical build: participants, decoding, dates, dispositions, form_status")

# ── 5. Validation ─────────────────────────────────────────────────────────────
issues <- validate_canonical(ds, extra = built$issues)
stopifnot(validation_passed(issues))
stopifnot("unmapped_events" %in% issues$code)       # unscheduled_arm_1 flagged
say("validation:", validation_summary(issues))

# Blocking case: duplicate IDs
dup <- ds
dup$participants <- bind_rows(dup$participants, dup$participants[1, ])
issues_dup <- validate_canonical(dup)
stopifnot(!validation_passed(issues_dup),
          "duplicate_ids" %in% issues_dup$code)
say("validation blocks duplicate participant IDs")

# ── 6. Store round-trip ───────────────────────────────────────────────────────
db <- file.path(tdir, "mytrial.sqlite")
imp_id <- canon_save(ds, issues, db, username = "smoke", file_label = "fixture")
stopifnot(!is.na(imp_id))
back <- canon_load(db)
stopifnot(nrow(back$dataset$participants) == 4)
stopifnot(back$dataset$participants$randomised_at[1] == as.Date("2026-03-14"))
stopifnot(nrow(canon_import_history(db)) == 1)
drift <- canon_check_drift(pkg, db)
stopifnot(isFALSE(drift$changed))
say("canonical store: save/load round-trip + no false drift")

# ── 7. Module availability ────────────────────────────────────────────────────
avail <- module_availability_stub <- NULL
source("modules/registry.R")
avail <- module_availability(cfg, ds)
st <- function(id) avail$status[avail$module == id]
stopifnot(st("overview") == "enabled",
          st("randomisations") == "enabled",
          st("sites") == "enabled")
stopifnot(st("safety") == "degraded")     # relatedness/narrative unmapped
stopifnot(st("deviations") == "hidden")   # nothing mapped
stopifnot(nzchar(avail$reason[avail$module == "deviations"]))
say("module availability: enabled/degraded/hidden with reasons")

# ── 8. trial.json round-trip (legacy config → JSON → cfg) ─────────────────────
legacy_cfg <- list(code = "legacy", name = "Legacy Trial", short_name = "LEG",
                   trial_target = 100L,
                   redcap_fields = list(record_id = "patid",
                                        site_name = "centre_name",
                                        randomisation_datetime = "rnd_date"),
                   redcap_events = list(baseline = "enrolment_arm_1"),
                   features = list(projections = TRUE))
tj <- convert_config_to_trial_json(legacy_cfg)
stopifnot(length(validate_trial_json(tj)) == 0)
tj_dir <- file.path(tdir, "legacy"); dir.create(tj_dir, showWarnings = FALSE)
save_trial_json(tj_dir, tj)
tj2 <- load_trial_json(tj_dir)
cfg2 <- apply_trial_json(list(), tj2)
stopifnot(cfg2$code == "legacy",
          cfg2$redcap_fields$record_id == "patid",
          cfg2$redcap_events$baseline == "enrolment_arm_1")
m2 <- resolve_concept_mapping(cfg2)
stopifnot(m2[["participant.id"]] == "patid",
          m2[["randomisation.datetime"]] == "rnd_date")
say("trial.json: convert → save → load → apply resolves the same mapping")

# ── 9. Generic CSV adapter through the same pipeline ─────────────────────────
gen <- data.frame(check.names = FALSE, stringsAsFactors = FALSE,
  `Subject` = c("S01", "S01", "S02", "S03"),
  `Hospital` = c("Northside", "", "Northside", "Westgate"),
  `Date Randomised` = c("01/02/2026", "", "12/02/2026", "03/03/2026"),
  `Visit` = c("Baseline", "Week 4", "Baseline", "Baseline"))
gen_path <- file.path(tdir, "other_edc.csv")
write.csv(gen, gen_path, row.names = FALSE)
pkg_g <- adapter_read(get_adapter("generic_tabular"), gen_path)
stopifnot(pkg_g$schema$type[pkg_g$schema$field_name == "Date Randomised"] == "date")
stopifnot(pkg_g$conventions$event_field == "Visit")
sugg_g <- top_suggestions(suggest_mappings(pkg_g))
stopifnot(sugg_g$field_name[sugg_g$concept_id == "participant.id"] == "Subject")
stopifnot(sugg_g$field_name[sugg_g$concept_id == "site.name"] == "Hospital")
stopifnot(sugg_g$field_name[sugg_g$concept_id == "randomisation.datetime"] == "Date Randomised")
cfg_g <- list(code = "gen", mapping = list(concepts = list(
  "participant.id"         = list(field = "Subject"),
  "site.name"              = list(field = "Hospital"),
  "randomisation.datetime" = list(field = "Date Randomised"))))
built_g <- build_canonical(pkg_g, cfg_g)
stopifnot(nrow(built_g$dataset$participants) == 3,
          all(built_g$dataset$participants$status == "randomised"))
stopifnot(validation_passed(validate_canonical(built_g$dataset)))
say("generic CSV: same pipeline, working canonical dataset — abstraction holds")

cat("\nAll pipeline smoke tests passed.\n")
