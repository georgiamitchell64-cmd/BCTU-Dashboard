# =============================================================================
# Multi-Trial Configuration System
# =============================================================================
#
# Each trial lives in trials/<trial_code>/config.R and returns a named list.
# To add a new trial:
#   1. Copy trials/tonic/ to trials/<your_trial>/
#   2. Edit config.R with your trial's details
#   3. Drop your logo into trials/<your_trial>/www/
#   4. Restart the app — new trial appears on the selector screen
#
# =============================================================================

# =============================================================================
# Active-trial accessors
# =============================================================================
# When a trial is selected, apply_trial_globals(cfg) sets a process-global
# .TRIAL_CFG holding the full config. fld() and evt() are thin lookups that
# read from it, so call sites can just write fld("operation_date") instead of
# hardcoding "iop_op_end_dt". Both fall back to the supplied default (or the
# logical name itself) if the field/event isn't mapped.

.TRIAL_CFG <- NULL  # set by apply_trial_globals()

#' Apply a trial config to the running session.
#' Sets process globals other code reads (TRIAL_TARGET, DATA_DIR, DB_PATH,
#' .TRIAL_CFG, cos_type_labels, ethnicity_labels). Called from trial_selector
#' when a trial is picked.
apply_trial_globals <- function(cfg) {
  .TRIAL_CFG   <<- cfg
  TRIAL_TARGET <<- cfg$trial_target %||% 0L
  DATA_DIR     <<- cfg$data_dir
  DB_PATH      <<- cfg$db_path
  if (!is.null(cfg$cos_type_labels))
    cos_type_labels <<- cfg$cos_type_labels
  if (!is.null(cfg$cos_type_descriptions))
    cos_type_descriptions <<- cfg$cos_type_descriptions
  if (!is.null(cfg$ethnicity_labels))
    ethnicity_labels <<- cfg$ethnicity_labels
  invisible(cfg)
}

#' Look up a REDCap field name by logical role.
#' Reads from .TRIAL_CFG$redcap_fields. Falls back to `default` (or `name`).
#' Nested lookup (e.g. follow_up_instruments$prodigi) via "follow_up_instruments.prodigi".
fld <- function(name, default = name, cfg = .TRIAL_CFG) {
  if (is.null(cfg) || is.null(cfg$redcap_fields)) return(default)
  fields <- cfg$redcap_fields
  if (grepl("\\.", name)) {
    parts <- strsplit(name, "\\.", fixed = FALSE)[[1]]
    out <- fields
    for (p in parts) {
      if (is.null(out) || is.null(out[[p]])) return(default)
      out <- out[[p]]
    }
    return(out %||% default)
  }
  fields[[name]] %||% default
}

#' Look up a REDCap event name by logical role.
evt <- function(name, default = name, cfg = .TRIAL_CFG) {
  if (is.null(cfg) || is.null(cfg$redcap_events)) return(default)
  cfg$redcap_events[[name]] %||% default
}

#' Get the active trial config (or NULL if none selected yet).
current_trial_config <- function() .TRIAL_CFG

#' Effective recruitment target for the current dashboard view.
#' Returns the per-work-package target when a WP is active and the config
#' defines `work_package_targets` (an integer vector aligned to
#' `work_packages`); otherwise the whole-trial target. Used so per-WP
#' dashboards show a meaningful denominator.
wp_effective_target <- function(cfg, active_wp = NULL) {
  if (is.null(cfg)) return(0L)
  base <- cfg$trial_target %||% 0L
  if (is.null(active_wp)) return(base)
  wpt <- cfg$work_package_targets
  if (!is.null(wpt) && length(wpt) >= active_wp) {
    v <- suppressWarnings(as.integer(wpt[[active_wp]]))
    if (!is.na(v) && v > 0) return(v)
  }
  base
}


# Process-wide cache for discover_trials(), keyed by trials_dir. Invalidated
# automatically when any config.R / overrides.json is added, removed or changed
# (the fingerprint includes file paths + mtimes). This avoids re-sourcing every
# trial config from disk on each of the ~24 call sites across the reactive graph.
.discover_cache <- new.env(parent = emptyenv())

#' Discover all available trial configs
#' @return Named list of trial config lists, keyed by trial code
discover_trials <- function(trials_dir = file.path(getwd(), "trials")) {
  if (!dir.exists(trials_dir)) {
    message("No trials/ directory found at: ", trials_dir)
    return(list())
  }

  trial_folders <- list.dirs(trials_dir, full.names = TRUE, recursive = FALSE)

  # Fingerprint the on-disk state; return the cached result if nothing changed.
  fp_files <- c(file.path(trial_folders, "config.R"),
                file.path(trial_folders, "trial.json"),
                file.path(trial_folders, "overrides.json"))
  fp_files <- fp_files[file.exists(fp_files)]
  fp <- paste0(trials_dir, "::",
               paste(fp_files, file.mtime(fp_files), collapse = "|"))
  cached <- .discover_cache[[trials_dir]]
  if (!is.null(cached) && identical(cached$fp, fp)) return(cached$val)

  configs <- list()

  for (folder in trial_folders) {
    config_file <- file.path(folder, "config.R")
    json_file   <- file.path(folder, "trial.json")
    if (!file.exists(config_file) && !file.exists(json_file)) next

    trial_code <- basename(folder)
    cfg <- if (file.exists(config_file)) {
      tryCatch({
        env <- new.env(parent = globalenv())
        source(config_file, local = env)
        if (exists("trial_config", envir = env)) {
          env$trial_config
        } else {
          message("Warning: ", config_file, " does not define 'trial_config'")
          NULL
        }
      }, error = function(e) {
        message("Error loading trial config from ", config_file, ": ", e$message)
        NULL
      })
    } else {
      list()  # trial defined entirely by trial.json — no R code required
    }

    # Declarative config overlays code config (deep merge, JSON wins per key).
    # See functions/pipeline/trial_config_json.R for the format and migration
    # model. A folder can carry only trial.json, only config.R, or both.
    if (!is.null(cfg) && file.exists(json_file)) {
      tj <- if (exists("apply_trial_json", mode = "function"))
        load_trial_json(folder) else NULL
      if (!is.null(tj)) {
        problems <- validate_trial_json(tj)
        if (length(problems) && !length(cfg)) {
          message("Invalid trial.json for ", trial_code, ": ",
                  paste(problems, collapse = "; "))
          cfg <- NULL
        } else {
          if (length(problems))
            message("trial.json issues for ", trial_code, " (using config.R values): ",
                    paste(problems, collapse = "; "))
          cfg <- apply_trial_json(cfg, tj)
        }
      } else if (!length(cfg)) {
        cfg <- NULL   # json-only trial but trial.json unreadable
      }
    }

    if (!is.null(cfg)) {
      # Ensure required fields
      cfg$code       <- cfg$code %||% trial_code
      cfg$trial_dir  <- folder
      cfg$data_dir   <- cfg$data_dir %||% file.path(folder, "data")
      cfg$db_path    <- cfg$db_path %||% file.path(cfg$data_dir, paste0(trial_code, ".sqlite"))
      cfg$logo_file  <- cfg$logo_file %||% file.path(folder, "www", "logo.jpg")

      # Apply overrides.json if present (Stage 5 — JSON overlay)
      if (exists("apply_overrides", mode = "function")) {
        cfg <- apply_overrides(cfg)
      }

      configs[[trial_code]] <- cfg
    }
  }

  .discover_cache[[trials_dir]] <- list(fp = fp, val = configs)
  configs
}


#' Validate a trial config has all required fields
#' @return Character vector of missing fields (empty if valid)
validate_trial_config <- function(cfg) {
  required <- c("code", "name", "short_name", "trial_target",
                 "redcap_events", "redcap_fields")
  missing <- required[!required %in% names(cfg)]

  # Check nested requirements

  if ("redcap_events" %in% names(cfg)) {
    ev_req <- c("baseline", "discharge")
    ev_missing <- ev_req[!ev_req %in% names(cfg$redcap_events)]
    if (length(ev_missing) > 0)
      missing <- c(missing, paste0("redcap_events$", ev_missing))
  }

  if ("redcap_fields" %in% names(cfg)) {
    fld_req <- c("record_id", "site_name", "randomisation_datetime")
    fld_missing <- fld_req[!fld_req %in% names(cfg$redcap_fields)]
    if (length(fld_missing) > 0)
      missing <- c(missing, paste0("redcap_fields$", fld_missing))
  }

  missing
}


#' Create a blank trial config template (for new trials)
#' @param trial_code Short lowercase code (e.g. "mytrial")
#' @param output_dir Where to create the trial folder
create_trial_template <- function(trial_code, output_dir = file.path(getwd(), "trials")) {
  trial_dir <- file.path(output_dir, trial_code)
  if (dir.exists(trial_dir)) {
    message("Trial folder already exists: ", trial_dir)
    return(invisible(trial_dir))
  }

  dir.create(file.path(trial_dir, "www"), recursive = TRUE)
  dir.create(file.path(trial_dir, "data"), recursive = TRUE)

  template <- '
# =============================================================================
# Trial Configuration: %s
# =============================================================================
# Copy this file, fill in your trial details, and place your logo in www/
#
# Required fields are marked with # REQUIRED
# Optional fields have sensible defaults
# =============================================================================

trial_config <- list(

  # ── Identity ───────────────────────────────────────────────────────────────
  code       = "%s",                          # REQUIRED: lowercase, no spaces

  name       = "My Trial Full Name",          # REQUIRED: display name
  short_name = "%s",                          # REQUIRED: abbreviation
  trial_target = 100L,                        # REQUIRED: total recruitment target

  # ── Branding (optional — falls back to app defaults) ───────────────────────
  logo_file  = NULL,                          # Path to logo image, or NULL
  colors = list(
    primary   = "#1B4F6B",                    # Navy
    secondary = "#2EC4A5",                    # Teal
    accent    = "#F59E0B"                     # Amber
  ),

  # ── Data source ────────────────────────────────────────────────────────────
  data_dir = NULL,                            # NULL = trials/<code>/data/
  # If your data lives on a network drive, set the full path:
  # data_dir = "K:/BCTU/Teams/MyTeam/MyTrial/data",

  # ── REDCap events ──────────────────────────────────────────────────────────
  # Map your REDCap event names to standard roles.
  # Only baseline is strictly required; others enable more dashboard features.
  redcap_events = list(
    baseline  = "baseline_arm_1",             # REQUIRED
    discharge = "discharge_arm_1",            # recommended
    day_30    = "day_30_arm_1",               # optional
    day_90    = "day_90_arm_1",               # optional
    sub_forms = c("sub_forms_arm_1",          # optional: SAE/COS events
                  "ad_hoc_arm_1")
  ),

  # ── REDCap field mappings ──────────────────────────────────────────────────
  # Map your REDCap variable names to the roles the dashboard expects.
  # Only record_id, site_name and randomisation_datetime are required.
  redcap_fields = list(
    record_id               = "record_id",           # REQUIRED
    site_name               = "site_name",           # REQUIRED
    randomisation_datetime  = "rand_dttm_s",         # REQUIRED

    # Dates
    operation_date          = "iop_op_end_dt",
    operation_datetime      = "iop_op_end_dttm",
    discharge_date          = "dis_discharge_day",

    # Demographics (optional — enables demographic cards)
    age                     = "cae_age",
    sex                     = "base_sex",
    ethnicity               = "base_ethnic_gp",
    nela_score              = "base_nela_score_mort",

    # Follow-up instrument completion fields (optional)
    # Set to NULL or omit instruments your trial does not have.
    follow_up_instruments = list(
      prodigi              = "prodigi_complete",
      eq5d                 = "eq5d_complete",
      qor15                = "qor15_complete",
      patient_satisfaction = "patient_satisfaction_complete",
      hruq                 = "hruq_complete"
    ),

    # Change of status / withdrawals (optional)
    cos_type                  = "cos_type",
    change_of_status_complete = "trial_exit_change_of_status_complete",
    change_of_status_date     = "cos_dt",
    change_of_status_reason      = "cos_withdraw_rsn_oth",  # free text (code 99 = Other)
    change_of_status_reason_code = "cos_withdraw_rsn_pt",   # coded reason

    # SAE detail log (optional — columns shown on the safety page).
    # Coded values (soc/category/severity) render via the default TONIC
    # label lists; override with sae_soc_labels / sae_category_labels /
    # sae_severity_labels at the top level of this config if they differ.
    sae_reported_date         = "sae_reported_dt",
    sae_diagnosis             = "sae_diagnosis",
    sae_soc                   = "sae_soc",
    sae_category              = "sae_category",
    sae_severity              = "sae_severity",
    sae_expectedness          = "sae_expectedness",
    sae_causality             = "sae_causality_cent",
    sae_death                 = "sae_death_yn",
    sae_death_date            = "sae_death_dt",

    # Intervention-specific (optional — enables crossover/adherence tracking)
    pn_start_datetime       = NULL,
    pn_late_reason          = "nut_o_pn_late_rsn",
    pn_no_line_reason       = NULL,
    pn_early_reason         = "nut_o_pn_early_rsn",

    # Postal tracking (optional)
    contact_preference      = NULL
  ),

  # ── COS type labels ───────────────────────────────────────────────────────
  cos_type_labels = c(
    "1" = "Death",
    "2" = "No Operation",
    "3" = "Part withdrawal",
    "4" = "Complete withdrawal",
    "5" = "Lost to follow-up"
  ),

  # ── Ethnicity labels (optional — for demographic cards) ────────────────────
  ethnicity_labels = NULL,   # Named character vector, or NULL to skip

  # ── Target schedule (optional — enables protocol plan line on charts) ──────
  # Data frame with month_date (Date) and cumulative_target (integer)
  target_schedule = NULL,

  # ── Participant table layout ───────────────────────────────────────────────
  # Defines which instruments appear under which timepoint headers.
  # Set to NULL to use a simple default based on follow_up_instruments.
  participant_table_layout = NULL,

  # ── Report defaults (optional — pre-fills the report generator) ────────────
  report_defaults = list(
    ci              = "",
    sponsor         = "",
    sponsor_ref     = "",
    funder          = "",
    funder_ref      = "",
    isrctn          = "",
    iras            = ""
  ),

  # ── Feature flags ──────────────────────────────────────────────────────────
  features = list(
    postal_tracking  = FALSE,     # Enable postal tracking tab
    return_rates     = FALSE,     # Enable return rates tab
    projections      = TRUE,      # Enable recruitment projections
    pilot_criteria   = FALSE,     # Enable pilot progression criteria
    consort_flow     = FALSE,     # Enable CONSORT flow diagram
    baseline_table   = FALSE      # Enable baseline characteristics table
  )
)
'

  writeLines(sprintf(template, toupper(trial_code), trial_code, toupper(trial_code)),
             file.path(trial_dir, "config.R"))

  message("Created trial template at: ", trial_dir)
  message("Next steps:")
  message("  1. Edit ", file.path(trial_dir, "config.R"))
  message("  2. Place your logo in ", file.path(trial_dir, "www/"))
  message("  3. Restart the app")


  invisible(trial_dir)
}
