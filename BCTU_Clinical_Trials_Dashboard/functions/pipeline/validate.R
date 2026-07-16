# =============================================================================
# Canonical dataset validation
# =============================================================================
# Runs after build_canonical(), before the dashboard sees the data
# (docs/ARCHITECTURE.md §8). Three severities:
#   blocking — dashboard must not load
#   warning  — dashboard loads with a visible banner
#   info     — configuration hints
#
# Rules are entries in .VALIDATION_RULES: adding a check is an addition,
# not a pipeline change. Each rule takes the dataset and returns NULL (pass)
# or a row of (severity, code, message, count).
# =============================================================================

.v_issue <- function(severity, code, message, count = NA_integer_) {
  tibble::tibble(severity = severity, code = code,
                 message = message, count = as.integer(count))
}

.VALIDATION_RULES <- list(

  no_participants = function(ds) {
    if (nrow(ds$participants) == 0)
      .v_issue("blocking", "no_participants", "No participants in the dataset.")
  },

  duplicate_ids = function(ds) {
    d <- ds$participants$participant_id[duplicated(ds$participants$participant_id)]
    if (length(d))
      .v_issue("blocking", "duplicate_ids",
               paste0("Duplicate participant IDs: ",
                      paste(utils::head(unique(d), 5), collapse = ", "),
                      if (length(unique(d)) > 5) ", …" else ""),
               length(unique(d)))
  },

  missing_site = function(ds) {
    n <- sum(is.na(ds$participants$site_name) |
               !nzchar(ds$participants$site_name %||% ""))
    if (n > 0)
      .v_issue("warning", "missing_site",
               "Participants have no site recorded.", n)
  },

  future_randomisation = function(ds) {
    r <- ds$participants$randomised_at
    n <- sum(!is.na(r) & r > Sys.Date())
    if (n > 0)
      .v_issue("warning", "future_randomisation",
               "Randomisation dates are in the future.", n)
  },

  status_change_before_randomisation = function(ds) {
    if (!nrow(ds$status_changes)) return(NULL)
    j <- merge(ds$status_changes[, c("participant_id", "date")],
               ds$participants[, c("participant_id", "randomised_at")],
               by = "participant_id")
    n <- sum(!is.na(j$date) & !is.na(j$randomised_at) & j$date < j$randomised_at)
    if (n > 0)
      .v_issue("warning", "status_change_before_randomisation",
               "Status changes dated before randomisation.", n)
  },

  sae_missing_onset = function(ds) {
    if (!nrow(ds$safety_events)) return(NULL)
    n <- sum(is.na(ds$safety_events$onset_date))
    if (n > 0)
      .v_issue("warning", "sae_missing_onset",
               "Safety events with no parseable onset date.", n)
  },

  unmapped_status_types = function(ds) {
    if (!nrow(ds$status_changes)) return(NULL)
    n <- sum(ds$status_changes$type == "other")
    if (n > 0)
      .v_issue("info", "unmapped_status_types",
               paste0("Status changes decoded as 'other' — add decodes for ",
                      "status_change.type to classify them."), n)
  },

  no_randomisation_mapped = function(ds) {
    if (all(is.na(ds$participants$randomised_at)))
      .v_issue("info", "no_randomisation_dates",
               paste0("No randomisation dates found — recruitment charts ",
                      "will be empty until randomisation.datetime is mapped."))
  }
)

#' Validate a canonical dataset.
#' @param ds     dataset from build_canonical()
#' @param extra  issues already collected during transformation
#' @return tibble(severity, code, message, count), blocking first
validate_canonical <- function(ds, extra = NULL) {
  if (is.null(ds)) {
    base <- .v_issue("blocking", "no_dataset", "Transformation produced no dataset.")
  } else {
    rows <- lapply(.VALIDATION_RULES, function(rule) {
      tryCatch(rule(ds), error = function(e)
        .v_issue("warning", "rule_error",
                 paste0("Validation rule failed: ", e$message)))
    })
    base <- dplyr::bind_rows(Filter(Negate(is.null), rows))
  }
  out <- dplyr::bind_rows(extra, base)
  if (!nrow(out)) return(
    tibble::tibble(severity = character(), code = character(),
                   message = character(), count = integer()))
  out$severity <- factor(out$severity, levels = c("blocking", "warning", "info"))
  out <- dplyr::arrange(out, severity)
  out$severity <- as.character(out$severity)
  out
}

#' TRUE if the dataset may be published to the dashboard.
validation_passed <- function(issues) {
  !any(issues$severity == "blocking")
}

#' One-line human summary for notifications.
validation_summary <- function(issues) {
  n <- table(factor(issues$severity, levels = c("blocking", "warning", "info")))
  parts <- c(
    if (n[["blocking"]] > 0) paste0(n[["blocking"]], " blocking"),
    if (n[["warning"]]  > 0) paste0(n[["warning"]],  " warning(s)"),
    if (n[["info"]]     > 0) paste0(n[["info"]],     " hint(s)")
  )
  if (!length(parts)) "All validation checks passed" else
    paste("Validation:", paste(parts, collapse = ", "))
}
