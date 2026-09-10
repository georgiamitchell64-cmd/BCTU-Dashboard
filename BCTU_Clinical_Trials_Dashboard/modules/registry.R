# =============================================================================
# Module registry & availability engine
# =============================================================================
# Modules declare the canonical concepts they need; the platform computes
# per-trial availability from the mapping + data (docs/ARCHITECTURE.md §10),
# replacing hand-maintained feature flags with explainable enablement.
#
# Status per module:
#   enabled   all `requires` satisfied
#   degraded  enabled, but some `enhanced_by` missing (module hides panels)
#   hidden    `requires` unmet — reason surfaced so users know what to map
#
# `modules.<id>.enabled: false` in trial.json (or legacy features flag FALSE)
# force-hides a module regardless of data.
# =============================================================================

.MODULE_MANIFESTS <- new.env(parent = emptyenv())

#' Register a dashboard module manifest.
register_module_manifest <- function(id, title,
                                     requires = character(0),
                                     enhanced_by = character(0),
                                     legacy_feature = NA_character_) {
  .MODULE_MANIFESTS[[id]] <- list(
    id = id, title = title,
    requires = requires, enhanced_by = enhanced_by,
    legacy_feature = legacy_feature
  )
  invisible(id)
}

list_module_manifests <- function() as.list(.MODULE_MANIFESTS)

# ── Manifests for the current dashboard tabs ─────────────────────────────────
# `requires` names either a concept id (satisfied when mapped AND populated)
# or a canonical table in the form "table:<name>" (satisfied when non-empty).

register_module_manifest("overview", "Overview",
  requires = c("participant.id"))

register_module_manifest("randomisations", "Randomisations",
  requires = c("participant.id", "randomisation.datetime"))

register_module_manifest("participants", "Participants / Data",
  requires = c("participant.id"),
  enhanced_by = c("table:form_status", "demographics.age",
                  "demographics.sex", "demographics.ethnicity"))

register_module_manifest("sites", "Sites",
  requires = c("participant.id", "site.name"))

register_module_manifest("safety", "Safety (SAEs)",
  requires = c("participant.id", "safety.sae.onset_date"),
  enhanced_by = c("safety.sae.term", "safety.sae.severity",
                  "safety.sae.relatedness", "safety.sae.narrative"))

register_module_manifest("deviations", "Protocol deviations",
  requires = c("participant.id", "deviation.date"),
  enhanced_by = c("deviation.term", "deviation.narrative"))

register_module_manifest("withdrawals", "Withdrawals / status changes",
  requires = c("participant.id", "status_change.type"),
  enhanced_by = c("status_change.date", "status_change.reason"))

register_module_manifest("postal_tracking", "Postal tracking",
  requires = c("participant.id", "operation.date"),
  legacy_feature = "postal_tracking")

register_module_manifest("return_rates", "Return rates",
  requires = c("participant.id"),
  legacy_feature = "return_rates")

register_module_manifest("projections", "Recruitment projections",
  requires = c("participant.id", "randomisation.datetime"),
  legacy_feature = "projections")

# ── Availability computation ─────────────────────────────────────────────────

#' Is one requirement satisfied by this trial's mapping + dataset?
.requirement_met <- function(req, cmap, dataset) {
  if (startsWith(req, "table:")) {
    tb <- dataset[[sub("^table:", "", req)]]
    return(!is.null(tb) && nrow(tb) > 0)
  }
  if (!(req %in% names(cmap))) return(FALSE)
  f <- cmap[[req]]
  if (is.null(f) || !nzchar(f)) return(FALSE)
  if (is.null(dataset)) return(TRUE)   # mapped; no data yet to disprove it
  # Populated check: concept present in observations, or backing a core column
  reg <- concept_registry()[[req]]
  if (!is.null(reg)) {
    tgt <- strsplit(reg$target, ".", fixed = TRUE)[[1]]
    if (length(tgt) == 2 && !is.null(dataset[[tgt[1]]])) {
      col <- dataset[[tgt[1]]][[tgt[2]]]
      if (!is.null(col))
        return(sum(!is.na(col) & nzchar(as.character(col))) > 0)
    }
  }
  obs <- dataset$observations
  if (!is.null(obs) && nrow(obs) && req %in% obs$concept) return(TRUE)
  # Mapped but not observable in canonical tables: trust the mapping
  TRUE
}

#' Compute availability of every registered module for a trial.
#' @param cfg     trial config (for force-disable flags)
#' @param dataset canonical dataset (NULL = not yet imported; availability is
#'                then computed from the mapping alone)
#' @return tibble(module, title, status, missing, reason)
module_availability <- function(cfg, dataset = NULL,
                                registry = concept_registry()) {
  cmap <- resolve_concept_mapping(cfg, registry)
  label_of <- function(req) {
    if (startsWith(req, "table:")) return(sub("^table:", "", req))
    registry[[req]]$label %||% req
  }

  rows <- lapply(list_module_manifests(), function(m) {
    # Explicit off-switch wins (trial.json modules block or legacy feature flag)
    forced_off <- isFALSE(cfg$modules[[m$id]]$enabled %||% NA) ||
      (!is.na(m$legacy_feature) &&
         isFALSE(cfg$features[[m$legacy_feature]] %||% NA))
    if (forced_off) {
      return(tibble::tibble(module = m$id, title = m$title, status = "hidden",
                            missing = "", reason = "disabled in trial settings"))
    }

    req_met <- vapply(m$requires, .requirement_met, logical(1),
                      cmap = cmap, dataset = dataset)
    if (!all(req_met)) {
      miss <- vapply(m$requires[!req_met], label_of, character(1))
      return(tibble::tibble(
        module = m$id, title = m$title, status = "hidden",
        missing = paste(miss, collapse = ", "),
        reason = paste0("map ", paste(miss, collapse = " and "), " to enable")))
    }

    enh_met <- vapply(m$enhanced_by, .requirement_met, logical(1),
                      cmap = cmap, dataset = dataset)
    if (length(enh_met) && !all(enh_met)) {
      miss <- vapply(m$enhanced_by[!enh_met], label_of, character(1))
      return(tibble::tibble(
        module = m$id, title = m$title, status = "degraded",
        missing = paste(miss, collapse = ", "),
        reason = paste0("some panels hidden — unmapped: ",
                        paste(miss, collapse = ", "))))
    }

    tibble::tibble(module = m$id, title = m$title, status = "enabled",
                   missing = "", reason = "")
  })
  dplyr::bind_rows(rows)
}
