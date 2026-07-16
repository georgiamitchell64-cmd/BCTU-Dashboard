# =============================================================================
# Source adapter API
# =============================================================================
# Contract 1 of the platform architecture (docs/ARCHITECTURE.md §4).
#
# An adapter turns one vendor's export files into a *source package* — a
# source-neutral structure holding schema metadata plus tidy raw tables.
# Adapters normalise SHAPE only; semantic mapping to dashboard concepts
# happens later in the shared pipeline (functions/pipeline/).
#
# Source package structure (all adapters MUST produce this):
#   list(
#     source  = list(adapter, adapter_version, files, fingerprint, exported_at),
#     schema  = tibble(field_name, label, form, form_label, section, type,
#                      choices <list-col>, required, branching, inferred),
#     events  = tibble(unique_name, label) — empty tibble if non-longitudinal,
#     forms   = tibble(name, label, repeating),
#     data    = list(records = tibble(...)),   # source column names preserved
#     conventions = list(...)                  # structural priors, e.g.
#       completion_fields  named chr: form → field carrying completion status
#       completion_done    value(s) meaning "complete" (REDCap: "2")
#       event_field        column holding the event name, if longitudinal
#       id_field_first     TRUE if the first column is conventionally the ID
#   )
# =============================================================================

# ── Registry ─────────────────────────────────────────────────────────────────

.ADAPTERS <- new.env(parent = emptyenv())

#' Register an adapter object (a structure with class c(<name>, "source_adapter")).
register_adapter <- function(adapter) {
  stopifnot(inherits(adapter, "source_adapter"))
  .ADAPTERS[[adapter$id]] <- adapter
  invisible(adapter)
}

#' All registered adapters, in registration order.
list_adapters <- function() {
  as.list(.ADAPTERS)
}

get_adapter <- function(id) {
  a <- .ADAPTERS[[id]]
  if (is.null(a)) stop("Unknown adapter: ", id)
  a
}

new_adapter <- function(id, label, version = "1.0.0") {
  structure(list(id = id, label = label, version = version),
            class = c(id, "source_adapter"))
}

# ── Generics ─────────────────────────────────────────────────────────────────

#' Confidence (0–1) that `paths` belong to this adapter, with reasons.
#' @return list(confidence = numeric, reasons = character vector)
adapter_detect <- function(adapter, paths) UseMethod("adapter_detect")

#' Declare the inputs this adapter expects (drives the upload UI).
#' @return list of list(id, label, required, extensions)
adapter_inputs <- function(adapter) UseMethod("adapter_inputs")

#' Read files into a source package. Throws classed errors with user-facing
#' messages (condition class "adapter_error").
adapter_read <- function(adapter, paths, options = list()) UseMethod("adapter_read")

adapter_detect.default <- function(adapter, paths) list(confidence = 0, reasons = character(0))
adapter_inputs.default <- function(adapter) {
  list(list(id = "data", label = "Data export", required = TRUE,
            extensions = c("csv", "xlsx")))
}

adapter_error <- function(msg, ...) {
  stop(structure(class = c("adapter_error", "error", "condition"),
                 list(message = sprintf(msg, ...), call = NULL)))
}

# ── Detection across all adapters ────────────────────────────────────────────

#' Run every registered adapter's detection over the supplied files and rank.
#' @return tibble(adapter, label, confidence, reasons) sorted by confidence
detect_source <- function(paths) {
  rows <- lapply(list_adapters(), function(a) {
    d <- tryCatch(adapter_detect(a, paths),
                  error = function(e) list(confidence = 0, reasons = e$message))
    tibble::tibble(adapter = a$id, label = a$label,
                   confidence = d$confidence %||% 0,
                   reasons = paste(d$reasons %||% character(0), collapse = "; "))
  })
  dplyr::arrange(dplyr::bind_rows(rows), dplyr::desc(confidence))
}

# ── Shared helpers for adapters ──────────────────────────────────────────────

#' Stable fingerprint of a source package's *schema* (names + types + choices).
#' Data content deliberately excluded — the fingerprint answers "has the
#' project structure changed since this trial was configured?" (drift, §9).
schema_fingerprint <- function(schema) {
  key <- paste(schema$field_name, schema$type,
               vapply(schema$choices, function(ch) {
                 if (is.null(ch) || !nrow(ch)) return("")
                 paste(ch$code, ch$label, sep = "=", collapse = "|")
               }, character(1)),
               sep = "\r", collapse = "\n")
  paste0("sha256:", digest::digest(key, algo = "sha256"))
}

#' Empty schema tibble in the contract shape.
empty_schema <- function() {
  tibble::tibble(field_name = character(), label = character(),
                 form = character(), form_label = character(),
                 section = character(), type = character(),
                 choices = list(), required = logical(),
                 branching = character(), inferred = logical())
}

#' Infer a normalised field type from raw data values.
#' Used when no data dictionary is available (`inferred = TRUE` rows).
infer_field_type <- function(x, n_max = 2000L) {
  v <- utils::head(x[!is.na(x)], n_max)
  v <- trimws(as.character(v))
  v <- v[nzchar(v)]
  if (!length(v)) return("text")

  frac <- function(ok) mean(ok)
  # dates: ISO or UK, with optional time part
  d <- sub("[ T].*$", "", v)
  is_iso <- grepl("^\\d{4}-\\d{2}-\\d{2}$", d)
  is_uk  <- grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", d)
  if (frac(is_iso | is_uk) > 0.9) {
    has_time <- mean(grepl("\\d[ T]\\d{1,2}:\\d{2}", v)) > 0.5
    return(if (has_time) "datetime" else "date")
  }
  num <- suppressWarnings(as.numeric(v))
  if (frac(!is.na(num)) > 0.95) {
    ints <- num[!is.na(num)]
    if (all(ints == floor(ints))) {
      # small closed code set → categorical; otherwise integer
      if (length(unique(ints)) <= 12 && length(v) > 20) return("categorical")
      return("integer")
    }
    return("numeric")
  }
  n_unique <- length(unique(v))
  if (n_unique <= max(12, 0.05 * length(v)) && length(v) > 20) return("categorical")
  if (mean(nchar(v)) > 80) return("notes")
  "text"
}

#' Build a schema purely by inference from a data frame (used by the generic
#' adapter, and by any adapter when its metadata artefact is missing).
infer_schema <- function(df) {
  if (is.null(df) || !ncol(df)) return(empty_schema())
  tibble::tibble(
    field_name = names(df),
    label      = names(df),
    form       = NA_character_,
    form_label = NA_character_,
    section    = NA_character_,
    type       = vapply(df, infer_field_type, character(1)),
    choices    = replicate(ncol(df), NULL, simplify = FALSE),
    required   = FALSE,
    branching  = NA_character_,
    inferred   = TRUE
  )
}

#' Read a delimited file with the BOM/whitespace hygiene the app already
#' applies (mirrors read_redcap_file in functions/helpers.R, kept separate so
#' the adapter layer has no dependency on legacy helpers).
adapter_read_delim <- function(filepath, nrows = -1L) {
  df <- utils::read.csv(filepath, stringsAsFactors = FALSE,
                        check.names = FALSE, nrows = nrows)
  names(df) <- gsub("^\xef\xbb\xbf", "", names(df))
  names(df) <- trimws(names(df))
  df
}
