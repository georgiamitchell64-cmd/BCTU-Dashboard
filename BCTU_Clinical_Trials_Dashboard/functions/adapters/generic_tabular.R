# =============================================================================
# Generic tabular adapter (CSV / Excel)
# =============================================================================
# The fallback for trials whose EDC has no dedicated adapter yet: any flat
# CSV or Excel export. Schema is fully inferred from the data. Multi-sheet
# workbooks are row-bound when sheets share a structure, otherwise the first
# sheet is used and the rest reported.
#
# This adapter is also the proof of the platform abstraction: if a plain CSV
# flows through the same mapping engine and produces a working dashboard,
# the source layer is genuinely decoupled (docs/ARCHITECTURE.md §4).
# =============================================================================

adapter_detect.generic_tabular <- function(adapter, paths) {
  ok <- grepl("\\.(csv|xlsx?)$", paths, ignore.case = TRUE)
  if (!any(ok)) return(list(confidence = 0, reasons = character(0)))
  # Always matches, at low confidence — vendor adapters should outrank it.
  list(confidence = 0.2,
       reasons = "tabular file(s); no vendor signature detected")
}

adapter_inputs.generic_tabular <- function(adapter) {
  list(list(id = "data", label = "Data file (CSV or Excel)",
            required = TRUE, extensions = c("csv", "xlsx", "xls")))
}

.read_tabular_one <- function(path) {
  if (grepl("\\.xlsx?$", path, ignore.case = TRUE)) {
    sheets <- readxl::excel_sheets(path)
    dfs <- lapply(sheets, function(s) {
      as.data.frame(readxl::read_excel(path, sheet = s),
                    stringsAsFactors = FALSE)
    })
    names(dfs) <- sheets
    dfs <- Filter(function(d) nrow(d) > 0 && ncol(d) > 0, dfs)
    if (!length(dfs)) return(NULL)
    if (length(dfs) == 1) return(dfs[[1]])
    # Row-bind sheets that share the first sheet's structure (common pattern:
    # one sheet per site/batch); otherwise take the largest sheet.
    ref <- names(dfs[[1]])
    same <- vapply(dfs, function(d) identical(names(d), ref), logical(1))
    if (all(same)) return(dplyr::bind_rows(dfs))
    dfs[[which.max(vapply(dfs, nrow, integer(1)))]]
  } else {
    adapter_read_delim(path)
  }
}

adapter_read.generic_tabular <- function(adapter, paths, options = list()) {
  paths <- paths[grepl("\\.(csv|xlsx?)$", paths, ignore.case = TRUE)]
  if (!length(paths)) adapter_error("No CSV or Excel file supplied.")
  df <- .read_tabular_one(paths[1])
  if (is.null(df) || !nrow(df))
    adapter_error("Could not read any rows from %s", basename(paths[1]))
  df <- tibble::as_tibble(df)

  schema <- infer_schema(df)

  # Longitudinal shape detection: a low-cardinality text column named like an
  # event/visit marks the export as long-format.
  ev_field <- NA_character_
  ev_candidates <- names(df)[grepl("event|visit|timepoint", names(df),
                                   ignore.case = TRUE)]
  for (cand in ev_candidates) {
    v <- unique(trimws(as.character(df[[cand]])))
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v) >= 2 && length(v) <= 30) { ev_field <- cand; break }
  }
  events <- if (!is.na(ev_field)) {
    evs <- unique(trimws(as.character(df[[ev_field]])))
    evs <- evs[!is.na(evs) & nzchar(evs)]
    tibble::tibble(unique_name = evs,
                   label = tools::toTitleCase(gsub("_", " ", evs)))
  } else {
    tibble::tibble(unique_name = character(), label = character())
  }

  list(
    source = list(
      adapter         = adapter$id,
      adapter_version = adapter$version,
      files           = basename(paths),
      fingerprint     = schema_fingerprint(schema),
      exported_at     = file.mtime(paths[1])
    ),
    schema = schema,
    events = events,
    forms  = tibble::tibble(name = character(), label = character(),
                            repeating = logical()),
    data   = list(records = df),
    conventions = list(
      completion_fields = character(0),
      completion_done   = character(0),
      event_field       = ev_field,
      id_field_first    = FALSE
    )
  )
}

register_adapter(new_adapter("generic_tabular", "Generic CSV / Excel"))
