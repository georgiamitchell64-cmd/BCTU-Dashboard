# =============================================================================
# REDCap adapter (CSV export + optional data dictionary)
# =============================================================================
# Reads a raw REDCap CSV export, plus — when supplied — the project's Data
# Dictionary (CSV or XLSX, the file from Project Setup → Data Dictionary).
# The dictionary is the preferred metadata source: labels, forms, types,
# choices and branching logic all come from it. Without a dictionary the
# schema is inferred from the data (`inferred = TRUE`).
# =============================================================================

# Fixed REDCap dictionary header (first 8 of 18 columns are enough to identify)
.REDCAP_DICT_COLS <- c(
  "Variable / Field Name", "Form Name", "Section Header", "Field Type",
  "Field Label", "Choices, Calculations, OR Slider Labels", "Field Note",
  "Text Validation Type OR Show Slider Number")

#' Is this file a REDCap data dictionary?
is_redcap_dictionary <- function(path) {
  hdr <- tryCatch({
    if (grepl("\\.xlsx?$", path, ignore.case = TRUE)) {
      names(readxl::read_excel(path, n_max = 0))
    } else {
      names(adapter_read_delim(path, nrows = 1))
    }
  }, error = function(e) character(0))
  length(hdr) >= 8 && all(.REDCAP_DICT_COLS[1:5] %in% hdr)
}

#' Is this file a REDCap raw data export?
is_redcap_export <- function(path) {
  if (!grepl("\\.csv$", path, ignore.case = TRUE)) return(FALSE)
  hdr <- tryCatch(names(adapter_read_delim(path, nrows = 1)),
                  error = function(e) character(0))
  if (!length(hdr)) return(FALSE)
  # Signature columns of a REDCap export
  any(c("redcap_event_name", "redcap_repeat_instrument",
        "redcap_data_access_group") %in% hdr) ||
    (hdr[1] %in% c("record_id", "participant_id") &&
       any(grepl("_complete$", hdr)))
}

#' Parse REDCap "1, Mild | 2, Moderate | 3, Severe" choice strings.
#' @return tibble(code, label) or NULL
parse_redcap_choices <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x))) return(NULL)
  parts <- strsplit(x, "|", fixed = TRUE)[[1]]
  codes <- character(0); labels <- character(0)
  for (p in parts) {
    p <- trimws(p)
    if (!nzchar(p)) next
    m <- regexpr(",", p, fixed = TRUE)
    if (m > 0) {
      codes  <- c(codes,  trimws(substr(p, 1, m - 1)))
      labels <- c(labels, trimws(substr(p, m + 1, nchar(p))))
    } else {
      codes  <- c(codes, p)
      labels <- c(labels, p)
    }
  }
  if (!length(codes)) return(NULL)
  tibble::tibble(code = codes, label = labels)
}

#' Normalise a REDCap field type + text validation into the platform type set.
normalise_redcap_type <- function(field_type, validation, has_choices) {
  ft <- tolower(trimws(field_type %||% ""))
  vl <- tolower(trimws(validation %||% ""))
  if (ft %in% c("radio", "dropdown", "yesno", "truefalse")) return("categorical")
  if (ft == "checkbox") return("checkbox")
  if (ft == "calc")     return("calc")
  if (ft == "notes")    return("notes")
  if (ft == "file")     return("file")
  if (ft %in% c("text", "")) {
    if (grepl("^datetime", vl)) return("datetime")
    if (grepl("^date", vl))     return("date")
    if (vl %in% c("integer", "number_1dp", "number_2dp", "number"))
      return(if (vl == "integer") "integer" else "numeric")
    if (has_choices) return("categorical")
    return("text")
  }
  "text"
}

#' Read a REDCap data dictionary (CSV or XLSX) into the schema contract shape.
read_redcap_dictionary <- function(path) {
  dd <- if (grepl("\\.xlsx?$", path, ignore.case = TRUE)) {
    as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE)
  } else {
    adapter_read_delim(path)
  }
  need <- .REDCAP_DICT_COLS[1:5]
  if (!all(need %in% names(dd)))
    adapter_error("File does not look like a REDCap data dictionary: %s",
                  basename(path))

  col <- function(nm) if (nm %in% names(dd)) as.character(dd[[nm]]) else
    rep(NA_character_, nrow(dd))

  choices_raw <- col("Choices, Calculations, OR Slider Labels")
  field_type  <- col("Field Type")
  validation  <- col("Text Validation Type OR Show Slider Number")
  choices     <- lapply(seq_len(nrow(dd)), function(i) {
    if (tolower(field_type[i] %||% "") == "calc") return(NULL)
    parse_redcap_choices(choices_raw[i])
  })

  tibble::tibble(
    field_name = trimws(col("Variable / Field Name")),
    label      = trimws(col("Field Label")),
    form       = trimws(col("Form Name")),
    form_label = tools::toTitleCase(gsub("_", " ", trimws(col("Form Name")))),
    section    = trimws(col("Section Header")),
    type       = vapply(seq_len(nrow(dd)), function(i) {
      normalise_redcap_type(field_type[i], validation[i],
                            !is.null(choices[[i]]))
    }, character(1)),
    choices    = choices,
    required   = tolower(col("Required Field?") %||% "") %in% "y",
    branching  = col("Branching Logic (Show field only if...)"),
    inferred   = FALSE
  )
}

# ── Adapter implementation ───────────────────────────────────────────────────

adapter_detect.redcap_csv <- function(adapter, paths) {
  reasons <- character(0); score <- 0
  if (any(vapply(paths, is_redcap_export, logical(1)))) {
    score <- 0.9
    reasons <- c(reasons, "REDCap export signature columns found")
  }
  if (any(vapply(paths, is_redcap_dictionary, logical(1)))) {
    score <- max(score, 0.95)
    reasons <- c(reasons, "REDCap data dictionary header found")
  }
  list(confidence = score, reasons = reasons)
}

adapter_inputs.redcap_csv <- function(adapter) {
  list(
    list(id = "export", label = "Raw data export (CSV)",
         required = TRUE, extensions = "csv"),
    list(id = "dictionary",
         label = "Data dictionary (Project Setup → Data Dictionary; CSV or XLSX)",
         required = FALSE, extensions = c("csv", "xlsx"))
  )
}

adapter_read.redcap_csv <- function(adapter, paths, options = list()) {
  dict_path   <- paths[vapply(paths, is_redcap_dictionary, logical(1))][1]
  export_path <- setdiff(paths, dict_path)
  export_path <- export_path[grepl("\\.csv$", export_path, ignore.case = TRUE)][1]
  if (is.na(export_path) || is.null(export_path))
    adapter_error("No CSV data export found among the supplied files.")

  df <- adapter_read_delim(export_path)
  if (!nrow(df)) adapter_error("Export is empty: %s", basename(export_path))

  redcap_package_from_df(
    df,
    files       = basename(stats::na.omit(c(export_path, dict_path))),
    dict_path   = if (!is.na(dict_path)) dict_path else NULL,
    exported_at = file.mtime(export_path),
    adapter     = adapter
  )
}

#' Build a REDCap source package from a data frame already in memory.
#' Used by adapter_read.redcap_csv and by the legacy upload flow, which reads
#' the export itself (read_redcap_file / read_wp_exports) before the adapter
#' layer sees it.
redcap_package_from_df <- function(df, files = character(0), dict_path = NULL,
                                   exported_at = Sys.time(),
                                   adapter = get_adapter("redcap_csv")) {
  # Schema: dictionary preferred, inference fallback. Structural REDCap
  # columns (redcap_event_name etc.) never appear in the dictionary, so
  # inference tops it up either way.
  schema <- if (!is.null(dict_path)) {
    read_redcap_dictionary(dict_path)
  } else {
    empty_schema()
  }
  extra <- setdiff(names(df), schema$field_name)
  # Checkbox exports expand field → field___1, field___2…; tie them back to
  # the dictionary row so labels/choices survive.
  if (length(extra) && nrow(schema)) {
    base_of <- sub("___.*$", "", extra)
    known   <- base_of %in% schema$field_name & base_of != extra
    if (any(known)) {
      cb <- schema[match(base_of[known], schema$field_name), ]
      cb$field_name <- extra[known]
      cb$label <- paste0(cb$label, " (checkbox option)")
      schema <- dplyr::bind_rows(schema, cb)
      extra  <- extra[!known]
    }
  }
  if (length(extra))
    schema <- dplyr::bind_rows(schema, infer_schema(df[, extra, drop = FALSE]))
  # Dictionary rows for fields absent from this export (e.g. survey-only
  # instruments) are kept — configuration can precede data.

  ev_field <- if ("redcap_event_name" %in% names(df)) "redcap_event_name" else NA_character_
  events <- if (!is.na(ev_field)) {
    evs <- unique(df[[ev_field]])
    evs <- evs[!is.na(evs) & nzchar(evs)]
    tibble::tibble(unique_name = evs,
                   label = tools::toTitleCase(gsub("_", " ", evs)))
  } else {
    tibble::tibble(unique_name = character(), label = character())
  }

  complete_cols <- grep("_complete$", names(df), value = TRUE)
  form_names <- unique(c(schema$form[!is.na(schema$form)],
                         sub("_complete$", "", complete_cols)))
  forms <- tibble::tibble(
    name = form_names,
    label = tools::toTitleCase(gsub("_", " ", form_names)),
    repeating = FALSE
  )
  if ("redcap_repeat_instrument" %in% names(df)) {
    rep_forms <- unique(df$redcap_repeat_instrument)
    rep_forms <- rep_forms[!is.na(rep_forms) & nzchar(rep_forms)]
    forms$repeating <- forms$name %in% rep_forms
  }

  completion_fields <- stats::setNames(complete_cols,
                                       sub("_complete$", "", complete_cols))

  list(
    source = list(
      adapter         = adapter$id,
      adapter_version = adapter$version,
      files           = files,
      fingerprint     = schema_fingerprint(schema),
      exported_at     = exported_at
    ),
    schema = schema,
    events = events,
    forms  = forms,
    data   = list(records = tibble::as_tibble(df)),
    conventions = list(
      completion_fields = completion_fields,
      completion_done   = "2",          # REDCap: 0 Incomplete, 1 Unverified, 2 Complete
      event_field       = ev_field,
      id_field_first    = TRUE
    )
  )
}

register_adapter(new_adapter("redcap_csv", "REDCap (CSV export + data dictionary)"))
