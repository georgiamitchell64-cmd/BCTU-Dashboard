# =============================================================================
# Codebook import — what the numbers in an export actually mean
# =============================================================================
# A REDCap export carries codes, not labels: index_panc_aetio = 2, base_sex = 1.
# The meanings live in the project's data dictionary, which trial teams have in
# one of three forms — a REDCap data dictionary CSV, a PDF codebook, or their
# own notes. All three land in the same place: cfg$column_labels, a list of
# column -> named character vector of code -> label, which the participant
# views and the reports read.
# =============================================================================

# "1, Male | 2, Female | 3, Not stated" -> c("1" = "Male", ...)
# Also accepts "1=Male; 2=Female", "1) Male, 2) Female" and newline-separated
# lists, which is what a pasted or PDF-extracted codebook usually looks like.
parse_choice_string <- function(x) {
  if (is.null(x) || !length(x)) return(character(0))
  x <- as.character(x)[1]
  if (is.na(x) || !nzchar(trimws(x))) return(character(0))

  parts <- unlist(strsplit(x, "\\||\n|;"))
  parts <- trimws(parts[nzchar(trimws(parts))])
  if (!length(parts)) return(character(0))

  codes <- character(0); labels <- character(0)
  for (p in parts) {
    m <- regmatches(p, regexec("^\\s*([A-Za-z0-9._-]+)\\s*(?:,|=|\\)|:|\\.)\\s*(.+?)\\s*$", p))[[1]]
    if (length(m) < 3) next
    code <- m[2]; label <- m[3]
    if (!nzchar(code) || !nzchar(label)) next
    codes  <- c(codes, code)
    labels <- c(labels, label)
  }
  if (!length(codes)) return(character(0))
  keep <- !duplicated(codes)
  stats::setNames(labels[keep], codes[keep])
}

# Locate a column by any of several header spellings (REDCap's own headers vary
# between versions and between the API and the UI export).
.cb_col <- function(df, patterns) {
  nm <- tolower(trimws(names(df)))
  for (p in patterns) {
    hit <- which(grepl(p, nm))
    if (length(hit)) return(names(df)[hit[1]])
  }
  NULL
}

#' Read a REDCap data dictionary (CSV or TSV) into codebook form.
#' Returns list(labels = list(column -> named vector), field_labels = named
#' vector of column -> human field label, n_fields, source).
parse_redcap_dictionary <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  sep <- if (grepl("\\.tsv$", path, ignore.case = TRUE)) "\t" else ","
  df <- utils::read.csv(path, sep = sep, stringsAsFactors = FALSE,
                        check.names = FALSE, fileEncoding = "UTF-8-BOM")
  if (!ncol(df)) stop("No columns found in ", basename(path))

  var_col    <- .cb_col(df, c("^variable", "field ?name", "^field$"))
  choice_col <- .cb_col(df, c("choices", "calculations", "slider"))
  label_col  <- .cb_col(df, c("field ?label", "^label$"))
  if (is.null(var_col) || is.null(choice_col))
    stop("This does not look like a REDCap data dictionary — expected a ",
         "variable-name column and a 'Choices, Calculations, OR Slider Labels' column.")

  labels <- list(); field_labels <- character(0)
  for (i in seq_len(nrow(df))) {
    v <- trimws(as.character(df[[var_col]][i]))
    if (is.na(v) || !nzchar(v)) next
    ch <- parse_choice_string(df[[choice_col]][i])
    if (length(ch)) labels[[v]] <- as.list(ch)
    if (!is.null(label_col)) {
      fl <- trimws(as.character(df[[label_col]][i]))
      if (!is.na(fl) && nzchar(fl)) field_labels[[v]] <- fl
    }
  }
  list(labels = labels, field_labels = field_labels,
       n_fields = length(labels), source = basename(path))
}

# =============================================================================
# REDCap "Codebook" CSV — the print-out, not the data dictionary
# =============================================================================
# REDCap's Codebook page exports one row per field header followed by one row
# per choice, with the codes sitting in the attributes column. The importer
# above wants a data dictionary, so convert the layout first: the result is a
# real data dictionary data frame, which parse_redcap_dictionary() reads and
# which can be written back out for anyone who wants the CSV itself.
# =============================================================================

# REDCap lets labels carry HTML (<b>Death</b>); the dashboard shows plain text.
.cb_plain <- function(x) trimws(gsub("\\s+", " ", gsub("<[^>]+>", "", x)))

.cb_strip_zwsp <- function(x) {
  x <- gsub("\u200b|\ufeff|\u00a0", " ", x, useBytes = TRUE)
  trimws(x)
}

.CB_DD_HEADERS <- c(
  "Variable / Field Name", "Form Name", "Section Header", "Field Type",
  "Field Label", "Choices, Calculations, OR Slider Labels", "Field Note",
  "Text Validation Type OR Show Slider Number", "Text Validation Min",
  "Text Validation Max", "Identifier?",
  "Branching Logic (Show field only if...)", "Required Field?",
  "Custom Alignment", "Question Number (surveys only)", "Matrix Group Name",
  "Matrix Ranking?", "Field Annotation"
)

# "text (date_dmy, Min: [x], Max: [y])" -> type text, validation date_dmy
.cb_split_type <- function(x) {
  x <- .cb_strip_zwsp(x)
  m <- regmatches(x, regexec("^([A-Za-z_]+)\\s*\\((.*)\\)\\s*$", x))[[1]]
  if (length(m) != 3) return(list(type = x, validation = "", min = "", max = ""))
  inner <- m[3]
  vmin <- ""; vmax <- ""
  mm <- regmatches(inner, regexec("Min:\\s*([^,)]+)", inner))[[1]]
  if (length(mm) == 2) vmin <- trimws(mm[2])
  mm <- regmatches(inner, regexec("Max:\\s*([^,)]+)", inner))[[1]]
  if (length(mm) == 2) vmax <- trimws(mm[2])
  val <- trimws(unlist(strsplit(inner, ","))[1])
  if (grepl("^(Min|Max):", val)) val <- ""
  list(type = m[2], validation = val, min = vmin, max = vmax)
}

# Form header rows look like "Record Creation   record_creation"
.cb_form_name <- function(x) {
  x <- .cb_strip_zwsp(x)
  m <- regmatches(x, regexec("\\s{2,}([a-z0-9_]+)\\s*$", x))[[1]]
  if (length(m) == 2) m[2] else ""
}

redcap_codebook_to_dictionary <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  # Read as bytes and mark UTF-8 by hand: the export carries a BOM and REDCap's
  # zero-width spaces, which read.csv() refuses under a non-UTF-8 locale.
  txt <- rawToChar(readBin(path, "raw", file.info(path)$size))
  Encoding(txt) <- "UTF-8"
  raw <- utils::read.csv(text = txt, header = FALSE, stringsAsFactors = FALSE,
                         colClasses = "character", check.names = FALSE,
                         blank.lines.skip = FALSE, encoding = "UTF-8")
  if (ncol(raw) < 5) stop("Not a REDCap codebook export: expected at least 5 columns.")
  while (ncol(raw) < 7) raw[[ncol(raw) + 1]] <- ""
  for (j in seq_len(ncol(raw))) raw[[j]][is.na(raw[[j]])] <- ""

  rows <- list(); form <- ""; cur <- NULL; expect_logic <- FALSE

  flush <- function() {
    if (is.null(cur)) return(invisible(NULL))
    ch <- cur$choices
    cur$choices <- if (length(ch)) paste(sprintf("%s, %s", names(ch), unname(ch)),
                                         collapse = " | ") else ""
    rows[[length(rows) + 1L]] <<- cur
    cur <<- NULL
  }

  for (i in seq_len(nrow(raw))) {
    c1 <- .cb_strip_zwsp(raw[[1]][i]); c2 <- .cb_strip_zwsp(raw[[2]][i])
    c3 <- .cb_strip_zwsp(raw[[3]][i]); c4 <- .cb_strip_zwsp(raw[[4]][i])
    c5 <- .cb_strip_zwsp(raw[[5]][i]); c6 <- .cb_strip_zwsp(raw[[6]][i])
    c7 <- .cb_strip_zwsp(raw[[7]][i])

    # Instrument header: number column empty, "!" marker, form name in col 2
    if (!nzchar(c1) && identical(c7, "!")) {
      fn <- .cb_form_name(raw[[2]][i])
      if (nzchar(fn)) { flush(); form <- fn; expect_logic <- FALSE; next }
    }

    # Field header: "<n>,<variable>,<label>,<type>"
    if (grepl("^[0-9]+$", c1) && grepl("^[A-Za-z][A-Za-z0-9_]*$", c2)) {
      flush()
      ty <- .cb_split_type(c4)
      cur <- list(field = c2, form = form, section = "", type = ty$type,
                  label = .cb_plain(c3), choices = character(0), note = "",
                  validation = ty$validation, vmin = ty$min, vmax = ty$max,
                  identifier = "", logic = "", required = "", align = "",
                  annotation = sub("\\s*\\|\\s*$", "", c6))
      expect_logic <- FALSE
      next
    }
    if (is.null(cur)) next

    # Attribute column: "Required" / "Optional, Identifier" / alignment
    if (nzchar(c4) && !grepl("^-?[0-9]+$", c4)) {
      if (grepl("^Required\\b", c4)) cur$required <- "y"
      if (grepl("\\bIdentifier\\b", c4)) cur$identifier <- "y"
      m <- regmatches(c4, regexec("Custom alignment:\\s*(\\S+)", c4))[[1]]
      if (length(m) == 2) cur$align <- m[2]
    }

    # Choice row: code in col 4, label in col 5
    if (grepl("^-?[0-9]+$", c4) && nzchar(c5)) {
      cur$choices[[c4]] <- .cb_plain(c5)
    }

    # Branching logic sits on the row after "Show the field ONLY if:"
    if (identical(c2, "Show the field ONLY if:")) { expect_logic <- TRUE; next }
    if (expect_logic && nzchar(c2)) {
      if (!grepl("^//", c2)) { cur$logic <- c2; expect_logic <- FALSE }
    }

    # Field note printed under the label, minus REDCap's HTML styling
    if (nzchar(c3) && !grepl("^<em>", c3) && !nzchar(cur$note))
      cur$note <- gsub("<[^>]+>", "", c3)
  }
  flush()

  if (!length(rows)) stop("No fields found — is this a REDCap Codebook CSV export?")

  out <- do.call(rbind, lapply(rows, function(r) data.frame(
    r$field, r$form, r$section, r$type, r$label, r$choices, r$note,
    r$validation, r$vmin, r$vmax, r$identifier, r$logic, r$required,
    r$align, "", "", "", r$annotation,
    stringsAsFactors = FALSE, check.names = FALSE
  )))
  names(out) <- .CB_DD_HEADERS
  out
}

# Written by hand rather than with write.csv() so the file is UTF-8 whatever
# locale R is running under.
write_redcap_dictionary <- function(df, path) {
  q <- function(v) {
    v <- enc2utf8(as.character(v)); v[is.na(v)] <- ""
    paste0("\"", gsub("\"", "\"\"", v, useBytes = TRUE), "\"")
  }
  body <- if (nrow(df)) apply(vapply(df, q, character(nrow(df))), 1,
                              paste, collapse = ",") else character(0)
  lines <- c(paste(q(names(df)), collapse = ","), body)
  con <- file(path, "wb"); on.exit(close(con), add = TRUE)
  writeLines(lines, con, sep = "\r\n", useBytes = TRUE)
  invisible(path)
}

# TRUE when a CSV is the Codebook print-out rather than a data dictionary.
.cb_is_codebook_export <- function(path) {
  first <- tryCatch(suppressWarnings(readLines(path, n = 1L, warn = FALSE)),
                    error = function(e) character(0))
  if (!length(first)) return(FALSE)
  h <- tolower(first[1])
  # The print-out's own attributes header mentions "Choices, Calculations",
  # so a dictionary is told apart by its separate choices column instead.
  grepl("field attributes", h, fixed = TRUE) &&
    !grepl("choices, calculations, or slider", h, fixed = TRUE)
}

#' Read a REDCap Codebook CSV export straight into codebook form.
parse_redcap_codebook_csv <- function(path) {
  dd <- redcap_codebook_to_dictionary(path)
  labels <- list(); field_labels <- character(0)
  for (i in seq_len(nrow(dd))) {
    v <- dd[["Variable / Field Name"]][i]
    if (!nzchar(v)) next
    ch <- parse_choice_string(dd[["Choices, Calculations, OR Slider Labels"]][i])
    if (length(ch)) labels[[v]] <- as.list(ch)
    fl <- dd[["Field Label"]][i]
    if (nzchar(fl)) field_labels[[v]] <- fl
  }
  list(labels = labels, field_labels = field_labels,
       n_fields = length(labels), source = basename(path))
}

#' Read a codebook out of free text — a pasted block or the text of a PDF.
#' Recognises a field name on its own line followed by indented code/label
#' pairs, and "field: 1, Label | 2, Label" on a single line.
parse_codebook_text <- function(txt) {
  if (is.null(txt) || !length(txt)) return(list(labels = list(), n_fields = 0))
  lines <- unlist(strsplit(paste(txt, collapse = "\n"), "\n"))
  lines <- gsub("\u00a0", " ", lines)

  labels <- list(); current <- NULL
  is_pair <- function(l)
    grepl("^\\s*[A-Za-z0-9._-]+\\s*(,|=|\\)|:)\\s*\\S", l) &&
    grepl("^\\s*[0-9-]", l)

  for (l in lines) {
    if (!nzchar(trimws(l))) next

    # "field_name: 1, Label | 2, Label" — a whole field on one line
    m <- regmatches(l, regexec("^\\s*([a-zA-Z][A-Za-z0-9._]*)\\s*[:\\-]\\s*(.+)$", l))[[1]]
    if (length(m) == 3 && grepl("\\||,|=", m[3]) && length(parse_choice_string(m[3]))) {
      ch <- parse_choice_string(m[3])
      if (length(ch)) { labels[[m[2]]] <- as.list(ch); current <- NULL; next }
    }
    # A bare variable name starts a block
    if (grepl("^\\s*[a-zA-Z][A-Za-z0-9._]*\\s*$", l)) {
      current <- trimws(l); next
    }
    # "1, Label" / "1 = Label" under the current field
    if (!is.null(current) && is_pair(l)) {
      ch <- parse_choice_string(l)
      if (length(ch)) {
        existing <- labels[[current]] %||% list()
        for (k in names(ch)) existing[[k]] <- unname(ch[[k]])
        labels[[current]] <- existing
      }
    }
  }
  labels <- Filter(function(x) length(x) > 0, labels)
  list(labels = labels, field_labels = character(0),
       n_fields = length(labels), source = "pasted text")
}

# REDCap's own "Data Dictionary Codebook" PDF lays each field out as
#
#   129 [index_panc_aetio]   Aetiology of this index episode      dropdown, Required
#                                                                 1    Gallstones
#                                                                 2    Alcohol
#                                                                -99   Unknown
#
# — the variable in brackets, its label, and the code list running down the
# right-hand column, which the generic text parser cannot see because the codes
# never start a line. This reads that layout directly.
.CB_ATTR_WORDS <- paste0("^(Min|Max|Custom|Field|Section|Show|Required|",
                         "Validation|Identifier|Branching|Choices|Note|",
                         "Matrix|Alignment|Annotation|Calculation|Action|",
                         "SELECT|FROM|WHERE)\\b")
.CB_TYPE_RE <- paste0("\\b(text|notes|dropdown|radio|checkbox|yesno|truefalse|",
                      "calc|descriptive|file|slider|sql|signature)\\b")

# REDCap's codebook PDF has no glyph mapping for the fi/ff/ffi ligatures, so
# pdftools reads "confirmed" as "con!rmed", "Staff" as 'Sta"' and "Office" as
# "O%ce". Restore them, but only between letters so real punctuation survives.
.cb_fix_ligatures <- function(x) {
  x <- gsub("\ufb01", "fi", x); x <- gsub("\ufb00", "ff", x)
  x <- gsub("\ufb03", "ffi", x)
  x <- gsub("(?<=[A-Za-z])!(?=[a-z])", "fi", x, perl = TRUE)
  x <- gsub("(?<=\\s)!(?=eld)",       "fi", x, perl = TRUE)
  x <- gsub("(?<=[A-Za-z])\"(?=[a-z])", "ff", x, perl = TRUE)
  x <- gsub("(?<=[A-Za-z])%(?=[a-z])", "ffi", x, perl = TRUE)
  x
}

parse_redcap_codebook_pdf_text <- function(txt) {
  lines <- .cb_fix_ligatures(unlist(strsplit(paste(txt, collapse = "\n"), "\n")))

  field_re <- "^\\s*[0-9]+\\s+\\[([A-Za-z0-9_]+)\\]\\s*(.*)$"

  labels <- list(); field_labels <- character(0)
  current <- NULL; attr_col <- NA_integer_; last_code <- NULL
  for (l in lines) {
    m <- regmatches(l, regexec(field_re, l))[[1]]
    if (length(m) == 3) {
      current <- m[2]
      # The attributes column starts where the field type is printed. Codes are
      # listed underneath it, so remembering that column is what separates a
      # code list from the wrapped field label to its left — the gap between
      # the two can be a single space on a long label.
      tpos <- regexpr(.CB_TYPE_RE, m[3], perl = TRUE)
      attr_col <- if (tpos > 0) nchar(l) - nchar(m[3]) + tpos else NA_integer_
      lbl <- trimws(sub("\\s{3,}.*$", "", m[3]))
      if (nzchar(lbl) && !grepl(.CB_ATTR_WORDS, lbl))
        field_labels[[current]] <- lbl
      last_code <- NULL
      next
    }
    if (is.null(current)) next
    # Read only the attributes column; without one, fall back to a wide indent.
    right <- if (!is.na(attr_col) && nchar(l) >= attr_col)
      substr(l, max(1L, attr_col - 3L), nchar(l)) else
      if (grepl("^\\s{35,}\\S", l)) l else ""
    cm <- regmatches(right, regexec("^\\s*(-?[0-9]+)\\s{1,8}(\\S.*?)\\s*$", right))[[1]]
    if (length(cm) != 3) {
      # A long choice wraps onto the next line with no code of its own
      # ("English, Welsh, Scottish, Northern Irish or / British"), so the tail
      # belongs to the code above it.
      tail <- trimws(right)
      if (!is.null(last_code) && nzchar(tail) && !grepl(.CB_ATTR_WORDS, tail) &&
          !grepl("^[0-9]", tail) && nchar(tail) < 80) {
        cur <- labels[[current]]
        if (!is.null(cur) && !is.null(cur[[last_code]]))
          labels[[current]][[last_code]] <- paste(cur[[last_code]], tail)
      } else if (!nzchar(tail)) {
        # Blank lines separate choices, not fields — keep the current code.
      } else last_code <- NULL
      next
    }
    code  <- cm[2]
    label <- trimws(cm[3])
    if (!nzchar(label) || grepl(.CB_ATTR_WORDS, label)) next
    if (grepl("^[0-9]+\\)?$", label)) next
    cur <- labels[[current]] %||% list()
    if (is.null(cur[[code]])) cur[[code]] <- label
    labels[[current]] <- cur
    last_code <- code
  }

  # A single code is a checkbox or a stray match, not a code list worth keeping.
  labels <- Filter(function(x) length(x) >= 2, labels)
  list(labels = labels, field_labels = field_labels,
       n_fields = length(labels), source = "REDCap codebook PDF")
}

#' Extract the text of a PDF codebook. Needs the pdftools package; without it
#' the caller is told to paste the text instead rather than failing silently.
parse_codebook_pdf <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  if (!requireNamespace("pdftools", quietly = TRUE))
    stop("Reading a PDF codebook needs the pdftools package ",
         "(install.packages(\"pdftools\")). Until then, paste the codebook ",
         "text into the box instead.")
  txt <- pdftools::pdf_text(path)
  # REDCap's own codebook export is the common case and needs its column
  # layout read; anything else falls back to the generic text parser.
  out <- if (any(grepl("Data Dictionary Codebook|Variable / Field Name", txt)))
    parse_redcap_codebook_pdf_text(txt) else parse_codebook_text(txt)
  if (!length(out$labels)) out <- parse_codebook_text(txt)
  out$source <- basename(path)
  out
}

#' Import a codebook from whatever the user has: a REDCap dictionary CSV/TSV,
#' a PDF, or a text file. Dispatches on extension and normalises the result.
import_codebook_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("csv", "tsv")) {
    if (.cb_is_codebook_export(path))
      return(tryCatch(parse_redcap_codebook_csv(path),
                      error = function(e) parse_redcap_dictionary(path)))
    out <- tryCatch(parse_redcap_dictionary(path), error = function(e) {
      # Not a dictionary — fall back to reading it as text.
      txt <- tryCatch(readLines(path, warn = FALSE), error = function(e2) NULL)
      if (is.null(txt)) stop(conditionMessage(e))
      res <- parse_codebook_text(txt); res$source <- basename(path); res
    })
    return(out)
  }
  if (ext == "pdf") return(parse_codebook_pdf(path))
  if (ext %in% c("txt", "md", "text")) {
    out <- parse_codebook_text(readLines(path, warn = FALSE))
    out$source <- basename(path); return(out)
  }
  stop("Unsupported codebook format: .", ext,
       " — use a REDCap data dictionary CSV, a PDF, or a text file.")
}

#' Merge imported labels into a trial's existing column_labels.
#' Existing labels win unless `overwrite = TRUE`, so an import can never quietly
#' undo names a trial manager typed by hand. Returns the merged list plus counts
#' of what was added and what was left alone.
merge_codebook_labels <- function(existing, imported, overwrite = FALSE) {
  existing <- existing %||% list()
  imported <- imported %||% list()
  added <- 0L; kept <- 0L
  for (col in names(imported)) {
    cur <- existing[[col]] %||% list()
    for (code in names(imported[[col]])) {
      new_val <- as.character(imported[[col]][[code]])
      has_cur <- !is.null(cur[[code]]) && nzchar(as.character(cur[[code]]))
      if (has_cur && !overwrite) { kept <- kept + 1L; next }
      cur[[code]] <- new_val
      added <- added + 1L
    }
    existing[[col]] <- cur
  }
  list(labels = existing, n_added = added, n_kept = kept,
       n_columns = length(imported))
}

#' Restrict an imported codebook to the columns an export actually has, so a
#' 300-field dictionary doesn't bury the handful of fields in this dashboard.
codebook_for_columns <- function(labels, columns) {
  if (is.null(labels) || !length(labels)) return(list())
  if (is.null(columns) || !length(columns)) return(labels)
  labels[intersect(names(labels), columns)]
}
