# =============================================================================
# Modifications register — shared by the Modifications tab and the reports
# =============================================================================
# The Modifications tab (modules/modifications*.R) keeps each trial's register
# in cfg$modifications. The TSC report's amendment tables, the NIHR report's
# Amendments section and the Reports tab all read it through these helpers, so
# what is entered or imported on that tab is what the reports show.
#
# Amendments typed into the Reports tab's old editor (cfg$amendments) are still
# included until they are moved into the register from the Modifications tab;
# they are matched on reference so nothing appears twice.
# =============================================================================

.MOD_SUB_CODES  <- c("substantial_a", "substantial_b", "substantial_c")
.MOD_CAT_LABELS <- c(non_substantial  = "Minor / non-substantial",
                     important_detail = "Modification of important detail",
                     substantial_a    = "Substantial — Category A",
                     substantial_b    = "Substantial — Category B",
                     substantial_c    = "Substantial — Category C")

.mod_chr <- function(x) {
  x <- as.character(unlist(x))
  if (!length(x) || is.na(x[1])) "" else trimws(x[1])
}
.mod_date <- function(x) {
  x <- .mod_chr(x)
  if (!nzchar(x)) return(as.Date(NA))
  suppressWarnings(tryCatch(as.Date(x), error = function(e) as.Date(NA)))
}

# ── The register, including old-editor amendments ───────────────────────────
# An amendment from the Reports tab's old editor, in register form. The old
# editor only knew Substantial / Non-substantial, so a substantial one is
# filed as Category A and flagged (category_unknown) until someone checks it.
.legacy_amendment_as_mod <- function(a, i = 1L) {
  sub <- identical(.mod_chr(a$type), "Substantial")
  st  <- switch(.mod_chr(a$status), Approved = "Approved", Rejected = "Rejected / unfavourable",
                Withdrawn = "Withdrawn", "Submitted")
  d   <- .mod_chr(a$date)
  ref <- .mod_chr(a$ref)
  list(id = paste0("legacy_", i), ref = ref,
       category = if (sub) "substantial_a" else "non_substantial", category_unknown = sub,
       title = if (nzchar(ref)) ref else sprintf("Amendment %d", i),
       description = .mod_chr(a$description),
       change_types = character(0), review_bodies = character(0), documents = character(0),
       date_prepared = "", date_submitted = d, date_rfi = "",
       date_approved = if (identical(st, "Approved")) d else "", date_implemented = "",
       iras_ref = "", rec_ref = "", mhra_ref = "", sponsor_authorised_by = "", status = st,
       notes = if (sub) "Moved from the Reports tab, where it was recorded as Substantial with no category. Filed as Category A — check and correct."
               else "Moved from the Reports tab.")
}

# Old-editor amendments that are not in the register yet (matched on reference)
legacy_amendments_pending <- function(cfg) {
  old <- cfg$amendments
  if (is.null(old) || !length(old)) return(list())
  refs <- tolower(vapply(cfg$modifications %||% list(), function(m) .mod_chr(m$ref), ""))
  Filter(function(a) { r <- tolower(.mod_chr(a$ref)); !nzchar(r) || !r %in% refs }, old)
}

trial_modification_items <- function(cfg) {
  old <- legacy_amendments_pending(cfg)
  c(cfg$modifications %||% list(),
    lapply(seq_along(old), function(i) .legacy_amendment_as_mod(old[[i]], i)))
}

# One row per modification, ready for any report: reference, type label,
# whether it is substantial, the date to show (submitted, else approved, else
# prepared), status (with the approval date when approved), title, description.
modifications_register_rows <- function(cfg) {
  items <- trial_modification_items(cfg)
  empty <- data.frame(ref = character(0), type = character(0), substantial = logical(0),
                      date = character(0), status = character(0), title = character(0),
                      description = character(0), summary = character(0),
                      sort_date = as.Date(character(0)),
                      stringsAsFactors = FALSE)
  if (!length(items)) return(empty)
  rows <- lapply(items, function(m) {
    code <- .mod_chr(m$category)
    sub  <- code %in% .MOD_SUB_CODES
    d <- .mod_date(m$date_submitted)
    if (is.na(d)) d <- .mod_date(m$date_approved)
    if (is.na(d)) d <- .mod_date(m$date_prepared)
    lab <- unname(.MOD_CAT_LABELS[code])
    type <- if (isTRUE(m$category_unknown) && sub) "Substantial"
            else if (is.na(lab)) "Unclassified" else lab
    st <- .mod_chr(m$status); if (!nzchar(st)) st <- "Draft"
    appr <- .mod_date(m$date_approved)
    if (st %in% c("Approved", "Approved with conditions", "Implemented") && !is.na(appr))
      st <- paste(st, format(appr, "%d %b %Y"))
    title <- .mod_chr(m$title); desc <- .mod_chr(m$description)
    # Title and description once each: an imported title is often the start
    # of the description, so show the description alone then
    summary <- if (!nzchar(desc)) title
               else if (!nzchar(title) || startsWith(desc, title)) desc
               else paste0(title, ". ", desc)
    data.frame(ref = .mod_chr(m$ref), type = type, substantial = sub,
               date = if (is.na(d)) "" else format(d, "%d %b %Y"), status = st,
               title = title, description = desc, summary = summary,
               sort_date = d, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out[order(is.na(out$sort_date), out$sort_date, out$ref), , drop = FALSE]
}

# The TSC report's two amendment tables: Date / Description / Status. Minor
# and important-detail modifications go in the non-substantial table.
modifications_report_df <- function(cfg, kind = c("substantial", "non_substantial")) {
  kind <- match.arg(kind)
  r <- modifications_register_rows(cfg)
  r <- r[if (kind == "substantial") r$substantial else !r$substantial, , drop = FALSE]
  if (!nrow(r)) return(NULL)
  # "REF — summary (Category A)"; the reference is left off when it is the title
  prefix <- ifelse(nzchar(r$ref) & r$ref != r$title, paste0(r$ref, " — "), "")
  extra  <- ifelse(grepl("Category", r$type), paste0(" (", sub("^Substantial — ", "", r$type), ")"),
            ifelse(r$type == .MOD_CAT_LABELS[["important_detail"]], " (important detail)", ""))
  text   <- ifelse(nzchar(r$summary), r$summary, r$ref)
  data.frame(date = r$date, description = paste0(prefix, text, extra), status = r$status,
             stringsAsFactors = FALSE)
}

# ── Import ───────────────────────────────────────────────────────────────────
# Target fields, with the column headings each is recognised by (matched on a
# lower-case, punctuation-free version of the heading). The dashboard's own
# CSV export matches every field.
MOD_IMPORT_FIELDS <- list(
  ref                   = list(label = "Reference",             pat = c("^ref", "reference", "^mod(ification)?( no| number| id| ref)?$", "^amendment( no| number| id| ref)?$", "^(sa|nsa|sm) ?no", "^id$", "^no$", "number$")),
  title                 = list(label = "Title",                 pat = c("^title$", "short title", "^name$", "^summary$", "subject", "title")),
  description           = list(label = "Description",           pat = c("description", "rationale", "detail", "summary of change", "^changes?$")),
  category              = list(label = "Category",              pat = c("^category$", "classification", "categor", "^type$", "amendment type", "modification type", "substantial")),
  status                = list(label = "Status",                pat = c("^status$", "outcome", "decision$", "^state$", "status")),
  date_prepared         = list(label = "Date prepared",         pat = c("prepared", "created", "drafted")),
  date_submitted        = list(label = "Date submitted",        pat = c("submitted", "submission", "date sent", "^sent")),
  date_rfi              = list(label = "RFI received",          pat = c("rfi", "further information")),
  date_approved         = list(label = "Date approved",         pat = c("approved", "approval", "favourable", "decision date")),
  date_implemented      = list(label = "Date implemented",      pat = c("implement")),
  iras_ref              = list(label = "IRAS / HRA reference",  pat = c("iras", "hra ref")),
  rec_ref               = list(label = "REC reference",         pat = c("^rec", "ethics ref")),
  mhra_ref              = list(label = "MHRA reference",        pat = c("mhra")),
  sponsor_authorised_by = list(label = "Sponsor authorised by", pat = c("sponsor auth", "authorised by")),
  change_types          = list(label = "Change types",          pat = c("change types?")),
  review_bodies         = list(label = "Review bodies",         pat = c("review bod")),
  documents             = list(label = "Affected documents",    pat = c("documents?")),
  notes                 = list(label = "Notes",                 pat = c("^notes?$", "comments?"))
)

.mod_norm <- function(h) trimws(gsub("\\s+", " ", gsub("[^a-z0-9]+", " ", tolower(h))))

# Best guess at which column feeds which field. Specific fields claim their
# column first, so "IRAS number" is not taken as the modification number.
mod_import_guess <- function(headers) {
  norm  <- .mod_norm(headers)
  taken <- character(0); out <- list()
  order <- c("iras_ref", "rec_ref", "mhra_ref", "sponsor_authorised_by", "date_prepared",
             "date_submitted", "date_rfi", "date_approved", "date_implemented", "change_types",
             "review_bodies", "documents", "ref", "category", "status", "description", "title", "notes")
  for (f in order) {
    for (p in MOD_IMPORT_FIELDS[[f]]$pat) {
      i <- which(grepl(p, norm, perl = TRUE) & !headers %in% taken)
      if (length(i)) { out[[f]] <- headers[i[1]]; taken <- c(taken, headers[i[1]]); break }
    }
  }
  if (is.null(out$date_submitted)) {           # a lone "Date" column
    i <- which(norm == "date" & !headers %in% taken)
    if (length(i)) out$date_submitted <- headers[i[1]]
  }
  out
}

# Read a CSV or Excel file as text, dropping rows that are entirely blank
mod_import_read <- function(path, filename = basename(path)) {
  ext <- tolower(tools::file_ext(filename))
  df <- if (ext %in% c("xlsx", "xls", "xlsm")) {
    as.data.frame(readxl::read_excel(path, col_types = "text", .name_repair = "minimal"),
                  stringsAsFactors = FALSE)
  } else {
    rd <- function(enc) utils::read.csv(path, colClasses = "character", check.names = FALSE,
                                        na.strings = c("", "NA"), fileEncoding = enc,
                                        stringsAsFactors = FALSE)
    tryCatch(rd("UTF-8-BOM"), error = function(e) rd("latin1"))
  }
  if (!ncol(df)) return(data.frame())
  nm <- trimws(names(df)); nm[is.na(nm) | !nzchar(nm)] <- "Column"
  names(df) <- make.unique(nm)
  df[] <- lapply(df, function(x) { x <- trimws(as.character(x)); x[is.na(x)] <- ""; x })
  if (!nrow(df)) return(df)
  df[apply(df, 1, function(r) any(nzchar(r))), , drop = FALSE]
}

# Dates in any usual UK form, or Excel's day numbers, to ISO text ("" if none)
.mod_parse_date <- function(x) {
  x <- trimws(as.character(x)); x[is.na(x)] <- ""
  out <- rep("", length(x))
  num <- nzchar(x) & grepl("^[0-9]+(\\.0+)?$", x)
  if (any(num)) {
    n <- suppressWarnings(as.numeric(x[num]))
    out[num] <- ifelse(!is.na(n) & n > 20000 & n < 80000,
                       format(as.Date(n, origin = "1899-12-30")), "")
  }
  rest <- nzchar(x) & !num
  if (any(rest)) {
    d <- suppressWarnings(lubridate::parse_date_time(
      x[rest], orders = c("Ymd", "dmY", "dmy", "dbY", "dBY", "bdY", "Ymd HMS", "dmY HMS", "dmY HM"),
      quiet = TRUE))
    out[rest] <- ifelse(is.na(d), "", format(as.Date(d)))
  }
  out
}

# Category codes from free text: codes and labels, "non-substantial"/"minor",
# "important detail", "Cat A/B/C"; a bare "Substantial" takes `default_sub`.
# Returns "" where nothing is recognised.
.mod_parse_category <- function(x, default_sub = "substantial_a") {
  s <- tolower(trimws(as.character(x))); s[is.na(s)] <- ""
  out <- rep("", length(s))
  code <- s %in% names(.MOD_CAT_LABELS); out[code] <- s[code]
  lab  <- match(s, tolower(.MOD_CAT_LABELS)); hit <- !code & !is.na(lab)
  out[hit] <- names(.MOD_CAT_LABELS)[lab[hit]]
  todo <- !nzchar(out) & nzchar(s)
  imp  <- todo & grepl("important", s)
  non  <- todo & !imp & grepl("non|minor|^ns|nsa", s)
  out[imp] <- "important_detail"; out[non] <- "non_substantial"
  for (l in c("a", "b", "c")) {
    h <- !nzchar(out) & todo &
      (grepl(sprintf("cat(egory)?\\.?\\s*%s\\b", l), s, perl = TRUE) |
       grepl(sprintf("substantial\\W*%s\\b", l), s, perl = TRUE))
    out[h] <- paste0("substantial_", l)
  }
  plain <- !nzchar(out) & todo & grepl("substantial|major|^sa$|^sm$", s)
  out[plain] <- default_sub
  out
}

# Status from free text; where the file gives none, from the dates
.mod_parse_status <- function(x, d_sub, d_app, d_imp) {
  s <- tolower(trimws(as.character(x))); s[is.na(s)] <- ""
  out <- rep("", length(s))
  ex  <- match(s, tolower(MOD_STATUSES)); out[!is.na(ex)] <- MOD_STATUSES[ex[!is.na(ex)]]
  rule <- function(p, v) { h <- !nzchar(out) & nzchar(s) & grepl(p, s, perl = TRUE); out[h] <<- v }
  rule("condition", "Approved with conditions")
  rule("implement", "Implemented")
  rule("reject|unfavourable|refus|not approved", "Rejected / unfavourable")
  rule("withdr", "Withdrawn")
  rule("approv|favourable|accepted|granted", "Approved")
  rule("rfi|further info|quer", "Awaiting further information (RFI)")
  rule("draft|prepar|not (yet )?submitted", "Draft")
  rule("review|pending|awaiting|in progress|with (the )?(rec|hra|mhra|ethics|sponsor|committee)", "Under review")
  rule("submit|sent", "Submitted")
  rule("lock", "Locked for submission")
  blank <- !nzchar(out)
  out[blank & nzchar(d_imp)] <- "Implemented"
  out[blank & !nzchar(out) & nzchar(d_app)] <- "Approved"
  out[blank & !nzchar(out) & nzchar(d_sub)] <- "Submitted"
  out[!nzchar(out)] <- "Draft"
  out
}

# Split a cell of several values and keep those on the tab's own lists
.mod_split_match <- function(x, choices) {
  x <- .mod_chr(x)
  if (!nzchar(x)) return(list(match = character(0), other = character(0)))
  parts <- trimws(unlist(strsplit(x, "[;|\n]")))
  if (length(parts) == 1) parts <- trimws(unlist(strsplit(parts, ",")))
  parts <- parts[nzchar(parts)]
  m <- match(tolower(parts), tolower(choices))
  list(match = unique(choices[m[!is.na(m)]]), other = parts[is.na(m)])
}

# Turn the file's rows into register items, using `mapping` (field → column)
mod_import_build <- function(df, mapping, default_sub = "substantial_a") {
  col <- function(f) {
    h <- mapping[[f]]
    if (is.null(h) || !nzchar(h) || !h %in% names(df)) rep("", nrow(df)) else as.character(df[[h]])
  }
  d_prep <- .mod_parse_date(col("date_prepared")); d_sub <- .mod_parse_date(col("date_submitted"))
  d_rfi  <- .mod_parse_date(col("date_rfi"));      d_app <- .mod_parse_date(col("date_approved"))
  d_imp  <- .mod_parse_date(col("date_implemented"))
  cat_raw <- col("category")
  cat     <- .mod_parse_category(cat_raw, default_sub)
  no_cat  <- !nzchar(cat); cat[no_cat] <- "non_substantial"
  status  <- .mod_parse_status(col("status"), d_sub, d_app, d_imp)
  ref <- col("ref"); desc <- col("description"); title <- col("title")
  # A file with only a description still gets a usable title
  # (cut at the end of the first sentence, so "Protocol v3.0: ..." stays whole)
  title <- ifelse(nzchar(title), title,
           ifelse(nzchar(desc), substr(sub("(\\.\\s|\n).*$", "", desc), 1, 90), ref))
  keep <- nzchar(title) | nzchar(desc) | nzchar(ref)
  items <- lapply(which(keep), function(i) {
    ct <- .mod_split_match(col("change_types")[i],  MOD_CHANGE_TYPES)
    rb <- .mod_split_match(col("review_bodies")[i], MOD_REVIEW_BODIES)
    dc <- .mod_split_match(col("documents")[i],     MOD_DOCUMENTS)
    notes <- col("notes")[i]
    other <- c(ct$other, rb$other, dc$other)
    if (length(other))
      notes <- trimws(paste(notes, sprintf("Imported, not on the tab's lists: %s.", paste(other, collapse = "; "))))
    if (no_cat[i] && nzchar(cat_raw[i]))
      notes <- trimws(paste(notes, sprintf("Imported category \"%s\" was not recognised, so it is filed as minor — check.", cat_raw[i])))
    list(ref = ref[i], category = cat[i], title = title[i], description = desc[i],
         change_types = ct$match, review_bodies = rb$match, documents = dc$match,
         date_prepared = d_prep[i], date_submitted = d_sub[i], date_rfi = d_rfi[i],
         date_approved = d_app[i], date_implemented = d_imp[i],
         iras_ref = col("iras_ref")[i], rec_ref = col("rec_ref")[i], mhra_ref = col("mhra_ref")[i],
         sponsor_authorised_by = col("sponsor_authorised_by")[i],
         status = status[i], notes = notes)
  })
  list(items = items, n_no_category = sum(no_cat & keep), n_dropped = sum(!keep))
}

# Add imported items to the register. A reference already there is skipped or
# updated (keeping its id); a row with no reference gets the next free MOD-nnn.
mod_import_merge <- function(existing, incoming, on_duplicate = c("skip", "update")) {
  on_duplicate <- match.arg(on_duplicate)
  existing <- existing %||% list()
  refs  <- tolower(vapply(existing, function(m) .mod_chr(m$ref), ""))
  stamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  n_new <- 0L; n_upd <- 0L; n_skip <- 0L; next_n <- length(existing)
  for (i in seq_along(incoming)) {
    m <- incoming[[i]]; m$id <- NULL
    k <- tolower(.mod_chr(m$ref))
    hit <- if (nzchar(k)) which(refs == k) else integer(0)
    if (length(hit)) {
      if (on_duplicate == "skip") { n_skip <- n_skip + 1L; next }
      existing[[hit[1]]] <- c(list(id = existing[[hit[1]]]$id %||% paste0("mod_", stamp, "_", i)), m)
      n_upd <- n_upd + 1L
    } else {
      if (!nzchar(k)) {
        repeat { next_n <- next_n + 1L; cand <- sprintf("MOD-%03d", next_n)
                 if (!tolower(cand) %in% refs) break }
        m$ref <- cand
      }
      existing[[length(existing) + 1]] <- c(list(id = paste0("mod_", stamp, "_", i)), m)
      refs  <- c(refs, tolower(m$ref)); n_new <- n_new + 1L
    }
  }
  list(items = existing, n_new = n_new, n_updated = n_upd, n_skipped = n_skip)
}

# Move the old editor's amendments into the register
legacy_amendments_to_register <- function(cfg) {
  old <- legacy_amendments_pending(cfg)
  mod_import_merge(cfg$modifications %||% list(),
                   lapply(seq_along(old), function(i) .legacy_amendment_as_mod(old[[i]], i)),
                   "skip")
}
