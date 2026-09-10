# =============================================================================
# Canonical store
# =============================================================================
# Persists the Canonical Trial Dataset into the trial's existing SQLite
# database (tables prefixed canon_), plus an `imports` audit table
# (docs/ARCHITECTURE.md §5.5). Lets the dashboard reopen instantly from the
# last import without re-parsing source files, and makes imports auditable.
# =============================================================================

.CANON_TABLES <- c("participants", "visits", "form_status", "status_changes",
                   "safety_events", "deviations", "observations")

.canon_con <- function(db_path) {
  DBI::dbConnect(RSQLite::SQLite(), db_path)
}

#' Serialise Date/POSIXct columns to ISO text for SQLite round-tripping.
.canon_flatten <- function(df) {
  for (nm in names(df)) {
    if (inherits(df[[nm]], "Date"))
      df[[nm]] <- format(df[[nm]], "%Y-%m-%d")
    else if (inherits(df[[nm]], "POSIXt"))
      df[[nm]] <- format(df[[nm]], "%Y-%m-%d %H:%M:%S")
  }
  df
}

.canon_restore <- function(df) {
  date_cols <- c("randomised_at", "status_date", "date", "onset_date",
                 "report_date", "value_date")
  for (nm in intersect(date_cols, names(df)))
    df[[nm]] <- as.Date(df[[nm]])
  tibble::as_tibble(df)
}

#' Save a canonical dataset (and its validation result) to the trial DB.
#' @return the import id (integer), or NA on failure
canon_save <- function(dataset, issues, db_path,
                       username = NULL, file_label = NULL) {
  tryCatch({
    if (!dir.exists(dirname(db_path)))
      dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
    con <- .canon_con(db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS imports (
        import_id    INTEGER PRIMARY KEY AUTOINCREMENT,
        imported_at  TEXT NOT NULL,
        username     TEXT,
        adapter      TEXT,
        files        TEXT,
        fingerprint  TEXT,
        n_participants INTEGER,
        n_blocking   INTEGER,
        n_warning    INTEGER,
        n_info       INTEGER
      )")

    src <- dataset$meta$source %||% list()
    sev <- table(factor(issues$severity, levels = c("blocking", "warning", "info")))
    DBI::dbExecute(con, "
      INSERT INTO imports (imported_at, username, adapter, files, fingerprint,
                           n_participants, n_blocking, n_warning, n_info)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        username %||% "",
        src$adapter %||% "",
        file_label %||% paste(src$files %||% character(0), collapse = ", "),
        src$fingerprint %||% "",
        nrow(dataset$participants),
        sev[["blocking"]], sev[["warning"]], sev[["info"]]))
    import_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]

    for (tb in .CANON_TABLES) {
      df <- dataset[[tb]]
      if (is.null(df)) next
      DBI::dbWriteTable(con, paste0("canon_", tb),
                        .canon_flatten(as.data.frame(df)), overwrite = TRUE)
    }
    DBI::dbWriteTable(con, "canon_validation",
                      as.data.frame(issues), overwrite = TRUE)
    as.integer(import_id)
  }, error = function(e) {
    message("canon_save error: ", e$message)
    NA_integer_
  })
}

#' Load the last-saved canonical dataset from the trial DB.
#' @return list(dataset, issues, imported_at) or NULL if never saved
canon_load <- function(db_path) {
  if (!file.exists(db_path)) return(NULL)
  tryCatch({
    con <- .canon_con(db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    tabs <- DBI::dbListTables(con)
    if (!("canon_participants" %in% tabs)) return(NULL)

    dataset <- list()
    for (tb in .CANON_TABLES) {
      full <- paste0("canon_", tb)
      dataset[[tb]] <- if (full %in% tabs)
        .canon_restore(DBI::dbReadTable(con, full)) else NULL
    }
    issues <- if ("canon_validation" %in% tabs)
      tibble::as_tibble(DBI::dbReadTable(con, "canon_validation"))
    else tibble::tibble(severity = character(), code = character(),
                        message = character(), count = integer())

    last <- if ("imports" %in% tabs)
      DBI::dbGetQuery(con, "
        SELECT imported_at, fingerprint FROM imports
        ORDER BY import_id DESC LIMIT 1")
    else NULL

    list(dataset = dataset, issues = issues,
         imported_at = if (!is.null(last) && nrow(last)) last$imported_at[1] else NA,
         fingerprint = if (!is.null(last) && nrow(last)) last$fingerprint[1] else NA)
  }, error = function(e) {
    message("canon_load error: ", e$message)
    NULL
  })
}

#' Import history for the audit view on the Data tab.
canon_import_history <- function(db_path, limit = 20L) {
  if (!file.exists(db_path)) return(NULL)
  tryCatch({
    con <- .canon_con(db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    if (!("imports" %in% DBI::dbListTables(con))) return(NULL)
    tibble::as_tibble(DBI::dbGetQuery(con, sprintf(
      "SELECT * FROM imports ORDER BY import_id DESC LIMIT %d", limit)))
  }, error = function(e) NULL)
}

#' Has the source structure changed since the trial was configured?
#' Compares a fresh source package's fingerprint with the last import's.
#' @return list(changed = logical, previous = chr, current = chr)
canon_check_drift <- function(source_package, db_path) {
  cur <- source_package$source$fingerprint %||% ""
  prev <- NA_character_
  hist <- canon_import_history(db_path, limit = 1L)
  if (!is.null(hist) && nrow(hist)) prev <- hist$fingerprint[1]
  list(changed = !is.na(prev) && nzchar(prev) && !identical(prev, cur),
       previous = prev, current = cur)
}
