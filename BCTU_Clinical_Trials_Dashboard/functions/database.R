DB_PATH <- file.path(getwd(), "data", "tonic.sqlite")

db_connect <- function() {
  if (!dir.exists(dirname(DB_PATH))) {
    dir.create(dirname(DB_PATH), recursive = TRUE, showWarnings = FALSE)
  }
  dbConnect(RSQLite::SQLite(), DB_PATH)
}

db_init <- function() {
  con <- db_connect()
  on.exit(dbDisconnect(con))

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS accounts (
      user     TEXT PRIMARY KEY,
      password TEXT NOT NULL,
      role     TEXT,
      fullname TEXT,
      admin    INTEGER DEFAULT 0
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS profiles (
      id       INTEGER PRIMARY KEY AUTOINCREMENT,
      fullname TEXT NOT NULL,
      role     TEXT NOT NULL,
      created  TEXT NOT NULL
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sites (
      site_id        TEXT PRIMARY KEY,
      site_name      TEXT,
      city           TEXT,
      region         TEXT,
      country        TEXT,
      status         TEXT,
      site_open_date TEXT,
      siv_booked     INTEGER DEFAULT 0,
      siv_date       TEXT,
      monthly_target INTEGER,
      target         INTEGER,
      randomised     INTEGER,
      lat            REAL,
      lon            REAL,
      source         TEXT DEFAULT 'auto'
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS activity_log (
      id        INTEGER PRIMARY KEY AUTOINCREMENT,
      timestamp TEXT NOT NULL,
      site_id   TEXT NOT NULL,
      action    TEXT NOT NULL,
      note      TEXT
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS projection_settings (
      key   TEXT PRIMARY KEY,
      value REAL NOT NULL
    )
  ")

  # ── Migrate existing databases ──────────────────────────────────────────
  existing_cols <- tryCatch(
    dbGetQuery(con, "PRAGMA table_info(sites)")$name,
    error = function(e) character(0)
  )
  if (length(existing_cols) > 0) {
    if (!"country"    %in% existing_cols)
      dbExecute(con, "ALTER TABLE sites ADD COLUMN country TEXT")
    if (!"siv_booked" %in% existing_cols)
      dbExecute(con, "ALTER TABLE sites ADD COLUMN siv_booked INTEGER DEFAULT 0")
    if (!"siv_date"   %in% existing_cols)
      dbExecute(con, "ALTER TABLE sites ADD COLUMN siv_date TEXT")
    if (!"source"     %in% existing_cols)
      dbExecute(con, "ALTER TABLE sites ADD COLUMN source TEXT DEFAULT 'auto'")
  }
}

db_load_accounts <- function() {
  con <- db_connect()
  on.exit(dbDisconnect(con))
  df <- dbGetQuery(con, "SELECT * FROM accounts")
  if (nrow(df) == 0) {
    df <- default_admin
    db_save_accounts(df)
  }
  df$admin <- as.logical(df$admin)
  df
}

db_save_accounts <- function(accounts_df) {
  con <- db_connect()
  on.exit(dbDisconnect(con))
  df <- accounts_df
  df$admin <- as.integer(df$admin)
  dbExecute(con, "DELETE FROM accounts")
  dbWriteTable(con, "accounts", as.data.frame(df), append = TRUE)
}

db_load_sites <- function() {
  con <- db_connect()
  on.exit(dbDisconnect(con))
  df <- dbGetQuery(con, "SELECT * FROM sites")
  if (nrow(df) == 0) return(empty_sites)

  df$site_open_date <- as.Date(df$site_open_date)
  df$siv_date       <- as.Date(df$siv_date)
  df$siv_booked     <- as.logical(df$siv_booked)
  if (!"country" %in% names(df)) df$country <- NA_character_
  # If legacy rows have country=NA, default to United Kingdom so existing data stays on the UK map
  df$country[is.na(df$country) | nchar(trimws(df$country)) == 0] <- "United Kingdom"

  # Provenance: legacy rows pre-date the flag — treat them as auto-populated.
  if (!"source" %in% names(df)) df$source <- "auto"
  df$source[is.na(df$source) | nchar(trimws(df$source)) == 0] <- "auto"

  as_tibble(df)
}

db_save_sites <- function(sites_df) {
  con <- db_connect()
  on.exit(dbDisconnect(con))
  df <- sites_df

  # Ensure all expected columns exist
  for (col in c("country", "siv_booked", "siv_date", "source")) {
    if (!col %in% names(df)) {
      df[[col]] <- switch(col,
        "country"    = NA_character_,
        "siv_booked" = FALSE,
        "siv_date"   = as.Date(NA),
        "source"     = "auto"
      )
    }
  }

  df$site_open_date <- as.character(df$site_open_date)
  df$siv_date       <- as.character(df$siv_date)
  df$siv_booked     <- as.integer(df$siv_booked)

  dbExecute(con, "DELETE FROM sites")
  dbWriteTable(con, "sites", as.data.frame(df), append = TRUE)
}

db_load_log <- function() {
  con <- db_connect()
  on.exit(dbDisconnect(con))
  df <- dbGetQuery(con, "SELECT timestamp, site_id, action, note FROM activity_log ORDER BY timestamp DESC")
  if (nrow(df) == 0) return(empty_log)
  df$timestamp <- as.POSIXct(df$timestamp, tz = "UTC")
  as_tibble(df)
}

db_save_log <- function(log_df) {
  con <- db_connect()
  on.exit(dbDisconnect(con))
  df <- log_df
  df$timestamp <- as.character(df$timestamp)
  dbExecute(con, "DELETE FROM activity_log")
  dbWriteTable(con, "activity_log", as.data.frame(df), append = TRUE)
}

db_save_all <- function(sites, log, accounts) {
  if (!is.null(sites))    db_save_sites(sites)
  if (!is.null(log))      db_save_log(log)
  if (!is.null(accounts)) db_save_accounts(accounts)
}

# ── Projection settings (dashboard sliders) ─────────────────────────────────
#
# Stored as key/value pairs in the projection_settings table. Keys used:
#   rate_central, rate_optimistic, rate_pessimistic    (per-site per-month)
#   sites_central, sites_optimistic, sites_pessimistic (new sites per month)
#   target_sites                                        (peak total open sites)

# Defaults — a single source of truth for fallback values
projection_defaults <- function() {
  list(
    rate_central       = 3.0,
    rate_optimistic    = 4.0,
    rate_pessimistic   = 2.0,
    sites_central      = 2.0,
    sites_optimistic   = 3.0,
    sites_pessimistic  = 1.0,
    target_sites       = 24
  )
}

db_load_projection_settings <- function() {
  con <- db_connect()
  on.exit(dbDisconnect(con))
  df <- tryCatch(
    dbGetQuery(con, "SELECT key, value FROM projection_settings"),
    error = function(e) data.frame(key = character(), value = numeric())
  )
  defaults <- projection_defaults()
  if (nrow(df) == 0) return(defaults)
  # Overlay stored values onto defaults so missing keys fall back
  stored <- setNames(as.list(df$value), df$key)
  out <- defaults
  for (k in names(stored)) out[[k]] <- stored[[k]]
  out
}

db_save_projection_settings <- function(settings_list) {
  con <- db_connect()
  on.exit(dbDisconnect(con))
  df <- data.frame(
    key   = names(settings_list),
    value = as.numeric(unlist(settings_list)),
    stringsAsFactors = FALSE
  )
  dbExecute(con, "DELETE FROM projection_settings")
  dbWriteTable(con, "projection_settings", df, append = TRUE)
}
