# =============================================================================
# Activity log (Track B item 2)
# =============================================================================
# Cross-trial event stream stored in shared.sqlite. One row per meaningful
# action (trial created, site added, CSV uploaded, amendment edited, etc.).
#
# Schema: id, timestamp, username, trial_code, event_type, summary, metadata.
# `metadata` is a JSON blob for any extra context that doesn't fit the
# standard columns. Don't store participant-level data here.
#
# Auto-trim keeps the table to MAX_EVENTS rows so it never grows unbounded.
# =============================================================================

ACTIVITY_MAX_EVENTS <- 5000L

activity_db_init <- function() {
  con <- shared_db_connect()
  on.exit(dbDisconnect(con))
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS activity_events (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      timestamp   TEXT NOT NULL,
      username    TEXT,
      trial_code  TEXT,
      event_type  TEXT NOT NULL,
      summary     TEXT NOT NULL,
      metadata    TEXT
    )
  ")
  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_activity_ts
      ON activity_events (timestamp DESC)
  ")
}

# Append an event. Always non-blocking — failures here must never break a
# user-facing action.
log_activity <- function(event_type, summary,
                         username = NULL, trial_code = NULL,
                         metadata = NULL) {
  tryCatch({
    con <- shared_db_connect()
    on.exit(dbDisconnect(con))
    meta_json <- if (is.null(metadata)) NA_character_
                 else jsonlite::toJSON(metadata, auto_unbox = TRUE)
    dbExecute(con,
      "INSERT INTO activity_events
       (timestamp, username, trial_code, event_type, summary, metadata)
       VALUES (?, ?, ?, ?, ?, ?)",
      params = list(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        username %||% NA_character_,
        trial_code %||% NA_character_,
        event_type, summary, meta_json))

    # Trim if we're over the cap
    n <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM activity_events")$n
    if (n > ACTIVITY_MAX_EVENTS) {
      keep <- ACTIVITY_MAX_EVENTS
      dbExecute(con, sprintf(
        "DELETE FROM activity_events WHERE id NOT IN (
           SELECT id FROM activity_events ORDER BY id DESC LIMIT %d)",
        keep))
    }
  }, error = function(e) {
    message("activity log error: ", e$message)
  })
  invisible(NULL)
}

# Read recent events with optional filters.
list_activity <- function(limit = 200, trial_code = NULL,
                          event_type = NULL) {
  con <- shared_db_connect()
  on.exit(dbDisconnect(con))
  where_clauses <- character(0)
  params <- list()
  if (!is.null(trial_code) && nzchar(trial_code)) {
    where_clauses <- c(where_clauses, "trial_code = ?")
    params <- c(params, list(trial_code))
  }
  if (!is.null(event_type) && nzchar(event_type)) {
    where_clauses <- c(where_clauses, "event_type = ?")
    params <- c(params, list(event_type))
  }
  where_sql <- if (length(where_clauses))
    paste("WHERE", paste(where_clauses, collapse = " AND ")) else ""
  q <- sprintf(
    "SELECT id, timestamp, username, trial_code, event_type, summary, metadata
     FROM activity_events %s
     ORDER BY id DESC LIMIT %d",
    where_sql, as.integer(limit))
  tryCatch(dbGetQuery(con, q, params = params),
           error = function(e) data.frame())
}

# ── Rendering ──────────────────────────────────────────────────────────────
.activity_icon <- function(event_type) {
  switch(event_type,
    trial_created      = list(emoji = "&#x2728;", colour = "#10B981"),
    trial_deleted      = list(emoji = "&#x1F5D1;", colour = "#B91C1C"),
    site_added         = list(emoji = "&#x1F3E5;", colour = "#6366F1"),
    sites_bulk_added   = list(emoji = "&#x1F4CB;", colour = "#6366F1"),
    site_deleted       = list(emoji = "&#x2796;", colour = "#B91C1C"),
    csv_uploaded       = list(emoji = "&#x2B06;", colour = "#0EA5E9"),
    amendment_added    = list(emoji = "&#x1F4DD;", colour = "#8B5CF6"),
    amendment_edited   = list(emoji = "&#x270F;", colour = "#8B5CF6"),
    amendment_removed  = list(emoji = "&#x2716;", colour = "#94A3B8"),
    settings_saved     = list(emoji = "&#x2699;", colour = "#64748B"),
    membership_changed = list(emoji = "&#x1F465;", colour = "#F59E0B"),
    portfolio_role_changed = list(emoji = "&#x1F451;", colour = "#F59E0B"),
    list(emoji = "&#x1F4CC;", colour = "#94A3B8"))
}

.time_ago <- function(timestamp) {
  ts <- suppressWarnings(as.POSIXct(timestamp, tz = "UTC"))
  if (is.na(ts)) return(timestamp)
  diff_s <- as.numeric(difftime(Sys.time(), ts, units = "secs"))
  if (diff_s < 0) diff_s <- 0
  if (diff_s < 60)        return("just now")
  if (diff_s < 3600)      return(sprintf("%dm ago", as.integer(diff_s / 60)))
  if (diff_s < 86400)     return(sprintf("%dh ago", as.integer(diff_s / 3600)))
  if (diff_s < 86400 * 7) return(sprintf("%dd ago", as.integer(diff_s / 86400)))
  format(ts, "%d %b %Y")
}

render_activity_row <- function(row, trials = NULL) {
  ic <- .activity_icon(row$event_type)
  trial_label <- if (!is.na(row$trial_code) && nzchar(row$trial_code)) {
    cfg <- if (!is.null(trials)) trials[[row$trial_code]] else NULL
    cfg$short_name %||% toupper(row$trial_code)
  } else NULL

  div(style = "display:flex;gap:12px;padding:12px 0;
               border-bottom:1px solid #EEF2F7;",
      div(style = sprintf("width:32px;height:32px;border-radius:9px;
                           background:%s20;color:%s;
                           display:flex;align-items:center;justify-content:center;
                           font-size:14px;flex-shrink:0;",
                          ic$colour, ic$colour),
          HTML(ic$emoji)),
      div(style = "flex:1;min-width:0;",
          div(style = "display:flex;justify-content:space-between;align-items:baseline;
                       gap:10px;margin-bottom:2px;",
              div(style = "font-size:13px;color:#0F172A;line-height:1.5;",
                  HTML(row$summary)),
              span(style = "font-size:11px;color:#94A3B8;flex-shrink:0;
                            font-variant-numeric:tabular-nums;",
                   .time_ago(row$timestamp))),
          div(style = "font-size:11px;color:#64748B;",
              if (!is.null(trial_label))
                span(style = "background:#F1F5F9;color:#0F172A;
                              padding:1px 8px;border-radius:999px;
                              font-weight:500;margin-right:6px;",
                     trial_label),
              if (!is.na(row$username) && nzchar(row$username))
                span(sprintf("by %s", row$username))))
  )
}
