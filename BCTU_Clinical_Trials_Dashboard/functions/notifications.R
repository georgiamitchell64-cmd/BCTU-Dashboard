# =============================================================================
# Smart Notifications (Stage 9)
# =============================================================================
# Notifications are derived from per-trial Smart Insights at warning or alert
# severity. They appear in the bell-icon drawer on the home screen. Each
# notification is identified by a stable key (trial_code + insight title),
# and the user can dismiss any of them — dismissal persists in shared.sqlite.
# =============================================================================

# ── Schema ──────────────────────────────────────────────────────────────────
notifications_db_init <- function() {
  con <- shared_db_connect()
  on.exit(dbDisconnect(con))
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS dismissed_notifications (
      username      TEXT NOT NULL,
      key           TEXT NOT NULL,
      dismissed_at  TEXT NOT NULL,
      PRIMARY KEY (username, key)
    )
  ")
}

.notification_key <- function(trial_code, insight) {
  paste(trial_code, insight$severity, insight$title, sep = "::")
}

# ── Build the live notification list from portfolio insights ───────────────
build_notifications <- function(trials = NULL, username = NULL) {
  if (is.null(trials)) trials <- discover_trials()
  if (!length(trials)) return(list())

  dismissed <- if (!is.null(username) && nzchar(username)) {
    con <- tryCatch(shared_db_connect(), error = function(e) NULL)
    if (is.null(con)) character(0) else {
      on.exit(dbDisconnect(con))
      tryCatch(
        dbGetQuery(con, "SELECT key FROM dismissed_notifications WHERE username = ?",
                   params = list(username))$key,
        error = function(e) character(0))
    }
  } else character(0)

  notes <- list()
  for (code in names(trials)) {
    cfg <- trials[[code]]
    s <- tryCatch(trial_insight_summary(cfg), error = function(e) NULL)
    if (is.null(s) || !length(s$insights)) next
    actionable <- Filter(function(i)
      i$severity %in% c("alert", "warning"), s$insights)
    for (ins in actionable) {
      key <- .notification_key(code, ins)
      if (key %in% dismissed) next
      notes[[length(notes) + 1]] <- list(
        key       = key,
        trial     = s$short,
        code      = code,
        category  = s$category,
        severity  = ins$severity,
        icon      = ins$icon,
        title     = ins$title,
        body      = ins$body,
        value     = ins$value
      )
    }
  }
  # Sort: alerts first, then warnings; within each, by trial
  ord <- order(
    match(vapply(notes, function(n) n$severity, character(1)),
          c("alert", "warning")),
    vapply(notes, function(n) n$trial, character(1)))
  notes[ord]
}

dismiss_notification <- function(username, key) {
  if (is.null(username) || !nzchar(username)) return(invisible(FALSE))
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  dbExecute(con,
    "INSERT OR REPLACE INTO dismissed_notifications (username, key, dismissed_at)
     VALUES (?, ?, ?)",
    params = list(username, key,
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  invisible(TRUE)
}

clear_all_dismissed <- function(username) {
  if (is.null(username) || !nzchar(username)) return(invisible(FALSE))
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  dbExecute(con, "DELETE FROM dismissed_notifications WHERE username = ?",
            params = list(username))
  invisible(TRUE)
}

# ── Rendering ──────────────────────────────────────────────────────────────
render_notification_row <- function(n) {
  cols <- switch(n$severity,
    alert   = list(bg = "#FEF2F2", border = "#FECACA",
                   icon_bg = "#FEE2E2", icon_fg = "#B91C1C"),
    warning = list(bg = "#FFFBEB", border = "#FDE68A",
                   icon_bg = "#FEF3C7", icon_fg = "#B45309")
  )

  div(style = sprintf("background:%s;border:1px solid %s;border-radius:10px;
                       padding:12px 14px;margin-bottom:10px;",
                      cols$bg, cols$border),
      div(style = "display:flex;gap:10px;align-items:flex-start;",
          div(style = sprintf("width:30px;height:30px;border-radius:8px;background:%s;
                               color:%s;display:flex;align-items:center;justify-content:center;
                               font-size:14px;flex-shrink:0;",
                              cols$icon_bg, cols$icon_fg),
              HTML(n$icon)),
          div(style = "flex:1;min-width:0;",
              div(style = "display:flex;justify-content:space-between;
                           align-items:baseline;margin-bottom:3px;gap:10px;",
                  div(style = "font-size:11.5px;color:#64748B;font-weight:600;",
                      sprintf("%s · %s", n$trial, n$category)),
                  if (!is.null(n$value))
                    span(style = sprintf("font-size:11px;font-weight:700;color:%s;",
                                         cols$icon_fg), n$value)),
              div(style = "font-weight:600;color:#0F172A;font-size:13px;line-height:1.4;",
                  n$title),
              div(style = "font-size:12px;color:#475569;line-height:1.5;margin-top:3px;",
                  n$body),
              div(style = "display:flex;gap:8px;margin-top:10px;",
                  tags$button(class = "btn btn-sm",
                              style = "background:#FFFFFF;border:1px solid #DDE5EE;
                                       color:#0F172A;font-size:11px;font-weight:500;
                                       padding:4px 10px;",
                              onclick = sprintf("Shiny.setInputValue('select_trial','%s',{priority:'event'});
                                                 Shiny.setInputValue('notif_close_drawer', Math.random(), {priority:'event'});",
                                                n$code),
                              "View trial"),
                  tags$button(class = "btn btn-sm",
                              style = "background:transparent;border:none;color:#64748B;
                                       font-size:11px;padding:4px 6px;",
                              onclick = sprintf("Shiny.setInputValue('notif_dismiss','%s',{priority:'event'})",
                                                gsub("'", "\\\\'", n$key)),
                              "Dismiss"))
          )))
}
