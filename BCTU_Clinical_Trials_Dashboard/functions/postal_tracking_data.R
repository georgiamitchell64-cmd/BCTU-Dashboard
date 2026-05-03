# ── postal_tracking_data.R ───────────────────────────────────────────────────
#
# Manages the SQLite store for postal questionnaire tracking.
#
# Table: postal_sent
#   participant_id   TEXT   — TONIC record ID
#   timepoint        TEXT   — "Day 30" or "Day 90"
#   sent             INTEGER — 0 or 1
#   date_sent        TEXT   — ISO date (YYYY-MM-DD), NULL until sent
#   notes            TEXT   — free-text notes
#   last_modified    TEXT   — ISO datetime of last update (audit trail)
#   modified_by      TEXT   — user who last modified the record
#
# Lives in the app's data/ folder alongside the main app database.
#
# ─────────────────────────────────────────────────────────────────────────────

POSTAL_DB_PATH <- file.path("data", "postal_tracking.sqlite")

# ── Connection helper ────────────────────────────────────────────────────────
postal_db_connect <- function() {
  if (!dir.exists("data")) dir.create("data", recursive = TRUE)
  DBI::dbConnect(RSQLite::SQLite(), POSTAL_DB_PATH)
}

# ── One-time init ────────────────────────────────────────────────────────────
postal_db_init <- function() {
  con <- postal_db_connect()
  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS postal_sent (
      participant_id  TEXT NOT NULL,
      timepoint       TEXT NOT NULL,
      sent            INTEGER DEFAULT 0,
      date_sent       TEXT,
      notes           TEXT,
      last_modified   TEXT,
      modified_by     TEXT,
      PRIMARY KEY (participant_id, timepoint)
    )
  ")

  invisible(TRUE)
}

# ── Read all sent records ────────────────────────────────────────────────────
postal_db_read_all <- function() {
  con <- postal_db_connect()
  on.exit(DBI::dbDisconnect(con))

  tryCatch(
    DBI::dbReadTable(con, "postal_sent"),
    error = function(e) {
      data.frame(
        participant_id = character(),
        timepoint      = character(),
        sent           = integer(),
        date_sent      = character(),
        notes          = character(),
        last_modified  = character(),
        modified_by    = character(),
        stringsAsFactors = FALSE
      )
    }
  )
}

# ── Upsert a single sent record ──────────────────────────────────────────────
postal_db_upsert <- function(participant_id, timepoint, sent,
                             date_sent = NULL, notes = NULL,
                             modified_by = "unknown") {

  con <- postal_db_connect()
  on.exit(DBI::dbDisconnect(con))

  sent_int  <- as.integer(as.logical(sent))
  date_chr  <- if (is.null(date_sent) || is.na(date_sent) || date_sent == "") NA_character_
               else as.character(date_sent)
  notes_chr <- if (is.null(notes)) NA_character_ else as.character(notes)
  now_stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  DBI::dbExecute(con, "
    INSERT INTO postal_sent
      (participant_id, timepoint, sent, date_sent, notes, last_modified, modified_by)
    VALUES (?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT(participant_id, timepoint) DO UPDATE SET
      sent          = excluded.sent,
      date_sent     = excluded.date_sent,
      notes         = excluded.notes,
      last_modified = excluded.last_modified,
      modified_by   = excluded.modified_by
  ", params = list(
    as.character(participant_id),
    as.character(timepoint),
    sent_int,
    date_chr,
    notes_chr,
    now_stamp,
    as.character(modified_by)
  ))

  invisible(TRUE)
}

# ── Build the working dataset ────────────────────────────────────────────────
#
# TONIC structure: cntct_questionnaires_pref, iop_op_end_dt and site_name all
# live on the baseline_arm_1 event row. We filter directly to that event —
# no flattening needed.
#
# Returns one row per postal-preference participant × timepoint (Day 30, 90).
#
# ─────────────────────────────────────────────────────────────────────────────
build_postal_tracking <- function(redcap_df,
                                  id_col    = "record_id",
                                  op_col    = "iop_op_end_dt",
                                  pref_col  = "cntct_questionnaires_pref",
                                  site_col  = "site_name",
                                  event_col = "redcap_event_name",
                                  baseline_event = "baseline_arm_1",
                                  lead_days = 7) {

  req_cols <- c(id_col, op_col, pref_col, event_col)
  missing  <- setdiff(req_cols, names(redcap_df))
  if (length(missing)) {
    warning("build_postal_tracking: missing columns — ",
            paste(missing, collapse = ", "))
    return(NULL)
  }

  # ── Diagnostics ─────────────────────────────────────────────────────────
  message("POSTAL: total export rows = ", nrow(redcap_df))
  message("POSTAL: unique events = ",
          paste(unique(redcap_df[[event_col]]), collapse = ", "))

  # Restrict to baseline event (all three fields live here)
  baseline <- redcap_df %>%
    dplyr::filter(.data[[event_col]] == baseline_event)

  message("POSTAL: baseline rows = ", nrow(baseline))
  message("POSTAL: unique pref values at baseline = ",
          paste(unique(as.character(baseline[[pref_col]])), collapse = " | "))

  # Coerce pref to integer so this works with character or numeric columns
  base <- baseline %>%
    dplyr::mutate(
      .pref_int = suppressWarnings(as.integer(as.character(.data[[pref_col]]))),
      .op_clean = trimws(as.character(.data[[op_col]])),
      .op_clean = dplyr::if_else(.op_clean %in% c("", "NA"), NA_character_, .op_clean)
    ) %>%
    dplyr::filter(
      .pref_int == 4,
      !is.na(.op_clean)
    ) %>%
    dplyr::transmute(
      participant_id = as.character(.data[[id_col]]),
      site_name      = if (site_col %in% names(baseline))
                         as.character(.data[[site_col]]) else NA_character_,
      op_date        = suppressWarnings(as.Date(.op_clean))
    ) %>%
    dplyr::filter(!is.na(op_date)) %>%
    dplyr::distinct(participant_id, .keep_all = TRUE)

  message("POSTAL: final postal + op-date rows = ", nrow(base))

  if (nrow(base) == 0) {
    return(
      data.frame(
        participant_id = character(), site_name = character(),
        op_date = as.Date(character()), timepoint = character(),
        due_date = as.Date(character()), days_to_due = integer(),
        status = character(), sent = integer(),
        date_sent = character(), notes = character(),
        last_modified = character(), modified_by = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  # Expand to Day 30 and Day 90
  tp <- data.frame(
    timepoint    = c("Day 30", "Day 90"),
    days_post_op = c(30L, 90L),
    stringsAsFactors = FALSE
  )

  grid <- base %>%
    tidyr::crossing(tp) %>%
    dplyr::mutate(
      due_date    = op_date + days_post_op,
      days_to_due = as.integer(due_date - Sys.Date())
    )

  # Join the sent records
  sent <- postal_db_read_all()
  if (nrow(sent) > 0) {
    grid <- grid %>%
      dplyr::left_join(sent, by = c("participant_id", "timepoint"))
  } else {
    grid <- grid %>%
      dplyr::mutate(
        sent          = 0L,
        date_sent     = NA_character_,
        notes         = NA_character_,
        last_modified = NA_character_,
        modified_by   = NA_character_
      )
  }

  # Status logic
  grid <- grid %>%
    dplyr::mutate(
      sent   = dplyr::coalesce(sent, 0L),
      status = dplyr::case_when(
        sent == 1                                  ~ "Sent",
        days_to_due <  0                           ~ "Overdue",
        days_to_due <= lead_days                   ~ "Due now",
        days_to_due <= lead_days + 14              ~ "Upcoming",
        TRUE                                       ~ "Future"
      )
    ) %>%
    dplyr::arrange(
      factor(status, levels = c("Overdue", "Due now", "Upcoming", "Sent", "Future")),
      due_date,
      participant_id
    )

  grid
}
