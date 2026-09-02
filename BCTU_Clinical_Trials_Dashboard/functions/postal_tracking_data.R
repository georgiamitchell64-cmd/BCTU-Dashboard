# ── postal_tracking_data.R ───────────────────────────────────────────────────
#
# Manages the SQLite store for postal questionnaire tracking.
#
# Table: postal_sent
#   participant_id    TEXT   — TONIC record ID
#   timepoint         TEXT   — "Day 30" or "Day 90"
#   sent              INTEGER — 0 or 1
#   date_sent         TEXT   — ISO date (YYYY-MM-DD), NULL until sent
#   returned          INTEGER — 0 or 1 (questionnaire returned by participant)
#   date_returned     TEXT   — ISO date
#   transcribed       INTEGER — 0 or 1 (entered onto trial database)
#   date_transcribed  TEXT   — ISO date
#   reason_not_sent   TEXT   — reason if not posted (free text / preset)
#   notes             TEXT   — free-text notes
#   last_modified     TEXT   — ISO datetime of last update (audit trail)
#   modified_by       TEXT   — user who last modified the record
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
      participant_id    TEXT NOT NULL,
      timepoint         TEXT NOT NULL,
      sent              INTEGER DEFAULT 0,
      date_sent         TEXT,
      returned          INTEGER DEFAULT 0,
      date_returned     TEXT,
      transcribed       INTEGER DEFAULT 0,
      date_transcribed  TEXT,
      reason_not_sent   TEXT,
      notes             TEXT,
      last_modified     TEXT,
      modified_by       TEXT,
      PRIMARY KEY (participant_id, timepoint)
    )
  ")

  # Forward-compat migration: ALTER TABLE ADD COLUMN for any column the
  # CREATE TABLE block above introduces after a database first existed.
  # SQLite errors with "duplicate column name" if the column already exists,
  # which is the only signal we get — swallow it.
  cols <- tryCatch(DBI::dbListFields(con, "postal_sent"),
                   error = function(e) character(0))
  add_if_missing <- function(name, ddl) {
    if (!name %in% cols)
      tryCatch(DBI::dbExecute(con, sprintf("ALTER TABLE postal_sent ADD COLUMN %s", ddl)),
               error = function(e) NULL)
  }
  add_if_missing("returned",         "returned INTEGER DEFAULT 0")
  add_if_missing("date_returned",    "date_returned TEXT")
  add_if_missing("transcribed",      "transcribed INTEGER DEFAULT 0")
  add_if_missing("date_transcribed", "date_transcribed TEXT")
  add_if_missing("reason_not_sent",  "reason_not_sent TEXT")

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
                             modified_by = "unknown",
                             returned = NULL, date_returned = NULL,
                             transcribed = NULL, date_transcribed = NULL,
                             reason_not_sent = NULL) {

  con <- postal_db_connect()
  on.exit(DBI::dbDisconnect(con))

  # Fetch existing row so partial updates preserve untouched fields.
  exist <- tryCatch(
    DBI::dbGetQuery(con, "
      SELECT * FROM postal_sent
       WHERE participant_id = ? AND timepoint = ?",
      params = list(as.character(participant_id), as.character(timepoint))),
    error = function(e) NULL)
  has_row <- !is.null(exist) && nrow(exist) > 0

  to_int <- function(x, fallback) {
    if (is.null(x)) return(if (has_row) as.integer(fallback) else 0L)
    as.integer(as.logical(x))
  }
  to_date <- function(x, fallback) {
    if (is.null(x)) return(if (has_row) fallback else NA_character_)
    if (is.na(x) || identical(as.character(x), "")) NA_character_
    else as.character(x)
  }
  to_text <- function(x, fallback) {
    if (is.null(x)) return(if (has_row) fallback else NA_character_)
    if (is.na(x)) NA_character_ else as.character(x)
  }

  sent_int          <- to_int (sent,             if (has_row) exist$sent             else 0L)
  date_sent_chr     <- to_date(date_sent,        if (has_row) exist$date_sent        else NA_character_)
  returned_int      <- to_int (returned,         if (has_row) exist$returned         else 0L)
  date_returned_chr <- to_date(date_returned,    if (has_row) exist$date_returned    else NA_character_)
  transcribed_int   <- to_int (transcribed,      if (has_row) exist$transcribed      else 0L)
  date_trans_chr    <- to_date(date_transcribed, if (has_row) exist$date_transcribed else NA_character_)
  reason_chr        <- to_text(reason_not_sent,  if (has_row) exist$reason_not_sent  else NA_character_)
  notes_chr         <- to_text(notes,            if (has_row) exist$notes            else NA_character_)
  now_stamp         <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  DBI::dbExecute(con, "
    INSERT INTO postal_sent
      (participant_id, timepoint,
       sent, date_sent,
       returned, date_returned,
       transcribed, date_transcribed,
       reason_not_sent, notes,
       last_modified, modified_by)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT(participant_id, timepoint) DO UPDATE SET
      sent             = excluded.sent,
      date_sent        = excluded.date_sent,
      returned         = excluded.returned,
      date_returned    = excluded.date_returned,
      transcribed      = excluded.transcribed,
      date_transcribed = excluded.date_transcribed,
      reason_not_sent  = excluded.reason_not_sent,
      notes            = excluded.notes,
      last_modified    = excluded.last_modified,
      modified_by      = excluded.modified_by
  ", params = list(
    as.character(participant_id), as.character(timepoint),
    sent_int, date_sent_chr,
    returned_int, date_returned_chr,
    transcribed_int, date_trans_chr,
    reason_chr, notes_chr,
    now_stamp, as.character(modified_by)
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
                                  rand_col  = "rand_dttm_s",
                                  cos_col   = "cos_type",
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

  # cos_type codes that mean "do not send post" (matches the default
  # cos_type_labels in trial_config.R). Trials with different coding can
  # override by editing the labels — the *strings* are looked up here.
  EXCLUSION_LABELS <- c(
    "1" = "Deceased",
    "3" = "Part withdrawal",
    "4" = "Withdrew from follow-up",
    "5" = "Lost to follow-up"
  )

  # ── Diagnostics ─────────────────────────────────────────────────────────
  message("POSTAL: total export rows = ", nrow(redcap_df))
  message("POSTAL: unique events = ",
          paste(unique(redcap_df[[event_col]]), collapse = ", "))

  # Restrict to baseline event (all three fields live here)
  baseline <- redcap_df %>%
    dplyr::filter(as.character(.data[[event_col]]) %in%
                    as.character(unlist(baseline_event)))

  message("POSTAL: baseline rows = ", nrow(baseline))
  message("POSTAL: unique pref values at baseline = ",
          paste(unique(as.character(baseline[[pref_col]])), collapse = " | "))

  # Randomisation gate: a participant is only "in" the trial — and therefore
  # only eligible for postal tracking — once they have a randomisation
  # date/time on the baseline event. Consented-but-not-yet-randomised IDs
  # are excluded entirely.
  has_rand_col <- !is.null(rand_col) && nzchar(rand_col) && rand_col %in% names(baseline)
  if (!has_rand_col) {
    message("POSTAL: rand column '", rand_col, "' not in export — ",
            "skipping randomisation gate (treating everyone as randomised).")
  }

  # Coerce pref to integer so this works with character or numeric columns
  base <- baseline %>%
    dplyr::mutate(
      .pref_int  = suppressWarnings(as.integer(as.character(.data[[pref_col]]))),
      .op_clean  = trimws(as.character(.data[[op_col]])),
      .op_clean  = dplyr::if_else(.op_clean %in% c("", "NA"), NA_character_, .op_clean),
      .rand_clean = if (has_rand_col)
                      trimws(as.character(.data[[rand_col]])) else "_NA_",
      .rand_clean = dplyr::if_else(.rand_clean %in% c("", "NA"), NA_character_, .rand_clean)
    ) %>%
    dplyr::filter(
      .pref_int == 4,
      !is.na(.op_clean),
      if (has_rand_col) !is.na(.rand_clean) else TRUE
    ) %>%
    dplyr::transmute(
      participant_id = as.character(.data[[id_col]]),
      site_name      = if (site_col %in% names(baseline))
                         as.character(.data[[site_col]]) else NA_character_,
      op_date        = suppressWarnings(as.Date(.op_clean))
    ) %>%
    dplyr::filter(!is.na(op_date)) %>%
    dplyr::distinct(participant_id, .keep_all = TRUE)

  message("POSTAL: randomised + postal + op-date rows = ", nrow(base))

  # ── COS lookup ─────────────────────────────────────────────────────────
  # cos_type can appear on any event row. Pick the first non-empty value
  # per participant. Mapped to a human-readable exclusion reason via
  # EXCLUSION_LABELS; codes not in that map (e.g. "2" = No operation) are
  # treated as not-excluding for postal purposes.
  cos_lookup <- data.frame(
    participant_id  = base$participant_id,
    excluded_reason = NA_character_,
    stringsAsFactors = FALSE
  )
  if (!is.null(cos_col) && nzchar(cos_col) && cos_col %in% names(redcap_df)) {
    cos_df <- redcap_df %>%
      dplyr::mutate(
        .id  = as.character(.data[[id_col]]),
        .cos = trimws(as.character(.data[[cos_col]]))
      ) %>%
      dplyr::filter(!is.na(.cos), nzchar(.cos), .cos != "NA") %>%
      dplyr::group_by(.id) %>%
      dplyr::summarise(cos_code = dplyr::first(.cos), .groups = "drop") %>%
      dplyr::mutate(excluded_reason = unname(EXCLUSION_LABELS[cos_code])) %>%
      dplyr::transmute(participant_id = .id, excluded_reason)
    cos_lookup <- cos_lookup %>%
      dplyr::select(-excluded_reason) %>%
      dplyr::left_join(cos_df, by = "participant_id")
  }

  message("POSTAL: final postal + op-date rows = ", nrow(base))

  empty_df <- data.frame(
    participant_id = character(), site_name = character(),
    op_date = as.Date(character()), timepoint = character(),
    due_date = as.Date(character()), days_to_due = integer(),
    status = character(), excluded_reason = character(),
    sent = integer(), date_sent = character(),
    returned = integer(), date_returned = character(),
    transcribed = integer(), date_transcribed = character(),
    reason_not_sent = character(),
    notes = character(),
    last_modified = character(), modified_by = character(),
    stringsAsFactors = FALSE
  )
  if (nrow(base) == 0) return(empty_df)

  # Expand to Day 30 and Day 90
  tp <- data.frame(
    timepoint    = c("Day 30", "Day 90"),
    days_post_op = c(30L, 90L),
    stringsAsFactors = FALSE
  )

  grid <- base %>%
    dplyr::left_join(cos_lookup, by = "participant_id") %>%
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
        sent             = 0L,
        date_sent        = NA_character_,
        returned         = 0L,
        date_returned    = NA_character_,
        transcribed      = 0L,
        date_transcribed = NA_character_,
        reason_not_sent  = NA_character_,
        notes            = NA_character_,
        last_modified    = NA_character_,
        modified_by      = NA_character_
      )
  }

  # Ensure every workflow column exists even on legacy DBs where the join
  # only brought back a subset.
  for (col in c("returned","transcribed")) {
    if (!col %in% names(grid)) grid[[col]] <- 0L
  }
  for (col in c("date_returned","date_transcribed","reason_not_sent",
                "notes","last_modified","modified_by")) {
    if (!col %in% names(grid)) grid[[col]] <- NA_character_
  }

  # Seven-state status derivation. Order matters: more "advanced"
  # workflow states win over earlier ones.
  grid <- grid %>%
    dplyr::mutate(
      sent        = dplyr::coalesce(sent,        0L),
      returned    = dplyr::coalesce(returned,    0L),
      transcribed = dplyr::coalesce(transcribed, 0L),
      has_reason  = !is.na(reason_not_sent) & nzchar(as.character(reason_not_sent)),
      has_excl    = !is.na(excluded_reason) & nzchar(as.character(excluded_reason)),
      # Excluded participants (deceased / withdrawn / lost to follow-up) win
      # over the workflow state so they don't show up in Overdue / Due now
      # counts but remain visible in the table for auditability.
      status = dplyr::case_when(
        has_excl                        ~ "Excluded",
        transcribed == 1                ~ "Transcribed",
        returned    == 1                ~ "Returned",
        sent        == 1                ~ "Sent",
        has_reason                      ~ "Not sent",
        days_to_due <  0                ~ "Overdue",
        days_to_due <= lead_days        ~ "Due now",
        days_to_due <= lead_days + 14   ~ "Upcoming",
        TRUE                            ~ "Future"
      )
    ) %>%
    dplyr::select(-has_reason, -has_excl) %>%
    dplyr::arrange(
      factor(status, levels = c("Excluded","Overdue","Due now","Upcoming",
                                "Sent","Returned","Transcribed",
                                "Not sent","Future")),
      due_date,
      participant_id
    )

  grid
}
