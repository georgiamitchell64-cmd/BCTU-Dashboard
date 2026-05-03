# =============================================================================
# Upload server: load REDCap CSV → optional autodetect modal → process_redcap
# =============================================================================
# Flow:
#   1. start_load()    — read CSV + run autodetect
#   2. If active config has unmapped roles that autodetect resolved, open the
#      review/amend modal (functions/autodetect_modal.R). Otherwise skip.
#   3. Apply / Amend / Reject → finish_load() does process_redcap and stores
#      results in rv.
# =============================================================================

upload_server <- function(input, output, session, state) {
  rv <- state$rv
  rv$loaded_file <- NULL

  pending <- reactiveValues(
    raw         = NULL,
    detected    = NULL,
    filepath    = NULL,
    reset_sites = FALSE
  )

  # ── Decide whether to prompt the user with the autodetect modal ───────────
  .should_prompt <- function(cfg, detected) {
    if (is.null(cfg) || is.null(detected)) return(FALSE)
    for (r in .AUTODETECT_FIELD_ROLES) {
      cur <- cfg$redcap_fields[[r$role]]
      det <- .autodetect_field(detected, r$role)
      if ((is.null(cur) || (is.character(cur) && !nzchar(cur))) &&
          !is.null(det) && nzchar(det)) return(TRUE)
    }
    for (r in .AUTODETECT_EVENT_ROLES) {
      cur <- cfg$redcap_events[[r$role]]
      det <- .autodetect_event(detected, r$role)
      if ((is.null(cur) || (is.character(cur) && !nzchar(cur))) &&
          !is.null(det) && nzchar(det)) return(TRUE)
    }
    fu <- cfg$redcap_fields$follow_up_instruments
    if ((is.null(fu) || length(fu) == 0) &&
        length(detected$instruments %||% character(0)) > 0) return(TRUE)
    FALSE
  }

  # ── Phase 1: read CSV + autodetect; open modal or finish_load directly ────
  start_load <- function(reset_sites = FALSE) {
    if (is.null(rv$trial_code)) {
      removeModal()
      return()
    }

    filepath <- find_latest_csv()
    if (is.null(filepath)) {
      removeModal()
      showNotification(
        HTML(paste0("No CSV found in:<br><code>",
                    normalizePath(DATA_DIR, mustWork = FALSE),
                    "</code><br>The dashboard is ready — upload a REDCap export to populate it.")),
        type = "warning", duration = 8)
      return()
    }

    raw <- tryCatch(
      read_redcap_file(filepath),
      error = function(e) {
        removeModal()
        showNotification(paste("Read error:", e$message), type = "error", duration = 12)
        NULL
      }
    )
    if (is.null(raw)) return()

    detected <- tryCatch(autodetect_redcap(filepath), error = function(e) NULL)
    # Stash full column list for the amend dropdowns.
    if (!is.null(detected)) detected$.all_cols <- names(raw)

    pending$raw         <- raw
    pending$detected    <- detected
    pending$filepath    <- filepath
    pending$reset_sites <- reset_sites

    cfg <- rv$trial_config
    if (.should_prompt(cfg, detected)) {
      # Initialise rv values to empty defaults so dashboard reactives can
      # resolve while the user decides on the modal. Without this, Shiny
      # shows a global loading overlay that covers the modal itself.
      if (is.null(rv$participants)) rv$participants <- empty_participants
      if (is.null(rv$sites))        rv$sites        <- if (reset_sites) empty_sites else empty_sites
      if (is.null(rv$raw_redcap))   rv$raw_redcap   <- raw[0, ]

      removeModal()
      showModal(modalDialog(
        title = div(style = "display:flex;align-items:center;gap:10px;",
                    span(style = "font-size:22px;color:#2EC4A5;", HTML("&#9733;")),
                    span("Auto-detected REDCap mappings"),
                    span(style = "font-size:11px;color:#64748B;font-weight:400;
                                  background:#EEF3F8;padding:3px 8px;border-radius:4px;
                                  margin-left:auto;",
                         basename(filepath))),
        size = "l",
        easyClose = FALSE,
        footer = div(
          style = "display:flex;justify-content:space-between;width:100%;",
          # Left side: Reject + (hidden) Back-to-review
          div(
            actionButton("autodetect_reject", "Reject",
                         class = "btn btn-outline-secondary"),
            shinyjs::hidden(
              actionButton("autodetect_amend_back", HTML("&larr; Back to review"),
                           class = "btn btn-outline-secondary",
                           style = "margin-left:6px;")
            )
          ),
          # Right side: Amend + Apply
          div(
            actionButton("autodetect_amend", HTML("&#9998; Amend"),
                         class = "btn btn-outline-secondary",
                         style = "margin-right:8px;"),
            actionButton("autodetect_apply", HTML("&#10003; Apply"),
                         class = "btn btn-primary",
                         style = "background:#1B4F6B;border-color:#1B4F6B;font-weight:600;")
          )
        ),
        autodetect_modal_ui(detected, cfg)
      ))
    } else {
      finish_load()
    }
  }

  # ── Phase 2: process_redcap with current (possibly augmented) cfg ─────────
  finish_load <- function() {
    raw <- pending$raw
    if (is.null(raw)) return()

    result <- tryCatch(
      process_redcap(raw, if (pending$reset_sites) empty_sites else rv$sites),
      error = function(e) {
        showNotification(paste("Parse error:", e$message), type = "error", duration = 12)
        NULL
      }
    )
    if (is.null(result)) {
      pending$raw <- NULL
      return()
    }

    rv$participants <- result$participants
    rv$sites        <- result$sites
    rv$raw_redcap   <- result$raw_data
    rv$loaded_file  <- basename(pending$filepath %||% "")

    n_p <- length(unique(result$participants$record_id))
    n_s <- nrow(result$sites)
    showNotification(
      paste0("Loaded: ", basename(pending$filepath), "\n",
             n_p, " participants · ", n_s, " sites"),
      type = "message", duration = 6)

    pending$raw <- NULL
  }

  # ── Modal wiring ──────────────────────────────────────────────────────────

  # Apply: collect dropdown values (or fall back to detected defaults if amend
  # was never opened), merge into cfg, apply, then finish load.
  observeEvent(input$autodetect_apply, {
    detected <- pending$detected
    cfg      <- rv$trial_config

    applied_fields <- list()
    for (r in .AUTODETECT_FIELD_ROLES) {
      v <- input[[paste0("autodetect_fld_", r$role)]]
      if (is.null(v) || !nzchar(v))
        v <- .autodetect_field(detected, r$role) %||% ""
      applied_fields[[r$role]] <- v
    }
    applied_events <- list()
    for (r in .AUTODETECT_EVENT_ROLES) {
      v <- input[[paste0("autodetect_evt_", r$role)]]
      if (is.null(v) || !nzchar(v))
        v <- .autodetect_event(detected, r$role) %||% ""
      applied_events[[r$role]] <- v
    }

    new_cfg <- merge_detected_into_config(
      cfg,
      applied_fields       = applied_fields,
      applied_events       = applied_events,
      detected_instruments = detected$instrument_fields
    )
    rv$trial_config <- new_cfg
    apply_trial_globals(new_cfg)

    n_filled <- sum(vapply(applied_fields, function(x) nzchar(x %||% ""), logical(1))) +
                sum(vapply(applied_events, function(x) nzchar(x %||% ""), logical(1)))
    showNotification(
      paste0("Applied ", n_filled, " mappings to ", new_cfg$short_name %||% "trial"),
      type = "message", duration = 5)

    removeModal()
    finish_load()
  })

  # Reject: load with current config unchanged.
  observeEvent(input$autodetect_reject, {
    removeModal()
    finish_load()
  })

  # Toggle review ↔ amend.
  observeEvent(input$autodetect_amend, {
    shinyjs::hide("autodetect_view_review")
    shinyjs::show("autodetect_view_amend")
    shinyjs::show("autodetect_amend_back")
    shinyjs::hide("autodetect_amend")
  })
  observeEvent(input$autodetect_amend_back, {
    shinyjs::show("autodetect_view_review")
    shinyjs::hide("autodetect_view_amend")
    shinyjs::hide("autodetect_amend_back")
    shinyjs::show("autodetect_amend")
  })

  # ── Triggers ──────────────────────────────────────────────────────────────
  observeEvent(rv$trigger_data_load, { start_load() }, ignoreInit = TRUE, ignoreNULL = TRUE)
  observeEvent(input$refresh_data,    { start_load(reset_sites = FALSE) })
  observeEvent(input$refresh_data_all, {
    start_load(reset_sites = TRUE)
    showNotification("Sites reset.", type = "message", duration = 4)
  })

  # ── Status panels (unchanged from previous version) ───────────────────────
  output$data_folder_status <- renderUI({
    invalidateLater(30000)
    folder_exists <- dir.exists(DATA_DIR)
    folder_path   <- normalizePath(DATA_DIR, mustWork = FALSE)
    loaded        <- rv$loaded_file

    ok  <- function(txt) div(span(style = "color:#059669;font-weight:600", HTML("&check; ")), txt)
    err <- function(txt) div(span(style = "color:#DC2626;font-weight:600", HTML("&cross; ")), txt)

    div(class = "status-grid",
        div(class = "status-card",
            div(class = "status-card-label", "Data folder"),
            if (folder_exists) ok(tags$code(style = "font-size:10px;color:var(--navy)", folder_path))
            else err(paste("Not found:", folder_path))),
        div(class = "status-card",
            div(class = "status-card-label", "Currently loaded"),
            if (!is.null(loaded))
              div(span(style = "color:#059669;font-weight:600", HTML("&check; ")),
                  span(style = "font-size:11px;word-break:break-all", loaded))
            else span(style = "color:var(--muted);font-size:11px", "None yet")),
        div(class = "status-card",
            div(class = "status-card-label", "Participants"),
            div(class = "status-card-val",
                length(unique(rv$participants$record_id[rv$participants$event_type == "Baseline"]))),
            div(class = "status-card-sub", "randomised")),
        div(class = "status-card",
            div(class = "status-card-label", "Sites"),
            div(class = "status-card-val", nrow(rv$sites)),
            div(class = "status-card-sub", "in dashboard"))
    )
  })

  output$folder_files_ui <- renderUI({
    invalidateLater(30000)
    files  <- list_csvs()
    loaded <- rv$loaded_file
    if (nrow(files) == 0) {
      return(div(style = "padding:10px;color:var(--muted);font-size:12px",
                 paste0("No CSV files in: ", normalizePath(DATA_DIR, mustWork = FALSE))))
    }
    file_rows <- lapply(seq_len(nrow(files)), function(i) {
      f <- files[i, ]
      is_loaded <- identical(f$file, loaded)
      div(class = "file-row",
          span(class = "file-name", f$file),
          span(class = "file-meta",
               paste0(format(f$modified, "%d %b %Y %H:%M"), " · ",
                      round(file.info(f$path)$size / 1024), " KB")),
          if (is_loaded)
            span(class = "file-loaded", HTML("&#x25CF; Loaded"))
          else
            span(style = "color:var(--muted);font-size:11px", HTML("&mdash;"))
      )
    })
    tagList(file_rows)
  })

  # Per-trial download filenames (used by sidebar download buttons).
  .trial_slug <- function() {
    cfg <- rv$trial_config
    cfg$short_name %||% cfg$code %||% "trial"
  }
  output$dl_sites_xlsx        <- xlsx_download(function() rv$sites,                            paste0(.trial_slug(), "_sites"))
  output$dl_monthly_xlsx      <- xlsx_download(function() make_monthly_df(rv$log, rv$sites),    paste0(.trial_slug(), "_monthly"))
  output$dl_log_xlsx          <- xlsx_download(function() rv$log,                              paste0(.trial_slug(), "_log"))
  output$dl_participants_xlsx <- xlsx_download(function() rv$participants,                     paste0(.trial_slug(), "_participants"))
}
