upload_server <- function(input, output, session, state) {
  rv <- state$rv
  rv$loaded_file <- NULL

  load_from_folder <- function(reset_sites = FALSE) {
    # Guard: need a trial selected first
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
    result <- tryCatch(
      process_redcap(raw, if (reset_sites) empty_sites else rv$sites),
      error = function(e) {
        removeModal()
        showNotification(paste("Parse error:", e$message), type = "error", duration = 12)
        NULL
      }
    )
    if (is.null(result)) return()

    rv$participants <- result$participants
    rv$sites        <- result$sites
    rv$raw_redcap   <- result$raw_data
    rv$loaded_file  <- basename(filepath)

    n_p <- length(unique(result$participants$record_id))
    n_s <- nrow(result$sites)
    removeModal()
    showNotification(
      paste0("Loaded: ", basename(filepath), "\n", n_p, " participants \u00b7 ", n_s, " sites"),
      type = "message", duration = 6)
  }

  # Load data when a trial is selected (not on app start)
  observeEvent(rv$trigger_data_load, {
    load_from_folder()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(input$refresh_data, load_from_folder(reset_sites = FALSE))
  observeEvent(input$refresh_data_all, {
    load_from_folder(reset_sites = TRUE)
    showNotification("Sites reset.", type = "message", duration = 4)
  })

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
               paste0(format(f$modified, "%d %b %Y %H:%M"), " \u00b7 ",
                      round(file.info(f$path)$size / 1024), " KB")),
          if (is_loaded)
            span(class = "file-loaded", HTML("&#x25CF; Loaded"))
          else
            span(style = "color:var(--muted);font-size:11px", HTML("&mdash;"))
      )
    })
    tagList(file_rows)
  })

  output$dl_sites_xlsx        <- xlsx_download(function() rv$sites, "TONIC_sites")
  output$dl_monthly_xlsx      <- xlsx_download(function() make_monthly_df(rv$log, rv$sites), "TONIC_monthly")
  output$dl_log_xlsx          <- xlsx_download(function() rv$log, "TONIC_log")
  output$dl_participants_xlsx <- xlsx_download(function() rv$participants, "TONIC_participants")
}
