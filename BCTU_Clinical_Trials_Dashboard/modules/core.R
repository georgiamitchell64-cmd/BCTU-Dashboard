init_app_state <- function(input, output, session) {
  # Discover available trials
  available_trials <- discover_trials()

  rv <- reactiveValues(
    # ── Trial selection state ──────────────────────────────────────────────
    available_trials = available_trials,
    trial_config     = NULL,
    trial_code       = NULL,

    # ── Per-trial data (loaded after trial selection) ─────────────────────
    sites        = empty_sites,
    log          = empty_log,
    participants = empty_participants,
    raw_redcap   = NULL,
    loaded_file  = NULL,

    # ── User state (set by welcome_server) ─────────────────────────────────
    accounts     = data.frame(),
    role         = NULL,
    username     = NULL,

    # ── Triggers ───────────────────────────────────────────────────────────
    trigger_data_load = NULL
  )

  # ── Sidebar logo (dynamic per trial) ──────────────────────────────────────
  output$sidebar_logo_ui <- renderUI({
    cfg <- rv$trial_config
    if (is.null(cfg)) {
      # Before trial selected — generic logo
      div(style = "font-size:16px;font-weight:700;color:#fff;",
          HTML("&#x2694; BCTU"))
    } else {
      logo <- cfg$logo_file
      if (!is.null(logo) && file.exists(logo)) {
        tags$img(src = paste0("trial_logos/", cfg$code, ".jpg"),
                 height = "34px",
                 style = "filter:brightness(0) invert(1);object-fit:contain;")
      } else {
        div(style = "font-size:16px;font-weight:700;color:#fff;",
            cfg$short_name %||% toupper(cfg$code))
      }
    }
  })

  # ── Back to trial selector ────────────────────────────────────────────────
  observeEvent(input$back_to_selector, {
    # Save current trial state
    if (!is.null(rv$trial_code)) {
      tryCatch(
        db_save_all(rv$sites, rv$log, rv$accounts),
        error = function(e) message("DB save on switch: ", e$message)
      )
    }

    # Reset trial-specific state
    rv$trial_config <- NULL
    rv$trial_code   <- NULL
    rv$sites        <- empty_sites
    rv$log          <- empty_log
    rv$participants <- empty_participants
    rv$raw_redcap   <- NULL
    rv$loaded_file  <- NULL

    # Show selector, hide dashboard
    shinyjs::show("trial_selector_panel")
    shinyjs::hide("dashboard_panel")
    shinyjs::hide("sidebar_nav_section")    # Hide sidebar nav
    shinyjs::hide("topbar_wrap")             # Hide topbar

    # Reset topbar
    runjs("$('.topbar-title').text('Clinical Trials Dashboard')")

    # Remove any trial-specific colour overrides
    runjs("$('#trial-colour-override').remove();")
  })

  # ── Standard outputs ──────────────────────────────────────────────────────
  output$sb_name         <- renderText({ req(rv$username); rv$username })
  output$sb_role         <- renderText({ req(rv$role); rv$role })
  output$topbar_username <- renderText({ req(rv$username); rv$username })
  output$topbar_role     <- renderText({ req(rv$role); rv$role })
  output$topbar_view_badge <- renderText({
    role <- rv$role
    if (is.null(role)) return("Loading")
    switch(role,
      "Trial Manager" = "TMG View",
      "CI"            = "CI View",
      "Team Leader"   = "Team View",
      "Guest View"
    )
  })

  # ── Tab navigation ────────────────────────────────────────────────────────
  switch_tab <- function(tab_name, btn_id) {
    updateTabsetPanel(session, "active_tab", selected = tab_name)
    runjs(sprintf("setActiveNav('%s')", btn_id))
  }
  observeEvent(input$go_overview,       switch_tab("overview", "go_overview"))
  observeEvent(input$go_reports,        switch_tab("reports", "go_reports"))
  observeEvent(input$go_randomisations, switch_tab("randomisations", "go_randomisations"))
  observeEvent(input$go_participants,   switch_tab("participants", "go_participants"))
  observeEvent(input$go_sites,          switch_tab("sites", "go_sites"))
  observeEvent(input$go_upload,         switch_tab("upload", "go_upload"))
  observeEvent(input$go_accounts,       switch_tab("accounts", "go_accounts"))
  observeEvent(input$go_settings,       switch_tab("settings", "go_settings"))
  observeEvent(input$go_postal,         switch_tab("postal_panel", "go_postal"))
  observeEvent(input$go_returns,        switch_tab("returns_panel", "go_returns"))

  # After trial is loaded, auto-switch to overview
  observeEvent(rv$trigger_data_load, {
    req(rv$trial_code)
    runjs("setActiveNav('go_overview')")
  }, ignoreInit = TRUE)

  site_choices <- reactive({
    df <- rv$sites %>%
      filter(!is.na(site_id), nchar(trimws(site_id)) > 0,
             !is.na(site_name), nchar(trimws(site_name)) > 0)
    if (nrow(df) > 0) {
      setNames(df$site_id, paste(df$site_id, df$site_name, sep = " \u2014 "))
    } else {
      character(0)
    }
  })

  observeEvent(site_choices(), {
    choices <- site_choices()
    updatePickerInput(session, "rpt_sites", choices = choices, selected = character(0))
    updateSelectInput(session, "bd_site", choices = choices)
  })

  filtered <- reactive({
    df <- rv$sites
    q <- tolower(trimws(input$search_txt %||% ""))
    if (nzchar(q)) {
      df <- df %>% filter(
        str_detect(tolower(coalesce(site_name, "")), fixed(q)) |
          str_detect(tolower(coalesce(city, "")), fixed(q)) |
          str_detect(tolower(coalesce(region, "")), fixed(q))
      )
    }
    if (length(input$status_filter) > 0) {
      df <- df %>% filter(status %in% input$status_filter)
    }
    df
  })

  list(rv = rv, filtered = filtered)
}
