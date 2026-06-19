init_app_state <- function(input, output, session) {
  # Discover available trials
  available_trials <- discover_trials()

  rv <- reactiveValues(
    # ── Trial selection state ──────────────────────────────────────────────
    available_trials = available_trials,
    trial_config     = NULL,
    trial_code       = NULL,
    # ── Multi-WP state ─────────────────────────────────────────────────────
    # NULL means "Overview / All work packages". Otherwise an integer index
    # into cfg$work_packages so downstream code can look up the WP label.
    active_wp        = NULL,

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

  # ── Session-scope the trial globals ────────────────────────────────────────
  # apply_trial_globals() sets process-wide globals (TRIAL_TARGET, DATA_DIR,
  # DB_PATH, .TRIAL_CFG). With several concurrent users on different trials,
  # whoever selected a trial last would win and other sessions would read —
  # and worse, db_save to — the wrong trial's database. Shiny runs one
  # session's flush at a time, so re-applying this session's config before
  # every flush makes the globals effectively session-scoped.
  session$onFlush(function() {
    cfg <- isolate(rv$trial_config)
    if (!is.null(cfg)) apply_trial_globals(cfg)
  }, once = FALSE)

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
    shinyjs::hide("topnav_wrap")             # Hide top tab bar
    runjs("document.body.classList.add('home-mode')")

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
  output$topbar_acct_name <- renderText({ req(rv$username); rv$username })
  output$topbar_acct_initials <- renderText({
    nm <- rv$username
    if (is.null(nm) || !nzchar(nm)) return("?")
    parts <- strsplit(trimws(nm), "\\s+")[[1]]
    parts <- parts[nzchar(parts)]
    if (!length(parts)) return("?")
    toupper(paste0(substr(parts[1], 1, 1),
                   if (length(parts) > 1) substr(parts[length(parts)], 1, 1) else ""))
  })
  output$topbar_trial_label <- renderUI({
    cfg <- rv$trial_config
    if (is.null(cfg)) return(span("BCTU Trials Dashboard"))
    name <- cfg$short_name %||% toupper(cfg$code %||% "")
    logo <- trial_logo_url(cfg)
    # When the trial has a logo, show it left of the name. The white-pill
    # background keeps colour logos legible on the navy topbar.
    if (!is.null(logo)) {
      tagList(
        tags$span(
          style = "display:inline-flex;align-items:center;gap:8px;",
          tags$span(
            style = "background:#fff;border-radius:6px;padding:2px 6px;
                     display:inline-flex;align-items:center;justify-content:center;
                     height:28px;",
            tags$img(src = logo,
                     style = "max-height:24px;max-width:120px;
                              object-fit:contain;display:block;",
                     alt = name)
          ),
          tags$span(name)
        )
      )
    } else {
      span(name)
    }
  })

  output$topbar_view_badge <- renderText({ "" })

  # ── Work-package picker (multi-WP trials only) ───────────────────────────
  # Visible whenever the active trial has cfg$work_packages defined. Selecting
  # a pill writes the WP index into rv$active_wp; "Overview" resets to NULL
  # so downstream filters know to roll up across all WPs.
  output$wp_picker_ui <- renderUI({
    cfg <- rv$trial_config
    if (is.null(cfg)) return(NULL)
    wps <- cfg$work_packages
    if (is.null(wps) || !length(wps)) return(NULL)

    # Strip the "WKPn: " prefix from stored values for the button label, but
    # keep the raw index so we know which WP got picked.
    pretty <- vapply(wps, function(s) {
      s <- as.character(s)
      sub("^WKP[0-9]+:\\s*", "", s)
    }, character(1))

    active <- rv$active_wp   # NULL = overview
    pills <- lapply(seq_along(wps), function(i) {
      lbl <- if (nzchar(pretty[i])) sprintf("WKP%d · %s", i, pretty[i])
             else                    sprintf("WKP%d", i)
      tags$button(
        id = paste0("wp_pill_", i),
        class = paste("wp-pill action-button",
                      if (!is.null(active) && active == i) "on" else ""),
        type = "button",
        onclick = sprintf(
          "Shiny.setInputValue('wp_pick', %d, {priority:'event'});
           setActiveWp('wp_pill_%d');", i, i),
        lbl)
    })

    overview_btn <- tags$button(
      id = "wp_pill_0",
      class = paste("wp-pill action-button",
                    if (is.null(active)) "on" else ""),
      type = "button",
      onclick = "Shiny.setInputValue('wp_pick', 0, {priority:'event'});
                 setActiveWp('wp_pill_0');",
      "Overview · all WPs")

    tagList(
      span(class = "wp-eye", "Work package"),
      overview_btn,
      pills
    )
  })

  observeEvent(input$wp_pick, {
    v <- suppressWarnings(as.integer(input$wp_pick))
    if (is.na(v) || v <= 0) rv$active_wp <- NULL else rv$active_wp <- v
  })

  # Show or hide the picker bar whenever the active config changes.
  observeEvent(rv$trial_config, {
    cfg <- rv$trial_config
    if (!is.null(cfg) && length(cfg$work_packages %||% character(0)) > 0)
      shinyjs::show("wp_picker_wrap")
    else
      shinyjs::hide("wp_picker_wrap")
    # Reset picker when switching trials
    rv$active_wp <- NULL
  }, ignoreNULL = FALSE)

  # ── Tab navigation ────────────────────────────────────────────────────────
  switch_tab <- function(tab_name, btn_id) {
    updateTabsetPanel(session, "active_tab", selected = tab_name)
    runjs(sprintf("setActiveNav('%s')", btn_id))
  }
  observeEvent(input$go_overview,       switch_tab("overview", "go_overview"))
  observeEvent(input$go_reports,        switch_tab("reports", "go_reports"))
  observeEvent(input$go_modifications,  switch_tab("modifications", "go_modifications"))
  observeEvent(input$go_charts,         switch_tab("charts",  "go_charts"))
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

  # ── Work-package-scoped data views ────────────────────────────────────────
  # When a WP pill is active (rv$active_wp set) these return only that work
  # package's records; with "Overview · all WPs" selected (active_wp = NULL)
  # they return the full trial data — the roll-up summary. Dashboard modules
  # read from these so the whole dashboard follows the WP picker.
  #
  # The canonical rv$sites / rv$participants / rv$raw_redcap stores are never
  # filtered here, so the DB save paths always persist the complete dataset.

  parts_wp <- reactive({
    p  <- rv$participants
    wp <- rv$active_wp
    if (is.null(wp) || is.null(p) || !"work_package" %in% names(p)) return(p)
    p %>% filter(!is.na(work_package), work_package == wp)
  })

  redcap_wp <- reactive({
    d  <- rv$raw_redcap
    wp <- rv$active_wp
    if (is.null(wp) || is.null(d) || !"work_package" %in% names(d)) return(d)
    d %>% filter(!is.na(work_package),
                 suppressWarnings(as.integer(work_package)) == wp)
  })

  # Per-WP randomised count per site (one Baseline row per participant). Used to
  # rescale the site list so KPIs / maps / tables reflect the active WP.
  .wp_site_counts <- reactive({
    wp <- rv$active_wp
    if (is.null(wp) || !"work_package" %in% names(rv$participants)) return(NULL)
    rv$participants %>%
      filter(!is.na(work_package), work_package == wp,
             event_type == "Baseline",
             !is.na(site_dag), nchar(trimws(site_dag)) > 0) %>%
      count(site_dag, name = "wp_rand")
  })

  sites_wp <- reactive({
    s   <- rv$sites
    cnt <- .wp_site_counts()
    if (is.null(cnt) || is.null(s) || !nrow(s)) return(s)
    s %>%
      left_join(cnt, by = c("site_name" = "site_dag")) %>%
      mutate(randomised = coalesce(as.integer(wp_rand), 0L)) %>%
      select(-wp_rand) %>%
      filter(site_name %in% cnt$site_dag)
  })

  filtered <- reactive({
    df <- sites_wp()
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

  list(rv = rv, filtered = filtered,
       parts_wp = parts_wp, redcap_wp = redcap_wp, sites_wp = sites_wp)
}
