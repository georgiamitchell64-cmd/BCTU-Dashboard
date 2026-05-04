trial_settings_server <- function(input, output, session, state) {
  rv <- state$rv

  # ── Theme picker ──────────────────────────────────────────────────────────
  selected_theme <- reactiveVal("custom")

  output$theme_picker_ui <- renderUI({
    active <- selected_theme()
    cards <- lapply(names(TRIAL_THEMES), function(key) {
      th <- TRIAL_THEMES[[key]]
      is_active <- identical(active, key)
      border_col <- if (is_active) th$secondary else "#EEF2F7"
      shadow <- if (is_active)
        sprintf("box-shadow: 0 0 0 2px %s;", th$secondary) else ""

      div(
        onclick = sprintf("Shiny.setInputValue('pick_theme', '%s', {priority:'event'})", key),
        style = sprintf("border:1px solid %s; %s
                         border-radius:12px; padding:12px; cursor:pointer;
                         background:#FFFFFF; transition: all .15s;",
                        border_col, shadow),

        # Swatch row
        div(style = "display:flex;gap:4px;margin-bottom:10px;",
            div(style = sprintf("flex:2;height:24px;border-radius:5px;background:%s;", th$primary)),
            div(style = sprintf("flex:1;height:24px;border-radius:5px;background:%s;", th$secondary)),
            div(style = sprintf("flex:1;height:24px;border-radius:5px;background:%s;", th$accent))),

        div(style = "display:flex;justify-content:space-between;align-items:center;",
            div(style = "font-size:13px;font-weight:600;color:#0F172A;", th$label),
            if (is_active)
              span(style = sprintf("font-size:10px;color:%s;font-weight:700;
                                    text-transform:uppercase;letter-spacing:.5px;", th$secondary),
                   HTML("&#10003; Active"))
        ),
        div(style = "font-size:11px;color:#64748B;margin-top:2px;", th$sublabel),
        div(style = "font-size:10px;color:#94A3B8;margin-top:4px;
                     text-transform:uppercase;letter-spacing:.5px;",
            sprintf("Sidebar: %s", th$sidebar))
      )
    })

    custom_card <- div(
      onclick = "Shiny.setInputValue('pick_theme', 'custom', {priority:'event'})",
      style = sprintf("border:1px dashed %s;border-radius:12px;padding:12px;cursor:pointer;
                       background:#FAFBFD;transition:all .15s;%s",
                      if (identical(active, "custom")) "#6366F1" else "#CBD5E1",
                      if (identical(active, "custom")) "box-shadow:0 0 0 2px #6366F1;" else ""),
      div(style = "display:flex;gap:4px;margin-bottom:10px;",
          div(style = "flex:2;height:24px;border-radius:5px;
                       background:repeating-linear-gradient(45deg,#E2E8F0 0 4px,#F1F5F9 4px 8px);"),
          div(style = "flex:1;height:24px;border-radius:5px;
                       background:repeating-linear-gradient(45deg,#E2E8F0 0 4px,#F1F5F9 4px 8px);"),
          div(style = "flex:1;height:24px;border-radius:5px;
                       background:repeating-linear-gradient(45deg,#E2E8F0 0 4px,#F1F5F9 4px 8px);")),
      div(style = "font-size:13px;font-weight:600;color:#0F172A;", "Custom"),
      div(style = "font-size:11px;color:#64748B;margin-top:2px;",
          "Set your own colours")
    )

    div(style = "display:grid;grid-template-columns:repeat(auto-fill,minmax(180px,1fr));gap:10px;",
        cards, custom_card)
  })

  observeEvent(input$pick_theme, {
    key <- input$pick_theme
    selected_theme(key)
    if (key == "custom") {
      shinyjs::show("custom_colors_panel")
    } else {
      shinyjs::hide("custom_colors_panel")
      th <- TRIAL_THEMES[[key]]
      if (!is.null(th)) {
        updateTextInput(session, "set_col_primary",   value = th$primary)
        updateTextInput(session, "set_col_secondary", value = th$secondary)
        updateTextInput(session, "set_col_accent",    value = th$accent)
      }
    }
  })

  # ── Live preview ──────────────────────────────────────────────────────────
  output$color_preview_bar <- renderUI({
    p <- input$set_col_primary   %||% "#1B4F6B"
    s <- input$set_col_secondary %||% "#2EC4A5"
    a <- input$set_col_accent    %||% "#F59E0B"

    runjs(sprintf("$('#preview_primary').css('background','%s')", p))
    runjs(sprintf("$('#preview_secondary').css('background','%s')", s))
    runjs(sprintf("$('#preview_accent').css('background','%s')", a))

    div(style = "display:flex;gap:2px;border-radius:6px;overflow:hidden;height:40px;",
        div(style = sprintf("flex:3;background:%s;display:flex;align-items:center;justify-content:center;
                             color:#fff;font-size:11px;font-weight:600;", p), "Sidebar"),
        div(style = "flex:5;background:#EEF3F8;display:flex;align-items:center;padding:0 12px;",
            div(style = sprintf("background:%s;color:#fff;padding:3px 10px;border-radius:4px;
                                 font-size:10px;font-weight:600;margin-right:8px;", s), "Chart"),
            div(style = sprintf("background:%s;color:#fff;padding:3px 10px;border-radius:4px;
                                 font-size:10px;font-weight:600;", a), "Accent")
        )
    )
  })

  # ── Populate fields when trial is loaded ──────────────────────────────────
  observeEvent(rv$trial_config, {
    cfg <- rv$trial_config
    if (is.null(cfg)) return()

    cols <- cfg$colors %||% list(primary = "#1B4F6B", secondary = "#2EC4A5", accent = "#F59E0B")
    updateTextInput(session, "set_col_primary",   value = cols$primary)
    updateTextInput(session, "set_col_secondary", value = cols$secondary)
    updateTextInput(session, "set_col_accent",    value = cols$accent)

    # Theme: if cfg has one, use it; otherwise fall back to "custom".
    saved_theme <- cfg$theme %||% "custom"
    if (!saved_theme %in% c(names(TRIAL_THEMES), "custom")) saved_theme <- "custom"
    selected_theme(saved_theme)
    if (saved_theme == "custom") shinyjs::show("custom_colors_panel")
    else                          shinyjs::hide("custom_colors_panel")

    feat <- cfg$features %||% list()
    updateCheckboxInput(session, "set_feat_projections", value = isTRUE(feat$projections))
    updateCheckboxInput(session, "set_feat_pilot",       value = isTRUE(feat$pilot_criteria))
    updateCheckboxInput(session, "set_feat_postal",      value = isTRUE(feat$postal_tracking))
    updateCheckboxInput(session, "set_feat_returns",     value = isTRUE(feat$return_rates))
    updateCheckboxInput(session, "set_feat_consort",     value = isTRUE(feat$consort_flow))
    updateCheckboxInput(session, "set_feat_baseline",    value = isTRUE(feat$baseline_table))

    updateTextInput(session,    "set_short_name", value = cfg$short_name %||% "")
    updateTextInput(session,    "set_full_name",  value = cfg$name %||% "")
    updateNumericInput(session, "set_target",     value = cfg$trial_target %||% 100)
    updateSelectInput(session,  "set_category",   selected = trial_category(cfg))
    rd <- cfg$report_defaults %||% list()
    updateTextInput(session, "set_ci",      value = rd$ci %||% "")
    updateTextInput(session, "set_sponsor", value = rd$sponsor %||% "")
  })

  # ── Config file path + override status ───────────────────────────────────
  output$settings_config_path <- renderUI({
    cfg <- rv$trial_config
    if (is.null(cfg)) return(span("No trial selected."))
    div(HTML(paste0("Config file: <code>",
                    file.path(cfg$trial_dir, "config.R"), "</code>")))
  })

  output$settings_overrides_status <- renderUI({
    cfg <- rv$trial_config
    if (is.null(cfg)) return(NULL)
    rv$settings_changed   # invalidate when settings change
    path <- overrides_path(cfg)
    if (file.exists(path)) {
      div(style = "color:#0F172A;",
          HTML(paste0("Overrides: <code>", path, "</code> ",
                      "<span style='color:#6366F1;font-weight:600;'>active</span>")))
    } else {
      div(HTML("Overrides: <em>none — using config.R as-is</em>"))
    }
  })

  # ── Apply features live (called after save / reset) ──────────────────────
  apply_features_live <- function(feat) {
    if (isTRUE(feat$postal_tracking)) shinyjs::show("go_postal_wrap")
    else                              shinyjs::hide("go_postal_wrap")
    if (isTRUE(feat$return_rates))    shinyjs::show("go_returns_wrap")
    else                              shinyjs::hide("go_returns_wrap")
  }

  # ── Save theme + colours ─────────────────────────────────────────────────
  observeEvent(input$settings_save_colors, {
    if (!require_role(rv, "manager")) return()
    cfg <- rv$trial_config
    if (is.null(cfg)) return()

    theme_key <- selected_theme()
    if (theme_key %in% names(TRIAL_THEMES)) {
      th <- TRIAL_THEMES[[theme_key]]
      new_colors <- list(primary = th$primary, secondary = th$secondary, accent = th$accent)
      sidebar_variant <- th$sidebar
    } else {
      new_colors <- list(
        primary   = input$set_col_primary   %||% "#1B4F6B",
        secondary = input$set_col_secondary %||% "#2EC4A5",
        accent    = input$set_col_accent    %||% "#F59E0B"
      )
      sidebar_variant <- "dark"
    }

    update_overrides(cfg, theme = theme_key, colors = new_colors)
    rv$trial_config$theme  <- theme_key
    rv$trial_config$colors <- new_colors
    apply_trial_colours(new_colors, sidebar = sidebar_variant)
    rv$settings_changed <- Sys.time()

    msg <- if (theme_key == "custom") "Custom colours applied."
           else sprintf("%s theme applied.", TRIAL_THEMES[[theme_key]]$label)
    showNotification(HTML(paste0("&#x2714; ", msg)), type = "message", duration = 4)
  })

  # ── Save identity + features ─────────────────────────────────────────────
  observeEvent(input$settings_save_features, {
    if (!require_role(rv, "manager")) return()
    cfg <- rv$trial_config
    if (is.null(cfg)) return()

    new_target   <- as.integer(input$set_target %||% cfg$trial_target %||% 100L)
    new_short    <- input$set_short_name %||% cfg$short_name %||% ""
    new_full     <- input$set_full_name  %||% cfg$name       %||% ""
    new_category <- input$set_category   %||% trial_category(cfg)

    new_features <- list(
      postal_tracking  = isTRUE(input$set_feat_postal),
      return_rates     = isTRUE(input$set_feat_returns),
      projections      = isTRUE(input$set_feat_projections),
      pilot_criteria   = isTRUE(input$set_feat_pilot),
      consort_flow     = isTRUE(input$set_feat_consort),
      baseline_table   = isTRUE(input$set_feat_baseline)
    )

    new_report_defaults <- list(
      ci      = input$set_ci      %||% (cfg$report_defaults$ci      %||% ""),
      sponsor = input$set_sponsor %||% (cfg$report_defaults$sponsor %||% "")
    )

    update_overrides(cfg,
      short_name       = new_short,
      name             = new_full,
      trial_target     = new_target,
      category         = new_category,
      features         = new_features,
      report_defaults  = new_report_defaults
    )

    # Update in-memory config so the rest of the app sees changes immediately.
    rv$trial_config$short_name       <- new_short
    rv$trial_config$name             <- new_full
    rv$trial_config$trial_target     <- new_target
    rv$trial_config$category         <- new_category
    rv$trial_config$features         <- new_features
    rv$trial_config$report_defaults  <- new_report_defaults

    # Re-apply globals so TRIAL_TARGET etc. refresh.
    apply_trial_globals(rv$trial_config)
    apply_features_live(new_features)

    # Update topbar title in case short_name changed.
    runjs(sprintf("$('.topbar-title').text('%s Site Tracker')",
                  gsub("'", "\\\\'", new_short)))
    runjs(sprintf("document.title = '%s Dashboard'",
                  gsub("'", "\\\\'", new_short)))

    rv$settings_changed <- Sys.time()
    rv$home_membership_changed <- Sys.time()  # refresh home cards too

    log_activity("settings_saved",
                 sprintf("Updated trial settings for <strong>%s</strong>",
                         htmltools::htmlEscape(new_short)),
                 username = rv$username, trial_code = cfg$code)
    showNotification(HTML("&#x2714; Settings saved."), type = "message", duration = 4)
  })

  # ── Reset to defaults (delete overrides.json) ────────────────────────────
  observeEvent(input$settings_reset_overrides, {
    cfg <- rv$trial_config
    if (is.null(cfg)) return()

    showModal(modalDialog(
      title = div(style = "color:#DC2626;",
                  HTML("&#x26A0; Reset to defaults?")),
      size = "s", easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("settings_reset_confirm", "Yes, reset",
                     class = "btn btn-danger",
                     style = "background:#DC2626;border-color:#DC2626;font-weight:600;")
      ),
      div(style = "padding:6px 0;font-size:13px;line-height:1.6;",
          HTML("This deletes the trial's <code>overrides.json</code> and reverts
                identity, features, and colours to whatever's in <code>config.R</code>.
                The original config file is not touched."))
    ))
  })

  # ── Danger zone (admin only) ─────────────────────────────────────────────
  output$settings_danger_zone_ui <- renderUI({
    if (!isTRUE(rv$portfolio_role == "admin")) return(NULL)
    div(style = "background:#FFFFFF;border:1px solid #FEE2E2;border-radius:14px;
                 padding:18px 22px;",
        div(style = "display:flex;align-items:center;gap:10px;margin-bottom:8px;",
            span(style = "color:#DC2626;font-size:18px;", HTML("&#x26A0;")),
            span(style = "font-weight:600;color:#991B1B;font-size:14px;",
                 "Danger zone")),
        div(style = "font-size:12.5px;color:#7F1D1D;line-height:1.6;margin-bottom:12px;",
            HTML("Deleting a trial removes its config, sites, randomisation log,
                  and any uploaded REDCap exports. <strong>This cannot be undone.</strong>")),
        actionButton("settings_delete_trial",
                     HTML("&#x1F5D1; Delete this trial"),
                     class = "btn btn-sm",
                     style = "background:#FFFFFF;color:#DC2626;border:1px solid #FECACA;
                              font-weight:600;"))
  })

  observeEvent(input$settings_delete_trial, {
    if (!isTRUE(rv$portfolio_role == "admin")) return()
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    showModal(modalDialog(
      title = div(style = "color:#DC2626;",
                  HTML("&#x26A0; Delete trial?")),
      size = "m", easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("settings_delete_trial_confirm",
                     "Yes, delete this trial",
                     class = "btn btn-danger",
                     style = "background:#DC2626;border-color:#DC2626;font-weight:600;")
      ),
      div(style = "padding:6px 0;font-size:13px;line-height:1.7;",
          HTML(sprintf("You're about to permanently delete <strong>%s</strong>.<br><br>
                        The trial folder <code>trials/%s/</code> will be removed,
                        including its config, sites, randomisation log, and any
                        REDCap CSVs. This action cannot be undone.",
                       cfg$short_name %||% toupper(cfg$code),
                       cfg$code)))
    ))
  })

  observeEvent(input$settings_delete_trial_confirm, {
    if (!isTRUE(rv$portfolio_role == "admin")) return()
    if (!require_role(rv, "manager")) return()
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    code <- cfg$code

    trial_dir <- file.path(getwd(), "trials", code)
    success <- tryCatch({
      unlink(trial_dir, recursive = TRUE, force = TRUE)
      TRUE
    }, error = function(e) FALSE)

    logo_path <- file.path(getwd(), "www", "trial_logos", paste0(code, ".jpg"))
    if (file.exists(logo_path)) file.remove(logo_path)

    removeModal()

    if (success) {
      log_activity("trial_deleted",
                   sprintf("Deleted trial <strong>%s</strong>",
                           htmltools::htmlEscape(cfg$short_name %||% toupper(code))),
                   username = rv$username, trial_code = code)
      showNotification(sprintf("Trial '%s' deleted.",
                               cfg$short_name %||% toupper(code)),
                       type = "message", duration = 5)
      # Send the user back to the home screen
      rv$trial_config <- NULL
      rv$trial_code   <- NULL
      rv$trial_role   <- NULL
      rv$home_membership_changed <- Sys.time()
      shinyjs::hide("dashboard_panel")
      shinyjs::hide("sidebar_nav_section")
      shinyjs::hide("topbar_wrap")
      shinyjs::show("trial_selector_panel")
      shinyjs::runjs("document.body.classList.add('home-mode')")
    } else {
      showNotification("Could not delete trial folder. It may be in use.",
                       type = "error", duration = 8)
    }
  })

  observeEvent(input$settings_reset_confirm, {
    cfg <- rv$trial_config
    if (is.null(cfg)) return()

    clear_overrides(cfg)
    removeModal()

    # Reload original config from disk (without overrides) and refresh.
    fresh <- discover_trials()[[cfg$code]]
    if (!is.null(fresh)) {
      rv$trial_config <- fresh
      apply_trial_globals(fresh)
      .tk <- fresh$theme %||% "custom"
      .sv <- if (.tk %in% names(TRIAL_THEMES)) TRIAL_THEMES[[.tk]]$sidebar else "dark"
      apply_trial_colours(
        fresh$colors %||% list(primary = "#1B4F6B", secondary = "#2EC4A5", accent = "#F59E0B"),
        sidebar = .sv)
      apply_features_live(fresh$features %||% list())
    }

    rv$settings_changed <- Sys.time()
    rv$home_membership_changed <- Sys.time()
    showNotification(HTML("&#x2714; Overrides cleared."),
                     type = "message", duration = 4)
  })
}
