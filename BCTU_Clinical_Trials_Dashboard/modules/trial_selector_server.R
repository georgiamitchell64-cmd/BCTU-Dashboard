trial_selector_server <- function(input, output, session, state) {
  rv <- state$rv

  # ══════════════════════════════════════════════════════════════════════════

  # TRIAL CARD GRID
  # ══════════════════════════════════════════════════════════════════════════

  output$trial_cards_ui <- renderUI({
    trials <- rv$available_trials
    # Force re-render when a new trial is created
    input$wiz_create

    # Re-discover trials to pick up newly created ones
    trials <- discover_trials()
    rv$available_trials <- trials

    cards <- lapply(names(trials), function(code) {
      cfg <- trials[[code]]
      logo_exists <- !is.null(cfg$logo_file) && file.exists(cfg$logo_file)
      cols <- cfg$colors %||% list(primary = "#1B4F6B", secondary = "#2EC4A5")
      is_tm <- isTRUE(rv$role == "Trial Manager")

      div(
        class = "trial-select-card",
        style = sprintf("border-top:4px solid %s;position:relative;", cols$secondary),
        onclick = sprintf("Shiny.setInputValue('select_trial', '%s', {priority: 'event'})", code),

        # Delete button (TM only, top-right corner)
        if (is_tm) {
          tags$button(
            HTML("&times;"),
            onclick = sprintf("event.stopPropagation();
                               Shiny.setInputValue('delete_trial', '%s', {priority: 'event'});
                               return false;", code),
            style = "position:absolute;top:8px;right:8px;width:24px;height:24px;
                     border-radius:50%;border:1px solid #E2EAF0;background:#F8FAFD;
                     color:#94A3B8;font-size:18px;line-height:1;cursor:pointer;
                     display:flex;align-items:center;justify-content:center;
                     transition:all 0.15s;font-family:'Outfit',sans-serif;",
            onmouseover = "this.style.background='#FEF2F2';this.style.color='#EF4444';this.style.borderColor='#FECACA';",
            onmouseout  = "this.style.background='#F8FAFD';this.style.color='#94A3B8';this.style.borderColor='#E2EAF0';",
            title = "Delete trial"
          )
        },

        if (logo_exists) {
          tags$img(src = paste0("trial_logos/", code, ".jpg"),
                   style = "height:44px;margin-bottom:12px;object-fit:contain;")
        } else {
          div(style = sprintf("width:54px;height:54px;border-radius:14px;margin:0 auto 12px;
                               background:linear-gradient(135deg,%s,%s);
                               display:flex;align-items:center;justify-content:center;
                               font-size:20px;font-weight:700;color:#fff;",
                              cols$primary, cols$secondary),
              toupper(substr(cfg$short_name %||% code, 1, 3)))
        },

        div(style = "font-size:16px;font-weight:700;color:#1E293B;margin-bottom:4px;",
            cfg$short_name %||% toupper(code)),
        div(style = "font-size:11px;color:#64748B;line-height:1.4;margin-bottom:12px;min-height:30px;",
            cfg$name %||% ""),
        div(style = "display:flex;justify-content:center;gap:8px;",
            span(class = "pill pi",
                 HTML(paste0("&#x1F3AF; Target: ", cfg$trial_target %||% "\u2014"))))
      )
    })

    # "Add New Trial" card (Trial Managers only)
    add_card <- NULL
    if (isTRUE(rv$role == "Trial Manager")) {
      add_card <- div(
        class = "trial-select-card",
        style = "border-top:4px dashed #CBD5E1;border-style:dashed;",
        onclick = "Shiny.setInputValue('open_wizard', Math.random(), {priority: 'event'})",

        div(style = "width:54px;height:54px;border-radius:14px;margin:0 auto 12px;
                     background:#F1F5F9;display:flex;align-items:center;justify-content:center;
                     font-size:26px;color:#94A3B8;", "+"),
        div(style = "font-size:16px;font-weight:700;color:var(--muted);margin-bottom:4px;",
            "Add New Trial"),
        div(style = "font-size:11px;color:var(--muted);line-height:1.4;margin-bottom:12px;min-height:30px;",
            "Set up a new trial dashboard")
      )
    }

    div(style = "display:grid;grid-template-columns:repeat(auto-fill,minmax(240px,1fr));gap:18px;
                 justify-items:center;",
        cards, add_card)
  })


  # ══════════════════════════════════════════════════════════════════════════
  # TRIAL SELECTION (existing logic)
  # ══════════════════════════════════════════════════════════════════════════

  observeEvent(input$select_trial, {
    code <- input$select_trial
    cfg  <- rv$available_trials[[code]]
    if (is.null(cfg)) return()

    showModal(modalDialog(
      div(style = "text-align:center;padding:30px 20px",
          div(style = "font-size:22px;font-weight:700;color:#1B4F6B;margin-bottom:10px",
              paste("Loading", cfg$short_name %||% toupper(code))),
          div(style = "font-size:12px;color:#64748B;margin-bottom:20px",
              "Preparing dashboard\u2026"),
          div(style = "width:60px;height:60px;margin:0 auto;border:4px solid #E2EAF0;
                       border-top:4px solid #2EC4A5;border-radius:50%;
                       animation:spin 1s linear infinite"),
          tags$style(HTML("@keyframes spin{to{transform:rotate(360deg)}}"))
      ),
      title = NULL, footer = NULL, easyClose = FALSE, size = "s"
    ))

    rv$trial_config <- cfg
    rv$trial_code   <- code

    apply_trial_globals(cfg)
    if (!dir.exists(dirname(DB_PATH))) dir.create(dirname(DB_PATH), recursive = TRUE)
    db_init()

    rv$sites <- db_load_sites()
    rv$log   <- db_load_log()

    trial_name <- cfg$short_name %||% toupper(code)
    runjs(sprintf("$('.topbar-title').text('%s Site Tracker')", trial_name))
    runjs(sprintf("document.title = '%s Dashboard'", trial_name))

    shinyjs::hide("trial_selector_panel")
    shinyjs::show("dashboard_panel")
    shinyjs::show("sidebar_nav_section")    # Show the sidebar nav
    shinyjs::show("topbar_wrap")            # Show the topbar

    rv$trigger_data_load <- Sys.time()

    # Apply feature flags — show/hide tabs based on config
    feat <- cfg$features %||% list()
    if (isTRUE(feat$postal_tracking)) shinyjs::show("go_postal_wrap") else shinyjs::hide("go_postal_wrap")
    if (isTRUE(feat$return_rates))    shinyjs::show("go_returns_wrap") else shinyjs::hide("go_returns_wrap")

    # Apply trial colours dynamically
    apply_trial_colours(cfg$colors %||% list(primary = "#1B4F6B", secondary = "#2EC4A5", accent = "#F59E0B"))
  })


  # ══════════════════════════════════════════════════════════════════════════
  # WIZARD: step navigation
  # ══════════════════════════════════════════════════════════════════════════

  wiz_step <- reactiveVal(1L)
  WIZ_TOTAL <- 6L

  # Open wizard modal
  observeEvent(input$open_wizard, {
    wiz_step(1L)
    showModal(new_trial_wizard_ui())
    shinyjs::hide("wiz_prev")
    shinyjs::hide("wiz_create")
    shinyjs::show("wiz_next")
  })

  # Step indicator
  output$wiz_step_indicator <- renderUI({
    step <- wiz_step()
    dots <- lapply(seq_len(WIZ_TOTAL), function(i) {
      active <- if (i == step) "background:#2EC4A5;" else "background:#DDE5EE;"
      span(style = paste0("width:10px;height:10px;border-radius:50%;display:inline-block;
                            margin:0 3px;transition:background .2s;", active))
    })
    div(style = "text-align:center;margin-bottom:18px;", dots)
  })

  # Next step
  observeEvent(input$wiz_next, {
    step <- wiz_step()

    # Validate step 1
    if (step == 1L) {
      sn <- trimws(input$wiz_short_name %||% "")
      if (!nzchar(sn)) {
        showNotification("Please enter a short name for the trial.", type = "warning")
        return()
      }
      # Check if trial code already exists
      code <- tolower(gsub("[^a-zA-Z0-9]", "", sn))
      if (code %in% names(rv$available_trials)) {
        showNotification(paste0("A trial with code '", code, "' already exists."), type = "warning")
        return()
      }
    }

    if (step < WIZ_TOTAL) {
      shinyjs::hide(paste0("wiz_step_", step))
      wiz_step(step + 1L)
      shinyjs::show(paste0("wiz_step_", step + 1L))

      # Button visibility
      shinyjs::show("wiz_prev")
      if (step + 1L == WIZ_TOTAL) {
        shinyjs::hide("wiz_next")
        shinyjs::show("wiz_create")
      }
    }
  })

  # Previous step
  observeEvent(input$wiz_prev, {
    step <- wiz_step()
    if (step > 1L) {
      shinyjs::hide(paste0("wiz_step_", step))
      wiz_step(step - 1L)
      shinyjs::show(paste0("wiz_step_", step - 1L))

      shinyjs::show("wiz_next")
      shinyjs::hide("wiz_create")
      if (step - 1L == 1L) shinyjs::hide("wiz_prev")
    }
  })

  # Review summary
  output$wiz_review_summary <- renderUI({
    sn   <- trimws(input$wiz_short_name %||% "")
    code <- tolower(gsub("[^a-zA-Z0-9]", "", sn))

    row <- function(label, value) {
      div(style = "display:flex;justify-content:space-between;padding:5px 0;
                    border-bottom:1px solid rgba(46,196,165,.15);font-size:13px;",
          span(style = "color:#64748B;font-weight:500;", label),
          span(style = "color:#1B4F6B;font-weight:600;", value))
    }

    data_loc <- if (input$wiz_data_source == "network" && nzchar(input$wiz_data_path %||% ""))
      input$wiz_data_path else paste0("trials/", code, "/data/")

    features_on <- c()
    if (isTRUE(input$wiz_feat_projections)) features_on <- c(features_on, "Projections")
    if (isTRUE(input$wiz_feat_postal))      features_on <- c(features_on, "Postal")
    if (isTRUE(input$wiz_feat_returns))     features_on <- c(features_on, "Return rates")
    if (isTRUE(input$wiz_feat_pilot))       features_on <- c(features_on, "Pilot criteria")
    if (isTRUE(input$wiz_feat_consort))     features_on <- c(features_on, "CONSORT")
    if (isTRUE(input$wiz_feat_baseline))    features_on <- c(features_on, "Baseline table")
    if (length(features_on) == 0) features_on <- "None"

    tagList(
      row("Trial code",          code),
      row("Short name",          sn),
      row("Full name",           input$wiz_full_name %||% "\u2014"),
      row("Target",              as.character(input$wiz_target %||% 100)),
      row("CI",                  input$wiz_ci %||% "\u2014"),
      row("Data location",       data_loc),
      row("Baseline event",      input$wiz_ev_baseline %||% "\u2014"),
      row("Features",            paste(features_on, collapse = ", "))
    )
  })


  # ══════════════════════════════════════════════════════════════════════════
  # DELETE TRIAL
  # ══════════════════════════════════════════════════════════════════════════

  observeEvent(input$delete_trial, {
    code <- input$delete_trial
    cfg  <- rv$available_trials[[code]]
    if (is.null(cfg)) return()

    showModal(modalDialog(
      title = div(style = "display:flex;align-items:center;gap:10px;color:#DC2626;",
                  span(style = "font-size:22px;", HTML("&#x26A0;")),
                  span("Delete trial?")),
      div(style = "padding:8px 0;",
          HTML(sprintf("Are you sure you want to delete <strong>%s</strong>?",
                       cfg$short_name %||% toupper(code))),
          tags$br(), tags$br(),
          div(style = "background:#FEF2F2;border-left:3px solid #DC2626;padding:12px 14px;
                       border-radius:6px;font-size:13px;color:#7F1D1D;line-height:1.6;",
              HTML("This will permanently delete:"),
              tags$ul(style = "margin:6px 0 0 16px;",
                      tags$li("The trial config file"),
                      tags$li("Any sites, randomisation logs, and dashboard data"),
                      tags$li("Uploaded REDCap exports stored in the trial folder")),
              tags$br(),
              tags$strong("This cannot be undone.")
          )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_trial", "Yes, delete trial",
                     class = "btn btn-danger",
                     style = "background:#DC2626;border-color:#DC2626;font-weight:600;")
      ),
      easyClose = TRUE,
      size = "m"
    ))

    # Store the code being deleted
    rv$pending_delete <- code
  })

  observeEvent(input$confirm_delete_trial, {
    code <- rv$pending_delete
    if (is.null(code)) return()

    trial_dir <- file.path(getwd(), "trials", code)
    success <- tryCatch({
      unlink(trial_dir, recursive = TRUE, force = TRUE)
      TRUE
    }, error = function(e) FALSE)

    # Also remove the logo if it was copied to www/
    logo_path <- file.path(getwd(), "www", "trial_logos", paste0(code, ".jpg"))
    if (file.exists(logo_path)) file.remove(logo_path)

    removeModal()

    if (success) {
      showNotification(sprintf("Trial '%s' deleted.", code),
                       type = "message", duration = 5)
      # Refresh trial list
      rv$available_trials <- discover_trials()
    } else {
      showNotification("Could not delete trial folder. It may be in use.",
                       type = "error", duration = 8)
    }

    rv$pending_delete <- NULL
  })


  # ══════════════════════════════════════════════════════════════════════════
  # WIZARD: create the trial
  # ══════════════════════════════════════════════════════════════════════════

  observeEvent(input$wiz_create, {
    sn   <- trimws(input$wiz_short_name %||% "")
    code <- tolower(gsub("[^a-zA-Z0-9]", "", sn))

    if (!nzchar(code)) {
      showNotification("Trial short name is required.", type = "error")
      return()
    }

    trials_dir <- file.path(getwd(), "trials")
    trial_dir  <- file.path(trials_dir, code)

    if (dir.exists(trial_dir)) {
      showNotification(paste0("Folder already exists: trials/", code), type = "error")
      return()
    }

    # Create folder structure
    dir.create(file.path(trial_dir, "www"),     recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(trial_dir, "data"),    recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(trial_dir, "reports"), recursive = TRUE, showWarnings = FALSE)

    # Helper: safely quote a string for R code
    rq <- function(x) {
      x <- x %||% ""
      if (!nzchar(trimws(x))) return("NULL")
      sprintf('"%s"', gsub('"', '\\\\"', x))
    }

    # Build data_dir line
    data_dir_line <- if (input$wiz_data_source == "network" && nzchar(input$wiz_data_path %||% "")) {
      sprintf('  data_dir = "%s",', gsub("\\\\", "/", input$wiz_data_path))
    } else {
      "  data_dir = NULL,    # uses trials/<code>/data/"
    }

    # Build sub_forms events
    sf_raw <- trimws(input$wiz_ev_subforms %||% "")
    sf_vec <- if (nzchar(sf_raw)) {
      parts <- trimws(strsplit(sf_raw, ",")[[1]])
      parts <- parts[nzchar(parts)]
      if (length(parts) > 0) sprintf('c(%s)', paste(sprintf('"%s"', parts), collapse = ", "))
      else "NULL"
    } else "NULL"

    # Build optional field lines
    opt_field <- function(name, input_id) {
      val <- trimws(input[[input_id]] %||% "")
      if (nzchar(val)) sprintf('    %-28s= "%s",', name, val) else sprintf('    %-28s= NULL,', name)
    }

    config_text <- sprintf('# ===========================================================================
# Trial Configuration: %s
# ===========================================================================
# Auto-generated by the dashboard wizard on %s
# Edit this file to fine-tune settings.
# ===========================================================================

trial_config <- list(

  # -- Identity --
  code         = "%s",
  name         = %s,
  short_name   = "%s",
  trial_target = %dL,

  # -- Branding --
  logo_file = NULL,
  colors = list(
    primary   = "%s",
    secondary = "%s",
    accent    = "%s"
  ),

  # -- Data source --
%s

  # -- REDCap events --
  redcap_events = list(
    baseline  = %s,
    discharge = %s,
    day_30    = %s,
    day_90    = %s,
    sub_forms = %s
  ),

  # -- REDCap field mappings --
  redcap_fields = list(
    record_id               = "%s",
    site_name               = "%s",
    randomisation_datetime  = "%s",

%s
%s
%s
%s
%s
%s

    follow_up_instruments = list(),
    cos_type              = %s
  ),

  # -- COS type labels (standard defaults) --
  cos_type_labels = c(
    "1" = "Death", "2" = "No Operation", "3" = "Part withdrawal",
    "4" = "Complete withdrawal", "5" = "Lost to follow-up"
  ),

  ethnicity_labels       = NULL,
  white_ethnicity_codes  = NULL,
  target_schedule        = NULL,
  participant_table_layout = NULL,

  projection_defaults = list(
    rate_central = 3.0, rate_optimistic = 4.0, rate_pessimistic = 2.0,
    sites_central = 2.0, sites_optimistic = 3.0, sites_pessimistic = 1.0,
    target_sites = 24
  ),

  report_defaults = list(
    ci      = %s,
    sponsor = %s
  ),

  # -- Feature flags --
  features = list(
    postal_tracking  = %s,
    return_rates     = %s,
    projections      = %s,
    pilot_criteria   = %s,
    consort_flow     = %s,
    baseline_table   = %s
  )
)
',
      toupper(sn),
      format(Sys.Date(), "%%d %%B %%Y"),
      code,
      rq(input$wiz_full_name),
      sn,
      as.integer(input$wiz_target %||% 100),
      input$wiz_col_primary %||% "#1B4F6B",
      input$wiz_col_secondary %||% "#2EC4A5",
      input$wiz_col_accent %||% "#F59E0B",
      data_dir_line,
      rq(input$wiz_ev_baseline),
      rq(input$wiz_ev_discharge),
      rq(input$wiz_ev_day30),
      rq(input$wiz_ev_day90),
      sf_vec,
      input$wiz_fld_record_id %||% "record_id",
      input$wiz_fld_site %||% "site_name",
      input$wiz_fld_rand_dt %||% "rand_dttm_s",
      opt_field("operation_date",   "wiz_fld_op_date"),
      opt_field("operation_datetime", "wiz_fld_op_date"),  # same field, both formats
      opt_field("discharge_date",   "wiz_fld_discharge_date"),
      opt_field("age",              "wiz_fld_age"),
      opt_field("sex",              "wiz_fld_sex"),
      opt_field("ethnicity",        "wiz_fld_ethnicity"),
      rq(input$wiz_fld_cos_type),
      rq(input$wiz_ci),
      rq(input$wiz_sponsor),
      if (isTRUE(input$wiz_feat_postal))      "TRUE" else "FALSE",
      if (isTRUE(input$wiz_feat_returns))     "TRUE" else "FALSE",
      if (isTRUE(input$wiz_feat_projections)) "TRUE" else "FALSE",
      if (isTRUE(input$wiz_feat_pilot))       "TRUE" else "FALSE",
      if (isTRUE(input$wiz_feat_consort))     "TRUE" else "FALSE",
      if (isTRUE(input$wiz_feat_baseline))    "TRUE" else "FALSE"
    )

    # Write config file
    tryCatch({
      writeLines(config_text, file.path(trial_dir, "config.R"))
      removeModal()
      showNotification(
        HTML(sprintf("Trial <strong>%s</strong> created successfully!<br>
                      Folder: <code>trials/%s/</code><br>
                      Place your REDCap CSV exports in the data folder and select the trial to begin.",
                     sn, code)),
        type = "message", duration = 10
      )
      # Refresh trial list
      rv$available_trials <- discover_trials()
    }, error = function(e) {
      showNotification(paste("Error creating trial:", e$message), type = "error", duration = 10)
    })
  })
}
