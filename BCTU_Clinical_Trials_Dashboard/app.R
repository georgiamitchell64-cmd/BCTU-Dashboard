# =============================================================================
# Clinical Trials Dashboard — Multi-Trial Entry Point
# =============================================================================
# Users self-register a profile on first visit and sign in with a password.
# Trial access is granted per-user from the Accounts tab (see
# functions/permissions.R for the role model).
# =============================================================================

source("globals/packages.R",       local = TRUE)
# app_paths.R must come first: constants.R and database.R resolve their
# writable locations through app_data_dir().
source("globals/app_paths.R",      local = TRUE)
source("globals/constants.R",      local = TRUE)
source("globals/datasets.R",       local = TRUE)
source("globals/trial_config.R",   local = TRUE)
source("globals/trial_templates.R", local = TRUE)

source("functions/helpers.R",            local = TRUE)
source("functions/help_tooltips.R",      local = TRUE)
source("functions/theme.R",             local = TRUE)
source("functions/ui_helpers.R",         local = TRUE)
source("functions/database.R",          local = TRUE)
source("functions/permissions.R",       local = TRUE)
source("functions/trial_overrides.R",   local = TRUE)
source("functions/cross_trial_sites.R", local = TRUE)
source("functions/smart_insights.R",    local = TRUE)
source("functions/portfolio_insights.R", local = TRUE)
source("functions/participant_breakdowns.R", local = TRUE)
source("functions/notifications.R",      local = TRUE)
source("functions/report_sections.R",    local = TRUE)
source("functions/report_builder.R",     local = TRUE)
source("functions/backup_restore.R",     local = TRUE)
source("functions/activity_log.R",       local = TRUE)
source("functions/adapters/adapter_api.R",     local = TRUE)
source("functions/adapters/redcap_csv.R",      local = TRUE)
source("functions/adapters/generic_tabular.R", local = TRUE)
source("functions/pipeline/concepts.R",        local = TRUE)
source("functions/pipeline/suggest.R",         local = TRUE)
source("functions/pipeline/transform.R",       local = TRUE)
source("functions/pipeline/validate.R",        local = TRUE)
source("functions/pipeline/canonical_store.R", local = TRUE)
source("functions/pipeline/trial_config_json.R", local = TRUE)
source("functions/csv_autodetect.R",    local = TRUE)
source("functions/autodetect_modal.R",  local = TRUE)
source("functions/apply_colours.R",     local = TRUE)
source("functions/layout.R",            local = TRUE)
source("functions/prepare_report_data.R", local = TRUE)
source("functions/html_to_docx.R",      local = TRUE)
source("functions/tsc_charts.R",        local = TRUE)
source("functions/geocoding.R",         local = TRUE)
source("functions/consort_flow.R",      local = TRUE)
source("functions/flat_completeness.R", local = TRUE)
source("functions/baseline_table.R",    local = TRUE)
source("functions/return_rates_data.R", local = TRUE)
source("functions/projection_math.R",   local = TRUE)
source("functions/postal_tracking_data.R", local = TRUE)
source("functions/safety_events.R",      local = TRUE)

source("modules/registry.R",            local = TRUE)
source("modules/welcome.R",             local = TRUE)
source("modules/welcome_server.R",      local = TRUE)
source("modules/core.R",                local = TRUE)
source("modules/trial_selector.R",      local = TRUE)
source("modules/new_trial.R",           local = TRUE)
source("modules/trial_selector_server.R", local = TRUE)
source("modules/overview.R",            local = TRUE)
source("modules/overview_server.R",     local = TRUE)
source("modules/reports.R",             local = TRUE)
source("modules/reports_server.R",      local = TRUE)
source("modules/charts.R",              local = TRUE)
source("modules/randomisations.R",      local = TRUE)
source("modules/randomisations_server.R", local = TRUE)
source("modules/participants.R",        local = TRUE)
source("modules/participants_server.R", local = TRUE)
source("modules/sites.R",              local = TRUE)
source("modules/sites_server.R",        local = TRUE)
source("modules/upload.R",             local = TRUE)
source("modules/upload_server.R",       local = TRUE)
source("modules/accounts.R",           local = TRUE)
source("modules/accounts_server.R",     local = TRUE)
source("modules/return_rates_ui.R",     local = TRUE)
source("modules/return_rates_server.R", local = TRUE)
# Postal tracking modules removed in the desktop build.
source("modules/trial_settings.R",          local = TRUE)
source("modules/trial_settings_server.R",   local = TRUE)
source("modules/modifications.R",           local = TRUE)
source("modules/modifications_server.R",    local = TRUE)


# ── Seed writable user data, then initialise databases ────────────────────────
# In the desktop build BCTU_DATA_ROOT points at a per-user writable folder that
# survives app updates. seed_user_data() copies the trial configs out of the
# read-only bundle on first launch (configs and logos only, never databases —
# a fresh install starts with an empty database).
seed_user_data()
message("Data root: ", app_data_root())
db_init()
shared_db_init()
notifications_db_init()
activity_db_init()

# ── Copy trial logos to www/ ──────────────────────────────────────────────────
trials <- discover_trials()
logo_dir <- file.path(getwd(), "www", "trial_logos")
if (!dir.exists(logo_dir)) dir.create(logo_dir, recursive = TRUE)
for (code in names(trials)) {
  cfg <- trials[[code]]
  logo <- cfg$logo_file
  if (!is.null(logo) && file.exists(logo)) {
    ext <- tools::file_ext(logo)
    file.copy(logo, file.path(logo_dir, paste0(code, ".", ext)), overwrite = TRUE)
  }
}

# ── UI (no shinymanager — just the app with welcome overlay) ──────────────────
ui <- build_app_ui()

server <- function(input, output, session) {
  state <- init_app_state(input, output, session)

  # ── Desktop build: no login ─────────────────────────────────────────────────
  # This is a single-user app on the user's own machine, so the self-registration
  # / password gate is bypassed. welcome_server() is deliberately NOT called, but
  # modules/welcome_server.R is still sourced because trial_selector_server()
  # calls apply_trial_role_visibility() from it.
  # Full rights are granted so manager-only controls (.tm-only) stay visible.
  state$rv$username <- Sys.getenv("USERNAME", Sys.getenv("USER", "Local user"))
  shinyjs::hide("welcome_screen")
  shinyjs::runjs("$('.tm-only').show(); $('.dl-data-btn').show();")

  # Trial selector
  tryCatch(trial_selector_server(input, output, session, state),
           error = function(e) message("TRIAL SELECTOR: ", e$message))

  # Module servers
  tryCatch(overview_server(input, output, session, state),
           error = function(e) message("OVERVIEW: ", e$message))
  tryCatch(reports_server(input, output, session, state),
           error = function(e) message("REPORTS: ", e$message))
  tryCatch(randomisations_server(input, output, session, state),
           error = function(e) message("RANDOMISATIONS: ", e$message))
  tryCatch(participants_server(input, output, session, state),
           error = function(e) message("PARTICIPANTS: ", e$message))
  tryCatch(sites_server(input, output, session, state),
           error = function(e) message("SITES: ", e$message))
  tryCatch(upload_server(input, output, session, state),
           error = function(e) message("UPLOAD: ", e$message))
  # Accounts tab removed in the desktop build (no login, single user).
  tryCatch(trial_settings_server(input, output, session, state),
           error = function(e) message("SETTINGS: ", e$message))
  tryCatch(modifications_tab_server(input, output, session, state),
           error = function(e) message("MODIFICATIONS: ", e$message))

  rr_data <- reactive({
    req(state$rv$trial_code)
    invalidateLater(5 * 60 * 1000)
    cfg <- state$rv$trial_config
    load_return_rates(
      dir        = cfg$return_rates_dir,        # folder pasted in Trial Settings
      trial_code = state$rv$trial_code
    )
  })
  tryCatch(return_rates_server("rr", rr_data = rr_data),
           error = function(e) message("RETURN RATES: ", e$message))

  # Postal tracking tab removed in the desktop build.

  session$onSessionEnded(function() {
    tryCatch({
      # This callback runs outside the reactive flush, so the process-wide
      # trial globals (DB_PATH etc.) may belong to another user's session.
      # Re-apply this session's config before saving so the data lands in
      # the right trial database.
      cfg <- isolate(state$rv$trial_config)
      if (is.null(cfg)) return(invisible(NULL))
      apply_trial_globals(cfg)
      isolate(db_save_all(state$rv$sites, state$rv$log, state$rv$accounts))
    }, error = function(e) message("DB save error: ", e$message))
  })
}

shinyApp(ui, server)
