# ─────────────────────────────────────────────────────────────────────────────
# Trial settings — six tabs:
#   Trial profile · Appearance & modules · Data & mapping · Visits & CRFs ·
#   Recruitment & monitoring · Reports & admin
# Every card saves on its own, and "Find a setting" searches all six tabs.
# Servers: modules/trial_settings_server.R (identity, colours, features,
# schedule, demographics, detail fields, reports) and
# modules/settings_monitoring_server.R (mapping, CRFs, monitoring, import).
# ─────────────────────────────────────────────────────────────────────────────

.ST_SECTIONS <- list(
  list(id = "profile",    label = "Trial profile",            desc = "Names, target, people, portfolio",
       icon = '<circle cx="12" cy="8" r="4"/><path d="M4 21a8 8 0 0 1 16 0"/>'),
  list(id = "look",       label = "Appearance & modules",     desc = "Trial colours, dashboard modules",
       icon = '<rect x="3" y="3" width="7" height="7" rx="1.5"/><rect x="14" y="3" width="7" height="7" rx="1.5"/><rect x="3" y="14" width="7" height="7" rx="1.5"/><rect x="14" y="14" width="7" height="7" rx="1.5"/>'),
  list(id = "data",       label = "Data & mapping",           desc = "Sources, fields, events, codes",
       icon = '<ellipse cx="12" cy="5" rx="8" ry="3"/><path d="M4 5v14c0 1.7 3.6 3 8 3s8-1.3 8-3V5"/><path d="M4 12c0 1.7 3.6 3 8 3s8-1.3 8-3"/>'),
  list(id = "visits",     label = "Visits & CRFs",            desc = "Forms, due dates, grace periods",
       icon = '<rect x="3" y="4" width="18" height="17" rx="2"/><path d="M3 9h18M8 2v4M16 2v4"/><path d="m9 15 2 2 4-4"/>'),
  list(id = "monitoring", label = "Recruitment & monitoring", desc = "Targets, hours, health thresholds",
       icon = '<path d="M3 3v18h18"/><path d="m7 15 4-4 3 3 5-6"/>'),
  list(id = "reports",    label = "Reports & admin",          desc = "Report content, templates, config",
       icon = '<path d="M14 3H6a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V9z"/><path d="M14 3v6h6M8 13h8M8 17h5"/>')
)

.st_icon <- function(path) HTML(sprintf(
  '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">%s</svg>', path))

.st_card <- function(title, sub, ..., save = NULL, extra = NULL) {
  tags$section(class = "s-card",
    div(class = "s-card-head",
        div(style = "flex:1;min-width:0;", div(class = "s-card-title", title), div(class = "s-card-sub", sub)),
        extra, save),
    div(class = "s-card-body", ...))
}
.st_save  <- function(id, label = "Save") actionButton(id, label, class = "btn-primary-sm")
.st_field <- function(label, input, hint = NULL) div(class = "s-field",
  span(class = "s-field-l", label), input, if (!is.null(hint)) div(class = "s-hint", hint))
.st_feat  <- function(id, title, desc, value = FALSE, pill = NULL) div(class = "feat-row",
  checkboxInput(id, label = NULL, value = value),
  div(style = "flex:1;min-width:0;",
      div(class = "feat-title", title, if (!is.null(pill)) span(class = "rec-pill", pill)),
      div(class = "feat-desc", desc)))
.st_sec   <- function(id, ..., first = FALSE) div(id = paste0("settings_sec_", id), class = "settings-section",
  style = if (!first) "display:none;", ...)
.st_sublabel <- function(txt) div(class = "s-sublabel", txt)

trial_settings_tab_ui <- function() {
  tabPanel("settings",

    # Section switching + "Find a setting" search across every card
    tags$script(HTML("
      function settingsShow(sec) {
        $('.settings-section').hide();
        $('#settings_sec_' + sec).show();
      }
      function settingsClearSearch() {
        $('.settings-section .s-card').show();
        settingsShow($('.settings-item.on').data('section') || 'profile');
        $('#settings_search_note').text('');
      }
      $(document).on('click', '.settings-item', function() {
        var sec = $(this).data('section');
        if (!sec) return;
        $('#settings_search').val('');
        $('.settings-item').removeClass('on').removeAttr('aria-current');
        $(this).addClass('on').attr('aria-current', 'page');
        settingsClearSearch();
        Shiny.setInputValue('settings_active_section', sec, {priority: 'event'});
      });
      $(document).on('input', '#settings_search', function() {
        var q = (this.value || '').trim().toLowerCase();
        if (!q) { settingsClearSearch(); return; }
        $('.settings-section').show();
        var n = 0;
        $('.settings-section .s-card').each(function() {
          var hit = this.textContent.toLowerCase().indexOf(q) >= 0;
          $(this).toggle(hit); if (hit) n++;
        });
        $('#settings_search_note').text(n ? n + (n === 1 ? ' matching setting' : ' matching settings')
                                          : 'No settings match \\u201C' + this.value.trim() + '\\u201D');
      });
    ")),

    div(class = "settings-shell",

      # ═══ Side nav ═══════════════════════════════════════════════════
      tags$aside(class = "settings-nav",
        div(class = "settings-trial",
            uiOutput("settings_trial_mark"),
            div(style = "min-width:0;",
                div(class = "settings-eyebrow", "Configuring"),
                uiOutput("settings_trial_name"))),
        div(class = "settings-search",
            tags$input(type = "search", id = "settings_search", placeholder = "Find a setting…",
                       `aria-label` = "Find a setting", autocomplete = "off")),
        tags$nav(class = "settings-list", `aria-label` = "Settings sections",
          lapply(seq_along(.ST_SECTIONS), function(i) {
            s <- .ST_SECTIONS[[i]]
            tags$button(type = "button", class = paste("settings-item", if (i == 1) "on"),
                        `data-section` = s$id, `aria-current` = if (i == 1) "page",
                        span(class = "settings-ic", .st_icon(s$icon)),
                        span(style = "flex:1;min-width:0;",
                             span(class = "settings-lbl", s$label),
                             span(class = "settings-desc", s$desc)))
          })),
        div(class = "settings-meta",
            uiOutput("settings_config_path"),
            uiOutput("settings_overrides_status"))
      ),

      # ═══ Main canvas ════════════════════════════════════════════════
      tags$main(class = "settings-main",
        div(class = "settings-header",
            div(class = "crumbs",
                span("Trial settings"),
                span(class = "sep", HTML("&rsaquo;")),
                span(class = "crumb-now", textOutput("settings_breadcrumb", inline = TRUE))),
            div(style = "flex:1;"),
            div(id = "settings_search_note", class = "settings-search-note", `aria-live` = "polite")),

        div(class = "settings-body",

          # ── 1. Trial profile ────────────────────────────────────────
          .st_sec("profile", first = TRUE,
            .st_card("Names & target", "The short name appears in navigation, page headers and document footers.",
              save = .st_save("settings_save_profile"),
              div(class = "form-grid",
                  .st_field("Short name", textInput("set_short_name", NULL, "")),
                  .st_field("Recruitment target", numericInput("set_target", NULL, 100, min = 1))),
              .st_field("Full trial name", textInput("set_full_name", NULL, "", width = "100%")),
              .st_field("Portfolio category",
                        selectInput("set_category", NULL, choices = TRIAL_CATEGORIES, selected = "Other", width = "100%")),
              div(class = "form-grid",
                  .st_field("Chief Investigator", textInput("set_ci", NULL, "")),
                  .st_field("Sponsor", textInput("set_sponsor", NULL, "")))),

            .st_card("Team & access", "Who can open this trial and what they can do. Admins always have full access; new people are added by an admin under People on the home screen.",
              uiOutput("st_team_ui"),
              .st_sublabel("Give someone access"),
              div(class = "team-add",
                  selectizeInput("st_team_add_user", NULL, choices = NULL, width = "100%",
                                 options = list(placeholder = "Choose a person…")),
                  selectInput("st_team_add_role", NULL, width = "100%",
                              c("Read-only" = "readonly", "Coordinator" = "coordinator",
                                "Statistician" = "statistician", "Manager" = "manager")),
                  actionButton("st_team_add", "Add", class = "btn-primary-sm"))),

            .st_card("Portfolio review", "Trial-level facts for the BCTU Trial Update Summary. Meeting dates and RAG status are entered on the Reports tab.",
              save = .st_save("pr_save"),
              div(class = "form-grid",
                  .st_field("Team Leader", textInput("pr_team_leader", NULL, "", width = "100%")),
                  .st_field("Chief Investigator (CI)", textInput("pr_ci", NULL, "", width = "100%", placeholder = "Falls back to Names & target")),
                  .st_field("Funder", textInput("pr_funder", NULL, "", width = "100%")),
                  .st_field("Trial type (CTIMP / non-CTIMP)", textInput("pr_trial_type", NULL, "", width = "100%")),
                  .st_field("Intervention", textInput("pr_intervention", NULL, "", width = "100%")),
                  .st_field("Sponsor", textInput("pr_sponsor", NULL, "", width = "100%", placeholder = "Falls back to Names & target")),
                  .st_field("Trial Coordinator", textInput("pr_coordinator", NULL, "", width = "100%")),
                  .st_field("Sample size", textInput("pr_sample_size", NULL, "", width = "100%", placeholder = "Falls back to the recruitment target"))),
              .st_sublabel("Fixed dates"),
              div(class = "form-grid",
                  .st_field("Grant start date", dateInput("pr_grant_start", NULL, value = NA, format = "dd M yyyy", width = "100%")),
                  .st_field("Grant end date", dateInput("pr_grant_end", NULL, value = NA, format = "dd M yyyy", width = "100%")),
                  .st_field("First patient recruited", dateInput("pr_first_patient_date", NULL, value = NA, format = "dd M yyyy", width = "100%")),
                  .st_field("All approvals in place", dateInput("pr_approvals_date", NULL, value = NA, format = "dd M yyyy", width = "100%")),
                  .st_field("Open to recruitment", dateInput("pr_open_recruitment_date", NULL, value = NA, format = "dd M yyyy", width = "100%")),
                  .st_field("Pilot phase ended", selectInput("pr_pilot_phase_ended", NULL, c("N/A", "No", "Yes"), "N/A", width = "100%"))),
              .st_sublabel("Stage & summary"),
              .st_field("Current stage", selectInput("pr_stage", NULL, c("In set-up", "Recruiting", "In Follow-up", "Analysis"), "Recruiting", width = "100%")),
              .st_field("Brief summary of the trial", textAreaInput("pr_brief_summary", NULL, "", width = "100%", height = "120px",
                                                                   placeholder = "A short paragraph describing the trial")),
              .st_sublabel("Key funder milestones"),
              .st_field("Approvals submitted / in place", textInput("pr_ms_approvals", NULL, "", width = "100%")),
              .st_field("Recruitment milestone", textInput("pr_ms_recruitment", NULL, "", width = "100%", placeholder = "e.g. at least 60 across 6 sites")),
              .st_field("Data capture target (%)", textInput("pr_ms_data_capture", NULL, "", width = "100%")),
              .st_field("Other milestone", textInput("pr_ms_other", NULL, "", width = "100%")),
              .st_sublabel("Database development"),
              div(class = "form-grid",
                  .st_field("Final CRF sign-off", textInput("pr_db_crf_signoff", NULL, "", width = "100%")),
                  .st_field("Functional requirement specification", textInput("pr_db_func_spec", NULL, "", width = "100%")),
                  .st_field("Database requirement specification", textInput("pr_db_req_spec", NULL, "", width = "100%")),
                  .st_field("Release for testing", textInput("pr_db_release_test", NULL, "", width = "100%")),
                  .st_field("Final release (expected / actual)", textInput("pr_db_final_release", NULL, "", width = "100%"))),
              .st_sublabel("Finance & staffing"),
              .st_field("Staffing awarded (FTE / duration)", textInput("pr_fin_staffing_awarded", NULL, "", width = "100%")),
              div(class = "form-grid",
                  .st_field("Default staffing status", textInput("pr_fin_staffing_status", NULL, "", width = "100%",
                                                                 placeholder = "Can be changed per report on the Reports tab")),
                  .st_field("Default financial status", selectInput("pr_fin_status", NULL, c("", "On track", "Underspent", "Overspent"), "", width = "100%"))))
          ),

          # ── 2. Appearance & modules ─────────────────────────────────
          .st_sec("look",
            .st_card("Trial colours", "Used only for this trial's charts and bold section headings. The rest of the dashboard always stays University of Birmingham black, white and gold.",
              save = .st_save("settings_save_colors", "Save colours"),
              uiOutput("theme_picker_ui"),
              shinyjs::hidden(div(id = "custom_colors_panel", style = "margin-top:14px;",
                div(class = "color-grid",
                    div(class = "s-color",
                        span(class = "s-field-l", "Primary", span(class = "s-field-h", "headings, main series")),
                        textInput("set_col_primary", NULL, value = "#1B1B1B"),
                        div(id = "preview_primary", class = "s-color-swatch", style = "background:#1B1B1B;")),
                    div(class = "s-color",
                        span(class = "s-field-l", "Secondary", span(class = "s-field-h", "second series")),
                        textInput("set_col_secondary", NULL, value = "#00788E"),
                        div(id = "preview_secondary", class = "s-color-swatch", style = "background:#00788E;")),
                    div(class = "s-color",
                        span(class = "s-field-l", "Accent", span(class = "s-field-h", "projections, highlights")),
                        textInput("set_col_accent", NULL, value = "#C59A00"),
                        div(id = "preview_accent", class = "s-color-swatch", style = "background:#C59A00;"))))),
              div(style = "margin-top:14px;", uiOutput("color_preview_bar"))),

            .st_card("Dashboard modules", "Choose which tabs and sections this trial uses. Changes apply straight away.",
              save = .st_save("settings_save_features"),
              div(class = "feat-list",
                  .st_feat("set_feat_projections", "Recruitment projections", "Forecast corridor on the Overview and in reports.", TRUE, "Recommended"),
                  .st_feat("set_feat_replay", "Trial replay", "Animated CONSORT, site race and pace on the Overview."),
                  .st_feat("set_feat_consort", "CONSORT flow", "Static participant-flow diagram, used when the replay is off."),
                  .st_feat("set_feat_baseline", "Baseline characteristics", "Table 1 from REDCap fields, for reports."),
                  .st_feat("set_feat_pilot", "Pilot progression criteria", "Stop–go thresholds for the internal pilot."),
                  .st_feat("set_feat_postal", "Postal tracking", "A tab to log questionnaires sent and received."),
                  .st_feat("set_feat_returns", "Return rates", "A tab for return rates by timepoint, site and form, from return-rate files or the REDCap export."),
                  .st_feat("set_feat_questionnaires", "Patient questionnaires (PROMs)",
                           "Show questionnaire completion on the Data tab. Turn off for trials without PROMs.", TRUE)))
          ),

          # ── 3. Data & mapping ───────────────────────────────────────
          .st_sec("data",
            .st_card("Data sources", "Paste K: drive or network paths. The dashboard reads the newest file in each folder.",
              save = .st_save("settings_save_paths", "Save paths"),
              # Hidden for trials with several work packages: they upload one
              # export per work package (card below). Toggled by the server.
              div(id = "settings_data_dir_wrap",
                  .st_field("REDCap export folder",
                            textInput("set_data_dir", NULL, "", width = "100%", placeholder = "K:/BCTU/Teams/MyTeam/MyTrial/Data"),
                            "Leave blank to use this trial's own data folder.")),
              shinyjs::hidden(div(id = "settings_data_dir_wp_note", class = "s-hint", style = "margin-bottom:12px;",
                HTML("This trial has several work packages, so there is no single REDCap folder. Upload one export per work package under <strong>Work packages</strong> below."))),
              .st_field("Return-rate CSV folder",
                        textInput("set_rr_dir", NULL, "", width = "100%", placeholder = "K:/BCTU/Teams/MyTeam/MyTrial/ReturnRates"),
                        "Leave blank to work the rates out from the REDCap export. Earlier files in the folder show as a trend."),
              .st_field("Trial logo file",
                        textInput("set_logo_path", NULL, "", width = "100%", placeholder = "K:/BCTU/Teams/MyTeam/MyTrial/logo.png"),
                        "A .png or .jpg shown in the top bar and on reports.")),

            # Shown only for trials with more than one work package (server toggles it)
            shinyjs::hidden(div(id = "settings_nav_workpackages",
              .st_card("Work packages", "Each work package has its own recruitment target, outcomes and REDCap export. Upload an export against a work package and its rows are tagged with it, so every work-package view and report reads only that work package's data.",
                save = .st_save("settings_save_wps"),
                uiOutput("settings_wp_ui")))),

            .st_card("Field mapping", "Which REDCap variable holds each piece of information. Pick from your latest export or type a variable name; anything left as “Not used” is simply skipped.",
              save = .st_save("settings_save_fields", "Save mapping"),
              uiOutput("settings_fields_ui")),

            .st_card("Follow-up schedule", "The REDCap events this trial collects. They drive the Data tab and the automatic CRF schedule.",
              save = .st_save("settings_save_schedule", "Save schedule"),
              div(class = "sch-baseline",
                  div(class = "sch-field",
                      tags$label("Baseline / randomisation event"),
                      textInput("set_ev_baseline", NULL, placeholder = "baseline_arm_1", width = "100%")),
                  div(class = "sch-hint", "The event participants are randomised at.")),
              div(class = "sch-colhead",
                  span(class = "sch-colhead-l", "Follow-up timepoint"),
                  span(class = "sch-colhead-r", "REDCap event name"),
                  span(style = "width:34px;")),
              uiOutput("settings_timepoints_ui"),
              div(style = "margin-top:12px;", actionButton("settings_tp_add", HTML("&#43; Add timepoint"), class = "btn-ghost-sm")),
              div(class = "sch-subforms",
                  div(class = "sch-field",
                      tags$label("Sub-form and safety events (comma-separated)"),
                      textInput("set_ev_subforms", NULL, placeholder = "sub_forms_arm_1, ad_hoc_arm_1", width = "100%")),
                  div(class = "sch-hint", "Events holding SAEs, deviations, withdrawals or change-of-status forms."))),

            .st_card("Change-of-status codes", "The codes in your change-of-status form and what each one means. A code whose name includes “part” keeps the participant in follow-up.",
              save = .st_save("settings_save_cos", "Save codes"),
              uiOutput("settings_cos_ui"),
              div(style = "margin-top:10px;", actionButton("settings_cos_add", HTML("&#43; Add code"), class = "btn-ghost-sm"))),

            .st_card("Import a codebook", "Load the meanings of coded values from your REDCap data dictionary (CSV), a PDF codebook, or code lists you paste. Names you have already typed are kept.",
              save = .st_save("settings_cb_import", "Import"),
              div(class = "form-grid",
                  .st_field("Codebook file",
                            fileInput("settings_cb_file", NULL, accept = c(".csv", ".tsv", ".pdf", ".txt"), width = "100%"),
                            "REDCap: Project Setup → Data Dictionary → Download. A PDF codebook works too."),
                  .st_field("Or paste code lists",
                            textAreaInput("settings_cb_text", NULL, rows = 5, width = "100%",
                                          placeholder = "index_panc_aetio: 1, Gallstones | 2, Alcohol\nbase_sex: 1, Male | 2, Female"))),
              checkboxInput("settings_cb_overwrite", "Overwrite names I have already typed", FALSE),
              uiOutput("settings_cb_status")),

            .st_card("Demographics & codebook", "Which demographic breakdowns appear on the Data tab, the names shown for each coded value, and what every other numeric code in the export means.",
              save = .st_save("settings_save_demographics"),
              uiOutput("settings_demographics_ui")),

            .st_card("Detail fields", "Extra columns from your export to show in the SAE, withdrawal and complications detail — on the Data tab and in reports.",
              save = .st_save("settings_save_detail", "Save fields"),
              uiOutput("settings_detail_ui"))
          ),

          # ── 4. Visits & CRFs ────────────────────────────────────────
          .st_sec("visits",
            .st_card("CRF schedule", "Every form each participant should have, when it falls due and how long sites have to enter it. Built automatically from your events and completion fields until you change it.",
              save = .st_save("settings_save_crf", "Save schedule"),
              extra = actionButton("settings_crf_reset", "Reset to automatic", class = "btn-ghost-sm"),
              uiOutput("settings_crf_status"),
              div(class = "crf-scroll",
                  div(class = "crf-colhead", span("Timepoint"), span("Form"), span("Completion field"),
                      span("REDCap event"), span("Due from"), span("+ days"), span("Grace"), span("Type"), span()),
                  uiOutput("settings_crf_ui")),
              div(style = "margin-top:10px;", actionButton("settings_crf_add", HTML("&#43; Add form"), class = "btn-ghost-sm")),
              div(class = "s-hint", style = "margin-top:10px;",
                  "A form falls due on its visit date (the chosen date plus the days after) and becomes overdue once the grace period has passed. Leave Grace blank to use the default below.")),

            .st_card("Due-date rules", "The default grace period, and when a missing operation or discharge date should be flagged.",
              save = .st_save("settings_save_crf_rules", "Save rules"),
              div(class = "form-grid g3",
                  .st_field("Default grace period (days)", numericInput("set_crf_grace", NULL, 14, min = 0, max = 365)),
                  .st_field("Flag a missing operation date after (days)", numericInput("set_op_expected", NULL, 7, min = 1)),
                  .st_field("Flag a missing discharge date after (days)", numericInput("set_dis_expected", NULL, 60, min = 1))),
              div(class = "s-hint",
                  "Until those dates are entered, forms that depend on them fall due on an estimated date, so nothing drops out of view."))
          ),

          # ── 5. Recruitment & monitoring ─────────────────────────────
          .st_sec("monitoring",
            .st_card("Recruitment target schedule", "The protocol's cumulative target by month, one month per line as “YYYY-MM, target”. You can paste two columns straight from Excel.",
              save = .st_save("settings_save_target", "Save schedule"),
              div(class = "form-grid",
                  textAreaInput("set_target_schedule", NULL, "", width = "100%", height = "220px",
                                placeholder = "2026-03, 0\n2026-04, 4\n2026-05, 12"),
                  uiOutput("settings_target_preview"))),

            .st_card("Monthly recruitment targets", "How many participants the trial team expects each month. Reports plot these behind the actual monthly figures and report progress against them. Leave empty for a trial with no agreed monthly profile: the report then shows recruitment as it happened, with no plan comparison.",
              save = .st_save("rct_save"),
              .st_field("Targets \u2014 one month per line",
                        textAreaInput("rct_targets", NULL, "", width = "100%", height = "220px",
                                      placeholder = "2026-01, 5\n2026-02, 8\n2026-03, 8"),
                        HTML("Month first, then the target: <code>2026-01, 5</code>. <code>Jan 2026 5</code> and <code>01/2026: 5</code> are read the same way. Months you leave out count as no target for that month.")),
              uiOutput("rct_summary"),
              .st_sublabel("Fill a flat schedule"),
              div(class = "s-hint", "For an evenly spread plan: this writes one line per month into the box above, ready to edit. It replaces what is there."),
              div(class = "form-grid g3",
                  .st_field("First month", textInput("rct_gen_start", NULL, "", width = "100%", placeholder = "2026-01")),
                  .st_field("Number of months", numericInput("rct_gen_months", NULL, 12, min = 1, max = 120, step = 1, width = "100%")),
                  .st_field("Target per month", numericInput("rct_gen_per", NULL, 5, min = 0, step = 1, width = "100%"))),
              actionButton("rct_generate", "Fill the box", class = "btn-ghost-sm")),

            .st_card("Internal pilot", "Stop–go targets for the internal pilot, shown in the trial replay and health views. Leave Participants blank if there's no pilot.",
              save = .st_save("settings_save_pilot"),
              div(class = "form-grid g3",
                  .st_field("Participants", numericInput("set_pilot_target", NULL, NA, min = 0)),
                  .st_field("Sites", numericInput("set_pilot_sites", NULL, NA, min = 0)),
                  .st_field("Months", numericInput("set_pilot_months", NULL, NA, min = 0)))),

            .st_card("Sites & projections", "Used for new sites and for the site-based recruitment projection.",
              save = .st_save("settings_save_siteproj"),
              div(class = "form-grid g3",
                  .st_field("Planned number of sites", numericInput("set_target_sites", NULL, 24, min = 1)),
                  .st_field("New sites opening per month", numericInput("set_sites_per_month", NULL, 2, min = 0, step = 0.5)),
                  .st_field("Default monthly target per site", numericInput("set_site_monthly", NULL, 2, min = 0, step = 0.5))),
              .st_field("Projection window (weeks)", numericInput("set_proj_window", NULL, 12, min = 4, max = 52),
                        "How many recent complete weeks the current-pace and trend projections are based on.")),

            .st_card("Working hours", "Defines in-hours and out-of-hours recruitment on the Randomisations tab.",
              save = .st_save("settings_save_hours"),
              div(class = "form-grid g3",
                  .st_field("Working day starts", textInput("set_wh_start", NULL, "08:00")),
                  .st_field("Working day ends", textInput("set_wh_end", NULL, "18:00")),
                  div()),
              .st_field("Working days", checkboxGroupInput("set_wh_days", NULL,
                        choices = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                        selected = c("Mon", "Tue", "Wed", "Thu", "Fri"), inline = TRUE)),
              checkboxInput("set_wh_bh", "Count England & Wales bank holidays as out of hours", TRUE),
              .st_field("Other non-working days",
                        textAreaInput("set_wh_extra", NULL, "", width = "100%", height = "70px",
                                      placeholder = "One date per line, e.g. 2026-11-30"),
                        "For example Scottish or Northern Irish bank holidays, or local closures.")),

            .st_card("Health thresholds", "When sites and the trial are flagged amber or red. Change-of-status and overdue-CRF limits are funnel-plot limits, so a small site isn't flagged just because of chance.",
              save = .st_save("settings_save_thresholds"),
              div(class = "form-grid",
                  .st_field("Warning limit", selectInput("set_z_warn", NULL,
                            c("95% (z = 1.96)" = "1.96", "90% (z = 1.64)" = "1.64", "99% (z = 2.58)" = "2.58"), "1.96")),
                  .st_field("Alarm limit", selectInput("set_z_alarm", NULL,
                            c("99.8% (z = 3.09)" = "3.09", "99% (z = 2.58)" = "2.58", "99.9% (z = 3.29)" = "3.29"), "3.09"))),
              div(class = "form-grid g3",
                  .st_field("Overdue CRFs amber at (%)", numericInput("set_over_amber", NULL, 10, min = 0, max = 100)),
                  .st_field("Overdue CRFs red at (%)", numericInput("set_over_red", NULL, 25, min = 0, max = 100)),
                  .st_field("Judge rates from (participants)", numericInput("set_min_n", NULL, 5, min = 1))),
              div(class = "form-grid g3",
                  .st_field("Quiet site amber after (days)", numericInput("set_quiet_amber", NULL, 30, min = 1)),
                  .st_field("Quiet site red after (days)", numericInput("set_quiet_red", NULL, 60, min = 1)),
                  .st_field("Expected attrition (%)", numericInput("set_exp_attr", NULL, 15, min = 0, max = 100))),
              .st_field("Change-of-status reasons not held against a site",
                        textInput("set_cos_exclude", NULL, "death", width = "100%"),
                        "Comma-separated words matched against the code names, for example: death, died."),
              .st_sublabel("Health score weights"),
              div(class = "form-grid g4",
                  sliderInput("set_w_recruit", "Recruitment", 0, 100, 30, step = 5, width = "100%"),
                  sliderInput("set_w_retain", "Retention", 0, 100, 30, step = 5, width = "100%"),
                  sliderInput("set_w_data", "Data returns", 0, 100, 30, step = 5, width = "100%"),
                  sliderInput("set_w_activity", "Site activity", 0, 100, 10, step = 5, width = "100%")),
              div(class = "s-hint", "Weights are relative — they don't need to add up to 100."))
          ),

          # ── 6. Reports & admin ──────────────────────────────────────
          .st_sec("reports",
            .st_card("Report content", "The parts of the report unique to this trial — cover title, registration line and the general information table. Used by both the TMG/iTMG and TSC reports.",
              save = .st_save("rc_save"),
              div(class = "form-grid",
                  .st_field("Trial short name on cover", textInput("rc_short_name", NULL, "", width = "100%", placeholder = "e.g. TONIC")),
                  .st_field("Trial title (cover and header)", textInput("rc_trial_title", NULL, "", width = "100%", placeholder = "Defaults to the short name"))),
              .st_field("One-line subtitle", textInput("rc_trial_subtitle", NULL, "", width = "100%",
                                                        placeholder = "e.g. A randomised trial comparing X with Y in patients with Z")),
              .st_field("Registration line (TSC cover)", textInput("rc_registration", NULL, "", width = "100%",
                                                                   placeholder = "e.g. ISRCTN 11056540 · IRAS 328678")),
              .st_field("General information (TMG/iTMG cover panel)",
                        textAreaInput("rc_general_info", NULL, "", width = "100%", height = "200px",
                                      placeholder = paste("CI: Mr Matthew Lee", "Programme: NIHR HTA Programme",
                                                          "Sponsor: University of Birmingham", "ISRCTN: ISRCTN11056540",
                                                          "Sample / Sites: 898 · 25 UK sites", sep = "\n")),
                        "One row per line as “Label: Value”. Every field is optional.")),

            .st_card("Report templates", "Edit this trial's TMG/iTMG, TSC and TSC Interim report templates. Saved templates live in the trial's reports folder.",
              tags$div(class = "rt-tabs",
                tags$button(id = "rt_pick_tonic", class = "rt-pick-btn on action-button", type = "button",
                            onclick = "Shiny.setInputValue('rt_pick','tonic',{priority:'event'}); $('.rt-pick-btn').removeClass('on'); $(this).addClass('on');",
                            "TMG / iTMG"),
                tags$button(id = "rt_pick_tsc", class = "rt-pick-btn action-button", type = "button",
                            onclick = "Shiny.setInputValue('rt_pick','tsc',{priority:'event'}); $('.rt-pick-btn').removeClass('on'); $(this).addClass('on');",
                            "TSC"),
                tags$button(id = "rt_pick_tsc_interim", class = "rt-pick-btn action-button", type = "button",
                            onclick = "Shiny.setInputValue('rt_pick','tsc_interim',{priority:'event'}); $('.rt-pick-btn').removeClass('on'); $(this).addClass('on');",
                            "TSC Interim")),
              div(style = "margin-top:14px;",
                  span(class = "s-field-l", "External template path (optional)"),
                  div(style = "display:flex;gap:8px;align-items:center;",
                      textInput("rt_override_path", NULL, "", width = "100%",
                                placeholder = "e.g. K:/BCTU/Teams/MyTeam/MyTrial/Reports/my_report.Rmd"),
                      actionButton("rt_override_save", "Use this path", class = "btn-primary-sm"),
                      actionButton("rt_override_clear", "Clear", class = "btn-ghost-sm")),
                  div(class = "s-hint", "When set, reports are built from this file instead of the trial's own copy below.")),
              div(style = "margin-top:12px;font-size:11.5px;color:var(--ov-muted);", uiOutput("rt_status_line", inline = TRUE)),
              div(style = "margin-top:8px;", textAreaInput("rt_content", NULL, "", width = "100%", height = "480px", resize = "vertical")),
              div(style = "display:flex;gap:8px;margin-top:12px;align-items:center;flex-wrap:wrap;",
                  actionButton("rt_save", HTML("&check; Save template"), class = "btn-primary-sm"),
                  actionButton("rt_reset", "Reset to default", class = "btn-ghost-sm"),
                  actionButton("rt_reseed_all", "Re-seed both templates", class = "btn-ghost-sm"),
                  span(style = "flex:1;"),
                  span(class = "s-hint", "Reset and re-seed copy the standard templates from the project.")),
              tags$style(HTML("
                .rt-tabs { display:flex; gap:4px; }
                .rt-pick-btn { padding:6px 14px; border-radius:6px; border:1px solid var(--ov-line); background:#fff;
                               color:var(--ov-muted); font-size:12px; font-weight:500; font-family:inherit; cursor:pointer; }
                .rt-pick-btn.on { background:var(--uob-black); color:#fff; border-color:var(--uob-black); font-weight:600; }
                #rt_content { font-family:'JetBrains Mono', Menlo, monospace !important; font-size:12px !important;
                              line-height:1.5 !important; white-space:pre !important; }
              "))),

            .st_card("Configuration", "Your changes are saved in this trial's overrides.json, on top of config.R. Export them to copy this set-up to another trial, or import another trial's settings here.",
              extra = tagList(downloadButton("settings_export", "Export settings", class = "btn-ghost-sm"),
                              actionButton("settings_reset_overrides", HTML("&#x21BA; Reset to defaults"), class = "btn-ghost-sm")),
              fileInput("settings_import", "Import settings from another trial", accept = ".json",
                        buttonLabel = "Choose file…", placeholder = "An exported settings file (.json)", width = "100%")),

            uiOutput("settings_danger_zone_ui")
          )
        )
      )
    )
  )
}
