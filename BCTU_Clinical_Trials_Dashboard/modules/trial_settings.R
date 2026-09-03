trial_settings_tab_ui <- function() {
  tabPanel("settings",

    # JS for settings section switching
    tags$script(HTML("
      $(document).on('click', '.settings-item', function(){
        var sec = $(this).data('section');
        if (!sec) return;
        $('.settings-item').removeClass('on');
        $(this).addClass('on');
        $('.settings-section').hide();
        $('#settings_sec_' + sec).show();
        Shiny.setInputValue('settings_active_section', sec, {priority:'event'});
      });
    ")),

    div(class = "settings-shell",

      # ═══ Side nav ═══════════════════════════════════════════════════
      tags$aside(class = "settings-nav",

        # Trial identity
        div(class = "settings-trial",
            uiOutput("settings_trial_mark"),
            div(style = "min-width:0;",
                div(style = "font-size:9.5px;font-weight:600;color:var(--ov-muted);
                             text-transform:uppercase;letter-spacing:.6px;", "Configuring"),
                uiOutput("settings_trial_name"))
        ),

        # Navigation
        tags$nav(class = "settings-list",
          tags$button(class = "settings-item on", `data-section` = "appearance",
                      span(class = "settings-ic", HTML("&#x25D0;")),
                      span(style = "flex:1;min-width:0;",
                           span(class = "settings-lbl", "Appearance"),
                           span(class = "settings-desc", "Theme & branding")),
                      span(class = "settings-caret", HTML("&rsaquo;"))),
          tags$button(class = "settings-item", `data-section` = "identity",
                      span(class = "settings-ic", HTML("&#x25C7;")),
                      span(style = "flex:1;min-width:0;",
                           span(class = "settings-lbl", "Trial identity"),
                           span(class = "settings-desc", "Names, target, data paths"))),
          tags$button(class = "settings-item", `data-section` = "features",
                      span(class = "settings-ic", HTML("&#x229E;")),
                      span(style = "flex:1;min-width:0;",
                           span(class = "settings-lbl", "Features"),
                           span(class = "settings-desc", "Tabs & dashboard modules"))),
          tags$button(class = "settings-item", `data-section` = "schedule",
                      span(class = "settings-ic", HTML("&#x1F4C5;")),
                      span(style = "flex:1;min-width:0;",
                           span(class = "settings-lbl", "Follow-up schedule"),
                           span(class = "settings-desc", "Timepoints & REDCap events"))),
          tags$button(class = "settings-item", `data-section` = "demographics",
                      span(class = "settings-ic", HTML("&#x1F465;")),
                      span(style = "flex:1;min-width:0;",
                           span(class = "settings-lbl", "Demographics & codebook"),
                           span(class = "settings-desc", "Breakdowns, group names & code meanings"))),
          tags$button(class = "settings-item", `data-section` = "detail",
                      span(class = "settings-ic", HTML("&#x1F50E;")),
                      span(style = "flex:1;min-width:0;",
                           span(class = "settings-lbl", "Detail fields"),
                           span(class = "settings-desc", "Map extra import columns"))),
          tags$button(class = "settings-item", `data-section` = "config",
                      span(class = "settings-ic", HTML("&#x2351;")),
                      span(style = "flex:1;min-width:0;",
                           span(class = "settings-lbl", "Config & overrides"),
                           span(class = "settings-desc", "JSON overlay, reset"))),
          tags$button(class = "settings-item", `data-section` = "report_content",
                      span(class = "settings-ic", HTML("&#x1F4D1;")),
                      span(style = "flex:1;min-width:0;",
                           span(class = "settings-lbl", "Report content"),
                           span(class = "settings-desc", "Trial-specific fields"))),
          tags$button(class = "settings-item", `data-section` = "portfolio_review",
                      span(class = "settings-ic", HTML("&#x1F5C2;")),
                      span(style = "flex:1;min-width:0;",
                           span(class = "settings-lbl", "Portfolio review"),
                           span(class = "settings-desc", "Fixed dates & trial summary"))),
          tags$button(class = "settings-item", `data-section` = "report_templates",
                      span(class = "settings-ic", HTML("&#x1F4DD;")),
                      span(style = "flex:1;min-width:0;",
                           span(class = "settings-lbl", "Report templates"),
                           span(class = "settings-desc", "Edit TMG/iTMG and TSC Rmd"))),
          tags$button(class = "settings-item", `data-section` = "danger",
                      span(class = "settings-ic", HTML("&#x26A0;")),
                      span(style = "flex:1;min-width:0;",
                           span(class = "settings-lbl", "Danger zone"),
                           span(class = "settings-desc", "Archive or delete trial")))
        ),

        # Config meta
        div(class = "settings-meta",
            uiOutput("settings_config_path"),
            uiOutput("settings_overrides_status")
        )
      ),

      # ═══ Main canvas ════════════════════════════════════════════════
      tags$main(class = "settings-main",

        # Header
        div(class = "settings-header",
            div(class = "crumbs",
                span("Trial settings"),
                span(class = "sep", HTML("&rsaquo;")),
                span(style = "color:var(--ov-ink);font-weight:600;",
                     textOutput("settings_breadcrumb", inline = TRUE))),
            div(style = "flex:1;"),
            actionButton("settings_save_colors", "Save changes",
                         class = "btn-primary",
                         style = "font-size:12px;")
        ),

        div(class = "settings-body",

          # ── Section: Appearance ─────────────────────────────────────
          div(id = "settings_sec_appearance", class = "settings-section",

            tags$section(class = "s-card",
              div(class = "s-card-head",
                  div(style = "flex:1;min-width:0;",
                      div(class = "s-card-title", "Theme preset"),
                      div(class = "s-card-sub",
                          "Curated colour combinations tuned for clinical dashboards."))),
              div(class = "s-card-body",
                  uiOutput("theme_picker_ui"))
            ),

            tags$section(class = "s-card",
              div(class = "s-card-head",
                  div(style = "flex:1;min-width:0;",
                      div(class = "s-card-title", "Custom colours"),
                      div(class = "s-card-sub",
                          "Override individual hues. Changes apply to charts, headers, and exported reports."))),
              div(class = "s-card-body",
                  shinyjs::hidden(div(id = "custom_colors_panel",
                    div(class = "color-grid",
                        div(class = "s-color",
                            div(class = "s-color-head",
                                span(class = "s-field-l", "Primary",
                                     span(class = "s-field-h", "Sidebar, headers"))),
                            div(class = "s-color-input",
                                textInput("set_col_primary", label = NULL, value = "#1B4F6B")),
                            div(id = "preview_primary",
                                class = "s-color-swatch",
                                style = "background:#1B4F6B;")),
                        div(class = "s-color",
                            div(class = "s-color-head",
                                span(class = "s-field-l", "Secondary",
                                     span(class = "s-field-h", "Charts, accents"))),
                            div(class = "s-color-input",
                                textInput("set_col_secondary", label = NULL, value = "#2EC4A5")),
                            div(id = "preview_secondary",
                                class = "s-color-swatch",
                                style = "background:#2EC4A5;")),
                        div(class = "s-color",
                            div(class = "s-color-head",
                                span(class = "s-field-l", "Accent",
                                     span(class = "s-field-h", "Highlights"))),
                            div(class = "s-color-input",
                                textInput("set_col_accent", label = NULL, value = "#F59E0B")),
                            div(id = "preview_accent",
                                class = "s-color-swatch",
                                style = "background:#F59E0B;"))
                    ),
                    div(class = "preview-strip",
                        div(class = "ps-label", "Live preview"),
                        uiOutput("color_preview_bar"))
                  ))
              )
            )
          ),

          # ── Section: Identity ──────────────────────────────────────
          div(id = "settings_sec_identity", class = "settings-section",
              style = "display:none;",

            tags$section(class = "s-card",
              div(class = "s-card-head",
                  div(style = "flex:1;min-width:0;",
                      div(class = "s-card-title", "Names & target"),
                      div(class = "s-card-sub",
                          "The short name appears in navigation and document footers."))),
              div(class = "s-card-body",
                  div(class = "form-grid",
                      div(class = "s-field",
                          span(class = "s-field-l", "Short name"),
                          textInput("set_short_name", label = NULL, value = "")),
                      div(class = "s-field",
                          span(class = "s-field-l", "Recruitment target"),
                          numericInput("set_target", label = NULL, value = 100, min = 1))
                  ),
                  div(class = "s-field",
                      span(class = "s-field-l", "Full trial name"),
                      textInput("set_full_name", label = NULL, value = "", width = "100%")),
                  div(class = "s-field",
                      span(class = "s-field-l", "Portfolio category"),
                      selectInput("set_category", label = NULL,
                                  choices = TRIAL_CATEGORIES, selected = "Other", width = "100%")),
                  div(class = "form-grid",
                      div(class = "s-field",
                          span(class = "s-field-l", "Chief Investigator"),
                          textInput("set_ci", label = NULL, value = "")),
                      div(class = "s-field",
                          span(class = "s-field-l", "Sponsor"),
                          textInput("set_sponsor", label = NULL, value = ""))
                  )
              )
            ),

            tags$section(class = "s-card",
              div(class = "s-card-head",
                  div(style = "flex:1;min-width:0;",
                      div(class = "s-card-title", "Data paths"),
                      div(class = "s-card-sub",
                          "Paste K: drive or network paths. The dashboard reads the newest file from each folder.")),
                  actionButton("settings_save_paths", "Save paths",
                               class = "btn-primary-sm")),
              div(class = "s-card-body",
                  div(class = "s-field",
                      span(class = "s-field-l", HTML("REDCap data CSV folder &#x1F4C1;")),
                      textInput("set_data_dir", label = NULL, value = "", width = "100%",
                                placeholder = "K:/BCTU/Teams/MyTeam/MyTrial/Data")),
                  div(style = "font-size:11px;color:#64748B;margin-top:-8px;margin-bottom:12px;",
                      "Folder containing your REDCap CSV exports. Leave blank to use the local trials/<code>/data/ folder."),

                  div(class = "s-field",
                      span(class = "s-field-l", HTML("Return rates CSV folder &#x1F4C1;")),
                      textInput("set_rr_dir", label = NULL, value = "", width = "100%",
                                placeholder = "K:/BCTU/Teams/MyTeam/MyTrial/ReturnRates")),
                  div(style = "font-size:11px;color:#64748B;margin-top:-8px;margin-bottom:12px;",
                      "Folder containing return-rate CSVs. Leave blank if not applicable."),

                  div(class = "s-field",
                      span(class = "s-field-l", HTML("Trial logo file &#x1F5BC;")),
                      textInput("set_logo_path", label = NULL, value = "", width = "100%",
                                placeholder = "K:/BCTU/Teams/MyTeam/MyTrial/logo.png")),
                  div(style = "font-size:11px;color:#64748B;margin-top:-8px;margin-bottom:12px;",
                      "Path to a .png or .jpg logo. Displayed in topbar and reports.")
              )
            )
          ),

          # ── Section: Features ──────────────────────────────────────
          div(id = "settings_sec_features", class = "settings-section",
              style = "display:none;",

            tags$section(class = "s-card",
              div(class = "s-card-head",
                  div(style = "flex:1;min-width:0;",
                      div(class = "s-card-title", "Dashboard features"),
                      div(class = "s-card-sub",
                          "Toggle which tabs and features are shown in the dashboard.")),
                  actionButton("settings_save_features", "Save",
                               class = "btn-primary-sm")),
              div(class = "s-card-body",
                  div(class = "feat-list",
                      div(class = "feat-row",
                          checkboxInput("set_feat_projections", label = NULL, value = TRUE),
                          div(style = "flex:1;min-width:0;",
                              div(style = "display:flex;align-items:center;gap:8px;",
                                  span(style = "font-size:13px;font-weight:600;color:var(--ov-ink);",
                                       "Recruitment projections"),
                                  span(class = "rec-pill", "Recommended")),
                              div(style = "font-size:11.5px;color:var(--ov-muted);margin-top:2px;",
                                  "Forecast curves on overview & reports"))),
                      div(class = "feat-row",
                          checkboxInput("set_feat_pilot", label = NULL, value = FALSE),
                          div(style = "flex:1;",
                              span(style = "font-size:13px;font-weight:600;color:var(--ov-ink);",
                                   "Pilot progression criteria"),
                              div(style = "font-size:11.5px;color:var(--ov-muted);margin-top:2px;",
                                  "Stop-go thresholds for internal pilot"))),
                      div(class = "feat-row",
                          checkboxInput("set_feat_postal", label = NULL, value = FALSE),
                          div(style = "flex:1;",
                              span(style = "font-size:13px;font-weight:600;color:var(--ov-ink);",
                                   "Postal tracking"),
                              div(style = "font-size:11.5px;color:var(--ov-muted);margin-top:2px;",
                                  "Questionnaire send/receipt log"))),
                      div(class = "feat-row",
                          checkboxInput("set_feat_returns", label = NULL, value = FALSE),
                          div(style = "flex:1;",
                              span(style = "font-size:13px;font-weight:600;color:var(--ov-ink);",
                                   "Return rates"),
                              div(style = "font-size:11.5px;color:var(--ov-muted);margin-top:2px;",
                                  "Per-site/per-instrument return %"))),
                      div(class = "feat-row",
                          checkboxInput("set_feat_consort", label = NULL, value = FALSE),
                          div(style = "flex:1;",
                              span(style = "font-size:13px;font-weight:600;color:var(--ov-ink);",
                                   "CONSORT flow"),
                              div(style = "font-size:11.5px;color:var(--ov-muted);margin-top:2px;",
                                  "Eligibility → randomisation → analysis diagram"))),
                      div(class = "feat-row",
                          checkboxInput("set_feat_baseline", label = NULL, value = FALSE),
                          div(style = "flex:1;",
                              span(style = "font-size:13px;font-weight:600;color:var(--ov-ink);",
                                   "Baseline characteristics"),
                              div(style = "font-size:11.5px;color:var(--ov-muted);margin-top:2px;",
                                  "Table 1 generator from REDCap fields"))),
                      div(class = "feat-row",
                          checkboxInput("set_feat_questionnaires", label = NULL, value = TRUE),
                          div(style = "flex:1;",
                              span(style = "font-size:13px;font-weight:600;color:var(--ov-ink);",
                                   "Patient-completed questionnaires (PROMs)"),
                              div(style = "font-size:11.5px;color:var(--ov-muted);margin-top:2px;",
                                  "Show the per-participant questionnaire grid on the Data tab. Turn off for trials without PROMs.")))
                  )
              )
            )
          ),

          # ── Section: Follow-up schedule ────────────────────────────
          div(id = "settings_sec_schedule", class = "settings-section",
              style = "display:none;",

            tags$section(class = "s-card",
              div(class = "s-card-head",
                  div(style = "flex:1;min-width:0;",
                      div(class = "s-card-title", "Follow-up schedule"),
                      div(class = "s-card-sub",
                          "The timepoints this trial collects. These drive the Data tab KPI donuts and event classification. Map each to the REDCap event name in your export's redcap_event_name column.")),
                  actionButton("settings_save_schedule", "Save schedule",
                               class = "btn-primary-sm")),
              div(class = "s-card-body",
                  div(class = "sch-baseline",
                      div(class = "sch-field",
                          tags$label("Baseline / randomisation event"),
                          textInput("set_ev_baseline", label = NULL,
                                    placeholder = "baseline_arm_1", width = "100%")),
                      div(class = "sch-hint",
                          "The event participants are randomised at — counted as 'randomised'.")),

                  div(class = "sch-colhead",
                      span(class = "sch-colhead-l", "Follow-up timepoint"),
                      span(class = "sch-colhead-r", "REDCap event name"),
                      span(style = "width:34px;")),
                  uiOutput("settings_timepoints_ui"),

                  div(style = "margin-top:12px;",
                      actionButton("settings_tp_add",
                                   HTML("&#43; Add timepoint"),
                                   class = "btn-ghost-sm")),

                  div(class = "sch-subforms",
                      div(class = "sch-field",
                          tags$label("Sub-form / safety events (comma-separated)"),
                          textInput("set_ev_subforms", label = NULL,
                                    placeholder = "sub_forms_arm_1, ad_hoc_arm_1",
                                    width = "100%")),
                      div(class = "sch-hint",
                          "Events holding SAEs, deviations, withdrawals or change-of-status forms."))
              )
            )
          ),

          # ── Section: Demographics ──────────────────────────────────
          div(id = "settings_sec_demographics", class = "settings-section",
              style = "display:none;",

            # Import a codebook rather than typing every code by hand.
            tags$section(class = "s-card",
              div(class = "s-card-head",
                  div(style = "flex:1;min-width:0;",
                      div(class = "s-card-title", "Import a codebook"),
                      div(class = "s-card-sub",
                          "Load the meanings of coded values from your REDCap data dictionary (CSV), a PDF codebook, or text you paste below. Existing names you have typed are kept.")),
                  actionButton("settings_cb_import", "Import",
                               class = "btn-primary-sm")),
              div(class = "s-card-body",
                  div(class = "nt-grid-2",
                      div(class = "s-field",
                          tags$label("Codebook file"),
                          fileInput("settings_cb_file", label = NULL,
                                    accept = c(".csv", ".tsv", ".pdf", ".txt"),
                                    width = "100%"),
                          div(class = "sch-hint",
                              "REDCap: Project Setup \u2192 Data Dictionary \u2192 Download. A PDF codebook works too.")),
                      div(class = "s-field",
                          tags$label("or paste code lists"),
                          textAreaInput("settings_cb_text", label = NULL, rows = 5,
                                        width = "100%",
                                        placeholder = "index_panc_aetio: 1, Gallstones | 2, Alcohol\nbase_sex: 1, Male | 2, Female"))),
                  checkboxInput("settings_cb_overwrite",
                                "Overwrite names I have already typed", value = FALSE),
                  uiOutput("settings_cb_status"))
            ),

            tags$section(class = "s-card",
              div(class = "s-card-head",
                  div(style = "flex:1;min-width:0;",
                      div(class = "s-card-title", "Demographic groupings & codebook"),
                      div(class = "s-card-sub",
                          "Choose which demographic breakdowns appear on the Data tab, rename the groups shown for each coded value, and record what every other numeric code in the export means.")),
                  actionButton("settings_save_demographics", "Save",
                               class = "btn-primary-sm")),
              div(class = "s-card-body",
                  uiOutput("settings_demographics_ui"))
            )
          ),

          # ── Section: Detail fields ─────────────────────────────────
          div(id = "settings_sec_detail", class = "settings-section",
              style = "display:none;",

            tags$section(class = "s-card",
              div(class = "s-card-head",
                  div(style = "flex:1;min-width:0;",
                      div(class = "s-card-title", "Detail fields"),
                      div(class = "s-card-sub",
                          "Pull extra columns from your REDCap export into the SAE, withdrawal and complications detail — shown on the Data tab and in reports. Pick a column from your export and give it a heading.")),
                  actionButton("settings_save_detail", "Save fields",
                               class = "btn-primary-sm")),
              div(class = "s-card-body",
                  uiOutput("settings_detail_ui"))
            )
          ),

          # ── Section: Config & overrides ────────────────────────────
          div(id = "settings_sec_config", class = "settings-section",
              style = "display:none;",

            tags$section(class = "s-card",
              div(class = "s-card-head",
                  div(style = "flex:1;min-width:0;",
                      div(class = "s-card-title", "Configuration"),
                      div(class = "s-card-sub",
                          "Settings live in two layers — base config.R (read-only) and overrides.json overlay.")),
                  actionButton("settings_reset_overrides",
                               HTML("&#x21BA; Reset to defaults"),
                               class = "btn-ghost-sm")),
              div(class = "s-card-body",
                  div(style = "font-size:12px;color:var(--ov-muted);line-height:1.6;",
                      "Edits made above are saved to overrides.json next to the config file.
                       Reset to defaults removes the overrides and reverts to the original config.")
              )
            )
          ),

          # ── Section: Report content (trial-specific text in reports) ──
          div(id = "settings_sec_report_content", class = "settings-section",
              style = "display:none;",

            tags$section(class = "s-card",
              div(class = "s-card-head",
                  div(style = "flex:1;min-width:0;",
                      div(class = "s-card-title", "Trial-specific report content"),
                      div(class = "s-card-sub",
                          "Fill in the bits of the report that are unique to this
                           trial (cover title, registration line, general info
                           table). The same values flow into both TMG/iTMG and
                           TSC reports.")),
                  actionButton("rc_save", "Save",
                               class = "btn-primary-sm")),
              div(class = "s-card-body",
                  div(class = "form-grid",
                      div(class = "s-field",
                          span(class = "s-field-l", "Trial short name on cover"),
                          textInput("rc_short_name", label = NULL, value = "", width = "100%",
                                    placeholder = "e.g. TONIC, PANORAMA")),
                      div(class = "s-field",
                          span(class = "s-field-l", "Trial title (large text on cover/header)"),
                          textInput("rc_trial_title", label = NULL, value = "", width = "100%",
                                    placeholder = "Defaults to short name"))
                  ),
                  div(class = "s-field",
                      span(class = "s-field-l", "One-line trial subtitle"),
                      textInput("rc_trial_subtitle", label = NULL, value = "", width = "100%",
                                placeholder = "e.g. A randomised trial comparing X vs Y in patients with Z")),
                  div(class = "s-field",
                      span(class = "s-field-l", "Registration line (TSC cover)"),
                      textInput("rc_registration", label = NULL, value = "", width = "100%",
                                placeholder = "e.g. ISRCTN 11056540 · IRAS 328678")),
                  div(class = "s-field",
                      span(class = "s-field-l", "General report information (TMG/iTMG cover panel)"),
                      textAreaInput("rc_general_info", label = NULL, value = "",
                                    width = "100%", height = "220px",
                                    placeholder = paste("CI: Mr Matthew Lee",
                                                        "Programme: NIHR HTA Programme",
                                                        "Sponsor: University of Birmingham",
                                                        "ISRCTN: ISRCTN11056540",
                                                        "Project dates: 01 Jun 2024 to 29 Feb 2028",
                                                        "Sample / Sites: 898 · 25 UK sites",
                                                        sep = "\n"))),
                  div(style = "font-size:11px;color:var(--ov-muted);margin-top:-8px;",
                      "One row per line. Use ", tags$code("Label: Value"),
                      " — anything before the first colon becomes the row label."),

                  div(style = "margin-top:18px;font-size:11px;color:var(--ov-muted);font-style:italic;",
                      "All fields are optional. Empty fields fall back to neutral defaults
                       in the report. Saved values persist in this trial's overrides.json.")
              )
            )
          ),

          # ── Section: Report templates ──────────────────────────────
          div(id = "settings_sec_report_templates", class = "settings-section",
              style = "display:none;",

            tags$section(class = "s-card",
              div(class = "s-card-head",
                  div(style = "flex:1;min-width:0;",
                      div(class = "s-card-title", "Report templates"),
                      div(class = "s-card-sub",
                          "Edit the trial's TMG/iTMG and TSC Rmd templates.
                           Saved templates live in your trial's reports/
                           folder and are used for every report generated
                           for this trial."))),
              div(class = "s-card-body",
                  tags$div(class = "rt-tabs",
                    tags$button(id = "rt_pick_tonic", class = "rt-pick-btn on action-button",
                                type = "button",
                                onclick = "Shiny.setInputValue('rt_pick','tonic',{priority:'event'});
                                           $('.rt-pick-btn').removeClass('on');
                                           $(this).addClass('on');",
                                "TMG / iTMG"),
                    tags$button(id = "rt_pick_tsc", class = "rt-pick-btn action-button",
                                type = "button",
                                onclick = "Shiny.setInputValue('rt_pick','tsc',{priority:'event'});
                                           $('.rt-pick-btn').removeClass('on');
                                           $(this).addClass('on');",
                                "TSC")),

                  # Override-path field: lets a user point the renderer at an
                  # Rmd they already maintain elsewhere (e.g. on a network drive).
                  # When set + the file exists, it wins over the in-trial copy.
                  div(style = "margin-top:14px;",
                      tags$label(class = "s-field-l",
                                 "External Rmd path (optional)"),
                      div(style = "display:flex;gap:8px;",
                          textInput("rt_override_path", label = NULL, value = "",
                                    width = "100%",
                                    placeholder = "e.g. K:/BCTU/Teams/MyTeam/MyTrial/Reports/my_report.Rmd"),
                          actionButton("rt_override_save", "Use this path",
                                       class = "btn-primary-sm"),
                          actionButton("rt_override_clear", "Clear",
                                       class = "btn-ghost-sm"))),
                  div(style = "font-size:11px;color:var(--ov-muted);margin-top:4px;line-height:1.5;",
                      "When set, the dashboard renders from this file instead
                       of the trial's own copy — useful if you already maintain
                       an Rmd elsewhere and want to keep it as the single source
                       of truth. Leave empty to use the trial's own copy below."),

                  div(style = "margin-top:14px;font-size:11.5px;color:var(--ov-muted);",
                      uiOutput("rt_status_line", inline = TRUE)),

                  div(style = "margin-top:8px;",
                      textAreaInput("rt_content", label = NULL, value = "",
                                    width = "100%", height = "520px",
                                    resize = "vertical")),
                  tags$style(HTML("
                    #rt_content {
                      font-family: 'JetBrains Mono', Menlo, monospace !important;
                      font-size: 12px !important; line-height: 1.5 !important;
                      white-space: pre !important;
                    }
                  ")),

                  div(style = "display:flex;gap:8px;margin-top:14px;align-items:center;flex-wrap:wrap;",
                      actionButton("rt_save", HTML("&check; Save template"),
                                   class = "btn-primary-sm"),
                      actionButton("rt_reset", "Reset to default",
                                   class = "btn-ghost-sm"),
                      actionButton("rt_reseed_all", "Re-seed both templates",
                                   class = "btn-ghost-sm"),
                      span(style = "flex:1;"),
                      span(style = "font-size:11px;color:var(--ov-muted);font-style:italic;",
                           "Reset / re-seed re-copy the canonical templates from the project root."))
              )
            ),

            tags$style(HTML("
              .rt-tabs { display:flex; gap:4px; }
              .rt-pick-btn {
                padding:6px 14px; border-radius:6px;
                border:1px solid var(--ov-line); background:#fff;
                color:var(--ov-muted); font-size:12px; font-weight:500;
                font-family:inherit; cursor:pointer;
              }
              .rt-pick-btn.on {
                background:var(--ov-navy); color:#fff; border-color:var(--ov-navy);
                font-weight:600;
              }
            "))
          ),

          # ── Section: Portfolio review (fixed dates & summary) ───────
          div(id = "settings_sec_portfolio_review", class = "settings-section",
              style = "display:none;",

            tags$section(class = "s-card",
              div(class = "s-card-head",
                  div(style = "flex:1;min-width:0;",
                      div(class = "s-card-title", "Portfolio review — fixed fields"),
                      div(class = "s-card-sub",
                          "Trial-level metadata for the BCTU Trial Update
                           Summary template. These values are stable across
                           reports — meeting dates (TSC / DMC / TMG) and
                           RAG status are entered on the Reports tab.")),
                  actionButton("pr_save", "Save",
                               class = "btn-primary-sm")),
              div(class = "s-card-body",
                  div(class = "form-grid",
                      div(class = "s-field",
                          span(class = "s-field-l", "Team Leader"),
                          textInput("pr_team_leader", label = NULL, value = "",
                                    width = "100%")),
                      div(class = "s-field",
                          span(class = "s-field-l", "Chief Investigator (CI)"),
                          textInput("pr_ci", label = NULL, value = "",
                                    width = "100%",
                                    placeholder = "Falls back to Trial identity → CI")),
                      div(class = "s-field",
                          span(class = "s-field-l", "Funder"),
                          textInput("pr_funder", label = NULL, value = "",
                                    width = "100%")),
                      div(class = "s-field",
                          span(class = "s-field-l", "Trial type (CTIMP / non-CTIMP)"),
                          textInput("pr_trial_type", label = NULL, value = "",
                                    width = "100%")),
                      div(class = "s-field",
                          span(class = "s-field-l", "Intervention"),
                          textInput("pr_intervention", label = NULL, value = "",
                                    width = "100%")),
                      div(class = "s-field",
                          span(class = "s-field-l", "Sponsor"),
                          textInput("pr_sponsor", label = NULL, value = "",
                                    width = "100%",
                                    placeholder = "Falls back to Trial identity → Sponsor")),
                      div(class = "s-field",
                          span(class = "s-field-l", "Trial Coordinator"),
                          textInput("pr_coordinator", label = NULL, value = "",
                                    width = "100%")),
                      div(class = "s-field",
                          span(class = "s-field-l", "Sample size"),
                          textInput("pr_sample_size", label = NULL, value = "",
                                    width = "100%",
                                    placeholder = "Falls back to trial target"))
                  ),

                  div(style = "margin-top:14px;font-size:11px;font-weight:600;
                               color:var(--ov-muted);text-transform:uppercase;
                               letter-spacing:.6px;", "Fixed dates"),
                  div(class = "form-grid",
                      div(class = "s-field",
                          span(class = "s-field-l", "Grant start date"),
                          dateInput("pr_grant_start", label = NULL,
                                    value = NA, format = "dd M yyyy", width = "100%")),
                      div(class = "s-field",
                          span(class = "s-field-l", "Grant end date"),
                          dateInput("pr_grant_end", label = NULL,
                                    value = NA, format = "dd M yyyy", width = "100%")),
                      div(class = "s-field",
                          span(class = "s-field-l", "Date first patient recruited"),
                          dateInput("pr_first_patient_date", label = NULL,
                                    value = NA, format = "dd M yyyy", width = "100%")),
                      div(class = "s-field",
                          span(class = "s-field-l", "All approvals in place (date)"),
                          dateInput("pr_approvals_date", label = NULL,
                                    value = NA, format = "dd M yyyy", width = "100%")),
                      div(class = "s-field",
                          span(class = "s-field-l", "Open to recruitment (date)"),
                          dateInput("pr_open_recruitment_date", label = NULL,
                                    value = NA, format = "dd M yyyy", width = "100%")),
                      div(class = "s-field",
                          span(class = "s-field-l", "Pilot phase ended"),
                          selectInput("pr_pilot_phase_ended", label = NULL,
                                      choices = c("N/A", "No", "Yes"),
                                      selected = "N/A", width = "100%"))
                  ),

                  div(style = "margin-top:14px;font-size:11px;font-weight:600;
                               color:var(--ov-muted);text-transform:uppercase;
                               letter-spacing:.6px;", "Stage & summary"),
                  div(class = "s-field",
                      span(class = "s-field-l", "Current stage"),
                      selectInput("pr_stage", label = NULL,
                                  choices = c("In set-up", "Recruiting",
                                              "In Follow-up", "Analysis"),
                                  selected = "Recruiting", width = "100%")),
                  div(class = "s-field",
                      span(class = "s-field-l", "Brief summary of trial"),
                      textAreaInput("pr_brief_summary", label = NULL, value = "",
                                    width = "100%", height = "140px",
                                    placeholder = "Short paragraph describing the trial")),

                  div(style = "margin-top:14px;font-size:11px;font-weight:600;
                               color:var(--ov-muted);text-transform:uppercase;
                               letter-spacing:.6px;", "Key (Funder) Milestones — defaults"),
                  div(class = "s-field",
                      span(class = "s-field-l", "Trial approvals submitted / in place"),
                      textInput("pr_ms_approvals", label = NULL, value = "", width = "100%")),
                  div(class = "s-field",
                      span(class = "s-field-l", "Recruitment milestone (e.g. min X across X sites)"),
                      textInput("pr_ms_recruitment", label = NULL, value = "", width = "100%")),
                  div(class = "s-field",
                      span(class = "s-field-l", "% Data capture target"),
                      textInput("pr_ms_data_capture", label = NULL, value = "", width = "100%")),
                  div(class = "s-field",
                      span(class = "s-field-l", "Other milestone"),
                      textInput("pr_ms_other", label = NULL, value = "", width = "100%")),

                  div(style = "margin-top:14px;font-size:11px;font-weight:600;
                               color:var(--ov-muted);text-transform:uppercase;
                               letter-spacing:.6px;", "Database development dates"),
                  div(class = "form-grid",
                      div(class = "s-field",
                          span(class = "s-field-l", "Final CRF sign-off"),
                          textInput("pr_db_crf_signoff", label = NULL, value = "", width = "100%")),
                      div(class = "s-field",
                          span(class = "s-field-l", "Functional Requirement Specification"),
                          textInput("pr_db_func_spec", label = NULL, value = "", width = "100%")),
                      div(class = "s-field",
                          span(class = "s-field-l", "Database Requirement Specification"),
                          textInput("pr_db_req_spec", label = NULL, value = "", width = "100%")),
                      div(class = "s-field",
                          span(class = "s-field-l", "Release for testing"),
                          textInput("pr_db_release_test", label = NULL, value = "", width = "100%")),
                      div(class = "s-field",
                          span(class = "s-field-l", "Final Release Date (expected/actual)"),
                          textInput("pr_db_final_release", label = NULL, value = "", width = "100%"))
                  ),

                  div(style = "margin-top:14px;font-size:11px;font-weight:600;
                               color:var(--ov-muted);text-transform:uppercase;
                               letter-spacing:.6px;", "Finance & staffing — defaults"),
                  div(class = "s-field",
                      span(class = "s-field-l", "Staffing awarded (FTE / duration)"),
                      textInput("pr_fin_staffing_awarded", label = NULL, value = "", width = "100%")),
                  div(class = "s-field",
                      span(class = "s-field-l", "Default staffing status"),
                      textInput("pr_fin_staffing_status", label = NULL, value = "", width = "100%",
                                placeholder = "Override per-report on the Reports tab")),
                  div(class = "s-field",
                      span(class = "s-field-l", "Default financial status"),
                      selectInput("pr_fin_status", label = NULL,
                                  choices = c("", "On track", "Underspent", "Overspent"),
                                  selected = "", width = "100%")),

                  div(style = "margin-top:18px;font-size:11px;color:var(--ov-muted);font-style:italic;",
                      "These values are saved into the trial's overrides.json
                       under ", tags$code("portfolio_review"),
                      " and feed the Portfolio review report template.")
              )
            )
          ),

          # ── Section: Danger zone ───────────────────────────────────
          div(id = "settings_sec_danger", class = "settings-section",
              style = "display:none;",

            uiOutput("settings_danger_zone_ui")
          )
        )
      )
    )
  )
}
