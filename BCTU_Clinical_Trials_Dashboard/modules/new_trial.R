# =============================================================================
# New-trial setup — full-page, step-by-step wizard
# =============================================================================
# Replaces the old modal (new_trial_wizard_ui). This is a dedicated page that
# the home selector swaps in when the user clicks "New trial". It keeps every
# wiz_* input ID the create/validation logic in trial_selector_server.R already
# expects, so the server pipeline is reused unchanged — only the shell and the
# stepper are new.
#
# Visibility is toggled like the other top-level panels (trial_selector_panel /
# dashboard_panel): hidden by default, shown via show_step()/open_new_trial()
# in the server.
# =============================================================================

# Step metadata (kept in sync with WIZ_TOTAL / the wiz_step_N divs below and
# the show_step() helper in trial_selector_server.R).
NT_STEPS <- list(
  list(n = 1, title = "Trial basics",   blurb = "Name, target and design"),
  list(n = 2, title = "Data & branding", blurb = "Folders, logo and colours"),
  list(n = 3, title = "REDCap events",  blurb = "Map your event names"),
  list(n = 4, title = "Field mapping",  blurb = "Map your variable names"),
  list(n = 5, title = "Features",       blurb = "Tabs and sections to show"),
  list(n = 6, title = "Review & create", blurb = "Check it over and go")
)

new_trial_setup_ui <- function() {

  # Section header inside a step's content card.
  step_head <- function(num, title, subtitle) {
    div(class = "nt-step-head",
        div(class = "nt-step-head-num", num),
        div(tags$h2(title),
            div(class = "nt-step-head-sub", subtitle)))
  }
  hint <- function(txt) div(class = "nt-hint", txt)
  group_label <- function(txt) div(class = "nt-group-label", txt)

  shinyjs::hidden(div(id = "new_trial_panel", class = "nt-root",

    tooltip_styles(),

    # ── Top bar ──────────────────────────────────────────────────────────
    div(class = "nt-topbar",
        div(class = "nt-topbar-left",
            tags$img(src = "BlackText-landscape.png", alt = "BCTU",
                     class = "nt-topbar-logo"),
            div(class = "nt-topbar-divider"),
            div(
              div(class = "nt-topbar-title", "Set up a new trial"),
              div(class = "nt-topbar-sub", "A guided, step-by-step setup"))),
        tags$button(class = "nt-topbar-exit", type = "button",
                    onclick = "Shiny.setInputValue('wiz_cancel', Math.random(), {priority:'event'})",
                    HTML("&times; Cancel"))
    ),

    # ── Two-column shell: stepper rail + content ─────────────────────────
    div(class = "nt-shell",

        tags$aside(class = "nt-rail",
            uiOutput("wiz_step_indicator"),
            div(class = "nt-rail-foot",
                div(class = "nt-rail-foot-ic", HTML("&#128161;")),
                div(div(class = "nt-rail-foot-t", "Everything is editable later"),
                    div(class = "nt-rail-foot-d",
                        "Defaults are sensible — you can fine-tune anything from Trial Settings, or when you upload your first REDCap export.")))
        ),

        tags$section(class = "nt-main",

            # All inputs live inside this wrapper so shinyjs::reset() can clear
            # the form when the wizard is reopened.
            div(id = "new_trial_form",

              # ── Step 1 — Trial basics ─────────────────────────────────
              div(class = "nt-step", id = "wiz_step_1",
                  step_head("1", "Trial basics",
                            "The essentials. Everything else has sensible defaults."),
                  div(class = "nt-card",
                      div(class = "nt-grid-2",
                          textInput("wiz_short_name", "Short name (e.g. TONIC, LOCI)",
                                    placeholder = "MYTRIAL"),
                          numericInput("wiz_target", "Recruitment target", value = 100, min = 1)),
                      textInput("wiz_full_name", "Full trial name",
                                placeholder = "e.g. A randomised trial comparing X vs Y in patients with Z",
                                width = "100%"),
                      div(class = "nt-grid-2",
                          textInput("wiz_ci", "Chief Investigator", placeholder = "e.g. Prof Jane Smith"),
                          textInput("wiz_sponsor", "Sponsor", placeholder = "e.g. University of Birmingham")),
                      div(class = "nt-grid-2",
                          selectInput("wiz_category", "Portfolio category",
                                      choices = TRIAL_CATEGORIES, selected = "Surgery",
                                      width = "100%"),
                          selectInput("wiz_trial_type", "Trial type",
                                      choices = c(
                                        "Randomised (open label)"   = "randomised",
                                        "Randomised (single blind)" = "single_blind",
                                        "Randomised (double blind)" = "double_blind",
                                        "Observational"             = "observational",
                                        "Single-arm / cohort"       = "single_arm",
                                        "Platform / umbrella"       = "platform",
                                        "Other"                     = "other"),
                                      selected = "randomised", width = "100%"))
                  ),
                  # Multi-work-package
                  div(class = "nt-card nt-card-soft",
                      checkboxInput("wiz_is_multi_wp",
                                    "This is a platform trial or has multiple work packages",
                                    value = FALSE),
                      hint("Tick if the trial has multiple sub-projects (e.g. Panorama). The dashboard shows per-WP tabs and a roll-up summary."),
                      shinyjs::hidden(div(id = "wiz_wp_panel", class = "nt-wp-panel",
                          div(class = "nt-wp-count",
                              tags$label("How many work packages?"),
                              numericInput("wiz_n_wps", label = NULL,
                                           value = 2, min = 1, max = 10, step = 1,
                                           width = "100px")),
                          uiOutput("wiz_wp_fields_ui")))
                  )
              ),

              # ── Step 2 — Data & branding ──────────────────────────────
              shinyjs::hidden(div(class = "nt-step", id = "wiz_step_2",
                  step_head("2", "Data & branding",
                            "Where your data lives, and how the trial looks."),
                  div(class = "nt-card",
                      group_label("REDCap exports"),
                      hint("Paste the K: drive (or network) folder paths. Use forward slashes. Leave blank to use the local app folder. The app loads the newest CSV in each folder."),

                      # Single-export field (default). Hidden when a separate
                      # export per work package is chosen.
                      div(id = "wiz_single_export_wrap", class = "nt-field",
                          tags$label(help_label(HTML("REDCap data CSV folder &#x1F4C1;"),
                              "The folder your REDCap CSV exports are saved to. The dashboard always reads the most recently modified .csv in it.")),
                          textInput("wiz_data_path", label = NULL,
                                    placeholder = "K:/BCTU/Teams/MyTeam/MyTrial/Data",
                                    width = "100%")),

                      # Multi-export option — only meaningful for multi-WP trials.
                      shinyjs::hidden(div(id = "wiz_multi_export_wrap", class = "nt-subcard",
                          checkboxInput("wiz_multi_export",
                                        "This trial has a separate REDCap export per work package",
                                        value = FALSE),
                          hint("Platform / multi-WP trials are often exported one work package at a time. Tick to give each WP its own folder — the app loads, tags and combines them automatically."),
                          shinyjs::hidden(div(id = "wiz_wp_export_panel",
                              uiOutput("wiz_wp_export_fields_ui"))))),

                      div(class = "nt-field", style = "margin-top:14px;",
                          tags$label(help_label(HTML("Return rates CSV folder &#x1F4C1;"),
                              "Optional. Folder of questionnaire return-rate CSVs, if your trial tracks postal questionnaire returns.")),
                          textInput("wiz_rr_path", label = NULL,
                                    placeholder = "K:/BCTU/Teams/MyTeam/MyTrial/ReturnRates",
                                    width = "100%"),
                          hint("Leave blank if not applicable."))
                  ),
                  div(class = "nt-card",
                      group_label("Branding"),
                      div(class = "nt-field",
                          tags$label(HTML("Trial logo file &#x1F5BC;")),
                          textInput("wiz_logo_path", label = NULL,
                                    placeholder = "K:/BCTU/Teams/MyTeam/MyTrial/logo.png",
                                    width = "100%"),
                          hint("Path to a .png or .jpg — shown in the topbar and reports.")),
                      div(class = "nt-grid-3 nt-colors",
                          .nt_color("wiz_col_primary",   "Primary",   "#1B4F6B"),
                          .nt_color("wiz_col_secondary", "Secondary", "#2EC4A5"),
                          .nt_color("wiz_col_accent",    "Accent",    "#F59E0B")))
              )),

              # ── Step 3 — Follow-up schedule (events) ──────────────────
              shinyjs::hidden(div(class = "nt-step", id = "wiz_step_3",
                  step_head("3", "Follow-up schedule",
                            "Which REDCap events make up your trial's timeline?"),
                  div(class = "nt-callout",
                      HTML("For each timepoint, enter its <strong>REDCap event name</strong> — the value in the <code>redcap_event_name</code> column of your export (e.g. <code>baseline_arm_1</code>). Only Baseline is required. Not sure of the names? Leave them as-is and let the app auto-detect them on your first upload.")),

                  div(class = "nt-card",
                      div(class = "nt-field",
                          tags$label(help_label("Baseline / randomisation event *",
                              "The event participants are randomised or enrolled at — this is what the dashboard counts as 'randomised'. Usually baseline_arm_1.")),
                          textInput("wiz_ev_baseline", label = NULL,
                                    value = "baseline_arm_1", width = "100%"))),

                  div(class = "nt-card",
                      group_label("Follow-up timepoints"),
                      div(class = "nt-field",
                          tags$label(help_label("Which follow-ups does this trial collect?",
                              "Pick the common ones or type your own (e.g. '6 months', 'End of treatment') and press enter. Surgical trials often have Discharge + Day 30/90; others may have 3/6/12-month visits. Each choice gets a box below for its REDCap event name.")),
                          selectizeInput("wiz_timepoints", label = NULL,
                              choices  = c("Discharge", "Day 30", "Day 90", "Week 6",
                                           "Month 3", "Month 6", "Month 12", "Month 24"),
                              selected = c("Discharge", "Day 30", "Day 90"),
                              multiple = TRUE, width = "100%",
                              options = list(create = TRUE,
                                             placeholder = "Add timepoints…",
                                             plugins = list("remove_button")))),
                      uiOutput("wiz_tp_fields_ui")),

                  div(class = "nt-card",
                      div(class = "nt-field",
                          tags$label(help_label("Sub-form / safety events (optional)",
                              "Events that hold SAEs, protocol deviations, withdrawals or change-of-status forms. Comma-separated, e.g. sub_forms_arm_1, ad_hoc_arm_1.")),
                          textInput("wiz_ev_subforms", label = NULL,
                                    value = "sub_forms_arm_1, ad_hoc_arm_1", width = "100%")))
              )),

              # ── Step 4 — Field mapping ────────────────────────────────
              shinyjs::hidden(div(class = "nt-step", id = "wiz_step_4",
                  step_head("4", "Field mapping",
                            "Map the REDCap variable names your export uses."),
                  div(class = "nt-callout",
                      HTML("Enter <strong>variable names</strong> (the column headers in your export), not their labels. The optional groups below adapt to what your trial captures — switch off anything that doesn't apply, so you're not asked for fields you don't collect.")),

                  div(class = "nt-card",
                      group_label("Required"),
                      div(class = "nt-grid-3",
                          div(class = "nt-field",
                              tags$label(help_label("Record ID",
                                  "The unique participant identifier column. Almost always record_id.")),
                              textInput("wiz_fld_record_id", label = NULL, value = "record_id", width = "100%")),
                          div(class = "nt-field",
                              tags$label(help_label("Site name",
                                  "Column holding the site / centre name — often the REDCap Data Access Group, or a dedicated site field. Drives all the sites views.")),
                              textInput("wiz_fld_site", label = NULL, value = "site_name", width = "100%")),
                          div(class = "nt-field",
                              tags$label(help_label("Randomisation datetime",
                                  "Column with the date (or date-time) the participant was randomised / enrolled. Drives recruitment-over-time and projections.")),
                              textInput("wiz_fld_rand_dt", label = NULL, value = "rand_dttm_s", width = "100%")))),

                  # Demographics — relevant to most trials.
                  div(class = "nt-card",
                      checkboxInput("wiz_cap_demographics",
                                    "This trial captures patient demographics", value = TRUE),
                      hint("Age, sex and ethnicity — powers the demographic breakdown cards on the Data tab."),
                      div(id = "wiz_demographics_fields", class = "nt-grid-3",
                          div(class = "nt-field",
                              tags$label(help_label("Age", "Variable holding age at baseline, e.g. cae_age or dem_age.")),
                              textInput("wiz_fld_age", label = NULL, placeholder = "dem_age", width = "100%")),
                          div(class = "nt-field",
                              tags$label(help_label("Sex", "Variable holding sex, e.g. base_sex or dem_sex.")),
                              textInput("wiz_fld_sex", label = NULL, placeholder = "dem_sex", width = "100%")),
                          div(class = "nt-field",
                              tags$label(help_label("Ethnicity", "Variable holding ethnicity, e.g. base_ethnic_gp or dem_ethnicity.")),
                              textInput("wiz_fld_ethnicity", label = NULL, placeholder = "dem_ethnicity", width = "100%")))),

                  # Procedure — surgical / interventional trials only.
                  div(class = "nt-card",
                      checkboxInput("wiz_cap_procedure",
                                    "This trial involves an operation / procedure", value = TRUE),
                      hint("For surgical or procedural trials — adds operation and discharge date handling. Leave off for observational, registry or drug trials with no procedure."),
                      div(id = "wiz_procedure_fields", class = "nt-grid-2",
                          div(class = "nt-field",
                              tags$label(help_label("Operation / procedure date",
                                  "Variable with the date of the operation or main procedure, e.g. iop_op_end_dt or surgery_date.")),
                              textInput("wiz_fld_op_date", label = NULL, placeholder = "surgery_date", width = "100%")),
                          div(class = "nt-field",
                              tags$label(help_label("Discharge date",
                                  "Variable with the hospital discharge date, e.g. dis_discharge_day or discharge_dt.")),
                              textInput("wiz_fld_discharge_date", label = NULL, placeholder = "discharge_dt", width = "100%")))),

                  # Withdrawals — general.
                  div(class = "nt-card",
                      group_label("Withdrawals & change of status"),
                      div(class = "nt-field", style = "max-width:340px;",
                          tags$label(help_label("Change-of-status field",
                              "Variable that flags withdrawals, deaths or loss to follow-up (often cos_type). Powers the withdrawals donut and safety tiles.")),
                          textInput("wiz_fld_cos_type", label = NULL, placeholder = "cos_type", width = "100%")))
              )),

              # ── Step 5 — Features ─────────────────────────────────────
              shinyjs::hidden(div(class = "nt-step", id = "wiz_step_5",
                  step_head("5", "Features",
                            "Choose which tabs and sections to show. Change these any time in Trial Settings."),
                  div(class = "nt-card",
                      div(class = "nt-grid-2 nt-features",
                          checkboxInput("wiz_feat_projections",    "Recruitment projections",                  value = TRUE),
                          checkboxInput("wiz_feat_questionnaires", "Patient-completed questionnaires (PROMs)",  value = TRUE),
                          checkboxInput("wiz_feat_postal",         "Postal tracking tab",                       value = FALSE),
                          checkboxInput("wiz_feat_returns",        "Return rates tab",                          value = FALSE),
                          checkboxInput("wiz_feat_pilot",          "Pilot progression criteria",                value = FALSE),
                          checkboxInput("wiz_feat_consort",        "CONSORT flow diagram",                      value = FALSE),
                          checkboxInput("wiz_feat_baseline",       "Baseline characteristics table",            value = FALSE)),
                      hint("Uncheck PROMs for trials where patients don't complete questionnaires (e.g. observational, registry, biomarker-only)."))
              )),

              # ── Step 6 — Review & create ──────────────────────────────
              shinyjs::hidden(div(class = "nt-step", id = "wiz_step_6",
                  step_head(HTML("&#x2714;"), "Review & create",
                            "One last look before the dashboard is generated."),
                  div(class = "nt-card nt-review",
                      uiOutput("wiz_review_summary")),
                  div(class = "nt-review-note",
                      HTML("Click <strong>Create trial</strong> to generate the configuration. You can fine-tune everything later by editing <code>trials/&lt;code&gt;/config.R</code> or via Trial Settings."))
              ))
            ),

            # ── Sticky footer: navigation ─────────────────────────────────
            div(class = "nt-foot",
                tags$button(id = "wiz_prev", class = "nt-btn nt-btn-ghost action-button",
                            type = "button",
                            onclick = "Shiny.setInputValue('wiz_prev', Math.random(), {priority:'event'})",
                            HTML("&larr; Back")),
                div(class = "nt-foot-spacer"),
                tags$button(class = "nt-btn nt-btn-ghost", type = "button",
                            onclick = "Shiny.setInputValue('wiz_cancel', Math.random(), {priority:'event'})",
                            "Cancel"),
                tags$button(id = "wiz_next", class = "nt-btn nt-btn-primary action-button",
                            type = "button",
                            onclick = "Shiny.setInputValue('wiz_next', Math.random(), {priority:'event'})",
                            HTML("Next &rarr;")),
                shinyjs::hidden(
                  tags$button(id = "wiz_create", class = "nt-btn nt-btn-create action-button",
                              type = "button",
                              onclick = "Shiny.setInputValue('wiz_create', Math.random(), {priority:'event'})",
                              HTML("&#x2714; Create trial")))
            )
        )
    )
  ))
}

# A small colour field: a native colour picker + hex text box, both mirrored to
# Shiny input `id` (read by the create handler as wiz_col_*). No value is sent
# until the user interacts; until then the create handler's own defaults apply,
# so input$<id> simply being NULL is fine.
.nt_color <- function(id, label, default) {
  swatch_id <- paste0(id, "_swatch")
  hex_id    <- paste0(id, "_hex")
  div(class = "nt-color-field",
      tags$label(label),
      div(class = "nt-color-row",
          tags$input(type = "color", value = default, id = swatch_id,
                     class = "nt-color-swatch",
                     oninput = sprintf(
                       "Shiny.setInputValue('%s', this.value); document.getElementById('%s').value = this.value;",
                       id, hex_id)),
          tags$input(type = "text", value = default, id = hex_id,
                     class = "nt-color-hex form-control",
                     onchange = sprintf(
                       "Shiny.setInputValue('%s', this.value); try{document.getElementById('%s').value = this.value;}catch(e){}",
                       id, swatch_id))))
}
