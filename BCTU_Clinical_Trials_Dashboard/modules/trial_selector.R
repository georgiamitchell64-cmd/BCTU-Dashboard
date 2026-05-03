trial_selector_ui <- function() {
  div(id = "trial_selector_panel",
      style = "min-height:80vh;display:flex;align-items:center;justify-content:center;",

      div(style = "text-align:center;max-width:900px;width:100%;padding:40px 20px;",

          # Header
          div(style = "margin-bottom:36px;",
              # Generic icon
              div(style = "width:64px;height:64px;margin:0 auto 14px;border-radius:18px;
                           background:linear-gradient(135deg,#4338CA,#8B5CF6);
                           display:flex;align-items:center;justify-content:center;",
                  HTML('<svg width="32" height="32" viewBox="0 0 24 24" fill="none" stroke="white"
                        stroke-width="2.3" stroke-linecap="round" stroke-linejoin="round">
                        <path d="M12 2L2 7l10 5 10-5-10-5z"/><path d="M2 17l10 5 10-5"/>
                        <path d="M2 12l10 5 10-5"/></svg>')),
              div(style = "font-size:28px;font-weight:700;color:#1E293B;letter-spacing:-0.5px;",
                  "Clinical Trials Dashboard"),
              div(style = "font-size:14px;color:#64748B;margin-top:6px;",
                  "Select a trial to open, or create a new one")
          ),

          # Trial cards container (populated by server)
          uiOutput("trial_cards_ui"),

          # Footer
          div(style = "margin-top:40px;font-size:12px;color:#94A3B8;",
              HTML("Birmingham Clinical Trials Unit &middot; University of Birmingham"))
      )
  )
}


# ── "Add New Trial" wizard modal ──────────────────────────────────────────────

new_trial_wizard_ui <- function() {

  step_label <- function(num, title) {
    div(style = "margin-bottom:14px;",
        div(style = "display:flex;align-items:center;gap:10px;margin-bottom:8px;",
            span(style = "width:26px;height:26px;border-radius:50%;background:#1B4F6B;color:#fff;
                          display:inline-flex;align-items:center;justify-content:center;
                          font-size:12px;font-weight:700;", num),
            span(style = "font-size:14px;font-weight:600;color:#1B4F6B;", title)
        )
    )
  }

  help_text <- function(txt) {
    div(style = "font-size:11px;color:#64748B;margin-top:-6px;margin-bottom:10px;
                 line-height:1.5;font-style:italic;", txt)
  }

  modalDialog(
    title = div(style = "display:flex;align-items:center;gap:10px;",
                span(style = "font-size:22px;", HTML("&#x2795;")),
                span("Add a New Trial")),
    size = "l",
    easyClose = TRUE,
    footer = div(
      style = "display:flex;justify-content:space-between;width:100%;",
      modalButton("Cancel"),
      div(
        actionButton("wiz_prev", HTML("&larr; Back"),
                     class = "btn btn-outline-secondary",
                     style = "margin-right:8px;"),
        actionButton("wiz_next", HTML("Next &rarr;"),
                     class = "btn btn-primary",
                     style = "background:#1B4F6B;border-color:#1B4F6B;margin-right:8px;"),
        shinyjs::hidden(
          actionButton("wiz_create", HTML("&#x2714; Create Trial"),
                       class = "btn btn-success",
                       style = "background:#2EC4A5;border-color:#2EC4A5;font-weight:600;")
        )
      )
    ),

    # Step indicator
    uiOutput("wiz_step_indicator"),

    # ── Step 1: Basics ──────────────────────────────────────────────────────
    div(id = "wiz_step_1",
        step_label("1", "Trial basics"),
        help_text("These are the essential details. Everything else has sensible defaults you can tweak later."),
        div(style = "display:grid;grid-template-columns:1fr 1fr;gap:14px;",
            textInput("wiz_short_name", "Short name (e.g. TONIC, LOCI)", placeholder = "MYTRIAL"),
            numericInput("wiz_target", "Recruitment target", value = 100, min = 1)
        ),
        textInput("wiz_full_name", "Full trial name",
                  placeholder = "e.g. A randomised trial comparing X vs Y in patients with Z",
                  width = "100%"),
        div(style = "display:grid;grid-template-columns:1fr 1fr;gap:14px;",
            textInput("wiz_ci", "Chief Investigator", placeholder = "e.g. Prof Jane Smith"),
            textInput("wiz_sponsor", "Sponsor", placeholder = "e.g. University of Birmingham")
        )
    ),

    # ── Step 2: Data source ─────────────────────────────────────────────────
    shinyjs::hidden(div(id = "wiz_step_2",
        step_label("2", "Data source"),
        help_text("Where does this trial's REDCap data live?"),
        radioButtons("wiz_data_source", NULL,
                     choices = c("Inside the app folder (recommended for testing)" = "local",
                                 "Network drive path (e.g. K: drive)" = "network"),
                     selected = "local", inline = FALSE),
        conditionalPanel(
          condition = "input.wiz_data_source == 'network'",
          textInput("wiz_data_path", "Full folder path to REDCap CSV exports",
                    placeholder = "K:/BCTU/Teams/MyTeam/MyTrial/Data", width = "100%"),
          help_text("Use forward slashes. The app needs read access to this folder.")
        )
    )),

    # ── Step 3: REDCap events ───────────────────────────────────────────────
    shinyjs::hidden(div(id = "wiz_step_3",
        step_label("3", "REDCap events"),
        help_text("Map your REDCap event names. Only Baseline is required."),
        div(style = "display:grid;grid-template-columns:1fr 1fr;gap:12px;",
            textInput("wiz_ev_baseline",  "Baseline event *",  value = "baseline_arm_1"),
            textInput("wiz_ev_discharge", "Discharge event",   value = "discharge_arm_1"),
            textInput("wiz_ev_day30",     "Day 30 event",      placeholder = "day_30_arm_1"),
            textInput("wiz_ev_day90",     "Day 90 event",      placeholder = "day_90_arm_1")
        ),
        textInput("wiz_ev_subforms", "Sub-forms / SAE events (comma-separated)",
                  value = "sub_forms_arm_1, ad_hoc_arm_1", width = "100%")
    )),

    # ── Step 4: REDCap fields ───────────────────────────────────────────────
    shinyjs::hidden(div(id = "wiz_step_4",
        step_label("4", "REDCap field names"),
        help_text("Map the key variable names from your REDCap project. Only the first three are required."),
        div(style = "font-size:11px;font-weight:600;color:#1B4F6B;margin-bottom:6px;
                     text-transform:uppercase;letter-spacing:.5px;", "Required"),
        div(style = "display:grid;grid-template-columns:1fr 1fr 1fr;gap:12px;",
            textInput("wiz_fld_record_id", "Record ID",              value = "record_id"),
            textInput("wiz_fld_site",      "Site name",              value = "site_name"),
            textInput("wiz_fld_rand_dt",   "Randomisation datetime", value = "rand_dttm_s")
        ),
        tags$hr(style = "border:none;border-top:1px solid #EEF3F8;margin:12px 0;"),
        div(style = "font-size:11px;font-weight:600;color:#1B4F6B;margin-bottom:6px;
                     text-transform:uppercase;letter-spacing:.5px;", "Optional"),
        help_text("Leave blank if not applicable."),
        div(style = "display:grid;grid-template-columns:1fr 1fr 1fr;gap:12px;",
            textInput("wiz_fld_op_date",        "Operation date",   placeholder = "iop_op_end_dt"),
            textInput("wiz_fld_discharge_date", "Discharge date",   placeholder = "dis_discharge_day"),
            textInput("wiz_fld_age",            "Age",              placeholder = "cae_age"),
            textInput("wiz_fld_sex",            "Sex",              placeholder = "base_sex"),
            textInput("wiz_fld_ethnicity",      "Ethnicity",        placeholder = "base_ethnic_gp"),
            textInput("wiz_fld_cos_type",       "Change of status", placeholder = "cos_type")
        )
    )),

    # ── Step 5: Features ────────────────────────────────────────────────────
    shinyjs::hidden(div(id = "wiz_step_5",
        step_label("5", "Features"),
        help_text("Choose which dashboard tabs to show. You can change these later in the config file."),
        div(style = "display:grid;grid-template-columns:1fr 1fr;gap:6px 20px;margin-bottom:16px;",
            checkboxInput("wiz_feat_projections", "Recruitment projections",      value = TRUE),
            checkboxInput("wiz_feat_postal",      "Postal tracking tab",          value = FALSE),
            checkboxInput("wiz_feat_returns",     "Return rates tab",             value = FALSE),
            checkboxInput("wiz_feat_pilot",       "Pilot progression criteria",   value = FALSE),
            checkboxInput("wiz_feat_consort",     "CONSORT flow diagram",         value = FALSE),
            checkboxInput("wiz_feat_baseline",    "Baseline characteristics table", value = FALSE)
        )
    )),

    # ── Step 6: Review ──────────────────────────────────────────────────────
    shinyjs::hidden(div(id = "wiz_step_6",
        step_label(HTML("&#x2714;"), "Review & create"),
        div(style = "background:#F0F9F7;border:1px solid #C4EBE4;border-radius:8px;padding:16px 18px;",
            uiOutput("wiz_review_summary")
        ),
        div(style = "margin-top:14px;font-size:12px;color:#64748B;line-height:1.6;",
            HTML("Click <strong>Create Trial</strong> to generate the configuration.
                  You can fine-tune settings later by editing
                  <code>trials/&lt;code&gt;/config.R</code>."))
    ))
  )
}
