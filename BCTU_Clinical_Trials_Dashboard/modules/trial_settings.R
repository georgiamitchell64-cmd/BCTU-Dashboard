trial_settings_tab_ui <- function() {
  tabPanel("settings",

    div(class = "section-heading", HTML("&#x2699; Trial Settings")),

    div(class = "grid-2",

      # ── Left: Colour scheme ──────────────────────────────────────────────
      tonic_card(
        title = "Colour scheme",
        tools = actionButton("settings_save_colors", HTML("&#x1F4BE; Save"),
                             class = "btn btn-sm btn-success",
                             style = "background:#2EC4A5;border-color:#2EC4A5;font-size:11px;"),

        div(style = "font-size:11px;color:var(--muted);margin-bottom:14px;",
            "Pick a preset or set custom colours. Changes apply after restarting the app."),

        # Preset buttons
        div(style = "display:flex;flex-wrap:wrap;gap:8px;margin-bottom:16px;",
            actionButton("preset_navy_teal", div(style = "display:flex;align-items:center;gap:6px;",
              span(style = "width:14px;height:14px;border-radius:50%;background:linear-gradient(135deg,#1B4F6B,#2EC4A5);display:inline-block;"),
              "Navy & Teal"), class = "btn btn-sm btn-outline-secondary"),
            actionButton("preset_indigo_violet", div(style = "display:flex;align-items:center;gap:6px;",
              span(style = "width:14px;height:14px;border-radius:50%;background:linear-gradient(135deg,#312E81,#8B5CF6);display:inline-block;"),
              "Indigo"), class = "btn btn-sm btn-outline-secondary"),
            actionButton("preset_emerald", div(style = "display:flex;align-items:center;gap:6px;",
              span(style = "width:14px;height:14px;border-radius:50%;background:linear-gradient(135deg,#064E3B,#10B981);display:inline-block;"),
              "Emerald"), class = "btn btn-sm btn-outline-secondary"),
            actionButton("preset_slate_coral", div(style = "display:flex;align-items:center;gap:6px;",
              span(style = "width:14px;height:14px;border-radius:50%;background:linear-gradient(135deg,#334155,#F97316);display:inline-block;"),
              "Slate & Coral"), class = "btn btn-sm btn-outline-secondary"),
            actionButton("preset_burgundy", div(style = "display:flex;align-items:center;gap:6px;",
              span(style = "width:14px;height:14px;border-radius:50%;background:linear-gradient(135deg,#7F1D1D,#DC2626);display:inline-block;"),
              "Burgundy"), class = "btn btn-sm btn-outline-secondary"),
            actionButton("preset_ocean", div(style = "display:flex;align-items:center;gap:6px;",
              span(style = "width:14px;height:14px;border-radius:50%;background:linear-gradient(135deg,#0C4A6E,#06B6D4);display:inline-block;"),
              "Ocean"), class = "btn btn-sm btn-outline-secondary")
        ),

        tags$hr(style = "border:none;border-top:1px solid #EEF3F8;margin:12px 0;"),

        div(style = "font-size:11px;font-weight:600;color:var(--navy);margin-bottom:8px;
                     text-transform:uppercase;letter-spacing:.5px;", "Current colours"),
        div(style = "display:grid;grid-template-columns:1fr 1fr 1fr;gap:12px;",
            div(textInput("set_col_primary", "Primary (sidebar, headers)", value = "#1B4F6B"),
                div(id = "preview_primary",
                    style = "height:28px;border-radius:5px;margin-top:4px;background:#1B4F6B;")),
            div(textInput("set_col_secondary", "Secondary (accents, charts)", value = "#2EC4A5"),
                div(id = "preview_secondary",
                    style = "height:28px;border-radius:5px;margin-top:4px;background:#2EC4A5;")),
            div(textInput("set_col_accent", "Accent (highlights)", value = "#F59E0B"),
                div(id = "preview_accent",
                    style = "height:28px;border-radius:5px;margin-top:4px;background:#F59E0B;"))
        ),

        # Live preview bar
        div(style = "margin-top:14px;",
            div(style = "font-size:10px;font-weight:600;color:var(--navy);margin-bottom:6px;
                         text-transform:uppercase;letter-spacing:.5px;", "Preview"),
            uiOutput("color_preview_bar")
        )
      ),

      # ── Right: Features & identity ───────────────────────────────────────
      tonic_card(
        title = "Features & tabs",
        tools = actionButton("settings_save_features", HTML("&#x1F4BE; Save"),
                             class = "btn btn-sm btn-success",
                             style = "background:#2EC4A5;border-color:#2EC4A5;font-size:11px;"),

        div(style = "font-size:11px;color:var(--muted);margin-bottom:14px;",
            "Toggle which tabs and features are shown in the dashboard."),

        div(style = "display:grid;grid-template-columns:1fr 1fr;gap:4px 20px;",
            checkboxInput("set_feat_projections", "Recruitment projections",      value = TRUE),
            checkboxInput("set_feat_pilot",       "Pilot progression criteria",   value = FALSE),
            checkboxInput("set_feat_postal",      "Postal tracking tab",          value = FALSE),
            checkboxInput("set_feat_returns",     "Return rates tab",             value = FALSE),
            checkboxInput("set_feat_consort",     "CONSORT flow diagram",         value = FALSE),
            checkboxInput("set_feat_baseline",    "Baseline characteristics",     value = FALSE)
        ),

        tags$hr(style = "border:none;border-top:1px solid #EEF3F8;margin:14px 0;"),

        div(style = "font-size:11px;font-weight:600;color:var(--navy);margin-bottom:8px;
                     text-transform:uppercase;letter-spacing:.5px;", "Trial identity"),
        div(style = "display:grid;grid-template-columns:1fr 1fr;gap:12px;",
            textInput("set_short_name", "Short name", value = ""),
            numericInput("set_target", "Recruitment target", value = 100, min = 1)
        ),
        textInput("set_full_name", "Full trial name", value = "", width = "100%"),
        div(style = "display:grid;grid-template-columns:1fr 1fr;gap:12px;",
            textInput("set_ci", "Chief Investigator", value = ""),
            textInput("set_sponsor", "Sponsor", value = "")
        )
      )
    ),

    div(style = "margin-bottom:14px;"),

    # ── Bottom row: status ─────────────────────────────────────────────────
    tonic_card(
      title = "Config file",
      div(style = "font-size:12px;color:var(--muted);line-height:1.6;",
          uiOutput("settings_config_path"),
          div(style = "margin-top:8px;font-style:italic;",
              "For advanced settings (REDCap field mappings, ethnicity labels, target schedule, report defaults),
               edit the config file directly. Changes take effect on app restart.")
      )
    )
  )
}
