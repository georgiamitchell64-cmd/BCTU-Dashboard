trial_settings_tab_ui <- function() {
  tabPanel("settings",

    div(class = "section-heading", HTML("&#x2699; Trial Settings")),

    div(class = "grid-2",

      # ── Left: Theme ──────────────────────────────────────────────────────
      tonic_card(
        title = "Theme",
        tools = actionButton("settings_save_colors", HTML("&#x1F4BE; Save"),
                             class = "btn btn-sm btn-success",
                             style = "background:#2EC4A5;border-color:#2EC4A5;font-size:11px;"),

        div(style = "font-size:11px;color:var(--muted);margin-bottom:14px;",
            "Choose a curated theme or pick Custom to set colours directly.
             Saving applies the theme immediately."),

        uiOutput("theme_picker_ui"),

        # Custom colours panel — only shown when "custom" is the active theme
        shinyjs::hidden(div(id = "custom_colors_panel",
          tags$hr(style = "border:none;border-top:1px solid #EEF3F8;margin:14px 0;"),
          div(style = "font-size:11px;font-weight:600;color:var(--navy);margin-bottom:8px;
                       text-transform:uppercase;letter-spacing:.5px;", "Custom colours"),
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
          div(style = "margin-top:14px;",
              div(style = "font-size:10px;font-weight:600;color:var(--navy);margin-bottom:6px;
                           text-transform:uppercase;letter-spacing:.5px;", "Preview"),
              uiOutput("color_preview_bar")
          )
        ))
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
        selectInput("set_category", "Portfolio category",
                    choices = TRIAL_CATEGORIES, selected = "Other", width = "100%"),
        div(style = "display:grid;grid-template-columns:1fr 1fr;gap:12px;",
            textInput("set_ci", "Chief Investigator", value = ""),
            textInput("set_sponsor", "Sponsor", value = "")
        )
      )
    ),

    div(style = "margin-bottom:14px;"),

    # ── Bottom row: config + reset ─────────────────────────────────────────
    tonic_card(
      title = "Config & overrides",
      tools = actionButton("settings_reset_overrides",
                           HTML("&#x21BA; Reset to defaults"),
                           class = "btn btn-sm btn-outline-secondary",
                           style = "font-size:11px;"),
      div(style = "font-size:12px;color:var(--muted);line-height:1.6;",
          uiOutput("settings_config_path"),
          uiOutput("settings_overrides_status"),
          div(style = "margin-top:8px;font-style:italic;",
              "Edits made above are saved to overrides.json next to the config file.
               Reset to defaults removes the overrides and reverts to the original config.")
      )
    ),

    div(style = "margin-bottom:14px;"),

    # ── Danger zone: delete trial (admin only) ─────────────────────────────
    uiOutput("settings_danger_zone_ui")
  )
}
