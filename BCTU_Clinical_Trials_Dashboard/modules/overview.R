overview_tab_ui <- function() {

  # Calendar glyph for the date-range pill
  cal_icon <- HTML(
    '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" aria-hidden="true">
       <rect x="2" y="3" width="12" height="11" rx="2"
             stroke="#8A8A8C" stroke-width="1.2" fill="none"/>
       <path d="M2 7h12" stroke="#8A8A8C" stroke-width="1.2"/>
       <path d="M5.5 1.5v3M10.5 1.5v3"
             stroke="#8A8A8C" stroke-width="1.2" stroke-linecap="round"/>
     </svg>'
  )

  tabPanel("overview",
    div(class = "pov-shell ov-shell",

      # ── Page header + date range ─────────────────────────────────────
      div(class = "pov-page-head",
        tags$h1("Overview"),
        div(class = "pov-daterange",
          div(class = "pov-daterange-box",
              cal_icon,
              dateInput("date_from", label = NULL,
                        value = Sys.Date() %m-% months(1),
                        format = "d M yyyy"),
              span(class = "pov-daterange-arrow", HTML("&rarr;")),
              dateInput("date_to", label = NULL,
                        value = Sys.Date(),
                        format = "d M yyyy")),
          span(class = "pov-daterange-note",
               HTML("&Delta; = change over selected range"))
        )
      ),

      # Hidden text output retained for any callers still wired to it
      tags$div(style = "display:none;",
               textOutput("meeting_label_txt", inline = TRUE)),

      # ── KPI cards ────────────────────────────────────────────────────
      div(class = "pov-kpis",
        # Sites
        div(class = "pov-kpi",
          div(class = "pov-kpi-body",
            div(class = "pov-kpi-label", "Sites"),
            div(class = "pov-kpi-value", textOutput("n_sites", inline = TRUE)),
            div(class = "pov-kpi-sub", "active in dashboard"),
            div(class = "pov-kpi-delta-row",
                uiOutput("delta_sites", inline = TRUE),
                span(class = "pov-delta-label", "this period"))),
          div(class = "pov-kpi-spark", uiOutput("kpi_spark_sites"))
        ),
        # Total randomised / registered — the label follows the trial's
        # recruitment model, so cohort studies do not read "randomised".
        div(class = "pov-kpi",
          div(class = "pov-kpi-body",
            div(class = "pov-kpi-label", textOutput("kpi_rand_label", inline = TRUE)),
            div(class = "pov-kpi-value", textOutput("n_rand", inline = TRUE)),
            div(class = "pov-kpi-sub", textOutput("n_rand_sub", inline = TRUE)),
            div(class = "pov-kpi-delta-row",
                uiOutput("delta_rand", inline = TRUE),
                span(class = "pov-delta-label", "this period"))),
          div(class = "pov-kpi-spark", uiOutput("kpi_spark_rand"))
        ),
        # Of trial target
        div(class = "pov-kpi",
          div(class = "pov-kpi-body",
            div(class = "pov-kpi-label", "Of trial target"),
            div(class = "pov-kpi-value", textOutput("n_pct", inline = TRUE)),
            div(class = "pov-kpi-sub", textOutput("n_target_sub", inline = TRUE)),
            div(class = "pov-kpi-delta-row",
                uiOutput("delta_pct", inline = TRUE),
                span(class = "pov-delta-label", "pp this period"))),
          div(class = "pov-kpi-spark", uiOutput("kpi_spark_pct"))
        )
      ),

      # ── Trial health: score, components, what needs attention ────────
      uiOutput("health_card_ui"),

      # ── Work-package summary (multi-WP trials, "all WPs" view only) ──
      uiOutput("wp_summary_ui"),

      # ── Smart Insights ───────────────────────────────────────────────
      tags$section(class = "pov-card",
        div(class = "pov-card-head",
            tags$h3("Smart Insights"),
            span(class = "pov-card-tool-note",
                 "Auto-generated from trial data")),
        uiOutput("smart_insights_ui")
      ),

      # ── Recruitment projection (full width) ─────────────────────────
      tags$section(class = "pov-card",
        div(class = "pov-card-head",
          div(
            tags$h3("Recruitment projection"),
            span(class = "pov-card-sub",
                 textOutput("proj_completion_tool", inline = TRUE))
          ),
          div(class = "pov-card-tools",
            actionButton("toggle_proj_settings", "Assumptions",
                         icon  = icon("sliders"),
                         class = "btn-outline")
          )
        ),

        shinyjs::hidden(
          div(id = "proj_settings_panel", class = "pov-assumptions",
              div(style = "display:grid;grid-template-columns:1fr 1fr 1fr;gap:16px;",
                  div(sliderInput("proj_rate_pessimistic", "Pessimistic — rate/site/month",
                                  min = 0, max = 10, value = 2, step = 0.5, width = "100%")),
                  div(sliderInput("proj_rate_central", "Central — rate/site/month",
                                  min = 0, max = 10, value = 3, step = 0.5, width = "100%")),
                  div(sliderInput("proj_rate_optimistic", "Optimistic — rate/site/month",
                                  min = 0, max = 10, value = 4, step = 0.5, width = "100%"))),
              div(style = "display:grid;grid-template-columns:1fr 1fr 1fr;gap:16px;margin-top:8px;",
                  div(sliderInput("proj_sites_pessimistic", "Sites opening/month",
                                  min = 0, max = 6, value = 1, step = 0.5, width = "100%")),
                  div(sliderInput("proj_sites_central", "Sites opening/month",
                                  min = 0, max = 6, value = 2, step = 0.5, width = "100%")),
                  div(sliderInput("proj_sites_optimistic", "Sites opening/month",
                                  min = 0, max = 6, value = 3, step = 0.5, width = "100%"))),
              div(style = "max-width:340px;margin-top:8px;",
                  sliderInput("proj_target_sites", "Total sites when ramped",
                              min = 1, max = 60, value = 24, step = 1, width = "100%")),
              div(style = "display:flex;justify-content:flex-end;margin-top:6px;",
                  actionButton("reset_proj_settings", "Reset to defaults",
                               class = "ov-link"))
          )
        ),

        withSpinner(echarts4rOutput("proj_chart", height = "320px"),
                    type = 4, color = col_teal),

        div(class = "ov-chart-note",
            textOutput("proj_note", inline = TRUE))
      ),

      # ── Trial replay: living CONSORT, site race, pace (feature flag) ──
      uiOutput("trial_replay_ui"),

      # ── CONSORT flow (only when the trial enables it) ───────────────
      uiOutput("consort_card_ui"),

      # ── Portfolio review — collapsed by default behind a toggle ─────
      tags$section(class = "pov-card pov-collapsible",
        div(class = "pov-card-head pov-collapsible-head",
          div(style = "display:flex;align-items:center;gap:12px;",
            tags$h3("Portfolio review"),
            span(class = "pov-card-tool-note",
                 "Hidden by default — open when reviewing portfolio rates.")
          ),
          div(class = "pov-card-tools",
            actionButton("toggle_pr_panel",
                         label = tagList(
                           span(id = "pr_toggle_lbl", "Show chart"),
                           HTML('<svg width="11" height="11" viewBox="0 0 16 16" fill="none" aria-hidden="true" style="margin-left:6px;"><path d="M3 6l5 5 5-5" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round" fill="none"/></svg>')
                         ),
                         class = "btn-outline")
          )
        ),
        shinyjs::hidden(
          div(id = "pr_panel",
              div(class = "pov-card-tools",
                  style = "justify-content:flex-end;gap:8px;margin-bottom:10px;",
                  span(class = "pov-card-tool-note",
                       "Edit values — chart updates live."),
                  actionButton("pr_save", HTML("&check; Save"),
                               class = "btn-outline",
                               style = "font-size:11.5px;"),
                  actionButton("pr_reset", "Reset",
                               class = "btn-outline",
                               style = "font-size:11.5px;")),
              div(style = "display:grid;grid-template-columns:minmax(0, 1.3fr) minmax(0, 1fr);
                           gap:18px;align-items:start;",
                  div(style = "min-width:0;",
                      withSpinner(echarts4rOutput("pr_chart", height = "320px"),
                                  type = 4, color = col_teal)),
                  div(style = "min-width:0;",
                      uiOutput("pr_form_ui"))
              ),
              div(style = "margin-top:14px;",
                  uiOutput("pr_data_table"))
          )
        )
      ),

      # ── Map + pipeline grid ──────────────────────────────────────────
      div(class = "pov-map-grid",
        tags$section(class = "pov-card",
          div(class = "pov-card-head",
              tags$h3("UK site map"),
              span(class = "pov-card-tool-note",
                   textOutput("sites_bubble_note", inline = TRUE))),
          leafletOutput("site_map", height = 380)
        ),
        div(class = "pov-map-col-right",
          tags$section(class = "pov-card",
            div(class = "pov-pipeline-h4", "Site pipeline"),
            withSpinner(echarts4rOutput("pipeline_chart", height = "200px"),
                        type = 4, color = col_teal)
          ),
          tags$section(class = "pov-card",
            div(class = "pov-pipeline-h4", "Top recruiting sites"),
            withSpinner(echarts4rOutput("top_sites_chart", height = "180px"),
                        type = 4, color = col_teal)
          )
        )
      ),

      # ── Sites table ─────────────────────────────────────────────────
      div(class = "pov-sitestable",
        div(class = "pov-sitestable-head",
            tags$h3("All sites"),
            div(class = "pov-sitestable-search",
                textInput("site_search_ov", label = NULL,
                          placeholder = "Filter sites…", width = "180px"))),
        div(class = "ov-table-wrap",
            withSpinner(reactableOutput("overview_table"),
                        type = 4, color = col_teal))
      )
    )
  )
}
