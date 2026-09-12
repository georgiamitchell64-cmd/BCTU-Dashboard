randomisations_tab_ui <- function() {
  tabPanel("randomisations",
    div(class = "rand-shell",

      # ── KPI headline row: volume, recent trend, in / out of hours ─────
      uiOutput("rand_health_kpis"),

      # ── Where recruitment is heading ─────────────────────────────────
      div(class = "th-section",
        div(class = "th-section-head",
          div(tags$h3("Where recruitment is heading"),
              div(class = "th-sub-t",
                  "Cumulative recruitment with three projections from recent weeks, and the likely range from 1,000 simulations. Toggle lines on and off; hover for values."))),
        uiOutput("rand_trajectory_ui")),

      # ── When recruitment happens ─────────────────────────────────────
      div(class = "th-grid-2",
        div(class = "th-section",
          div(class = "th-section-head",
            div(tags$h3(textOutput("rand_when_title", inline = TRUE)),
                div(class = "th-sub-t",
                    "Weekday by hour of day. The shaded band is working hours, set in Settings → Recruitment & monitoring."))),
          uiOutput("rand_punchcard_ui")),
        div(class = "th-section",
          div(class = "th-section-head",
            div(tags$h3(textOutput("rand_monthly_title", inline = TRUE)),
                div(class = "th-sub-t", "Split into in hours, weekday out of hours, and weekends and bank holidays."))),
          withSpinner(echarts4rOutput("rand_monthly_chart", height = "300px"),
                      type = 4, color = col_teal))),

      # ── Out-of-hours recruitment by site ─────────────────────────────
      div(class = "th-section",
        div(class = "th-section-head",
          div(tags$h3("Out-of-hours recruitment by site"),
              div(class = "th-sub-t",
                  "Which sites randomise outside working hours, most first. Bar length is the site's total; click a site for its details."))),
        uiOutput("rand_ooh_sites_ui")),

      # ── Recruitment by site ──────────────────────────────────────────
      div(class = "th-section",
        div(class = "th-section-head",
          div(tags$h3("Recruitment by site"),
              div(class = "th-sub-t",
                  "Pace, timing and gaps for each site. Slow starts, long gaps and quiet spells show where a site may need support."))),
        withSpinner(reactableOutput("rand_site_patterns"), type = 4, color = col_teal)),

      # ── Activity log ────────────────────────────────────────────────
      div(class = "th-section",
        div(class = "th-section-head", div(tags$h3("Activity log"))),
        withSpinner(reactableOutput("log_table"), type = 4, color = col_teal))
    )
  )
}
