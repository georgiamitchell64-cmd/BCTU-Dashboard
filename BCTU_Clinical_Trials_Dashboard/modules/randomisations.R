randomisations_tab_ui <- function() {
  tabPanel("randomisations",
    div(class = "rand-shell",

      # ── KPI headline row ─────────────────────────────────────────
      uiOutput("rand_kpi_strip"),

      # ── Charts: monthly bar + cumulative line ────────────────────
      div(class = "rand-charts",
        tags$section(class = "tonic-card ov-card",
          div(class = "ov-card-head",
              tags$h2(textOutput("rand_monthly_title", inline = TRUE))),
          div(class = "ov-card-body",
              withSpinner(echarts4rOutput("rand_monthly_chart", height = "280px"),
                          type = 4, color = col_teal))
        ),
        tags$section(class = "tonic-card ov-card",
          div(class = "ov-card-head",
              tags$h2(textOutput("rand_cumulative_title", inline = TRUE))),
          div(class = "ov-card-body",
              withSpinner(echarts4rOutput("rand_cumulative_chart", height = "280px"),
                          type = 4, color = col_teal))
        )
      ),

      # ── Per-site randomisation counts (read-only) ────────────────
      tags$section(class = "tonic-card ov-card",
        div(class = "ov-card-head",
            tags$h2(textOutput("rand_site_title", inline = TRUE)),
            span(class = "ov-card-tool-note",
                 "Counts come from the latest REDCap CSV export.")),
        div(class = "ov-card-body",
            withSpinner(reactableOutput("rand_table"),
                        type = 4, color = col_teal))
      ),

      # ── Activity log ────────────────────────────────────────────
      tags$section(class = "tonic-card ov-card",
        div(class = "ov-card-head",
            tags$h2("Activity log")),
        div(class = "ov-card-body",
            withSpinner(reactableOutput("log_table"),
                        type = 4, color = col_teal))
      )
    )
  )
}
