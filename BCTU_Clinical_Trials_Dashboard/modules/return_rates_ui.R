# ── return_rates_ui.R ────────────────────────────────────────────────────────
#
# Two functions:
#   return_rates_tab_ui()  — outer wrapper, matches the app's tabPanel pattern
#   return_rates_ui(id)    — the actual module UI, called from the wrapper
#
# ─────────────────────────────────────────────────────────────────────────────

return_rates_tab_ui <- function() {
  tabPanel("returns_panel", value = "returns_panel",
    return_rates_ui("rr")
  )
}

return_rates_ui <- function(id) {
  ns <- NS(id)

  div(class = "rr-shell",

    # ── Hero band ──────────────────────────────────────────────────────
    div(class = "rr-hero",
        div(class = "rr-hero-main",
            uiOutput(ns("summary_cards"))
        ),
        div(class = "rr-hero-spark",
            # Sparkline placeholder — rendered by server or left as visual element
            tags$svg(width = "120", height = "40", viewBox = "0 0 120 40",
                     tags$polyline(
                       points = "0,35 20,28 40,30 60,18 80,22 100,12 120,8",
                       fill = "none", stroke = "rgba(255,255,255,.5)",
                       `stroke-width` = "2")))
    ),

    # ── Filter bar ─────────────────────────────────────────────────────
    div(class = "rr-filters",
        div(class = "rr-filter-group",
            span(class = "rr-filter-label", "Rate type"),
            radioButtons(
              ns("rate_type"),
              label    = NULL,
              choices  = c("% due entered" = "pct_due", "% expected entered" = "pct_expected"),
              selected = "pct_due",
              inline   = TRUE
            )
        ),
        div(class = "rr-filter-group",
            span(class = "rr-filter-label", "Timepoints"),
            checkboxGroupInput(
              ns("selected_timepoints"),
              label    = NULL,
              choices  = c("Baseline", "Discharge", "Day 30", "Day 90"),
              selected = c("Baseline", "Discharge", "Day 30", "Day 90"),
              inline   = TRUE
            )
        ),
        div(style = "margin-left:auto;",
            actionButton(
              ns("expand_all"),
              "Expand all sites",
              class = "btn-outline",
              icon  = icon("chevron-down")
            )
        )
    ),

    # ── Overall heatmap ─────────────────────────────────────────────────
    tags$section(class = "tonic-card ov-card",
      div(class = "ov-card-head",
          tags$h2("Overall — all sites combined")),
      div(class = "ov-card-body",
          uiOutput(ns("overall_heatmap")))
    ),

    # ── Per-site panels ─────────────────────────────────────────────────
    tags$section(class = "tonic-card ov-card",
      div(class = "ov-card-head",
          tags$h2("By site")),
      div(class = "ov-card-body",
          uiOutput(ns("site_panels")))
    ),

    # ── Source footer ───────────────────────────────────────────────────
    div(style = "margin-top:16px;padding-top:12px;border-top:1px solid var(--ov-line2);
                 font-size:11px;color:var(--ov-muted);text-align:right;",
        uiOutput(ns("source_info")))
  )
}
