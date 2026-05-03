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

  tagList(

    # ── Top summary bar ──────────────────────────────────────────────────────
    uiOutput(ns("summary_cards")),

    tags$hr(style = "margin: 16px 0;"),

    # ── Controls ─────────────────────────────────────────────────────────────
    fluidRow(
      column(4,
        radioButtons(
          ns("rate_type"),
          label    = "Rate to display",
          choices  = c("% due entered" = "pct_due", "% expected entered" = "pct_expected"),
          selected = "pct_due",
          inline   = TRUE
        )
      ),
      column(4,
        checkboxGroupInput(
          ns("selected_timepoints"),
          label    = "Timepoints",
          choices  = c("Baseline", "Discharge", "Day 30", "Day 90"),
          selected = c("Baseline", "Discharge", "Day 30", "Day 90"),
          inline   = TRUE
        )
      ),
      column(4,
        tags$div(
          style = "padding-top: 26px;",
          actionButton(
            ns("expand_all"),
            "Expand all sites",
            class = "btn btn-sm btn-outline-secondary",
            icon  = icon("chevron-down")
          )
        )
      )
    ),

    tags$hr(style = "margin: 8px 0 16px;"),

    # ── Overall heatmap ───────────────────────────────────────────────────────
    tags$h5("Overall — all sites combined",
            style = "color: #1B4F6B; font-weight: 600; margin-bottom: 8px;"),
    uiOutput(ns("overall_heatmap")),

    tags$hr(style = "margin: 20px 0 12px;"),

    # ── Per-site accordion ────────────────────────────────────────────────────
    tags$h5("By site",
            style = "color: #1B4F6B; font-weight: 600; margin-bottom: 12px;"),
    uiOutput(ns("site_panels")),

    # ── Source footer ─────────────────────────────────────────────────────────
    tags$div(
      style = "margin-top: 20px; padding-top: 12px; border-top: 1px solid #e9ecef;
               font-size: 0.75rem; color: #868e96; text-align: right;",
      uiOutput(ns("source_info"))
    )
  )
}
