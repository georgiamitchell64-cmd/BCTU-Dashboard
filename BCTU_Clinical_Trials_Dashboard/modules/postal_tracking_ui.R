# ── postal_tracking_ui.R ─────────────────────────────────────────────────────

postal_tracking_tab_ui <- function() {
  tabPanel("postal_panel", value = "postal_panel",
    postal_tracking_ui("postal")
  )
}

postal_tracking_ui <- function(id) {
  ns <- NS(id)

  tags$div(class = "pt-view",

    # ── KPI cards ────────────────────────────────────────────────────────────
    uiOutput(ns("kpi_cards")),

    # ── Controls strip ───────────────────────────────────────────────────────
    tags$div(class = "pt-controls",
      tags$div(style = "min-width:170px;",
        radioButtons(
          ns("status_filter"),
          label    = "Show",
          choices  = c("Action needed"            = "action",
                       "All postal participants"  = "all",
                       "Sent only"                = "sent"),
          selected = "action",
          inline   = FALSE
        )
      ),
      tags$div(style = "min-width:170px;",
        checkboxGroupInput(
          ns("timepoint_filter"),
          label    = "Timepoints",
          choices  = c("Day 30", "Day 90"),
          selected = c("Day 30", "Day 90"),
          inline   = TRUE
        )
      ),
      tags$div(style = "flex:1; min-width:200px;",
        textInput(
          ns("search"),
          label       = "Search ID or site",
          placeholder = "e.g. TON001 or QEHB",
          width       = "100%"
        )
      ),
      tags$div(style = "margin-left:auto; align-self:flex-end;",
        downloadButton(
          ns("export_xlsx"),
          HTML("&darr; Export audit log"),
          class = "pt-export-btn"
        )
      )
    ),

    # ── Table card (table + legend) ──────────────────────────────────────────
    tags$div(class = "pt-table-card",
      reactable::reactableOutput(ns("postal_table")),
      tags$div(class = "pt-legend",
        tags$span(tags$span(class = "pt-leg-dot", style = "background:#e05c3a;"), "Overdue"),
        tags$span(tags$span(class = "pt-leg-dot", style = "background:#f0a500;"), "Due now (within 7 days)"),
        tags$span(tags$span(class = "pt-leg-dot", style = "background:#1B4F6B;"), "Upcoming (next 14 days)"),
        tags$span(tags$span(class = "pt-leg-dot", style = "background:#2EC4A5;"), "Sent"),
        tags$span(tags$span(class = "pt-leg-dot", style = "background:#10B981;"), "Returned"),
        tags$span(tags$span(class = "pt-leg-dot", style = "background:#6366F1;"), "Transcribed"),
        tags$span(tags$span(class = "pt-leg-dot", style = "background:#94A3B8;"), "Not sent / Future"),
        tags$span(tags$span(class = "pt-leg-dot", style = "background:#DC2626;"),
                  "Excluded (deceased / withdrawn / lost to follow-up — do not send)")
      )
    ),

    # ── Source footer ────────────────────────────────────────────────────────
    tags$div(class = "pt-footer",
      uiOutput(ns("source_info"))
    )
  )
}
