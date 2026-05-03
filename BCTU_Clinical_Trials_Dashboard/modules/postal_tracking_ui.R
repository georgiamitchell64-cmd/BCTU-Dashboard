# ── postal_tracking_ui.R ─────────────────────────────────────────────────────

postal_tracking_tab_ui <- function() {
  tabPanel("postal_panel", value = "postal_panel",
    postal_tracking_ui("postal")
  )
}

postal_tracking_ui <- function(id) {
  ns <- NS(id)

  tagList(

    # ── KPI cards ────────────────────────────────────────────────────────────
    uiOutput(ns("kpi_cards")),

    tags$hr(style = "margin: 16px 0;"),

    # ── Controls ─────────────────────────────────────────────────────────────
    fluidRow(
      column(3,
        radioButtons(
          ns("status_filter"),
          label    = "Show",
          choices  = c("Action needed" = "action",
                       "All postal participants" = "all",
                       "Sent only" = "sent"),
          selected = "action",
          inline   = FALSE
        )
      ),
      column(3,
        checkboxGroupInput(
          ns("timepoint_filter"),
          label    = "Timepoints",
          choices  = c("Day 30", "Day 90"),
          selected = c("Day 30", "Day 90"),
          inline   = TRUE
        )
      ),
      column(3,
        textInput(
          ns("search"),
          label       = "Search ID or site",
          placeholder = "e.g. TON001 or QEHB"
        )
      ),
      column(3,
        tags$div(
          style = "padding-top: 26px; display: flex; gap: 8px;",
          downloadButton(
            ns("export_xlsx"),
            "Export audit log",
            class = "btn btn-sm",
            style = paste0("background-color: #1B4F6B; color: white; border: none;")
          )
        )
      )
    ),

    tags$hr(style = "margin: 8px 0 16px;"),

    # ── Main table ───────────────────────────────────────────────────────────
    tags$div(
      style = "background: white; border-radius: 8px; padding: 4px;",
      reactable::reactableOutput(ns("postal_table"))
    ),

    # ── Legend ───────────────────────────────────────────────────────────────
    tags$div(
      style = "margin-top: 14px; font-size: 0.8rem; color: #6c757d;
               display: flex; gap: 18px; flex-wrap: wrap;",
      tags$span(tags$span(style = "display:inline-block;width:10px;height:10px;border-radius:50%;background:#e05c3a;margin-right:6px;vertical-align:middle;"), "Overdue"),
      tags$span(tags$span(style = "display:inline-block;width:10px;height:10px;border-radius:50%;background:#f0a500;margin-right:6px;vertical-align:middle;"), "Due now (within 7 days)"),
      tags$span(tags$span(style = "display:inline-block;width:10px;height:10px;border-radius:50%;background:#1B4F6B;margin-right:6px;vertical-align:middle;"), "Upcoming (next 14 days)"),
      tags$span(tags$span(style = "display:inline-block;width:10px;height:10px;border-radius:50%;background:#2EC4A5;margin-right:6px;vertical-align:middle;"), "Sent"),
      tags$span(tags$span(style = "display:inline-block;width:10px;height:10px;border-radius:50%;background:#adb5bd;margin-right:6px;vertical-align:middle;"), "Future (>3 weeks away)")
    ),

    # ── Source footer ────────────────────────────────────────────────────────
    tags$div(
      style = "margin-top: 24px; padding-top: 12px; border-top: 1px solid #e9ecef;
               font-size: 0.75rem; color: #868e96; text-align: right;",
      uiOutput(ns("source_info"))
    )
  )
}
