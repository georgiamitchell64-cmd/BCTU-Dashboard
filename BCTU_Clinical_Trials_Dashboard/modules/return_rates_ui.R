# ── return_rates_ui.R ────────────────────────────────────────────────────────
#
# Two functions:
#   return_rates_tab_ui()  — outer wrapper, matches the app's tabPanel pattern
#   return_rates_ui(id)    — the actual module UI, called from the wrapper
#
# The body is drawn by the server (output$page) so a trial with neither a
# return-rate file nor a REDCap export gets one clear message instead of a
# column of empty panels. Styles: www/return_rates.css (.rt-*), on top of the
# trial-health sections and KPIs (.th-*).
#
# ─────────────────────────────────────────────────────────────────────────────

return_rates_tab_ui <- function() {
  tabPanel("returns_panel", value = "returns_panel",
    return_rates_ui("rr")
  )
}

return_rates_ui <- function(id) {
  ns <- NS(id)

  div(class = "rt-shell",

    # ── Title, where the figures come from, download ────────────────────
    div(class = "rt-top",
        div(class = "rt-top-l",
            tags$h2(class = "rt-title", "Return rates"),
            uiOutput(ns("source_line"))),
        div(class = "rt-top-r",
            uiOutput(ns("source_pick"), inline = TRUE),
            downloadButton(ns("dl_rates"), HTML("&darr; Download rates"),
                           class = "btn-ghost-sm"))),

    # ── Timepoint and form-type filters ─────────────────────────────────
    uiOutput(ns("filters")),

    # ── KPIs, timepoints, trend, sites, forms, participants ─────────────
    uiOutput(ns("page"))
  )
}
