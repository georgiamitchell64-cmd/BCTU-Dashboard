sites_tab_ui <- function() {
  tabPanel("sites",
    div(id = "add_site_box", class = "tm-only",
      tonic_card(title = "Add site manually",
        div(class = "info-box-tonic",
          HTML("&#x2139; Sites are auto-created from the site_name column when you load a REDCap CSV. Use this form to add sites manually or before the first load. Enter the City name and Country to place the site on the map.")),

        # Row 1 — basic identity
        div(class = "form-grid g5",
          div(class = "form-field", textInput("ns_id", "Site ID", placeholder = "Auto-generated")),
          div(class = "form-field", style = "grid-column:span 2",
            tags$label("Site / Trust name"),
            selectizeInput("ns_name", label = NULL,
              choices  = c("", get_hospital_names()),
              selected = "",
              width    = "100%",
              options  = list(create = TRUE,
                placeholder  = "Type or select hospital\u2026",
                createOnBlur = TRUE
              ))),
          div(class = "form-field", textInput("ns_city", "City", placeholder = "e.g. Leeds")),
          div(class = "form-field", textInput("ns_region", "Region / State", placeholder = "e.g. Yorkshire"))
        ),

        # Row 2 — country, status, dates
        div(class = "form-grid g5",
          div(class = "form-field",
            selectizeInput("ns_country", "Country",
              choices  = site_countries,
              selected = "United Kingdom",
              options  = list(create = TRUE))),
          div(class = "form-field",
            selectInput("ns_status", "Status",
              choices = c("Identified", "Set-up", "Open", "Recruiting", "Closed"))),
          div(class = "form-field", dateInput("ns_open", "Open date", value = NULL)),
          div(class = "form-field",
            tags$label("SIV booked"),
            div(style = "padding-top:9px",
              checkboxInput("ns_siv_booked", label = "Yes", value = FALSE))),
          div(class = "form-field", dateInput("ns_siv_date", "SIV date", value = NULL))
        ),

        # Row 3 — targets
        div(class = "form-grid g3",
          div(class = "form-field", numericInput("ns_mo_tgt", "Monthly target", value = 2, min = 0)),
          div(class = "form-field", numericInput("ns_tgt", "Overall target", value = 42, min = 0)),
          div(class = "form-field", numericInput("ns_rand", "Already randomised", value = 0, min = 0))
        ),

        div(style = "display:flex;gap:10px;flex-wrap:wrap",
          actionButton("add_site", HTML("+ Add site"),
            class = "btn btn-success"),
          actionButton("bulk_add_sites", HTML("&#x1F4CB; Bulk add…"),
            class = "btn",
            style = "background:#6366F1;color:#fff;border:none;font-weight:500;"),
          actionButton("delete_site", HTML("&#x1F5D1; Delete selected"),
            class = "btn btn-danger")
        )
      )
    ),
    tonic_card(
      title = "All sites \u2014 click any field to edit inline",
      withSpinner(reactableOutput("manage_table"), type = 4, color = col_teal),
      div(style = "padding:8px 12px;font-size:11px;color:var(--muted);font-style:italic;border-top:1px solid #EEF3F8",
        "Enter a City and Country to place the site on the map. For non-UK sites the location is looked up via OpenStreetMap.")
    )
  )
}
