sites_tab_ui <- function() {
  tabPanel("sites",
    div(class = "sites-shell",
      tags$section(class = "sites-canvas",

        # ── Toolbar ────────────────────────────────────────────────────
        div(class = "sites-toolbar",
            div(class = "st-left",
                div(class = "searchwrap",
                    span(class = "sw-ic", HTML("&#x2315;")),
                    tags$input(type = "text", class = "searchinput",
                               id = "sites_search_input",
                               placeholder = "Search sites, PIs, codes…")),
                div(class = "fchips",
                    tags$button(class = "fchip", `data-status` = "Recruiting",
                                span(class = "fchip-dot", style = "background:#10B981;"),
                                "Recruiting"),
                    tags$button(class = "fchip", `data-status` = "Set-up",
                                span(class = "fchip-dot", style = "background:#94A3B8;"),
                                "Set-up"),
                    tags$button(class = "fchip", `data-status` = "Paused",
                                span(class = "fchip-dot", style = "background:#F59E0B;"),
                                "Paused"),
                    tags$button(class = "fchip", `data-status` = "Closed",
                                span(class = "fchip-dot", style = "background:#64748B;"),
                                "Closed")
                )
            ),
            div(class = "st-right",
                actionButton("bulk_add_sites", HTML("&#x2191; Import CSV"),
                             class = "btn-ghost-sm"),
                actionButton("toggle_add_form", "+ Add site",
                             class = "btn-primary-sm")
            )
        ),

        # ── Summary stats ──────────────────────────────────────────────
        uiOutput("sites_summary_stats"),

        # ── Add site form (hidden, admin-only) ─────────────────────────
        div(id = "add_site_box", class = "tm-only",
          tags$section(class = "tonic-card ov-card",
            div(class = "ov-card-head",
                tags$h2("Add site manually"),
                span(class = "ov-card-tool-note",
                     "Sites are auto-created from REDCap CSV. Use this for manual entry.")),
            div(class = "ov-card-body",

              # Row 1 — basic identity
              div(class = "form-grid g5",
                div(class = "form-field",
                    textInput("ns_id", "Site ID", placeholder = "Auto-generated")),
                div(class = "form-field", style = "grid-column:span 2",
                  tags$label("Site / Trust name"),
                  selectizeInput("ns_name", label = NULL,
                    choices  = c("", get_hospital_names()),
                    selected = "",
                    width    = "100%",
                    options  = list(create = TRUE,
                      placeholder  = "Type or select hospital…",
                      createOnBlur = TRUE
                    ))),
                div(class = "form-field",
                    textInput("ns_city", "City", placeholder = "e.g. Leeds")),
                div(class = "form-field",
                    textInput("ns_region", "Region / State", placeholder = "e.g. Yorkshire"))
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
                div(class = "form-field",
                    dateInput("ns_open", "Open date", value = NULL)),
                div(class = "form-field",
                  tags$label("SIV booked"),
                  div(style = "padding-top:9px",
                    checkboxInput("ns_siv_booked", label = "Yes", value = FALSE))),
                div(class = "form-field",
                    dateInput("ns_siv_date", "SIV date", value = NULL))
              ),

              # Row 3 — targets
              div(class = "form-grid g3",
                div(class = "form-field",
                    numericInput("ns_mo_tgt", "Monthly target", value = 2, min = 0)),
                div(class = "form-field",
                    numericInput("ns_tgt", "Overall target", value = 42, min = 0)),
                div(class = "form-field",
                    numericInput("ns_rand", "Already randomised", value = 0, min = 0))
              ),

              div(style = "display:flex;gap:10px;flex-wrap:wrap;margin-top:10px;",
                actionButton("add_site", "+ Add site", class = "btn-primary-sm"),
                actionButton("delete_site", HTML("&#x1F5D1; Delete selected"),
                             class = "btn-danger-sm")
              )
            )
          )
        ),

        # ── Sites table ──────────────────────────────────────────────
        div(class = "list-section",
          div(class = "list-head",
              tags$h3("Sites"),
              div(class = "list-meta", "Click any field to edit inline")),
          tags$section(class = "tonic-card ov-card",
            div(class = "ov-card-body p0",
                div(class = "sitestbl-wrap",
                    withSpinner(reactableOutput("manage_table"),
                                type = 4, color = col_teal)),
                div(style = "padding:8px 14px;font-size:11px;color:var(--ov-muted);
                             font-style:italic;border-top:1px solid var(--ov-line2);",
                    "Enter a City and Country to place the site on the map. For non-UK sites the location is looked up via OpenStreetMap.")
            )
          )
        )
      )
    )
  )
}
