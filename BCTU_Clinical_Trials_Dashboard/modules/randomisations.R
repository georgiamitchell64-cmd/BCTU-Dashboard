randomisations_tab_ui <- function() {
                  tabPanel("randomisations",
                           tonic_card(
                             title = "Randomisation counter",
                             div(class = "info-box-tonic",
                                 HTML("&#x2139;&#xFE0F; Click <strong>+</strong> or <strong>&minus;</strong> to update each site's count. All changes are logged with a timestamp below.")),
                             withSpinner(reactableOutput("rand_table"), type = 4, color = col_teal),
                             tags$hr(style = "border:none;border-top:1px solid #EEF3F8;margin:18px 0"),
                             div(id = "backdate_form", class = "tm-only",
                                 div(style = "font-size:10px;font-weight:600;color:var(--navy);text-transform:uppercase;letter-spacing:.5px;margin-bottom:10px",
                                     "Add randomisation with specific date"),
                                 div(style = "display:flex;gap:12px;align-items:flex-end;flex-wrap:wrap",
                                     div(class = "fg",
                                         tags$label("Site"),
                                         selectInput("bd_site", label = NULL, choices = NULL)),
                                     div(class = "fg",
                                         tags$label("Date"),
                                         dateInput("bd_date", label = NULL, value = Sys.Date())),
                                     div(class = "fg",
                                         tags$label("Note (optional)"),
                                         textInput("bd_note", label = NULL,
                                                   placeholder = "e.g. late entry")),
                                     actionButton("add_backdate", "+ Add",
                                                  class = "btn btn-success",
                                                  style = "margin-bottom:16px")
                                 )
                             ),
                             tags$hr(style = "border:none;border-top:1px solid #EEF3F8;margin:18px 0"),
                             div(style = "font-size:10px;font-weight:600;color:var(--navy);text-transform:uppercase;letter-spacing:.5px;margin-bottom:10px",
                                 "Activity log"),
                             withSpinner(reactableOutput("log_table"), type = 4, color = col_teal)
                           )
                  )
}
