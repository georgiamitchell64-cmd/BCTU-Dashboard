overview_tab_ui <- function() {
                  tabPanel("overview",

                           # Meeting date bar
                           div(class = "meeting-bar",
                               span(style = "font-size:11px;font-weight:600;color:var(--navy);text-transform:uppercase;letter-spacing:.6px;white-space:nowrap",
                                    HTML("&#x1F4C5; Last meeting date")),
                               dateInput("last_meeting", label = NULL, value = Sys.Date() %m-% months(1),
                                         width = "155px", format = "d M yyyy"),
                               span(style = "font-size:11px;color:var(--muted);font-style:italic",
                                    textOutput("meeting_label_txt", inline = TRUE)),
                               span(style = "font-size:11px;color:var(--muted);margin-left:auto",
                                    HTML("&Delta; = change since last meeting"))
                           ),

                           # Value boxes (3)
                           layout_columns(
                             col_widths = c(4, 4, 4), gap = "13px",
                             vbox_html("fa-solid fa-hospital", "Sites", "n_sites",
                                       "active in dashboard",
                                       delta_id="delta_sites"),
                             vbox_html("fa-solid fa-users", "Total Randomised", "n_rand",
                                       textOutput("n_rand_sub", inline=TRUE),
                                       delta_id="delta_rand"),
                             vbox_html("fa-solid fa-bullseye", "Of Trial Target", "n_pct",
                                       paste0("of ",TRIAL_TARGET," participants"),
                                       top_color="#F59E0B",
                                       icon_bg="background:#FEF3C7;color:#D97706",
                                       delta_id="delta_pct")
                           ),
                           div(style = "margin-bottom:14px"),

                           # ── Recruitment projection (chart + sliders) ────
                           tonic_card(
                             title = "Recruitment projection vs protocol plan",
                             tools = tagList(
                               span(style = "font-size:11px;color:var(--muted);margin-right:10px;",
                                    textOutput("proj_completion_tool", inline = TRUE)),
                               actionButton("toggle_proj_settings", "Adjust assumptions",
                                            icon  = icon("sliders"),
                                            class = "btn btn-sm btn-outline-secondary")
                             ),

                             shinyjs::hidden(
                               div(id = "proj_settings_panel",
                                   style = "background:#F8FAFD;border:1px solid #DDE5EE;
                                            border-radius:6px;padding:14px 16px;margin-bottom:12px;",

                                   div(style = "display:flex;justify-content:space-between;align-items:baseline;margin-bottom:10px;",
                                       div(style = "font-size:12px;font-weight:600;color:var(--navy);text-transform:uppercase;letter-spacing:.5px;",
                                           "Projection assumptions"),
                                       actionButton("reset_proj_settings", "Reset to defaults",
                                                    class = "btn btn-sm btn-link",
                                                    style = "color:var(--teal);padding:0;")
                                   ),

                                   # Row 1: per-site per-month rate
                                   div(style = "font-size:11px;font-weight:600;color:var(--navy);margin:8px 0 4px;",
                                       "Per-site per-month recruitment rate"),
                                   div(style = "display:grid;grid-template-columns:1fr 1fr 1fr;gap:14px;",
                                       div(sliderInput("proj_rate_pessimistic", "Pessimistic",
                                                       min = 0, max = 10, value = 2, step = 0.5, width = "100%")),
                                       div(sliderInput("proj_rate_central",    "Central",
                                                       min = 0, max = 10, value = 3, step = 0.5, width = "100%")),
                                       div(sliderInput("proj_rate_optimistic", "Optimistic",
                                                       min = 0, max = 10, value = 4, step = 0.5, width = "100%"))
                                   ),

                                   # Row 2: new sites per month
                                   div(style = "font-size:11px;font-weight:600;color:var(--navy);margin:14px 0 4px;",
                                       "New sites opening per month (until target reached)"),
                                   div(style = "display:grid;grid-template-columns:1fr 1fr 1fr;gap:14px;",
                                       div(sliderInput("proj_sites_pessimistic", "Pessimistic",
                                                       min = 0, max = 6, value = 1, step = 0.5, width = "100%")),
                                       div(sliderInput("proj_sites_central",    "Central",
                                                       min = 0, max = 6, value = 2, step = 0.5, width = "100%")),
                                       div(sliderInput("proj_sites_optimistic", "Optimistic",
                                                       min = 0, max = 6, value = 3, step = 0.5, width = "100%"))
                                   ),

                                   # Row 3: target site count
                                   div(style = "font-size:11px;font-weight:600;color:var(--navy);margin:14px 0 4px;",
                                       "Total sites when fully ramped"),
                                   div(style = "max-width:340px;",
                                       sliderInput("proj_target_sites", NULL,
                                                   min = 1, max = 60, value = 24, step = 1, width = "100%")
                                   ),

                                   div(style = "font-size:11px;color:var(--muted);font-style:italic;margin-top:10px;",
                                       "Changes save automatically. Defaults come from live recruitment data once \u22653 months are available.")
                               )
                             ),

                             withSpinner(
                               echarts4rOutput("proj_chart", height = "360px"),
                               type = 4, color = col_teal
                             ),

                             div(style = "padding:10px 4px 0;font-size:11px;color:var(--muted);font-style:italic",
                                 textOutput("proj_note", inline = TRUE))
                           ),
                           div(style = "margin-bottom:14px"),

                           # Map + pipeline
                           div(class = "grid-8-4",
                               tonic_card(
                                 title = "UK Site Map",
                                 tools = span(style = "font-size:11px;color:var(--muted)",
                                              "Bubble size = randomisations \u00b7 Add city to place on map"),
                                 leafletOutput("site_map", height = 440)
                               ),
                               tonic_card(
                                 title = "Site pipeline",
                                 withSpinner(echarts4rOutput("pipeline_chart", height = "230px"),
                                             type = 4, color = col_teal),
                                 tags$hr(style = "border:none;border-top:1px solid #EEF3F8;margin:14px 0"),
                                 div(style = "font-size:10px;font-weight:600;color:var(--navy);text-transform:uppercase;letter-spacing:.5px;margin-bottom:10px",
                                     "Top recruiting sites"),
                                 withSpinner(echarts4rOutput("top_sites_chart", height = "180px"),
                                             type = 4, color = col_teal)
                               )
                           ),
                           div(style = "margin-bottom:14px"),

                           # Sites table
                           tonic_card(
                             title = "All sites \u2014 current status",
                             tools = textInput("site_search_ov", label = NULL,
                                               placeholder = "Filter\u2026", width = "160px"),
                             withSpinner(reactableOutput("overview_table"),
                                         type = 4, color = col_teal)
                           )
                  )
}
