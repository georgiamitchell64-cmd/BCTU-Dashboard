upload_tab_ui <- function() {
                  tabPanel("upload",
                           div(class = "grid-2",
                               tonic_card(title = "REDCap data folder",
                                          div(class = "info-box-tonic",
                                              HTML("&#x1F4C2; <strong>How to update the data:</strong>"),
                                              tags$ol(style = "margin:6px 0 0 16px;line-height:1.9",
                                                      tags$li("Export from REDCap \u2192 Reports \u2192 Export Data \u2192 CSV"),
                                                      tags$li("Save the CSV into the data folder shown below"),
                                                      tags$li("Click ", tags$strong("Load latest file"))
                                              ),
                                              div(style = "margin-top:4px;font-size:11px",
                                                  "The app always loads the most recently modified CSV in the folder.")),
                                          withSpinner(uiOutput("data_folder_status"), type = 4, color = col_teal),
                                          div(id = "refresh_btn_wrap", class = "tm-only",
                                              div(style = "display:flex;gap:10px;margin-top:14px",
                                                  actionButton("refresh_data", HTML("&#x1F504; Load latest file"),
                                                               class = "btn btn-success", style = "flex:1"),
                                                  actionButton("refresh_data_all", HTML("&#x21BA; Reload & reset sites"),
                                                               class = "btn btn-outline-secondary", style = "flex:1")
                                              )
                                          ),
                                          tags$hr(style = "border:none;border-top:1px solid #EEF3F8;margin:14px 0"),
                                          div(style = "font-size:10px;font-weight:600;color:var(--navy);text-transform:uppercase;letter-spacing:.5px;margin-bottom:8px",
                                              "Files in folder"),
                                          uiOutput("folder_files_ui")
                               ),
                               tonic_card(title = "Export data",
                                          div(class = "info-box-tonic",
                                              "Data downloads: Trial Managers only.",
                                              tags$br(), "Graph exports: CI, Team Leader, and Trial Manager."),
                                          div(style = "display:flex;flex-direction:column;gap:8px",
                                              downloadButton("dl_sites_xlsx", HTML("&#x2B07; Site data (.xlsx)"),
                                                             style = "font-family:'Outfit',sans-serif;background:#1B4F6B;color:#fff;border:none;text-align:center;justify-content:center",
                                                             class = "dl-data-btn"),
                                              downloadButton("dl_monthly_xlsx", HTML("&#x2B07; Monthly recruitment (.xlsx)"),
                                                             style = "font-family:'Outfit',sans-serif;background:#0FA88E;color:#fff;border:none;text-align:center;justify-content:center",
                                                             class = "dl-data-btn"),
                                              downloadButton("dl_log_xlsx", HTML("&#x2B07; Activity log (.xlsx)"),
                                                             style = "font-family:'Outfit',sans-serif;background:#fff;color:#475569;border:1px solid #DDE5EE;text-align:center;justify-content:center",
                                                             class = "dl-data-btn"),
                                              downloadButton("dl_participants_xlsx", HTML("&#x2B07; Participant data (.xlsx)"),
                                                             style = "font-family:'Outfit',sans-serif;background:#fff;color:#475569;border:1px solid #DDE5EE;text-align:center;justify-content:center",
                                                             class = "dl-data-btn")
                                          )
                               )
                           )
                  )
}
