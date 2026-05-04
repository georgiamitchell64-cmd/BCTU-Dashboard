reports_tab_ui <- function() {
                  tabPanel("reports",

                           # Filter bar
                           div(class = "fbar",
                               div(class = "fg",
                                   tags$label("Sites"),
                                   pickerInput("rpt_sites", label = NULL, choices = NULL, selected = NULL,
                                               multiple = TRUE,
                                               options = list(`actions-box` = TRUE,
                                                              `none-selected-text` = "All sites",
                                                              title = "All sites", width = "160px"))
                               ),
                               div(class = "fg",
                                   tags$label("Date range"),
                                   dateRangeInput("rpt_dates", label = NULL,
                                                  start = floor_date(Sys.Date() %m-% months(11), "month"),
                                                  end = Sys.Date(), format = "M yyyy",
                                                  startview = "year", width = "220px")
                               ),
                               div(class = "fg",
                                   tags$label("View"),
                                   radioGroupButtons("rpt_view", label = NULL,
                                                     choices = c("Monthly" = "monthly",
                                                                 "Cumulative" = "cumulative"),
                                                     selected = "monthly", status = "primary", size = "sm")
                               ),
                               div(class = "fg",
                                   tags$label("Breakdown"),
                                   radioGroupButtons("rpt_breakdown", label = NULL,
                                                     choices = c("All sites" = "overall",
                                                                 "Per site" = "per_site"),
                                                     selected = "overall", status = "primary", size = "sm")
                               ),
                               div(class = "fg", style = "margin-left:auto",
                                   tags$label(HTML("&#x1F4CB; Report")),
                                   div(
                                     downloadButton("download_report",
                                                    HTML("&#x2B07; Generate report"),
                                                    style = "background:#2EC4A5;color:#fff;border:none;font-family:'Outfit',sans-serif;font-size:12px;font-weight:600;padding:7px 14px;border-radius:5px;",
                                                    class = "btn btn-sm dl-data-btn")
                                   )
                               )
                           ),

                           # ── New Report Builder (Stage 10) ────────────────
                           tonic_card(
                             title = "Report builder",
                             tools = tagList(
                               span(style = "font-size:11px;color:#64748B;
                                             font-style:italic;margin-right:10px;",
                                    "Customise sections + order, save as a template"),
                               downloadButton("rb_download",
                                 HTML("&#x2B07; Generate"),
                                 class = "btn btn-sm",
                                 style = "background:#6366F1;color:#fff;border:none;
                                          font-size:11px;font-weight:600;padding:6px 12px;
                                          border-radius:6px;")
                             ),
                             div(style = "display:grid;grid-template-columns:1fr 1.4fr;
                                          gap:18px;",
                                 div(uiOutput("rb_template_picker_ui"),
                                     div(style = "margin-top:14px;",
                                         tags$label(style = "font-size:11px;font-weight:600;
                                                              color:#1B4F6B;text-transform:uppercase;
                                                              letter-spacing:.5px;",
                                                    "Free text"),
                                         textAreaInput("rb_custom_text", label = NULL,
                                                       placeholder = "Notes for the Custom Text section…",
                                                       rows = 3, width = "100%"),
                                         textAreaInput("rb_next_period", label = NULL,
                                                       placeholder = "Plans for next reporting period…",
                                                       rows = 3, width = "100%"))),
                                 div(uiOutput("rb_sections_ui"))
                             )
                           ),
                           div(style = "margin-bottom:14px"),

                           # Report configuration card — report type selector + TSC panel
                           tonic_card(
                             title = "Classic report options",
                             div(
                               style = "padding:4px 2px;",

                               # Row: type + scope + extras
                               div(
                                 style = "display:flex; flex-wrap:wrap; gap:24px; align-items:flex-start;",

                                 div(
                                   style = "min-width:280px;",
                                   tags$label(style = "font-size:11px;font-weight:600;color:#1B4F6B;text-transform:uppercase;letter-spacing:.5px;", "Report type"),
                                   radioGroupButtons("report_type", label = NULL,
                                                     choices = c("TMG"  = "TMG",
                                                                 "iTMG" = "iTMG",
                                                                 "TSC"  = "TSC"),
                                                     selected = "TMG", status = "primary", size = "sm")
                                 ),

                                 div(
                                   style = "min-width:240px;",
                                   tags$label(style = "font-size:11px;font-weight:600;color:#1B4F6B;text-transform:uppercase;letter-spacing:.5px;", "Data scope"),
                                   div(style = "padding-top:4px;",
                                     checkboxInput("rpt_full_trial",
                                                    label = "Full trial data (ignore filters)",
                                                    value = FALSE)
                                   )
                                 ),

                                 div(
                                   style = "min-width:240px;",
                                   tags$label(style = "font-size:11px;font-weight:600;color:#1B4F6B;text-transform:uppercase;letter-spacing:.5px;", "Extras"),
                                   div(style = "padding-top:4px;",
                                     checkboxInput("include_withdrawn",
                                                    label = "Include withdrawn participants",
                                                    value = FALSE),
                                     checkboxInput("report_appendix",
                                                    label = "Include appendix (TMG/iTMG)",
                                                    value = FALSE),
                                     tags$label(style = "font-size:11px;font-weight:600;color:#1B4F6B;text-transform:uppercase;letter-spacing:.5px;margin-top:10px;display:block;",
                                                "Completeness table style"),
                                     radioButtons("completeness_style",
                                                  label = NULL,
                                                  choices = c(
                                                    "Heatmap (by event)"          = "heatmap",
                                                    "Flat table (protocol forms)" = "flat",
                                                    "Both"                        = "both"
                                                  ),
                                                  selected = "heatmap",
                                                  inline   = FALSE)
                                   )
                                 )
                               ),

                               # TSC-only side panel
                               conditionalPanel(
                                 condition = "input.report_type == 'TSC'",

                                 tags$hr(style = "margin:14px 0;"),

                                 div(
                                   style = "background:#F8FAFB; border-left:3px solid #1B4F6B; border-radius:0 4px 4px 0; padding:12px 16px;",
                                   h5("TSC report details",
                                      style = "margin-top:0; color:#1B4F6B; font-family:Outfit,sans-serif;"),

                                   tabsetPanel(
                                     id = "tsc_tabs",

                                     tabPanel("Meeting details",
                                       br(),
                                       fluidRow(
                                         column(6, dateInput("meeting_date", "Date of TSC meeting",
                                                              value = Sys.Date())),
                                         column(6, textInput("protocol_version", "Protocol version",
                                                              value = "", placeholder = "e.g. v2.0"))
                                       ),
                                       fluidRow(
                                         column(6, textInput("prepared_by", "Report prepared by",
                                                              value = "", placeholder = "Name")),
                                         column(6, textInput("reviewed_by", "Report reviewed by",
                                                              value = "", placeholder = "Name"))
                                       )
                                     ),

                                     tabPanel("Trial summary",
                                       br(),
                                       helpText("Pre-filled from TONIC protocol \u2014 review and edit before generating if needed."),
                                       textAreaInput("ts_objectives", "Objectives",
                                         rows = 6, resize = "vertical",
                                         value = paste(
                                           "Primary clinical objective: to determine whether early parenteral nutrition (PN) in patients undergoing emergency laparotomy/laparoscopy reduces in-hospital post-operative complications assessed at hospital discharge as compared to usual nutritional care, measured using the Comprehensive Complication Index (CCI).",
                                           "",
                                           "Secondary objectives: to assess the impact of early PN on post-operative complications, activities of daily living, muscle function (sit-to-stand test), patient-reported outcomes (PRO-diGI, QoR-15, EQ-5D), SAE rates, unplanned readmissions, hospital length of stay, discharge destination, participant satisfaction, and to report PN use (duration and calories) and pathway metrics (randomisation, line insertion, operation, time to starting PN) up to 90 days post-operation.",
                                           "",
                                           "Economic objectives: primary \u2014 economic evaluation alongside the trial, assessing cost-effectiveness over 90 days from an NHS and personal social service cost perspective. Secondary \u2014 cost-effectiveness from NHS, personal social service and societal perspectives; lifetime-horizon extrapolation from an NHS perspective; Expected Value of Information analysis.",
                                           sep = "\n"
                                         )),
                                       textAreaInput("ts_design", "Trial design",
                                         rows = 3, resize = "vertical",
                                         value = "Multi-centre, two-arm, parallel-group, superiority, individual participant randomised controlled trial with 1:1 allocation to early parenteral nutrition (PN) or standard nutritional care. Includes an internal pilot (first 6 months of recruitment) and a full economic evaluation."),
                                       textAreaInput("ts_eligibility", "Eligibility criteria",
                                         rows = 2, resize = "vertical",
                                         value = "Adults undergoing National Emergency Laparotomy Audit (NELA) eligible emergency laparotomy or laparoscopy."),
                                       textAreaInput("ts_interventions", "Interventions",
                                         rows = 3, resize = "vertical",
                                         value = paste(
                                           "Intervention arm: early parenteral nutrition (PN) started within 48 hours of emergency laparotomy/laparoscopy.",
                                           "Control arm: standard nutritional care.",
                                           sep = "\n"
                                         )),
                                       textAreaInput("ts_primary_outcome", "Primary outcome measure",
                                         rows = 3, resize = "vertical",
                                         value = "In-hospital post-operative complications assessed at hospital discharge, measured using the Comprehensive Complication Index (CCI)."),
                                       textAreaInput("ts_secondary_outcomes", "Secondary outcome measures",
                                         rows = 8, resize = "vertical",
                                         value = paste(
                                           "Post-operative complications up to 90 days post-operation",
                                           "Activities of daily living up to 90 days post-operation (Barthel Index)",
                                           "Muscle function (sit-to-stand test) at hospital discharge",
                                           "Patient-reported outcomes (PRO-diGI, QoR-15, EQ-5D) up to 90 days post-operation",
                                           "SAE rate related to early PN up to 89 days post-operation",
                                           "Unplanned readmissions following discharge up to 90 days post-operation",
                                           "Hospital length of stay during index admission",
                                           "Discharge destination following index admission",
                                           "Participant satisfaction with treatment",
                                           "PN use in both arms (duration and calories administered)",
                                           "Use of any nutritional intervention in either trial arm",
                                           "Pathway metrics: randomisation, line insertion, operation, and time to starting PN",
                                           sep = "\n"
                                         ))
                                     ),

                                     tabPanel("Funder update",
                                       br(),
                                       helpText("Optional narrative for funder extension request / update. Leave blank to omit the section."),
                                       textAreaInput("funder_update", "Funder update narrative",
                                                      rows = 6, resize = "vertical", width = "100%")
                                     ),

                                     tabPanel("Amendments",
                                       br(),
                                       h6("Substantial amendments",
                                          style = "color:#1B4F6B; font-family:Outfit,sans-serif;"),
                                       uiOutput("amd_sub_ui"),
                                       div(style = "display:flex; gap:6px; margin-top:4px;",
                                         actionButton("amd_sub_add",    "+ Add substantial amendment",
                                                       class = "btn-sm dl-data-btn",
                                                       style = "background:#2EC4A5;color:#fff;border:none;"),
                                         actionButton("amd_sub_remove", HTML("&minus; Remove last"),
                                                       class = "btn-sm dl-data-btn",
                                                       style = "background:#94A3B8;color:#fff;border:none;")
                                       ),
                                       br(),
                                       h6("Non-substantial amendments",
                                          style = "color:#1B4F6B; font-family:Outfit,sans-serif;"),
                                       uiOutput("amd_nonsub_ui"),
                                       div(style = "display:flex; gap:6px; margin-top:4px;",
                                         actionButton("amd_nonsub_add",    "+ Add non-substantial amendment",
                                                       class = "btn-sm dl-data-btn",
                                                       style = "background:#2EC4A5;color:#fff;border:none;"),
                                         actionButton("amd_nonsub_remove", HTML("&minus; Remove last"),
                                                       class = "btn-sm dl-data-btn",
                                                       style = "background:#94A3B8;color:#fff;border:none;")
                                       )
                                     ),

                                     tabPanel("Custom sections",
                                       br(),
                                       helpText(HTML(paste(
                                         "Add narrative sections that aren't in the data (e.g. PI Associate Scheme updates,",
                                         "sponsor commentary, operational notes). Each slots into a fixed position in the report."
                                       ))),
                                       uiOutput("cs_ui"),
                                       div(style = "display:flex; gap:6px; margin-top:4px;",
                                         actionButton("cs_add",    "+ Add custom section",
                                                       class = "btn-sm dl-data-btn",
                                                       style = "background:#2EC4A5;color:#fff;border:none;"),
                                         actionButton("cs_remove", HTML("&minus; Remove last"),
                                                       class = "btn-sm dl-data-btn",
                                                       style = "background:#94A3B8;color:#fff;border:none;")
                                       )
                                     )
                                   )
                                 )
                               )
                             )
                           ),

                           # Chart 1
                           # \u2500\u2500 Amendments editor (used by the Amendments report section) \u2500\u2500
                           tonic_card(
                             title = "Amendments",
                             tools = actionButton("amend_add",
                                                  HTML("&#x2795; Add amendment"),
                                                  class = "btn btn-sm",
                                                  style = "background:#6366F1;color:#fff;
                                                           border:none;font-size:11px;
                                                           font-weight:500;padding:6px 12px;"),
                             div(style = "font-size:12px;color:#64748B;margin-bottom:10px;",
                                 "Track substantial and non-substantial amendments. The
                                  list below feeds the Amendments section of any report
                                  template that includes it."),
                             uiOutput("amendments_list_ui")
                           )
                  )
}
