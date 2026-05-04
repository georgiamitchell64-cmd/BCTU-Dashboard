participants_tab_ui <- function() {
                  tabPanel("participants",

                           div(class = "section-heading",
                               HTML("&#x1F464; Patient-completed questionnaires")),
                           div(class = "info-box-tonic",
                               tags$strong("Patient Completed Questionaires - PROMS."),
                               " EQ-5D \u00b7 HRUQ \u00b7 PRODIGI \u00b7 QoR-15 \u00b7 VAS (Patient Satisfaction)",
                               tags$br(),
                               span(style = "font-size:11px",
                                    "Instruments: PRODIGI \u00b7 EQ-5D \u00b7 QoR-15 \u00b7 Patient Satisfaction \u00b7 HRUQ")),

                           layout_column_wrap(
                             width = "25%", gap = "13px",
                             vbox_html("fa-solid fa-clipboard", "Enrolled (baseline)", "n_p_baseline",
                                       "baseline event recorded",
                                       top_color = "#7C3AED",
                                       icon_bg = "background:#EDE9FE;color:#7C3AED"),
                             vbox_html("fa-solid fa-bed", "Discharge recorded", "n_p_discharge",
                                       "discharge event recorded",
                                       top_color = "#2563EB",
                                       icon_bg = "background:#DBEAFE;color:#2563EB"),
                             vbox_html("fa-solid fa-calendar", "Day 30 data", "n_p_d30",
                                       "day 30 event recorded"),
                             vbox_html("fa-solid fa-check", "Day 90 data", "n_p_d90",
                                       "day 90 event recorded",
                                       top_color = "#059669",
                                       icon_bg = "background:#D1FAE5;color:#059669")
                           ),
                           div(style = "margin-bottom:14px"),
                           # ── Customisable demographic breakdowns ──────────
                           tonic_card(
                             title = "Demographic breakdowns",
                             tools = tagList(
                               span(style = "font-size:11px;color:#64748B;
                                             font-style:italic;margin-right:10px;",
                                    textOutput("breakdowns_summary_txt", inline = TRUE)),
                               actionButton("configure_breakdowns",
                                            HTML("&#x2699; Configure"),
                                            class = "btn btn-sm tm-only",
                                            style = "background:#FFFFFF;color:#1B4F6B;
                                                     border:1px solid #DDE5EE;font-size:11px;
                                                     font-weight:500;")
                             ),
                             uiOutput("participant_breakdowns_ui")
                           ),
                           div(style="margin-bottom:14px"),
                           tonic_card(
                             title="Patient questionnaire completion \u2014 by participant",
                             tools=div(class="d-flex gap-2 align-items-center flex-wrap",
                                       span(style="font-size:11px;color:#64748B",
                                            HTML("<span class='c-complete'>&#10003;</span> Complete &nbsp;<span class='c-partial'>&#9679;</span> Started &nbsp;<span class='c-unverified'>?</span> Unverified &nbsp;<span class='c-none'>&mdash;</span> Not started")),
                                       downloadButton("dl_participants","Download",
                                                      style="font-size:11px;background:#1B4F6B;color:#fff;border:none;",
                                                      class="dl-data-btn btn btn-sm")),
                             div(class="d-flex gap-3 align-items-end flex-wrap",
                                 style="padding:0 0 10px;border-bottom:1px solid #EEF3F8;margin-bottom:10px",
                                 div(style="min-width:160px",
                                     tags$label("Site", class="form-label fw-semibold text-uppercase",
                                                style="font-size:10px;letter-spacing:.6px;color:#1B4F6B;margin-bottom:2px"),
                                     pickerInput("pq_site_filter", label=NULL, choices=NULL, selected=NULL,
                                                 multiple=TRUE,
                                                 options=list(`actions-box`=TRUE, `none-selected-text`="All sites",
                                                              title="All sites", width="180px"))
                                 ),
                                 div(style="min-width:140px",
                                     tags$label("Record ID", class="form-label fw-semibold text-uppercase",
                                                style="font-size:10px;letter-spacing:.6px;color:#1B4F6B;margin-bottom:2px"),
                                     textInput("pq_id_filter", label=NULL, placeholder="e.g. 1001", width="140px")
                                 ),
                                 div(style="margin-bottom:16px;font-size:11px;color:#94A3B8;font-style:italic",
                                     textOutput("pq_showing_label", inline=TRUE))
                             ),
                             div(class="comp-tbl", uiOutput("participants_ui"))
                           ),

                           div(style = "margin-bottom:6px"),
                           div(class = "section-heading amber",
                               HTML("&#x26A0; Site-reported safety & regulatory events")),
                           div(class = "info-box-amber",
                               tags$strong("Reported by site staff \u2014 not patient-completed."),
                               " SAEs \u00b7 Protocol deviations \u00b7 Withdrawals \u00b7 Pregnancy notifications/outcomes."),

                           layout_column_wrap(
                             width = "16.66%", gap = "13px",
                             vbox_html("fa-solid fa-triangle-exclamation", "SAEs", "n_s_saes", "",
                                       top_color = col_red,
                                       icon_bg = "background:#FEE2E2;color:#DC2626",
                                       delta_id = "delta_saes"),
                             vbox_html("fa-solid fa-circle-exclamation", "Deviations", "n_s_dev", "",
                                       top_color = col_amber,
                                       icon_bg = "background:#FEF3C7;color:#D97706"),
                             vbox_html("fa-solid fa-user-minus", "Withdrawals", "n_s_wd", "COS recorded",
                                       top_color = col_amber,
                                       icon_bg = "background:#FEF3C7;color:#D97706",
                                       delta_id = "delta_wd"),
                             vbox_html("fa-solid fa-person-pregnant", "Preg. notifications", "n_s_pn", "",
                                       top_color = "#7C3AED",
                                       icon_bg = "background:#EDE9FE;color:#7C3AED"),
                             vbox_html("fa-solid fa-baby", "Preg. outcomes", "n_s_po", "",
                                       top_color = "#2563EB",
                                       icon_bg = "background:#DBEAFE;color:#2563EB"),
                             vbox_html("fa-solid fa-hospital", "Sites with events", "n_s_sites",
                                       "have safety events")
                           ),
                           div(style = "margin-bottom:14px"),

                           div(class = "grid-2",
                               tonic_card(title = "Site-reported safety events \u2014 per site",
                                          amber = TRUE,
                                          withSpinner(reactableOutput("safety_table"),
                                                      type = 4, color = col_amber)),
                               tonic_card(title = "Withdrawals \u2014 by type (COS)",
                                          amber = TRUE,
                                          div(class = "info-box-amber", style = "font-size:11px",
                                              "1=Death \u00b7 2=No Operation \u00b7 3=Part withdrawal \u00b7 4=Complete withdrawal \u00b7 5=Lost to follow-up"),
                                          withSpinner(reactableOutput("withdrawal_table"),
                                                      type = 4, color = col_amber))
                           )
                  )
}
