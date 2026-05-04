charts_tab_ui <- function() {
  tabPanel("charts",

    # Filter bar (same controls as before — chart-driving filters live here)
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
        )
    ),

    # Recruitment chart
    tonic_card(
      title = uiOutput("c1_title"),
      tools = div(class = "d-flex gap-2 align-items-center",
                  span(style = "font-size:11px;color:var(--muted);font-style:italic",
                       HTML("&#x1F4F7; Camera icon saves as PNG")),
                  downloadButton("dl_c1_data", HTML("&#x2B07; Download data"),
                                 style = "font-size:11px;background:#1B4F6B;color:#fff;border:none;",
                                 class = "dl-data-btn btn btn-sm")),
      withSpinner(echarts4rOutput("chart_recruit", height = "360px"),
                  type = 4, color = col_teal)
    ),

    div(class = "grid-2",
        tonic_card(
          title = "Sites recruiting — by month",
          tools = downloadButton("dl_c2_data", HTML("&#x2B07; Download"),
                                 style = "font-size:11px;background:#1B4F6B;color:#fff;border:none;",
                                 class = "dl-data-btn btn btn-sm"),
          withSpinner(echarts4rOutput("chart_sites_recruiting", height = "280px"),
                      type = 4, color = col_teal)
        ),
        tonic_card(
          title = "Recruitment rate vs monthly target (%)",
          tools = downloadButton("dl_c3_data", HTML("&#x2B07; Download"),
                                 style = "font-size:11px;background:#1B4F6B;color:#fff;border:none;",
                                 class = "dl-data-btn btn btn-sm"),
          withSpinner(echarts4rOutput("chart_rate", height = "280px"),
                      type = 4, color = col_teal)
        )
    ),
    div(style = "margin-bottom:15px"),

    # Heatmap
    tonic_card(
      title = "Monthly target achievement — site by site",
      tools = div(class = "d-flex gap-2 align-items-center flex-wrap",
                  span(style = "background:#DCFCE7;color:#166534;padding:2px 9px;border-radius:20px;font-size:10px;font-weight:600",
                       HTML("&check; Target met")),
                  span(style = "background:#FEF9C3;color:#854D0E;padding:2px 9px;border-radius:20px;font-size:10px;font-weight:600",
                       "~ Within 80%"),
                  span(style = "background:#FEE2E2;color:#991B1B;padding:2px 9px;border-radius:20px;font-size:10px;font-weight:600",
                       HTML("&cross; Below 80%")),
                  span(style = "background:#F8FAFC;color:#94A3B8;padding:2px 9px;border-radius:20px;font-size:10px;font-weight:600",
                       HTML("&middot; No data")),
                  downloadButton("dl_heatmap", HTML("&#x2B07; Download"),
                                 style = "font-size:11px;background:#1B4F6B;color:#fff;border:none;",
                                 class = "dl-data-btn btn btn-sm")),
      div(style = "overflow-x:auto", uiOutput("heatmap_ui"))
    ),

    # Per-site summary
    tonic_card(
      title = "Per-site summary",
      tools = downloadButton("dl_summary", HTML("&#x2B07; Download"),
                             style = "font-size:11px;background:#1B4F6B;color:#fff;border:none;",
                             class = "dl-data-btn btn btn-sm"),
      withSpinner(reactableOutput("summary_table"),
                  type = 4, color = col_teal)
    )
  )
}
