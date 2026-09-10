randomisations_server <- function(input, output, session, state) {
  rv <- state$rv

  # Headings follow the trial's recruitment model: a cohort study recruits,
  # it does not randomise.
  output$rand_when_title    <- renderText(sprintf("When %s happen", recruit_term("noun", rv$trial_config)))
  output$rand_monthly_title <- renderText(sprintf("Monthly %s", recruit_term("noun", rv$trial_config)))
  # Everything on this tab reads the shared trial-health build (state$health),
  # so it follows the active work package and agrees with the Overview, Data
  # and Sites tabs. rv$log (manual activity log) stays trial-wide.
  health <- state$health

  # Empty-state chart: a single hidden series so e_charts() has data to
  # initialise and the loading spinner clears even when there's nothing to plot.
  .empty_chart <- function(msg) {
    data.frame(x = "—", y = 0) |>
      echarts4r::e_charts(x) |>
      echarts4r::e_bar(y, legend = FALSE,
                       itemStyle = list(color = "transparent")) |>
      echarts4r::e_title(msg, left = "center",
                         textStyle = list(color = "#8A8A8C", fontSize = 13)) |>
      echarts4r::e_x_axis(show = FALSE) |>
      echarts4r::e_y_axis(show = FALSE) |>
      echarts4r::e_legend(show = FALSE) |>
      echarts4r::e_tooltip(show = FALSE)
  }

  # ── KPIs, trajectory and punchcard ──────────────────────────────────────
  output$rand_health_kpis <- renderUI(th_rand_kpis(health()))

  output$rand_trajectory_ui <- renderUI({
    H <- health()
    if (is.null(H)) return(th_empty_note("No randomisations yet — upload a REDCap export."))
    th_trajectory_widget(H, "th-traj-rand")
  })

  output$rand_punchcard_ui <- renderUI({
    H <- health()
    if (is.null(H)) return(th_empty_note("No randomisations yet — upload a REDCap export."))
    th_punchcard_widget(H, "th-punch-rand")
  })

  # ── Monthly randomisations, stacked by when they happened ───────────────
  output$rand_monthly_chart <- renderEcharts4r({
    pt <- health()$patterns
    if (is.null(pt)) return(.empty_chart("No randomisation data — upload a REDCap CSV"))
    pal <- trial_palette(rv$trial_config)
    mc  <- pt$monthly_by_cat
    df  <- data.frame(
      month    = format(as.Date(paste0(pt$months, "-01")), "%b %y"),
      in_hours = mc[["In hours"]],
      ooh      = mc[["Weekday out of hours"]],
      weekend  = mc[["Weekend"]] + mc[["Bank holiday"]],
      untimed  = mc[["Time not recorded"]])
    ch <- df |>
      echarts4r::e_charts(month) |>
      echarts4r::e_bar(in_hours, name = "In hours", stack = "n",
                       itemStyle = list(color = pal[["primary"]])) |>
      echarts4r::e_bar(ooh, name = "Weekday out of hours", stack = "n",
                       itemStyle = list(color = pal[["secondary"]])) |>
      echarts4r::e_bar(weekend, name = "Weekends & bank holidays", stack = "n",
                       itemStyle = list(color = pal[["accent"]]))
    if (sum(df$untimed))
      ch <- ch |> echarts4r::e_bar(untimed, name = "Time not recorded", stack = "n",
                                   itemStyle = list(color = "#CFCFCF"))
    ch |>
      echarts4r::e_x_axis(axisLabel = list(fontSize = 10)) |>
      echarts4r::e_y_axis(minInterval = 1) |>
      echarts4r::e_tooltip(trigger = "axis") |>
      echarts4r::e_legend(bottom = 0, textStyle = list(fontSize = 11)) |>
      echarts4r::e_grid(left = 36, right = 12, top = 16, bottom = 56)
  })

  # ── Recruitment by site: pace, timing and gaps ──────────────────────────
  output$rand_site_patterns <- renderReactable({
    H <- health()
    S <- if (is.null(H)) NULL else H$sites
    if (is.null(S) || !nrow(S)) return(empty_reactable("No randomisations yet — upload a REDCap export."))
    bs <- H$patterns$by_site
    P  <- H$participants
    fl <- H$settings$flags
    pal <- trial_palette(rv$trial_config)
    recent_from <- .th_today_s(H$today) - 90 * .TH_DAY
    pick <- function(k) vapply(S$site, function(s) {
      v <- bs[[s]][[k]]
      if (is.null(v)) NA_real_ else as.numeric(v)
    }, numeric(1), USE.NAMES = FALSE)
    df <- data.frame(
      site = S$site, rag = S$rag, n = S$n,
      last90 = vapply(S$site, function(s) sum(P$site == s & P$rand > recent_from), numeric(1), USE.NAMES = FALSE),
      rate = S$rate, target = S$target_rate,
      in_hours = pick("in_hours_pct"), ooh = pick("ooh_pct"), weekend = pick("weekend_pct"),
      latency = S$latency, median_gap = S$median_gap, max_gap = S$max_gap, quiet = S$days_quiet,
      spark = vapply(S$site, function(s)
        as.character(th_sparkline(bs[[s]]$monthly %||% integer(), w = 110, h = 26, col = pal[["primary"]])),
        character(1), USE.NAMES = FALSE),
      stringsAsFactors = FALSE)
    pct  <- function(v) if (is.na(v)) "—" else sprintf("%.0f%%", v)
    days <- function(v) if (is.na(v)) "—" else sprintf("%.0f days", v)
    reactable(df,
      compact = TRUE, highlight = TRUE, defaultSorted = list(n = "desc"),
      onClick = htmlwidgets::JS("function(row) { Shiny.setInputValue('th_site_open', {site: row.values.site, n: Math.random()}, {priority: 'event'}); }"),
      rowStyle = list(cursor = "pointer"),
      defaultColDef = colDef(style = list(fontSize = "12.5px"), headerStyle = list(fontSize = "11px")),
      columns = list(
        site = colDef(name = "Site", minWidth = 150, style = list(fontWeight = 600, fontSize = "12.5px"),
                      cell = function(v, i) tagList(span(class = paste0("th-dot rag-", df$rag[i])), v)),
        rag = colDef(show = FALSE),
        n = colDef(name = "Randomised", width = 96, align = "right"),
        last90 = colDef(name = "Last 90 days", width = 100, align = "right"),
        rate = colDef(name = "Per month", width = 104, align = "right",
                      cell = function(v, i) if (is.na(v)) "—" else
                        tagList(sprintf("%.1f", v), span(class = "th-m-of", paste0(" / ", .th_num1(df$target[i])))),
                      style = function(v, i) if (!is.na(v) && v < 0.6 * df$target[i])
                        list(color = "#C20019", fontWeight = 700)),
        target = colDef(show = FALSE),
        in_hours = colDef(name = "In hours", width = 84, align = "right", cell = pct),
        ooh = colDef(name = "Out of hours", width = 100, align = "right", cell = pct),
        weekend = colDef(name = "Weekends", width = 88, align = "right", cell = pct),
        latency = colDef(name = "Open → first", width = 104, align = "right", cell = days),
        median_gap = colDef(name = "Typical gap", width = 96, align = "right", cell = days),
        max_gap = colDef(name = "Longest gap", width = 100, align = "right", cell = days),
        quiet = colDef(name = "Since last", width = 94, align = "right", cell = days,
                       style = function(v) if (!is.na(v) && v >= fl$quiet_amber)
                         list(color = if (v >= fl$quiet_red) "#C20019" else "#CF4527", fontWeight = 700)),
        spark = colDef(name = "Monthly", html = TRUE, width = 128, sortable = FALSE)))
  })

  # ── Activity log ────────────────────────────────────────────────────────
  output$log_table <- renderReactable({
    if (nrow(rv$log) == 0) return(empty_reactable("No activity yet"))
    rv$log %>% arrange(desc(timestamp)) %>%
      mutate(timestamp = format(timestamp, "%d %b %Y  %H:%M")) %>%
      select(Timestamp = timestamp, `Site ID` = site_id, Action = action, Note = note) %>%
      reactable(striped = TRUE, highlight = TRUE, compact = TRUE,
                defaultPageSize = 25, showPageSizeOptions = TRUE,
                pageSizeOptions = c(10, 25, 50, 100),
                defaultColDef = colDef(style = list(fontSize = "12.5px")),
                columns = list(
                  `Site ID` = colDef(cell = function(v) htmltools::span(class = "sid", v)),
                  Action = colDef(minWidth = 70, cell = function(v)
                    htmltools::HTML(if (v == "+1")
                      "<span style='color:#007838;font-weight:700'>+1</span>"
                      else "<span style='color:#C20019;font-weight:700'>−1</span>"))
                ))
  })
}
