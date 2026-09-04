randomisations_server <- function(input, output, session, state) {
  rv <- state$rv
  # WP-scoped views — randomisation charts, KPIs and the site table follow the
  # active work package. rv$log (manual activity log) stays trial-wide.
  redcap_wp <- state$redcap_wp
  sites_wp  <- state$sites_wp

  # Card headings follow the trial's recruitment model: a cohort study
  # recruits, it does not randomise.
  output$rand_monthly_title <- renderText({
    sprintf("Monthly %s", recruit_term("noun", rv$trial_config))
  })
  output$rand_cumulative_title <- renderText({
    sprintf("Cumulative %s", recruit_term("noun", rv$trial_config))
  })
  output$rand_site_title <- renderText({
    sprintf("Per-site %s", recruit_term("noun", rv$trial_config))
  })

  # Resolve the randomisation-date column from the trial config, with the
  # same fallbacks the portfolio-review chart uses. Returns NA when nothing
  # in the loaded CSV looks like a randomisation date.
  .rand_col <- function(df) {
    if (is.null(df) || !ncol(df)) return(NA_character_)
    cands <- character(0)
    cfg_col <- tryCatch(fld("randomisation_datetime", default = "rand_dttm_s"),
                        error = function(e) NULL)
    if (!is.null(cfg_col)) cands <- c(cands, cfg_col)
    cands <- c(cands, "rand_dttm_s", "rand_dttm", "rand_date",
               "randomisation_date", "randomization_date",
               "date_randomised", "date_of_randomisation")
    for (c in cands) if (c %in% names(df)) return(c)
    nm <- tolower(names(df))
    hit <- which(grepl("rand", nm) & grepl("dt|date", nm))
    if (length(hit)) return(names(df)[hit[1]])
    NA_character_
  }

  # All randomisation dates from the latest REDCap export. Returns Date(0)
  # if no data is loaded — never errors.
  rand_dates <- reactive({
    df <- redcap_wp()
    if (is.null(df) || !nrow(df)) return(as.Date(character(0)))
    col <- .rand_col(df)
    if (is.na(col)) return(as.Date(character(0)))
    d <- suppressWarnings(as.Date(df[[col]]))
    d[!is.na(d)]
  })

  # Why there is nothing to plot. "Upload a REDCap CSV" is right when nothing
  # is loaded, and actively misleading when an export is loaded but does not
  # carry the field the recruitment date is mapped to: every count reads 0 and
  # the screen blames the upload. Name the field instead.
  .no_dates_msg <- function() {
    df <- redcap_wp()
    if (is.null(df) || !nrow(df))
      return(recruit_term("no_data", rv$trial_config))
    if (is.na(.rand_col(df))) {
      cand <- as.character(unlist(
        (rv$trial_config$redcap_fields %||% list())$randomisation_datetime %||%
          character(0)))
      return(sprintf(
        "Export loaded, but it has no %s date field (looked for %s). Map it in Settings \u2192 Field mapping.",
        recruit_term("event", rv$trial_config),
        if (length(cand)) paste(cand, collapse = " or ") else "a registration date"))
    }
    sprintf("No usable %s dates in the loaded export.",
            recruit_term("event", rv$trial_config))
  }

  # Empty-state chart: a single hidden series so e_charts() has data to
  # initialise and the loading spinner clears even when there's nothing to plot.
  .empty_chart <- function(msg) {
    data.frame(x = "—", y = 0) |>
      echarts4r::e_charts(x) |>
      echarts4r::e_bar(y, legend = FALSE,
                       itemStyle = list(color = "transparent")) |>
      echarts4r::e_title(msg, left = "center",
                         textStyle = list(color = "#94A3B8", fontSize = 13)) |>
      echarts4r::e_x_axis(show = FALSE) |>
      echarts4r::e_y_axis(show = FALSE) |>
      echarts4r::e_legend(show = FALSE) |>
      echarts4r::e_tooltip(show = FALSE)
  }

  # ── KPI strip ───────────────────────────────────────────────────────────
  output$rand_kpi_strip <- renderUI({
    df  <- sites_wp()
    d   <- rand_dates()

    # How many were recruited does not depend on having a date for it. Counting
    # dates read 0 against an export that carries the recruitment fields but no
    # date column, which is the whole headline wrong. Use the trial's
    # recruitment definition where it applies, and fall back to counting dates
    # for trials that define none.
    rec <- tryCatch(recruited_ids(redcap_wp(), rv$trial_config),
                    error = function(e) NULL)
    total_rand    <- if (!is.null(rec)) length(rec) else length(d)
    n_recruiting  <- sum(df$status == "Recruiting", na.rm = TRUE)
    trial_target  <- wp_effective_target(rv$trial_config, rv$active_wp)
    if (trial_target <= 0) trial_target <- 100L

    # Anything per-period genuinely needs dates. With none, an em dash says so;
    # a 0 would read as "nobody this month" rather than "we cannot tell".
    month_start <- as.Date(format(Sys.Date(), "%Y-%m-01"))
    this_month   <- if (length(d)) sum(d >= month_start) else "\u2014"
    monthly_rate <- if (length(d)) {
      first_date     <- min(d)
      months_elapsed <- max(1, as.numeric(difftime(Sys.Date(), first_date,
                                                    units = "days")) / 30.44)
      round(length(d) / months_elapsed, 1)
    } else "\u2014"

    pct <- if (trial_target > 0) round(100 * total_rand / trial_target) else 0

    make_kpi <- function(value, label, sub = NULL) {
      div(class = "rand-kpi",
          div(class = "rand-kpi-label", label),
          div(class = "rand-kpi-value", value),
          if (!is.null(sub)) div(class = "rand-kpi-sub", sub))
    }

    div(class = "rand-kpi-row",
        make_kpi(total_rand,   recruit_term("total_label", rv$trial_config),
                 sprintf("%d%% of target (%s)", pct,
                         format(trial_target, big.mark = ","))),
        make_kpi(this_month,   "This month"),
        make_kpi(monthly_rate, "Avg / month"),
        make_kpi(n_recruiting, "Sites recruiting"))
  })

  # ── Monthly bar chart ───────────────────────────────────────────────────
  output$rand_monthly_chart <- renderEcharts4r({
    d <- rand_dates()
    if (!length(d))
      return(.empty_chart(.no_dates_msg()))

    months_chr <- format(d, "%Y-%m")
    monthly <- as.data.frame(table(months_chr), stringsAsFactors = FALSE)
    names(monthly) <- c("month", "n")
    monthly <- monthly[order(monthly$month), , drop = FALSE]
    monthly$month <- factor(monthly$month, levels = monthly$month)

    monthly |>
      echarts4r::e_charts(month) |>
      echarts4r::e_bar(n, name = recruit_term("Noun", rv$trial_config),
                       itemStyle = list(color = "#2EC4A5",
                                         borderRadius = c(4, 4, 0, 0))) |>
      echarts4r::e_x_axis(axisLabel = list(rotate = 45, fontSize = 10)) |>
      echarts4r::e_y_axis(name = "Count", minInterval = 1) |>
      echarts4r::e_tooltip(trigger = "axis") |>
      echarts4r::e_legend(show = FALSE) |>
      echarts4r::e_grid(left = "12%", right = "5%", bottom = "18%")
  })

  # ── Cumulative line chart ───────────────────────────────────────────────
  output$rand_cumulative_chart <- renderEcharts4r({
    d <- rand_dates()
    if (!length(d))
      return(.empty_chart(.no_dates_msg()))

    trial_target <- wp_effective_target(rv$trial_config, rv$active_wp)
    if (trial_target <= 0) trial_target <- 100L

    daily <- as.data.frame(table(d), stringsAsFactors = FALSE)
    names(daily) <- c("date", "n")
    daily$date <- as.Date(daily$date)
    daily <- daily[order(daily$date), , drop = FALSE]
    daily$cumulative <- cumsum(daily$n)

    daily |>
      echarts4r::e_charts(date) |>
      echarts4r::e_line(cumulative, name = "Cumulative",
                        smooth = TRUE,
                        areaStyle = list(opacity = 0.15, color = "#1B4F6B"),
                        lineStyle = list(color = "#1B4F6B", width = 2),
                        itemStyle = list(color = "#1B4F6B")) |>
      echarts4r::e_mark_line(data = list(yAxis = trial_target),
                             label = list(formatter = "Target", position = "end"),
                             lineStyle = list(color = "#F59E0B",
                                              type  = "dashed")) |>
      echarts4r::e_x_axis(type = "time") |>
      echarts4r::e_y_axis(name = "Total", minInterval = 1) |>
      echarts4r::e_tooltip(trigger = "axis") |>
      echarts4r::e_legend(show = FALSE) |>
      echarts4r::e_grid(left = "12%", right = "5%", bottom = "12%")
  })

  output$rand_table <- renderReactable({
    df <- sites_wp()
    if (nrow(df) == 0) return(empty_reactable("No sites loaded."))
    df <- df %>%
      mutate(
        status_html = vapply(status, status_pill_html, character(1)),
        prog_html   = prog_bar_html(randomised, target)
      )
    reactable(
      df %>% select(site_id, site_name, status_html, randomised, target, prog_html),
      striped = TRUE, highlight = TRUE, compact = TRUE,
      defaultColDef = colDef(style = list(fontFamily = "Outfit", fontSize = "13px")),
      columns = list(
        site_id     = colDef(name = "Site ID", minWidth = 90,
                             cell = function(v) htmltools::span(class = "sid", v)),
        site_name   = colDef(name = "Site", minWidth = 180),
        status_html = colDef(name = "Status", html = TRUE, minWidth = 120),
        randomised  = colDef(name = recruit_term("Past", rv$trial_config),
                             align = "center", minWidth = 110),
        target      = colDef(name = "Target", align = "center", minWidth = 90,
                             style = list(color = col_muted)),
        prog_html   = colDef(name = "Progress", html = TRUE, minWidth = 180)
      )
    )
  })

  output$log_table <- renderReactable({
    if (nrow(rv$log) == 0) return(empty_reactable("No activity yet"))
    rv$log %>% arrange(desc(timestamp)) %>%
      mutate(timestamp = format(timestamp, "%d %b %Y  %H:%M")) %>%
      select(Timestamp = timestamp, `Site ID` = site_id, Action = action, Note = note) %>%
      reactable(striped = TRUE, highlight = TRUE, compact = TRUE,
                defaultPageSize = 25, showPageSizeOptions = TRUE,
                pageSizeOptions = c(10, 25, 50, 100),
                defaultColDef = colDef(style = list(fontFamily = "Outfit", fontSize = "12.5px")),
                columns = list(
                  `Site ID` = colDef(cell = function(v) htmltools::span(class = "sid", v)),
                  Action = colDef(minWidth = 70, cell = function(v)
                    htmltools::HTML(if (v == "+1")
                      "<span style='color:#059669;font-weight:700'>+1</span>"
                      else "<span style='color:#DC2626;font-weight:700'>\u22121</span>"))
                ))
  })
}
