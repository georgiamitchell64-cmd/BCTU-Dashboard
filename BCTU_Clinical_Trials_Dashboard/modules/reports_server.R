reports_server <- function(input, output, session, state) {
  rv <- state$rv
  # WP-scoped views drive the Charts tab (rpt_monthly). Generated reports
  # (TMG / TSC documents) deliberately stay on the full rv$ stores — a report
  # is a whole-trial artefact, not a per-WP view.
  redcap_wp <- state$redcap_wp
  sites_wp  <- state$sites_wp
  rpt_filter <- reactive({
    s <- input$rpt_sites
    if (is.null(s) || length(s) == 0) NULL else s
  })
  
  # populate site picker
  observe({
    req(rv$sites)
    req("site_name" %in% names(rv$sites))
    updatePickerInput(
      session,
      inputId  = "rpt_sites",
      choices  = sort(unique(rv$sites$site_name)),
      selected = character(0)
    )
  })
  
  rpt_monthly <- reactive({
    raw   <- redcap_wp()
    dates <- input$rpt_dates
    rand_col <- fld("randomisation_datetime", default = "rand_dttm_s")
    site_col <- fld("site_name",              default = "site_name")
    if (is.null(raw) || nrow(raw) == 0)          return(NULL)
    if (is.null(dates) || length(dates) != 2)     return(NULL)
    if (!rand_col %in% names(raw))                return(NULL)

    rands <- raw %>%
      mutate(.rand_raw = trimws(as.character(.data[[rand_col]]))) %>%
      filter(nchar(.rand_raw) > 0, !is.na(.rand_raw),
             !is.na(.data[[site_col]]), nchar(trimws(.data[[site_col]])) > 0) %>%
      group_by(record_id) %>% slice(1) %>% ungroup() %>%
      mutate(
        rand_date = suppressWarnings(lubridate::parse_date_time(
          .rand_raw,
          orders = c("dmY HM", "dmy HM", "dmY HMS", "Ymd HM", "Ymd HMS", "dmy", "Ymd"),
          tz     = "Europe/London",
          quiet  = TRUE
        )),
        month = as.Date(format(floor_date(rand_date, "month"), "%Y-%m-%d"))
      ) %>%
      filter(!is.na(month))
    
    if (nrow(rands) == 0) return(NULL)
    
    site_meta <- sites_wp() %>%
      transmute(.matched_id = site_id, monthly_target,
                .jk = tolower(trimws(site_name)))

    rands <- rands %>%
      mutate(.jk = tolower(trimws(.data[[site_col]]))) %>%
      left_join(site_meta, by = ".jk") %>%
      select(-.jk) %>%
      filter(!is.na(.matched_id)) %>%
      mutate(site_id = .matched_id) %>%
      select(-.matched_id)
    
    if (nrow(rands) == 0) return(NULL)
    
    s <- rpt_filter()
    if (!is.null(s) && length(s) > 0)
      rands <- rands %>% filter(site_id %in% s)
    
    if (nrow(rands) == 0) return(NULL)
    
    monthly_act <- rands %>%
      count(site_id, site_name, month, name = "actual")
    
    site_ranges <- rands %>%
      group_by(site_id, site_name, monthly_target) %>%
      summarise(open_month = min(month, na.rm = TRUE), .groups = "drop")
    
    grid <- do.call(rbind, lapply(seq_len(nrow(site_ranges)), function(i) {
      r      <- site_ranges[i, ]
      months <- as.Date(seq(r$open_month,
                            floor_date(Sys.Date(), "month"),
                            by = "month"))
      data.frame(
        site_id        = r$site_id,
        site_name      = r$site_name,
        month          = months,
        monthly_target = r$monthly_target,
        stringsAsFactors = FALSE
      )
    }))
    grid$month <- as.Date(grid$month)
    
    df <- grid %>%
      left_join(monthly_act %>% select(site_id, month, actual),
                by = c("site_id", "month")) %>%
      replace_na(list(actual = 0L)) %>%
      arrange(site_id, month) %>%
      group_by(site_id) %>%
      mutate(cum_actual = cumsum(actual),
             cum_target = cumsum(monthly_target)) %>%
      ungroup()
    
    df %>%
      filter(
        month >= floor_date(as.Date(dates[1]), "month"),
        month <= floor_date(as.Date(dates[2]), "month")
      )
  })
  
  rpt_overall <- reactive({
    df <- rpt_monthly()
    # NULL (not req()) so charts can fall through to their empty-state
    # rendering instead of suspending behind the loading spinner forever.
    if (is.null(df)) return(NULL)
    make_overall_df(df)
  })
  
  output$c1_title <- renderUI({
    req(input$rpt_view, input$rpt_breakdown)
    v <- if (input$rpt_view == "cumulative") "Cumulative" else "Monthly"
    b <- if (input$rpt_breakdown == "overall") "all sites" else "per site"
    tags$span(paste(v, "recruitment vs target", b))
  })
  
  empty_e <- function(msg = "No data \u2014 load REDCap CSV or add randomisations") {
    empty_echart(msg)
  }
  
  output$chart_recruit <- renderEcharts4r({
    df_m <- rpt_monthly(); df_ov <- rpt_overall()
    if (is.null(df_m) || nrow(df_m) == 0) return(empty_e())
    isCum <- input$rpt_view == "cumulative"
    isPer <- input$rpt_breakdown == "per_site"
    
    if (!isPer) {
      if (is.null(df_ov)) return(empty_e())
      ya <- if (isCum) df_ov$cum_actual else df_ov$actual
      yt <- if (isCum) df_ov$cum_target else df_ov$monthly_target
      data.frame(month = df_ov$month_label, actual = ya, target = yt) %>%
        e_charts(month) %>%
        e_bar(actual, name = "Actual", color = col_teal, barMaxWidth = 40,
              itemStyle = list(borderRadius = c(4, 4, 0, 0))) %>%
        e_line(target, name = "Target", color = col_navy,
               lineStyle = list(type = "dashed", width = 2.5),
               symbol = "circle", symbolSize = 5) %>%
        e_tonic() %>%
        e_y_axis(name = if (isCum) "Cumulative randomisations" else "Randomisations / month",
                 nameTextStyle = list(fontFamily = "Outfit", fontSize = 11,
                                      color = col_muted)) %>%
        e_toolbox(feature = list(saveAsImage = list(title = "Save PNG")))
    } else {
      ycol  <- if (isCum) "cum_actual" else "actual"
      tycol <- if (isCum) "cum_target" else "monthly_target"
      sids  <- unique(df_m$site_id)
      pal   <- colorRampPalette(c(col_teal, col_navy, col_amber,
                                  "#A78BFA", "#F97316"))(length(sids))
      
      df_m2 <- df_m %>%
        mutate(month_label = factor(
          format(as.Date(month), "%b %Y"),
          levels = unique(format(sort(unique(as.Date(month))), "%b %Y"))
        ))
      
      tgt <- df_m2 %>% group_by(month_label) %>%
        summarise(tgt = sum(.data[[tycol]]), .groups = "drop")
      
      wide <- df_m2 %>%
        filter(!is.na(site_name), nchar(trimws(site_name)) > 0) %>%
        select(month_label, site_name, val = all_of(ycol)) %>%
        pivot_wider(names_from = site_name, values_from = val,
                    values_fill = 0) %>%
        left_join(tgt, by = "month_label")
      
      p <- wide %>% e_charts(month_label)
      for (i in seq_along(sids)) {
        sname <- unique(df_m$site_name[df_m$site_id == sids[i]])[1]
        if (!is.null(sname) && sname %in% names(wide)) {
          p <- p %>% e_bar(!!sym(sname), name = sname, color = pal[i],
                           stack = "total",
                           itemStyle = list(borderRadius = c(3, 3, 0, 0)))
        }
      }
      p %>%
        e_line(tgt, name = "Target", color = col_navy,
               lineStyle = list(type = "dashed", width = 2.5),
               symbol = "circle", symbolSize = 4) %>%
        e_tonic() %>%
        e_toolbox(feature = list(saveAsImage = list(title = "Save PNG")))
    }
  })
  
  output$chart_sites_recruiting <- renderEcharts4r({
    df <- rpt_monthly()
    if (is.null(df) || nrow(df) == 0) return(empty_e("No data"))
    spm <- df %>%
      filter(actual > 0) %>%
      distinct(site_id, month) %>%
      count(month, name = "n") %>%
      arrange(month) %>%
      mutate(month_label = format(as.Date(month), "%b %Y"))
    spm %>% e_charts(month_label) %>%
      e_bar(n, name = "Sites recruiting", color = col_navy, barMaxWidth = 40,
            itemStyle = list(borderRadius = c(4, 4, 0, 0))) %>%
      e_tonic() %>%
      e_legend(show = FALSE) %>%
      e_y_axis(minInterval = 1, name = "Sites with 1 rand",
               nameTextStyle = list(fontFamily = "Outfit", fontSize = 11,
                                    color = col_muted)) %>%
      e_toolbox(feature = list(saveAsImage = list(title = "Save PNG")))
  })
  
  output$chart_rate <- renderEcharts4r({
    df <- rpt_overall()
    if (is.null(df) || nrow(df) == 0) return(empty_e("No data"))
    df <- df %>%
      mutate(rate = ifelse(monthly_target > 0,
                           round(100 * actual / monthly_target), 0))
    df %>% e_charts(month_label) %>%
      e_bar(rate, name = "% of target", barMaxWidth = 40,
            itemStyle = list(
              borderRadius = c(4, 4, 0, 0),
              color = JS("function(p){return p.value>=100?'#2EC4A5':'#F59E0B';}")
            )) %>%
      e_mark_line(data = list(yAxis = 100),
                  lineStyle = list(type = "dashed", color = col_navy, width = 2),
                  label = list(show = TRUE, formatter = "100% target",
                               fontFamily = "Outfit", fontSize = 10,
                               color = col_navy)) %>%
      e_tonic() %>%
      e_legend(show = FALSE) %>%
      e_y_axis(min = 0,
               axisLabel = list(formatter = "{value}%", fontFamily = "Outfit",
                                fontSize = 11, color = col_muted)) %>%
      e_tooltip(trigger = "axis",
                formatter = JS("function(p){return p[0].name+'<br/>'+p[0].value+'%';}"),
                backgroundColor = "rgba(27,79,107,.92)",
                textStyle = list(color = "#fff", fontFamily = "Outfit")) %>%
      e_toolbox(feature = list(saveAsImage = list(title = "Save PNG")))
  })
  
  output$heatmap_ui <- renderUI({
    df <- rpt_monthly()
    if (is.null(df) || nrow(df) == 0)
      return(div(style = "padding:20px;color:#64748B;font-family:Outfit,sans-serif",
                 "No data"))
    df    <- df %>% mutate(month_label = format(month, "%b %Y"))
    mos   <- unique(df$month_label[order(df$month)])
    sites <- unique(df %>% select(site_id, site_name, monthly_target))
    
    thead <- tags$thead(
      tags$tr(
        tags$th(class = "site-td", "Site"),
        tags$th("Mo. target"),
        lapply(mos, tags$th),
        tags$th("Total"), tags$th("Target"), tags$th("%")
      )
    )
    tbody_rows <- lapply(sites$site_id, function(sid) {
      row    <- sites %>% filter(site_id == sid)
      sname  <- row$site_name
      mo_tgt <- row$monthly_target
      site_df <- df %>% filter(site_id == sid)
      cells  <- lapply(mos, function(m) {
        r <- site_df %>% filter(month_label == m)
        tags$td(if (nrow(r) > 0)
          HTML(hm_cell_html(r$actual[1], mo_tgt))
          else HTML('<span style="color:#CBD5E1"></span>'))
      })
      total   <- sum(site_df$actual, na.rm = TRUE)
      tgt     <- sum(site_df$monthly_target, na.rm = TRUE)
      pct     <- if (tgt > 0) round(100 * total / tgt) else 0
      pct_col <- if (pct >= 100) "#059669" else if (pct >= 80) "#D97706" else "#DC2626"
      tags$tr(
        tags$td(class = "site-td",
                tags$div(sname),
                tags$span(
                  style = "font-size:10px;color:#64748B;font-weight:400",
                  paste0("Target: ", mo_tgt, "/mo"))),
        tags$td(style = "text-align:center;color:#64748B", mo_tgt),
        cells,
        tags$td(style = "text-align:center;font-weight:700;color:#1B4F6B", total),
        tags$td(style = "text-align:center;color:#64748B", tgt),
        tags$td(style = paste0("text-align:center;font-weight:700;color:", pct_col),
                paste0(pct, "%"))
      )
    })
    tags$table(class = "hm-table", thead, tags$tbody(tbody_rows))
  })
  
  output$summary_table <- renderReactable({
    df <- rpt_monthly()
    if (is.null(df) || nrow(df) == 0)
      return(empty_reactable("No data"))
    df %>%
      group_by(site_id, site_name) %>%
      summarise(Total  = sum(actual),
                Target = sum(monthly_target),
                Pct    = paste0(ifelse(Target > 0,
                                       round(100 * Total / Target), 0), "%"),
                Months = n(),
                Best   = max(actual),
                .groups = "drop") %>%
      rename(`Site ID` = site_id, Site = site_name,
             `%` = Pct, `Months active` = Months, `Best month` = Best) %>%
      arrange(desc(Total)) %>%
      reactable(
        striped  = TRUE, highlight = TRUE, compact = TRUE,
        defaultColDef = colDef(style = list(fontFamily = "Outfit",
                                            fontSize = "12.5px")),
        columns = list(
          `Site ID`     = colDef(cell = function(v)
            htmltools::span(class = "sid", v)),
          Total         = colDef(align = "center"),
          Target        = colDef(align = "center"),
          `%`           = colDef(align = "center"),
          `Months active` = colDef(align = "center"),
          `Best month`  = colDef(align = "center")
        )
      )
  })
  
  # ── Excel downloads ────────────────────────────────────────────────────────
  output$dl_c1_data <- xlsx_download(
    function() if (input$rpt_breakdown == "overall") rpt_overall() else rpt_monthly(),
    "TONIC_recruitment")
  output$dl_c2_data <- xlsx_download(
    function() {
      df <- rpt_monthly(); if (is.null(df)) return(NULL)
      df %>% filter(actual > 0) %>% distinct(site_id, month) %>% count(month)
    }, "TONIC_sites_recruiting")
  output$dl_c3_data <- xlsx_download(
    function() {
      df <- rpt_overall(); if (is.null(df)) return(NULL)
      df %>% mutate(rate = ifelse(monthly_target > 0,
                                  round(100 * actual / monthly_target), 0))
    }, "TONIC_rate")
  output$dl_heatmap <- xlsx_download(rpt_monthly, "TONIC_heatmap")
  output$dl_summary <- xlsx_download(
    function() {
      df <- rpt_monthly(); if (is.null(df)) return(NULL)
      df %>%
        group_by(site_id, site_name) %>%
        summarise(total      = sum(actual),
                  target     = sum(monthly_target),
                  pct        = round(100 * total / pmax(target, 1)),
                  best_month = max(actual),
                  .groups    = "drop")
    }, "TONIC_summary")
  
  # ── TSC: dynamic input row counters ────────────────────────────────────────
  n_amd_sub    <- reactiveVal(0L)
  n_amd_nonsub <- reactiveVal(0L)
  n_custom     <- reactiveVal(0L)
  
  observeEvent(input$amd_sub_add,       n_amd_sub(n_amd_sub() + 1L))
  observeEvent(input$amd_sub_remove,    if (n_amd_sub() > 0)    n_amd_sub(n_amd_sub() - 1L))
  observeEvent(input$amd_nonsub_add,    n_amd_nonsub(n_amd_nonsub() + 1L))
  observeEvent(input$amd_nonsub_remove, if (n_amd_nonsub() > 0) n_amd_nonsub(n_amd_nonsub() - 1L))
  observeEvent(input$cs_add,            n_custom(n_custom() + 1L))
  observeEvent(input$cs_remove,         if (n_custom() > 0)     n_custom(n_custom() - 1L))
  
  # Render dynamic amendment rows
  output$amd_sub_ui <- renderUI({
    n <- n_amd_sub()
    if (n == 0) return(p("No substantial amendments added yet.",
                          style = "color:#64748B; font-style:italic;"))
    lapply(seq_len(n), function(i) {
      div(
        style = "border:1px solid #e0e8ef; border-radius:4px; padding:8px 10px; margin-bottom:6px;",
        fluidRow(
          column(3, dateInput(paste0("sub_date_", i), "Date", value = Sys.Date())),
          column(6, textAreaInput(paste0("sub_desc_", i), "Description", rows = 2)),
          column(3, selectInput(paste0("sub_status_", i), "Status",
                                 choices = c("Submitted", "Approved", "In preparation", "Withdrawn"),
                                 selected = "Submitted"))
        )
      )
    })
  })
  
  output$amd_nonsub_ui <- renderUI({
    n <- n_amd_nonsub()
    if (n == 0) return(p("No non-substantial amendments added yet.",
                          style = "color:#64748B; font-style:italic;"))
    lapply(seq_len(n), function(i) {
      div(
        style = "border:1px solid #e0e8ef; border-radius:4px; padding:8px 10px; margin-bottom:6px;",
        fluidRow(
          column(3, dateInput(paste0("nonsub_date_", i), "Date", value = Sys.Date())),
          column(6, textAreaInput(paste0("nonsub_desc_", i), "Description", rows = 2)),
          column(3, selectInput(paste0("nonsub_status_", i), "Status",
                                 choices = c("Submitted", "Approved", "In preparation", "Withdrawn"),
                                 selected = "Submitted"))
        )
      )
    })
  })
  
  output$cs_ui <- renderUI({
    n <- n_custom()
    if (n == 0) return(p("No custom sections added yet.",
                          style = "color:#64748B; font-style:italic;"))
    lapply(seq_len(n), function(i) {
      div(
        style = "border:1px solid #e0e8ef; border-radius:4px; padding:10px 12px; margin-bottom:8px;",
        fluidRow(
          column(5, textInput(paste0("cs_title_", i), "Section title",
                               placeholder = "e.g. PI Associate Scheme update")),
          column(7, selectInput(paste0("cs_pos_", i), "Insert at",
                                 choices = c(
                                   "After General Report Information" = "after_general_info",
                                   "After Recruitment Details"        = "after_recruitment",
                                   "After Screening data"             = "after_screening",
                                   "After Safety"                     = "after_safety",
                                   "End of report"                    = "end_of_report"
                                 )))
        ),
        textAreaInput(paste0("cs_content_", i), "Content", rows = 4, resize = "vertical",
                       placeholder = "Narrative content to include in the report\u2026")
      )
    })
  })
  
  # Helpers to assemble TSC inputs into data structures for the Rmd
  collect_amendments <- function(kind) {
    n <- if (kind == "sub") n_amd_sub() else n_amd_nonsub()
    if (n == 0) return(NULL)
    rows <- lapply(seq_len(n), function(i) {
      d <- input[[paste0(kind, "_date_", i)]]
      data.frame(
        Date        = if (!is.null(d)) format(as.Date(d), "%d %b %Y") else "",
        Description = if (!is.null(input[[paste0(kind, "_desc_", i)]])) input[[paste0(kind, "_desc_", i)]] else "",
        Status      = if (!is.null(input[[paste0(kind, "_status_", i)]])) input[[paste0(kind, "_status_", i)]] else "",
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  }
  
  collect_custom_sections <- function() {
    n <- n_custom()
    if (n == 0) return(NULL)
    lapply(seq_len(n), function(i) {
      list(
        title    = input[[paste0("cs_title_", i)]]   %||% "",
        content  = input[[paste0("cs_content_", i)]] %||% "",
        position = input[[paste0("cs_pos_", i)]]     %||% "end_of_report"
      )
    })
  }
  `%||%` <- function(a, b) if (is.null(a) || (is.character(a) && length(a) == 1 && !nzchar(a))) b else a
  
  # ── Report download (routes by report_type) ────────────────────────────────
  output$download_report <- downloadHandler(
    
    filename = function() {
      rt    <- if (is.null(input$report_type)) "TMG" else input$report_type
      stamp <- format(Sys.Date(), "%Y-%m-%d")
      ext   <- if (rt == "TSC") "docx" else "html"
      sprintf("TONIC_%s_Report_%s.%s", rt, stamp, ext)
    },
    
    content = function(file) {
     tryCatch({
      rt <- if (is.null(input$report_type)) "TMG" else input$report_type

      notif_id <- showNotification(
        sprintf("Generating %s report \u2014 please wait\u2026", rt),
        duration = NULL, type = "message"
      )
      on.exit(removeNotification(notif_id), add = TRUE)
      
      # If user opted for full trial data, clear filters for prepare_report_data
      use_filters <- !isTRUE(input$rpt_full_trial)
      sites_for_prep <- if (use_filters) input$rpt_sites else NULL
      from_for_prep  <- if (use_filters) input$rpt_dates[1] else NULL
      to_for_prep    <- if (use_filters) input$rpt_dates[2] else NULL
      
      # Locate the newest return-rate CSV at report time so any freshly
      # exported file is picked up. Falls back to NULL if none is found
      # (prepare_report_data handles NULL gracefully).
      latest_crf_path <- tryCatch(
        {
          p <- latest_return_rate_file(
            dir        = rv$trial_config$return_rates_dir,
            trial_code = rv$trial_config$code
          )
          if (is.null(p) || !file.exists(p)) NULL
          # Use path relative to the app root so it works identically in
          # every render context (intermediates_dir resolves relative
          # to the Rmd, but prepare_report_data runs in the app wd).
          else p
        },
        error = function(e) NULL
      )

      report_data <- prepare_report_data(
        df                = rv$raw_redcap,
        selected_sites    = sites_for_prep,
        date_from         = from_for_prep,
        date_to           = to_for_prep,
        include_withdrawn = isTRUE(input$include_withdrawn),
        pipeline_df       = rv$sites,
        crf_csv_path      = latest_crf_path
      )
      
      site_label <- if (is.null(sites_for_prep) || length(sites_for_prep) == 0) {
        "All sites"
      } else {
        paste(sites_for_prep, collapse = ", ")
      }
      
      tmp_dir  <- tempdir()

      # Make sure pandoc is reachable on this machine (Mac / Windows / Linux).
      if (!ensure_pandoc()) {
        showNotification(
          HTML("Pandoc not found. Install it (https://pandoc.org/installing.html)
                or open this app from inside RStudio so it can find the bundled
                pandoc."),
          type = "error", duration = 12)
        return()
      }

      tryCatch({
        if (rt == "TSC") {
          # ── TSC: Word output ─────────────────────────────────────────────
          rmd_src <- resolve_report_template(rv$trial_config, "tsc")
          if (is.null(rmd_src))
            stop("TSC report template not found. Expected: ",
                 trial_report_template_path(rv$trial_config, "tsc"),
                 " (or top-level fallback ",
                 default_report_template_path("tsc"), ")")
          rmd_dest <- file.path(tmp_dir, "tsc_report.Rmd")
          file.copy(rmd_src, rmd_dest, overwrite = TRUE)
          # Also copy the ggplot helpers alongside so the Rmd can find them
          file.copy("functions/tsc_charts.R", file.path(tmp_dir, "tsc_charts.R"), overwrite = TRUE)
          file.copy("functions/consort_flow.R", file.path(tmp_dir, "consort_flow.R"), overwrite = TRUE)
          file.copy("functions/flat_completeness.R", file.path(tmp_dir, "flat_completeness.R"), overwrite = TRUE)
          file.copy("functions/baseline_table.R", file.path(tmp_dir, "baseline_table.R"), overwrite = TRUE)
          
          trial_summary <- list(
            objectives         = input$ts_objectives,
            design             = input$ts_design,
            eligibility        = input$ts_eligibility,
            interventions      = input$ts_interventions,
            primary_outcome    = input$ts_primary_outcome,
            secondary_outcomes = input$ts_secondary_outcomes
          )
          
          rmarkdown::render(
            input             = rmd_dest,
            output_file       = file,
            output_format     = "word_document",
            params = filter_params_for_rmd(list(
              report_data     = report_data,
              selected_sites  = site_label,
              date_from       = from_for_prep,
              date_to         = to_for_prep,
              crf_csv_path    = latest_crf_path,
              screening_xlsx_path = "screening/TONIC_screening.xlsx",
              meeting_date    = input$meeting_date,
              report_date     = format(Sys.Date(), "%d %B %Y"),
              prepared_by     = input$prepared_by,
              reviewed_by     = input$reviewed_by,
              protocol_version = input$protocol_version,
              trial_summary   = trial_summary,
              funder_update   = input$funder_update,
              amendments_substantial     = collect_amendments("sub"),
              amendments_non_substantial = collect_amendments("nonsub"),
              custom_sections = collect_custom_sections(),
              completeness_style = input$completeness_style %||% "heatmap",
              report_content  = rv$trial_config$report_content,
              logo_path       = resolve_logo_path(rv$trial_config)
            ), rmd_dest),
            envir             = new.env(parent = globalenv()),
            intermediates_dir = tmp_dir,
            clean             = TRUE,
            quiet             = TRUE
          )
        } else {
          # ── TMG / iTMG: HTML output (existing flow + report_type param) ───
          rmd_src <- resolve_report_template(rv$trial_config, "tonic")
          if (is.null(rmd_src))
            stop("TMG/iTMG report template not found. Expected: ",
                 trial_report_template_path(rv$trial_config, "tonic"),
                 " (or top-level fallback ",
                 default_report_template_path("tonic"), ")")
          rmd_dest <- file.path(tmp_dir, "tonic_report.Rmd")
          file.copy(rmd_src, rmd_dest, overwrite = TRUE)
          file.copy("functions/consort_flow.R", file.path(tmp_dir, "consort_flow.R"), overwrite = TRUE)
          file.copy("functions/flat_completeness.R", file.path(tmp_dir, "flat_completeness.R"), overwrite = TRUE)
          file.copy("functions/baseline_table.R", file.path(tmp_dir, "baseline_table.R"), overwrite = TRUE)
          # Stage the BCTU brand logo so the report header can embed it
          if (file.exists("www/BlackText-landscape.png"))
            file.copy("www/BlackText-landscape.png",
                      file.path(tmp_dir, "BlackText-landscape.png"), overwrite = TRUE)
          
          rmarkdown::render(
            input             = rmd_dest,
            output_file       = file,
            output_format     = "html_document",
            params            = filter_params_for_rmd(list(
              report_data       = report_data,
              selected_sites    = site_label,
              date_from         = from_for_prep,
              date_to           = to_for_prep,
              include_withdrawn = isTRUE(input$include_withdrawn),
              include_appendix  = isTRUE(input$report_appendix),
              report_date       = format(Sys.Date(), "%d %B %Y"),
              logo_path         = resolve_logo_path(rv$trial_config),
              crf_csv_path      = latest_crf_path,
              screening_xlsx_path = "screening/TONIC_screening.xlsx",
              report_type       = rt,   # "TMG" or "iTMG"
              completeness_style = input$completeness_style %||% "heatmap",
              report_content    = rv$trial_config$report_content
            ), rmd_dest),
            envir             = new.env(parent = globalenv()),
            intermediates_dir = tmp_dir,
            clean             = TRUE,
            quiet             = FALSE
          )
        }
      }, error = function(e) {
        msg <- paste0("Report generation failed: ", conditionMessage(e))
        tb <- tryCatch(
          paste(capture.output(traceback(max.lines = 50)), collapse = "\n"),
          error = function(e2) ""
        )
        calls_txt <- tryCatch({
          cs <- sys.calls()
          paste(vapply(seq_along(cs), function(i)
            paste0(i, ": ", paste(deparse(cs[[i]]), collapse = " ")),
            character(1)), collapse = "\n")
        }, error = function(e2) "")
        message("\n=== TONIC report error ===\n", msg, "\n", calls_txt, "\n")
        showNotification(msg, duration = 15, type = "error")
        tryCatch(
          writeLines(
            c("TONIC report generation failed.",
              "",
              msg,
              "",
              "--- Call stack ---",
              calls_txt,
              "",
              "--- traceback() ---",
              tb,
              "",
              "Please share this file with the dashboard maintainer."),
            con = file
          ),
          error = function(e2) NULL
        )
      })
     },
     error = function(e) {
       msg <- paste0("Report generation failed (outer): ", conditionMessage(e))
       tb <- tryCatch(
         paste(capture.output(traceback(max.lines = 80)), collapse = "\n"),
         error = function(e2) "")
       calls_txt <- tryCatch({
         cs <- sys.calls()
         paste(vapply(seq_along(cs), function(i)
           paste0(i, ": ", paste(deparse(cs[[i]]), collapse = " ")),
           character(1)), collapse = "\n")
       }, error = function(e2) "")
       message("\n=== TONIC report OUTER error ===\n", msg, "\n",
               calls_txt, "\n", tb, "\n")
       showNotification(msg, duration = 15, type = "error")
       tryCatch(
         writeLines(
           c("TONIC report generation failed before render.",
             "", msg, "",
             "--- Call stack ---", calls_txt, "",
             "--- traceback() ---", tb),
           con = file),
         error = function(e2) NULL)
     })
    }
  )

  # ════════════════════════════════════════════════════════════════════════
  # Amendments editor (feeds the Amendments report section)
  # ════════════════════════════════════════════════════════════════════════
  amendments_state <- reactiveVal(list())

  observeEvent(rv$trial_config, {
    cfg <- rv$trial_config
    if (is.null(cfg)) { amendments_state(list()); return() }
    saved <- cfg$amendments
    amendments_state(if (is.null(saved)) list() else saved)
  }, ignoreNULL = TRUE)

  output$amendments_list_ui <- renderUI({
    items <- amendments_state()
    if (!length(items)) {
      return(div(style = "padding:20px;text-align:center;color:#94A3B8;
                          font-size:12.5px;font-style:italic;",
                 "No amendments tracked yet — click Add amendment."))
    }
    rows <- lapply(seq_along(items), function(i) {
      a <- items[[i]]
      sev <- if (identical(a$type, "Substantial"))
        list(bg = "#FEF2F2", fg = "#B91C1C", border = "#FECACA")
      else
        list(bg = "#F5F3FF", fg = "#6366F1", border = "#C7D2FE")

      div(style = sprintf("display:grid;grid-template-columns:auto 1fr auto;
                           gap:14px;padding:12px 14px;border:1px solid %s;
                           border-radius:10px;margin-bottom:8px;background:%s;",
                          sev$border, sev$bg),
          div(style = sprintf("font-size:10.5px;font-weight:700;
                               text-transform:uppercase;letter-spacing:.5px;
                               color:%s;align-self:center;width:90px;",
                              sev$fg),
              a$type %||% "Amendment"),
          div(div(style = "font-weight:600;color:#0F172A;font-size:13px;",
                  sprintf("%s — %s",
                          a$ref     %||% sprintf("Amendment %d", i),
                          a$status  %||% "Pending")),
              div(style = "font-size:11.5px;color:#64748B;margin-top:2px;",
                  sprintf("Submitted %s",
                          a$date %||% "—")),
              div(style = "font-size:12.5px;color:#475569;margin-top:6px;
                           line-height:1.5;",
                  a$description %||% "")),
          div(style = "display:flex;gap:6px;align-self:start;",
              actionButton(paste0("amend_edit_", i),
                           HTML("&#9998;"),
                           class = "btn btn-sm",
                           style = "padding:2px 8px;font-size:11px;
                                    background:#FFFFFF;border:1px solid #DDE5EE;
                                    color:#475569;"),
              actionButton(paste0("amend_del_", i),
                           HTML("&times;"),
                           class = "btn btn-sm",
                           style = "padding:2px 8px;font-size:13px;
                                    background:#FFFFFF;border:1px solid #FECACA;
                                    color:#B91C1C;"))
      )
    })
    div(rows)
  })

  # Persist + reflect into rv$trial_config
  .save_amendments <- function(items) {
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    tryCatch(update_overrides(cfg, amendments = items),
             error = function(e) message("amend save: ", e$message))
    rv$trial_config$amendments <- items
  }

  amend_editing <- reactiveVal(NULL)   # NULL = adding new

  show_amend_modal <- function(idx = NULL) {
    items <- amendments_state()
    a <- if (is.null(idx)) list() else items[[idx]]
    showModal(modalDialog(
      title = if (is.null(idx)) "Add amendment" else "Edit amendment",
      size = "m", easyClose = TRUE,
      footer = tagList(
        if (!is.null(idx))
          actionButton("amend_save", "Save changes",
                       class = "btn btn-primary",
                       style = "background:#6366F1;border-color:#6366F1;")
        else
          actionButton("amend_save", "Add",
                       class = "btn btn-primary",
                       style = "background:#6366F1;border-color:#6366F1;"),
        modalButton("Cancel")
      ),
      div(style = "display:grid;grid-template-columns:1fr 1fr;gap:12px;",
          selectInput("amend_type", "Type",
                      choices = c("Substantial", "Non-substantial"),
                      selected = a$type %||% "Substantial"),
          dateInput("amend_date", "Date submitted",
                    value = a$date %||% Sys.Date(),
                    format = "d M yyyy")),
      div(style = "display:grid;grid-template-columns:1fr 1fr;gap:12px;",
          textInput("amend_ref", "Reference (e.g. Amendment 3)",
                    value = a$ref %||% ""),
          selectInput("amend_status", "Status",
                      choices = c("Pending", "Approved", "Rejected", "Withdrawn"),
                      selected = a$status %||% "Pending")),
      textAreaInput("amend_description", "Description",
                    value = a$description %||% "",
                    rows = 4, width = "100%",
                    placeholder = "Brief summary of the amendment…")
    ))
    amend_editing(idx)
  }

  observeEvent(input$amend_add, show_amend_modal(NULL))

  observeEvent(input$amend_save, {
    items <- amendments_state()
    new_a <- list(
      type        = input$amend_type %||% "Substantial",
      date        = format(input$amend_date %||% Sys.Date(), "%Y-%m-%d"),
      ref         = input$amend_ref %||% "",
      status      = input$amend_status %||% "Pending",
      description = input$amend_description %||% ""
    )
    idx <- amend_editing()
    if (is.null(idx)) {
      items[[length(items) + 1]] <- new_a
    } else {
      items[[idx]] <- new_a
    }
    amendments_state(items)
    .save_amendments(items)
    cfg <- rv$trial_config
    log_activity(
      if (is.null(idx)) "amendment_added" else "amendment_edited",
      sprintf("%s amendment %s — %s",
              if (is.null(idx)) "Added" else "Edited",
              htmltools::htmlEscape(new_a$ref %||% "(unnamed)"),
              htmltools::htmlEscape(new_a$type %||% "")),
      username = rv$username,
      trial_code = if (!is.null(cfg)) cfg$code else NULL)
    removeModal()
    showNotification("Amendment saved.", type = "message", duration = 3)
  })

  # Wire edit/delete buttons (one per amendment, up to 50 supported)
  lapply(seq_len(50), function(i) {
    observeEvent(input[[paste0("amend_edit_", i)]], {
      show_amend_modal(i)
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("amend_del_", i)]], {
      items <- amendments_state()
      if (i <= length(items)) {
        removed_ref <- items[[i]]$ref %||% sprintf("Amendment %d", i)
        items[[i]] <- NULL
        amendments_state(items)
        .save_amendments(items)
        cfg <- rv$trial_config
        log_activity("amendment_removed",
                     sprintf("Removed amendment <strong>%s</strong>",
                             htmltools::htmlEscape(removed_ref)),
                     username = rv$username,
                     trial_code = if (!is.null(cfg)) cfg$code else NULL)
        showNotification("Amendment removed.", type = "message", duration = 3)
      }
    }, ignoreInit = TRUE)
  })

  # ════════════════════════════════════════════════════════════════════════
  # Stage 10: Report Builder
  # ════════════════════════════════════════════════════════════════════════
  rb_template_choice <- reactiveVal("TMG")
  rb_section_order <- reactiveVal(REPORT_TEMPLATES$TMG$sections)

  # Initialise from saved template overrides if present
  observeEvent(rv$trial_config, {
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    saved <- cfg$report_templates
    pick <- rb_template_choice()
    if (!is.null(saved) && !is.null(saved[[pick]]) &&
        length(saved[[pick]]$sections)) {
      rb_section_order(saved[[pick]]$sections)
    } else if (!is.null(REPORT_TEMPLATES[[pick]])) {
      rb_section_order(REPORT_TEMPLATES[[pick]]$sections)
    }
  }, ignoreNULL = TRUE)

  # Template picker UI
  output$rb_template_picker_ui <- renderUI({
    cur <- rb_template_choice()
    cards <- lapply(names(REPORT_TEMPLATES), function(k) {
      t <- REPORT_TEMPLATES[[k]]
      active <- identical(cur, k)
      div(onclick = sprintf("Shiny.setInputValue('rb_pick_template','%s',{priority:'event'})", k),
          style = sprintf("background:#FFFFFF;border:1px solid %s;border-radius:10px;
                           padding:12px 14px;cursor:pointer;margin-bottom:8px;
                           %s",
                          if (active) "#6366F1" else "#EEF2F7",
                          if (active) "box-shadow:0 0 0 2px #C7D2FE;" else ""),
          div(style = "display:flex;justify-content:space-between;align-items:baseline;",
              span(style = "font-weight:600;color:#0F172A;font-size:13.5px;", t$label),
              if (active)
                span(style = "font-size:10px;color:#6366F1;font-weight:700;
                              text-transform:uppercase;letter-spacing:.5px;",
                     HTML("&#10003; Active"))),
          div(style = "font-size:11.5px;color:#64748B;margin-top:3px;",
              t$description))
    })
    tagList(
      tags$label(style = "font-size:11px;font-weight:600;color:#1B4F6B;
                          text-transform:uppercase;letter-spacing:.5px;",
                 "Template"),
      div(style = "margin-top:6px;", cards),
      div(style = "margin-top:10px;display:flex;gap:8px;",
          actionButton("rb_save_template", HTML("&#x1F4BE; Save as default for this trial"),
                       class = "btn btn-sm",
                       style = "font-size:11px;padding:5px 10px;background:#FFFFFF;
                                color:#1B4F6B;border:1px solid #DDE5EE;font-weight:500;"),
          actionButton("rb_reset_template", HTML("&#x21BA; Reset"),
                       class = "btn btn-sm",
                       style = "font-size:11px;padding:5px 10px;background:transparent;
                                color:#64748B;border:1px solid #DDE5EE;"))
    )
  })

  observeEvent(input$rb_pick_template, {
    k <- input$rb_pick_template
    if (!k %in% names(REPORT_TEMPLATES)) return()
    rb_template_choice(k)
    cfg <- rv$trial_config
    saved <- cfg$report_templates
    if (!is.null(saved) && !is.null(saved[[k]]) &&
        length(saved[[k]]$sections)) {
      rb_section_order(saved[[k]]$sections)
    } else {
      rb_section_order(REPORT_TEMPLATES[[k]]$sections)
    }
  })

  observeEvent(input$rb_reset_template, {
    k <- rb_template_choice()
    rb_section_order(REPORT_TEMPLATES[[k]]$sections)
    showNotification(sprintf("Reset to default %s sections.",
                             REPORT_TEMPLATES[[k]]$label),
                     type = "message", duration = 3)
  })

  # Sections UI: ordered list (reorderable) + checkboxes for off-list sections
  output$rb_sections_ui <- renderUI({
    chosen <- rb_section_order()
    chosen <- chosen[chosen %in% vapply(REPORT_SECTIONS, function(s) s$id, character(1))]

    chosen_block <- if (length(chosen) == 0) {
      div(style = "padding:14px;color:#94A3B8;font-style:italic;font-size:12px;",
          "No sections selected. Pick from below.")
    } else {
      rows <- lapply(seq_along(chosen), function(i) {
        sec <- report_section_by_id(chosen[i])
        div(style = "display:flex;align-items:center;gap:8px;
                     padding:8px 10px;background:#FFFFFF;border:1px solid #EEF2F7;
                     border-radius:8px;margin-bottom:6px;",
            span(style = "color:#94A3B8;font-size:11px;font-weight:600;
                          width:22px;text-align:center;", i),
            div(style = "flex:1;",
                div(style = "font-weight:500;color:#0F172A;font-size:13px;", sec$label),
                div(style = "font-size:10.5px;color:#94A3B8;", sec$group)),
            actionButton(paste0("rb_up_", chosen[i]), HTML("&uarr;"),
                         class = "btn btn-sm",
                         style = "padding:1px 7px;font-size:11px;background:#FFFFFF;
                                  border:1px solid #DDE5EE;color:#475569;"),
            actionButton(paste0("rb_down_", chosen[i]), HTML("&darr;"),
                         class = "btn btn-sm",
                         style = "padding:1px 7px;font-size:11px;background:#FFFFFF;
                                  border:1px solid #DDE5EE;color:#475569;"),
            actionButton(paste0("rb_remove_", chosen[i]), HTML("&times;"),
                         class = "btn btn-sm",
                         style = "padding:1px 7px;font-size:12px;background:#FFFFFF;
                                  border:1px solid #FECACA;color:#B91C1C;")
        )
      })
      tagList(rows)
    }

    avail <- setdiff(vapply(REPORT_SECTIONS, function(s) s$id, character(1)),
                     chosen)
    avail_chips <- if (length(avail)) {
      lapply(avail, function(id) {
        sec <- report_section_by_id(id)
        actionButton(paste0("rb_add_", id),
                     HTML(sprintf("&#43; %s", htmltools::htmlEscape(sec$label))),
                     class = "btn btn-sm",
                     style = "background:#F5F3FF;color:#6366F1;border:1px solid #C7D2FE;
                              font-size:11px;font-weight:500;padding:4px 10px;
                              margin:0 6px 6px 0;")
      })
    } else NULL

    tagList(
      tags$label(style = "font-size:11px;font-weight:600;color:#1B4F6B;
                          text-transform:uppercase;letter-spacing:.5px;",
                 "Sections (in order)"),
      div(style = "margin:6px 0 14px;", chosen_block),
      if (length(avail)) tagList(
        tags$label(style = "font-size:11px;font-weight:600;color:#64748B;
                            text-transform:uppercase;letter-spacing:.5px;",
                   "Add more"),
        div(style = "margin-top:6px;display:flex;flex-wrap:wrap;",
            avail_chips)
      )
    )
  })

  # Wire up + / - / up / down buttons (one observer per section id)
  lapply(REPORT_SECTIONS, function(sec) {
    id <- sec$id
    observeEvent(input[[paste0("rb_add_", id)]], {
      cur <- rb_section_order()
      if (!id %in% cur) rb_section_order(c(cur, id))
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("rb_remove_", id)]], {
      rb_section_order(setdiff(rb_section_order(), id))
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("rb_up_", id)]], {
      cur <- rb_section_order()
      i <- match(id, cur)
      if (!is.na(i) && i > 1) {
        cur[c(i - 1, i)] <- cur[c(i, i - 1)]
        rb_section_order(cur)
      }
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("rb_down_", id)]], {
      cur <- rb_section_order()
      i <- match(id, cur)
      if (!is.na(i) && i < length(cur)) {
        cur[c(i, i + 1)] <- cur[c(i + 1, i)]
        rb_section_order(cur)
      }
    }, ignoreInit = TRUE)
  })

  # Save the current section order as the trial's default for this template
  observeEvent(input$rb_save_template, {
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    k <- rb_template_choice()
    secs <- rb_section_order()
    existing <- cfg$report_templates %||% list()
    existing[[k]] <- list(sections = as.list(secs))
    tryCatch(update_overrides(cfg, report_templates = existing),
             error = function(e) message("rb save: ", e$message))
    rv$trial_config$report_templates <- existing
    showNotification(sprintf("Saved %s template for %s.",
                             REPORT_TEMPLATES[[k]]$label,
                             cfg$short_name %||% "this trial"),
                     type = "message", duration = 4)
  })

  # ════════════════════════════════════════════════════════════════════════
  # Generate Report Modal + Multi-format download
  # ════════════════════════════════════════════════════════════════════════
  rb_export_format <- reactiveVal("docx")

  observeEvent(input$rb_open_generate_modal, {
    cfg <- rv$trial_config
    if (is.null(cfg)) return()

    period <- if (!is.null(input$rpt_dates))
      sprintf("%s — %s",
              format(input$rpt_dates[1], "%b %Y"),
              format(input$rpt_dates[2], "%b %Y")) else "—"
    n_secs <- length(rb_section_order())

    showModal(modalDialog(
      title = NULL,
      size = "m", easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        downloadButton("rb_download",
                       uiOutput("rb_download_label", inline = TRUE),
                       class = "btn",
                       style = "background:#1B4F6B;color:#fff;border:none;
                                font-weight:600;padding:10px 22px;border-radius:8px;
                                font-size:13px;")
      ),

      div(
        tags$h2(class = "rpt-modal-title",
                sprintf("Generate %s report", rb_template_choice())),
        div(class = "rpt-modal-sub",
            sprintf("%s · %s",
                    cfg$short_name %||% toupper(cfg$code), period)),

        div(class = "rpt-format-label", "Output format"),

        div(class = "rpt-format-grid",
            div(id = "fmt_docx", class = "rpt-format-card rpt-format-active",
                onclick = "Shiny.setInputValue('rb_set_format','docx',{priority:'event'});
                           $('.rpt-format-card').removeClass('rpt-format-active');
                           $(this).addClass('rpt-format-active');",
                div(class = "rpt-format-ext", ".docx"),
                div(class = "rpt-format-desc", "Editable Word document, page-formatted")),
            div(id = "fmt_pdf", class = "rpt-format-card",
                onclick = "Shiny.setInputValue('rb_set_format','pdf',{priority:'event'});
                           $('.rpt-format-card').removeClass('rpt-format-active');
                           $(this).addClass('rpt-format-active');",
                div(class = "rpt-format-ext", ".pdf"),
                div(class = "rpt-format-desc", "Print-ready PDF, A4 with chrome")),
            div(id = "fmt_html", class = "rpt-format-card",
                onclick = "Shiny.setInputValue('rb_set_format','html',{priority:'event'});
                           $('.rpt-format-card').removeClass('rpt-format-active');
                           $(this).addClass('rpt-format-active');",
                div(class = "rpt-format-ext", ".html"),
                div(class = "rpt-format-desc", "Self-contained HTML for archive"))
        ),

        div(class = "rpt-modal-info",
            sprintf("%d sections · ~%d pages · Filtered to reporting period",
                    n_secs, n_secs + 1))
      )
    ))
  })

  observeEvent(input$rb_set_format, {
    rb_export_format(input$rb_set_format)
  })

  output$rb_download_label <- renderUI({
    fmt <- rb_export_format()
    label <- switch(fmt,
                    "docx" = "Generate DOCX",
                    "pdf"  = "Generate PDF",
                    "html" = "Generate HTML",
                    "Generate")
    HTML(label)
  })

  # Generate (multi-format)
  output$rb_download <- downloadHandler(
    filename = function() {
      cfg <- rv$trial_config
      slug <- gsub("[^A-Za-z0-9]", "_", cfg$short_name %||% "trial")
      fmt <- rb_export_format()
      template_choice <- rb_template_choice()
      # TSC always emits docx (template is word-native)
      if (template_choice == "TSC") fmt <- "docx"
      ext <- switch(fmt, "docx" = "docx", "pdf" = "pdf", "html")
      sprintf("%s_%s_%s.%s",
              slug, template_choice, format(Sys.Date(), "%Y-%m-%d"), ext)
    },
    content = function(file) {
      cfg <- rv$trial_config
      fmt <- rb_export_format()
      template_choice <- rb_template_choice()

      # Portfolio review template renders directly from the section
      # registry — no Rmd. Build an HTML document, then convert per fmt.
      if (template_choice == "Portfolio") {
        period <- if (!is.null(input$rpt_dates))
          sprintf("%s – %s",
                  format(input$rpt_dates[1], "%d %b %Y"),
                  format(input$rpt_dates[2], "%d %b %Y")) else "—"
        ctx <- list(
          rv = rv, cfg = cfg,
          template_label = REPORT_TEMPLATES$Portfolio$label,
          period_label = period,
          prepared_by = input$prepared_by %||% rv$username,
          reviewed_by = input$reviewed_by,
          custom_text = input$rb_custom_text,
          next_period_text = input$rb_next_period,
          portfolio = portfolio_ctx()
        )
        secs <- rb_section_order()
        # Match the in-app preview: pin the design-mandated order so the
        # download mirrors what the user sees on screen.
        ordered_default <- c("pr_trial_summary", "pr_review_progress",
                             "pr_rag_status", "pr_recruitment",
                             "pr_milestones", "pr_database",
                             "pr_finance", "pr_issues")
        secs <- c(intersect(ordered_default, secs),
                  setdiff(secs, ordered_default))
        body_html <- paste(vapply(secs, function(id) {
          s <- report_section_by_id(id); if (is.null(s)) return("")
          tryCatch(s$render(ctx),
                   error = function(e) sprintf("<p>Failed: %s</p>", e$message))
        }, character(1)), collapse = "\n")
        trial_label <- htmltools::htmlEscape(cfg$short_name %||% cfg$code %||% "Trial")
        full_html <- paste0(
          "<!doctype html><html><head><meta charset='utf-8'>",
          sprintf("<title>%s — Portfolio Review</title>", trial_label),
          "<link href='https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Source+Serif+4:wght@400;600&family=JetBrains+Mono:wght@400;500&display=swap' rel='stylesheet'>",
          "<style>",
          "body{font-family:'Inter',system-ui,sans-serif;color:#0F1A24;",
          "background:#F4F6F9;font-size:13px;margin:0;padding:24px;}",
          ".pf-page{width:794px;background:#fff;margin:0 auto 24px;",
          "padding:18px 48px 28px;box-shadow:0 1px 2px rgba(15,26,36,.06),",
          "0 8px 24px rgba(15,26,36,.08);}",
          ".pf-page-meta{display:flex;justify-content:space-between;",
          "font-size:9.5px;color:#64748B;margin-bottom:12px;letter-spacing:.3px;}",
          ".pf-banner{background:#7030A0;color:#fff;font-size:15px;font-weight:700;",
          "letter-spacing:.04em;text-transform:uppercase;text-align:center;",
          "padding:10px 16px;border-radius:4px 4px 0 0;}",
          ".pf-header-block{border:1px solid #E2E8EE;border-radius:4px;overflow:hidden;}",
          ".pf-info-grid{display:grid;grid-template-columns:1fr 1fr;border-top:1px solid #E2E8EE;}",
          ".pf-info-row{display:grid;grid-template-columns:160px 1fr;border-bottom:1px solid #EEF2F6;}",
          ".pf-info-cell{padding:5px 10px;font-size:11px;line-height:1.4;}",
          ".pf-info-cell.label{font-weight:600;background:#F8FAFC;}",
          ".pf-info-cell.value{color:#27384A;}",
          ".pf-status-row{display:flex;gap:18px;padding:8px 10px;border:1px solid #E2E8EE;border-top:0;background:#fff;}",
          ".pf-check-item{display:flex;align-items:center;gap:6px;font-size:11px;}",
          ".pf-check-item.small{font-size:10.5px;}",
          ".pf-checkbox{width:14px;height:14px;border:1.5px solid #E2E8EE;border-radius:3px;",
          "display:inline-flex;align-items:center;justify-content:center;",
          "font-size:9px;color:#fff;background:#fff;line-height:1;}",
          ".pf-checkbox.sm{width:12px;height:12px;font-size:8px;}",
          ".pf-checkbox.checked{background:#7030A0;border-color:#7030A0;}",
          ".pf-summary{border:1px solid #E2E8EE;border-top:0;padding:8px 10px;background:#fff;border-radius:0 0 4px 4px;margin-bottom:10px;}",
          ".pf-summary-label{font-size:10.5px;font-weight:600;margin-bottom:4px;}",
          ".pf-summary-text{font-family:'Source Serif 4',Georgia,serif;font-size:11px;color:#27384A;line-height:1.55;}",
          ".pf-section{margin-bottom:10px;}",
          ".pf-section-banner{background:#7030A0;color:#fff;font-size:11.5px;",
          "font-weight:700;letter-spacing:.04em;padding:6px 12px;",
          "border-radius:4px 4px 0 0;text-transform:uppercase;}",
          ".pf-section-banner.alert{background:#7F1D1D;}",
          ".pf-progress-grid,.pf-rag-grid,.pf-chart-container,.pf-kv-grid,",
          ".pf-staffing-grid,.pf-issues-block{border:1px solid #E2E8EE;border-top:0;",
          "border-radius:0 0 4px 4px;background:#fff;}",
          ".pf-yn-row,.pf-meeting-row{display:grid;grid-template-columns:200px 1fr;",
          "border-bottom:1px solid #EEF2F6;align-items:center;}",
          ".pf-yn-label,.pf-meeting-label,.pf-further-label{font-size:10.5px;",
          "font-weight:500;padding:6px 10px;background:#F8FAFC;}",
          ".pf-yn-answer{display:flex;align-items:center;gap:14px;padding:6px 10px;}",
          ".pf-yn-date{font-size:10.5px;color:#64748B;margin-left:8px;}",
          ".pf-yn-date strong{color:#0F1A24;font-weight:600;}",
          ".pf-divider{height:1px;background:#E2E8EE;}",
          ".pf-meeting-dates{display:flex;gap:28px;padding:6px 10px;font-size:10.5px;color:#64748B;}",
          ".pf-meeting-dates strong{color:#0F1A24;font-weight:600;font-family:'JetBrains Mono',monospace;font-size:10px;}",
          ".pf-further-row{display:grid;grid-template-columns:200px 1fr;align-items:start;}",
          ".pf-further-text{font-size:10.5px;color:#27384A;line-height:1.55;padding:6px 10px;}",
          ".pf-rag-grid{display:flex;flex-direction:column;}",
          ".pf-rag-item{display:flex;align-items:center;gap:10px;padding:7px 12px;border-bottom:1px solid #EEF2F6;}",
          ".pf-rag-item:last-child{border-bottom:0;}",
          ".pf-rag-dot{width:10px;height:10px;border-radius:50%;flex-shrink:0;}",
          ".pf-rag-text{font-size:10.5px;color:#64748B;line-height:1.4;}",
          ".pf-chart-container{padding:12px 14px;}",
          ".pf-chart-placeholder{display:flex;flex-direction:column;align-items:center;justify-content:center;height:200px;border:2px dashed #E2E8EE;border-radius:6px;background:#F8FAFC;gap:8px;}",
          ".pf-chart-placeholder-text{font-size:12px;color:#64748B;font-weight:500;}",
          ".pf-chart-placeholder-sub{font-size:10.5px;color:#94A3B8;}",
          ".pf-recruit-stats{display:grid;grid-template-columns:repeat(4,1fr);margin-top:8px;border-top:1px solid #E2E8EE;border-bottom:1px solid #E2E8EE;}",
          ".pf-recruit-stat{padding:7px 10px;border-right:1px solid #EEF2F6;display:flex;flex-direction:column;}",
          ".pf-recruit-stat:last-child{border-right:0;}",
          ".pf-recruit-stat .k{font-size:9px;font-weight:600;color:#64748B;text-transform:uppercase;letter-spacing:.5px;}",
          ".pf-recruit-stat .v{font-size:16px;font-weight:700;color:#0F1A24;font-variant-numeric:tabular-nums;}",
          ".pf-table{width:100%;border-collapse:collapse;font-size:10.5px;border:1px solid #E2E8EE;border-top:0;border-radius:0 0 4px 4px;overflow:hidden;}",
          ".pf-table thead th{font-size:9px;font-weight:600;color:#64748B;text-transform:uppercase;letter-spacing:.6px;text-align:left;padding:7px 10px;border-bottom:1.5px solid #0F1A24;background:#F8FAFC;}",
          ".pf-table tbody td{padding:6px 10px;border-bottom:1px solid #EEF2F6;color:#27384A;}",
          ".pf-table tbody td.mono{font-family:'JetBrains Mono',monospace;font-size:10px;}",
          ".pf-pill{display:inline-flex;font-size:9.5px;font-weight:600;padding:1px 7px;border-radius:10px;text-transform:uppercase;letter-spacing:.3px;}",
          ".pf-pill.green{background:#D1FAE5;color:#065F46;}",
          ".pf-pill.amber{background:#FEF3C7;color:#92400E;}",
          ".pf-pill.grey{background:#EEF2F6;color:#27384A;}",
          ".pf-pill.red{background:#FEE2E2;color:#991B1B;}",
          ".pf-data-capture{display:flex;align-items:center;gap:8px;}",
          ".pf-dc-bar{flex:1;height:6px;background:#EEF2F6;border-radius:3px;overflow:hidden;max-width:120px;}",
          ".pf-dc-fill{height:100%;background:#7030A0;}",
          ".pf-dc-label{font-family:'JetBrains Mono',monospace;font-size:10.5px;color:#0F1A24;font-weight:600;}",
          ".pf-kv-row,.pf-staff-row{display:grid;grid-template-columns:240px 1fr;border-bottom:1px solid #EEF2F6;}",
          ".pf-kv-row:last-child,.pf-staff-row:last-child{border-bottom:0;}",
          ".pf-kv-label,.pf-staff-label{font-size:10.5px;font-weight:500;padding:5px 10px;background:#F8FAFC;}",
          ".pf-kv-value{font-family:'JetBrains Mono',monospace;font-size:10.5px;color:#27384A;padding:5px 10px;}",
          ".pf-staff-value{font-size:10.5px;color:#27384A;padding:6px 10px;line-height:1.5;}",
          ".pf-issues-block{display:grid;grid-template-columns:1fr 1fr;}",
          ".pf-issues-col{padding:8px 12px;border-right:1px solid #EEF2F6;}",
          ".pf-issues-col:last-child{border-right:0;}",
          ".pf-issues-heading{font-size:10px;font-weight:600;text-transform:uppercase;letter-spacing:.5px;margin-bottom:6px;padding-bottom:4px;border-bottom:1px solid #EEF2F6;}",
          ".pf-issues-list{margin:0;padding:0 0 0 14px;font-size:10.5px;color:#27384A;line-height:1.55;}",
          ".pf-issues-list li{margin-bottom:6px;}",
          "@media print{body{background:#fff;padding:0;}.pf-page{box-shadow:none;width:100%;page-break-after:always;}}",
          "</style></head><body>",
          sprintf("<div class='pf-page'><div class='pf-page-meta'><span>Birmingham Clinical Trials Unit</span><span>Version 4.0 · %s</span></div>%s</div>",
                  htmltools::htmlEscape(period), body_html),
          "</body></html>")

        tmp_html <- tempfile(fileext = ".html")
        writeLines(full_html, tmp_html, useBytes = TRUE)

        if (fmt == "html") {
          file.copy(tmp_html, file, overwrite = TRUE)
        } else if (fmt == "pdf" && requireNamespace("chromote", quietly = TRUE)) {
          tryCatch({
            b <- chromote::ChromoteSession$new()
            on.exit(try(b$close(), silent = TRUE), add = TRUE)
            b$Page$navigate(paste0("file://", normalizePath(tmp_html)))
            Sys.sleep(1.5)
            pdf_data <- b$Page$printToPDF(
              paperWidth = 8.27, paperHeight = 11.69,
              marginTop = 0.4, marginBottom = 0.4,
              marginLeft = 0.4, marginRight = 0.4,
              printBackground = TRUE)
            writeBin(jsonlite::base64_dec(pdf_data$data), file)
          }, error = function(e) {
            file.copy(tmp_html, file, overwrite = TRUE)
            showNotification(paste("PDF failed:", e$message,
                                   "— delivered HTML instead."),
                             type = "warning", duration = 10)
          })
        } else if (fmt == "docx") {
          # Editable Word: bake charts in + branded reference doc (same path
          # as the TMG/iTMG download) so text, tables and charts come through.
          ok_docx <- tryCatch(
            html_to_editable_docx(tmp_html, file),
            error = function(e) { message("Portfolio DOCX: ", conditionMessage(e)); FALSE })
          if (!isTRUE(ok_docx)) {
            file.copy(tmp_html, file, overwrite = TRUE)
            showNotification("DOCX generation failed — delivered HTML instead.",
                             type = "warning", duration = 10)
          }
        } else {
          # pdf requested but chromote unavailable: best-effort pandoc to docx
          tryCatch({
            pandoc_dir <- Sys.getenv("RSTUDIO_PANDOC")
            pandoc_bin <- if (nzchar(pandoc_dir)) file.path(pandoc_dir, "pandoc") else "pandoc"
            tmp_out <- tempfile(fileext = ".docx")
            system2(pandoc_bin,
                    args = c(shQuote(tmp_html), "-f", "html",
                             "-t", "docx", "-o", shQuote(tmp_out)),
                    stdout = TRUE, stderr = TRUE)
            if (!file.exists(tmp_out) || file.info(tmp_out)$size == 0)
              stop("pandoc produced no output")
            file.copy(tmp_out, file, overwrite = TRUE)
          }, error = function(e) {
            file.copy(tmp_html, file, overwrite = TRUE)
            showNotification(paste("Export failed:", e$message,
                                   "— delivered HTML instead."),
                             type = "warning", duration = 10)
          })
        }
        removeModal()
        return(invisible())
      }

      # Resolve which Rmd to use. TMG and iTMG share tonic_report.Rmd
      # (the Rmd reads `report_type` to switch headers). TSC has its own.
      rmd_kind <- if (template_choice == "TSC") "tsc" else "tonic"
      if (template_choice == "TSC") fmt <- "docx"   # TSC is always docx

      rmd_src <- resolve_report_template(cfg, rmd_kind)
      if (is.null(rmd_src)) {
        showNotification(
          sprintf("Report template missing: %s", report_template_filename(rmd_kind)),
          type = "error", duration = 8)
        return()
      }

      # Build the report_data the same way the legacy download_report does
      # (same prepare_report_data() pipeline so output is identical).
      # Default scope is full-trial: only apply date/site filters when the
      # user has explicitly switched to "Filtered". This keeps every section
      # (demographics, site-by-site recruitment, monthly counts) consistent
      # with the headline total instead of showing a stale rolling window.
      use_filters    <- identical(input$rb_scope, "filtered")
      sites_for_prep <- if (use_filters) input$rpt_sites else NULL
      from_for_prep  <- if (use_filters) input$rpt_dates[1] else NULL
      to_for_prep    <- if (use_filters) input$rpt_dates[2] else NULL

      latest_crf_path <- tryCatch({
        p <- latest_return_rate_file(
          dir        = rv$trial_config$return_rates_dir,
          trial_code = rv$trial_config$code
        )
        if (is.null(p) || !file.exists(p)) NULL else p
      }, error = function(e) NULL)

      report_data <- tryCatch(
        prepare_report_data(
          df                = rv$raw_redcap,
          selected_sites    = sites_for_prep,
          date_from         = from_for_prep,
          date_to           = to_for_prep,
          include_withdrawn = isTRUE(input$include_withdrawn),
          pipeline_df       = rv$sites,
          crf_csv_path      = latest_crf_path),
        error = function(e) {
          showNotification(
            paste("Cannot generate report:", conditionMessage(e)),
            type = "error", duration = 12)
          NULL
        })
      if (is.null(report_data)) return(invisible())

      site_label <- if (is.null(sites_for_prep) || !length(sites_for_prep))
        "All sites" else paste(sites_for_prep, collapse = ", ")

      # Stage Rmd + helpers in a temp dir so render() can find them
      tmp_dir  <- tempdir()
      rmd_dest <- file.path(tmp_dir, basename(rmd_src))
      file.copy(rmd_src, rmd_dest, overwrite = TRUE)
      for (h in c("functions/flat_completeness.R",
                  "functions/baseline_table.R",
                  "functions/consort_flow.R",
                  "functions/tsc_charts.R",
                  "www/BlackText-landscape.png")) {   # BCTU header logo
        if (file.exists(h))
          file.copy(h, file.path(tmp_dir, basename(h)), overwrite = TRUE)
      }

      # Ensure pandoc is findable (rmarkdown + the docx fallback both need it).
      # Cross-platform — handles macOS RStudio bundles, Windows installs, and
      # Linux. If we still can't find it, surface a clear error so the user
      # knows they need to install pandoc rather than seeing the cryptic
      # "cannot open connection" from rmarkdown.
      if (!ensure_pandoc()) {
        showNotification(
          HTML("Pandoc not found. Install it (https://pandoc.org/installing.html)
                or open this app from inside RStudio so it can find the bundled
                pandoc."),
          type = "error", duration = 12)
        return()
      }

      # ── TSC branch: render the word-native template directly ──────────
      if (rmd_kind == "tsc") {
        tryCatch({
          rmarkdown::render(
            input             = rmd_dest,
            output_file       = file,
            output_format     = "word_document",
            params = filter_params_for_rmd(list(
              report_data         = report_data,
              selected_sites      = site_label,
              date_from           = from_for_prep,
              date_to             = to_for_prep,
              crf_csv_path        = latest_crf_path,
              report_date         = format(Sys.Date(), "%d %B %Y"),
              prepared_by         = input$prepared_by %||% rv$username,
              reviewed_by         = input$reviewed_by,
              completeness_style  = input$completeness_style %||% "heatmap",
              report_content      = cfg$report_content,
              logo_path           = resolve_logo_path(cfg)
            ), rmd_dest),
            envir             = new.env(parent = globalenv()),
            intermediates_dir = tmp_dir,
            clean             = TRUE,
            quiet             = TRUE
          )
        }, error = function(e) {
          showNotification(paste("TSC render failed:", e$message),
                           type = "error", duration = 12)
        })
        return(invisible())
      }

      # ── TMG / iTMG branch: render the Rmd to HTML, then convert ────────
      # The tonic_report.Rmd is HTML-styled; PDF preserves formatting via
      # chromote (it's just printing the HTML). DOCX via pandoc loses most
      # styling — we surface a warning so the user knows.
      report_type_param <- if (template_choice == "iTMG") "iTMG" else "TMG"
      html_out <- file.path(tmp_dir, "tonic_rendered.html")

      ok <- tryCatch({
        rmarkdown::render(
          input             = rmd_dest,
          output_file       = html_out,
          output_format     = "html_document",
          params = filter_params_for_rmd(list(
            report_data         = report_data,
            selected_sites      = site_label,
            date_from           = from_for_prep,
            date_to             = to_for_prep,
            include_withdrawn   = isTRUE(input$include_withdrawn),
            include_appendix    = isTRUE(input$report_appendix),
            report_date         = format(Sys.Date(), "%d %B %Y"),
            logo_path           = resolve_logo_path(cfg),
            crf_csv_path        = latest_crf_path,
            report_type         = report_type_param,
            completeness_style  = input$completeness_style %||% "heatmap",
            report_content      = cfg$report_content
          ), rmd_dest),
          envir             = new.env(parent = globalenv()),
          intermediates_dir = tmp_dir,
          clean             = TRUE,
          quiet             = TRUE
        )
        TRUE
      }, error = function(e) {
        showNotification(paste("Report render failed:", e$message),
                         type = "error", duration = 12)
        FALSE
      })
      if (!isTRUE(ok)) return(invisible())

      if (fmt == "html") {
        file.copy(html_out, file, overwrite = TRUE)

      } else if (fmt == "pdf") {
        # Print the rendered HTML to PDF via headless Chrome — preserves
        # CSS / page sizing exactly, which is the whole point.
        tryCatch({
          if (!requireNamespace("chromote", quietly = TRUE))
            stop("chromote not installed (install.packages('chromote'))")
          b <- chromote::ChromoteSession$new()
          on.exit(try(b$close(), silent = TRUE), add = TRUE)
          b$Page$navigate(paste0("file://", normalizePath(html_out)))
          Sys.sleep(2)   # let webfonts + base64 logos resolve
          pdf_data <- b$Page$printToPDF(
            landscape       = FALSE,
            paperWidth      = 8.27, paperHeight = 11.69,   # A4 in inches
            marginTop       = 0.31, marginBottom = 0.31,
            marginLeft      = 0.31, marginRight  = 0.31,
            printBackground = TRUE,
            preferCSSPageSize = TRUE
          )
          writeBin(jsonlite::base64_dec(pdf_data$data), file)
        }, error = function(e) {
          # Fallback: deliver HTML and surface why
          file.copy(html_out, file, overwrite = TRUE)
          showNotification(paste("PDF generation failed:", e$message,
                                 "— delivered HTML instead."),
                           type = "warning", duration = 10)
        })

      } else {
        # DOCX: the TMG/iTMG template is HTML-styled with JS-drawn charts, so a
        # plain pandoc conversion loses every chart and all layout. Instead we
        # bake the charts into the HTML via headless Chrome (the same engine
        # that makes the PDF perfect) and convert with a branded reference doc,
        # producing an *editable* Word doc with real text, tables and charts.
        ok_docx <- tryCatch(
          html_to_editable_docx(html_out, file),
          error = function(e) { message("DOCX convert: ", conditionMessage(e)); FALSE })
        if (isTRUE(ok_docx)) {
          showNotification(
            HTML("DOCX generated — editable text, tables and charts. The
                  coloured banner bars and multi-column card layouts can't be
                  reproduced exactly in Word; use PDF if you need a pixel match
                  to the on-screen report."),
            type = "message", duration = 10)
        } else {
          file.copy(html_out, file, overwrite = TRUE)
          showNotification(
            "DOCX generation failed — delivered HTML instead.",
            type = "warning", duration = 10)
        }
      }

      removeModal()
    }
  )

  # ════════════════════════════════════════════════════════════════════════
  # New design (workbench) — outputs that drive the 3-column layout
  # ════════════════════════════════════════════════════════════════════════
  rb_active_btab <- reactiveVal("sections")
  observeEvent(input$rb_active_btab, {
    rb_active_btab(input$rb_active_btab)
  })

  output$rb_trial_card <- renderUI({
    cfg <- rv$trial_config
    if (is.null(cfg))
      return(div(style = "color:#94A3B8;font-size:12px;font-style:italic;
                          padding:12px 4px;", "No trial selected."))
    n <- length(rv$participants$record_id %||% character(0))
    if (n == 0) n <- 0
    target <- cfg$trial_target %||% 0L
    pct <- if (target > 0) round(n / target * 100) else 0
    div(class = "rb-trial-card",
        div(class = "rb-trial-mark",
            substring(toupper(cfg$short_name %||% cfg$code %||% "T"), 1, 2)),
        div(style = "flex:1;min-width:0;",
            div(class = "rb-trial-meta-k", "Current trial"),
            div(class = "rb-trial-meta-name", cfg$short_name %||% toupper(cfg$code)),
            div(class = "rb-trial-meta-sub",
                sprintf("%d / %d randomised · %d%%", n, target, pct))))
  })

  output$rb_template_seg <- renderUI({
    cur <- rb_template_choice()
    keys <- names(REPORT_TEMPLATES)
    div(class = "rb-seg",
        lapply(keys, function(k) {
          tags$button(
            id = paste0("rb_pick_", k),
            class = paste("action-button", if (identical(k, cur)) "on" else ""),
            type = "button",
            onclick = sprintf("Shiny.setInputValue('rb_pick_template','%s',{priority:'event'})", k),
            k)
        }))
  })

  output$rb_template_desc <- renderUI({
    k <- rb_template_choice()
    div(class = "rb-tdesc",
        REPORT_TEMPLATES[[k]]$description %||% "")
  })

  output$rb_summary_tiles <- renderUI({
    cfg <- rv$trial_config
    n_rand <- length(unique(rv$participants$record_id %||% character(0)))
    n_sites <- nrow(rv$sites %||% data.frame())
    n_open  <- sum(rv$sites$status %in% c("Open", "Recruiting"), na.rm = TRUE)
    n_amend <- length(amendments_state())

    tile <- function(label, value, sub) {
      div(class = "rb-stat-tile",
          div(class = "rb-stat-label", label),
          div(class = "rb-stat-value", value),
          div(class = "rb-stat-sub", sub))
    }

    div(class = "rb-stat-row",
        tile("Randomised",   n_rand,  "across the trial"),
        tile("Active sites", n_open,  sprintf("of %d", n_sites)),
        tile("Insights",
             length(tryCatch(
               compute_insights(rv$raw_redcap, rv$sites, cfg %||% list()),
               error = function(e) list())),
             "auto-detected"),
        tile("Amendments",   n_amend, "in register"))
  })

  output$rb_canvas_title <- renderText({
    cfg <- rv$trial_config
    sprintf("%s %s Report",
            cfg$short_name %||% toupper(cfg$code %||% "Trial"),
            rb_template_choice())
  })

  output$rb_canvas_meta <- renderText({
    secs <- rb_section_order()
    sprintf("· %d sections · A4 · classic", length(secs))
  })

  # ── TMG / iTMG preview: render the Rmd live and embed in an iframe ──────
  # The TMG/iTMG download path uses tonic_report.Rmd. We render the same Rmd
  # for the on-screen preview so what the user sees matches what they
  # download (byte-for-byte for HTML format).
  tmg_preview_state <- reactiveValues(html = NULL, error = NULL, rendering = FALSE)

  render_tmg_preview_html <- function() {
    cfg <- rv$trial_config
    if (is.null(cfg)) return(NULL)
    tmpl_choice <- rb_template_choice()
    if (!tmpl_choice %in% c("TMG", "iTMG")) return(NULL)

    rmd_src <- resolve_report_template(cfg, "tonic")
    if (is.null(rmd_src)) {
      tmg_preview_state$error <- "TMG report template not found."
      return(NULL)
    }
    if (!ensure_pandoc()) {
      tmg_preview_state$error <- "Pandoc not found — preview unavailable."
      return(NULL)
    }

    # Default scope is full-trial (see the generate handler above) so the
    # live preview matches the headline totals rather than a rolling window.
    use_filters    <- identical(input$rb_scope, "filtered")
    sites_for_prep <- if (use_filters) input$rpt_sites else NULL
    from_for_prep  <- if (use_filters) input$rpt_dates[1] else NULL
    to_for_prep    <- if (use_filters) input$rpt_dates[2] else NULL

    latest_crf_path <- tryCatch({
      p <- latest_return_rate_file(
        dir        = rv$trial_config$return_rates_dir,
        trial_code = rv$trial_config$code
      )
      if (is.null(p) || !file.exists(p)) NULL else p
    }, error = function(e) NULL)

    report_data <- tryCatch(
      prepare_report_data(
        df                = rv$raw_redcap,
        selected_sites    = sites_for_prep,
        date_from         = from_for_prep,
        date_to           = to_for_prep,
        include_withdrawn = isTRUE(input$include_withdrawn),
        pipeline_df       = rv$sites,
        crf_csv_path      = latest_crf_path),
      error = function(e) { tmg_preview_state$error <- e$message; NULL })
    if (is.null(report_data)) return(NULL)

    tmp_dir  <- tempfile("tmg_preview_"); dir.create(tmp_dir)
    rmd_dest <- file.path(tmp_dir, basename(rmd_src))
    file.copy(rmd_src, rmd_dest, overwrite = TRUE)
    for (h in c("functions/flat_completeness.R",
                "functions/baseline_table.R",
                "functions/consort_flow.R",
                "www/BlackText-landscape.png")) {   # BCTU header logo
      if (file.exists(h))
        file.copy(h, file.path(tmp_dir, basename(h)), overwrite = TRUE)
    }
    html_out <- file.path(tmp_dir, "tmg_preview.html")

    site_label <- if (is.null(sites_for_prep) || !length(sites_for_prep))
      "All sites" else paste(sites_for_prep, collapse = ", ")

    ok <- tryCatch({
      rmarkdown::render(
        input         = rmd_dest,
        output_file   = html_out,
        output_format = "html_document",
        params = filter_params_for_rmd(list(
          report_data       = report_data,
          selected_sites    = site_label,
          date_from         = from_for_prep,
          date_to           = to_for_prep,
          include_withdrawn = isTRUE(input$include_withdrawn),
          include_appendix  = isTRUE(input$report_appendix),
          report_date       = format(Sys.Date(), "%d %B %Y"),
          logo_path         = resolve_logo_path(cfg),
          crf_csv_path      = latest_crf_path,
          report_type       = tmpl_choice,
          completeness_style = input$completeness_style %||% "heatmap",
          report_content    = cfg$report_content
        ), rmd_dest),
        envir = new.env(parent = globalenv()),
        intermediates_dir = tmp_dir, clean = TRUE, quiet = TRUE)
      TRUE
    }, error = function(e) { tmg_preview_state$error <- e$message; FALSE })

    if (!isTRUE(ok) || !file.exists(html_out)) return(NULL)
    paste(readLines(html_out, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }

  # Re-render when inputs that affect the report change. Debounced so
  # rapid changes don't trigger a flood of renders.
  preview_trigger <- reactive({
    list(rb_template_choice(),
         rv$trial_config$code,
         input$rb_scope,
         input$rpt_sites,
         input$rpt_dates,
         input$include_withdrawn,
         input$report_appendix,
         input$completeness_style,
         nrow(rv$raw_redcap %||% data.frame()),
         length(rv$trial_config$report_content))
  }) |> debounce(800)

  observeEvent(preview_trigger(), {
    tryCatch({
      tmpl <- rb_template_choice()
      if (!tmpl %in% c("TMG", "iTMG")) return()
      cfg <- rv$trial_config
      if (is.null(cfg)) return()
      tmg_preview_state$rendering <- TRUE
      tmg_preview_state$error <- NULL
      html <- tryCatch(render_tmg_preview_html(),
                       error = function(e) {
                         tmg_preview_state$error <- conditionMessage(e)
                         message("TMG preview render failed: ", conditionMessage(e))
                         NULL
                       })
      tmg_preview_state$rendering <- FALSE
      tmg_preview_state$html <- html
    }, error = function(e) {
      message("TMG preview observer error: ", conditionMessage(e))
      tmg_preview_state$rendering <- FALSE
      tmg_preview_state$error <- conditionMessage(e)
    })
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  # Document preview: cover page + content pages, one per enabled section
  output$rb_document_preview <- renderUI({
    cfg <- rv$trial_config
    if (is.null(cfg))
      return(div(style = "padding:60px;color:#94A3B8;font-style:italic;",
                 "Select a trial to preview the report."))

    # TMG / iTMG → live Rmd render embedded as an iframe
    tmpl_choice <- rb_template_choice()
    if (tmpl_choice %in% c("TMG", "iTMG")) {
      if (isTRUE(tmg_preview_state$rendering))
        return(div(style = "padding:60px;text-align:center;color:#64748B;",
                   div(style="font-size:13px;font-weight:500;","Rendering preview…"),
                   div(style="font-size:11px;margin-top:6px;color:#94A3B8;",
                       "First render can take 10–20 seconds.")))
      err <- tmg_preview_state$error
      if (!is.null(err) && is.null(tmg_preview_state$html))
        return(div(style = "padding:40px;color:#B91C1C;font-size:13px;",
                   tags$b("Preview failed:"), tags$br(), tags$code(err),
                   tags$div(style="margin-top:10px;color:#64748B;font-size:12px;",
                            "Try clicking Download anyway, or restart the Shiny app.")))
      html <- tmg_preview_state$html
      if (is.null(html))
        return(div(style = "padding:60px;color:#94A3B8;font-style:italic;",
                   "Preparing preview…"))
      return(tags$iframe(
        srcdoc = html,
        style  = "width:100%;height:calc(100vh - 220px);min-height:760px;
                  border:1px solid #E2E8EE;border-radius:6px;background:#e8eef2;",
        sandbox = "allow-scripts allow-same-origin"))
    }

    secs   <- rb_section_order()
    template <- REPORT_TEMPLATES[[rb_template_choice()]]
    period <- if (!is.null(input$rpt_dates))
      sprintf("%s – %s",
              format(input$rpt_dates[1], "%d %b %Y"),
              format(input$rpt_dates[2], "%d %b %Y")) else "—"

    # Title page
    title_page <- div(class = "rb-page",
      div(class = "rb-titlepage",
        div(class = "rb-tp-stripe"),
        div(class = "rb-tp-top",
            div(class = "rb-tp-mark",
                div(class = "lm",
                    substring(toupper(cfg$short_name %||% "T"), 1, 1)),
                div(div(class = "ln-a", cfg$short_name %||% toupper(cfg$code)),
                    div(class = "ln-b", "BCTU Clinical Trials"))),
            div(class = "rb-tp-spons", "Sponsor",
                tags$strong(cfg$report_defaults$sponsor %||% "—"))),
        div(class = "rb-tp-mid",
            div(class = "rb-tp-eyebrow",
                span(class = "bar"),
                paste(rb_template_choice(), "Report")),
            tags$h1(class = "rb-tp-title",
                    template$label %||% "Trial Report"),
            tags$p(class = "rb-tp-sub", cfg$name %||% ""),
            div(class = "rb-tp-period",
                "Reporting period · ",
                tags$strong(period)),
            div(class = "rb-tp-meta",
                div(class = "item",
                    div(class = "k", "Trial code"),
                    div(class = "v", toupper(cfg$code %||% "—"))),
                div(class = "item",
                    div(class = "k", "Chief Investigator"),
                    div(class = "v", cfg$report_defaults$ci %||% "—")),
                div(class = "item",
                    div(class = "k", "Prepared by"),
                    div(class = "v", input$prepared_by %||% rv$username %||% "—")),
                div(class = "item",
                    div(class = "k", "Date generated"),
                    div(class = "v", format(Sys.Date(), "%d %B %Y"))))))
    )

    # Build content pages — one section per page using existing render functions.
    ctx <- list(
      rv = rv, cfg = cfg,
      template_label = template$label,
      period_label = period,
      prepared_by = input$prepared_by %||% rv$username,
      reviewed_by = input$reviewed_by,
      custom_text = input$rb_custom_text,
      next_period_text = input$rb_next_period,
      portfolio = portfolio_ctx()
    )

    # ── Portfolio template: render the design-matched two-page layout ──
    if (identical(rb_template_choice(), "Portfolio")) {
      render_pr <- function(id) {
        s <- report_section_by_id(id); if (is.null(s)) return("")
        tryCatch(s$render(ctx),
                 error = function(e)
                   sprintf("<p style='color:#B91C1C;'>Failed: %s</p>",
                           htmltools::htmlEscape(e$message)))
      }
      reporting_period <- period
      version_meta <- sprintf("Version 4.0 · %s", reporting_period)
      footer_meta  <- sprintf("%s · Portfolio Review · %s",
                              toupper(cfg$short_name %||% cfg$code %||% ""),
                              reporting_period)

      # Default split mirrors the design: page 1 = summary/progress/RAG,
      # page 2 = recruitment/milestones/db/finance/issues. Sections that
      # the user has added (e.g. custom_text) are appended to page 2.
      page1_ids <- intersect(secs,
        c("pr_trial_summary", "pr_review_progress", "pr_rag_status"))
      page2_ids <- intersect(secs,
        c("pr_recruitment", "pr_milestones", "pr_database",
          "pr_finance", "pr_issues"))
      remainder <- setdiff(secs, c(page1_ids, page2_ids))
      page2_ids <- c(page2_ids, remainder)

      build_page <- function(ids, page_no, total) {
        body_html <- paste(vapply(ids, render_pr, character(1)),
                           collapse = "")
        div(class = "rb-page pf-page",
            div(class = "rb-page-inner",
                div(class = "pf-page-meta",
                    span("Birmingham Clinical Trials Unit"),
                    span(version_meta)),
                HTML(body_html)),
            div(class = "rb-page-foot",
                span(footer_meta),
                span(sprintf("Page %d of %d", page_no, total))))
      }

      total <- 2
      pages <- list(
        build_page(page1_ids, 1, total),
        div(class = "rb-page-marker",
            span(class = "ln"), "Page break", span(class = "ln")),
        build_page(page2_ids, 2, total))
      return(do.call(tagList, pages))
    }

    content_pages <- lapply(seq_along(secs), function(i) {
      sec_id <- secs[i]
      sec    <- report_section_by_id(sec_id)
      if (is.null(sec)) return(NULL)
      html <- tryCatch(sec$render(ctx),
                       error = function(e)
                         sprintf("<p style='color:#B91C1C;'>Failed: %s</p>",
                                 htmltools::htmlEscape(e$message)))
      tagList(
        div(class = "rb-page-marker",
            span(class = "ln"),
            sprintf("Page %d", i + 1),
            span(class = "ln")),
        div(class = "rb-page",
            div(class = "rb-page-inner",
                div(class = "rb-doc",
                    div(class = "secblock",
                        div(class = "secblock-head",
                            span(class = "n", sprintf("§ %02d", i)),
                            tags$h2(sec$label)),
                        HTML(html)))),
            div(class = "rb-page-foot",
                span(sprintf("%s · %s",
                             cfg$short_name %||% toupper(cfg$code),
                             rb_template_choice())),
                span(sprintf("Page %d of %d", i + 1, length(secs) + 1))))
      )
    })

    do.call(tagList, c(list(title_page), content_pages))
  })

  # Builder body: switches based on active tab in the right rail.
  output$rb_builder_body <- renderUI({
    tab <- rb_active_btab() %||% "sections"

    if (identical(tab, "sections")) {
      chosen <- rb_section_order()
      all_ids <- vapply(REPORT_SECTIONS, function(s) s$id, character(1))
      avail <- setdiff(all_ids, chosen)

      list_block <- if (!length(chosen))
        div(style = "padding:14px;color:#94A3B8;font-size:12px;font-style:italic;",
            "No sections selected. Add some below.")
      else
        div(class = "rb-section-list",
            lapply(seq_along(chosen), function(i) {
              sec <- report_section_by_id(chosen[i])
              if (is.null(sec)) return(NULL)
              div(class = "rb-sec-item",
                  span(class = "rb-sec-handle", HTML("&#8942;&#8942;")),
                  tags$div(
                    class = "rb-sec-toggle on",
                    onclick = sprintf("Shiny.setInputValue('rb_remove_%s', Math.random(), {priority:'event'})",
                                      chosen[i])),
                  div(class = "rb-sec-meta",
                      div(class = "rb-sec-title", sec$label),
                      div(class = "rb-sec-group", sec$group)),
                  div(class = "rb-sec-arrows",
                      tags$button(class = "action-button", type = "button",
                                  onclick = sprintf("Shiny.setInputValue('rb_up_%s', Math.random(), {priority:'event'})",
                                                    chosen[i]),
                                  HTML("&#9650;")),
                      tags$button(class = "action-button", type = "button",
                                  onclick = sprintf("Shiny.setInputValue('rb_down_%s', Math.random(), {priority:'event'})",
                                                    chosen[i]),
                                  HTML("&#9660;"))),
                  span(class = "rb-sec-page", sprintf("p.%d", i + 1)))
            }))

      add_block <- if (length(avail))
        div(class = "rb-chips",
            lapply(avail, function(id) {
              sec <- report_section_by_id(id)
              tags$button(class = "rb-chip action-button", type = "button",
                          onclick = sprintf("Shiny.setInputValue('rb_add_%s', Math.random(), {priority:'event'})",
                                            id),
                          paste0("+ ", sec$label))
            }))
      else
        span(style = "font-size:11.5px;color:#64748B;font-style:italic;",
             "All sections in report.")

      tagList(
        tags$h4("In this report"),
        list_block,
        tags$h4("Add section"),
        add_block,
        div(style = "margin-top:18px;display:flex;gap:8px;",
            actionButton("rb_save_template",
                         HTML("&#x1F4BE; Save"),
                         class = "action-button",
                         style = "flex:1;background:#fff;color:#1B4F6B;
                                  border:1px solid #E2E8EE;font-size:11.5px;
                                  font-weight:500;padding:6px 10px;border-radius:6px;"),
            actionButton("rb_reset_template",
                         HTML("&#x21BA; Reset"),
                         class = "action-button",
                         style = "flex:1;background:transparent;color:#64748B;
                                  border:1px solid #E2E8EE;font-size:11.5px;
                                  padding:6px 10px;border-radius:6px;"))
      )

    } else if (identical(tab, "narrative")) {
      tagList(
        tags$h4("Custom text / notes"),
        tags$textarea(class = "rb-ta", id = "rb_custom_text", rows = 4,
                      placeholder = "Notes for the Custom Text section…",
                      input$rb_custom_text %||% ""),
        tags$h4("Plans for next reporting period"),
        tags$textarea(class = "rb-ta", id = "rb_next_period", rows = 6,
                      placeholder = "Plans for next reporting period…",
                      input$rb_next_period %||% ""),
        tags$script(HTML("
          $(document).off('input.rbnar').on('input.rbnar',
            '#rb_custom_text, #rb_next_period', function(){
              Shiny.setInputValue(this.id, this.value, {priority:'event'});
          });"))
      )

    } else if (identical(tab, "meeting")) {
      tagList(
        tags$h4("Meeting details"),
        div(class = "rb-field",
            tags$label("Date generated"),
            tags$input(class = "rb-rail-input", type = "text",
                       value = format(Sys.Date(), "%d %b %Y"), readonly = "readonly")),
        div(class = "rb-field",
            tags$label("Prepared by"),
            tags$input(id = "prepared_by", class = "rb-rail-input action-button",
                       type = "text",
                       value = input$prepared_by %||% rv$username %||% "")),
        div(class = "rb-field",
            tags$label("Reviewed by"),
            tags$input(id = "reviewed_by", class = "rb-rail-input action-button",
                       type = "text",
                       value = input$reviewed_by %||% "")),
        tags$script(HTML("
          $(document).off('input.rbmtg').on('input.rbmtg',
            '#prepared_by, #reviewed_by', function(){
              Shiny.setInputValue(this.id, this.value, {priority:'event'});
          });"))
      )

    } else if (identical(tab, "amend")) {
      items <- amendments_state()
      sub    <- Filter(function(a) identical(a$type, "Substantial"), items)
      nonsub <- Filter(function(a) !identical(a$type, "Substantial"), items)

      render_a <- function(a, css_cls) {
        div(class = "rb-amend",
            div(class = "rb-amend-head",
                span(class = "rb-amend-ref",
                     paste0(a$ref %||% "—", " · ", a$date %||% "")),
                span(class = paste("rb-amend-status", css_cls),
                     a$status %||% "Pending")),
            div(class = "rb-amend-desc", a$description %||% ""))
      }

      tagList(
        tags$h4(sprintf("Substantial · %d", length(sub))),
        if (length(sub)) lapply(sub, render_a, css_cls = "sub")
        else span(style = "font-size:11.5px;color:#64748B;font-style:italic;",
                  "None tracked yet."),
        tags$h4(sprintf("Non-substantial · %d", length(nonsub))),
        if (length(nonsub)) lapply(nonsub, render_a, css_cls = "nonsub")
        else span(style = "font-size:11.5px;color:#64748B;font-style:italic;",
                  "None tracked yet."),
        actionButton("amend_add",
                     HTML("&#43; Add amendment"),
                     class = "rb-add-btn action-button",
                     style = "margin-top:12px;background:transparent;
                              border:1px dashed #E2E8EE;color:#64748B;
                              border-radius:6px;padding:7px;width:100%;")
      )

    } else if (identical(tab, "portfolio")) {
      # Variable per-report fields for the Portfolio review template:
      # TSC / DMC / TMG meeting dates (previous + next), RAG status,
      # narrative for issues / remedial actions / further info, and
      # per-report milestone / finance overrides.
      fld <- function(id, label, ph = "") {
        div(class = "rb-field",
            tags$label(label),
            tags$input(id = id, class = "rb-rail-input action-button",
                       type = "text", placeholder = ph,
                       value = input[[id]] %||% ""))
      }
      ta <- function(id, label, ph = "", rows = 4) {
        div(class = "rb-field",
            tags$label(label),
            tags$textarea(id = id, class = "rb-ta", rows = rows,
                          placeholder = ph, input[[id]] %||% ""))
      }
      rag <- input$pr_rag_status %||% ""
      rag_btn <- function(val, colour, label) {
        on <- if (identical(tolower(rag), tolower(val))) "color:#fff;" else
              "background:#fff;color:#0F172A;"
        sprintf("<button type='button' class='action-button' id='rb_pr_rag_%s'
                          style='flex:1;padding:8px;border-radius:6px;
                                  border:1px solid %s;font-weight:600;
                                  font-size:11.5px;cursor:pointer;
                                  background:%s;%s'>%s</button>",
                val, colour,
                if (identical(tolower(rag), tolower(val))) colour else "#fff",
                on, label)
      }
      tagList(
        tags$h4("RAG status"),
        HTML(sprintf("<div style='display:flex;gap:6px;margin-bottom:10px;'>%s%s%s</div>",
                     rag_btn("red",   "#DC2626", "Red"),
                     rag_btn("amber", "#F59E0B", "Amber"),
                     rag_btn("green", "#16A34A", "Green"))),
        ta("pr_rag_note",
           "RAG rationale (optional)",
           "Brief comment from team leader on RAG choice"),

        tags$h4("TMG meeting"),
        fld("pr_tmg_prev", "Previous", "e.g. 12 Mar 2026"),
        fld("pr_tmg_next", "Next",     "e.g. 18 Jun 2026"),

        tags$h4("TSC meeting"),
        fld("pr_tsc_prev", "Previous", "e.g. 04 Feb 2026"),
        fld("pr_tsc_next", "Next",     "e.g. 09 Jul 2026"),

        tags$h4("DMC / DMEC meeting"),
        fld("pr_dmc_prev", "Previous", "e.g. 21 Jan 2026"),
        fld("pr_dmc_next", "Next",     "e.g. 22 Jul 2026"),

        tags$h4("Further information"),
        ta("pr_further_info", NULL,
           "Any further notes on review of progress", rows = 4),

        tags$h4("Major issues of concern"),
        ta("pr_issues_concerns", "Issues of concern (bullet points)",
           "- Issue 1\n- Issue 2", rows = 5),
        ta("pr_issues_remedial", "Current / planned remedial action",
           "- Action 1\n- Action 2", rows = 5),

        tags$h4("Milestone progress (this report)"),
        fld("pr_ms_approvals",    "Trial approvals submitted/in place"),
        fld("pr_ms_recruitment",  "Recruitment milestone"),
        fld("pr_ms_data_capture", "% Data capture"),
        fld("pr_ms_other",        "Other"),

        tags$h4("Finance & staffing (this report)"),
        fld("pr_fin_staffing_status", "Staffing status",
            "e.g. all staff in post / recruiting for X"),
        fld("pr_fin_status", "Current financial status",
            "On track / Underspent / Overspent"),

        tags$script(HTML("
          $(document).off('input.rbpr').on('input.rbpr',
            '#rb_panel input.rb-rail-input, #rb_panel textarea.rb-ta', function(){
              if (!this.id || this.id.indexOf('pr_') !== 0) return;
              Shiny.setInputValue(this.id, this.value, {priority:'event'});
          });
          $(document).off('click.rbprrag').on('click.rbprrag',
            '#rb_pr_rag_red, #rb_pr_rag_amber, #rb_pr_rag_green', function(){
              var v = this.id.replace('rb_pr_rag_','');
              Shiny.setInputValue('pr_rag_status', v, {priority:'event'});
          });
        "))
      )
    }
  })

  # Collect the portfolio panel inputs into a single list, ready to drop
  # into the render ctx so the .rs_render_pr_* functions can read them.
  portfolio_ctx <- reactive({
    keys <- c("rag_status", "rag_note",
              "tmg_prev", "tmg_next",
              "tsc_prev", "tsc_next",
              "dmc_prev", "dmc_next",
              "further_info",
              "issues_concerns", "issues_remedial",
              "ms_approvals", "ms_recruitment",
              "ms_data_capture", "ms_other",
              "fin_staffing_status", "fin_status")
    out <- list()
    for (k in keys) out[[k]] <- input[[paste0("pr_", k)]]
    out
  })
}
