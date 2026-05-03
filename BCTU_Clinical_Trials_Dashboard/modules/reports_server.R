reports_server <- function(input, output, session, state) {
  rv <- state$rv
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
    raw   <- rv$raw_redcap
    dates <- input$rpt_dates
    if (is.null(raw) || nrow(raw) == 0)          return(NULL)
    if (is.null(dates) || length(dates) != 2)     return(NULL)
    if (!"rand_dttm_s" %in% names(raw))           return(NULL)
    
    rands <- raw %>%
      mutate(.rand_raw = trimws(as.character(rand_dttm_s))) %>%
      filter(nchar(.rand_raw) > 0, !is.na(.rand_raw),
             !is.na(site_name), nchar(trimws(site_name)) > 0) %>%
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
    
    site_meta <- rv$sites %>%
      transmute(.tonic_id = site_id, monthly_target,
                .jk = tolower(trimws(site_name)))
    
    rands <- rands %>%
      mutate(.jk = tolower(trimws(site_name))) %>%
      left_join(site_meta, by = ".jk") %>%
      select(-.jk) %>%
      filter(!is.na(.tonic_id)) %>%
      mutate(site_id = .tonic_id) %>%
      select(-.tonic_id)
    
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
    req(df)
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
          p <- latest_return_rate_file()
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
      
      tryCatch({
        if (rt == "TSC") {
          # ── TSC: Word output ─────────────────────────────────────────────
          rmd_src  <- "report/tsc_report.Rmd"
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
            params = list(
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
              completeness_style = input$completeness_style %||% "heatmap"
            ),
            envir             = new.env(parent = globalenv()),
            intermediates_dir = tmp_dir,
            clean             = TRUE,
            quiet             = TRUE
          )
        } else {
          # ── TMG / iTMG: HTML output (existing flow + report_type param) ───
          rmd_src  <- "report/tonic_report.Rmd"
          rmd_dest <- file.path(tmp_dir, "tonic_report.Rmd")
          file.copy(rmd_src, rmd_dest, overwrite = TRUE)
          file.copy("functions/consort_flow.R", file.path(tmp_dir, "consort_flow.R"), overwrite = TRUE)
          file.copy("functions/flat_completeness.R", file.path(tmp_dir, "flat_completeness.R"), overwrite = TRUE)
          file.copy("functions/baseline_table.R", file.path(tmp_dir, "baseline_table.R"), overwrite = TRUE)
          
          rmarkdown::render(
            input             = rmd_dest,
            output_file       = file,
            output_format     = "html_document",
            params            = list(
              report_data       = report_data,
              selected_sites    = site_label,
              date_from         = from_for_prep,
              date_to           = to_for_prep,
              include_withdrawn = isTRUE(input$include_withdrawn),
              include_appendix  = isTRUE(input$report_appendix),
              report_date       = format(Sys.Date(), "%d %B %Y"),
              logo_path         = "K:/BCTU/BCTU/Teams/Coloproctology/CURRENT TRIALS/TONIC/TONIC Meeting Organiser/TONIC TMG Report/TONIC_app/www/TONIC Logo.jpg",
              crf_csv_path      = latest_crf_path,
              screening_xlsx_path = "screening/TONIC_screening.xlsx",
              report_type       = rt,   # "TMG" or "iTMG"
              completeness_style = input$completeness_style %||% "heatmap"
            ),
            envir             = new.env(parent = globalenv()),
            intermediates_dir = tmp_dir,
            clean             = TRUE,
            quiet             = TRUE
          )
        }
      }, error = function(e) {
        # On failure, notify AND write a tiny diagnostic file matching
        # the expected extension so the browser doesn't serve a broken
        # partial Word/HTML file.
        msg <- paste0("Report generation failed: ", conditionMessage(e))
        showNotification(msg, duration = 15, type = "error")
        tryCatch(
          writeLines(
            c("TONIC report generation failed.",
              "",
              msg,
              "",
              "Please share this message with the dashboard maintainer."),
            con = file
          ),
          error = function(e2) NULL
        )
      })
    }
  )
}
