overview_server <- function(input, output, session, state) {
  rv <- state$rv
  filtered <- state$filtered

  output$meeting_label_txt <- renderText({
    req(input$last_meeting)
    paste("Dashboard highlights changes since", format(input$last_meeting, "%d %b %Y"))
  })

  rand_at_meeting <- reactive({
    req(input$last_meeting)
    rv$log %>%
      filter(action == "+1", as.Date(timestamp) <= as.Date(input$last_meeting)) %>%
      nrow()
  })

  sites_at_meeting <- reactive({
    req(input$last_meeting)
    rv$sites %>%
      filter(!is.na(site_open_date), as.Date(site_open_date) <= as.Date(input$last_meeting)) %>%
      nrow()
  })

  # ── KPI outputs (preserve original IDs: n_sites, n_rand, n_pct, n_rand_sub) ──
  output$n_sites <- renderText({
    as.character(nrow(filtered()))
  })
  output$n_rand <- renderText({
    r <- tryCatch(sum(filtered()$randomised, na.rm = TRUE),
                  error = function(e) 0)
    as.character(r)
  })
  output$n_pct <- renderText({
    r <- tryCatch(sum(filtered()$randomised, na.rm = TRUE),
                  error = function(e) 0)
    paste0(round(100 * r / TRIAL_TARGET, 1), "%")
  })
  output$n_rand_sub <- renderText({ paste0("of ", TRIAL_TARGET, " trial target") })

  output$delta_sites <- renderUI({
    delta_badge_ui(nrow(filtered()), sites_at_meeting(), " site")
  })
  output$delta_rand <- renderUI({
    delta_badge_ui(sum(filtered()$randomised, na.rm = TRUE), rand_at_meeting())
  })
  output$delta_pct <- renderUI({
    current <- sum(filtered()$randomised, na.rm = TRUE)
    prev    <- rand_at_meeting()
    if (is.null(prev)) return(NULL)
    diff <- round((current - prev) / TRIAL_TARGET * 100, 1)
    delta_badge_ui(diff, 0, "%")
  })

  # ── Recruitment projection (chart + sliders) ─────────────────────────────
  #
  # Builds a 3-line projection corridor over the protocol's 27-month schedule:
  #   · Pessimistic   — slow site ramp, low per-site rate
  #   · Central       — expected mid-point
  #   · Optimistic    — faster ramp, higher per-site rate
  #
  # Slider defaults come from actuals once ≥3 months in; otherwise from
  # projection_defaults() in functions/database.R. Values persist to the
  # projection_settings table in SQLite.

  # ── Load persisted slider values at app start, push into inputs ─────────
  proj_loaded <- reactiveVal(FALSE)

  observe({
    req(!proj_loaded())

    # Wait for rv$sites and rv$raw_redcap to be populated
    req(rv$sites)

    # Load persisted settings; fall back to defaults on any error
    s <- tryCatch(
      db_load_projection_settings(),
      error = function(e) {
        message("Projection settings load failed (using defaults): ", e$message)
        if (exists("projection_defaults")) projection_defaults() else list(
          rate_pessimistic = 2, rate_central = 3, rate_optimistic = 4,
          sites_pessimistic = 1, sites_central = 2, sites_optimistic = 3,
          target_sites = 24
        )
      }
    )

    # Try to derive smart defaults from actuals if ≥3 months of data
    smart <- tryCatch(
      .projection_smart_defaults(rv$raw_redcap, rv$sites),
      error = function(e) NULL
    )
    if (!is.null(smart)) {
      s$rate_pessimistic <- smart$rate_pessimistic
      s$rate_central     <- smart$rate_central
      s$rate_optimistic  <- smart$rate_optimistic
    }

    updateSliderInput(session, "proj_rate_pessimistic",  value = s$rate_pessimistic)
    updateSliderInput(session, "proj_rate_central",      value = s$rate_central)
    updateSliderInput(session, "proj_rate_optimistic",   value = s$rate_optimistic)
    updateSliderInput(session, "proj_sites_pessimistic", value = s$sites_pessimistic)
    updateSliderInput(session, "proj_sites_central",     value = s$sites_central)
    updateSliderInput(session, "proj_sites_optimistic",  value = s$sites_optimistic)
    updateSliderInput(session, "proj_target_sites",      value = s$target_sites)

    proj_loaded(TRUE)
  })

  # ── Settings panel toggle ────────────────────────────────────────────────
  observeEvent(input$toggle_proj_settings, {
    shinyjs::toggle("proj_settings_panel", anim = TRUE, animType = "slide")
  })

  # ── Reset to defaults ────────────────────────────────────────────────────
  observeEvent(input$reset_proj_settings, {
    d <- projection_defaults()
    updateSliderInput(session, "proj_rate_pessimistic",  value = d$rate_pessimistic)
    updateSliderInput(session, "proj_rate_central",      value = d$rate_central)
    updateSliderInput(session, "proj_rate_optimistic",   value = d$rate_optimistic)
    updateSliderInput(session, "proj_sites_pessimistic", value = d$sites_pessimistic)
    updateSliderInput(session, "proj_sites_central",     value = d$sites_central)
    updateSliderInput(session, "proj_sites_optimistic",  value = d$sites_optimistic)
    updateSliderInput(session, "proj_target_sites",      value = d$target_sites)
    showNotification("Projection assumptions reset to defaults.",
                     type = "message", duration = 2)
  })

  # ── Debounced save when any slider changes ──────────────────────────────
  proj_settings <- reactive({
    req(proj_loaded())
    list(
      rate_pessimistic  = input$proj_rate_pessimistic,
      rate_central      = input$proj_rate_central,
      rate_optimistic   = input$proj_rate_optimistic,
      sites_pessimistic = input$proj_sites_pessimistic,
      sites_central     = input$proj_sites_central,
      sites_optimistic  = input$proj_sites_optimistic,
      target_sites      = input$proj_target_sites
    )
  }) %>% debounce(1000)   # save 1s after user stops moving a slider

  observeEvent(proj_settings(), {
    tryCatch(
      db_save_projection_settings(proj_settings()),
      error = function(e) message("Couldn't save projection settings: ", e$message)
    )
  }, ignoreInit = TRUE)

  # ── Build projection data (central + pessimistic + optimistic) ──────────
  projection_data <- reactive({
    req(rv$sites)
    s <- list(
      rate_pessimistic  = input$proj_rate_pessimistic  %||% 2,
      rate_central      = input$proj_rate_central      %||% 3,
      rate_optimistic   = input$proj_rate_optimistic   %||% 4,
      sites_pessimistic = input$proj_sites_pessimistic %||% 1,
      sites_central     = input$proj_sites_central     %||% 2,
      sites_optimistic  = input$proj_sites_optimistic  %||% 3,
      target_sites      = input$proj_target_sites      %||% 24
    )

    # Actuals
    rand_dates <- tryCatch({
      rand_col <- fld("randomisation_datetime", default = "rand_dttm_s")
      if (rand_col %in% names(rv$raw_redcap)) {
        d <- suppressWarnings(as.Date(rv$raw_redcap[[rand_col]]))
        d[!is.na(d)]
      } else {
        as.Date(character(0))
      }
    }, error = function(e) as.Date(character(0)))

    n_open <- sum(rv$sites$status %in% c("Open", "Recruiting"), na.rm = TRUE)

    .build_projection_series(rand_dates, n_open_now = n_open, settings = s,
                             trial_target = TRIAL_TARGET)
  })

  # ── The chart ────────────────────────────────────────────────────────────
  output$proj_chart <- renderEcharts4r({
    pd <- tryCatch(projection_data(), error = function(e) {
      message("Projection data error: ", e$message)
      NULL
    })
    if (is.null(pd) || nrow(pd) == 0) return(empty_echart("No projection available"))

    # Defensive: every column that feeds e_line MUST be numeric with NAs
    # converted to actual NA_real_ (not character "NA" or integer NA coerced
    # from missing rows). Missing data confuses the echarts renderer when
    # stacked lines are involved.
    num <- function(x) suppressWarnings(as.numeric(x))
    pd$plan        <- num(pd$plan)
    pd$actual      <- num(pd$actual)
    pd$central     <- num(pd$central)
    pd$pessimistic <- num(pd$pessimistic)
    pd$optimistic  <- num(pd$optimistic)

    # For the shaded band: two stacked invisible lines — base = pessimistic,
    # top differential = optimistic minus pessimistic (rendered as area).
    pd$band_base <- pd$pessimistic
    pd$band_diff <- pd$optimistic - pd$pessimistic

    # Avoid NA in the band columns (NA + stack misbehaves). Replace with 0
    # where both endpoints are NA so the band simply collapses to the axis.
    pd$band_base[is.na(pd$band_base)] <- 0
    pd$band_diff[is.na(pd$band_diff)] <- 0

    tryCatch({
      pd %>%
        e_charts(month_label) %>%

        # ── Shaded band (pessimistic base + differential to optimistic) ──
        # Scaffolding: invisible base line that the stack sits on top of
        e_line(band_base,
               stack     = "band",
               symbol    = "none",
               showSymbol = FALSE,
               lineStyle = list(opacity = 0),
               itemStyle = list(opacity = 0),
               legend    = FALSE,
               tooltip   = list(show = FALSE)) %>%
        # The visible area — filled with translucent amber
        e_line(band_diff,
               name       = "Projection range",
               stack      = "band",
               symbol     = "none",
               showSymbol = FALSE,
               lineStyle  = list(opacity = 0),
               areaStyle  = list(color = "rgba(245, 158, 11, 0.18)")) %>%

        # ── Protocol plan (dotted teal) ─────────────────────────────────
        e_line(plan,
               name       = "Protocol plan",
               smooth     = TRUE,
               symbol     = "none",
               showSymbol = FALSE,
               lineStyle  = list(color = "#2EC4A5",
                                 width = 2,
                                 type  = "dotted")) %>%

        # ── Central projection (dashed amber) ────────────────────────────
        e_line(central,
               name       = "Projection (central)",
               smooth     = TRUE,
               symbol     = "none",
               showSymbol = FALSE,
               lineStyle  = list(color = "#F59E0B",
                                 width = 2.5,
                                 type  = "dashed")) %>%

        # ── Actuals (solid navy, visible points) ─────────────────────────
        e_line(actual,
               name         = "Actual",
               smooth       = FALSE,
               symbol       = "circle",
               symbolSize   = 7,
               connectNulls = FALSE,
               lineStyle    = list(color = "#1B4F6B", width = 3),
               itemStyle    = list(color = "#1B4F6B")) %>%

        # ── Styling ──────────────────────────────────────────────────────
        e_tooltip(trigger = "axis") %>%
        e_legend(bottom = 0,
                 textStyle = list(color = "#1B4F6B", fontSize = 11),
                 data = list("Actual", "Projection (central)",
                             "Projection range", "Protocol plan")) %>%
        e_grid(left = "55", right = "30", top = 20, bottom = 60) %>%
        e_y_axis(name          = "Cumulative participants",
                 nameLocation  = "middle",
                 nameGap       = 42,
                 nameTextStyle = list(fontSize = 11, color = "#64748B"),
                 axisLabel     = list(fontSize = 10, color = "#64748B"),
                 max           = TRIAL_TARGET,
                 splitLine     = list(lineStyle = list(color = "#EEF3F8"))) %>%
        e_x_axis(axisLabel = list(rotate = 45,
                                   fontSize = 10,
                                   color = "#64748B"),
                 axisLine  = list(lineStyle = list(color = "#CBD5E1")))
    },
    error = function(e) {
      message("Projection chart render error: ", e$message)
      empty_echart("Could not render projection")
    })
  })

  # ── Completion date summary (shown in card header "tools") ───────────────
  output$proj_completion_tool <- renderText({
    pd <- projection_data()
    if (is.null(pd) || nrow(pd) == 0) return("")

    # Find first month where central projection >= target
    hit_row <- which(pd$central >= TRIAL_TARGET)[1]
    if (is.na(hit_row)) return("Target not reached within protocol window")
    est_date <- pd$month_date[hit_row]
    months_from_now <- as.integer(round(
      as.numeric(difftime(est_date, Sys.Date(), units = "days")) / 30.44
    ))
    if (months_from_now <= 0) return("Target already reached")
    sprintf("Central estimate: %s (\u2248 %d months from now)",
            format(est_date, "%b %Y"), months_from_now)
  })

  output$proj_note <- renderText({
    pd <- projection_data()
    if (is.null(pd) || nrow(pd) == 0) return("")
    n_actual <- sum(!is.na(pd$actual)) - 1  # subtract month 0 start
    sprintf(
      "Shaded band = range between pessimistic and optimistic assumptions. Central line uses your current settings. Actuals based on %d recorded randomisations.",
      max(0, n_actual)
    )
  })


  # ── Map (auto-fit worldwide) ─────────────────────────────────────────────
  output$site_map <- renderLeaflet({
    df <- filtered() %>% filter(!is.na(lat), !is.na(lon))
    m  <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addLegend("bottomright", colors = unname(status_cols), labels = names(status_cols),
                title = "Site status", opacity = 0.9)

    if (nrow(df) == 0) {
      # No sites — default world view
      return(m %>% setView(lng = 0, lat = 25, zoom = 2))
    }

    for (i in seq_len(nrow(df))) {
      r <- df[i, ]
      m <- m %>% addMarkers(
        lng = r$lon, lat = r$lat,
        icon = make_map_icon(r$randomised, r$status),
        popup = make_popup(r$site_name, r$site_id, r$region, r$status, r$randomised, r$target))
    }

    # Auto-fit to loaded sites — works UK-only or worldwide
    m <- m %>% fitBounds(
      lng1 = min(df$lon, na.rm = TRUE) - 0.5,
      lat1 = min(df$lat, na.rm = TRUE) - 0.5,
      lng2 = max(df$lon, na.rm = TRUE) + 0.5,
      lat2 = max(df$lat, na.rm = TRUE) + 0.5
    )
    m
  })

  # ── Pipeline chart (unchanged) ───────────────────────────────────────────
  output$pipeline_chart <- renderEcharts4r({
    df <- filtered() %>% count(status)
    if (nrow(df) == 0) return(empty_echart("No sites loaded"))
    df %>%
      e_charts(status) %>%
      e_bar(n, legend = FALSE,
            itemStyle = list(color = JS(paste0(
              "function(p){var m={",
              paste(sprintf("'%s':'%s'", names(status_cols), unname(status_cols)), collapse = ","),
              "};return m[p.name]||'#94A3B8';}")))) %>%
      e_flip_coords() %>%
      e_tooltip(trigger = "item",
                formatter = JS("function(p){return p.name+': '+p.value;}"),
                backgroundColor = "rgba(27,79,107,.92)",
                textStyle = list(color = "#fff", fontFamily = "Outfit")) %>%
      e_grid(left = "30%", right = "12%", top = "3%", bottom = "3%") %>%
      e_x_axis(minInterval = 1, axisLabel = list(fontFamily = "Outfit", fontSize = 11, color = col_muted)) %>%
      e_y_axis(axisLabel = list(fontFamily = "Outfit", fontSize = 11, color = col_muted)) %>%
      e_toolbox_feature(feature = "saveAsImage", title = "Save")
  })

  # ── Top sites chart (unchanged) ──────────────────────────────────────────
  output$top_sites_chart <- renderEcharts4r({
    df <- filtered() %>% filter(randomised > 0) %>%
      arrange(desc(randomised)) %>% slice_head(n = 8) %>%
      mutate(label = str_trunc(site_name, 20))
    if (nrow(df) == 0) return(empty_echart("No randomisations yet"))
    df %>%
      e_charts(label) %>%
      e_bar(randomised, legend = FALSE, itemStyle = list(color = col_teal),
            label = list(show = TRUE, position = "right",
                         textStyle = list(fontFamily = "Outfit", fontSize = 10))) %>%
      e_flip_coords() %>%
      e_tonic() %>%
      e_grid(left = "35%", right = "15%", top = "3%", bottom = "3%") %>%
      e_toolbox_feature(feature = "saveAsImage", title = "Save")
  })

  # ── Overview table (add country + SIV columns) ───────────────────────────
  output$overview_table <- renderReactable({
    df <- filtered()
    if (nrow(df) == 0) return(empty_reactable("No sites \u2014 load REDCap CSV or add manually"))

    # Cope with older data that may not have the new columns
    if (!"country"    %in% names(df)) df$country    <- NA_character_
    if (!"siv_booked" %in% names(df)) df$siv_booked <- FALSE
    if (!"siv_date"   %in% names(df)) df$siv_date   <- as.Date(NA)

    df <- df %>%
      mutate(
        status_html = vapply(status, status_pill_html, character(1)),
        prog_html   = prog_bar_html(randomised, target),
        siv_html    = dplyr::case_when(
          isTRUE(siv_booked) & !is.na(siv_date) ~
            paste0('<span style="color:#0d4037;background:#D1FAE5;',
                   'padding:2px 9px;border-radius:12px;font-size:11px;font-weight:600">',
                   format(siv_date, "%d %b %Y"), '</span>'),
          siv_booked ~
            '<span style="color:#854F0B;background:#FEF3C7;padding:2px 9px;border-radius:12px;font-size:11px;font-weight:600">Booked \u2014 no date</span>',
          TRUE ~
            '<span style="color:#64748B;font-size:11px">\u2014</span>'
        )
      )

    reactable(
      df %>% select(site_id, site_name, city, country, region, status_html,
                    siv_html, randomised, target, prog_html, monthly_target),
      striped = TRUE, highlight = TRUE, bordered = FALSE, compact = TRUE,
      defaultColDef = colDef(style = list(fontFamily = "Outfit", fontSize = "13px")),
      columns = list(
        site_id        = colDef(name = "Site ID", minWidth = 90,
                                cell = function(v) htmltools::span(class = "sid", v)),
        site_name      = colDef(name = "Site", minWidth = 180),
        city           = colDef(name = "City", minWidth = 100),
        country        = colDef(name = "Country", minWidth = 110),
        region         = colDef(name = "Region", minWidth = 110),
        status_html    = colDef(name = "Status", html = TRUE, minWidth = 120),
        siv_html       = colDef(name = "SIV", html = TRUE, minWidth = 130),
        randomised     = colDef(name = "Total", align = "center", minWidth = 70),
        target         = colDef(name = "Target", align = "center", minWidth = 70,
                                style = list(color = col_muted)),
        prog_html      = colDef(name = "Progress", html = TRUE, minWidth = 160),
        monthly_target = colDef(name = "Mo. target", align = "center", minWidth = 90)
      )
    )
  })
}
