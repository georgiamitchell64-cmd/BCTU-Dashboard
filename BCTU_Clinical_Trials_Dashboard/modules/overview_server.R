overview_server <- function(input, output, session, state) {
  rv <- state$rv
  filtered <- state$filtered
  # WP-scoped views: follow the work-package picker. When "all WPs" is selected
  # these return the full trial data, so the Overview doubles as the roll-up
  # summary tab.
  redcap_wp <- state$redcap_wp
  sites_wp  <- state$sites_wp
  parts_wp  <- state$parts_wp

  # ── Smart Insights ──────────────────────────────────────────────────────
  # compute_insights() walks raw REDCap + sites every render; without caching
  # it re-runs whenever anything in the overview reactive graph fires. Cache
  # on a fingerprint of the inputs so it only recomputes when data changes.
  insights_cached <- reactive({
    cfg <- rv$trial_config
    req(cfg)
    tryCatch(
      compute_insights(redcap_wp(), sites_wp(), cfg),
      error = function(e) {
        message("Smart insights error: ", e$message)
        list()
      }
    )
  }) %>% bindCache(
    rv$trial_config$code %||% "",
    rv$active_wp %||% 0L,
    nrow(redcap_wp() %||% data.frame()),
    nrow(sites_wp() %||% data.frame()),
    digest::digest(sites_wp()$randomised)
  )

  output$smart_insights_ui <- renderUI({
    if (is.null(rv$trial_config)) return(NULL)
    render_insights_panel(insights_cached())
  })

  # ── CONSORT flow diagram (only when the trial's consort_flow feature is on) ─
  output$consort_card_ui <- renderUI({
    cfg <- rv$trial_config
    if (is.null(cfg) || !isTRUE(cfg$features$consort_flow)) return(NULL)
    counts <- tryCatch(consort_counts_live(redcap_wp(), cfg), error = function(e) NULL)
    if (is.null(counts)) return(NULL)
    tags$section(class = "pov-card",
      div(class = "pov-card-head",
          tags$h3("CONSORT flow"),
          span(class = "pov-card-tool-note",
               "Participant flow — randomisation, follow-up and withdrawals by type")),
      HTML(consort_html(counts, cfg)))
  })

  # ── Per-work-package roll-up ──────────────────────────────────────────────
  # Computed from the full (unscoped) participant table so the "all WPs" summary
  # can compare every work package side by side, regardless of which pill is
  # currently active.
  wp_rollup <- reactive({
    cfg <- rv$trial_config
    if (is.null(cfg)) return(NULL)
    wps <- cfg$work_packages
    if (is.null(wps) || !length(wps)) return(NULL)
    p <- rv$participants
    if (is.null(p) || !"work_package" %in% names(p)) return(NULL)

    base <- p %>% filter(event_type == "Baseline",
                         !is.na(record_id), nchar(trimws(record_id)) > 0)
    rc       <- rv$raw_redcap
    rand_col <- fld("randomisation_datetime", default = "rand_dttm_s")
    wpt      <- cfg$work_package_targets

    lapply(seq_along(wps), function(i) {
      bi      <- base %>% filter(!is.na(work_package), work_package == i)
      rand_n  <- dplyr::n_distinct(bi$record_id)
      sites_n <- dplyr::n_distinct(bi$site_dag[!is.na(bi$site_dag) &
                                                nchar(trimws(bi$site_dag)) > 0])
      tgt <- if (!is.null(wpt) && length(wpt) >= i)
               suppressWarnings(as.integer(wpt[[i]])) else NA_integer_
      last_d <- as.Date(NA)
      if (!is.null(rc) && rand_col %in% names(rc) && "work_package" %in% names(rc)) {
        dd <- suppressWarnings(as.Date(
          rc[[rand_col]][suppressWarnings(as.integer(rc$work_package)) == i]))
        dd <- dd[!is.na(dd)]
        if (length(dd)) last_d <- max(dd)
      }
      list(i = i, label = as.character(wps[i]), rand = rand_n,
           sites = sites_n, target = tgt, last = last_d)
    })
  })

  output$wp_summary_ui <- renderUI({
    cfg <- rv$trial_config
    if (is.null(cfg)) return(NULL)
    wps <- cfg$work_packages
    if (is.null(wps) || !length(wps)) return(NULL)   # single-WP trial — skip
    if (!is.null(rv$active_wp))       return(NULL)    # only on the roll-up view
    roll <- wp_rollup()
    if (is.null(roll)) return(NULL)

    total_rand <- sum(vapply(roll, function(x) x$rand, integer(1)))
    pretty <- function(s) sub("^WKP[0-9]+\\s*[:.·-]?\\s*", "", as.character(s))

    cards <- lapply(roll, function(x) {
      has_tgt <- !is.na(x$target) && x$target > 0
      pct     <- if (has_tgt) min(100, round(100 * x$rand / x$target))
                 else if (total_rand > 0) round(100 * x$rand / total_rand) else 0
      bar_lbl <- if (has_tgt) sprintf("%d%% of %d target", pct, x$target)
                 else sprintf("%d%% of trial total", pct)
      last_lbl <- if (inherits(x$last, "Date") && !is.na(x$last))
                    paste("Last:", format(x$last, "%d %b %Y")) else "No randomisations yet"
      nm <- pretty(x$label); if (!nzchar(nm)) nm <- paste0("Work package ", x$i)

      tags$button(
        class = "wp-sum-card",
        onclick = sprintf(
          "Shiny.setInputValue('wp_pick', %d, {priority:'event'}); setActiveWp('wp_pill_%d');",
          x$i, x$i),
        div(class = "wp-sum-card-top",
            span(class = "wp-sum-badge", paste0("WKP", x$i)),
            span(class = "wp-sum-name", title = nm, nm)),
        div(class = "wp-sum-num", x$rand,
            tags$small(if (x$rand == 1) "participant" else "participants")),
        div(class = "wp-sum-bar", tags$i(style = sprintf("width:%d%%;", pct))),
        div(class = "wp-sum-meta",
            span(bar_lbl),
            span(sprintf("%d %s", x$sites, if (x$sites == 1) "site" else "sites"))),
        div(class = "wp-sum-meta", span(last_lbl)),
        div(class = "wp-sum-cta", "Open dashboard →")
      )
    })

    tags$section(class = "pov-card wp-summary",
      div(class = "wp-summary-head",
          tags$h3("Work packages"),
          span(class = "wp-summary-sub",
               sprintf("%d work packages · %d participants total · click to open",
                       length(roll), total_rand))),
      div(class = "wp-sum-grid", cards)
    )
  })

  output$meeting_label_txt <- renderText({
    req(input$date_from, input$date_to)
    sprintf("Range: %s → %s",
            format(input$date_from, "%d %b %Y"),
            format(input$date_to,   "%d %b %Y"))
  })

  # Stable cache-key for rv$log — max(empty) warns and returns -Inf, so
  # guard against an empty/all-NA log explicitly. Same shape used by both
  # rand_at_meeting and rand_at_to.
  log_fp <- function() {
    df <- rv$log
    if (is.null(df) || !nrow(df) ||
        !"timestamp" %in% names(df) || all(is.na(df$timestamp))) {
      return("empty")
    }
    suppressWarnings(as.character(max(df$timestamp, na.rm = TRUE)))
  }
  sites_fp <- function() {
    df <- rv$sites
    if (is.null(df) || !nrow(df)) return("empty")
    digest::digest(df$site_open_date)
  }

  # Count of randomisations recorded on or before `date_from`
  # (i.e. baseline against which to measure the delta over the range).
  # bindCache keys on the date + a cheap fingerprint of the log so we skip
  # the filter when neither has changed (was re-running on every input tick).
  rand_at_meeting <- reactive({
    req(input$date_from)
    rv$log %>%
      filter(action == "+1", as.Date(timestamp) <= as.Date(input$date_from)) %>%
      nrow()
  }) %>% bindCache(input$date_from, nrow(rv$log), log_fp())

  # Sites already open at `date_from`.
  sites_at_meeting <- reactive({
    req(input$date_from)
    rv$sites %>%
      filter(!is.na(site_open_date),
             as.Date(site_open_date) <= as.Date(input$date_from)) %>%
      nrow()
  }) %>% bindCache(input$date_from, nrow(rv$sites), sites_fp())

  # "Current" totals are clamped to date_to so the range is symmetric.
  rand_at_to <- reactive({
    req(input$date_to)
    rv$log %>%
      filter(action == "+1", as.Date(timestamp) <= as.Date(input$date_to)) %>%
      nrow()
  }) %>% bindCache(input$date_to, nrow(rv$log), log_fp())

  sites_at_to <- reactive({
    req(input$date_to)
    rv$sites %>%
      filter(!is.na(site_open_date),
             as.Date(site_open_date) <= as.Date(input$date_to)) %>%
      nrow()
  }) %>% bindCache(input$date_to, nrow(rv$sites), sites_fp())

  # ── KPI outputs (preserve original IDs: n_sites, n_rand, n_pct, n_rand_sub) ──
  output$n_sites <- renderText({
    as.character(nrow(filtered()))
  })
  output$n_rand <- renderText({
    r <- tryCatch(sum(filtered()$randomised, na.rm = TRUE),
                  error = function(e) 0)
    as.character(r)
  })
  # Read the target from rv$trial_config (reactive) rather than the
  # TRIAL_TARGET global so these update when the user switches trials.
  # When a work package is active, use its per-WP target if the config defines
  # work_package_targets; otherwise fall back to the whole-trial target.
  trial_target_r <- reactive({
    wp_effective_target(rv$trial_config, rv$active_wp)
  })
  output$n_pct <- renderText({
    r <- tryCatch(sum(filtered()$randomised, na.rm = TRUE),
                  error = function(e) 0)
    tgt <- trial_target_r()
    if (tgt <= 0) return("\u2014")
    paste0(round(100 * r / tgt, 1), "%")
  })
  output$n_rand_sub <- renderText({ paste0("of ", trial_target_r(), " trial target") })

  output$delta_sites <- renderUI({
    delta_badge_ui(sites_at_to(), sites_at_meeting(), " site")
  })
  output$delta_rand <- renderUI({
    delta_badge_ui(rand_at_to(), rand_at_meeting())
  })
  output$delta_pct <- renderUI({
    current <- rand_at_to()
    prev    <- rand_at_meeting()
    if (is.null(prev) || is.null(current)) return(NULL)
    tgt <- trial_target_r()
    if (tgt <= 0) return(NULL)
    diff <- round((current - prev) / tgt * 100, 1)
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
      .projection_smart_defaults(redcap_wp(), sites_wp()),
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

  # ── Portfolio review panel toggle (collapsed by default) ────────────────
  pr_panel_open <- reactiveVal(FALSE)
  observeEvent(input$toggle_pr_panel, {
    pr_panel_open(!pr_panel_open())
    shinyjs::toggle("pr_panel", anim = TRUE, animType = "slide")
    shinyjs::html("pr_toggle_lbl",
                  if (pr_panel_open()) "Hide chart" else "Show chart")
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

    # Actuals (scoped to the active work package via redcap_wp / sites_wp)
    rc <- redcap_wp()
    rand_dates <- tryCatch({
      rand_col <- fld("randomisation_datetime", default = "rand_dttm_s")
      if (!is.null(rc) && rand_col %in% names(rc)) {
        d <- suppressWarnings(as.Date(rc[[rand_col]]))
        d[!is.na(d)]
      } else {
        as.Date(character(0))
      }
    }, error = function(e) as.Date(character(0)))

    n_open <- sum(sites_wp()$status %in% c("Open", "Recruiting"), na.rm = TRUE)

    .build_projection_series(rand_dates, n_open_now = n_open, settings = s,
                             trial_target = trial_target_r())
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
                 max           = trial_target_r(),
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
    hit_row <- which(pd$central >= trial_target_r())[1]
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
      defaultPageSize = 25, showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 25, 50, 100),
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

  # ══════════════════════════════════════════════════════════════════════════
  # Portfolio review chart (monthly schedule)
  # ══════════════════════════════════════════════════════════════════════════
  # Six series, layout matches the BCTU Portfolio Review Table workbook:
  #   col B  Pts — original projection, monthly       (imported from Excel)
  #   col C  Pts — actual, monthly                    (auto: rv$raw_redcap)
  #   col D  Pts — original projection, cumulative    (imported)
  #   col E  Pts — actual, cumulative                 (auto: cumulative of C)
  #   col F  Sites — projected, cumulative            (imported)
  #   col G  Sites — actual, cumulative               (auto: rv$sites)
  # Projected series come from the imported Excel — no manual entry.
  # Actuals are computed live from dashboard data each render.

  PR_PROJECTED_DEFS <- list(
    list(key   = "proj_pts_monthly",
         label = "Pts — original projection, monthly",
         color = "#0F172A", lty = "solid", lw = 1.5,
         match = c("pts - original projection, monthly",
                   "pts.*projection.*monthly",
                   "projected.*monthly",
                   "original.*monthly")),
    list(key   = "proj_pts_cum",
         label = "Pts — original projection, cumulative",
         color = "#7C3AED", lty = "solid", lw = 2.5,
         match = c("pts - original projection, cumulative",
                   "pts.*projection.*cumulative",
                   "projected.*cumulative",
                   "original.*cumulative")),
    list(key   = "proj_sites_cum",
         label = "Sites — projected, cumulative",
         color = "#0EA5E9", lty = "solid", lw = 1.5,
         match = c("sites - projected, cumulative",
                   "sites.*projected.*cumulative",
                   "sites.*projection.*cumulative",
                   "centres.*projected.*cumulative"))
  )

  # Auto-computed series — derived from rv$raw_redcap / rv$sites at chart time.
  PR_ACTUAL_DEFS <- list(
    list(key   = "actual_pts_monthly",
         label = "Pts — actual, monthly",
         color = "#10B981", lty = "dashed", lw = 1.5,
         source = "Auto from REDCap randomisation dates"),
    list(key   = "actual_pts_cum",
         label = "Pts — actual, cumulative",
         color = "#059669", lty = "dashed", lw = 2.0,
         source = "Auto: cumulative actual recruits"),
    list(key   = "actual_sites_cum",
         label = "Sites — actual, cumulative",
         color = "#F59E0B", lty = "dashed", lw = 1.5,
         source = "Auto from Sites tab — date centre opened")
  )

  # Order on chart matches the workbook column order: proj-monthly, actual-
  # monthly, proj-cum, actual-cum, proj-sites, actual-sites.
  PR_SERIES_DEFS <- list(
    PR_PROJECTED_DEFS[[1]],   # proj_pts_monthly
    PR_ACTUAL_DEFS[[1]],      # actual_pts_monthly
    PR_PROJECTED_DEFS[[2]],   # proj_pts_cum
    PR_ACTUAL_DEFS[[2]],      # actual_pts_cum
    PR_PROJECTED_DEFS[[3]],   # proj_sites_cum
    PR_ACTUAL_DEFS[[3]]       # actual_sites_cum
  )

  # Default month sequence — 27 months starting March of the current year.
  .pr_default_months <- function(n_months = 27, start = NULL) {
    if (is.null(start) || !inherits(start, "Date")) {
      start <- as.Date(sprintf("%d-03-01", as.integer(format(Sys.Date(), "%Y"))))
    }
    seq(start, by = "month", length.out = n_months)
  }

  # Parse the BCTU Portfolio Review Table workbook (column-oriented). The
  # file has one row per month and one column per series:
  #   col A  Month (Excel date serial — converted to Date)
  #   col B  Pts - original projection, monthly
  #   col C  Pts - actual, monthly
  #   col D  Pts - original projection, cumulative
  #   col E  Pts - actual, cumulative
  #   col F  Sites - projected, cumulative
  #   col G  Sites - actual, cumulative
  # Returns: list(start_date, n_months, series = named list of numeric vectors,
  # months = Date vector). Errors with a helpful message on bad input.
  .pr_import_excel <- function(path) {
    if (is.null(path) || !nzchar(trimws(path)))
      stop("Enter the path to your Excel project plan first.")
    path <- trimws(path)
    if (!file.exists(path))
      stop("File not found: ", path)
    ext <- tolower(tools::file_ext(path))
    if (!ext %in% c("xlsx", "xls", "xlsm"))
      stop("Not an Excel file (expected .xlsx/.xls/.xlsm)")
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("readxl package not installed (install.packages('readxl'))")

    # Read with col_names = TRUE so column headers come back as names.
    raw <- tryCatch(
      readxl::read_excel(path, col_names = TRUE, .name_repair = "minimal",
                         sheet = 1),
      error = function(e) stop("Couldn't read the workbook: ", e$message)
    )
    raw <- as.data.frame(raw, stringsAsFactors = FALSE)
    if (!nrow(raw) || ncol(raw) < 2)
      stop("Workbook is empty or has fewer than 2 columns.")

    # Column 1 = months. Excel serial numbers come back as numeric on read;
    # explicit dates come back as POSIXct. Handle both.
    raw_months <- raw[[1]]
    if (is.numeric(raw_months)) {
      months <- as.Date(raw_months, origin = "1899-12-30")
    } else if (inherits(raw_months, c("POSIXct", "POSIXt"))) {
      months <- as.Date(raw_months)
    } else {
      months <- suppressWarnings(as.Date(as.character(raw_months)))
    }
    keep <- !is.na(months)
    if (!any(keep))
      stop("Couldn't parse any dates from column A.")
    months <- months[keep]
    raw    <- raw[keep, , drop = FALSE]

    # Match each projected series against the column headers (regex on tolower).
    headers <- tolower(trimws(as.character(names(raw))))
    found <- list()
    for (def in PR_PROJECTED_DEFS) {
      hit <- NA_integer_
      for (j in seq_along(headers)) {
        h <- headers[j]
        if (!nzchar(h)) next
        if (any(vapply(def$match,
                       function(p) grepl(p, h, perl = TRUE,
                                          ignore.case = TRUE),
                       logical(1)))) {
          hit <- j; break
        }
      }
      if (!is.na(hit)) {
        v <- suppressWarnings(as.numeric(raw[[hit]]))
        v[is.na(v)] <- 0
        found[[def$key]] <- v
      }
    }
    if (!length(found))
      stop("No matching projected-series columns found. Headers seen: ",
           paste(headers, collapse = " | "))

    list(
      start_date = min(months),
      n_months   = length(months),
      months     = months,
      series     = found
    )
  }

  # Try multiple plausible column names for the randomisation datetime.
  # `fld()` reads from the active trial config; we then try common fall-backs.
  .pr_rand_col <- function(df) {
    if (is.null(df) || !ncol(df)) return(NA_character_)
    cands <- character(0)
    cfg_col <- tryCatch(fld("randomisation_datetime",
                            default = "rand_dttm_s"),
                        error = function(e) NULL)
    if (!is.null(cfg_col)) cands <- c(cands, cfg_col)
    cands <- c(cands, "rand_dttm_s", "rand_dttm", "rand_date",
               "randomisation_date", "randomization_date",
               "date_randomised", "date_of_randomisation")
    for (c in cands) if (c %in% names(df)) return(c)
    # Last-ditch: any column containing "rand" + "dt" / "date".
    nm <- tolower(names(df))
    hit <- which(grepl("rand", nm) & grepl("dt|date", nm))
    if (length(hit)) return(names(df)[hit[1]])
    NA_character_
  }

  # Bucket a Date vector into month indices relative to start_date (1-based).
  .pr_month_index <- function(d, start_date) {
    (as.integer(format(d, "%Y")) - as.integer(format(start_date, "%Y"))) * 12L +
    (as.integer(format(d, "%m")) - as.integer(format(start_date, "%m"))) + 1L
  }

  # Pts — actual, monthly: count of randomisations falling in each month.
  .pr_actual_pts_monthly <- function(start_date, n_months) {
    out <- rep(0L, n_months)
    df  <- rv$raw_redcap
    if (is.null(df) || !nrow(df)) return(out)
    rand_col <- .pr_rand_col(df)
    if (is.na(rand_col)) return(out)
    d <- suppressWarnings(as.Date(df[[rand_col]]))
    d <- d[!is.na(d)]
    if (!length(d)) return(out)
    idx  <- .pr_month_index(d, start_date)
    keep <- idx >= 1L & idx <= n_months
    if (any(keep)) out <- as.integer(tabulate(idx[keep], nbins = n_months))
    out
  }

  # Pts — actual, cumulative: any pre-window randomisations + cumsum within.
  .pr_actual_pts_cumulative <- function(start_date, n_months) {
    monthly <- .pr_actual_pts_monthly(start_date, n_months)
    pre <- 0L
    df  <- rv$raw_redcap
    if (!is.null(df) && nrow(df)) {
      rand_col <- .pr_rand_col(df)
      if (!is.na(rand_col)) {
        d <- suppressWarnings(as.Date(df[[rand_col]]))
        pre <- sum(!is.na(d) & d < start_date)
      }
    }
    as.integer(pre + cumsum(monthly))
  }

  # Sites — actual, cumulative: count of sites whose site_open_date is on or
  # before the last day of each month in the window.
  .pr_actual_sites_cumulative <- function(start_date, n_months) {
    out <- rep(0L, n_months)
    df  <- rv$sites
    if (is.null(df) || !nrow(df)) return(out)
    if (!"site_open_date" %in% names(df)) return(out)
    d <- suppressWarnings(as.Date(df$site_open_date))
    d <- d[!is.na(d)]
    if (!length(d)) return(out)
    starts <- seq(start_date, by = "month", length.out = n_months + 1)
    eom <- starts[-1] - 1L
    for (i in seq_len(n_months)) out[i] <- sum(d <= eom[i])
    as.integer(out)
  }

  # ── Reactive state ───────────────────────────────────────────────────────
  # Projected series are populated by Excel import (no manual entry).
  # Stored as a list keyed by series id; loaded from overrides.json on trial
  # change. Always coerced to numeric so format()/round() never errors.
  pr_n_months   <- reactiveVal(27L)
  pr_start_date <- reactiveVal(.pr_default_months(1)[1])
  pr_excel_path_state <- reactiveVal("")
  pr_proj <- reactiveVal(list())   # named list of numeric vectors

  observeEvent(rv$trial_config, {
    cfg   <- rv$trial_config
    saved <- cfg$portfolio_review %||% list()

    n <- suppressWarnings(as.integer(saved$n_months %||% 27L))
    if (is.na(n) || n < 1) n <- 27L
    if (n > 120) n <- 120L
    pr_n_months(n)

    sd <- tryCatch(as.Date(saved$start_month %||% NA), error = function(e) NA)
    if (is.na(sd)) sd <- .pr_default_months(1)[1]
    pr_start_date(sd)

    pr_excel_path_state(as.character(saved$excel_path %||% ""))

    # Coerce loaded series to numeric (JSON may bring them back as character).
    raw_series <- saved$series %||% list()
    cleaned <- lapply(raw_series, function(v) {
      v <- suppressWarnings(as.numeric(unlist(v)))
      v[is.na(v)] <- 0
      v
    })
    pr_proj(cleaned)
  }, ignoreNULL = FALSE)

  # ── Form (import-only — no manual data entry) ────────────────────────────
  output$pr_form_ui <- renderUI({
    n   <- pr_n_months()
    sd  <- pr_start_date()
    saved_path  <- pr_excel_path_state()
    proj_state  <- pr_proj()

    # One small read-only line per series, showing whether data is loaded
    # and (where applicable) the highest value reached.
    series_summary <- function(def, vals, source = NULL) {
      pill_cls   <- "pr-summary-empty"
      pill_text  <- "—"
      vals_safe  <- suppressWarnings(as.numeric(vals))
      vals_safe  <- vals_safe[!is.na(vals_safe)]
      if (length(vals_safe) > 0 && any(vals_safe != 0)) {
        pill_cls  <- "pr-summary-ok"
        pill_text <- sprintf("%d values · max %s",
                             length(vals_safe),
                             format(max(vals_safe), big.mark = ","))
      }
      div(class = "pr-summary-row",
          tags$span(class = "pr-color-dot",
                    style = sprintf("background:%s;", def$color)),
          div(class = "pr-summary-meta",
              div(class = "pr-summary-label", def$label),
              if (!is.null(source))
                div(class = "pr-summary-source", source)),
          tags$span(class = paste("pr-summary-pill", pill_cls), pill_text))
    }

    proj_rows <- lapply(PR_PROJECTED_DEFS, function(s)
      series_summary(s, proj_state[[s$key]]))
    actual_rows <- lapply(PR_ACTUAL_DEFS, function(s)
      series_summary(s, NULL, source = s$source))

    div(
      tags$style(HTML("
        .pr-import-card {
          background:#F8FAFD; border:1px solid #DDE5EE; border-radius:10px;
          padding:12px 14px; margin-bottom:14px;
        }
        .pr-import-row { display:grid; grid-template-columns:1fr auto; gap:8px;
                         align-items:center; }
        .pr-controls { display:grid; grid-template-columns:1fr 1fr; gap:10px;
                       margin-bottom:12px; }
        .pr-section-eye {
          font-size:10px; font-weight:700; color:#1B4F6B;
          text-transform:uppercase; letter-spacing:.6px; margin:14px 0 8px;
        }
        .pr-summary-row {
          display:flex; align-items:center; gap:10px; padding:8px 10px;
          background:#FFFFFF; border:1px solid #EEF3F8; border-radius:8px;
          margin-bottom:6px;
        }
        .pr-color-dot {
          width:11px; height:11px; border-radius:50%; flex-shrink:0;
          border:1px solid rgba(15,23,42,.15);
        }
        .pr-summary-meta { flex:1; min-width:0; }
        .pr-summary-label { font-size:12px; font-weight:600; color:#0F172A; }
        .pr-summary-source { font-size:10.5px; color:#64748B; font-style:italic;
                             margin-top:1px; }
        .pr-summary-pill {
          font-size:10px; font-weight:700; padding:2px 8px; border-radius:10px;
          letter-spacing:.3px; flex-shrink:0; white-space:nowrap;
        }
        .pr-summary-empty { background:#F1F5F9; color:#94A3B8; }
        .pr-summary-ok    { background:#D4F5EC; color:#0F6E56; }
      ")),

      # Excel import card — the only way data enters this chart.
      div(class = "pr-import-card",
        div(style = "font-size:11.5px;font-weight:600;color:#0F172A;margin-bottom:6px;",
            HTML("&#128193; Excel project plan")),
        div(class = "pr-import-row",
            textInput("pr_excel_path", label = NULL,
                      value = saved_path, width = "100%",
                      placeholder = "K:/BCTU/Teams/MyTeam/MyTrial/Portfolio Review.xlsx"),
            actionButton("pr_excel_import", "Import",
                         class = "btn-primary-sm")),
        div(style = "font-size:10.5px;color:#64748B;margin-top:6px;line-height:1.5;",
            "Workbook must have one row per month. Column A = month dates, ",
            "with one column per series labelled like ",
            tags$em("Pts - original projection, monthly"), ", ",
            tags$em("Pts - original projection, cumulative"), ", ",
            tags$em("Sites - projected, cumulative"), ".")
      ),

      div(class = "pr-controls",
          div(tags$label("Start month",
                         style = "font-size:11px;font-weight:600;color:#0F172A;
                                  margin-bottom:3px;display:block;"),
              div(style = "padding:7px 10px;background:#F8FAFD;border:1px solid #E2E8EE;
                           border-radius:6px;font-size:12px;color:#0F172A;
                           font-variant-numeric:tabular-nums;",
                  format(sd, "%b %Y"))),
          div(tags$label("Number of months",
                         style = "font-size:11px;font-weight:600;color:#0F172A;
                                  margin-bottom:3px;display:block;"),
              div(style = "padding:7px 10px;background:#F8FAFD;border:1px solid #E2E8EE;
                           border-radius:6px;font-size:12px;color:#0F172A;
                           font-variant-numeric:tabular-nums;", n))),

      div(class = "pr-section-eye", "Projected (from Excel)"),
      proj_rows,

      div(class = "pr-section-eye", "Actual (auto-computed)"),
      actual_rows,

      div(style = "margin-top:14px;",
          downloadButton("pr_download_png",
                         HTML("&#x21E9; Download PNG (chart + table)"),
                         class = "btn-primary-sm",
                         style = "background:#1B4F6B;color:#fff;border-color:#1B4F6B;
                                  font-weight:600;width:100%;"))
    )
  })

  # ── Reactive bundling all 6 series + months ──────────────────────────────
  # Projected series: pr_proj() (loaded from JSON or import).
  # Actual series:    computed from rv$raw_redcap / rv$sites every render
  #                   (so the chain auto-invalidates when CSV is uploaded).
  pr_data <- reactive({
    n  <- pr_n_months()
    sd <- pr_start_date()
    months_seq <- .pr_default_months(n, sd)
    months_lbl <- format(months_seq, "%b-%y")
    df <- data.frame(month = months_lbl, stringsAsFactors = FALSE)

    proj <- pr_proj()
    pad_or_zero <- function(v, n) {
      v <- suppressWarnings(as.numeric(unlist(v)))
      v[is.na(v)] <- 0
      if (length(v) >= n) v[seq_len(n)] else c(v, rep(0, n - length(v)))
    }
    df$proj_pts_monthly <- pad_or_zero(proj$proj_pts_monthly, n)
    df$proj_pts_cum     <- pad_or_zero(proj$proj_pts_cum,     n)
    df$proj_sites_cum   <- pad_or_zero(proj$proj_sites_cum,   n)

    df$actual_pts_monthly  <- .pr_actual_pts_monthly(sd, n)
    df$actual_pts_cum      <- .pr_actual_pts_cumulative(sd, n)
    df$actual_sites_cum    <- .pr_actual_sites_cumulative(sd, n)

    df$month <- factor(df$month, levels = df$month)
    df
  })

  output$pr_chart <- renderEcharts4r({
    df <- pr_data()
    if (is.null(df) || !nrow(df))
      return(empty_echart("Import an Excel project plan to populate the chart"))
    cfg <- rv$trial_config
    title <- sprintf("%s — Projected and actual recruitment",
                     cfg$short_name %||% toupper(cfg$code %||% "Trial"))
    chart <- df |> echarts4r::e_charts(month)
    for (s in PR_SERIES_DEFS) {
      chart <- chart |> echarts4r::e_line_(
        serie       = s$key,
        name        = s$label,
        symbol      = if (s$lty == "solid") "rect" else "circle",
        symbolSize  = 5,
        showSymbol  = TRUE,
        connectNulls = TRUE,
        lineStyle   = list(color = s$color, width = s$lw,
                           type  = if (s$lty == "dashed") "dashed" else "solid"),
        itemStyle   = list(color = s$color)
      )
    }
    chart |>
      echarts4r::e_title(title, left = "center",
                         textStyle = list(color = "#1B4F6B", fontSize = 16,
                                          fontWeight = 700)) |>
      echarts4r::e_tooltip(trigger = "axis") |>
      echarts4r::e_legend(top = 30,
                          textStyle = list(fontSize = 11, color = "#475569")) |>
      echarts4r::e_grid(left = 60, right = 30, top = 110, bottom = 60) |>
      echarts4r::e_x_axis(axisLabel = list(rotate = 45, fontSize = 10,
                                           color = "#64748B"),
                          axisLine = list(lineStyle = list(color = "#CBD5E1"))) |>
      echarts4r::e_y_axis(axisLabel = list(fontSize = 10, color = "#64748B"),
                          splitLine = list(lineStyle = list(color = "#EEF3F8")))
  })

  # ── Data table that mirrors the Excel layout (rows = series, cols = months)
  # Rendered below the chart so what the user sees is exactly what gets baked
  # into the downloaded PNG.
  output$pr_data_table <- renderUI({
    df <- pr_data()
    if (is.null(df) || !nrow(df)) return(NULL)
    months <- as.character(df$month)
    header <- tags$tr(
      tags$th(class = "pr-tbl-rowhead", "Series"),
      lapply(months, function(m) tags$th(class = "pr-tbl-mhead", m)))
    body_rows <- lapply(PR_SERIES_DEFS, function(s) {
      vals <- df[[s$key]]
      tags$tr(
        tags$td(class = "pr-tbl-rowhead",
          tags$span(class = "pr-color-dot",
                    style = sprintf("background:%s;margin-right:6px;
                                     display:inline-block;vertical-align:middle;",
                                    s$color)),
          s$label),
        lapply(vals, function(v) tags$td(class = "pr-tbl-val",
          if (is.na(v)) "" else format(round(v, 1), big.mark = ",",
                                       trim = TRUE, scientific = FALSE))))
    })
    div(class = "pr-tbl-wrap",
        tags$style(HTML("
          .pr-tbl-wrap { overflow-x:auto; border:1px solid #E2E8EE;
                         border-radius:8px; background:#fff; margin-top:10px; }
          .pr-tbl { border-collapse:collapse; font-size:11.5px;
                    font-family:-apple-system, system-ui, sans-serif;
                    white-space:nowrap; min-width:100%; }
          .pr-tbl th, .pr-tbl td { padding:6px 10px; }
          .pr-tbl-rowhead { background:#F8FAFD; color:#0F172A; font-weight:600;
                            text-align:left; border-bottom:1px solid #E2E8EE;
                            border-right:1px solid #E2E8EE; position:sticky;
                            left:0; }
          .pr-tbl-mhead { background:#1B4F6B; color:#fff; font-weight:600;
                          text-align:center; font-size:11px; }
          .pr-tbl-val { text-align:right; color:#1B4F6B; font-weight:500;
                        font-variant-numeric:tabular-nums;
                        border-bottom:1px solid #EEF3F8; }
          .pr-tbl tr:last-child td { border-bottom:none; }
        ")),
        tags$table(class = "pr-tbl",
                   tags$thead(header),
                   tags$tbody(body_rows)))
  })

  # ── Save / Reset / Excel import / PNG download ──────────────────────────
  observeEvent(input$pr_save, {
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    n <- pr_n_months()
    proj <- pr_proj()
    series <- setNames(
      lapply(PR_PROJECTED_DEFS, function(s) {
        v <- suppressWarnings(as.numeric(unlist(proj[[s$key]])))
        v[is.na(v)] <- 0
        if (length(v) >= n) v[seq_len(n)] else c(v, rep(0, n - length(v)))
      }),
      vapply(PR_PROJECTED_DEFS, `[[`, character(1), "key"))
    payload <- list(
      n_months    = n,
      start_month = format(pr_start_date(), "%Y-%m-%d"),
      excel_path  = trimws(input$pr_excel_path %||% pr_excel_path_state()),
      series      = series
    )
    tryCatch({
      update_overrides(cfg, portfolio_review = payload)
      rv$trial_config$portfolio_review <- payload
      pr_excel_path_state(payload$excel_path)
      showNotification(HTML("&check; Portfolio review chart saved."),
                       type = "message", duration = 4)
    }, error = function(e) {
      showNotification(paste("Save failed:", e$message),
                       type = "error", duration = 8)
    })
  })

  observeEvent(input$pr_reset, {
    pr_n_months(27L)
    pr_start_date(.pr_default_months(1)[1])
    pr_proj(list())
    pr_excel_path_state("")
    showNotification("Portfolio review chart reset.",
                     type = "message", duration = 3)
  })

  observeEvent(input$pr_excel_import, {
    path <- input$pr_excel_path %||% ""
    res <- tryCatch(.pr_import_excel(path),
                    error = function(e) {
                      showNotification(paste("Import failed:", e$message),
                                       type = "error", duration = 10)
                      NULL
                    })
    if (is.null(res)) return()

    pr_n_months(max(1L, as.integer(res$n_months)))
    if (!is.null(res$start_date) && !is.na(res$start_date))
      pr_start_date(res$start_date)
    pr_proj(res$series)
    pr_excel_path_state(trimws(path))

    showNotification(
      HTML(sprintf("&check; Imported %d projected series from <code>%s</code>.
                    Window: %d months from %s.",
                   length(res$series),
                   htmltools::htmlEscape(basename(path)),
                   res$n_months, format(res$start_date, "%b %Y"))),
      type = "message", duration = 7)
  })

  # ── PNG download (chart + table baked into a single image) ──────────────
  output$pr_download_png <- downloadHandler(
    filename = function() {
      cfg  <- rv$trial_config
      slug <- gsub("[^A-Za-z0-9]", "_", cfg$short_name %||% "trial")
      sprintf("%s_portfolio_review_%s.png", slug, format(Sys.Date(), "%Y-%m-%d"))
    },
    content = function(file) {
      df <- pr_data()
      if (is.null(df) || !nrow(df)) {
        showNotification("Nothing to render — import the Excel project plan first.",
                         type = "warning", duration = 6)
        return()
      }
      cfg   <- rv$trial_config
      title <- sprintf("%s — Projected and actual recruitment",
                       cfg$short_name %||% toupper(cfg$code %||% "Trial"))

      ok <- tryCatch({
        .pr_render_png(file = file, df = df, title = title,
                       series_defs = PR_SERIES_DEFS)
        TRUE
      }, error = function(e) {
        showNotification(paste("PNG generation failed:", e$message),
                         type = "error", duration = 10)
        FALSE
      })
      if (!isTRUE(ok)) return()
    }
  )
}

# =============================================================================
# Render a portfolio-review PNG: chart on top, data table beneath. Uses ggplot2
# (always available) for the chart and gridExtra::tableGrob (preferred) for
# the table. Falls back to a chart-only PNG when gridExtra isn't installed.
# Standalone so it's testable outside the Shiny session.
# =============================================================================
.pr_render_png <- function(file, df, title, series_defs,
                            width = 14, height = 9, dpi = 150) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 not installed (install.packages('ggplot2'))")
  if (!requireNamespace("tidyr", quietly = TRUE) &&
      !requireNamespace("reshape2", quietly = TRUE))
    stop("tidyr or reshape2 needed for long-form reshape")

  # Long-format for ggplot
  series_keys  <- vapply(series_defs, `[[`, character(1), "key")
  series_lbls  <- vapply(series_defs, `[[`, character(1), "label")
  series_cols  <- vapply(series_defs, `[[`, character(1), "color")
  series_lty   <- vapply(series_defs, `[[`, character(1), "lty")
  series_lw    <- vapply(series_defs, `[[`, numeric(1),   "lw")

  long <- if (requireNamespace("tidyr", quietly = TRUE)) {
    tidyr::pivot_longer(df, cols = all_of(series_keys),
                        names_to = "series", values_to = "value")
  } else {
    do.call(rbind, lapply(series_keys, function(k)
      data.frame(month = df$month, series = k, value = df[[k]],
                 stringsAsFactors = FALSE)))
  }
  long$series <- factor(long$series, levels = series_keys, labels = series_lbls)

  p <- ggplot2::ggplot(long, ggplot2::aes(x = month, y = value, group = series,
                                          colour = series, linetype = series)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 1.4) +
    ggplot2::scale_colour_manual(values = setNames(series_cols, series_lbls)) +
    ggplot2::scale_linetype_manual(values = setNames(
      ifelse(series_lty == "dashed", "dashed", "solid"), series_lbls)) +
    ggplot2::labs(title = title, x = NULL, y = NULL,
                  colour = NULL, linetype = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold", colour = "#1B4F6B",
                                                 size = 14, hjust = 0.5),
      legend.position    = "top",
      legend.text        = ggplot2::element_text(size = 9, colour = "#475569"),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major   = ggplot2::element_line(colour = "#EEF3F8"),
      axis.text.x        = ggplot2::element_text(angle = 45, hjust = 1,
                                                 size = 8, colour = "#64748B"),
      axis.text.y        = ggplot2::element_text(size = 8, colour = "#64748B"),
      plot.margin        = ggplot2::margin(10, 14, 6, 14)) +
    ggplot2::guides(colour   = ggplot2::guide_legend(nrow = 2),
                    linetype = ggplot2::guide_legend(nrow = 2))

  # Table grob — rows = series, cols = months. Falls back to chart-only if
  # gridExtra is missing.
  if (requireNamespace("gridExtra", quietly = TRUE) &&
      requireNamespace("grid", quietly = TRUE)) {
    tbl <- as.data.frame(t(df[, series_keys, drop = FALSE]),
                         stringsAsFactors = FALSE)
    colnames(tbl) <- as.character(df$month)
    tbl <- cbind(Series = series_lbls, tbl)
    # Format numeric cells nicely
    for (j in 2:ncol(tbl)) {
      tbl[[j]] <- vapply(tbl[[j]], function(v) {
        v <- suppressWarnings(as.numeric(v))
        if (is.na(v)) "" else format(round(v, 1), big.mark = ",",
                                     trim = TRUE, scientific = FALSE)
      }, character(1))
    }
    tt <- gridExtra::ttheme_minimal(
      core = list(fg_params = list(cex = 0.62, hjust = 1, x = 0.95),
                  bg_params = list(fill = c("#FFFFFF", "#F8FAFD"))),
      colhead = list(fg_params = list(cex = 0.62, col = "#FFFFFF",
                                       fontface = "bold"),
                     bg_params = list(fill = "#1B4F6B")),
      rowhead = list(fg_params = list(cex = 0.62, hjust = 0, x = 0.02,
                                       fontface = "bold", col = "#0F172A"),
                     bg_params = list(fill = "#F1F5F9"))
    )
    g_table <- gridExtra::tableGrob(tbl[, -1, drop = FALSE],
                                    rows = tbl$Series, theme = tt)
    grDevices::png(file, width = width, height = height, units = "in",
                   res = dpi, type = "cairo")
    on.exit(grDevices::dev.off(), add = TRUE)
    gridExtra::grid.arrange(p, g_table, ncol = 1, heights = c(2.2, 1))
  } else {
    grDevices::png(file, width = width, height = height, units = "in",
                   res = dpi, type = "cairo")
    on.exit(grDevices::dev.off(), add = TRUE)
    print(p)
    message("gridExtra not installed — saved chart only without the table. ",
            "Install with: install.packages('gridExtra')")
  }
  invisible(file)
}
