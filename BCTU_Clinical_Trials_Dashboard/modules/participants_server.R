# ─────────────────────────────────────────────────────────────────────────────
# Data tab server.
# ----------------------------------------------------------------------------
# - Donut KPI cards driven by event_type counts (Baseline/Discharge/D30/D90).
# - Four clickable safety tiles, exactly one open at a time. Drill-down
#   pulls per-event detail via functions/safety_events.R helpers (sae_events,
#   deviation_events, withdrawal_events, preg_notif_events, preg_out_events)
#   which all resolve column names through fld(), so the same code works for
#   any trial. Missing columns render as em-dashes.
# - Withdrawal donut shows counts by COS code; clickable wedges via the
#   withdrawals tile.
# - Demographics rail keeps the existing configurable-breakdowns machinery
#   (functions/participant_breakdowns.R).
# ─────────────────────────────────────────────────────────────────────────────

participants_server <- function(input, output, session, state) {
  rv <- state$rv
  # WP-scoped views — the Data tab's cards, donuts, safety tiles and demographic
  # breakdowns all follow the active work package. File exports below stay on the
  # full rv$ stores so a download is never a silent partial export.
  parts_wp  <- state$parts_wp
  redcap_wp <- state$redcap_wp

  # Which safety tile is currently expanded ("sae" / "dev" / "wd" / "preg" /
  # NULL for nothing open). One-open-at-a-time.
  active_drill <- reactiveVal(NULL)

  # ── Data health (shared th_build via state$health) ───────────────────────
  health <- state$health

  output$data_health_kpis <- renderUI({
    H <- health()
    if (is.null(H) || !nrow(H$participants)) return(NULL)
    sm <- H$summary; a <- sm$attention
    kpi <- function(l, v, s, bad = FALSE) div(class = paste("th-kpi", if (bad) "bad"),
      div(class = "th-kpi-l", l), div(class = "th-kpi-v", v), div(class = "th-kpi-s", s))
    div(class = "th-kpis",
      kpi("CRF completeness", if (is.na(sm$completeness)) "—" else sprintf("%.0f%%", sm$completeness),
          sprintf("%d of %d expected CRFs entered", sm$complete_crfs, sm$expected_crfs)),
      kpi("Overdue CRFs", sm$overdue_crfs, "site forms past their grace period", sm$overdue_crfs > 0),
      kpi("Overdue questionnaires", sm$overdue_proms, "participant-completed forms"),
      kpi("Need urgent follow-up", a[["High"]], sprintf("%d more at medium priority", a[["Medium"]]),
          a[["High"]] > 0),
      kpi("Due now", sum(H$crf$status == "due"), "inside the grace window"))
  })

  output$crf_grid_ui <- renderUI({
    H <- health()
    if (is.null(H)) return(th_empty_note("Load a REDCap export to see CRF returns."))
    th_crfgrid_widget(H, "th-crfgrid-data")
  })

  observe({
    H <- health()
    s <- if (is.null(H) || is.null(H$issues)) character() else sort(unique(H$issues$site))
    updateSelectInput(session, "wl_site", choices = c("All sites" = "", s),
                      selected = isolate(input$wl_site) %||% "")
  })

  worklist <- reactive({
    H <- health()
    iss <- if (is.null(H)) NULL else H$issues
    if (is.null(iss) || !nrow(iss)) return(NULL)
    if (nzchar(input$wl_site %||% "")) iss <- iss[iss$site == input$wl_site, , drop = FALSE]
    iss[iss$priority %in% (input$wl_prio %||% character()), , drop = FALSE]
  })

  output$worklist_table <- renderReactable({
    iss <- worklist()
    if (is.null(iss) || !nrow(iss))
      return(empty_reactable("No participants need attention with these filters."))
    reactable(
      iss[, c("priority", "id", "site", "stage", "issues", "overdue_forms", "max_days_overdue")],
      compact = TRUE, highlight = TRUE, searchable = TRUE,
      defaultPageSize = 15, showPageSizeOptions = TRUE, pageSizeOptions = c(15, 30, 60),
      onClick = htmlwidgets::JS("function(row) { Shiny.setInputValue('th_participant_open', {id: row.values.id, n: Math.random()}, {priority: 'event'}); }"),
      rowStyle = list(cursor = "pointer"),
      defaultColDef = colDef(style = list(fontSize = "12.5px")),
      columns = list(
        priority = colDef(name = "Priority", width = 96,
                          cell = function(v) span(class = paste0("th-prio th-prio-", v), v)),
        id = colDef(name = "Participant", width = 110, style = list(fontWeight = 600, fontSize = "12.5px")),
        site = colDef(name = "Site", width = 130),
        stage = colDef(name = "Where they are", width = 150),
        issues = colDef(name = "What needs doing", minWidth = 240),
        overdue_forms = colDef(name = "Overdue forms", minWidth = 260,
                               style = list(color = "#58595B", fontSize = "12px")),
        max_days_overdue = colDef(name = "Longest overdue", width = 124, align = "right",
                                  cell = function(v) if (v > 0) paste(v, "days") else "—")))
  })

  output$dl_worklist <- downloadHandler(
    filename = function() sprintf("%s_worklist_%s.csv", rv$trial_config$code %||% "trial",
                                  format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      iss <- worklist()
      utils::write.csv(if (is.null(iss)) data.frame() else iss, file, row.names = FALSE)
    })

  # ── Donut KPI cards ────────────────────────────────────────────────────
  # Denominator for EVERY timepoint donut = number RANDOMISED (the same basis
  # the recruitment charts use), not "number of participants who appear in the
  # export". Numerators = participants whose form for that timepoint is COMPLETE
  # (REDCap *_complete == 2), resolved through the trial config so it works for
  # any trial.
  n_event <- function(et) {
    df <- parts_wp()
    if (is.null(df) || nrow(df) == 0) return(0L)
    length(unique(df$record_id[df$event_type == et]))
  }
  total_p <- function() {
    df <- parts_wp()
    if (is.null(df) || nrow(df) == 0) return(0L)
    length(unique(df$record_id))
  }

  # Number randomised — denominator for all four timepoint donuts. A record is
  # randomised when its randomisation-datetime field is non-empty (matches
  # reports_server's recruitment logic). A trial that defines a recruitment
  # model uses that instead, so the denominator is the recruited count rather
  # than everyone with a registration date. Falls back to total participants
  # only if the randomisation column can't be found.
  n_randomised <- function() {
    raw <- redcap_wp()
    if (is.null(raw) || nrow(raw) == 0 || !"record_id" %in% names(raw))
      return(0L)
    ids <- randomised_ids(raw, rv$trial_config)
    if (is.null(ids)) return(total_p())
    length(ids)
  }

  # Baseline donut's numerator: Baseline-event rows for RANDOMISED
  # participants only, so it can never read more than n_randomised(). A
  # participant on file (e.g. consented) but not yet randomised used to
  # inflate this past 100% — that count is shown separately on the Overview
  # tab instead of leaking into this ratio.
  n_baseline_randomised <- function() {
    raw <- redcap_wp()
    df  <- parts_wp()
    if (is.null(df) || !nrow(df)) return(0L)
    base_ids <- unique(df$record_id[df$event_type == "Baseline"])
    ids <- randomised_ids(raw, rv$trial_config)
    if (is.null(ids)) return(length(base_ids))
    length(intersect(base_ids, ids))
  }

  # Count participants whose form for a timepoint is COMPLETE.
  #   field_role     – redcap_fields key for the *_complete variable
  #                    (e.g. "discharge_complete", "day30_complete").
  #   event_role     – redcap_events key for the longitudinal event the form is
  #                    recorded at (e.g. "discharge", "day_30", "day_90").
  #   fallback_event – event_type label used if the completion field can't be
  #                    found, so the card degrades to event-presence instead of 0.
  # Restricting to the matching redcap_event_name is essential for TONIC because
  # the same `post_operation_complete` field is collected at BOTH day_30_arm_1
  # and day_90_arm_1 — without the event filter Day 30 and Day 90 would count
  # each other's completions.
  n_complete <- function(field_role, event_role, fallback_event) {
    raw <- redcap_wp()
    if (is.null(raw) || nrow(raw) == 0 || !"record_id" %in% names(raw))
      return(n_event(fallback_event))
    col <- fld(field_role, NA_character_)
    if (is.na(col) || !nzchar(col) || !col %in% names(raw))
      return(n_event(fallback_event))
    done <- trimws(as.character(raw[[col]])) %in% c("2", "Complete", "complete")

    # Limit to the correct longitudinal event when both the event column and a
    # configured event name are available.
    ev_col  <- fld("redcap_event_name", "redcap_event_name")
    ev_name <- evt(event_role, NA_character_)
    if (ev_col %in% names(raw) && !is.na(ev_name) && nzchar(ev_name)) {
      done <- done & (trimws(as.character(raw[[ev_col]])) == ev_name)
    }
    length(unique(raw$record_id[done]))
  }

  # SVG donut: pct in 0..100. The label is intentionally NOT rendered inside
  # the ring — it's already shown to the right of the donut as the panel
  # heading, and crowding it inside causes overlap on long words like
  # "DISCHARGE". The centre shows only the percentage.
  donut_svg <- function(pct, ring, fill, label = NULL) {
    pct  <- max(0, min(100, as.numeric(pct)))
    circ <- 201   # 2 * pi * r where r = 32
    dash <- round(circ * pct / 100, 1)
    HTML(sprintf(
'<svg viewBox="0 0 80 80" width="80" height="80" style="transform:rotate(-90deg)">
  <circle cx="40" cy="40" r="32" stroke="%s" stroke-width="10" fill="none"/>
  <circle cx="40" cy="40" r="32" stroke="%s" stroke-width="10" fill="none"
          stroke-dasharray="%s %s" stroke-linecap="round"/>
</svg>
<div class="donut-ctr">%d%%</div>',
      ring, fill, dash, circ, round(pct)))
  }

  donut_card_ui <- function(n, n_total, label, ring, fill, sub_extra = "",
                            not_due = FALSE) {
    # A timepoint whose window has not opened for anyone is not 0% complete —
    # there is nothing to complete yet. Say so instead of showing a bare zero.
    if (isTRUE(not_due)) {
      return(div(class = "donut-card",
        div(class = "donut", donut_svg(0, ring, "#CFCFCF", toupper(label))),
        div(class = "donut-info",
            div(class = "lbl", label),
            div(class = "vv", style = "font-size:15px;color:#58595B;", "Not yet due"),
            div(class = "sub", sub_extra))))
    }
    pct <- if (n_total > 0) 100 * n / n_total else 0
    div(class = "donut-card",
      div(class = "donut", donut_svg(pct, ring, fill, toupper(label))),
      div(class = "donut-info",
          div(class = "lbl", label),
          div(class = "vv", as.character(n),
              tags$span(style = "font-size:13px;color:#58595B;font-weight:500;",
                        sprintf(" / %d", n_total))),
          div(class = "sub", sub_extra)
      )
    )
  }

  # Per-participant anchor dates for the follow-up schedule: the recruitment
  # date every offset counts from, plus discharge dates for trials whose
  # follow-up runs from discharge instead.
  .timepoint_anchors <- function() {
    raw <- redcap_wp()
    empty <- list(ids = character(0), recruitment = character(0),
                  discharge = character(0))
    if (is.null(raw) || !nrow(raw) || !"record_id" %in% names(raw)) return(empty)
    spec <- recruitment_spec(rv$trial_config)
    ids  <- recruitment_counts(raw, rv$trial_config)$recruited
    if (!length(ids)) return(empty)

    pick <- function(field) {
      field <- as.character(unlist(field %||% character(0)))
      field <- field[!is.na(field) & nzchar(field) & field %in% names(raw)]
      if (!length(field))
        return(stats::setNames(rep(NA, length(ids)), ids))
      v <- .rec_values(raw, field[1])
      out <- stats::setNames(rep(NA_character_, length(ids)), ids)
      hit <- intersect(names(v), ids)
      out[hit] <- v[hit]
      suppressWarnings(stats::setNames(as.Date(substr(out, 1, 10)), ids))
    }
    list(ids = ids, recruitment = pick(spec$date_candidates %||% spec$date_field),
         discharge = pick(rv$trial_config$redcap_fields$discharge_date))
  }

  # Turn a redcap_events role key into a readable label: "day_30" -> "Day 30",
  # "month_12" -> "Month 12", "week_6" -> "Week 6".
  .pretty_role <- function(role) tools::toTitleCase(gsub("_", " ", role))

  # Timepoint donuts, driven by the trial's configured events so each trial
  # shows ITS timepoints (e.g. Week 6 / Month 6 / Month 12) rather than a fixed
  # Baseline/Day 30/Day 90 set. Baseline is always first; the remaining donuts
  # come from cfg$redcap_events (order preserved, excluding sub_forms). Where a
  # completion field is configured (TONIC-style) the donut shows completion %,
  # otherwise it falls back to "records present at this event".
  output$data_donuts <- renderUI({
    den <- n_randomised()
    base_n <- n_baseline_randomised()
    cards <- list(donut_card_ui(base_n, den, "Baseline",
                  ring = "#EFEFEF", fill = trial_palette()[["primary"]],
                  sub_extra = sprintf("%d missing", max(0L, den - base_n))))

    ev    <- (rv$trial_config$redcap_events) %||% list()
    roles <- setdiff(names(ev), c("baseline", "sub_forms"))
    if (!length(roles)) roles <- c("discharge", "day_30", "day_90")

    # Known completion-field roles (a list so a missing key returns NULL, not an
    # error) keep TONIC's completion donuts working; others use <role>_complete.
    known_fields <- list(discharge = "discharge_complete",
                         day_30 = "day30_complete", day_90 = "day90_complete")
    # Completion donuts are trial charts: one ring colour, the trial's primary
    palette <- list(c("#EFEFEF", trial_palette()[["primary"]]))

    # How many participants each timepoint has actually fallen due for. A
    # trial three months old has no 6-month follow-ups: its event is not in the
    # export at all, and counting it against everyone recruited would report a
    # failure that has not happened. Trials with no schedule configured keep
    # the old denominator (everyone recruited).
    tp_meta <- timepoint_spec(rv$trial_config)
    anchors <- .timepoint_anchors()
    due_for <- function(role) {
      m <- tp_meta[[role]]
      if (is.null(m) || is.na(m$offset_days) || !length(anchors$ids)) return(den)
      a <- if (identical(m$anchor, "discharge")) anchors$discharge else anchors$recruitment
      length(timepoint_due_ids(anchors$ids, a, m$offset_days, m$window_days))
    }

    for (i in seq_along(roles)) {
      role <- roles[i]
      m    <- tp_meta[[role]]
      lbl  <- (m$label %||% .pretty_role(role))
      fr   <- known_fields[[role]] %||% paste0(role, "_complete")
      num  <- n_complete(fr, role, lbl)
      dn   <- due_for(role)
      pal  <- palette[[((i - 1) %% length(palette)) + 1]]
      cards[[length(cards) + 1]] <- donut_card_ui(num, dn, lbl,
                    ring = pal[1], fill = pal[2],
                    not_due = dn == 0 && !is.null(m) && !is.na(m$offset_days),
                    sub_extra = if (dn == 0 && !is.null(m) && !is.na(m$offset_days))
                      sprintf("due %d days after %s", m$offset_days,
                              if (identical(m$anchor, "discharge")) "discharge" else "recruitment")
                    else sprintf("%d outstanding", max(0L, dn - num)))
    }
    div(class = "data-hero", cards)
  })

  # ── Event reactives ───────────────────────────────────────────────────
  # All cached per raw_redcap fingerprint so we don't recompute on every UI
  # render (the tile bodies subscribe to them).
  fp <- function() {
    raw <- redcap_wp()
    paste(rv$active_wp %||% 0L,
          if (is.null(raw)) 0L else nrow(raw),
          if (is.null(raw)) 0L else length(raw),
          # Detail-field mappings affect which columns are extracted, so
          # re-key the cache when they change.
          digest::digest(rv$trial_config$detail_fields %||% list()), sep = ":")
  }

  sae_df    <- reactive({ sae_events(redcap_wp()) })       %>% bindCache(fp())
  dev_df    <- reactive({ deviation_events(redcap_wp()) }) %>% bindCache(fp())
  wd_df     <- reactive({ withdrawal_events(redcap_wp()) })%>% bindCache(fp())
  pn_df     <- reactive({ preg_notif_events(redcap_wp()) })%>% bindCache(fp())
  po_df     <- reactive({ preg_out_events(redcap_wp()) })  %>% bindCache(fp())
  comp_df   <- reactive({ complication_events(redcap_wp()) }) %>% bindCache(fp())

  # Withdrawals & change of status: one summary feeds the tile, the panel and
  # the drill-down. cos_filter narrows the drill-down to one type or site.
  cos <- reactive({
    change_of_status_summary(redcap_wp(), rv$trial_config, wd = wd_df())
  })
  cos_filter <- reactiveVal(NULL)

  # Combined pregnancy view for the drill-down (notif + outcome).
  preg_df   <- reactive({ dplyr::bind_rows(pn_df(), po_df()) })

  # ── Safety tile bodies ─────────────────────────────────────────────────
  tile_body <- function(label, n, sub, active = FALSE) {
    tagList(
      span(class = "lbl", label),
      div(class = "vv", as.character(n)),
      div(class = "sub", sub),
      span(class = "arr", if (active) HTML("&#9660;") else HTML("&#9656;"))
    )
  }

  output$safety_tile_sae_body <- renderUI({
    df <- sae_df()
    n_sites <- length(unique(df$site[!is.na(df$site) & nchar(df$site) > 0]))
    tile_body(HTML("&#9888; SAEs"), nrow(df),
              sprintf("%d site%s", n_sites, if (n_sites == 1) "" else "s"),
              active = identical(active_drill(), "sae"))
  })
  output$safety_tile_dev_body <- renderUI({
    df <- dev_df()
    open <- sum(grepl("open|pending", tolower(df$status %||% "")), na.rm = TRUE)
    tile_body(HTML("&#9678; Deviations"), nrow(df),
              if (open > 0) sprintf("%d open", open) else "all closed",
              active = identical(active_drill(), "dev"))
  })
  output$safety_tile_wd_body <- renderUI({
    s <- cos()
    tile_body(HTML("&#8633; Change of status"), s$n_changed,
              if (s$n_rand > 0) sprintf("%.1f%% of randomised", 100 * s$overall)
              else "participants",
              active = identical(active_drill(), "wd"))
  })
  output$safety_tile_preg_body <- renderUI({
    df <- preg_df()
    n_n <- nrow(pn_df()); n_o <- nrow(po_df())
    tile_body(HTML("&#9968; Pregnancies"), nrow(df),
              sprintf("%d notif · %d outcome", n_n, n_o),
              active = identical(active_drill(), "preg"))
  })
  # Complications tile — only meaningful once complication columns are mapped.
  output$safety_tile_comp_body <- renderUI({
    if (!length(detail_fields_for("complication"))) return(NULL)
    df <- comp_df()
    tile_body(HTML("&#10010; Complications"), nrow(df),
              sprintf("%d participant%s", nrow(df), if (nrow(df) == 1) "" else "s"),
              active = identical(active_drill(), "comp"))
  })

  # ── Tile click handlers (toggle / switch) ──────────────────────────────
  set_drill <- function(key) {
    cur <- active_drill()
    active_drill(if (identical(cur, key)) NULL else key)
  }
  observeEvent(input$safety_tile_sae,  set_drill("sae"))
  observeEvent(input$safety_tile_dev,  set_drill("dev"))
  observeEvent(input$safety_tile_wd,   { cos_filter(NULL); set_drill("wd") })
  observeEvent(input$safety_tile_preg, set_drill("preg"))
  observeEvent(input$safety_tile_comp, set_drill("comp"))

  # Show the Complications tile only when complication columns are mapped.
  observeEvent(rv$trial_config, {
    shinyjs::toggle("safety_tile_comp",
                    condition = length(detail_fields_for("complication")) > 0)
  }, ignoreNULL = FALSE)

  # Mirror the active drill onto the tile DOM so CSS can highlight it.
  observe({
    key <- active_drill()
    shinyjs::runjs(sprintf("
      document.querySelectorAll('.s-tile').forEach(function(el){el.classList.remove('active');});
      var sel = %s;
      if (sel) { var el = document.getElementById(sel); if (el) el.classList.add('active'); }
    ", if (is.null(key)) "null"
       else sprintf("'safety_tile_%s'", key)))
  })

  # ── Drill-down renderer ────────────────────────────────────────────────
  output$safety_drill_ui <- renderUI({
    key <- active_drill()
    if (is.null(key)) return(NULL)
    # Withdrawals have their own columns (no severity, lag or status)
    if (identical(key, "wd")) return(cos_drill_ui())

    df <- switch(key,
      sae  = sae_df(),
      dev  = dev_df(),
      wd   = wd_df(),
      preg = preg_df(),
      comp = comp_df()
    )
    title <- switch(key,
      sae  = "Serious adverse events",
      dev  = "Protocol deviations",
      wd   = "Withdrawals / change of status",
      preg = "Pregnancy events",
      comp = "Complications"
    )
    icon_html <- switch(key,
      sae  = "!", dev = "?", wd = "←", preg = "⚙", comp = "+"
    )

    if (is.null(df) || nrow(df) == 0) {
      return(div(class = "safety-drill",
        div(class = "safety-drill-head",
          div(class = "safety-drill-title",
            span(class = "ic", icon_html), span(title, " · 0 events")),
          actionLink("safety_drill_close", HTML("&times;"),
                     class = "safety-drill-close")),
        div(style = "padding:18px;text-align:center;color:#58595B;font-size:12px;font-style:italic;",
            if (identical(key, "comp"))
              "No complications — map complication columns in Trial Settings → Detail fields, then upload your export."
            else "No events recorded.")
      ))
    }

    # Generic, column-aware render:
    #  · event-style rows (SAE/dev/wd/preg) show the standard clinical columns
    #  · every section also shows its mapped extra columns (x__<header>) and the
    #    reason/notes narrative, so SAE death/causality/expectedness, withdrawal
    #    cos/reason and complication details all surface.
    em        <- '<span style="color:#8A8A8C">&mdash;</span>'
    is_event  <- "term" %in% names(df)
    xcols     <- grep("^x__", names(df), value = TRUE)
    show_reason <- "narrative" %in% names(df) &&
      any(!is.na(df$narrative) & nzchar(trimws(df$narrative)))
    pal <- if (is_event) build_severity_palette(df$severity) else NULL

    cell <- function(v) if (is.null(v) || length(v) == 0 || is.na(v) || !nzchar(trimws(as.character(v))))
      HTML(em) else as.character(v)

    head_cells <- c(list(tags$th("Participant"), tags$th("Site")),
      if (is_event) list(tags$th("Event"), tags$th("Severity"),
                         tags$th("Date occurred"), tags$th("Date submitted"),
                         tags$th("Lag"), tags$th("Status")),
      lapply(xcols, function(c) tags$th(sub("^x__", "", c))),
      if (show_reason) list(tags$th("Reason / notes")))

    body <- lapply(seq_len(nrow(df)), function(i) {
      r <- df[i, ]
      cells <- c(
        list(tags$td(class = "id", as.character(r$record_id)),
             tags$td(cell(r$site))),
        if (is_event) list(
          tags$td(cell(r$term)),
          tags$td(HTML(severity_pill(r$severity, pal))),
          tags$td(if (!is.na(r$onset_date))  format(r$onset_date, "%d %b %Y") else HTML(em)),
          tags$td(if (!is.na(r$report_date)) format(r$report_date, "%d %b %Y") else HTML(em)),
          tags$td(HTML(lag_html(r$lag_days))),
          tags$td(HTML(status_dot(r$status)))),
        lapply(xcols, function(c) tags$td(cell(r[[c]]))),
        if (show_reason) list(tags$td(style = "max-width:280px;", cell(r$narrative))))
      do.call(tags$tr, cells)
    })

    div(class = "safety-drill",
      div(class = "safety-drill-head",
        div(class = "safety-drill-title",
          span(class = "ic", icon_html),
          span(sprintf("%s · %d event%s", title, nrow(df), if (nrow(df) == 1) "" else "s"))),
        div(class = "safety-drill-actions",
          actionLink("safety_drill_close", HTML("&times;"),
                     class = "safety-drill-close",
                     title = "Close"))
      ),
      tags$table(class = "safety-drill-tbl",
        tags$thead(do.call(tags$tr, head_cells)),
        tags$tbody(body)
      ),
      div(class = "safety-drill-foot",
        span(sprintf("%d of %d events shown%s",
                     nrow(df), nrow(df),
                     if (is_event) {
                       med <- suppressWarnings(stats::median(df$lag_days, na.rm = TRUE))
                       if (is.na(med)) "" else sprintf(" · median lag %d days", as.integer(med))
                     } else "")),
        span(style = "color:#8A8A8C;",
             "Source: REDCap export · mapped fields")
      )
    )
  })

  observeEvent(input$safety_drill_close, { active_drill(NULL) })

  # ── Withdrawals & change of status panel──────────────────────────────────────────────────
  output$cos_meta <- renderText({
    s <- cos()
    if (!s$n_rand) return("")
    sprintf("of %d randomised · click a type or site to list the participants", s$n_rand)
  })

  output$cos_panel_ui <- renderUI({
    s <- cos()
    if (!isTRUE(s$has_form))
      return(div(class = "cs-note cs-empty",
                 "This export has no change-of-status form. Map its columns in Settings → Data & mapping."))
    jsq  <- function(x) gsub("'", "\\\\'", gsub("\\\\", "\\\\\\\\", as.character(x)))
    pick <- function(kind, value) sprintf(
      "Shiny.setInputValue('cos_pick',{kind:'%s',value:'%s',n:Math.random()},{priority:'event'})",
      kind, jsq(value))
    pct <- function(x) if (s$n_rand > 0) sprintf("%.1f%%", 100 * x / s$n_rand) else "—"
    kpi <- function(l, v, sub, cls = NULL, of = NULL)
      div(class = paste(c("cs-kpi", cls), collapse = " "),
          div(class = "cs-kpi-l", l),
          div(class = "cs-kpi-v", v, if (!is.null(of)) span(class = "cs-kpi-of", of)),
          div(class = "cs-kpi-s", sub))

    # ── Headline figures ──
    trend <- if (s$n_last30 > s$n_prev30) "up" else if (s$n_last30 < s$n_prev30) "down" else "flat"
    arrow <- c(up = "&#9650;", down = "&#9660;", flat = "&#8211;")[[trend]]
    kpis <- div(class = "cs-kpis",
      kpi("Still in follow-up", s$n_active, sprintf("%s of randomised", pct(s$n_active)),
          "good", sprintf("/ %d", s$n_rand)),
      kpi("Any change of status", s$n_changed,
          sprintf("%s of randomised · %d record%s", pct(s$n_changed), s$n_events,
                  if (s$n_events == 1) "" else "s")),
      kpi("Left follow-up", s$n_ended,
          if (length(s$ending_labels)) paste(s$ending_labels, collapse = " · ")
          else "No status type is marked as ending follow-up",
          if (s$n_ended > 0) "warn"),
      kpi("Last 30 days", s$n_last30,
          HTML(sprintf("%s %d in the 30 days before%s", arrow, s$n_prev30,
                       if (!is.na(s$latest)) sprintf(" · latest %s", format(s$latest, "%d %b %Y")) else "")),
          c("trend", trend)))

    # ── By type, with reasons underneath ──
    ty <- s$types
    mx <- max(c(ty$people, 1))
    type_ui <- if (!nrow(ty)) div(class = "cs-note", "No change-of-status types are configured.") else
      lapply(seq_len(nrow(ty)), function(i) {
        r <- ty[i, ]
        tags$button(type = "button", class = paste("cs-row", if (r$people == 0) "zero"),
          title = if (nzchar(r$description)) r$description else r$label,
          onclick = pick("type", r$code), disabled = if (r$people == 0) NA else NULL,
          span(class = "cs-row-l", r$label,
               if (isTRUE(r$ends)) span(class = "cs-tag", "ends follow-up")),
          span(class = "cs-bar", span(class = "cs-bar-f", style = sprintf("width:%.1f%%;", 100 * r$people / mx))),
          span(class = "cs-row-n", r$people),
          span(class = "cs-row-p", pct(r$people)))
      })
    rs <- s$reasons
    reason_ui <- if (nrow(rs$table)) {
      top <- utils::head(rs$table, 5)
      div(class = "cs-sub", div(class = "cs-sub-t", "Most common reasons"),
          lapply(seq_len(nrow(top)), function(i)
            div(class = "cs-rsn", span(class = "cs-rsn-l", top$reason[i]),
                span(class = "cs-rsn-n", top$n[i]))))
    } else div(class = "cs-note",
      if (is.null(rs$field))
        "Reasons for withdrawal aren't mapped. Add the reason column under Settings → Data & mapping → Detail fields to see why people leave."
      else sprintf("No reasons recorded in this export yet (read from “%s”).", rs$field))

    # ── Per month ──
    mo <- s$months
    time_ui <- if (!s$n_events) div(class = "cs-note", "No changes of status recorded yet.") else {
      W <- 300; H <- 96; n <- nrow(mo); bw <- W / n; top <- max(c(mo$n, 1))
      marks <- vapply(seq_len(n), function(i) {
        h <- if (mo$n[i] > 0) max(3, mo$n[i] / top * (H - 18)) else 0
        x <- (i - 1) * bw
        paste0(sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="2" class="cs-mbar"><title>%s: %d</title></rect>',
                       x + bw * 0.18, H - h, bw * 0.64, h, format(mo$month[i], "%B %Y"), mo$n[i]),
               if (mo$n[i] > 0) sprintf('<text x="%.1f" y="%.1f" class="cs-mval">%d</text>',
                                        x + bw / 2, H - h - 4, mo$n[i]) else "")
      }, character(1))
      tagList(
        HTML(sprintf('<svg class="cs-chart" viewBox="0 0 %d %d" role="img" aria-label="Changes of status per month">%s<line x1="0" x2="%d" y1="%d" y2="%d" class="cs-base"/></svg>',
                     W, H, paste(marks, collapse = ""), W, H, H)),
        div(class = "cs-months", style = sprintf("grid-template-columns:repeat(%d,1fr);", n),
            lapply(seq_len(n), function(i)
              span(if (n <= 8 || (n - i) %% 2 == 0) format(mo$month[i], "%b") else ""))),
        div(class = "cs-foot",
            if (!is.na(s$median_days)) sprintf("Median %d days from randomisation to the change. ", s$median_days),
            if (s$n_undated) sprintf("%d record%s without a date not shown.", s$n_undated,
                                     if (s$n_undated == 1) "" else "s")))
    }

    # ── By site: each site's share of its own randomised participants ──
    st <- s$sites
    site_ui <- if (is.null(st) || nrow(st) < 2)
      div(class = "cs-note", "The site comparison appears once more than one site has randomised.") else {
      top <- utils::head(st, 8)
      mxs <- max(c(top$rate, s$overall, 0.01))
      tagList(
        lapply(seq_len(nrow(top)), function(i) {
          r <- top[i, ]
          tags$button(type = "button",
            class = paste("cs-row", if (isTRUE(r$high)) "high", if (r$n_changed == 0) "zero"),
            title = sprintf("%s: %d of %d randomised (%.0f%%)", r$site, r$n_changed, r$n_rand, 100 * r$rate),
            onclick = pick("site", r$site), disabled = if (r$n_changed == 0) NA else NULL,
            span(class = "cs-row-l", r$site),
            span(class = "cs-bar",
                 span(class = "cs-bar-f", style = sprintf("width:%.1f%%;", 100 * r$rate / mxs)),
                 span(class = "cs-bar-avg", style = sprintf("left:%.1f%%;", 100 * s$overall / mxs))),
            span(class = "cs-row-n", sprintf("%d/%d", r$n_changed, r$n_rand)),
            span(class = "cs-row-p", sprintf("%.0f%%", 100 * r$rate)))
        }),
        div(class = "cs-foot",
            sprintf("The line marks the trial-wide rate, %.0f%%.", 100 * s$overall),
            if (any(top$high)) " Amber sites are above it with at least 2 participants.",
            if (nrow(st) > 8) sprintf(" %d more site%s not shown.", nrow(st) - 8,
                                      if (nrow(st) - 8 == 1) "" else "s")))
    }

    div(class = "cs-panel", kpis,
        div(class = "cs-grid",
            div(class = "cs-col", div(class = "cs-col-t", "By type"), type_ui, reason_ui),
            div(class = "cs-col", div(class = "cs-col-t", "Per month"), time_ui),
            div(class = "cs-col", div(class = "cs-col-t", "By site"), site_ui)))
  })

  observeEvent(input$cos_pick, {
    p <- input$cos_pick
    ty <- cos()$types
    lab <- if (identical(p$kind, "type")) ty$label[ty$code == p$value][1] else p$value
    cos_filter(list(kind = p$kind, value = p$value, label = if (is.na(lab)) p$value else lab))
    active_drill("wd")
    shinyjs::runjs("setTimeout(function(){var d=document.querySelector('.safety-drill');if(d)d.scrollIntoView({behavior:'smooth',block:'start'});},300);")
  })
  observeEvent(input$cos_clear, cos_filter(NULL))

  # Drill-down for withdrawals: the columns that mean something for a change
  # of status, newest first, optionally narrowed to one type or site.
  cos_drill_ui <- function() {
    s <- cos(); ev <- s$events; f <- cos_filter()
    if (nrow(ev) && !is.null(f))
      ev <- if (identical(f$kind, "type")) ev[as.character(ev$severity) == f$value, , drop = FALSE]
            else ev[!is.na(ev$site) & ev$site == f$value, , drop = FALSE]
    em   <- HTML('<span class="cs-dash">&mdash;</span>')
    cell <- function(v) if (is.null(v) || !length(v) || is.na(v) || !nzchar(trimws(as.character(v)))) em else as.character(v)
    xcols <- grep("^x__", names(ev), value = TRUE)
    xcols <- xcols[vapply(xcols, function(c) any(!is.na(ev[[c]]) & nzchar(trimws(ev[[c]]))), logical(1))]
    show_reason <- "narrative" %in% names(ev) && any(!is.na(ev$narrative) & nzchar(trimws(ev$narrative)))
    if (nrow(ev)) ev <- ev[order(ev$onset_date, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
    n_people <- length(unique(ev$record_id))

    head_cells <- c(list(tags$th("Participant"), tags$th("Site"), tags$th("Change of status"),
                         tags$th("Date"), tags$th("Time in trial")),
                    lapply(xcols, function(c) tags$th(sub("^x__", "", c))),
                    if (show_reason) list(tags$th("Reason / notes")))
    body <- lapply(seq_len(nrow(ev)), function(i) {
      r <- ev[i, ]
      do.call(tags$tr, c(
        list(tags$td(class = "id", r$record_id), tags$td(cell(r$site)),
             tags$td(span(class = paste("cs-pill", if (isTRUE(r$ends)) "ends"), r$term)),
             tags$td(if (!is.na(r$onset_date)) format(r$onset_date, "%d %b %Y") else em),
             tags$td(if (!is.na(r$days_in)) sprintf("%d days", r$days_in) else em)),
        lapply(xcols, function(c) tags$td(cell(r[[c]]))),
        if (show_reason) list(tags$td(style = "max-width:280px;", cell(r$narrative)))))
    })

    div(class = "safety-drill",
      div(class = "safety-drill-head",
        div(class = "safety-drill-title",
            span(class = "ic", HTML("&larr;")),
            span(sprintf("Withdrawals & change of status · %d participant%s", n_people,
                         if (n_people == 1) "" else "s")),
            if (!is.null(f))
              span(class = "cs-chip", f$label,
                   actionLink("cos_clear", HTML("&times;"), class = "cs-chip-x", title = "Show everyone"))),
        div(class = "safety-drill-actions",
            actionLink("safety_drill_close", HTML("&times;"), class = "safety-drill-close", title = "Close"))),
      if (!nrow(ev)) div(class = "cs-note cs-empty", "No changes of status recorded.")
      else tags$table(class = "safety-drill-tbl",
                      tags$thead(do.call(tags$tr, head_cells)), tags$tbody(body)),
      div(class = "safety-drill-foot",
          span(sprintf("%d record%s%s", nrow(ev), if (nrow(ev) == 1) "" else "s",
                       if (!is.na(s$median_days))
                         sprintf(" · median %d days from randomisation to the change", s$median_days) else "")),
          span(class = "cs-muted", "Source: REDCap export · change-of-status form")))
  }

  # ── Demographic breakdowns (preserves existing config-driven machinery) ─
  # Column detection runs against the full export (column set is the same across
  # work packages, and the full data is the most robust for detection).
  detected_breakdowns <- reactive({
    cfg <- rv$trial_config
    detect_breakdown_columns(rv$raw_redcap, cfg)
  })

  selected_breakdowns <- reactiveVal(NULL)

  observeEvent(list(rv$trial_config, rv$raw_redcap), {
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    saved <- cfg$participant_breakdowns
    if (!is.null(saved) && length(saved) > 0) {
      selected_breakdowns(as.character(saved))
    } else {
      det <- detected_breakdowns()
      selected_breakdowns(default_breakdown_cols(det))
    }
  }, ignoreNULL = FALSE)

  output$demo_n_label <- renderText({
    n <- total_p()
    if (n == 0) "—" else sprintf("at baseline · %d participants", n)
  })

  output$participant_breakdowns_ui <- renderUI({
    raw <- redcap_wp()
    cfg <- rv$trial_config
    if (is.null(raw) || !nrow(raw))
      return(div(class = "info-box-tonic",
                 "No REDCap data — upload a CSV to populate demographics."))

    sel <- selected_breakdowns() %||% character(0)
    if (!length(sel))
      return(div(class = "info-box-tonic",
                 "No breakdowns configured. Click ‘Configure’ above to pick which demographic columns to show."))
    # Round-number bands on the Data tab. A column saved as a breakdown but
    # missing from this export gets a placeholder card instead of vanishing.
    breakdowns <- lapply(sel, function(c)
      compute_breakdown(raw, c, cfg, numeric_bins = "pretty") %||%
        list(type = "absent", label = .bd_title(c, cfg), column = c))
    tagList(render_demographics_strip(breakdowns), render_breakdowns_grid(breakdowns))
  })

  # ── Customise demographics (dialog) ───────────────────────────────────────
  # Which columns appear, in what order, what they are called, what each coded
  # value means, and how numeric ones are grouped (e.g. age under / over 75),
  # with a live preview. Edits go to a working copy (bdc) and are saved only on
  # Save. The dialog's own inputs send {col, field, value} events (see the
  # script in participants.R), so switching column never mixes their values.
  bdc <- reactiveValues(selected = character(0), titles = list(), cuts = list(),
                        labels = list(), active = NULL, nonce = 0)
  .s1   <- function(x) { x <- as.character(unlist(x)); if (!length(x) || is.na(x[1])) "" else x[1] }
  .bdcq <- function(x) gsub("'", "\\\\'", gsub("\\\\", "\\\\\\\\", as.character(x)))
  .bdc_ev <- function(id, col, extra = "")
    sprintf("Shiny.setInputValue('%s',{col:'%s'%s,n:Math.random()},{priority:'event'})", id, .bdcq(col), extra)
  .bdc_suggested <- function(cols)
    grepl("(?i)age|sex|gender|ethnic|nela|bmi|asa|frail|residence|smok", cols, perl = TRUE)

  # The values a column takes at baseline, most common first (numeric codes in order)
  .bdc_values <- function(col) {
    base <- baseline_rows(redcap_wp(), rv$trial_config)
    v <- if (col %in% names(base)) trimws(as.character(base[[col]])) else character(0)
    v <- v[!is.na(v) & nzchar(v)]
    tab <- table(v)
    out <- data.frame(code = names(tab), n = as.integer(tab), stringsAsFactors = FALSE)
    num <- suppressWarnings(as.numeric(out$code))
    out[if (all(!is.na(num))) order(num) else order(-out$n), , drop = FALSE]
  }

  # A column's grouping in editable form: being edited, else saved, else default
  .bdc_cut_edit <- function(col) {
    e <- bdc$cuts[[col]]
    if (!is.null(e) && !is.null(e$on)) return(e)
    ct <- breakdown_cut(col, rv$trial_config)
    if (is.null(ct)) list(on = FALSE, cut = "", unit = "", title = "Groups")
    else list(on = TRUE, cut = paste(as.character(ct$cut), collapse = ", "), unit = ct$unit, title = ct$title)
  }
  # Edited groupings back to the saved form (grouping off = no cut)
  .bdc_cuts_for_cfg <- function(cuts) {
    out <- list()
    for (col in names(cuts)) {
      e <- cuts[[col]]
      out[[col]] <- if (is.null(e$on)) e else
        list(cut = if (isTRUE(e$on)) .parse_cuts(e$cut) else numeric(0),
             unit = trimws(.s1(e$unit)),
             title = if (nzchar(trimws(.s1(e$title)))) trimws(.s1(e$title)) else "Groups")
    }
    out
  }

  observeEvent(input$configure_breakdowns, {
    det <- detected_breakdowns(); cfg <- rv$trial_config
    if (is.null(cfg) || is.null(det) || !nrow(det)) {
      showNotification("Load a REDCap export first: the columns to choose from come from it.",
                       type = "warning", duration = 5)
      return()
    }
    sel <- intersect(selected_breakdowns() %||% character(0), det$column)
    bdc$selected <- sel
    bdc$titles   <- cfg$breakdown_titles %||% list()
    bdc$cuts     <- cfg$breakdown_cuts   %||% list()
    bdc$labels   <- cfg$column_labels    %||% list()
    focus <- isolate(input$bd_cfg_focus)
    bdc$active <- if (!is.null(focus) && focus %in% det$column) focus
                  else if (length(sel)) sel[1] else det$column[1]
    bdc$nonce <- isolate(bdc$nonce) + 1
    showModal(modalDialog(
      title = NULL, footer = NULL, size = "xl", easyClose = FALSE,
      div(class = "bdc",
        div(class = "bdc-head",
          div(div(class = "bdc-title", "Customise demographics"),
              div(class = "bdc-sub",
                  "Choose which columns appear on the Data tab and in what order, what they are called, what each coded value means, and how numbers are grouped. The preview updates as you go; nothing is saved until you press Save.")),
          div(class = "bdc-head-actions",
              modalButton("Cancel"),
              actionButton("bd_cfg_save", "Save", class = "bdc-save"))),
        div(class = "bdc-body",
          tags$aside(class = "bdc-side",
            div(class = "bdc-h", "On the Data tab, in this order"),
            uiOutput("bd_cfg_order"),
            div(class = "bdc-h", "Columns in the export"),
            tags$input(type = "search", class = "bdc-search", placeholder = "Search columns",
                       `aria-label` = "Search columns", oninput = "bdcFilter(this.value)"),
            uiOutput("bd_cfg_list")),
          tags$section(class = "bdc-main",
            uiOutput("bd_cfg_editor"),
            div(class = "bdc-h", "Preview"),
            uiOutput("bd_cfg_preview"))))
    ))
  })

  # Working settings as a trial config, for the preview (debounced while typing)
  bdc_cfg <- reactive({
    cfg <- rv$trial_config; req(cfg)
    cfg$column_labels    <- bdc$labels
    cfg$breakdown_titles <- bdc$titles
    cfg$breakdown_cuts   <- .bdc_cuts_for_cfg(bdc$cuts)
    cfg
  })
  bdc_cfg_d <- debounce(bdc_cfg, 350)

  .bdc_name <- function(col, det) {
    t <- .s1(bdc$titles[[col]])
    if (nzchar(trimws(t))) trimws(t) else det$label[match(col, det$column)]
  }

  output$bd_cfg_order <- renderUI({
    s <- bdc$selected; a <- bdc$active; det <- detected_breakdowns()
    if (!length(s)) return(div(class = "bdc-empty", "Nothing chosen yet. Tick columns below to add them."))
    tags$ol(class = "bdc-order", lapply(seq_along(s), function(i) {
      col <- s[i]; nm <- .bdc_name(col, det)
      tags$li(class = paste("bdc-ord", if (identical(col, a)) "on"),
        tags$button(type = "button", class = "bdc-ord-name", title = nm,
                    onclick = .bdc_ev("bd_cfg_pick", col), nm),
        tags$button(type = "button", class = "bdc-ic", `aria-label` = paste("Move", nm, "up"),
                    disabled = if (i == 1) NA, onclick = .bdc_ev("bd_cfg_move", col, ",dir:'up'"), HTML("&uarr;")),
        tags$button(type = "button", class = "bdc-ic", `aria-label` = paste("Move", nm, "down"),
                    disabled = if (i == length(s)) NA, onclick = .bdc_ev("bd_cfg_move", col, ",dir:'down'"), HTML("&darr;")),
        tags$button(type = "button", class = "bdc-ic", `aria-label` = paste("Remove", nm),
                    onclick = .bdc_ev("bd_cfg_toggle", col), HTML("&times;")))
    }))
  })

  output$bd_cfg_list <- renderUI({
    det <- detected_breakdowns(); s <- bdc$selected; a <- bdc$active; bdc$titles
    base_n <- nrow(baseline_rows(redcap_wp(), rv$trial_config))
    sug <- .bdc_suggested(det$column)
    ord <- order(!sug, tolower(det$label))
    tagList(
      lapply(ord, function(i) {
        r <- det[i, ]; on <- r$column %in% s; nm <- .bdc_name(r$column, det)
        pct <- if (base_n > 0) max(0, min(100, 100 * (base_n - r$n_missing) / base_n)) else 0
        div(class = paste("bdc-col", if (on) "is-on", if (identical(r$column, a)) "is-active"),
            `data-q` = tolower(paste(nm, r$label, r$column)),
            tags$button(type = "button", class = "bdc-check", role = "checkbox",
                        `aria-checked` = if (on) "true" else "false",
                        `aria-label` = sprintf("Show %s on the Data tab", nm),
                        onclick = .bdc_ev("bd_cfg_toggle", r$column), if (on) HTML("&#10003;")),
            tags$button(type = "button", class = "bdc-col-main", onclick = .bdc_ev("bd_cfg_pick", r$column),
                        span(class = "bdc-col-name", nm, if (sug[i]) span(class = "bdc-tag", "suggested")),
                        span(class = "bdc-col-meta",
                             sprintf("%s · %s", r$column,
                                     if (r$type == "numeric") "numbers" else sprintf("%d values", r$n_unique))),
                        span(class = "bdc-bar", span(class = "bdc-bar-f", style = sprintf("width:%.0f%%;", pct))),
                        span(class = "bdc-col-pct", sprintf("%.0f%% recorded", pct))))
      }),
      # keep the search applied when the list redraws
      tags$script(HTML("var s=document.querySelector('.bdc-search'); if (s && window.bdcFilter) bdcFilter(s.value);")))
  })

  output$bd_cfg_editor <- renderUI({
    col <- bdc$active; bdc$nonce; sel <- bdc$selected
    req(col)
    det <- detected_breakdowns(); r <- det[det$column == col, , drop = FALSE]
    if (!nrow(r)) return(div(class = "bdc-empty", "Choose a column on the left."))
    isolate({
      on <- col %in% sel
      title <- trimws(.s1(bdc$titles[[col]]))
      inp <- function(field, value, placeholder = "", code = NULL, id = NULL)
        tags$input(type = "text", class = "bdc-in form-control", id = id, value = value,
                   placeholder = placeholder, autocomplete = "off",
                   `data-col` = col, `data-field` = field, `data-code` = code)
      head <- div(class = "bdc-ed-head",
        div(div(class = "bdc-ed-t", if (nzchar(title)) title else r$label),
            div(class = "bdc-ed-m", sprintf("%s · %s · %s", col,
                if (r$type == "numeric") "numbers" else sprintf("%d different values", r$n_unique),
                if (r$n_missing > 0) sprintf("%d missing", r$n_missing) else "none missing"))),
        tags$button(type = "button", class = paste("bdc-toggle", if (on) "on"),
                    onclick = .bdc_ev("bd_cfg_toggle", col),
                    if (on) HTML("&#10003; On the Data tab") else HTML("&#43; Add to the Data tab")))
      name_ui <- div(class = "bdc-fld",
        tags$label(`for` = "bdc_title", "Name"),
        inp("title", title, r$label, id = "bdc_title"),
        div(class = "bdc-hint", "Shown on the card and in reports. Leave blank to use the name worked out from the column."))

      body <- if (identical(r$type, "numeric")) {
        ce <- .bdc_cut_edit(col)
        div(class = "bdc-sec2",
          div(class = "bdc-sec2-h", "Groups"),
          tags$label(class = "bdc-switch",
                     tags$input(type = "checkbox", class = "bdc-in", `data-col` = col, `data-field` = "group_on",
                                checked = if (isTRUE(ce$on)) NA),
                     "Show groups as well as the spread"),
          if (isTRUE(ce$on)) div(class = "bdc-grid3",
            div(class = "bdc-fld", tags$label("Split at"), inp("cut", .s1(ce$cut), "e.g. 75, or 40, 60, 75")),
            div(class = "bdc-fld", tags$label("Unit (optional)"), inp("unit", .s1(ce$unit), "e.g. %")),
            div(class = "bdc-fld", tags$label("Heading"), inp("group_title", .s1(ce$title), "Groups"))),
          div(class = "bdc-hint",
              "One number makes two groups: under it, and it or over. For age, 75 gives “Under 75” and “75 or over”. Several numbers make more groups."))
      } else {
        vals <- .bdc_values(col)
        cur  <- bdc$labels[[col]] %||% list()
        cfg0 <- rv$trial_config; cfg0$column_labels[[col]] <- NULL
        sug  <- if (nrow(vals)) as.character(.resolve_value_labels(vals$code, col, cfg0)) else character(0)
        show <- utils::head(seq_len(nrow(vals)), 40)
        div(class = "bdc-sec2",
          div(class = "bdc-sec2-h", "What each value means",
              if (any(sug != vals$code))
                tags$button(type = "button", class = "bdc-link", onclick = .bdc_ev("bd_cfg_suggest", col),
                            "Fill in suggested names")),
          div(class = "bdc-hint",
              "Exports often store a number for each answer. Give each one a name; leave it blank to show the value as it is."),
          tags$table(class = "bdc-vals",
            tags$thead(tags$tr(tags$th("Value"), tags$th(class = "bdc-n", "Count"), tags$th("Shown as"))),
            tags$tbody(lapply(show, function(i) {
              code <- vals$code[i]
              tags$tr(tags$td(class = "bdc-code", code), tags$td(class = "bdc-n", vals$n[i]),
                      tags$td(inp("label", .s1(cur[[code]]), if (sug[i] != code) sug[i] else code, code = code)))
            }))),
          if (nrow(vals) > 40)
            div(class = "bdc-hint", sprintf("Showing the 40 most common of %d values.", nrow(vals))))
      }
      tagList(head, name_ui, body)
    })
  })

  output$bd_cfg_preview <- renderUI({
    cfg <- bdc_cfg_d(); col <- bdc$active; req(cfg, col)
    bd <- tryCatch(compute_breakdown(redcap_wp(), col, cfg, numeric_bins = "pretty"),
                   error = function(e) NULL)
    if (is.null(bd)) return(div(class = "bdc-empty", "This column has no values in the current data."))
    div(class = "bdc-preview", render_demographics_strip(list(bd)), render_breakdown_card(bd))
  })

  observeEvent(input$bd_cfg_pick, bdc$active <- input$bd_cfg_pick$col)

  observeEvent(input$bd_cfg_toggle, {
    col <- input$bd_cfg_toggle$col; s <- bdc$selected
    if (col %in% s) bdc$selected <- setdiff(s, col)
    else { bdc$selected <- c(s, col); bdc$active <- col }
  })

  observeEvent(input$bd_cfg_move, {
    m <- input$bd_cfg_move; s <- bdc$selected; i <- match(m$col, s)
    if (is.na(i)) return()
    j <- if (identical(m$dir, "up")) i - 1L else i + 1L
    if (j < 1 || j > length(s)) return()
    s[c(i, j)] <- s[c(j, i)]; bdc$selected <- s
  })

  observeEvent(input$bd_cfg_field, {
    f <- input$bd_cfg_field; col <- f$col
    if (is.null(col) || !nzchar(col)) return()
    v <- f$value
    if (identical(f$field, "title")) {
      t <- bdc$titles; t[[col]] <- if (nzchar(trimws(.s1(v)))) trimws(.s1(v)) else NULL; bdc$titles <- t
    } else if (identical(f$field, "label")) {
      l <- bdc$labels; m <- l[[col]] %||% list()
      m[[f$code]] <- if (nzchar(trimws(.s1(v)))) trimws(.s1(v)) else NULL
      l[[col]] <- m; bdc$labels <- l
    } else if (f$field %in% c("group_on", "cut", "unit", "group_title")) {
      cuts <- bdc$cuts
      cur  <- cuts[[col]]
      if (is.null(cur) || is.null(cur$on)) cur <- isolate(.bdc_cut_edit(col))
      if (identical(f$field, "group_on")) cur$on <- isTRUE(v)
      else cur[[if (identical(f$field, "group_title")) "title" else f$field]] <- .s1(v)
      cuts[[col]] <- cur; bdc$cuts <- cuts
      if (identical(f$field, "group_on")) bdc$nonce <- bdc$nonce + 1   # show / hide the fields
    }
  })

  observeEvent(input$bd_cfg_suggest, {
    col  <- input$bd_cfg_suggest$col
    vals <- .bdc_values(col)
    cfg0 <- rv$trial_config; cfg0$column_labels[[col]] <- NULL
    sug  <- as.character(.resolve_value_labels(vals$code, col, cfg0))
    l <- bdc$labels; m <- l[[col]] %||% list()
    for (i in seq_along(vals$code))
      if (sug[i] != vals$code[i] && !nzchar(.s1(m[[vals$code[i]]]))) m[[vals$code[i]]] <- sug[i]
    l[[col]] <- m; bdc$labels <- l; bdc$nonce <- bdc$nonce + 1
  })

  observeEvent(input$bd_cfg_save, {
    cfg <- rv$trial_config
    if (is.null(cfg)) { removeModal(); return() }
    labels <- lapply(bdc$labels, function(m) Filter(function(x) nzchar(.s1(x)), m))
    labels <- Filter(length, labels)
    titles <- Filter(function(x) nzchar(trimws(.s1(x))), bdc$titles)
    cuts   <- .bdc_cuts_for_cfg(bdc$cuts)
    sel    <- bdc$selected
    ok <- tryCatch({
      update_overrides(cfg, participant_breakdowns = as.list(sel), column_labels = labels,
                       breakdown_titles = titles, breakdown_cuts = cuts)
      TRUE
    }, error = function(e) {
      showNotification(paste("Couldn't save:", conditionMessage(e)), type = "error", duration = 8)
      FALSE
    })
    if (!ok) return()
    rv$trial_config$participant_breakdowns <- sel
    rv$trial_config$column_labels          <- labels
    rv$trial_config$breakdown_titles       <- titles
    rv$trial_config$breakdown_cuts         <- cuts
    selected_breakdowns(sel)
    removeModal()
    showNotification(sprintf("Saved: %d breakdown%s on the Data tab.", length(sel),
                             if (length(sel) == 1) "" else "s"), type = "message", duration = 3)
  })

  # ── Quick-action handlers ─────────────────────────────────────────────
  observeEvent(input$qa_open_returns, {
    # Reuse the existing sidebar nav. Set the URL hash so the trial selector
    # can pick it up; also click the hidden go_returns button if present.
    shinyjs::runjs("if(document.getElementById('go_returns')) document.getElementById('go_returns').click();")
  })
  observeEvent(input$qa_review_wd, { cos_filter(NULL); active_drill("wd") })

  # ── Downloads ──────────────────────────────────────────────────────────
  output$dl_participants <- xlsx_download(
    function() rv$participants,
    paste0(current_trial_config()$short_name %||% "trial", "_participants")
  )
  output$qa_export_full <- xlsx_download(
    function() rv$raw_redcap,
    paste0(current_trial_config()$short_name %||% "trial", "_full_export")
  )
}
