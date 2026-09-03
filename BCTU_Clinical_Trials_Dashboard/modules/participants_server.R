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
  # reports_server's recruitment logic). Falls back to total participants only
  # if the randomisation column can't be found.
  n_randomised <- function() {
    raw <- redcap_wp()
    if (is.null(raw) || nrow(raw) == 0 || !"record_id" %in% names(raw))
      return(0L)
    rc <- fld("randomisation_datetime", "rand_dttm_s")
    if (!rc %in% names(raw)) return(total_p())
    v <- trimws(as.character(raw[[rc]]))
    keep <- !is.na(v) & nzchar(v) & v != "NA"
    length(unique(raw$record_id[keep]))
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
        div(class = "donut", donut_svg(0, ring, "#CBD5E1", toupper(label))),
        div(class = "donut-info",
            div(class = "lbl", label),
            div(class = "vv", style = "font-size:15px;color:#64748B;", "Not yet due"),
            div(class = "sub", sub_extra))))
    }
    pct <- if (n_total > 0) 100 * n / n_total else 0
    div(class = "donut-card",
      div(class = "donut", donut_svg(pct, ring, fill, toupper(label))),
      div(class = "donut-info",
          div(class = "lbl", label),
          div(class = "vv", as.character(n),
              tags$span(style = "font-size:13px;color:#64748B;font-weight:500;",
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
      if (is.na(field) || !nzchar(field) || !field %in% names(raw))
        return(stats::setNames(rep(NA, length(ids)), ids))
      v <- .rec_values(raw, field)
      out <- stats::setNames(rep(NA_character_, length(ids)), ids)
      hit <- intersect(names(v), ids)
      out[hit] <- v[hit]
      suppressWarnings(stats::setNames(as.Date(substr(out, 1, 10)), ids))
    }
    dc_field <- mapping_first(rv$trial_config$redcap_fields$discharge_date, NA_character_)
    list(ids = ids, recruitment = pick(spec$date_field), discharge = pick(dc_field))
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
    base_n <- n_event("Baseline")
    cards <- list(donut_card_ui(base_n, den, "Baseline",
                  ring = "#EDE9FE", fill = "#7C3AED",
                  sub_extra = sprintf("%d missing", max(0L, den - base_n))))

    ev    <- (rv$trial_config$redcap_events) %||% list()
    roles <- setdiff(names(ev), c("baseline", "sub_forms"))
    if (!length(roles)) roles <- c("discharge", "day_30", "day_90")

    # Known completion-field roles (a list so a missing key returns NULL, not an
    # error) keep TONIC's completion donuts working; others use <role>_complete.
    known_fields <- list(discharge = "discharge_complete",
                         day_30 = "day30_complete", day_90 = "day90_complete")
    palette <- list(c("#DBEAFE","#2563EB"), c("#D1FAE5","#10B981"),
                    c("#A7F3D0","#059669"), c("#FEF3C7","#D97706"),
                    c("#FCE7F3","#DB2777"), c("#E0E7FF","#4F46E5"),
                    c("#CCFBF1","#0D9488"))

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
    df <- wd_df()
    n <- nrow(df)
    pct <- if (total_p() > 0) round(100 * n / total_p(), 1) else 0
    tile_body(HTML("&#8633; Withdrawals"), n,
              sprintf("%.1f%% of randomised", pct),
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
  observeEvent(input$safety_tile_wd,   set_drill("wd"))
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
        div(style = "padding:18px;text-align:center;color:#64748B;font-size:12px;font-style:italic;",
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
    em        <- '<span style="color:#94A3B8">&mdash;</span>'
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
        span(style = "color:#94A3B8;",
             "Source: REDCap export · mapped fields")
      )
    )
  })

  observeEvent(input$safety_drill_close, { active_drill(NULL) })

  # ── Withdrawal donut ──────────────────────────────────────────────────
  output$withdrawal_donut_ui <- renderUI({
    df <- wd_df()
    if (nrow(df) == 0) {
      return(div(style = "padding:18px;text-align:center;color:#64748B;font-style:italic;font-size:12px;",
                 "No withdrawals recorded."))
    }
    counts <- as.data.frame(table(code = df$severity), stringsAsFactors = FALSE)
    names(counts) <- c("code", "n")
    counts$label <- if (exists("cos_type_labels"))
      vapply(counts$code, function(c) {
        key <- as.character(c)
        # cos_type_labels is a named atomic vector — `[[` on a name that isn't
        # present throws "subscript out of bounds" (e.g. an NA/blank or unmapped
        # withdrawal code), so check membership before indexing.
        if (!is.na(key) && key %in% names(cos_type_labels)) cos_type_labels[[key]]
        else paste("Code:", c)
      }, character(1))
    else counts$code

    palette <- c("#DC2626", "#F59E0B", "#3B82F6", "#7C3AED", "#64748B",
                 "#0FA88E", "#EF4444")
    total <- sum(counts$n)
    circ  <- 314  # 2 * pi * 50
    offset <- 0
    arcs <- lapply(seq_len(nrow(counts)), function(i) {
      dash <- round(circ * counts$n[i] / total, 1)
      arc <- sprintf(
'<circle cx="60" cy="60" r="50" stroke="%s" stroke-width="16" fill="none"
         stroke-dasharray="%s %s" stroke-dashoffset="%s"/>',
        palette[((i - 1) %% length(palette)) + 1], dash, circ, -offset)
      offset <<- offset + dash
      arc
    })

    legend <- lapply(seq_len(nrow(counts)), function(i) {
      div(class = "wd-lr",
        div(class = "wd-l",
          div(class = "wd-dot",
              style = sprintf("background:%s",
                              palette[((i - 1) %% length(palette)) + 1])),
          span(counts$label[i])),
        span(class = "wd-n", counts$n[i])
      )
    })

    div(class = "wd-card",
      div(class = "wd-donut",
        HTML(sprintf(
'<svg viewBox="0 0 120 120" width="120" height="120" style="transform:rotate(-90deg)">
  <circle cx="60" cy="60" r="50" stroke="#FEE2E2" stroke-width="16" fill="none"/>
  %s
</svg>
<div class="wd-ctr"><b>%d</b><small>WITHDRAWN</small></div>',
          paste(unlist(arcs), collapse = "\n"), total))
      ),
      div(class = "wd-legend", legend)
    )
  })

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
    if (n == 0) "—" else sprintf("n = %d", n)
  })

  output$breakdowns_summary_txt <- renderText({
    sel <- selected_breakdowns()
    if (is.null(sel) || !length(sel)) return("None selected")
    paste(length(sel), if (length(sel) == 1) "breakdown" else "breakdowns")
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
    breakdowns <- lapply(sel, function(c) compute_breakdown(raw, c, cfg))
    render_breakdowns_grid(breakdowns)
  })

  # Configure-modal handlers (unchanged from previous implementation)
  observeEvent(input$configure_breakdowns, {
    det <- detected_breakdowns()
    sel <- selected_breakdowns() %||% character(0)
    raw <- rv$raw_redcap
    cfg <- rv$trial_config
    if (!nrow(det)) {
      showNotification("No usable columns detected — upload a CSV first.",
                       type = "warning", duration = 5)
      return()
    }
    editable <- find_editable_code_cols(raw, cfg, det)
    labels_section <- if (length(editable) > 0) {
      div(style = "margin-top:22px;border-top:1px solid #EEF3F8;padding-top:16px;",
          div(style = "font-weight:600;color:#0F172A;font-size:13px;margin-bottom:3px;",
              HTML("&#127991; Value labels &mdash; edit to rename")),
          div(style = "font-size:12px;color:#64748B;margin-bottom:14px;",
              "Give each coded value a readable name. These are the group names shown in the demographic cards — edit any field to rename a grouping. Existing names are pre-filled."),
          lapply(editable, function(ci) {
            div(class = "dg-col",
                div(class = "dg-col-head",
                    span(class = "dg-col-title", ci$label),
                    span(class = "dg-col-var", paste0("(", ci$col, ")")),
                    if (isTRUE(ci$labelled))
                      span(class = "mu-pill mu-pill-ok", style = "margin-left:auto;", "Labelled")),
                lapply(ci$values, function(v) {
                  input_id <- paste0("codelbl_", ci$col, "___", v)
                  div(class = "dg-row",
                      span(class = "dg-code", paste0(v, " =")),
                      textInput(input_id, label = NULL,
                                value = ci$suggested[[v]] %||% "",
                                placeholder = paste0("Name for code ", v),
                                width = "100%"))
                }))
          }))
    } else NULL
    showModal(modalDialog(
      title = div(style = "display:flex;align-items:center;gap:10px;",
                  span(style = "font-size:18px;color:#6366F1;", HTML("&#x2699;")),
                  span("Configure demographic breakdowns")),
      size = "l", easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_breakdowns", "Save",
                     class = "btn btn-primary",
                     style = "background:#6366F1;border-color:#6366F1;font-weight:600;")
      ),
      div(style = "font-size:12.5px;color:#64748B;margin-bottom:14px;line-height:1.6;",
          HTML(sprintf("Detected <strong>%d</strong> columns suitable for breakdowns from the latest CSV. Tick the ones you want to display.",
                       nrow(det)))),
      checkboxGroupInput(
        "breakdowns_choice", label = NULL,
        choiceNames = lapply(seq_len(nrow(det)), function(i) {
          r <- det[i, ]
          tagList(
            span(style = "font-weight:600;color:#0F172A;", r$label),
            span(style = "font-size:10.5px;color:#94A3B8;margin-left:6px;text-transform:uppercase;letter-spacing:.4px;",
                 r$type),
            span(style = "font-size:11px;color:#64748B;margin-left:6px;",
                 sprintf("· %s · %d unique%s", r$column, r$n_unique,
                         if (r$n_missing > 0) sprintf(" · %d missing", r$n_missing) else ""))
          )
        }),
        choiceValues = det$column,
        selected = intersect(sel, det$column)),
      labels_section
    ))
  })

  observeEvent(input$save_breakdowns, {
    cfg <- rv$trial_config
    if (is.null(cfg)) { removeModal(); return() }
    chosen <- input$breakdowns_choice %||% character(0)
    selected_breakdowns(chosen)
    all_inputs  <- reactiveValuesToList(input)
    label_keys  <- grep("^codelbl_", names(all_inputs), value = TRUE)
    col_labels  <- cfg$column_labels %||% list()
    for (key in label_keys) {
      val_text <- trimws(all_inputs[[key]] %||% "")
      parts <- strsplit(sub("^codelbl_", "", key), "___", fixed = TRUE)[[1]]
      if (length(parts) != 2) next
      col  <- parts[1]; code <- parts[2]
      if (!nzchar(val_text)) next
      if (is.null(col_labels[[col]])) col_labels[[col]] <- list()
      col_labels[[col]][[code]] <- val_text
    }
    tryCatch(
      update_overrides(cfg,
        participant_breakdowns = as.list(chosen),
        column_labels          = col_labels),
      error = function(e) message("breakdown save: ", e$message)
    )
    rv$trial_config$participant_breakdowns <- chosen
    rv$trial_config$column_labels          <- col_labels
    removeModal()
    showNotification(sprintf("Saved %d breakdown%s.",
                             length(chosen),
                             if (length(chosen) == 1) "" else "s"),
                     type = "message", duration = 3)
  })

  # ── Quick-action handlers ─────────────────────────────────────────────
  observeEvent(input$qa_open_returns, {
    # Reuse the existing sidebar nav. Set the URL hash so the trial selector
    # can pick it up; also click the hidden go_returns button if present.
    shinyjs::runjs("if(document.getElementById('go_returns')) document.getElementById('go_returns').click();")
  })
  observeEvent(input$qa_review_wd, { active_drill("wd") })

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
