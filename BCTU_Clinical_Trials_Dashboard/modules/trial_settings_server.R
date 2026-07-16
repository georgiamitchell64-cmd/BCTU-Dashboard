trial_settings_server <- function(input, output, session, state) {
  rv <- state$rv
  
  # ── Settings nav: trial mark (colour initial) ───────────────────────────
  output$settings_trial_mark <- renderUI({
    cfg <- rv$trial_config
    if (is.null(cfg)) return(NULL)
    col <- cfg$colors$primary %||% "#1B4F6B"
    initial <- substr(toupper(cfg$short_name %||% cfg$code %||% "T"), 1, 1)
    div(class = "settings-mark",
        style = sprintf("background:%s;color:#fff;width:32px;height:32px;
                         border-radius:8px;display:flex;align-items:center;
                         justify-content:center;font-weight:700;font-size:14px;
                         flex-shrink:0;", col),
        initial)
  })
  
  # ── Settings nav: trial name ────────────────────────────────────────────
  output$settings_trial_name <- renderUI({
    cfg <- rv$trial_config
    if (is.null(cfg)) return(span("No trial selected"))
    div(style = "font-size:13px;font-weight:600;color:var(--ov-ink);
                 white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
        cfg$short_name %||% toupper(cfg$code %||% ""))
  })
  
  # ── Breadcrumb for active section ───────────────────────────────────────
  output$settings_breadcrumb <- renderText({
    sec <- input$settings_active_section %||% "appearance"
    labels <- c(
      appearance       = "Appearance",
      identity         = "Trial identity & data paths",
      features         = "Features",
      schedule         = "Follow-up schedule",
      demographics     = "Demographics",
      detail           = "Detail fields",
      config           = "Config & overrides",
      report_content   = "Report content",
      portfolio_review = "Portfolio review",
      report_templates = "Report templates",
      danger           = "Danger zone"
    )
    # NB: single-bracket lookup so an unknown section returns NA (which
    # %||% can fall through), instead of [[ which throws "subscript out of
    # bounds" for missing names.
    out <- unname(labels[sec])
    if (is.na(out) || !nzchar(out)) sec else out
  })
  
  # ── Theme picker ──────────────────────────────────────────────────────────
  selected_theme <- reactiveVal("custom")
  
  output$theme_picker_ui <- renderUI({
    active <- selected_theme()
    cards <- lapply(names(TRIAL_THEMES), function(key) {
      th <- TRIAL_THEMES[[key]]
      is_active <- identical(active, key)
      border_col <- if (is_active) th$secondary else "#EEF2F7"
      shadow <- if (is_active)
        sprintf("box-shadow: 0 0 0 2px %s;", th$secondary) else ""
      
      div(
        onclick = sprintf("Shiny.setInputValue('pick_theme', '%s', {priority:'event'})", key),
        style = sprintf("border:1px solid %s; %s
                         border-radius:12px; padding:12px; cursor:pointer;
                         background:#FFFFFF; transition: all .15s;",
                        border_col, shadow),
        
        # Swatch row
        div(style = "display:flex;gap:4px;margin-bottom:10px;",
            div(style = sprintf("flex:2;height:24px;border-radius:5px;background:%s;", th$primary)),
            div(style = sprintf("flex:1;height:24px;border-radius:5px;background:%s;", th$secondary)),
            div(style = sprintf("flex:1;height:24px;border-radius:5px;background:%s;", th$accent))),
        
        div(style = "display:flex;justify-content:space-between;align-items:center;",
            div(style = "font-size:13px;font-weight:600;color:#0F172A;", th$label),
            if (is_active)
              span(style = sprintf("font-size:10px;color:%s;font-weight:700;
                                    text-transform:uppercase;letter-spacing:.5px;", th$secondary),
                   HTML("&#10003; Active"))
        ),
        div(style = "font-size:11px;color:#64748B;margin-top:2px;", th$sublabel),
        div(style = "font-size:10px;color:#94A3B8;margin-top:4px;
                     text-transform:uppercase;letter-spacing:.5px;",
            sprintf("Sidebar: %s", th$sidebar))
      )
    })
    
    custom_card <- div(
      onclick = "Shiny.setInputValue('pick_theme', 'custom', {priority:'event'})",
      style = sprintf("border:1px dashed %s;border-radius:12px;padding:12px;cursor:pointer;
                       background:#FAFBFD;transition:all .15s;%s",
                      if (identical(active, "custom")) "#6366F1" else "#CBD5E1",
                      if (identical(active, "custom")) "box-shadow:0 0 0 2px #6366F1;" else ""),
      div(style = "display:flex;gap:4px;margin-bottom:10px;",
          div(style = "flex:2;height:24px;border-radius:5px;
                       background:repeating-linear-gradient(45deg,#E2E8F0 0 4px,#F1F5F9 4px 8px);"),
          div(style = "flex:1;height:24px;border-radius:5px;
                       background:repeating-linear-gradient(45deg,#E2E8F0 0 4px,#F1F5F9 4px 8px);"),
          div(style = "flex:1;height:24px;border-radius:5px;
                       background:repeating-linear-gradient(45deg,#E2E8F0 0 4px,#F1F5F9 4px 8px);")),
      div(style = "font-size:13px;font-weight:600;color:#0F172A;", "Custom"),
      div(style = "font-size:11px;color:#64748B;margin-top:2px;",
          "Set your own colours")
    )
    
    div(style = "display:grid;grid-template-columns:repeat(auto-fill,minmax(180px,1fr));gap:10px;",
        cards, custom_card)
  })
  
  observeEvent(input$pick_theme, {
    key <- input$pick_theme
    selected_theme(key)
    if (key == "custom") {
      shinyjs::show("custom_colors_panel")
    } else {
      shinyjs::hide("custom_colors_panel")
      th <- TRIAL_THEMES[[key]]
      if (!is.null(th)) {
        updateTextInput(session, "set_col_primary",   value = th$primary)
        updateTextInput(session, "set_col_secondary", value = th$secondary)
        updateTextInput(session, "set_col_accent",    value = th$accent)
      }
    }
  })
  
  # ── Live preview ──────────────────────────────────────────────────────────
  output$color_preview_bar <- renderUI({
    p <- input$set_col_primary   %||% "#1B4F6B"
    s <- input$set_col_secondary %||% "#2EC4A5"
    a <- input$set_col_accent    %||% "#F59E0B"
    
    runjs(sprintf("$('#preview_primary').css('background','%s')", p))
    runjs(sprintf("$('#preview_secondary').css('background','%s')", s))
    runjs(sprintf("$('#preview_accent').css('background','%s')", a))
    
    div(style = "display:flex;gap:2px;border-radius:6px;overflow:hidden;height:40px;",
        div(style = sprintf("flex:3;background:%s;display:flex;align-items:center;justify-content:center;
                             color:#fff;font-size:11px;font-weight:600;", p), "Sidebar"),
        div(style = "flex:5;background:#EEF3F8;display:flex;align-items:center;padding:0 12px;",
            div(style = sprintf("background:%s;color:#fff;padding:3px 10px;border-radius:4px;
                                 font-size:10px;font-weight:600;margin-right:8px;", s), "Chart"),
            div(style = sprintf("background:%s;color:#fff;padding:3px 10px;border-radius:4px;
                                 font-size:10px;font-weight:600;", a), "Accent")
        )
    )
  })
  
  # ── Follow-up schedule editor ───────────────────────────────────────────
  # Each timepoint is a row {uid, label, event}. Stable per-row uids let the
  # render preserve typed values across add/remove without index shuffling.
  tp_seq  <- reactiveVal(0L)
  tp_rows <- reactiveVal(list())
  .tp_next_uid <- function() { n <- tp_seq() + 1L; tp_seq(n); n }
  
  .tp_init_from_cfg <- function(cfg) {
    ev    <- cfg$redcap_events %||% list()
    roles <- setdiff(names(ev), c("baseline", "sub_forms"))
    tp_rows(lapply(roles, function(r) {
      val <- ev[[r]]
      list(uid   = .tp_next_uid(),
           label = tools::toTitleCase(gsub("_", " ", r)),
           event = if (is.null(val)) "" else as.character(val)[1])
    }))
  }
  
  # Read current input values back into tp_rows before any structural change.
  .tp_sync <- function() {
    lapply(tp_rows(), function(r) {
      lbl <- input[[paste0("set_tp_label_", r$uid)]]
      evt <- input[[paste0("set_tp_event_", r$uid)]]
      list(uid = r$uid,
           label = if (is.null(lbl)) r$label else lbl,
           event = if (is.null(evt)) r$event else evt)
    })
  }
  
  output$settings_timepoints_ui <- renderUI({
    rows <- tp_rows()
    if (!length(rows))
      return(div(class = "sch-empty",
                 "No follow-up timepoints yet — add one below (e.g. Week 6, Month 6, Month 12)."))
    lapply(rows, function(r) {
      div(class = "sch-row",
          textInput(paste0("set_tp_label_", r$uid), label = NULL,
                    value = r$label, placeholder = "e.g. Month 6", width = "100%"),
          textInput(paste0("set_tp_event_", r$uid), label = NULL,
                    value = r$event, placeholder = "e.g. month_6_arm_1", width = "100%"),
          tags$button(class = "sch-remove", type = "button", title = "Remove timepoint",
                      onclick = sprintf("Shiny.setInputValue('settings_tp_remove', %d, {priority:'event'})", r$uid),
                      HTML("&times;")))
    })
  })
  outputOptions(output, "settings_timepoints_ui", suspendWhenHidden = FALSE)
  
  observeEvent(input$settings_tp_add, {
    rows <- .tp_sync()
    rows[[length(rows) + 1]] <- list(uid = .tp_next_uid(), label = "", event = "")
    tp_rows(rows)
  })
  observeEvent(input$settings_tp_remove, {
    rid <- suppressWarnings(as.integer(input$settings_tp_remove))
    tp_rows(Filter(function(r) !identical(r$uid, rid), .tp_sync()))
  })
  
  observeEvent(input$settings_save_schedule, {
    cfg <- rv$trial_config; req(cfg)
    .slug <- function(s) { s <- tolower(trimws(s)); s <- gsub("[^a-z0-9]+", "_", s); gsub("^_+|_+$", "", s) }
    
    base_ev <- trimws(input$set_ev_baseline %||% "")
    events  <- list(baseline = if (nzchar(base_ev)) base_ev
                    else (cfg$redcap_events$baseline %||% "baseline_arm_1"))
    used <- "baseline"
    for (r in .tp_sync()) {
      lbl <- trimws(r$label); if (!nzchar(lbl)) next
      key <- .slug(lbl); if (!nzchar(key) || key %in% used) next
      used <- c(used, key)
      events[[key]] <- trimws(r$event %||% "")   # "" keeps the timepoint with no mapping yet
    }
    sf_raw <- trimws(input$set_ev_subforms %||% "")
    if (nzchar(sf_raw)) {
      parts <- trimws(strsplit(sf_raw, ",")[[1]]); parts <- parts[nzchar(parts)]
      if (length(parts)) events$sub_forms <- as.list(parts)
    }
    
    ok <- tryCatch({ update_overrides(cfg, redcap_events = events); TRUE },
                   error = function(e) {
                     showNotification(paste("Save failed:", e$message), type = "error", duration = 8)
                     FALSE })
    if (!ok) return()
    new_cfg <- cfg; new_cfg$redcap_events <- events
    rv$trial_config <- new_cfg
    apply_trial_globals(new_cfg)
    rv$settings_changed <- Sys.time()
    n_tp <- length(used) - 1L
    showNotification(sprintf("Saved follow-up schedule — %d timepoint%s. Reload your CSV to re-classify events.",
                             n_tp, if (n_tp == 1) "" else "s"),
                     type = "message", duration = 5)
  })
  
  # ── Demographic groupings editor (mirrors the Data-tab Configure modal) ──
  # Uses set_bd_choice / setlbl_* input IDs so it never collides with the
  # Data-tab modal (breakdowns_choice / codelbl_*); both write the same overrides.
  output$settings_demographics_ui <- renderUI({
    rv$settings_changed                      # refresh after a save
    cfg <- rv$trial_config
    if (is.null(cfg)) return(div(class = "sch-empty", "No trial selected."))
    raw <- rv$raw_redcap
    
    if (is.null(raw) || !nrow(raw)) {
      cl <- cfg$column_labels %||% list()
      if (!length(cl))
        return(div(class = "sch-empty",
                   "Load a REDCap CSV on the Data tab to detect demographic columns. Group names you've already saved will appear here to rename."))
      cards <- lapply(names(cl), function(col) {
        vals <- cl[[col]]
        div(class = "dg-col",
            div(class = "dg-col-head",
                span(class = "dg-col-title", col),
                span(class = "mu-pill mu-pill-ok", style = "margin-left:auto;", "Labelled")),
            lapply(names(vals), function(code) {
              div(class = "dg-row",
                  span(class = "dg-code", paste0(code, " =")),
                  textInput(paste0("setlbl_", col, "___", code), label = NULL,
                            value = as.character(vals[[code]]), width = "100%"))
            }))
      })
      return(tagList(
        div(class = "sch-hint", style = "margin-bottom:12px;",
            "Showing saved group names — load a CSV on the Data tab to add new breakdowns."),
        cards))
    }
    
    det <- tryCatch(detect_breakdown_columns(raw, cfg), error = function(e) data.frame())
    if (!nrow(det))
      return(div(class = "sch-empty", "No demographic columns detected in the current export."))
    
    sel <- as.character(unlist(cfg$participant_breakdowns %||% default_breakdown_cols(det)))
    choice_names <- lapply(seq_len(nrow(det)), function(i) {
      r <- det[i, ]
      tagList(span(style = "font-weight:600;color:var(--ov-ink);", r$label),
              span(style = "font-size:11px;color:var(--ov-muted);margin-left:6px;",
                   sprintf("· %s · %d unique", r$column, r$n_unique)))
    })
    
    editable <- find_editable_code_cols(raw, cfg, det)
    label_cards <- if (length(editable)) lapply(editable, function(ci) {
      div(class = "dg-col",
          div(class = "dg-col-head",
              span(class = "dg-col-title", ci$label),
              span(class = "dg-col-var", paste0("(", ci$col, ")")),
              if (isTRUE(ci$labelled))
                span(class = "mu-pill mu-pill-ok", style = "margin-left:auto;", "Labelled")),
          lapply(ci$values, function(v) {
            div(class = "dg-row",
                span(class = "dg-code", paste0(v, " =")),
                textInput(paste0("setlbl_", ci$col, "___", v), label = NULL,
                          value = ci$suggested[[v]] %||% "",
                          placeholder = paste0("Name for code ", v), width = "100%"))
          }))
    }) else list(div(class = "sch-hint", "No coded columns need labels."))
    
    tagList(
      div(class = "dg-section-label", "Show these breakdowns"),
      checkboxGroupInput("set_bd_choice", label = NULL,
                         choiceNames = choice_names, choiceValues = det$column,
                         selected = intersect(sel, det$column)),
      div(class = "sch-subforms",
          div(class = "dg-section-label", "Group names"),
          div(class = "sch-hint", style = "margin-bottom:10px;",
              "Rename the groups shown for each coded value. Existing names are pre-filled."),
          label_cards)
    )
  })
  outputOptions(output, "settings_demographics_ui", suspendWhenHidden = FALSE)
  
  observeEvent(input$settings_save_demographics, {
    cfg <- rv$trial_config; req(cfg)
    all_inputs <- reactiveValuesToList(input)
    col_labels <- cfg$column_labels %||% list()
    for (key in grep("^setlbl_", names(all_inputs), value = TRUE)) {
      val_text <- trimws(all_inputs[[key]] %||% "")
      parts <- strsplit(sub("^setlbl_", "", key), "___", fixed = TRUE)[[1]]
      if (length(parts) != 2 || !nzchar(val_text)) next
      col <- parts[1]; code <- parts[2]
      if (is.null(col_labels[[col]])) col_labels[[col]] <- list()
      col_labels[[col]][[code]] <- val_text
    }
    args <- list(cfg = cfg, column_labels = col_labels)
    if ("set_bd_choice" %in% names(all_inputs))
      args$participant_breakdowns <- as.list(input$set_bd_choice %||% character(0))
    ok <- tryCatch({ do.call(update_overrides, args); TRUE },
                   error = function(e) {
                     showNotification(paste("Save failed:", e$message), type = "error", duration = 8)
                     FALSE })
    if (!ok) return()
    new_cfg <- cfg
    new_cfg$column_labels <- col_labels
    if ("set_bd_choice" %in% names(all_inputs))
      new_cfg$participant_breakdowns <- as.character(input$set_bd_choice %||% character(0))
    rv$trial_config <- new_cfg
    apply_trial_globals(new_cfg)
    rv$settings_changed <- Sys.time()
    showNotification("Saved demographic settings.", type = "message", duration = 4)
  })
  
  # ── Detail-fields editor (extra import columns for SAE / withdrawal /
  #    complications, with custom headings) ─────────────────────────────────
  DET_SECTIONS <- list(
    list(key = "sae",          title = "Serious adverse events",
         hint = "e.g. death, causality, expectedness, reason"),
    list(key = "withdrawal",   title = "Withdrawals / change of status",
         hint = "e.g. cos_type, cos, reason"),
    list(key = "complication", title = "Complications",
         hint = "e.g. complication type, Clavien-Dindo grade")
  )
  det_seq  <- reactiveVal(0L)
  det_rows <- reactiveVal(list())   # flat: list(uid, section, col, header)
  .det_next_uid <- function() { n <- det_seq() + 1L; det_seq(n); n }
  
  .det_init_from_cfg <- function(cfg) {
    rows <- list()
    dfl  <- cfg$detail_fields %||% list()
    for (s in DET_SECTIONS) {
      for (f in (dfl[[s$key]] %||% list())) {
        col <- f$col %||% f[["col"]]
        if (is.null(col) || !nzchar(col)) next
        rows[[length(rows) + 1]] <- list(uid = .det_next_uid(), section = s$key,
                                         col = col, header = f$header %||% f[["header"]] %||% col)
      }
    }
    det_rows(rows)
  }
  
  .det_sync <- function() {
    lapply(det_rows(), function(r) {
      cv <- input[[paste0("det_col_", r$uid)]]
      hv <- input[[paste0("det_hdr_", r$uid)]]
      list(uid = r$uid, section = r$section,
           col = if (is.null(cv)) r$col else cv,
           header = if (is.null(hv)) r$header else hv)
    })
  }
  
  output$settings_detail_ui <- renderUI({
    rows <- det_rows()
    cols <- names(rv$raw_redcap %||% list())
    grp <- function(s) {
      srows <- Filter(function(r) identical(r$section, s$key), rows)
      row_ui <- if (!length(srows))
        div(class = "sch-empty", "No fields mapped yet.")
      else lapply(srows, function(r) {
        div(class = "sch-row",
            selectizeInput(paste0("det_col_", r$uid), label = NULL,
                           choices = unique(c(r$col, cols)), selected = r$col,
                           width = "100%",
                           options = list(create = TRUE, placeholder = "REDCap column…")),
            textInput(paste0("det_hdr_", r$uid), label = NULL, value = r$header,
                      placeholder = "Heading shown", width = "100%"),
            tags$button(class = "sch-remove", type = "button", title = "Remove",
                        onclick = sprintf("Shiny.setInputValue('settings_det_remove', %d, {priority:'event'})", r$uid),
                        HTML("&times;")))
      })
      tagList(
        div(class = "dg-col-head", style = "margin-top:14px;",
            span(class = "dg-col-title", s$title)),
        div(class = "sch-hint", s$hint),
        div(class = "sch-colhead",
            span(class = "sch-colhead-l", "Import column"),
            span(class = "sch-colhead-r", "Heading shown"),
            span(style = "width:34px;")),
        row_ui,
        div(style = "margin:8px 0 4px;",
            actionButton(paste0("settings_det_add_", s$key),
                         HTML("&#43; Add field"), class = "btn-ghost-sm")))
    }
    head_note <- if (!length(cols))
      div(class = "sch-hint", style = "margin-bottom:6px;",
          "Tip: load a REDCap CSV on the Data tab to pick columns from a list — you can also type column names by hand.") else NULL
    tagList(head_note, lapply(DET_SECTIONS, grp))
  })
  outputOptions(output, "settings_detail_ui", suspendWhenHidden = FALSE)
  
  # One add-handler per section.
  lapply(DET_SECTIONS, function(s) {
    local({
      sk <- s$key
      observeEvent(input[[paste0("settings_det_add_", sk)]], {
        rows <- .det_sync()
        rows[[length(rows) + 1]] <- list(uid = .det_next_uid(), section = sk, col = "", header = "")
        det_rows(rows)
      })
    })
  })
  
  observeEvent(input$settings_det_remove, {
    rid <- suppressWarnings(as.integer(input$settings_det_remove))
    det_rows(Filter(function(r) !identical(r$uid, rid), .det_sync()))
  })
  
  observeEvent(input$settings_save_detail, {
    cfg <- rv$trial_config; req(cfg)
    rows <- .det_sync()
    out  <- list()
    for (s in DET_SECTIONS) {
      sr <- Filter(function(r) identical(r$section, s$key) && nzchar(trimws(r$col %||% "")), rows)
      out[[s$key]] <- lapply(sr, function(r) {
        col <- trimws(r$col); hdr <- trimws(r$header %||% "")
        list(col = col, header = if (nzchar(hdr)) hdr else col)
      })
    }
    ok <- tryCatch({ update_overrides(cfg, detail_fields = out); TRUE },
                   error = function(e) {
                     showNotification(paste("Save failed:", e$message), type = "error", duration = 8)
                     FALSE })
    if (!ok) return()
    new_cfg <- cfg; new_cfg$detail_fields <- out
    rv$trial_config <- new_cfg
    apply_trial_globals(new_cfg)
    rv$settings_changed <- Sys.time()
    n <- sum(vapply(out, length, integer(1)))
    showNotification(sprintf("Saved %d detail field%s.", n, if (n == 1) "" else "s"),
                     type = "message", duration = 4)
  })
  
  # ── Populate fields when trial is loaded ──────────────────────────────────
  observeEvent(rv$trial_config, {
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    
    cols <- cfg$colors %||% list(primary = "#1B4F6B", secondary = "#2EC4A5", accent = "#F59E0B")
    updateTextInput(session, "set_col_primary",   value = cols$primary)
    updateTextInput(session, "set_col_secondary", value = cols$secondary)
    updateTextInput(session, "set_col_accent",    value = cols$accent)
    
    # Theme: if cfg has one, use it; otherwise fall back to "custom".
    saved_theme <- cfg$theme %||% "custom"
    if (!saved_theme %in% c(names(TRIAL_THEMES), "custom")) saved_theme <- "custom"
    selected_theme(saved_theme)
    if (saved_theme == "custom") shinyjs::show("custom_colors_panel")
    else                          shinyjs::hide("custom_colors_panel")
    
    feat <- cfg$features %||% list()
    updateCheckboxInput(session, "set_feat_projections",    value = isTRUE(feat$projections))
    updateCheckboxInput(session, "set_feat_pilot",          value = isTRUE(feat$pilot_criteria))
    updateCheckboxInput(session, "set_feat_postal",         value = isTRUE(feat$postal_tracking))
    updateCheckboxInput(session, "set_feat_returns",        value = isTRUE(feat$return_rates))
    updateCheckboxInput(session, "set_feat_consort",        value = isTRUE(feat$consort_flow))
    updateCheckboxInput(session, "set_feat_baseline",       value = isTRUE(feat$baseline_table))
    # Default questionnaires flag to TRUE for legacy configs that pre-date this toggle
    updateCheckboxInput(session, "set_feat_questionnaires",
                        value = isTRUE(feat$participant_questionnaires %||% TRUE))
    
    # Data paths
    updateTextInput(session, "set_data_dir",   value = cfg$data_dir %||% "")
    updateTextInput(session, "set_rr_dir",     value = cfg$return_rates_dir %||% "")
    updateTextInput(session, "set_logo_path",  value = cfg$logo_file %||% "")
    
    updateTextInput(session,    "set_short_name", value = cfg$short_name %||% "")
    updateTextInput(session,    "set_full_name",  value = cfg$name %||% "")
    updateNumericInput(session, "set_target",     value = cfg$trial_target %||% 100)
    updateSelectInput(session,  "set_category",   selected = trial_category(cfg))
    rd <- cfg$report_defaults %||% list()
    updateTextInput(session, "set_ci",      value = rd$ci %||% "")
    updateTextInput(session, "set_sponsor", value = rd$sponsor %||% "")
    
    # Follow-up schedule
    ev <- cfg$redcap_events %||% list()
    updateTextInput(session, "set_ev_baseline", value = ev$baseline %||% "")
    sf <- ev$sub_forms
    updateTextInput(session, "set_ev_subforms",
                    value = if (is.null(sf)) "" else paste(unlist(sf), collapse = ", "))
    .tp_init_from_cfg(cfg)
    .det_init_from_cfg(cfg)
  })
  
  # ── Config file path + override status ───────────────────────────────────
  output$settings_config_path <- renderUI({
    cfg <- rv$trial_config
    if (is.null(cfg)) return(span("No trial selected."))
    div(HTML(paste0("Config file: <code>",
                    file.path(cfg$trial_dir, "config.R"), "</code>")))
  })
  
  output$settings_overrides_status <- renderUI({
    cfg <- rv$trial_config
    if (is.null(cfg)) return(NULL)
    rv$settings_changed   # invalidate when settings change
    path <- overrides_path(cfg)
    if (file.exists(path)) {
      div(style = "color:#0F172A;",
          HTML(paste0("Overrides: <code>", path, "</code> ",
                      "<span style='color:#6366F1;font-weight:600;'>active</span>")))
    } else {
      div(HTML("Overrides: <em>none — using config.R as-is</em>"))
    }
  })
  
  # ── Apply features live (called after save / reset) ──────────────────────
  apply_features_live <- function(feat) {
    if (isTRUE(feat$postal_tracking)) shinyjs::show("go_postal_wrap")
    else                              shinyjs::hide("go_postal_wrap")
    if (isTRUE(feat$return_rates))    shinyjs::show("go_returns_wrap")
    else                              shinyjs::hide("go_returns_wrap")
    # Patient questionnaires section on the Data tab (default ON for legacy).
    # Two separate divs because the demographic breakdowns card sits between them
    # and should NOT be hidden by the PROMs flag.
    if (isTRUE(feat$participant_questionnaires %||% TRUE)) {
      shinyjs::show("participant_questionnaire_section")
      shinyjs::show("participant_questionnaire_grid")
    } else {
      shinyjs::hide("participant_questionnaire_section")
      shinyjs::hide("participant_questionnaire_grid")
    }
  }
  
  # ── Save theme + colours ─────────────────────────────────────────────────
  observeEvent(input$settings_save_colors, {
    if (!require_role(rv, "manager")) return()
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    
    theme_key <- selected_theme()
    if (theme_key %in% names(TRIAL_THEMES)) {
      th <- TRIAL_THEMES[[theme_key]]
      new_colors <- list(primary = th$primary, secondary = th$secondary, accent = th$accent)
      sidebar_variant <- th$sidebar
    } else {
      # Fall back to the default when the user types an invalid hex code —
      # a bad value here would otherwise be injected straight into CSS.
      valid_hex <- function(x, default) {
        x <- trimws(x %||% "")
        if (grepl("^#[0-9A-Fa-f]{6}$", x) || grepl("^#[0-9A-Fa-f]{3}$", x)) x
        else default
      }
      new_colors <- list(
        primary   = valid_hex(input$set_col_primary,   "#1B4F6B"),
        secondary = valid_hex(input$set_col_secondary, "#2EC4A5"),
        accent    = valid_hex(input$set_col_accent,    "#F59E0B")
      )
      sidebar_variant <- "dark"
    }
    
    update_overrides(cfg, theme = theme_key, colors = new_colors)
    rv$trial_config$theme  <- theme_key
    rv$trial_config$colors <- new_colors
    apply_trial_colours(new_colors, sidebar = sidebar_variant)
    rv$settings_changed <- Sys.time()
    
    msg <- if (theme_key == "custom") "Custom colours applied."
    else sprintf("%s theme applied.", TRIAL_THEMES[[theme_key]]$label)
    showNotification(HTML(paste0("&#x2714; ", msg)), type = "message", duration = 4)
  })
  
  # ── Save identity + features ─────────────────────────────────────────────
  observeEvent(input$settings_save_features, {
    if (!require_role(rv, "manager")) return()
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    
    new_target   <- as.integer(input$set_target %||% cfg$trial_target %||% 100L)
    new_short    <- input$set_short_name %||% cfg$short_name %||% ""
    new_full     <- input$set_full_name  %||% cfg$name       %||% ""
    new_category <- input$set_category   %||% trial_category(cfg)
    
    new_features <- list(
      postal_tracking            = isTRUE(input$set_feat_postal),
      return_rates               = isTRUE(input$set_feat_returns),
      projections                = isTRUE(input$set_feat_projections),
      pilot_criteria             = isTRUE(input$set_feat_pilot),
      consort_flow               = isTRUE(input$set_feat_consort),
      baseline_table             = isTRUE(input$set_feat_baseline),
      participant_questionnaires = isTRUE(input$set_feat_questionnaires)
    )
    
    new_report_defaults <- list(
      ci      = input$set_ci      %||% (cfg$report_defaults$ci      %||% ""),
      sponsor = input$set_sponsor %||% (cfg$report_defaults$sponsor %||% "")
    )
    
    update_overrides(cfg,
                     short_name       = new_short,
                     name             = new_full,
                     trial_target     = new_target,
                     category         = new_category,
                     features         = new_features,
                     report_defaults  = new_report_defaults
    )
    
    # Update in-memory config so the rest of the app sees changes immediately.
    rv$trial_config$short_name       <- new_short
    rv$trial_config$name             <- new_full
    rv$trial_config$trial_target     <- new_target
    rv$trial_config$category         <- new_category
    rv$trial_config$features         <- new_features
    rv$trial_config$report_defaults  <- new_report_defaults
    
    # Re-apply globals so TRIAL_TARGET etc. refresh.
    apply_trial_globals(rv$trial_config)
    apply_features_live(new_features)
    
    # Update topbar title in case short_name changed.
    runjs(sprintf("$('.topbar-title').text('%s')",
                  gsub("'", "\\\\'", new_short)))
    runjs(sprintf("document.title = '%s Dashboard'",
                  gsub("'", "\\\\'", new_short)))
    
    rv$settings_changed <- Sys.time()
    rv$home_membership_changed <- Sys.time()  # refresh home cards too
    
    log_activity("settings_saved",
                 sprintf("Updated trial settings for <strong>%s</strong>",
                         htmltools::htmlEscape(new_short)),
                 username = rv$username, trial_code = cfg$code)
    showNotification(HTML("&#x2714; Settings saved."), type = "message", duration = 4)
  })
  
  # ── Save data paths ──────────────────────────────────────────────────────
  observeEvent(input$settings_save_paths, {
    if (!require_role(rv, "manager")) return()
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    
    new_data_dir <- trimws(input$set_data_dir %||% "")
    new_rr_dir   <- trimws(input$set_rr_dir %||% "")
    new_logo     <- trimws(input$set_logo_path %||% "")
    
    # Normalise slashes
    new_data_dir <- gsub("\\\\", "/", new_data_dir)
    new_rr_dir   <- gsub("\\\\", "/", new_rr_dir)
    new_logo     <- gsub("\\\\", "/", new_logo)
    
    update_overrides(cfg,
                     data_dir         = if (nzchar(new_data_dir)) new_data_dir else NULL,
                     return_rates_dir = if (nzchar(new_rr_dir))   new_rr_dir   else NULL,
                     logo_file        = if (nzchar(new_logo))      new_logo     else NULL
    )
    
    rv$trial_config$data_dir         <- if (nzchar(new_data_dir)) new_data_dir else NULL
    rv$trial_config$return_rates_dir <- if (nzchar(new_rr_dir))   new_rr_dir   else NULL
    rv$trial_config$logo_file        <- if (nzchar(new_logo))      new_logo     else NULL
    
    # Copy logo to www/trial_logos/ if valid
    if (nzchar(new_logo) && file.exists(new_logo)) {
      logos_dir <- file.path(getwd(), "www", "trial_logos")
      dir.create(logos_dir, recursive = TRUE, showWarnings = FALSE)
      ext <- tolower(tools::file_ext(new_logo))
      if (!ext %in% c("png", "jpg", "jpeg", "svg")) ext <- "png"
      dest <- file.path(logos_dir, paste0(cfg$code, ".", ext))
      tryCatch(file.copy(new_logo, dest, overwrite = TRUE),
               error = function(e) message("Logo copy: ", e$message))
    }
    
    rv$settings_changed <- Sys.time()
    log_activity("settings_paths_saved",
                 sprintf("Updated data paths for <strong>%s</strong>",
                         htmltools::htmlEscape(cfg$short_name %||% toupper(cfg$code))),
                 username = rv$username, trial_code = cfg$code)
    showNotification(HTML("&#x2714; Data paths saved."), type = "message", duration = 4)
  })
  
  # ── Reset to defaults (delete overrides.json) ────────────────────────────
  observeEvent(input$settings_reset_overrides, {
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    
    showModal(modalDialog(
      title = div(style = "color:#DC2626;",
                  HTML("&#x26A0; Reset to defaults?")),
      size = "s", easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("settings_reset_confirm", "Yes, reset",
                     class = "btn btn-danger",
                     style = "background:#DC2626;border-color:#DC2626;font-weight:600;")
      ),
      div(style = "padding:6px 0;font-size:13px;line-height:1.6;",
          HTML("This deletes the trial's <code>overrides.json</code> and reverts
                identity, features, and colours to whatever's in <code>config.R</code>.
                The original config file is not touched."))
    ))
  })
  
  # ── Danger zone (admin only) ─────────────────────────────────────────────
  output$settings_danger_zone_ui <- renderUI({
    if (!isTRUE(rv$portfolio_role == "admin")) return(NULL)
    div(style = "background:#FFFFFF;border:1px solid #FEE2E2;border-radius:14px;
                 padding:18px 22px;",
        div(style = "display:flex;align-items:center;gap:10px;margin-bottom:8px;",
            span(style = "color:#DC2626;font-size:18px;", HTML("&#x26A0;")),
            span(style = "font-weight:600;color:#991B1B;font-size:14px;",
                 "Danger zone")),
        div(style = "font-size:12.5px;color:#7F1D1D;line-height:1.6;margin-bottom:12px;",
            HTML("Deleting a trial removes its config, sites, randomisation log,
                  and any uploaded REDCap exports. <strong>This cannot be undone.</strong>")),
        actionButton("settings_delete_trial",
                     HTML("&#x1F5D1; Delete this trial"),
                     class = "btn btn-sm",
                     style = "background:#FFFFFF;color:#DC2626;border:1px solid #FECACA;
                              font-weight:600;"))
  })
  
  observeEvent(input$settings_delete_trial, {
    if (!isTRUE(rv$portfolio_role == "admin")) return()
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    showModal(modalDialog(
      title = div(style = "color:#DC2626;",
                  HTML("&#x26A0; Delete trial?")),
      size = "m", easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("settings_delete_trial_confirm",
                     "Yes, delete this trial",
                     class = "btn btn-danger",
                     style = "background:#DC2626;border-color:#DC2626;font-weight:600;")
      ),
      div(style = "padding:6px 0;font-size:13px;line-height:1.7;",
          HTML(sprintf("You're about to permanently delete <strong>%s</strong>.<br><br>
                        The trial folder <code>trials/%s/</code> will be removed,
                        including its config, sites, randomisation log, and any
                        REDCap CSVs. This action cannot be undone.",
                       cfg$short_name %||% toupper(cfg$code),
                       cfg$code)))
    ))
  })
  
  observeEvent(input$settings_delete_trial_confirm, {
    if (!isTRUE(rv$portfolio_role == "admin")) return()
    if (!require_role(rv, "manager")) return()
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    code <- cfg$code
    
    trial_dir <- file.path(getwd(), "trials", code)
    success <- tryCatch({
      unlink(trial_dir, recursive = TRUE, force = TRUE)
      TRUE
    }, error = function(e) FALSE)
    
    logo_path <- file.path(getwd(), "www", "trial_logos", paste0(code, ".jpg"))
    if (file.exists(logo_path)) file.remove(logo_path)
    
    removeModal()
    
    if (success) {
      log_activity("trial_deleted",
                   sprintf("Deleted trial <strong>%s</strong>",
                           htmltools::htmlEscape(cfg$short_name %||% toupper(code))),
                   username = rv$username, trial_code = code)
      showNotification(sprintf("Trial '%s' deleted.",
                               cfg$short_name %||% toupper(code)),
                       type = "message", duration = 5)
      # Send the user back to the home screen
      rv$trial_config <- NULL
      rv$trial_code   <- NULL
      rv$trial_role   <- NULL
      rv$home_membership_changed <- Sys.time()
      shinyjs::hide("dashboard_panel")
      shinyjs::hide("sidebar_nav_section")
      shinyjs::hide("topbar_wrap")
      shinyjs::hide("topnav_wrap")
      shinyjs::show("trial_selector_panel")
      shinyjs::runjs("document.body.classList.add('home-mode')")
    } else {
      showNotification("Could not delete trial folder. It may be in use.",
                       type = "error", duration = 8)
    }
  })
  
  observeEvent(input$settings_reset_confirm, {
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    
    clear_overrides(cfg)
    removeModal()
    
    # Reload original config from disk (without overrides) and refresh.
    fresh <- discover_trials()[[cfg$code]]
    if (!is.null(fresh)) {
      rv$trial_config <- fresh
      apply_trial_globals(fresh)
      .tk <- fresh$theme %||% "custom"
      .sv <- if (.tk %in% names(TRIAL_THEMES)) TRIAL_THEMES[[.tk]]$sidebar else "dark"
      apply_trial_colours(
        fresh$colors %||% list(primary = "#1B4F6B", secondary = "#2EC4A5", accent = "#F59E0B"),
        sidebar = .sv)
      apply_features_live(fresh$features %||% list())
    }
    
    rv$settings_changed <- Sys.time()
    rv$home_membership_changed <- Sys.time()
    showNotification(HTML("&#x2714; Overrides cleared."),
                     type = "message", duration = 4)
  })
  
  # ── Report content editor (per-trial text fields in reports) ────────────
  # Persisted under cfg$report_content in overrides.json. Both render paths
  # (the legacy download_report and the new rb_download) pass the same
  # dictionary as the `report_content` Rmd param, so TMG/iTMG and TSC pick up
  # whichever fields they reference.
  
  .rc_load <- function() {
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    rc <- cfg$report_content %||% list()
    updateTextInput(session, "rc_short_name",
                    value = rc$short_name %||% (cfg$short_name %||% ""))
    updateTextInput(session, "rc_trial_title",
                    value = rc$trial_title %||% "")
    updateTextInput(session, "rc_trial_subtitle",
                    value = rc$trial_subtitle %||% "")
    updateTextInput(session, "rc_registration",
                    value = rc$registration_line %||% "")
    # general_info may be stored as list of {label,value}; convert back to text
    gi_text <- if (is.list(rc$general_info) && length(rc$general_info)) {
      lines <- vapply(rc$general_info, function(row) {
        if (!is.null(row$label) && !is.null(row$value))
          paste0(row$label, ": ", row$value)
        else if (!is.null(names(row)) && length(row))
          paste0(names(row), ": ", as.character(row))
        else ""
      }, character(1))
      paste(lines[nzchar(lines)], collapse = "\n")
    } else ""
    updateTextAreaInput(session, "rc_general_info", value = gi_text)
  }
  
  observeEvent(input$settings_active_section, {
    if (identical(input$settings_active_section, "report_content")) .rc_load()
  })
  
  # Lazy-load when the trial changes too, so the form stays in sync with the
  # active trial without waiting for the user to click the section.
  observeEvent(rv$trial_config, { .rc_load() }, ignoreInit = TRUE)
  
  # Parse a "Label: Value" textarea into a list of named pairs, preserving
  # entry order. Lines without a colon are kept as label-only rows so the user
  # gets exactly what they typed.
  .parse_general_info <- function(txt) {
    if (is.null(txt) || !nzchar(trimws(txt))) return(list())
    lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
    rows <- lapply(lines, function(ln) {
      ln <- trimws(ln)
      if (!nzchar(ln)) return(NULL)
      idx <- regexpr(":", ln, fixed = TRUE)
      if (idx[[1]] < 1)
        return(list(label = ln, value = ""))
      list(label = trimws(substr(ln, 1, idx[[1]] - 1)),
           value = trimws(substr(ln, idx[[1]] + 1, nchar(ln))))
    })
    Filter(Negate(is.null), rows)
  }
  
  observeEvent(input$rc_save, {
    if (!require_role(rv, "manager")) return()
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    
    rc <- list(
      short_name        = trimws(input$rc_short_name %||% ""),
      trial_title       = trimws(input$rc_trial_title %||% ""),
      trial_subtitle    = trimws(input$rc_trial_subtitle %||% ""),
      registration_line = trimws(input$rc_registration %||% ""),
      general_info      = .parse_general_info(input$rc_general_info %||% "")
    )
    # Drop empty top-level scalars so JSON stays tidy
    rc <- rc[vapply(rc, function(x) {
      if (is.list(x)) length(x) > 0 else nzchar(as.character(x))
    }, logical(1))]
    
    tryCatch({
      update_overrides(cfg, report_content = rc)
      rv$trial_config$report_content <- rc
      rv$settings_changed <- Sys.time()
      log_activity("report_content_saved",
                   "Updated trial-specific report content",
                   username = rv$username, trial_code = cfg$code)
      showNotification(HTML("&check; Report content saved."),
                       type = "message", duration = 4)
    }, error = function(e) {
      showNotification(paste("Save failed:", e$message),
                       type = "error", duration = 8)
    })
  })
  
  # ── Portfolio review (fixed fields persisted in overrides) ──────────────
  # Stored under cfg$portfolio_review. Read by report_sections.R portfolio
  # render functions (.rs_render_pr_*). Variable per-report meeting dates
  # and RAG live on the Reports tab, not here.
  
  .pr_text_fields <- c("team_leader", "ci", "funder", "trial_type",
                       "intervention", "sponsor", "coordinator",
                       "sample_size", "stage", "pilot_phase_ended",
                       "brief_summary",
                       "ms_approvals", "ms_recruitment",
                       "ms_data_capture", "ms_other",
                       "db_crf_signoff", "db_func_spec", "db_req_spec",
                       "db_release_test", "db_final_release",
                       "fin_staffing_awarded", "fin_staffing_status",
                       "fin_status")
  .pr_date_fields <- c("grant_start", "grant_end", "first_patient_date",
                       "approvals_date", "open_recruitment_date")
  
  .pr_load <- function() {
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    pr <- cfg$portfolio_review %||% list()
    for (f in .pr_text_fields) {
      val <- pr[[f]] %||% ""
      inp <- paste0("pr_", f)
      if (f %in% c("brief_summary"))
        updateTextAreaInput(session, inp, value = val)
      else if (f %in% c("stage", "pilot_phase_ended", "fin_status"))
        updateSelectInput(session, inp, selected = val %||% "")
      else
        updateTextInput(session, inp, value = val)
    }
    for (f in .pr_date_fields) {
      val <- pr[[f]]
      d <- NA
      if (!is.null(val) && length(val) == 1 && nzchar(as.character(val))) {
        d <- tryCatch(as.Date(val, tryFormats = c("%Y-%m-%d", "%d %b %Y",
                                                  "%d/%m/%Y", "%d-%m-%Y")),
                      error = function(e) NA)
      }
      # NA clears the field but Shiny warns while coercing it — suppress.
      suppressWarnings(
        updateDateInput(session, paste0("pr_", f),
                        value = if (length(d) == 1 && !is.na(d)) d else NA))
    }
  }
  
  observeEvent(input$settings_active_section, {
    if (identical(input$settings_active_section, "portfolio_review")) .pr_load()
  })
  observeEvent(rv$trial_config, { .pr_load() }, ignoreInit = TRUE)
  
  observeEvent(input$pr_save, {
    if (!require_role(rv, "manager")) return()
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    pr <- list()
    for (f in .pr_text_fields) {
      v <- trimws(as.character(input[[paste0("pr_", f)]] %||% ""))
      if (nzchar(v)) pr[[f]] <- v
    }
    for (f in .pr_date_fields) {
      v <- input[[paste0("pr_", f)]]
      if (!is.null(v) && !is.na(v) && nzchar(as.character(v)))
        pr[[f]] <- format(as.Date(v), "%d %b %Y")
    }
    tryCatch({
      update_overrides(cfg, portfolio_review = pr)
      rv$trial_config$portfolio_review <- pr
      rv$settings_changed <- Sys.time()
      log_activity("portfolio_review_saved",
                   "Updated portfolio review fixed fields",
                   username = rv$username, trial_code = cfg$code)
      showNotification(HTML("&check; Portfolio review saved."),
                       type = "message", duration = 4)
    }, error = function(e) {
      showNotification(paste("Save failed:", e$message),
                       type = "error", duration = 8)
    })
  })
  
  # ── Report templates editor ──────────────────────────────────────────────
  # Lets the trial manager edit the per-trial Rmd templates (TMG/iTMG and TSC)
  # in-app. Saves write to trials/<code>/reports/<kind>_report.Rmd; "Reset to
  # default" re-copies from the canonical template at the project root.
  
  rt_kind <- reactiveVal("tonic")
  rt_modified_marker <- reactiveVal(0L)
  
  observeEvent(input$rt_pick, {
    rt_kind(input$rt_pick %||% "tonic")
    .rt_load_into_editor()
    .rt_sync_override_field()
  })
  
  # Re-load the editor when entering the section, so the textarea reflects
  # what's actually on disk.
  observeEvent(input$settings_active_section, {
    if (identical(input$settings_active_section, "report_templates")) {
      .rt_load_into_editor()
      .rt_sync_override_field()
    }
  })
  
  .rt_load_into_editor <- function() {
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    # Make sure the trial has its own copy on disk before editing.
    seed_trial_report_templates(cfg, overwrite = FALSE)
    
    path <- trial_report_template_path(cfg, rt_kind())
    content <- if (file.exists(path)) {
      tryCatch(paste(readLines(path, warn = FALSE), collapse = "\n"),
               error = function(e) "")
    } else ""
    updateTextAreaInput(session, "rt_content", value = content)
  }
  
  .rt_sync_override_field <- function() {
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    val <- cfg$report_template_paths[[rt_kind()]] %||% ""
    updateTextInput(session, "rt_override_path", value = val)
  }
  
  # Status line: shows which file the renderer will actually use, given the
  # current override / per-trial / fallback resolution order.
  output$rt_status_line <- renderUI({
    rt_modified_marker()
    cfg <- rv$trial_config
    if (is.null(cfg)) return("No trial selected.")
    
    active_path <- resolve_report_template(cfg, rt_kind())
    override    <- cfg$report_template_paths[[rt_kind()]] %||% ""
    trial_path  <- trial_report_template_path(cfg, rt_kind())
    
    src_label <- if (!is.null(active_path) && nzchar(override) &&
                     normalizePath(active_path, mustWork = FALSE) ==
                     normalizePath(override,    mustWork = FALSE)) {
      "external override"
    } else if (!is.null(active_path) &&
               normalizePath(active_path, mustWork = FALSE) ==
               normalizePath(trial_path,  mustWork = FALSE)) {
      "trial copy"
    } else if (!is.null(active_path)) {
      "project-root fallback"
    } else {
      "missing"
    }
    
    if (is.null(active_path))
      return(HTML(paste0("<em>No file resolved for <code>",
                         rt_kind(), "</code>. Set a path or save the editor below to create one.</em>")))
    
    info <- tryCatch(file.info(active_path), error = function(e) NULL)
    mtime <- if (!is.null(info) && !is.na(info$mtime))
      format(info$mtime, "%d %b %Y %H:%M") else "?"
    nlines <- tryCatch(length(readLines(active_path, warn = FALSE)),
                       error = function(e) NA_integer_)
    
    HTML(sprintf("Active source (<strong>%s</strong>): <code>%s</code> · Last modified %s%s",
                 src_label, htmltools::htmlEscape(active_path), mtime,
                 if (!is.na(nlines)) sprintf(" · %d lines", nlines) else ""))
  })
  
  # Save the override path. Refuses to set a path that doesn't exist so we
  # never silently render from a stale value.
  observeEvent(input$rt_override_save, {
    if (!require_role(rv, "manager")) return()
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    p <- trimws(input$rt_override_path %||% "")
    if (!nzchar(p)) {
      showNotification("Path is empty — use Clear to remove the override instead.",
                       type = "warning", duration = 5)
      return()
    }
    if (!file.exists(p)) {
      showNotification(paste0("File not found at: ", p),
                       type = "error", duration = 8)
      return()
    }
    paths <- cfg$report_template_paths %||% list()
    paths[[rt_kind()]] <- p
    tryCatch({
      update_overrides(cfg, report_template_paths = paths)
      rv$trial_config$report_template_paths <- paths
      rt_modified_marker(rt_modified_marker() + 1L)
      showNotification(HTML(sprintf("&check; %s template will render from <code>%s</code>.",
                                    toupper(rt_kind()),
                                    htmltools::htmlEscape(p))),
                       type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Save failed:", e$message),
                       type = "error", duration = 8)
    })
  })
  
  observeEvent(input$rt_override_clear, {
    if (!require_role(rv, "manager")) return()
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    paths <- cfg$report_template_paths %||% list()
    paths[[rt_kind()]] <- NULL
    tryCatch({
      update_overrides(cfg, report_template_paths = paths)
      rv$trial_config$report_template_paths <- paths
      updateTextInput(session, "rt_override_path", value = "")
      rt_modified_marker(rt_modified_marker() + 1L)
      showNotification(HTML(sprintf("Override cleared — %s now uses the trial's own copy.",
                                    toupper(rt_kind()))),
                       type = "message", duration = 4)
    }, error = function(e) {
      showNotification(paste("Clear failed:", e$message),
                       type = "error", duration = 8)
    })
  })
  
  # Save the textarea contents back to disk. We pull the value from the
  # browser via Shiny's input pipe — `rt_content` is bound by the textarea id
  # and Shiny serialises it on every keystroke.
  observeEvent(input$rt_save, {
    if (!require_role(rv, "manager")) return()
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    body <- input$rt_content %||% ""
    if (!nzchar(trimws(body))) {
      showNotification("Refusing to save an empty template.",
                       type = "warning", duration = 5)
      return()
    }
    path <- trial_report_template_path(cfg, rt_kind())
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    tryCatch({
      writeLines(body, path, useBytes = TRUE)
      mark_trial_template_edited(cfg, rt_kind())
      rt_modified_marker(rt_modified_marker() + 1L)
      log_activity("report_template_saved",
                   sprintf("Edited <strong>%s</strong> template",
                           toupper(rt_kind())),
                   username = rv$username, trial_code = cfg$code)
      showNotification(HTML(sprintf("&check; %s template saved.",
                                    toupper(rt_kind()))),
                       type = "message", duration = 4)
    }, error = function(e) {
      showNotification(paste("Save failed:", e$message),
                       type = "error", duration = 8)
    })
  })
  
  observeEvent(input$rt_reset, {
    if (!require_role(rv, "manager")) return()
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    showModal(modalDialog(
      title = HTML("&#x21BA; Reset template?"),
      size = "s", easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("rt_reset_confirm", "Yes, reset",
                     class = "btn btn-danger",
                     style = "background:#DC2626;border-color:#DC2626;font-weight:600;")
      ),
      div(style = "padding:6px 0;font-size:13px;line-height:1.6;",
          HTML(sprintf("This re-copies the canonical <strong>%s</strong> template
                        from the project root, overwriting your trial's edits.
                        This can't be undone.",
                       toupper(rt_kind()))))
    ))
  })
  
  # Re-seed BOTH templates from the canonical project-root copies. Useful
  # when the trial's per-trial Rmd files predate generic templates and still
  # contain TONIC-specific text — one click pulls the latest from the root.
  observeEvent(input$rt_reseed_all, {
    if (!require_role(rv, "manager")) return()
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    showModal(modalDialog(
      title = HTML("&#x21BA; Re-seed both templates?"),
      size = "s", easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("rt_reseed_all_confirm", "Yes, re-seed both",
                     class = "btn btn-danger",
                     style = "background:#DC2626;border-color:#DC2626;font-weight:600;")
      ),
      div(style = "padding:6px 0;font-size:13px;line-height:1.6;",
          HTML("This overwrites <strong>both</strong> the TMG/iTMG and TSC
                templates in this trial's reports/ folder with fresh copies
                from the project root. Any per-trial edits to those files
                will be lost."))
    ))
  })
  
  observeEvent(input$rt_reseed_all_confirm, {
    cfg <- rv$trial_config
    if (is.null(cfg)) { removeModal(); return() }
    removeModal()
    tryCatch({
      seed_trial_report_templates(cfg, overwrite = TRUE)
      rt_modified_marker(rt_modified_marker() + 1L)
      .rt_load_into_editor()
      log_activity("report_templates_reseeded",
                   "Re-seeded all report templates from project-root canonical versions",
                   username = rv$username, trial_code = cfg$code)
      showNotification(HTML("&check; Both templates re-seeded from project root."),
                       type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Re-seed failed:", e$message),
                       type = "error", duration = 8)
    })
  })
  
  observeEvent(input$rt_reset_confirm, {
    cfg <- rv$trial_config
    if (is.null(cfg)) { removeModal(); return() }
    src <- default_report_template_path(rt_kind())
    dst <- trial_report_template_path(cfg, rt_kind())
    removeModal()
    if (!file.exists(src)) {
      showNotification(paste("Default template missing:", src),
                       type = "error", duration = 8)
      return()
    }
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    tryCatch({
      file.copy(src, dst, overwrite = TRUE)
      clear_trial_template_edited(cfg, rt_kind())
      rt_modified_marker(rt_modified_marker() + 1L)
      .rt_load_into_editor()
      showNotification(HTML(sprintf("&check; %s template reset to default.",
                                    toupper(rt_kind()))),
                       type = "message", duration = 4)
    }, error = function(e) {
      showNotification(paste("Reset failed:", e$message),
                       type = "error", duration = 8)
    })
  })
}