participants_server <- function(input, output, session, state) {
  rv <- state$rv
  n_event <- function(et) {
    df <- rv$participants
    if (is.null(df)||nrow(df)==0) return(0L)
    length(unique(df$record_id[df$event_type==et]))
  }
  # Event labels are derived from the active trial's redcap_events mapping
  # (process_redcap() title-cases the role names: "baseline" → "Baseline",
  # "day_30" → "Day 30"). The four headline boxes assume the standard four
  # timepoints; if a trial is missing one, that box just shows 0.
  output$n_p_baseline  <- renderText(n_event("Baseline"))
  output$n_p_discharge <- renderText(n_event("Discharge"))
  output$n_p_d30       <- renderText(n_event("Day 30"))
  output$n_p_d90       <- renderText(n_event("Day 90"))
  
  # Update site filter choices when data changes
  observe({
    raw <- rv$raw_redcap
    if (is.null(raw)||nrow(raw)==0) return()
    sites <- sort(unique(raw$site_dag[!is.na(raw$site_dag) & nchar(raw$site_dag)>0]))
    updatePickerInput(session, "pq_site_filter", choices=sites, selected=character(0))
  })
  
  # Filtered participant IDs
  pq_filtered <- reactive({
    raw <- rv$raw_redcap
    if (is.null(raw)||nrow(raw)==0) return(NULL)
    
    ids <- sort(unique(raw$record_id))
    
    # Apply site filter
    site_sel <- input$pq_site_filter
    if (!is.null(site_sel) && length(site_sel)>0) {
      site_ids <- unique(raw$record_id[raw$site_dag %in% site_sel])
      ids <- intersect(ids, site_ids)
    }
    
    # Apply record ID text filter
    id_txt <- trimws(input$pq_id_filter %||% "")
    if (nchar(id_txt)>0) {
      ids <- ids[grepl(id_txt, ids, fixed=TRUE)]
    }
    
    no_filter <- (is.null(site_sel) || length(site_sel)==0) && nchar(id_txt)==0
    list(ids=ids, no_filter=no_filter)
  })
  
  output$pq_showing_label <- renderText({
    filt <- pq_filtered()
    if (is.null(filt)) return("")
    n_total <- length(filt$ids)
    if (filt$no_filter) {
      n_show <- min(10L, n_total)
      sprintf("Showing last %d of %d (filter to see all)", n_show, n_total)
    } else {
      sprintf("Showing %d participant%s", n_total, if(n_total!=1) "s" else "")
    }
  })
  
  # ── Customisable demographic breakdowns ──────────────────────────────────
  # Detect usable columns in the uploaded CSV and let the user pick which to
  # render. Selection persists in overrides.json under participant_breakdowns.

  detected_breakdowns <- reactive({
    cfg <- rv$trial_config
    detect_breakdown_columns(rv$raw_redcap, cfg)
  })

  selected_breakdowns <- reactiveVal(NULL)

  # Initialise from cfg (or auto-pick) when the trial / data changes
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

  output$breakdowns_summary_txt <- renderText({
    sel <- selected_breakdowns()
    if (is.null(sel) || !length(sel)) return("None selected")
    paste(length(sel), if (length(sel) == 1) "breakdown" else "breakdowns")
  })

  output$participant_breakdowns_ui <- renderUI({
    raw <- rv$raw_redcap
    cfg <- rv$trial_config
    if (is.null(raw) || !nrow(raw))
      return(div(class = "info-box-tonic",
                 "No REDCap data — upload a CSV to populate demographics."))

    sel <- selected_breakdowns() %||% character(0)
    breakdowns <- lapply(sel, function(c) compute_breakdown(raw, c, cfg))
    render_breakdowns_grid(breakdowns)
  })

  # ── Configure modal ──────────────────────────────────────────────────────
  observeEvent(input$configure_breakdowns, {
    det <- detected_breakdowns()
    sel <- selected_breakdowns() %||% character(0)
    if (!nrow(det)) {
      showNotification("No usable columns detected — upload a CSV first.",
                       type = "warning", duration = 5)
      return()
    }
    showModal(modalDialog(
      title = div(style = "display:flex;align-items:center;gap:10px;",
                  span(style = "font-size:18px;color:#6366F1;", HTML("&#x2699;")),
                  span("Configure demographic breakdowns")),
      size = "l", easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_breakdowns", "Save selection",
                     class = "btn btn-primary",
                     style = "background:#6366F1;border-color:#6366F1;font-weight:600;")
      ),

      div(style = "font-size:12.5px;color:#64748B;margin-bottom:14px;line-height:1.6;",
          HTML(sprintf("Detected <strong>%d</strong> columns suitable for breakdowns
                        from the latest CSV. Tick the ones you want to display.",
                       nrow(det)))),

      checkboxGroupInput(
        "breakdowns_choice", label = NULL,
        choiceNames = lapply(seq_len(nrow(det)), function(i) {
          r <- det[i, ]
          tagList(
            span(style = "font-weight:600;color:#0F172A;", r$label),
            span(style = "font-size:10.5px;color:#94A3B8;margin-left:6px;
                          text-transform:uppercase;letter-spacing:.4px;",
                 r$type),
            span(style = "font-size:11px;color:#64748B;margin-left:6px;",
                 sprintf("· %s · %d unique%s", r$column, r$n_unique,
                         if (r$n_missing > 0)
                           sprintf(" · %d missing", r$n_missing) else ""))
          )
        }),
        choiceValues = det$column,
        selected = intersect(sel, det$column))
    ))
  })

  observeEvent(input$save_breakdowns, {
    cfg <- rv$trial_config
    if (is.null(cfg)) { removeModal(); return() }
    chosen <- input$breakdowns_choice %||% character(0)
    selected_breakdowns(chosen)
    # Persist to overrides.json
    tryCatch(
      update_overrides(cfg, participant_breakdowns = as.list(chosen)),
      error = function(e) message("breakdown save: ", e$message)
    )
    rv$trial_config$participant_breakdowns <- chosen
    removeModal()
    showNotification(sprintf("Saved %d breakdown%s.",
                             length(chosen),
                             if (length(chosen) == 1) "" else "s"),
                     type = "message", duration = 3)
  })

    output$participants_ui <- renderUI({
    raw <- rv$raw_redcap
    if (is.null(raw)||nrow(raw)==0)
      return(div(class="info-box-tonic","No REDCap data \u2014 use Data / Export tab to load CSV."))

    # The participant table is rendered from the active trial's
    # participant_table_layout. Each timepoint has a display name (matches
    # event_type produced by process_redcap), an event role, and a list of
    # instruments with `label` + `field` (the REDCap completion column).
    cfg    <- current_trial_config()
    layout <- cfg$participant_table_layout
    tps    <- layout$timepoints
    if (is.null(tps) || length(tps) == 0)
      return(div(class="info-box-tonic",
                 "No participant_table_layout configured for this trial."))

    all_fields <- unlist(lapply(tps, function(tp)
      vapply(tp$instruments, function(ins) ins$field %||% "", character(1))))
    if (!any(all_fields %in% names(raw)))
      return(div("Questionnaire columns not found in export."))

    filt <- pq_filtered()
    if (is.null(filt) || length(filt$ids)==0)
      return(div(class="info-box-tonic","No participants match the current filter."))

    ids <- filt$ids
    if (filt$no_filter && length(ids) > 10) ids <- tail(ids, 10)

    # Header: top row = timepoint name spanning its instruments;
    # second row = one cell per instrument label.
    top_cells <- lapply(tps, function(tp) {
      tags$th(colspan = length(tp$instruments),
              style  = "text-align:center",
              tp$name %||% "")
    })
    sub_cells <- unlist(lapply(tps, function(tp)
      lapply(tp$instruments, function(ins) tags$th(ins$label %||% ""))),
      recursive = FALSE)

    header <- tags$thead(
      tags$tr(style="background:#F8FAFD",
              tags$th(rowspan=2, style="text-align:left;border-right:2px solid #EEF3F8;min-width:90px;background:#F8FAFD", "Record ID"),
              tags$th(rowspan=2, style="text-align:left;border-right:2px solid #EEF3F8;min-width:110px;background:#F8FAFD", "Site"),
              top_cells),
      tags$tr(sub_cells)
    )

    tbody_rows <- lapply(ids, function(rid) {
      p_rows <- raw %>% filter(record_id == rid)
      site   <- coalesce(p_rows$site_dag[1], "")
      cells <- unlist(lapply(tps, function(tp) {
        r <- p_rows %>% filter(event_type == (tp$name %||% ""))
        lapply(tp$instruments, function(ins) {
          col <- ins$field %||% ""
          val <- if (nrow(r) > 0 && col %in% names(r))
            suppressWarnings(as.integer(r[[col]][1])) else NA_integer_
          tags$td(style = "text-align:center", HTML(comp_label(val)))
        })
      }), recursive = FALSE)
      tags$tr(
        tags$td(class="sid", style="text-align:left;border-right:2px solid #EEF3F8", rid),
        tags$td(style="text-align:left;border-right:2px solid #EEF3F8", site),
        cells
      )
    })

    div(class="comp-tbl",
        tags$table(style="width:100%;border-collapse:collapse;font-size:12.5px;min-width:1100px",
                   header, tags$tbody(tbody_rows)))
  })
  
  # Safety helpers
  safety_df <- reactive({ parse_safety(rv$raw_redcap) })
  has_dev_col <- reactive({
    dev_field <- fld("deviation_complete", default = "deviation_complete")
    !is.null(rv$raw_redcap) && dev_field %in% names(rv$raw_redcap)
  })
  safe_n <- function(col) {
    df <- safety_df()
    if (is.null(df)||nrow(df)==0) return(0L)
    length(unique(df$record_id[!is.na(df[[col]])&df[[col]]]))
  }
  output$n_s_saes  <- renderText(safe_n("sae"))
  output$n_s_dev   <- renderText(if(!has_dev_col()) "N/A" else safe_n("deviation"))
  output$n_s_wd    <- renderText({ df<-safety_df(); if(is.null(df)) 0L else length(unique(df$record_id[df$withdrawn])) })
  output$n_s_pn    <- renderText(safe_n("preg_notif"))
  output$n_s_po    <- renderText(safe_n("preg_out"))
  output$n_s_sites <- renderText({
    df <- safety_df()
    if(is.null(df)) return(0L)
    length(unique(df$site_dag[df$sae|df$withdrawn|df$preg_notif|df$preg_out]))
  })
  
  output$delta_saes <- renderUI({
    delta_badge_ui(safe_n("sae"), 0L)
  })
  output$delta_wd <- renderUI({
    df <- safety_df(); if(is.null(df)) return(NULL)
    current <- length(unique(df$record_id[df$withdrawn]))
    delta_badge_ui(current, 0L)
  })
  
  output$safety_table <- renderReactable({
    df <- safety_df()
    if (is.null(df)||nrow(df)==0)
      return(empty_reactable("No data \u2014 load REDCap CSV"))
    tbl <- df %>% group_by(Site=site_dag) %>%
      summarise(SAEs=length(unique(record_id[sae])),
                Deviations=if(has_dev_col()) length(unique(record_id[deviation])) else NA_integer_,
                Withdrawals=length(unique(record_id[withdrawn])),
                Preg_notif=length(unique(record_id[preg_notif])),
                Preg_out=length(unique(record_id[preg_out])),.groups="drop") %>%
      rename(`Preg. notif.`=Preg_notif, `Preg. outcomes`=Preg_out)
    if (!has_dev_col()) tbl$Deviations <- "N/A"
    reactable(tbl, striped=TRUE, highlight=TRUE, compact=TRUE,
              defaultColDef=colDef(align="center",style=list(fontFamily="Outfit",fontSize="12.5px")),
              columns=list(Site=colDef(align="left")))
  })
  
  output$withdrawal_table <- renderReactable({
    df <- safety_df()
    if (is.null(df)||nrow(df)==0)
      return(empty_reactable("No withdrawals"))
    w <- df %>%
      filter(withdrawn,!is.na(cos_type),cos_type!="NA",nchar(trimws(cos_type))>0) %>%
      distinct(record_id,site_dag,cos_type) %>%
      mutate(Code=trimws(cos_type),
             Reason=dplyr::recode(Code,!!!cos_type_labels,.default=paste("Code:",Code))) %>%
      count(Site=site_dag, Code, Reason, name="Count") %>%
      arrange(Site, Code)
    if (nrow(w)==0)
      return(empty_reactable("No withdrawals recorded"))
    reactable(w, striped=TRUE, highlight=TRUE, compact=TRUE,
              defaultColDef=colDef(style=list(fontFamily="Outfit",fontSize="12.5px")),
              columns=list(Count=colDef(align="center"),Code=colDef(align="center")))
  })
  
  output$dl_participants <- xlsx_download(
    function() rv$participants,
    paste0(current_trial_config()$short_name %||% "trial", "_participants")
  )
}
