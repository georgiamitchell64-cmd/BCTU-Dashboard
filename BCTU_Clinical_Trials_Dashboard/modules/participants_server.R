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
  
  # --- Demographics summary cards ---
  # White-ethnicity codes & ethnicity labels come from the active trial config.
  # Fall back to the NHS 19-category scheme that TONIC uses.
  WHITE_CODES <- {
    cfg <- current_trial_config()
    as.integer(cfg$white_ethnicity_codes %||% as.character(13:17))
  }

  demo_data <- reactive({
    raw <- rv$raw_redcap
    if (is.null(raw) || nrow(raw)==0) return(NULL)
    bl <- raw %>% filter(event_type=="Baseline") %>% distinct(record_id, .keep_all=TRUE)
    if (nrow(bl)==0) return(NULL)

    c_age  <- fld("age",        default = "cae_age")
    c_eth  <- fld("ethnicity",  default = "base_ethnic_gp")
    c_nela <- fld("nela_score", default = "base_nela_score_mort")

    age  <- if (c_age  %in% names(bl)) suppressWarnings(as.numeric(bl[[c_age]]))  else rep(NA_real_,    nrow(bl))
    eth  <- if (c_eth  %in% names(bl)) suppressWarnings(as.integer(bl[[c_eth]]))  else rep(NA_integer_, nrow(bl))
    nela <- if (c_nela %in% names(bl)) suppressWarnings(as.numeric(bl[[c_nela]])) else rep(NA_real_,    nrow(bl))

    list(age=age, eth=eth, nela=nela, n=nrow(bl))
  })
  
  # Age outputs
  output$demo_age_headline <- renderText({
    d <- demo_data(); if (is.null(d)) return("No data")
    n_valid <- sum(!is.na(d$age))
    n_over75 <- sum(d$age >= 75, na.rm=TRUE)
    pct <- if (n_valid>0) round(100*n_over75/n_valid) else 0
    sprintf("%d%% aged 75+", pct)
  })
  output$demo_age_under75 <- renderText({
    d <- demo_data(); if (is.null(d)) return("-")
    n <- sum(d$age < 75, na.rm=TRUE); n_valid <- sum(!is.na(d$age))
    pct <- if (n_valid>0) round(100*n/n_valid) else 0
    sprintf("%d  (%d%%)", n, pct)
  })
  output$demo_age_over75 <- renderText({
    d <- demo_data(); if (is.null(d)) return("-")
    n <- sum(d$age >= 75, na.rm=TRUE); n_valid <- sum(!is.na(d$age))
    pct <- if (n_valid>0) round(100*n/n_valid) else 0
    sprintf("%d  (%d%%)", n, pct)
  })
  
  # Ethnicity outputs
  output$demo_eth_headline <- renderText({
    d <- demo_data(); if (is.null(d)) return("No data")
    n_valid <- sum(!is.na(d$eth))
    n_nonwhite <- sum(!d$eth %in% WHITE_CODES, na.rm=TRUE)
    pct <- if (n_valid>0) round(100*n_nonwhite/n_valid) else 0
    sprintf("%d%% non-white", pct)
  })
  output$demo_eth_breakdown <- renderUI({
    d <- demo_data()
    if (is.null(d)) return(div(class="demo-label","No data"))
    eth_valid <- d$eth[!is.na(d$eth)]
    if (length(eth_valid)==0) return(div(class="demo-label","No ethnicity data"))
    # Group into broad categories
    broad <- dplyr::case_when(
      eth_valid %in% 1:5   ~ "Asian or Asian British",
      eth_valid %in% 6:8   ~ "Black, Black British or Caribbean",
      eth_valid %in% 9:12  ~ "Mixed or multiple ethnic groups",
      eth_valid %in% 13:17 ~ "White",
      eth_valid %in% 18:19 ~ "Other ethnic group",
      TRUE                 ~ "Unknown"
    )
    tbl <- sort(table(broad), decreasing=TRUE)
    n_total <- length(eth_valid)
    rows <- lapply(names(tbl), function(grp) {
      n <- as.integer(tbl[grp])
      pct <- round(100*n/n_total)
      div(class="demo-row",
          span(class="demo-label", grp),
          span(class="demo-val", sprintf("%d  (%d%%)", n, pct)))
    })
    tagList(rows)
  })
  
  # NELA outputs
  output$demo_nela_headline <- renderText({
    d <- demo_data(); if (is.null(d)) return("No data")
    n_valid <- sum(!is.na(d$nela))
    n_high <- sum(d$nela >= 5, na.rm=TRUE)
    pct <- if (n_valid>0) round(100*n_high/n_valid) else 0
    sprintf("%d%% NELA 5%%+", pct)
  })
  output$demo_nela_under5 <- renderText({
    d <- demo_data(); if (is.null(d)) return("-")
    n <- sum(d$nela < 5, na.rm=TRUE); n_valid <- sum(!is.na(d$nela))
    pct <- if (n_valid>0) round(100*n/n_valid) else 0
    sprintf("%d  (%d%%)", n, pct)
  })
  output$demo_nela_over5 <- renderText({
    d <- demo_data(); if (is.null(d)) return("-")
    n <- sum(d$nela >= 5, na.rm=TRUE); n_valid <- sum(!is.na(d$nela))
    pct <- if (n_valid>0) round(100*n/n_valid) else 0
    sprintf("%d  (%d%%)", n, pct)
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
