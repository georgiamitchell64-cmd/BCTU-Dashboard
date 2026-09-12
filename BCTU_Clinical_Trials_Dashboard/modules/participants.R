# ─────────────────────────────────────────────────────────────────────────────
# Data tab — visual-first redesign (v2 layout).
# ----------------------------------------------------------------------------
# Stack rows full-width so the page has no big empty rectangle regardless of
# whether the safety drill-down is open or closed:
#
#   Row 1  : 4 donut KPI cards         + download button (inline)
#   Row 2  : Safety & regulatory card  — full width, 4 clickable tiles
#   Row 2a : Drill-down panel          — full width, only when a tile is open
#   Row 3  : 3-column bottom row       — Demographics · Withdrawals · Quick
#                                        actions (all roughly the same height)
#
# Per-participant questionnaire completion has moved to the Returns tab.
# ─────────────────────────────────────────────────────────────────────────────

participants_tab_ui <- function() {
  tabPanel("participants",

    # ── Row 1: timepoint donut KPIs (config-driven) + inline download ────
    div(class = "data-hero-row",
      uiOutput("data_donuts"),
      div(class = "data-hero-download",
          downloadButton("dl_participants", HTML("&darr; Download all data"),
                         class = "btn data-dl-btn"))
    ),

    # ── Data health: every scheduled form for every participant ─────────
    div(class = "th-section",
      div(class = "th-section-head",
        div(tags$h3("Data health"),
            div(class = "th-sub-t",
                "Every scheduled CRF and questionnaire for every participant. Due dates come from the visit schedule in Settings → Visits & CRFs; hover over a cell for details or click a row to open the participant.")),
      ),
      uiOutput("data_health_kpis"),
      uiOutput("crf_grid_ui")),

    # ── Worklist: who needs chasing, most urgent first ──────────────────
    div(class = "th-section",
      div(class = "th-section-head",
        div(tags$h3("Participants needing attention"),
            div(class = "th-sub-t",
                "Overdue site CRFs and missing operation or discharge dates rank highest; overdue questionnaires add a little. Click a row for the full CRF history.")),
        div(class = "th-toolbar",
            selectInput("wl_site", NULL, choices = c("All sites" = ""), width = "190px"),
            checkboxGroupInput("wl_prio", NULL, choices = c("High", "Medium", "Low"),
                               selected = c("High", "Medium"), inline = TRUE),
            downloadButton("dl_worklist", HTML("&darr; Download"), class = "btn-ghost-sm"))),
      reactableOutput("worklist_table")),

    # ── Row 2: Safety & regulatory card (full width) ────────────────────
    div(class = "data-safety-card",
      div(class = "data-safety-head",
        span(class = "data-safety-title", "Safety & regulatory"),
        span(class = "data-safety-meta",
             "click a tile to see the records")
      ),
      div(class = "data-safety-tiles",
        actionButton("safety_tile_sae",  label = NULL,
                     class = "s-tile s-tile-sae",
                     uiOutput("safety_tile_sae_body",  inline = TRUE)),
        actionButton("safety_tile_dev",  label = NULL,
                     class = "s-tile s-tile-dev",
                     uiOutput("safety_tile_dev_body",  inline = TRUE)),
        actionButton("safety_tile_wd",   label = NULL,
                     class = "s-tile s-tile-wd",
                     uiOutput("safety_tile_wd_body",   inline = TRUE)),
        actionButton("safety_tile_preg", label = NULL,
                     class = "s-tile s-tile-preg",
                     uiOutput("safety_tile_preg_body", inline = TRUE)),
        # Complications — hidden until complication columns are mapped in
        # Trial Settings → Detail fields (shown by the server when present).
        shinyjs::hidden(actionButton("safety_tile_comp", label = NULL,
                     class = "s-tile s-tile-comp",
                     uiOutput("safety_tile_comp_body", inline = TRUE)))
      )
    ),

    # ── Row 2a: Drill-down (renders only when a tile is open) ───────────
    uiOutput("safety_drill_ui"),

    # ── Row 3: Demographics — full width so breakdowns lay out horizontally
    # (the inner grid in render_breakdowns_grid is auto-fill 280px minimums,
    # so giving it the whole page width lets it spread into 3–4 columns).
    div(class = "data-panel",
      div(class = "data-panel-head",
        span(class = "data-panel-title", "Demographics"),
        span(class = "data-panel-meta",
             textOutput("demo_n_label", inline = TRUE))
      ),
      uiOutput("participant_breakdowns_ui"),
      div(style = "font-size:11px;color:#58595B;font-style:italic;
                   padding-top:8px;margin-top:8px;border-top:1px dashed #E3E3E3;
                   display:flex;justify-content:space-between;align-items:center;",
          span(textOutput("breakdowns_summary_txt", inline = TRUE)),
          actionLink("configure_breakdowns",
                     HTML("&#x2699; Configure"),
                     style = "color:#1B1B1B;font-weight:600;font-size:11px;"))
    ),

    # ── Row 4: Withdrawals & change of status — retention, type, month, site
    div(class = "data-panel",
      div(class = "data-panel-head",
        span(class = "data-panel-title", "Withdrawals & change of status"),
        span(class = "data-panel-meta", textOutput("cos_meta", inline = TRUE))
      ),
      uiOutput("cos_panel_ui")
    ),

    # ── Row 5: Quick actions ─────────────────────────────────────────────
    div(class = "data-quick-row",
      actionButton("qa_open_returns", HTML("&#8599; Open Returns tab"), class = "qa-btn"),
      actionButton("qa_review_wd", HTML("&#8599; List every change of status"), class = "qa-btn"),
      downloadButton("qa_export_full", HTML("&darr; Export full dataset"), class = "qa-btn")
    )
  )
}
