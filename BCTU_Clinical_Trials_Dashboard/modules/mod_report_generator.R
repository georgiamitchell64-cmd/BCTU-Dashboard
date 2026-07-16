# =============================================================================
# TONIC Dashboard — Report Generator Module
# =============================================================================
# Handles TMG, iTMG and TSC report generation from a single UI.
# TMG/iTMG use tonic_report.Rmd (HTML); TSC uses tsc_report.Rmd (Word .docx).
#
# Usage in app:
#   # UI:
#   mod_report_generator_ui("report_gen")
#
#   # Server:
#   mod_report_generator_server("report_gen",
#     report_data_reactive = reactive({ prepare_report_data(my_df, ...) }),
#     current_filters      = reactive(list(selected_sites = input$sites,
#                                          date_from = input$from,
#                                          date_to = input$to)),
#     crf_csv_path         = reactive(input$crf_path),
#     rmd_dir              = "inst/rmd")   # folder containing the two Rmd files

suppressPackageStartupMessages({
  library(shiny)
  library(rmarkdown)
})

# --- TONIC default trial summary text (pre-fills the TSC side panel) -------
TONIC_DEFAULTS <- list(
  trial_title = "A randomised trial comparing early parenteral nutrition vs standard nutritional care in adults undergoing emergency laparotomy.",
  short_title = "TONIC",
  ci = "Mr Matthew Lee",
  sponsor = "University of Birmingham",
  sponsor_ref = "RG_25-059",
  funder = "National Institute for Health and Care Research, Health Technology Assessment Programme (NIHR \u2013 HTA)",
  funder_ref = "NIHR155875",
  isrctn = "59200516",
  iras = "328678",

  ts_objectives = paste(
    "Primary clinical objective: to determine whether early parenteral nutrition (PN) in patients undergoing emergency laparotomy/laparoscopy reduces in-hospital post-operative complications assessed at hospital discharge as compared to usual nutritional care, measured using the Comprehensive Complication Index (CCI).",
    "",
    "Secondary objectives: to assess the impact of early PN on post-operative complications, activities of daily living, muscle function (sit-to-stand test), patient-reported outcomes (PRO-diGI, QoR-15, EQ-5D), SAE rates, unplanned readmissions, hospital length of stay, discharge destination, participant satisfaction, and to report PN use (duration and calories) and pathway metrics (randomisation, line insertion, operation, time to starting PN) up to 90 days post-operation.",
    "",
    "Economic objectives: primary \u2014 economic evaluation alongside the trial, assessing cost-effectiveness over 90 days from an NHS and personal social service cost perspective. Secondary \u2014 cost-effectiveness from NHS, personal social service and societal perspectives; lifetime-horizon extrapolation from an NHS perspective; Expected Value of Information analysis.",
    sep = "\n"
  ),

  ts_design = "Multi-centre, two-arm, parallel-group, superiority, individual participant randomised controlled trial with 1:1 allocation to early parenteral nutrition (PN) or standard nutritional care. Includes an internal pilot (first 6 months of recruitment) and a full economic evaluation.",

  ts_eligibility = "Adults undergoing National Emergency Laparotomy Audit (NELA) eligible emergency laparotomy or laparoscopy.",

  ts_interventions = paste(
    "Intervention arm: early parenteral nutrition (PN) started within 48 hours of emergency laparotomy/laparoscopy.",
    "Control arm: standard nutritional care.",
    sep = "\n"
  ),

  ts_primary_outcome = "In-hospital post-operative complications assessed at hospital discharge, measured using the Comprehensive Complication Index (CCI).",

  ts_secondary_outcomes = paste(
    "Post-operative complications up to 90 days post-operation",
    "Activities of daily living up to 90 days post-operation (Barthel Index)",
    "Muscle function (sit-to-stand test) at hospital discharge",
    "Patient-reported outcomes (PRO-diGI, QoR-15, EQ-5D) up to 90 days post-operation",
    "SAE rate related to early PN up to 89 days post-operation",
    "Unplanned readmissions following discharge up to 90 days post-operation",
    "Hospital length of stay during index admission",
    "Discharge destination following index admission",
    "Participant satisfaction with treatment",
    "PN use in both arms (duration and calories administered)",
    "Use of any nutritional intervention in either trial arm",
    "Pathway metrics: randomisation, line insertion, operation, and time to starting PN",
    sep = "\n"
  ),

  pilot_info = paste(
    "Internal pilot: first 6 months of recruitment, aiming for 60 participants (7% of total sample) from 6 sites.",
    "Assessed on: recruitment rate, data completeness for primary outcome, 90-day questionnaire return rate, intervention delivery within 48 hours, and cross-over rates between arms.",
    "Continuation guided by pre-defined stop-go criteria.",
    sep = " "
  )
)

# --- UI --------------------------------------------------------------------
mod_report_generator_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "display:flex; gap:16px; align-items:flex-start;",

      # Left: core controls (always visible)
      div(
        style = "flex:1; min-width:280px;",
        h4("Generate Report", style = "margin-top:0;"),

        radioButtons(ns("report_type"), "Report type",
                     choices = c("TMG — Trial Management Group"     = "TMG",
                                 "iTMG — Independent TMG"           = "iTMG",
                                 "TSC — Trial Steering Committee"   = "TSC"),
                     selected = "TMG"),

        radioButtons(ns("data_scope"), "Data scope",
                     choices = c("Use current dashboard filters" = "filtered",
                                 "Full trial data (ignore filters)" = "all"),
                     selected = "filtered",
                     inline = FALSE),

        downloadButton(ns("generate"), "Generate report",
                       class = "btn-primary",
                       style = "width:100%; margin-top:8px;")
      ),

      # Right: TSC-only side panel (hidden otherwise)
      conditionalPanel(
        condition = sprintf("input['%s'] == 'TSC'", ns("report_type")),
        div(
          style = paste("flex:1.5; min-width:360px; background:#f8fafb;",
                        "border:1px solid #d0dde6; border-left:3px solid #1B4F6B;",
                        "border-radius:4px; padding:14px 16px;"),
          h5("TSC report details", style = "margin-top:0; color:#1B4F6B;"),

          tabsetPanel(
            id = ns("tsc_tabs"),
            tabPanel("Meeting details",
              br(),
              dateInput(ns("meeting_date"), "Date of TSC meeting", value = Sys.Date()),
              textInput(ns("prepared_by"), "Report prepared by", value = ""),
              textInput(ns("reviewed_by"), "Report reviewed by", value = ""),
              textInput(ns("protocol_version"), "Protocol version", value = "")
            ),
            tabPanel("Trial summary",
              br(),
              helpText("Pre-filled from TONIC protocol \u2014 review and edit before generating if needed."),
              textAreaInput(ns("ts_objectives"),         "Objectives",                  rows = 6, resize = "vertical",
                            value = TONIC_DEFAULTS$ts_objectives),
              textAreaInput(ns("ts_design"),             "Trial design",                rows = 3, resize = "vertical",
                            value = TONIC_DEFAULTS$ts_design),
              textAreaInput(ns("ts_eligibility"),        "Eligibility criteria",        rows = 2, resize = "vertical",
                            value = TONIC_DEFAULTS$ts_eligibility),
              textAreaInput(ns("ts_interventions"),      "Interventions",               rows = 3, resize = "vertical",
                            value = TONIC_DEFAULTS$ts_interventions),
              textAreaInput(ns("ts_primary_outcome"),    "Primary outcome measure",     rows = 3, resize = "vertical",
                            value = TONIC_DEFAULTS$ts_primary_outcome),
              textAreaInput(ns("ts_secondary_outcomes"), "Secondary outcome measures",  rows = 8, resize = "vertical",
                            value = TONIC_DEFAULTS$ts_secondary_outcomes)
            ),
            tabPanel("Funder update",
              br(),
              helpText("Optional narrative for funder extension request / update. Leave blank to omit the section."),
              textAreaInput(ns("funder_update"), "Funder update narrative", rows = 6, resize = "vertical")
            ),
            tabPanel("Amendments",
              br(),
              h6("Substantial amendments", style = "color:#1B4F6B;"),
              uiOutput(ns("amd_sub_ui")),
              actionButton(ns("amd_sub_add"), "+ Add substantial amendment", class = "btn-sm"),
              actionButton(ns("amd_sub_remove"), "− Remove last", class = "btn-sm"),
              br(), br(),
              h6("Non-substantial amendments", style = "color:#1B4F6B;"),
              uiOutput(ns("amd_nonsub_ui")),
              actionButton(ns("amd_nonsub_add"), "+ Add non-substantial amendment", class = "btn-sm"),
              actionButton(ns("amd_nonsub_remove"), "− Remove last", class = "btn-sm")
            ),
            tabPanel("Custom sections",
              br(),
              helpText(HTML(paste(
                "Add narrative sections that aren't in the data (e.g. PI Associate Scheme updates,",
                "sponsor commentary, operational notes). Each slots into a fixed position in the report."
              ))),
              uiOutput(ns("cs_ui")),
              actionButton(ns("cs_add"), "+ Add custom section", class = "btn-sm"),
              actionButton(ns("cs_remove"), "− Remove last", class = "btn-sm")
            )
          )
        )
      )
    )
  )
}

# --- Server ----------------------------------------------------------------
mod_report_generator_server <- function(id,
                                         report_data_reactive,
                                         current_filters = reactive(list()),
                                         crf_csv_path    = reactive(NULL),
                                         rmd_dir         = ".",
                                         logo_path       = NULL) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Reactive counters for dynamic rows ----
    n_amd_sub    <- reactiveVal(0L)
    n_amd_nonsub <- reactiveVal(0L)
    n_custom     <- reactiveVal(0L)

    observeEvent(input$amd_sub_add,    n_amd_sub(n_amd_sub() + 1L))
    observeEvent(input$amd_sub_remove, if (n_amd_sub() > 0) n_amd_sub(n_amd_sub() - 1L))
    observeEvent(input$amd_nonsub_add,    n_amd_nonsub(n_amd_nonsub() + 1L))
    observeEvent(input$amd_nonsub_remove, if (n_amd_nonsub() > 0) n_amd_nonsub(n_amd_nonsub() - 1L))
    observeEvent(input$cs_add,    n_custom(n_custom() + 1L))
    observeEvent(input$cs_remove, if (n_custom() > 0) n_custom(n_custom() - 1L))

    # ---- Dynamic UI generators ----
    amendment_row_ui <- function(i, kind) {
      div(
        style = "border:1px solid #e0e8ef; border-radius:4px; padding:8px 10px; margin-bottom:6px;",
        fluidRow(
          column(3, dateInput(ns(paste0(kind, "_date_", i)), "Date", value = Sys.Date())),
          column(6, textAreaInput(ns(paste0(kind, "_desc_", i)), "Description", rows = 2)),
          column(3, selectInput(ns(paste0(kind, "_status_", i)), "Status",
                                 choices = c("Submitted", "Approved", "In preparation", "Withdrawn"),
                                 selected = "Submitted"))
        )
      )
    }

    output$amd_sub_ui <- renderUI({
      n <- n_amd_sub()
      if (n == 0) return(p("No substantial amendments added yet.",
                            style = "color:#6b7c8d; font-style:italic;"))
      lapply(seq_len(n), function(i) amendment_row_ui(i, "sub"))
    })

    output$amd_nonsub_ui <- renderUI({
      n <- n_amd_nonsub()
      if (n == 0) return(p("No non-substantial amendments added yet.",
                            style = "color:#6b7c8d; font-style:italic;"))
      lapply(seq_len(n), function(i) amendment_row_ui(i, "nonsub"))
    })

    output$cs_ui <- renderUI({
      n <- n_custom()
      if (n == 0) return(p("No custom sections added yet.",
                            style = "color:#6b7c8d; font-style:italic;"))
      lapply(seq_len(n), function(i) {
        div(
          style = "border:1px solid #e0e8ef; border-radius:4px; padding:10px 12px; margin-bottom:8px;",
          fluidRow(
            column(5, textInput(ns(paste0("cs_title_", i)),
                                 "Section title",
                                 value = "", placeholder = "e.g. PI Associate Scheme update")),
            column(7, selectInput(ns(paste0("cs_pos_", i)), "Insert at",
                                   choices = c(
                                     "After General Report Information" = "after_general_info",
                                     "After Recruitment Details"        = "after_recruitment",
                                     "After Screening data"             = "after_screening",
                                     "After Safety"                     = "after_safety",
                                     "End of report"                    = "end_of_report"
                                   )))
          ),
          textAreaInput(ns(paste0("cs_content_", i)),
                         "Content", rows = 4, resize = "vertical",
                         placeholder = "Narrative content to include in the report…")
        )
      })
    })

    # ---- Collect amendments into a data frame ----
    collect_amendments <- function(kind) {
      n <- if (kind == "sub") n_amd_sub() else n_amd_nonsub()
      if (n == 0) return(NULL)
      rows <- lapply(seq_len(n), function(i) {
        data.frame(
          Date        = format(input[[paste0(kind, "_date_", i)]],  "%d %b %Y") %||% "",
          Description = input[[paste0(kind, "_desc_", i)]]    %||% "",
          Status      = input[[paste0(kind, "_status_", i)]]  %||% "",
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

    # ---- Download handler ----
    output$generate <- downloadHandler(
      filename = function() {
        stamp <- format(Sys.Date(), "%Y-%m-%d")
        ext   <- if (input$report_type == "TSC") "docx" else "html"
        sprintf("TONIC_%s_Report_%s.%s", input$report_type, stamp, ext)
      },

      content = function(file) {
        showNotification("Generating report…", type = "message", duration = 3)

        # Either use existing filtered rd or re-prepare with no filters
        rd <- report_data_reactive()
        # If "all" requested and app supports it, caller should rebuild rd.
        # For simplicity we note it via the filters passed to the Rmd.

        filters <- current_filters()
        if (input$data_scope == "all") {
          filters$selected_sites <- "All sites"
          filters$date_from <- NULL
          filters$date_to   <- NULL
        }

        if (input$report_type == "TSC") {
          rmd_file <- file.path(rmd_dir, "tsc_report.Rmd")

          # Assemble trial summary
          trial_summary <- list(
            objectives         = input$ts_objectives,
            design             = input$ts_design,
            eligibility        = input$ts_eligibility,
            interventions      = input$ts_interventions,
            primary_outcome    = input$ts_primary_outcome,
            secondary_outcomes = input$ts_secondary_outcomes
          )

          rmarkdown::render(
            input  = rmd_file,
            output_file = file,
            params = list(
              report_data    = rd,
              selected_sites = filters$selected_sites,
              date_from      = filters$date_from,
              date_to        = filters$date_to,
              crf_csv_path   = crf_csv_path(),
              meeting_date   = input$meeting_date,
              prepared_by    = input$prepared_by,
              reviewed_by    = input$reviewed_by,
              protocol_version = input$protocol_version,
              trial_summary  = trial_summary,
              funder_update  = input$funder_update,
              amendments_substantial     = collect_amendments("sub"),
              amendments_non_substantial = collect_amendments("nonsub"),
              custom_sections = collect_custom_sections()
            ),
            envir = new.env(parent = globalenv())
          )
        } else {
          # TMG / iTMG share the same Rmd, just a label difference
          rmd_file <- file.path(rmd_dir, "tonic_report.Rmd")
          rmarkdown::render(
            input  = rmd_file,
            output_file = file,
            params = list(
              report_data    = rd,
              selected_sites = filters$selected_sites,
              date_from      = filters$date_from,
              date_to        = filters$date_to,
              crf_csv_path   = crf_csv_path(),
              logo_path      = logo_path,
              report_type    = input$report_type   # "TMG" or "iTMG"
            ),
            envir = new.env(parent = globalenv())
          )
        }

        showNotification("Report ready — downloading…", type = "message", duration = 3)
      }
    )

  })
}
