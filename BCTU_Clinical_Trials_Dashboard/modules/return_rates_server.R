# ── return_rates_server.R ─────────────────────────────────────────────────────
#
# Reads the return-rate CSV produced by the REDCap export pipeline.
# CSV columns expected:
#   Site, Event, Form, Expected, Due, Entered, "% Due Entered", "% Expected Entered"
#
# .Overall rows drive the top summary cards and the overall heatmap.
# All other Site rows drive the per-site accordion panels.
#
# Colour thresholds (% rate):
#   >= 90  → teal  (#2EC4A5)  — on target
#   >= 70  → amber (#f0a500)  — caution
#   >= 1   → coral (#e05c3a)  — below target
#   NA / 0 expected → grey   (#adb5bd)  — not yet due / not applicable
#
# ─────────────────────────────────────────────────────────────────────────────

return_rates_server <- function(id, rr_data) {
  # rr_data: reactive returning the loaded CSV as a data frame
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Colour helpers ───────────────────────────────────────────────────────
    TONIC_TEAL   <- "#2EC4A5"
    TONIC_NAVY   <- "#1B4F6B"
    TONIC_AMBER  <- "#f0a500"
    TONIC_CORAL  <- "#e05c3a"
    TONIC_GREY   <- "#adb5bd"
    TONIC_LTGREY <- "#f4f6f8"
    
    # Recompute pct as min(entered, due) / due so early entries (entered before
    # the due window officially opens) never produce rates above 100%.
    # Once those participants' windows open, due increments and they count normally.
    safe_pct_due <- function(entered, due) {
      dplyr::if_else(due > 0, pmin(entered, due) / due * 100, NA_real_)
    }
    
    rate_colour <- function(pct) {
      dplyr::case_when(
        is.na(pct)   ~ TONIC_GREY,
        pct >= 90    ~ TONIC_TEAL,
        pct >= 70    ~ TONIC_AMBER,
        pct >= 1     ~ TONIC_CORAL,
        TRUE         ~ TONIC_GREY
      )
    }
    
    text_colour <- function(bg) {
      # readable text colour on each badge background
      dplyr::case_when(
        bg == TONIC_TEAL  ~ "#0d4037",
        bg == TONIC_AMBER ~ "#4d3100",
        bg == TONIC_CORAL ~ "#5a1a08",
        TRUE              ~ "#5a5f65"
      )
    }
    
    # ── Tidy the incoming data ───────────────────────────────────────────────
    tidy_rr <- reactive({
      req(rr_data())
      df <- rr_data()
      
      # Robust column rename regardless of how the CSV arrives
      names(df) <- c("site", "event", "form",
                     "expected", "due", "entered",
                     "pct_due", "pct_expected")
      
      df <- df %>%
        dplyr::mutate(
          expected     = as.integer(expected),
          due          = as.integer(due),
          entered      = as.integer(entered),
          # Recompute from raw counts so early entries (entered before the due
          # window opens) never inflate the rate above 100%.
          # Formula: min(entered, due) / due — early entries are capped at due.
          pct_due      = safe_pct_due(entered, due),
          pct_expected = suppressWarnings(as.numeric(pct_expected)),
          event        = factor(event,
                                levels = c("Baseline", "Discharge", "Day 30", "Day 90"))
        )
      df
    })
    
    # ── Active rate column ────────────────────────────────────────────────────
    # Return rates are ALWAYS calculated against forms *due* in the period, never
    # against total *expected*. TONIC is a longitudinal study with four
    # timepoints (Baseline, Discharge, Day 30, Day 90); "expected" counts every
    # participant who will *eventually* reach a timepoint, including windows that
    # have not yet opened (e.g. Day 90 with 0 due). Counting those in the
    # denominator deflates the rate, so we only ever divide by forms due now.
    active_rate <- reactive({
      "pct_due"
    })
    
    # ── Filter timepoints ────────────────────────────────────────────────────
    filtered_data <- reactive({
      req(input$selected_timepoints)
      tidy_rr() %>%
        dplyr::filter(event %in% input$selected_timepoints)
    })
    
    # ────────────────────────────────────────────────────────────────────────
    # Helper: build one heatmap table for a given site (character string)
    # Returns a reactable widget
    # ────────────────────────────────────────────────────────────────────────
    build_heatmap <- function(site_name, df, rate_col) {
      
      site_df <- df %>%
        dplyr::filter(site == site_name) %>%
        dplyr::select(form, event, all_of(rate_col), expected, due, entered)
      
      # Pivot: rows = forms, cols = timepoints
      wide <- site_df %>%
        dplyr::select(form, event, rate = all_of(rate_col),
                      expected, due, entered) %>%
        tidyr::pivot_wider(
          names_from  = event,
          values_from = c(rate, expected, due, entered),
          names_glue  = "{event}__{.value}"
        )
      
      timepoints <- intersect(
        c("Baseline", "Discharge", "Day 30", "Day 90"),
        input$selected_timepoints
      )
      
      # Build reactable column definitions dynamically
      tp_cols <- purrr::map(timepoints, function(tp) {
        
        rate_col_name <- paste0(tp, "__rate")
        exp_col_name  <- paste0(tp, "__expected")
        due_col_name  <- paste0(tp, "__due")
        ent_col_name  <- paste0(tp, "__entered")
        
        reactable::colDef(
          name   = tp,
          width  = 110,
          align  = "center",
          cell   = function(value, index) {
            row       <- wide[index, ]
            exp_val   <- row[[exp_col_name]]
            due_val   <- row[[due_col_name]]
            ent_val   <- row[[ent_col_name]]
            
            # Fallback for missing cells (timepoint not in this site's data)
            if (is.null(exp_val) || length(exp_val) == 0 ||
                is.na(exp_val)   || exp_val == 0) {
              return(
                tags$div(
                  style = paste0(
                    "background:", TONIC_GREY, "; color:#5a5f65;",
                    "border-radius:6px; padding:4px 6px;",
                    "font-size:0.8rem; text-align:center;"
                  ),
                  "N/A"
                )
              )
            }
            
            pct   <- if (is.null(value) || is.na(value)) NA_real_ else value
            bg    <- rate_colour(pct)
            fg    <- text_colour(bg)
            label <- if (is.na(pct)) "—" else paste0(round(pct, 0), "%")
            sub   <- paste0(ent_val, " / ", due_val, " due")
            
            tags$div(
              style = paste0(
                "background:", bg, "; color:", fg, ";",
                "border-radius:6px; padding:4px 6px;",
                "font-size:0.8rem; text-align:center; line-height:1.35;"
              ),
              tags$div(style = "font-weight:600; font-size:0.9rem;", label),
              tags$div(style = "font-size:0.72rem; opacity:0.85;", sub)
            )
          },
          style = function(value) list(padding = "4px")
        )
      }) %>% purrr::set_names(purrr::map_chr(timepoints, ~ paste0(.x, "__rate")))
      
      col_list <- c(
        list(
          form = reactable::colDef(
            name    = "Form",
            minWidth = 160,
            style   = list(fontWeight = "500", color = TONIC_NAVY)
          )
        ),
        tp_cols
      )
      
      # Keep only columns that exist in wide
      wide_display <- wide %>%
        dplyr::select(form, dplyr::any_of(names(col_list) %>% setdiff("form")),
                      dplyr::everything()) %>%
        dplyr::select(form, dplyr::any_of(
          c("Baseline__rate", "Discharge__rate", "Day 30__rate", "Day 90__rate")
        ))
      
      col_list <- col_list[names(col_list) %in% c("form", names(wide_display))]
      
      reactable::reactable(
        wide_display,
        columns          = col_list,
        bordered         = FALSE,
        striped          = FALSE,
        highlight        = TRUE,
        compact          = TRUE,
        pagination       = FALSE,
        defaultPageSize  = 50,
        theme = reactable::reactableTheme(
          headerStyle      = list(
            background = TONIC_LTGREY,
            color      = TONIC_NAVY,
            fontWeight = "600",
            fontSize   = "0.82rem",
            borderBottom = paste0("2px solid ", TONIC_NAVY)
          ),
          cellStyle        = list(verticalAlign = "middle"),
          rowStyle         = list(borderBottom = "1px solid #e9ecef")
        )
      )
    }
    
    # ────────────────────────────────────────────────────────────────────────
    # Top summary cards — one per timepoint, showing overall % entered
    # ────────────────────────────────────────────────────────────────────────
    output$summary_cards <- renderUI({
      req(filtered_data())
      
      overall <- filtered_data() %>%
        dplyr::filter(site == ".Overall") %>%
        dplyr::group_by(event) %>%
        dplyr::summarise(
          total_expected = sum(expected, na.rm = TRUE),
          total_due      = sum(due,      na.rm = TRUE),
          total_entered  = sum(entered,  na.rm = TRUE),
          .groups        = "drop"
        ) %>%
        dplyr::mutate(
          # Denominator is forms DUE. Numerator is capped at total_due so that
          # early entries (entered before window opens) cannot exceed 100%.
          pct_due      = dplyr::if_else(total_due > 0,
                                        pmin(total_entered, total_due) / total_due * 100,
                                        NA_real_),
          pct_expected = dplyr::if_else(total_expected > 0,
                                        total_entered / total_expected * 100, NA_real_),
          rate         = pct_due
        )
      
      timepoints <- c("Baseline", "Discharge", "Day 30", "Day 90")
      
      cards <- purrr::map(timepoints, function(tp) {
        row <- dplyr::filter(overall, event == tp)
        
        if (nrow(row) == 0 || !(tp %in% input$selected_timepoints)) {
          return(column(3,
                        tags$div(
                          class = "card",
                          style = paste0(
                            "border-left: 4px solid ", TONIC_GREY, ";",
                            "padding: 12px 16px; margin-bottom: 8px;",
                            "background: #f9fafb;"
                          ),
                          tags$div(style = paste0("font-size:0.78rem; color:#868e96;",
                                                  "text-transform:uppercase; letter-spacing:.04em;"),
                                   tp),
                          tags$div(style = "font-size:1.5rem; font-weight:700; color:#adb5bd;", "—")
                        )
          ))
        }
        
        pct  <- row$rate
        bg   <- rate_colour(pct)
        pct_label <- if (is.na(pct)) "Not yet due" else paste0(round(pct, 1), "%")
        sub_label <- paste0(row$total_entered, " / ", row$total_due, " due")
        
        column(3,
               tags$div(
                 style = paste0(
                   "border-left: 4px solid ", bg, ";",
                   "padding: 12px 16px; margin-bottom: 8px;",
                   "background: #f9fafb; border-radius: 4px;"
                 ),
                 tags$div(
                   style = paste0("font-size:0.78rem; color:#6c757d;",
                                  "text-transform:uppercase; letter-spacing:.04em;"),
                   tp
                 ),
                 tags$div(
                   style = paste0("font-size:1.6rem; font-weight:700; color:", bg, ";"),
                   pct_label
                 ),
                 tags$div(
                   style = "font-size:0.8rem; color:#6c757d; margin-top:2px;",
                   sub_label
                 )
               )
        )
      })
      
      fluidRow(cards)
    })
    
    # ────────────────────────────────────────────────────────────────────────
    # Overall heatmap (all sites combined)
    # ────────────────────────────────────────────────────────────────────────
    output$overall_heatmap <- renderUI({
      req(filtered_data())
      build_heatmap(".Overall", filtered_data(), active_rate())
    })
    
    # ────────────────────────────────────────────────────────────────────────
    # Per-site accordion panels
    # ────────────────────────────────────────────────────────────────────────
    sites_list <- reactive({
      req(filtered_data())
      filtered_data() %>%
        dplyr::filter(site != ".Overall") %>%
        dplyr::pull(site) %>%
        unique() %>%
        sort()
    })
    
    # Track open/closed state for each site
    open_sites <- reactiveVal(character(0))
    
    observeEvent(input$expand_all, {
      if (length(open_sites()) == length(sites_list())) {
        open_sites(character(0))          # collapse all
        updateActionButton(session, "expand_all", label = "Expand all sites")
      } else {
        open_sites(sites_list())          # expand all
        updateActionButton(session, "expand_all", label = "Collapse all sites")
      }
    })
    
    # Individual site toggle
    observe({
      purrr::walk(sites_list(), function(s) {
        btn_id <- paste0("toggle_", gsub("[^A-Za-z0-9]", "_", s))
        observeEvent(input[[btn_id]], {
          current <- open_sites()
          if (s %in% current) {
            open_sites(setdiff(current, s))
          } else {
            open_sites(c(current, s))
          }
        }, ignoreInit = TRUE)
      })
    })
    
    output$site_panels <- renderUI({
      req(sites_list(), filtered_data())
      
      panels <- purrr::map(sites_list(), function(s) {
        btn_id   <- paste0("toggle_", gsub("[^A-Za-z0-9]", "_", s))
        is_open  <- s %in% open_sites()
        
        # Quick summary for the header: overall % for this site
        site_summary <- filtered_data() %>%
          dplyr::filter(site == s) %>%
          dplyr::summarise(
            total_exp  = sum(expected, na.rm = TRUE),
            total_due  = sum(due,      na.rm = TRUE),
            total_ent  = sum(entered,  na.rm = TRUE),
            .groups    = "drop"
          ) %>%
          dplyr::mutate(
            # Site headline: min(entered, due) / due — capped so early entries
            # cannot push the rate above 100%.
            pct = dplyr::if_else(
              total_due > 0,
              pmin(total_ent, total_due) / total_due * 100,
              NA_real_
            )
          )
        
        pct_val   <- site_summary$pct
        bg_col    <- rate_colour(pct_val)
        pct_label <- if (is.na(pct_val)) "—" else paste0(round(pct_val, 0), "%")
        
        tags$div(
          style = "margin-bottom: 8px;",
          
          # Header row -------------------------------------------------------
          tags$div(
            style = paste0(
              "display:flex; align-items:center; justify-content:space-between;",
              "padding: 10px 14px;",
              "background: ", if (is_open) "#EEF3F7" else "#f4f6f8", ";",
              "border-radius: 6px;",
              "border-left: 4px solid ", TONIC_NAVY, ";",
              "cursor: pointer;"
            ),
            onclick = sprintf(
              "Shiny.setInputValue('%s', Math.random())",
              ns(btn_id)
            ),
            
            tags$span(
              style = paste0("font-weight:600; color:", TONIC_NAVY, ";"),
              s
            ),
            tags$div(
              style = "display:flex; align-items:center; gap:10px;",
              tags$span(
                style = paste0(
                  "background:", bg_col, ";",
                  "color:", text_colour(bg_col), ";",
                  "font-weight:700; font-size:0.85rem;",
                  "padding:3px 10px; border-radius:12px;"
                ),
                pct_label
              ),
              tags$span(
                style = "color:#6c757d; font-size:1rem;",
                if (is_open) "▲" else "▼"
              )
            )
          ),
          
          # Body (heatmap) — only rendered when open --------------------------
          if (is_open) {
            tags$div(
              style = "padding: 12px 4px 4px;",
              build_heatmap(s, filtered_data(), active_rate())
            )
          }
        )
      })
      
      tagList(panels)
    })
    
    # ────────────────────────────────────────────────────────────────────────
    # Source footer — shows which CSV file the dashboard is currently using
    # ────────────────────────────────────────────────────────────────────────
    output$source_info <- renderUI({
      df <- rr_data()
      req(df)
      
      src   <- attr(df, "source_file")
      mtime <- attr(df, "file_mtime")
      
      if (is.null(src)) return(NULL)
      
      mtime_txt <- if (!is.null(mtime)) {
        format(mtime, "%d %b %Y, %H:%M")
      } else {
        "unknown"
      }
      
      tags$span(
        tags$span(style = "font-weight:500;", "Source: "),
        src,
        tags$span(style = "margin-left: 12px;", "•"),
        tags$span(style = "margin-left: 12px;", paste0("Exported: ", mtime_txt))
      )
    })
    
  })
}