# ── postal_tracking_server.R ─────────────────────────────────────────────────
#
# Reads the main REDCap export (passed in as a reactive), builds the postal
# tracking table, renders it with inline edit controls (checkbox + date),
# persists updates to SQLite, and exports an audit-ready XLSX.
#
# Args to postal_tracking_server():
#   id            — namespace ID (e.g. "postal")
#   redcap_data   — reactive returning the main REDCap data frame
#   current_user  — reactive or value returning the current user name
#                   (used to stamp audit records)
#   id_col, op_col, pref_col, site_col, lead_days — passed through to
#                   build_postal_tracking(); sensible defaults.
#
# ─────────────────────────────────────────────────────────────────────────────

postal_tracking_server <- function(id, redcap_data,
                                   current_user = reactive("unknown"),
                                   id_col         = "record_id",
                                   op_col         = "iop_op_end_dt",
                                   pref_col       = "cntct_questionnaires_pref",
                                   site_col       = "site_name",
                                   baseline_event = "baseline_arm_1",
                                   lead_days      = 7) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Each col/event arg may be either a plain string or a reactive expression
    # (so app.R can wire them to the active trial's config). Resolve both.
    .resolve <- function(x) if (is.reactive(x)) x() else x

    # Ensure the table exists
    postal_db_init()

    # Brand colours
    NAVY   <- "#1B4F6B"
    TEAL   <- "#2EC4A5"
    AMBER  <- "#f0a500"
    CORAL  <- "#e05c3a"
    GREY   <- "#adb5bd"
    LTGREY <- "#f4f6f8"

    # Trigger that re-reads the DB after each upsert
    refresh_trigger <- reactiveVal(0)

    # ── Build the master dataset ────────────────────────────────────────────
    postal_data <- reactive({
      refresh_trigger()      # take a dependency so edits refresh the table

      # Diagnostic: write state to a file that persists after reactives run
      rd <- tryCatch(redcap_data(), error = function(e) NULL)
      diag <- list(
        time          = format(Sys.time()),
        is_null       = is.null(rd),
        class         = paste(class(rd), collapse = "/"),
        nrow          = if (is.data.frame(rd)) nrow(rd) else NA,
        ncol          = if (is.data.frame(rd)) ncol(rd) else NA,
        has_pref      = if (is.data.frame(rd)) "cntct_questionnaires_pref" %in% names(rd) else NA,
        has_op        = if (is.data.frame(rd)) "iop_op_end_dt" %in% names(rd) else NA,
        has_event     = if (is.data.frame(rd)) "redcap_event_name" %in% names(rd) else NA
      )
      writeLines(
        paste0(names(diag), ": ", unlist(diag), collapse = "\n"),
        file.path("data", "POSTAL_DIAG.txt")
      )
      message("POSTAL DIAG written to data/POSTAL_DIAG.txt")

      req(redcap_data())

      build_postal_tracking(
        redcap_df      = redcap_data(),
        id_col         = .resolve(id_col),
        op_col         = .resolve(op_col),
        pref_col       = .resolve(pref_col),
        site_col       = .resolve(site_col),
        baseline_event = .resolve(baseline_event),
        lead_days      = lead_days
      )
    })

    # ── Filter for display ──────────────────────────────────────────────────
    display_data <- reactive({
      df <- postal_data()
      req(df)

      # Status filter
      df <- switch(input$status_filter,
        "action" = df %>% dplyr::filter(status %in% c("Overdue", "Due now")),
        "sent"   = df %>% dplyr::filter(status == "Sent"),
        df
      )

      # Timepoint filter
      if (!is.null(input$timepoint_filter) && length(input$timepoint_filter) > 0) {
        df <- df %>% dplyr::filter(timepoint %in% input$timepoint_filter)
      }

      # Search
      q <- trimws(input$search %||% "")
      if (nzchar(q)) {
        q_lower <- tolower(q)
        df <- df %>% dplyr::filter(
          grepl(q_lower, tolower(participant_id), fixed = TRUE) |
          grepl(q_lower, tolower(site_name %||% ""), fixed = TRUE)
        )
      }

      df
    })

    # ── KPI cards ───────────────────────────────────────────────────────────
    output$kpi_cards <- renderUI({
      df <- postal_data()
      req(df)

      overdue  <- sum(df$status == "Overdue",  na.rm = TRUE)
      due_now  <- sum(df$status == "Due now",  na.rm = TRUE)
      upcoming <- sum(df$status == "Upcoming", na.rm = TRUE)
      sent_ct  <- sum(df$status == "Sent",     na.rm = TRUE)

      make_card <- function(label, value, colour, sub = NULL) {
        column(3,
          tags$div(
            style = paste0(
              "border-left: 4px solid ", colour, ";",
              "padding: 12px 16px; margin-bottom: 8px;",
              "background: #f9fafb; border-radius: 4px;"
            ),
            tags$div(
              style = "font-size:0.78rem; color:#6c757d;
                       text-transform:uppercase; letter-spacing:.04em;",
              label
            ),
            tags$div(
              style = paste0("font-size:1.8rem; font-weight:700; color:", colour, ";"),
              value
            ),
            if (!is.null(sub))
              tags$div(style = "font-size:0.8rem; color:#6c757d; margin-top:2px;", sub)
          )
        )
      }

      fluidRow(
        make_card("Overdue",  overdue,  CORAL,  "Past due date, not sent"),
        make_card("Due now",  due_now,  AMBER,  "Within next 7 days"),
        make_card("Upcoming", upcoming, NAVY,   "Within next 3 weeks"),
        make_card("Sent",     sent_ct,  TEAL,   "Total marked as sent")
      )
    })

    # ── Status badge renderer ───────────────────────────────────────────────
    status_badge <- function(status) {
      colour <- switch(status,
        "Overdue"  = CORAL,
        "Due now"  = AMBER,
        "Upcoming" = NAVY,
        "Sent"     = TEAL,
        GREY
      )
      sprintf(
        '<span style="background:%s;color:white;font-weight:600;font-size:0.75rem;padding:3px 10px;border-radius:10px;display:inline-block;">%s</span>',
        colour, status
      )
    }

    # ── Days-to-due display ─────────────────────────────────────────────────
    days_display <- function(days, status) {
      if (is.na(days)) return("—")
      if (status == "Sent") return('<span style="color:#6c757d;">—</span>')
      if (days < 0) {
        sprintf(
          '<span style="color:%s;font-weight:600;">%d day%s overdue</span>',
          CORAL, abs(days), ifelse(abs(days) == 1, "", "s")
        )
      } else if (days == 0) {
        sprintf('<span style="color:%s;font-weight:600;">Due today</span>', AMBER)
      } else {
        sprintf("in %d day%s", days, ifelse(days == 1, "", "s"))
      }
    }

    # ── Main table ──────────────────────────────────────────────────────────
    output$postal_table <- reactable::renderReactable({
      df <- display_data()

      if (is.null(df) || nrow(df) == 0) {
        return(reactable::reactable(
          data.frame(Message = "No participants match the current filters."),
          bordered = FALSE, pagination = FALSE
        ))
      }

      # Add a row identifier we can read in the checkbox handler
      df$row_key <- paste(df$participant_id, df$timepoint, sep = "|")

      reactable::reactable(
        df,
        columns = list(
          row_key        = reactable::colDef(show = FALSE),
          days_post_op   = reactable::colDef(show = FALSE),
          last_modified  = reactable::colDef(show = FALSE),
          modified_by    = reactable::colDef(show = FALSE),

          participant_id = reactable::colDef(
            name     = "Participant",
            minWidth = 110,
            style    = list(fontWeight = "500", color = NAVY)
          ),
          site_name = reactable::colDef(
            name     = "Site",
            minWidth = 200
          ),
          op_date = reactable::colDef(
            name     = "Op date",
            minWidth = 100,
            cell     = function(v) if (is.na(v)) "—" else format(as.Date(v), "%d %b %Y")
          ),
          timepoint = reactable::colDef(
            name     = "Timepoint",
            minWidth = 90,
            align    = "center"
          ),
          due_date = reactable::colDef(
            name     = "Due",
            minWidth = 110,
            cell     = function(v) if (is.na(v)) "—" else format(as.Date(v), "%d %b %Y")
          ),
          days_to_due = reactable::colDef(
            name     = "When",
            minWidth = 140,
            html     = TRUE,
            cell     = function(v, idx) days_display(v, df$status[idx])
          ),
          status = reactable::colDef(
            name     = "Status",
            minWidth = 100,
            align    = "center",
            html     = TRUE,
            cell     = function(v) status_badge(v)
          ),
          sent = reactable::colDef(
            name     = "Sent?",
            minWidth = 70,
            align    = "center",
            html     = TRUE,
            cell     = function(v, idx) {
              rk <- df$row_key[idx]
              checked <- if (!is.na(v) && v == 1) "checked" else ""
              sprintf(
                '<input type="checkbox" %s
                   onchange="Shiny.setInputValue(\'%s\',
                     {row: \'%s\', sent: this.checked, nonce: Math.random()},
                     {priority: \'event\'})"
                   style="width:18px;height:18px;cursor:pointer;accent-color:%s;"/>',
                checked, ns("toggle_sent"), rk, TEAL
              )
            }
          ),
          date_sent = reactable::colDef(
            name     = "Date sent",
            minWidth = 130,
            html     = TRUE,
            cell     = function(v, idx) {
              rk    <- df$row_key[idx]
              value <- if (is.na(v) || v == "") "" else substr(v, 1, 10)
              sprintf(
                '<input type="date" value="%s"
                   onchange="Shiny.setInputValue(\'%s\',
                     {row: \'%s\', date: this.value, nonce: Math.random()},
                     {priority: \'event\'})"
                   style="font-size:0.8rem; padding:2px 4px;
                          border:1px solid #ced4da; border-radius:4px; width:100%%;"/>',
                value, ns("update_date"), rk
              )
            }
          ),
          notes = reactable::colDef(
            name     = "Notes",
            minWidth = 160,
            html     = TRUE,
            cell     = function(v, idx) {
              rk    <- df$row_key[idx]
              value <- if (is.na(v)) "" else gsub('"', '&quot;', v)
              sprintf(
                '<input type="text" value="%s" placeholder="Add note..."
                   onchange="Shiny.setInputValue(\'%s\',
                     {row: \'%s\', notes: this.value, nonce: Math.random()},
                     {priority: \'event\'})"
                   style="font-size:0.8rem; padding:2px 6px; width:100%%;
                          border:1px solid #ced4da; border-radius:4px;"/>',
                value, ns("update_notes"), rk
              )
            }
          )
        ),
        columnGroups = list(
          reactable::colGroup(name = "Participant",
            columns = c("participant_id", "site_name", "op_date")),
          reactable::colGroup(name = "Questionnaire",
            columns = c("timepoint", "due_date", "days_to_due", "status")),
          reactable::colGroup(name = "Action log",
            columns = c("sent", "date_sent", "notes"))
        ),
        bordered        = FALSE,
        striped         = TRUE,
        highlight       = TRUE,
        compact         = TRUE,
        pagination      = TRUE,
        defaultPageSize = 20,
        theme = reactable::reactableTheme(
          headerStyle = list(
            background   = LTGREY,
            color        = NAVY,
            fontWeight   = "600",
            fontSize     = "0.82rem",
            borderBottom = paste0("2px solid ", NAVY)
          ),
          cellStyle = list(verticalAlign = "middle", padding = "6px 8px"),
          rowStyle  = list(borderBottom = "1px solid #e9ecef")
        )
      )
    })

    # ── Edit handlers ───────────────────────────────────────────────────────

    # Unpack row_key helper
    split_row_key <- function(rk) {
      parts <- strsplit(rk, "|", fixed = TRUE)[[1]]
      list(participant_id = parts[1], timepoint = parts[2])
    }

    # Get existing record (so partial updates don't wipe other fields)
    get_existing <- function(pid, tp) {
      all <- postal_db_read_all()
      row <- all[all$participant_id == pid & all$timepoint == tp, , drop = FALSE]
      if (nrow(row) == 0) {
        list(sent = 0L, date_sent = NA_character_, notes = NA_character_)
      } else {
        list(
          sent      = as.integer(row$sent[1]),
          date_sent = row$date_sent[1],
          notes     = row$notes[1]
        )
      }
    }

    # Toggle sent checkbox
    observeEvent(input$toggle_sent, {
      ev    <- input$toggle_sent
      keys  <- split_row_key(ev$row)
      exist <- get_existing(keys$participant_id, keys$timepoint)

      # Auto-stamp today's date when ticking, if no date is set
      new_date <- exist$date_sent
      if (isTRUE(ev$sent) && (is.na(new_date) || new_date == "")) {
        new_date <- as.character(Sys.Date())
      }

      postal_db_upsert(
        participant_id = keys$participant_id,
        timepoint      = keys$timepoint,
        sent           = ev$sent,
        date_sent      = new_date,
        notes          = exist$notes,
        modified_by    = current_user()
      )
      refresh_trigger(refresh_trigger() + 1)
    }, ignoreInit = TRUE)

    # Update date_sent
    observeEvent(input$update_date, {
      ev    <- input$update_date
      keys  <- split_row_key(ev$row)
      exist <- get_existing(keys$participant_id, keys$timepoint)

      postal_db_upsert(
        participant_id = keys$participant_id,
        timepoint      = keys$timepoint,
        sent           = if (!is.na(exist$sent) && exist$sent == 1) 1 else
                         (nzchar(ev$date) && !is.na(ev$date)),
        date_sent      = ev$date,
        notes          = exist$notes,
        modified_by    = current_user()
      )
      refresh_trigger(refresh_trigger() + 1)
    }, ignoreInit = TRUE)

    # Update notes
    observeEvent(input$update_notes, {
      ev    <- input$update_notes
      keys  <- split_row_key(ev$row)
      exist <- get_existing(keys$participant_id, keys$timepoint)

      postal_db_upsert(
        participant_id = keys$participant_id,
        timepoint      = keys$timepoint,
        sent           = exist$sent,
        date_sent      = exist$date_sent,
        notes          = ev$notes,
        modified_by    = current_user()
      )
      refresh_trigger(refresh_trigger() + 1)
    }, ignoreInit = TRUE)

    # ── XLSX export ─────────────────────────────────────────────────────────
    output$export_xlsx <- downloadHandler(
      filename = function() {
        paste0("TONIC_postal_audit_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        df <- postal_data()

        # ── Sanitisers ─────────────────────────────────────────────────────
        # Strip XML-illegal control characters that openxlsx will pass
        # through verbatim, breaking the resulting workbook. The XML 1.0
        # spec only allows: \t (0x09), \n (0x0A), \r (0x0D) and chars
        # >= 0x20 (apart from a couple of high-Unicode exclusions which
        # never occur in normal text). Everything else (NULs, vertical
        # tabs, form feeds, bell, etc.) must be removed.
        sanitise_text <- function(x) {
          if (is.null(x)) return(NA_character_)
          x <- as.character(x)
          # Remove illegal XML control chars (NUL is excluded here because
          # R strings cannot contain literal NUL bytes anyway — if a NUL
          # somehow reaches us it's already been truncated by R's string
          # type. We strip the rest: 0x01-0x08, 0x0B, 0x0C, 0x0E-0x1F).
          x <- gsub("[\x01-\x08\x0B\x0C\x0E-\x1F]", "", x, perl = TRUE)
          # Collapse \r\n and stray \r into \n so Excel renders
          # multi-line notes consistently
          x <- gsub("\r\n", "\n", x, fixed = TRUE)
          x <- gsub("\r",   "\n", x, fixed = TRUE)
          # Trim leading/trailing whitespace
          x <- trimws(x)
          # Convert empty strings to NA so Excel shows a blank cell
          x[!nzchar(x)] <- NA_character_
          x
        }

        # Format dates as ISO strings explicitly so openxlsx doesn't
        # try to interpret POSIX/Date objects in a way that bloats XML
        # with timezone offsets etc.
        fmt_date <- function(x) {
          if (is.null(x)) return(NA_character_)
          d <- suppressWarnings(as.Date(x))
          ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
        }

        export_df <- df %>%
          dplyr::transmute(
            `Participant ID` = sanitise_text(participant_id),
            `Site`           = sanitise_text(site_name),
            `Op date`        = fmt_date(op_date),
            `Timepoint`      = sanitise_text(timepoint),
            `Due date`       = fmt_date(due_date),
            `Days to due`    = suppressWarnings(as.integer(days_to_due)),
            `Status`         = sanitise_text(status),
            `Sent`           = dplyr::if_else(!is.na(sent) & sent == 1, "Yes", "No"),
            `Date sent`      = fmt_date(date_sent),
            `Notes`          = sanitise_text(notes),
            `Last modified`  = sanitise_text(last_modified),
            `Modified by`    = sanitise_text(modified_by)
          )

        # Coerce to a plain data.frame to avoid any tibble surprises
        export_df <- as.data.frame(export_df, stringsAsFactors = FALSE)

        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Postal audit log")
        openxlsx::writeData(wb, "Postal audit log", export_df,
                            headerStyle = openxlsx::createStyle(
                              textDecoration = "bold",
                              fontColour     = "#FFFFFF",
                              fgFill         = "#1B4F6B",
                              halign         = "center"
                            ))
        openxlsx::setColWidths(wb, "Postal audit log",
                               cols = 1:ncol(export_df), widths = "auto")
        openxlsx::freezePane(wb, "Postal audit log", firstRow = TRUE)

        # Conditional formatting on Status column
        status_col <- which(names(export_df) == "Status")
        n_rows     <- nrow(export_df) + 1

        if (length(status_col) > 0 && n_rows > 1) {
          openxlsx::conditionalFormatting(wb, "Postal audit log",
            cols = status_col, rows = 2:n_rows,
            rule = '"Overdue"', type = "contains",
            style = openxlsx::createStyle(fgFill = "#F8D7DA", fontColour = "#7D1E27"))
          openxlsx::conditionalFormatting(wb, "Postal audit log",
            cols = status_col, rows = 2:n_rows,
            rule = '"Due now"', type = "contains",
            style = openxlsx::createStyle(fgFill = "#FFF3CD", fontColour = "#7A5A00"))
          openxlsx::conditionalFormatting(wb, "Postal audit log",
            cols = status_col, rows = 2:n_rows,
            rule = '"Sent"', type = "contains",
            style = openxlsx::createStyle(fgFill = "#D4F4E9", fontColour = "#0d4037"))
        }

        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

    # ── Source footer ───────────────────────────────────────────────────────
    output$source_info <- renderUI({
      df <- postal_data()
      req(df)

      total_postal <- length(unique(df$participant_id))
      tags$span(
        tags$span(style = "font-weight:500;",
                  "Postal-preference participants: "),
        total_postal,
        tags$span(style = "margin-left: 12px;", "•"),
        tags$span(style = "margin-left: 12px;",
                  paste0("Data refreshed: ", format(Sys.time(), "%d %b %Y, %H:%M")))
      )
    })

  })
}

# Small null-coalesce helper (if not already defined in your helpers)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
