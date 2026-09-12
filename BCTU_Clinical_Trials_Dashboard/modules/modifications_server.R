# =============================================================================
# Modifications tab — Server
# CRUD over cfg$modifications (a list of named lists), persisted via
# update_overrides(). Each item:
#   id, ref, category, title, description, documents (chr vec),
#   date_prepared, date_submitted, date_approved, date_implemented,
#   iras_ref, rec_ref, mhra_ref, status, notes
# =============================================================================

modifications_tab_server <- function(input, output, session, state) {
  rv <- state$rv

  selected_id <- reactiveVal(NULL)

  # ── Helpers ────────────────────────────────────────────────────────────────
  mods_list <- reactive({
    cfg <- rv$trial_config
    if (is.null(cfg)) return(list())
    cfg$modifications %||% list()
  })

  next_ref <- function(items) {
    n <- length(items)
    sprintf("MOD-%03d", n + 1L)
  }

  category_label <- function(code) {
    nm <- names(MOD_CATEGORIES)[MOD_CATEGORIES == code]
    if (length(nm) == 0) code else nm
  }

  category_pill_class <- function(code) {
    switch(code %||% "",
      non_substantial  = "pi",
      important_detail = "po",
      substantial_a    = "pc",
      substantial_b    = "ps",
      substantial_c    = "po",
      "pi")
  }

  status_pill_class <- function(s) {
    switch(s %||% "",
      "Approved" = , "Approved with conditions" = , "Implemented" = "pr",
      "Submitted" = , "Under review" = ,
      "Awaiting further information (RFI)" = , "Locked for submission" = "ps",
      "Rejected / unfavourable" = , "Rejected" = , "Withdrawn" = "pc",
      "pi")
  }

  empty_form <- function() {
    items <- mods_list()
    updateTextInput(session,     "mod_ref",         value = next_ref(items))
    updatePickerInput(session,   "mod_category",    selected = "non_substantial")
    updateTextInput(session,     "mod_title",       value = "")
    updateTextAreaInput(session, "mod_description", value = "")
    updatePickerInput(session,   "mod_change_types",  selected = character(0))
    updatePickerInput(session,   "mod_review_bodies", selected = character(0))
    updatePickerInput(session,   "mod_documents",     selected = character(0))
    updateDateInput(session,     "mod_date_prepared",    value = Sys.Date())
    updateDateInput(session,     "mod_date_submitted",   value = NULL)
    updateDateInput(session,     "mod_date_rfi",         value = NULL)
    updateDateInput(session,     "mod_date_approved",    value = NULL)
    updateDateInput(session,     "mod_date_implemented", value = NULL)
    updateTextInput(session,     "mod_iras_ref",                 value = "")
    updateTextInput(session,     "mod_rec_ref",                  value = "")
    updateTextInput(session,     "mod_mhra_ref",                 value = "")
    updateTextInput(session,     "mod_sponsor_authorised_by",    value = "")
    updatePickerInput(session,   "mod_status",      selected = "Draft")
    updateTextAreaInput(session, "mod_notes",       value = "")
    selected_id(NULL)
  }

  fill_form_from <- function(item) {
    updateTextInput(session,     "mod_ref",         value = item$ref %||% "")
    updatePickerInput(session,   "mod_category",    selected = item$category %||% "non_substantial")
    updateTextInput(session,     "mod_title",       value = item$title %||% "")
    updateTextAreaInput(session, "mod_description", value = item$description %||% "")
    updatePickerInput(session,   "mod_change_types",  selected = item$change_types  %||% character(0))
    updatePickerInput(session,   "mod_review_bodies", selected = item$review_bodies %||% character(0))
    updatePickerInput(session,   "mod_documents",     selected = item$documents     %||% character(0))
    safe_date <- function(x) {
      if (is.null(x) || length(x) == 0) return(NULL)
      xc <- as.character(x)
      if (is.na(xc) || !nzchar(xc) || identical(xc, "NA")) NULL else as.Date(x)
    }
    updateDateInput(session,     "mod_date_prepared",    value = safe_date(item$date_prepared))
    updateDateInput(session,     "mod_date_submitted",   value = safe_date(item$date_submitted))
    updateDateInput(session,     "mod_date_rfi",         value = safe_date(item$date_rfi))
    updateDateInput(session,     "mod_date_approved",    value = safe_date(item$date_approved))
    updateDateInput(session,     "mod_date_implemented", value = safe_date(item$date_implemented))
    updateTextInput(session,     "mod_iras_ref",              value = item$iras_ref %||% "")
    updateTextInput(session,     "mod_rec_ref",               value = item$rec_ref %||% "")
    updateTextInput(session,     "mod_mhra_ref",              value = item$mhra_ref %||% "")
    updateTextInput(session,     "mod_sponsor_authorised_by", value = item$sponsor_authorised_by %||% "")
    updatePickerInput(session,   "mod_status",      selected = item$status %||% "Draft")
    updateTextAreaInput(session, "mod_notes",       value = item$notes %||% "")
  }

  read_form <- function() {
    safe_d <- function(x) {
      if (is.null(x) || length(x) == 0) return("")
      xc <- as.character(x)
      if (is.na(xc) || !nzchar(xc) || identical(xc, "NA")) "" else as.character(as.Date(x))
    }
    list(
      ref              = trimws(input$mod_ref %||% ""),
      category         = input$mod_category %||% "non_substantial",
      title            = trimws(input$mod_title %||% ""),
      description      = input$mod_description %||% "",
      change_types     = input$mod_change_types  %||% character(0),
      review_bodies    = input$mod_review_bodies %||% character(0),
      documents        = input$mod_documents %||% character(0),
      date_prepared    = safe_d(input$mod_date_prepared),
      date_submitted   = safe_d(input$mod_date_submitted),
      date_rfi         = safe_d(input$mod_date_rfi),
      date_approved    = safe_d(input$mod_date_approved),
      date_implemented = safe_d(input$mod_date_implemented),
      iras_ref               = trimws(input$mod_iras_ref %||% ""),
      rec_ref                = trimws(input$mod_rec_ref %||% ""),
      mhra_ref               = trimws(input$mod_mhra_ref %||% ""),
      sponsor_authorised_by  = trimws(input$mod_sponsor_authorised_by %||% ""),
      status                 = input$mod_status %||% "Draft",
      notes                  = input$mod_notes %||% ""
    )
  }

  persist <- function(items) {
    cfg <- rv$trial_config
    if (is.null(cfg)) return(invisible())
    tryCatch(
      update_overrides(cfg, modifications = items),
      error = function(e) {
        showNotification(paste("Could not save modifications:", e$message),
                         type = "error", duration = 8)
      })
    rv$trial_config$modifications <- items
    if (exists("log_activity"))
      tryCatch(log_activity("modifications_saved",
                            sprintf("%d items", length(items))),
               error = function(e) NULL)
  }

  # Pre-populate the reference on first entry into the tab
  observe({
    req(rv$trial_config)
    if (!nzchar(input$mod_ref %||% "")) {
      updateTextInput(session, "mod_ref", value = next_ref(mods_list()))
    }
  })

  # Category colour map (mirrors design's MD_CATEGORIES palette)
  CAT_META <- list(
    non_substantial  = list(short = "Minor",     color = "#8A8A8C"),
    important_detail = list(short = "Important", color = "#2581C4"),
    substantial_a    = list(short = "Cat A",     color = "#F07F3C"),
    substantial_b    = list(short = "Cat B",     color = "#EA580C"),
    substantial_c    = list(short = "Cat C",     color = "#E30513")
  )
  STATUS_COLOR <- list(
    "Draft" = "#8A8A8C", "Locked for submission" = "#58595B",
    "Submitted" = "#2581C4", "Under review" = "#C59A00",
    "Awaiting further information (RFI)" = "#F07F3C",
    "Approved" = "#3AAA35", "Approved with conditions" = "#00788E",
    "Implemented" = "#007838", "Rejected / unfavourable" = "#E30513",
    "Withdrawn" = "#6B7280"
  )

  # ── Summary strip ──────────────────────────────────────────────────────────
  output$mod_summary_strip <- renderUI({
    items <- mods_list()
    # MOD_CATEGORIES is a named vector (label = code); we want counts keyed
    # by the *code* so chip("substantial_a") etc. can look itself up.
    cat_counts <- setNames(
      vapply(MOD_CATEGORIES, function(code)
        sum(vapply(items, function(x) identical(x$category, code), logical(1))),
        integer(1)),
      MOD_CATEGORIES
    )
    status_counts <- function(s) {
      sum(vapply(items, function(x) identical(x$status, s), logical(1)))
    }
    cat_filter <- input$mod_cat_filter %||% "all"
    status_filter <- input$mod_status_filter %||% "all"

    chip <- function(code) {
      meta <- CAT_META[[code]]
      on <- identical(cat_filter, code)
      tags$button(
        class = paste("md-cat-chip", if (on) "on" else ""),
        type  = "button",
        style = if (on) sprintf("background:%s;border-color:%s;color:#fff;", meta$color, meta$color)
                else    sprintf("border-color:%s55;", meta$color),
        onclick = sprintf(
          "Shiny.setInputValue('mod_cat_filter','%s',{priority:'event'})",
          if (on) "all" else code),
        tags$span(class = "md-cat-chip-dot", style = sprintf("background:%s;", meta$color)),
        tags$span(meta$short),
        tags$span(class = "md-cat-chip-n", cat_counts[[code]] %||% 0L)
      )
    }
    pipe_step <- function(s) {
      n <- status_counts(s)
      col <- STATUS_COLOR[[s]] %||% "#8A8A8C"
      on <- identical(status_filter, s)
      tags$button(
        class = paste("md-pipe-step", if (on) "on" else ""),
        type  = "button",
        onclick = sprintf(
          "Shiny.setInputValue('mod_status_filter_set','%s',{priority:'event'})",
          if (on) "all" else s),
        div(class = "md-pipe-step-v", style = sprintf("color:%s;", col), n),
        div(class = "md-pipe-step-l", s)
      )
    }

    div(class = "md-summary",
      div(class = "md-summary-left",
        div(class = "md-summary-total",
          div(class = "md-summary-total-v", length(items)),
          div(class = "md-summary-total-l", "Total modifications")
        ),
        div(class = "md-cat-chips",
          chip("non_substantial"), chip("important_detail"),
          chip("substantial_a"),   chip("substantial_b"), chip("substantial_c")
        )
      ),
      div(class = "md-pipeline",
        pipe_step("Draft"), pipe_step("Submitted"),
        pipe_step("Under review"), pipe_step("Approved"),
        pipe_step("Implemented")
      )
    )
  })

  # Pipeline step buttons set the status filter (kept separate from the
  # selectInput so a chip click can sync the dropdown).
  observeEvent(input$mod_status_filter_set, {
    updateSelectInput(session, "mod_status_filter",
                      selected = input$mod_status_filter_set)
  }, ignoreInit = TRUE)

  # ── Register table ─────────────────────────────────────────────────────────
  mods_df <- reactive({
    items <- mods_list()
    if (length(items) == 0)
      return(data.frame(
        ref = character(), category = character(), cat_code = character(),
        title = character(), change_types = character(),
        status = character(), prepared = character(),
        submitted = character(), approved = character(), id = character(),
        stringsAsFactors = FALSE))
    do.call(rbind, lapply(items, function(x) {
      data.frame(
        ref          = x$ref %||% "",
        category     = category_label(x$category %||% ""),
        cat_code     = x$category %||% "",
        title        = x$title %||% "",
        change_types = paste(x$change_types %||% character(0), collapse = "||"),
        status       = x$status %||% "",
        prepared     = x$date_prepared  %||% "",
        submitted    = x$date_submitted %||% "",
        approved     = x$date_approved  %||% "",
        id           = x$id %||% "",
        stringsAsFactors = FALSE)
    }))
  })

  mods_df_filtered <- reactive({
    df <- mods_df()
    if (nrow(df) == 0) return(df)
    cf <- input$mod_cat_filter %||% "all"
    sf <- input$mod_status_filter %||% "all"
    q  <- trimws(input$mod_search %||% "")
    if (!identical(cf, "all"))    df <- df[df$cat_code == cf, , drop = FALSE]
    if (!identical(sf, "all"))    df <- df[df$status   == sf, , drop = FALSE]
    if (nzchar(q)) {
      ql <- tolower(q)
      df <- df[grepl(ql, tolower(df$ref), fixed = TRUE) |
               grepl(ql, tolower(df$title), fixed = TRUE), , drop = FALSE]
    }
    df
  })

  fmt_d <- function(v) {
    if (is.null(v) || !nzchar(v %||% "")) return("—")
    out <- suppressWarnings(format(as.Date(v), "%d %b %Y"))
    if (is.na(out)) "—" else out
  }

  output$mod_table <- renderReactable({
    df <- mods_df_filtered()
    if (nrow(df) == 0) {
      return(empty_reactable("No modifications match your filters."))
    }
    reactable(
      df[, c("ref","cat_code","category","title","change_types",
             "status","prepared","submitted","approved")],
      selection = "single", onClick = "select", defaultSelected = NULL,
      defaultPageSize = 20, compact = TRUE, bordered = FALSE, highlight = TRUE,
      columns = list(
        ref = colDef(name = "Ref", maxWidth = 100, html = TRUE,
          cell = function(v) sprintf('<span class="md-mono">%s</span>',
                                     htmltools::htmlEscape(v))),
        cat_code = colDef(show = FALSE),
        category = colDef(name = "Category", maxWidth = 130, html = TRUE,
          cell = function(value, idx) {
            code <- df$cat_code[idx]
            meta <- CAT_META[[code]] %||% list(short = value, color = "#8A8A8C")
            sprintf(
              '<span class="md-cat-pill" style="background:%s18;color:%s;"><span class="md-cat-pill-dot" style="background:%s;"></span>%s</span>',
              meta$color, meta$color, meta$color, htmltools::htmlEscape(meta$short))
          }),
        title = colDef(name = "Title", minWidth = 260, html = TRUE,
          cell = function(value, idx) {
            cts <- df$change_types[idx]
            tags_html <- ""
            if (nzchar(cts)) {
              parts <- strsplit(cts, "||", fixed = TRUE)[[1]]
              short <- function(s) sub(" — .*$", "", sub(" / .*$", "", s))
              shown <- head(parts, 2)
              chips <- paste0(
                vapply(shown, function(s)
                  sprintf('<span class="md-tag">%s</span>',
                          htmltools::htmlEscape(short(s))),
                  character(1)),
                collapse = "")
              extra <- length(parts) - length(shown)
              if (extra > 0)
                chips <- paste0(chips,
                  sprintf('<span class="md-tag-more">+%d</span>', extra))
              tags_html <- sprintf('<div class="md-title-tags">%s</div>', chips)
            }
            sprintf('<div class="md-title-cell"><div class="md-title-text">%s</div>%s</div>',
                    htmltools::htmlEscape(value), tags_html)
          }),
        change_types = colDef(show = FALSE),
        status = colDef(name = "Status", maxWidth = 160, html = TRUE,
          cell = function(value) {
            col <- STATUS_COLOR[[value]] %||% "#8A8A8C"
            short <- sub(" \\(.*$", "", sub(" /.*$", "", value))
            sprintf(
              '<span class="md-status-pill" style="background:%s18;color:%s;"><span class="md-status-dot" style="background:%s;"></span>%s</span>',
              col, col, col, htmltools::htmlEscape(short))
          }),
        prepared = colDef(name = "Prepared", maxWidth = 110, html = TRUE,
          cell = function(v) sprintf('<span class="md-muted">%s</span>', fmt_d(v))),
        submitted = colDef(name = "Submitted", maxWidth = 110, html = TRUE,
          cell = function(v) sprintf('<span class="md-muted">%s</span>', fmt_d(v))),
        approved = colDef(name = "Approved", maxWidth = 110, html = TRUE,
          cell = function(v) sprintf('<span class="md-muted">%s</span>', fmt_d(v)))
      ),
      theme = reactableTheme(
        borderColor = "transparent",
        headerStyle = list(background = "#F8F8F8", color = "#58595B", fontWeight = 600,
                           fontSize = "10px", textTransform = "uppercase",
                           letterSpacing = "0.5px"),
        rowSelectedStyle = list(background = "#F1F8FB")
      )
    )
  })

  # Track selection by row index → id (against the *filtered* df shown in
  # the table) and open the slide-over editor.
  observe({
    sel <- getReactableState("mod_table", "selected")
    if (is.null(sel) || length(sel) == 0) {
      selected_id(NULL); return()
    }
    df <- mods_df_filtered()
    if (nrow(df) == 0 || sel[1] > nrow(df)) { selected_id(NULL); return() }
    sid <- df$id[sel[1]]
    items <- mods_list()
    idx <- which(vapply(items, function(x)
      identical(x$id %||% x$ref, sid), logical(1)))
    if (length(idx) == 0) { selected_id(NULL); return() }
    selected_id(sid)
    fill_form_from(items[[idx[1]]])
    shinyjs::show("mod_editor_scrim")
    shinyjs::show("mod_save_update")
    shinyjs::hide("mod_save_new")
  })

  # ── Editor open / close ────────────────────────────────────────────────────
  observeEvent(input$mod_add_open, {
    empty_form()
    shinyjs::show("mod_editor_scrim")
    shinyjs::show("mod_save_new")
    shinyjs::hide("mod_save_update")
  })
  close_editor <- function() {
    shinyjs::hide("mod_editor_scrim")
    selected_id(NULL)
    # Clear table selection so re-clicking the same row re-opens the editor.
    tryCatch(
      reactable::updateReactable("mod_table", selected = NA),
      error = function(e) NULL)
  }
  observeEvent(input$mod_editor_close_btn, close_editor())
  observeEvent(input$mod_editor_close,     close_editor())

  # Eyebrow + category header inside the slide-over
  output$mod_editor_eyebrow <- renderUI({
    sid <- selected_id()
    if (is.null(sid)) "New modification"
    else paste("Editing", input$mod_ref %||% "")
  })
  output$mod_editor_cat <- renderUI({
    code <- input$mod_category %||% "non_substantial"
    label <- category_label(code)
    col   <- (CAT_META[[code]] %||% list(color = "#1B1B1B"))$color
    tags$span(style = sprintf("color:%s;", col), label)
  })

  # ── Save: add new ──────────────────────────────────────────────────────────
  observeEvent(input$mod_save_new, {
    if (is.null(rv$trial_config)) {
      showNotification("Select a trial first.", type = "warning"); return()
    }
    form <- read_form()
    if (!nzchar(form$title)) {
      showNotification("Add a title before saving.", type = "warning"); return()
    }
    items <- mods_list()
    new_item <- c(list(id = paste0("mod_", as.integer(Sys.time()))), form)
    if (!nzchar(new_item$ref)) new_item$ref <- next_ref(items)
    items <- c(items, list(new_item))
    persist(items)
    showNotification(sprintf("Added %s", new_item$ref), type = "message", duration = 3)
    empty_form()
    close_editor()
  })

  # ── Save: update selected ──────────────────────────────────────────────────
  observeEvent(input$mod_save_update, {
    sid <- selected_id()
    if (is.null(sid)) {
      showNotification("Select a row to update.", type = "warning"); return()
    }
    items <- mods_list()
    idx <- which(vapply(items, function(x)
      identical(x$id %||% x$ref, sid), logical(1)))
    if (length(idx) == 0) {
      showNotification("Selection not found — refreshing.", type = "warning")
      empty_form(); return()
    }
    form <- read_form()
    items[[idx[1]]] <- c(list(id = items[[idx[1]]]$id %||% sid), form)
    persist(items)
    showNotification(sprintf("Updated %s", form$ref), type = "message", duration = 3)
    close_editor()
  })

  # ── Cancel button (slide-over) ────────────────────────────────────────────
  observeEvent(input$mod_clear, { empty_form(); close_editor() })

  # ── Remove selected ───────────────────────────────────────────────────────
  observeEvent(input$mod_remove, {
    sid <- selected_id()
    if (is.null(sid)) {
      showNotification("Select a row to remove.", type = "warning"); return()
    }
    items <- mods_list()
    idx <- which(vapply(items, function(x)
      identical(x$id %||% x$ref, sid), logical(1)))
    if (length(idx) == 0) return()
    items <- items[-idx[1]]
    persist(items)
    showNotification("Modification removed.", type = "message", duration = 3)
    empty_form()
    close_editor()
  })

  # ── Duplicate selected ────────────────────────────────────────────────────
  observeEvent(input$mod_duplicate, {
    sid <- selected_id()
    if (is.null(sid)) {
      showNotification("Select a row to duplicate.", type = "warning"); return()
    }
    items <- mods_list()
    idx <- which(vapply(items, function(x)
      identical(x$id %||% x$ref, sid), logical(1)))
    if (length(idx) == 0) return()
    new_item <- items[[idx[1]]]
    new_item$id <- paste0("mod_", as.integer(Sys.time()))
    new_item$ref <- next_ref(items)
    new_item$status <- "Draft"
    new_item$date_prepared <- as.character(Sys.Date())
    new_item$date_submitted <- ""
    new_item$date_approved  <- ""
    new_item$date_implemented <- ""
    items <- c(items, list(new_item))
    persist(items)
    showNotification(sprintf("Duplicated as %s", new_item$ref),
                     type = "message", duration = 3)
    close_editor()
  })

  # ── CSV export ────────────────────────────────────────────────────────────
  output$mod_export_csv <- downloadHandler(
    filename = function() {
      code <- (rv$trial_config %||% list())$short_name %||%
              (rv$trial_config %||% list())$code %||% "trial"
      sprintf("%s_modifications_%s.csv", code, format(Sys.Date(), "%Y-%m-%d"))
    },
    content = function(file) {
      items <- mods_list()
      if (length(items) == 0) {
        writeLines(paste("ref,category,title,description,change_types,review_bodies,",
                         "documents,date_prepared,date_submitted,date_rfi,date_approved,",
                         "date_implemented,iras_ref,rec_ref,mhra_ref,",
                         "sponsor_authorised_by,status,notes", sep = ""), file)
        return()
      }
      df_out <- do.call(rbind, lapply(items, function(x) {
        data.frame(
          ref                   = x$ref %||% "",
          category              = category_label(x$category %||% ""),
          title                 = x$title %||% "",
          description           = x$description %||% "",
          change_types          = paste(x$change_types  %||% character(0), collapse = "; "),
          review_bodies         = paste(x$review_bodies %||% character(0), collapse = "; "),
          documents             = paste(x$documents     %||% character(0), collapse = "; "),
          date_prepared         = x$date_prepared    %||% "",
          date_submitted        = x$date_submitted   %||% "",
          date_rfi              = x$date_rfi         %||% "",
          date_approved         = x$date_approved    %||% "",
          date_implemented      = x$date_implemented %||% "",
          iras_ref              = x$iras_ref %||% "",
          rec_ref               = x$rec_ref %||% "",
          mhra_ref              = x$mhra_ref %||% "",
          sponsor_authorised_by = x$sponsor_authorised_by %||% "",
          status                = x$status %||% "",
          notes                 = x$notes %||% "",
          stringsAsFactors = FALSE)
      }))
      write.csv(df_out, file, row.names = FALSE, na = "")
    }
  )

  # ── Old Reports-tab amendments: move them into the register ───────────────
  output$mod_legacy_banner <- renderUI({
    old <- legacy_amendments_pending(rv$trial_config %||% list())
    if (!length(old)) return(NULL)
    n <- length(old)
    div(class = "md-banner",
        div(class = "md-banner-t",
            sprintf("%d amendment%s entered on the Reports tab %s not in this register yet",
                    n, if (n == 1) "" else "s", if (n == 1) "is" else "are")),
        div(class = "md-banner-s",
            "The reports already include them. Move them here so everything is edited in one place; any marked only “Substantial” are filed as Category A for you to check."),
        actionButton("mod_legacy_move", "Move them into the register", class = "md-btn-primary"))
  })

  observeEvent(input$mod_legacy_move, {
    cfg <- rv$trial_config; req(cfg)
    res <- legacy_amendments_to_register(cfg)
    persist(res$items)
    tryCatch(update_overrides(cfg, amendments = list()), error = function(e) NULL)
    rv$trial_config$amendments <- list()
    showNotification(sprintf("Moved %d amendment%s into the register.", res$n_new,
                             if (res$n_new == 1) "" else "s"), type = "message", duration = 4)
  })

  # ── Import from a CSV or Excel file ───────────────────────────────────────
  imp_raw  <- reactiveVal(NULL)    # the file's rows, as text
  imp_name <- reactiveVal("")

  observeEvent(input$mod_import_open, {
    if (is.null(rv$trial_config)) { showNotification("Select a trial first.", type = "warning"); return() }
    imp_raw(NULL); imp_name("")
    showModal(modalDialog(
      title = "Import modifications", size = "l", easyClose = TRUE,
      footer = tagList(modalButton("Cancel"),
                       actionButton("mod_import_go", "Import", class = "md-btn-primary")),
      div(class = "md-imp",
          tags$p(class = "md-imp-lead",
                 "Upload a spreadsheet of modifications, as CSV or Excel. Columns are matched automatically; check the matches and the preview before importing. Export CSV gives you a file in exactly this layout."),
          fileInput("mod_import_file", NULL, accept = c(".csv", ".xlsx", ".xls"),
                    buttonLabel = "Choose file…", placeholder = "No file chosen", width = "100%"),
          uiOutput("mod_import_body"))))
  })

  observeEvent(input$mod_import_file, {
    f <- input$mod_import_file; req(f)
    df <- tryCatch(mod_import_read(f$datapath, f$name), error = function(e) {
      showNotification(paste("Couldn't read that file:", conditionMessage(e)), type = "error", duration = 8)
      NULL })
    if (!is.null(df) && !nrow(df)) {
      showNotification("That file has no rows to import.", type = "warning"); df <- NULL }
    imp_raw(df); imp_name(f$name)
  })

  imp_mapping <- reactive({
    df <- imp_raw(); req(df)
    guess <- mod_import_guess(names(df))
    setNames(lapply(names(MOD_IMPORT_FIELDS), function(f) {
      v <- input[[paste0("mod_map_", f)]]
      if (is.null(v)) guess[[f]] %||% "" else v
    }), names(MOD_IMPORT_FIELDS))
  })

  imp_built <- reactive({
    df <- imp_raw(); req(df)
    mod_import_build(df, imp_mapping(), input$mod_import_default_sub %||% "substantial_a")
  })

  # The column matches (drawn once per file) and the options
  output$mod_import_body <- renderUI({
    df <- imp_raw()
    if (is.null(df)) return(div(class = "md-imp-empty", "Choose a file to see its columns."))
    guess <- mod_import_guess(names(df))
    sel <- function(f) div(class = "md-imp-map-row",
      tags$label(`for` = paste0("mod_map_", f), MOD_IMPORT_FIELDS[[f]]$label),
      selectInput(paste0("mod_map_", f), NULL, choices = c("— not in this file —" = "", names(df)),
                  selected = guess[[f]] %||% "", width = "100%"))
    main <- c("ref", "title", "description", "category", "status", "date_submitted", "date_approved")
    more <- setdiff(names(MOD_IMPORT_FIELDS), main)
    plural <- function(n, w) sprintf("%d %s%s", n, w, if (n == 1) "" else "s")
    tagList(
      div(class = "md-imp-file", sprintf("%s · %s · %s", imp_name(), plural(nrow(df), "row"),
                                         plural(ncol(df), "column"))),
      div(class = "md-imp-h", "Match the columns"),
      div(class = "md-imp-map", lapply(main, sel)),
      tags$details(class = "md-imp-more",
                   tags$summary(sprintf("More fields (%d)", length(more))),
                   div(class = "md-imp-map", lapply(more, sel))),
      div(class = "md-imp-opts",
          selectInput("mod_import_default_sub", "Rows marked only “Substantial” are filed as",
                      choices = MOD_CATEGORIES[MOD_CATEGORIES %in% .MOD_SUB_CODES],
                      selected = "substantial_a", width = "100%"),
          radioButtons("mod_import_dupes", "If a reference is already in the register",
                       c("Skip that row" = "skip", "Update it with the imported row" = "update"),
                       selected = "skip")),
      div(class = "md-imp-h", "Preview"),
      uiOutput("mod_import_preview"))
  })

  output$mod_import_preview <- renderUI({
    b <- imp_built(); items <- b$items
    if (!length(items))
      return(div(class = "md-imp-empty", "No rows with a reference, title or description to import."))
    existing <- tolower(vapply(mods_list(), function(m) m$ref %||% "", ""))
    dup  <- vapply(items, function(m) nzchar(m$ref) && tolower(m$ref) %in% existing, logical(1))
    mode <- input$mod_import_dupes %||% "skip"
    stat <- function(v, l, warn = FALSE)
      div(class = paste("md-imp-stat", if (warn) "warn"),
          div(class = "md-imp-stat-v", v), div(class = "md-imp-stat-l", l))
    rows <- lapply(utils::head(seq_along(items), 8), function(i) {
      m <- items[[i]]
      tags$tr(class = if (dup[i]) "dup",
              tags$td(if (nzchar(m$ref)) m$ref else tags$em("next free")),
              tags$td(category_label(m$category)), tags$td(m$title), tags$td(m$status),
              tags$td(fmt_d(m$date_submitted)), tags$td(fmt_d(m$date_approved)))
    })
    tagList(
      div(class = "md-imp-stats",
          stat(sum(!dup), "new"),
          if (any(dup)) stat(sum(dup), sprintf("already in the register — %s",
                                               if (mode == "skip") "skipped" else "updated"),
                             mode == "update"),
          if (b$n_no_category) stat(b$n_no_category, "with no category — filed as minor", TRUE),
          if (b$n_dropped) stat(b$n_dropped, "empty rows ignored")),
      div(class = "md-imp-table",
          tags$table(tags$thead(tags$tr(lapply(c("Ref", "Category", "Title", "Status", "Submitted", "Approved"),
                                               tags$th))),
                     tags$tbody(rows))),
      if (length(items) > 8) div(class = "md-imp-more-n", sprintf("…and %d more.", length(items) - 8)))
  })

  observeEvent(input$mod_import_go, {
    if (is.null(imp_raw())) { showNotification("Choose a file first.", type = "warning"); return() }
    b <- imp_built()
    if (!length(b$items)) { showNotification("There is nothing in that file to import.", type = "warning"); return() }
    res <- mod_import_merge(mods_list(), b$items, input$mod_import_dupes %||% "skip")
    persist(res$items)
    n <- res$n_new + res$n_updated
    if (exists("log_activity"))
      tryCatch(log_activity("modifications_imported",
                            sprintf("Imported %d modification%s from <strong>%s</strong>", n,
                                    if (n == 1) "" else "s", htmltools::htmlEscape(imp_name())),
                            username = rv$username, trial_code = rv$trial_config$code),
               error = function(e) NULL)
    removeModal(); imp_raw(NULL)
    showNotification(sprintf("Imported %d new, updated %d, skipped %d.", res$n_new, res$n_updated,
                             res$n_skipped), type = "message", duration = 5)
  })
}
