# =============================================================================
# Settings — the configurable side of the trial-health features
# =============================================================================
#   Data & mapping             field mapping, change-of-status codes
#   Visits & CRFs              CRF schedule editor, due-date rules
#   Recruitment & monitoring   target schedule, pilot, sites & projections,
#                              working hours, health thresholds and weights
#   Reports & admin            settings export / import
#
# Each card saves into overrides.json (update_overrides) and updates
# rv$trial_config, so every tab reacts straight away. Runs alongside
# modules/trial_settings_server.R, which owns the original editors.
# =============================================================================

settings_monitoring_server <- function(input, output, session, state) {
  rv <- state$rv

  num <- function(x, default = NA_real_) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) == 1 && !is.na(x)) x else default
  }

  # Save a patch to overrides.json and the live config. `cfg_patch` lets the
  # in-memory value differ from its JSON form (named vector vs list, etc.);
  # a NULL value removes the key.
  save_cfg <- function(json_patch, msg, cfg_patch = json_patch) {
    cfg <- rv$trial_config
    if (is.null(cfg) || !require_role(rv, "manager")) return(invisible(FALSE))
    ok <- tryCatch({ do.call(update_overrides, c(list(cfg), json_patch)); TRUE },
                   error = function(e) {
                     showNotification(paste("Save failed:", conditionMessage(e)), type = "error", duration = 8)
                     FALSE })
    if (!ok) return(invisible(FALSE))
    for (k in names(cfg_patch)) {
      if (is.null(cfg_patch[[k]])) cfg[[k]] <- NULL else cfg[[k]] <- cfg_patch[[k]]
    }
    rv$trial_config <- cfg
    apply_trial_globals(cfg)
    rv$settings_changed <- Sys.time()
    log_activity("settings_saved", htmltools::htmlEscape(msg),
                 username = rv$username, trial_code = cfg$code)
    showNotification(HTML(paste0("&#x2714; ", htmltools::htmlEscape(msg))), type = "message", duration = 4)
    invisible(TRUE)
  }

  save_monitoring <- function(part, values, msg, extra = list()) {
    m <- rv$trial_config$monitoring %||% list()
    m[[part]] <- values
    save_cfg(c(list(monitoring = m), extra), msg)
  }

  warn <- function(msg) showNotification(msg, type = "warning", duration = 7)

  # ════════════════════════════════════════════════════════════════════════
  # Data & mapping — field mapping
  # ════════════════════════════════════════════════════════════════════════
  FIELD_ROLES <- list(
    c("record_id",              "Participant ID"),
    c("site_name",              "Site / data access group"),
    c("randomisation_datetime", "Randomisation date & time"),
    c("operation_date",         "Operation date"),
    c("operation_datetime",     "Operation date & time"),
    c("discharge_date",         "Discharge date"),
    c("cos_type",               "Change-of-status code"),
    c("cos_date",               "Change-of-status date"),
    c("randomisation_complete", "Randomisation form complete"),
    c("consent_complete",       "Consent form complete"),
    c("discharge_complete",     "Discharge form complete"),
    c("day30_complete",         "Day 30 form complete"),
    c("day90_complete",         "Day 90 form complete"),
    c("age",                    "Age"),
    c("sex",                    "Sex"),
    c("ethnicity",              "Ethnicity"))

  output$settings_fields_ui <- renderUI({
    cfg <- rv$trial_config
    if (is.null(cfg)) return(NULL)
    rv$settings_changed
    cols <- names(rv$raw_redcap %||% list())
    f <- cfg$redcap_fields %||% list()
    rows <- lapply(FIELD_ROLES, function(r) {
      cur <- f[[r[1]]]
      cur <- if (is.character(cur) && length(cur) == 1) cur else ""
      state_cls <- if (!nzchar(cur)) "off" else if (!length(cols)) "unk" else if (cur %in% cols) "ok" else "bad"
      div(class = "map-row",
          div(class = "map-l", r[2], span(class = "map-role", r[1])),
          selectizeInput(paste0("map_", r[1]), NULL, choices = unique(c("", cur, cols)),
                         selected = cur, width = "100%",
                         options = list(create = TRUE, placeholder = "Not used")),
          span(class = paste("map-state", state_cls),
               switch(state_cls, off = "Not used", unk = "—", ok = "Found in export", bad = "Not in export")))
    })
    tagList(
      if (!length(cols))
        div(class = "s-hint", style = "margin-bottom:10px;",
            "Load a REDCap export to pick variables from a list and check each one exists."),
      div(class = "map-list", rows))
  })
  outputOptions(output, "settings_fields_ui", suspendWhenHidden = FALSE)

  observeEvent(input$settings_save_fields, {
    f <- rv$trial_config$redcap_fields %||% list()
    # "" (not NULL) so a field mapped in config.R can still be switched off
    for (r in FIELD_ROLES) f[[r[1]]] <- trimws(input[[paste0("map_", r[1])]] %||% "")
    save_cfg(list(redcap_fields = f), "Field mapping saved. Reload the export to re-read it.")
  })

  # ════════════════════════════════════════════════════════════════════════
  # Data & mapping — change-of-status codes
  # ════════════════════════════════════════════════════════════════════════
  cos_seq  <- reactiveVal(0L)
  cos_rows <- reactiveVal(list())
  cos_uid  <- function() { n <- cos_seq() + 1L; cos_seq(n); n }
  cos_sync <- function() lapply(cos_rows(), function(r) list(
    uid   = r$uid,
    code  = input[[paste0("cos_code_", r$uid)]] %||% r$code,
    label = input[[paste0("cos_lbl_",  r$uid)]] %||% r$label))

  output$settings_cos_ui <- renderUI({
    rows <- cos_rows()
    if (!length(rows)) return(div(class = "sch-empty", "No change-of-status codes yet — add one below."))
    tagList(
      div(class = "cos-colhead", span("Code"), span("What it means"), span()),
      lapply(rows, function(r) div(class = "cos-row",
        textInput(paste0("cos_code_", r$uid), NULL, r$code, placeholder = "e.g. 4"),
        textInput(paste0("cos_lbl_", r$uid), NULL, r$label, width = "100%",
                  placeholder = "e.g. Complete withdrawal"),
        tags$button(type = "button", class = "sch-remove", title = "Remove code",
                    onclick = sprintf("Shiny.setInputValue('settings_cos_remove', %d, {priority:'event'})", r$uid),
                    HTML("&times;")))))
  })
  outputOptions(output, "settings_cos_ui", suspendWhenHidden = FALSE)

  observeEvent(input$settings_cos_add, {
    r <- cos_sync(); r[[length(r) + 1]] <- list(uid = cos_uid(), code = "", label = ""); cos_rows(r)
  })
  observeEvent(input$settings_cos_remove, {
    id <- suppressWarnings(as.integer(input$settings_cos_remove))
    cos_rows(Filter(function(r) !identical(r$uid, id), cos_sync()))
  })
  observeEvent(input$settings_save_cos, {
    r <- Filter(function(x) nzchar(trimws(x$code)) && nzchar(trimws(x$label)), cos_sync())
    codes <- vapply(r, function(x) trimws(x$code), "")
    if (anyDuplicated(codes)) return(warn("Each change-of-status code can only appear once."))
    labs <- setNames(vapply(r, function(x) trimws(x$label), ""), codes)
    save_cfg(list(cos_type_labels = as.list(labs)),
             sprintf("Saved %d change-of-status code%s.", length(labs), if (length(labs) == 1) "" else "s"),
             cfg_patch = list(cos_type_labels = labs))
  })

  # ════════════════════════════════════════════════════════════════════════
  # Visits & CRFs — CRF schedule editor
  # ════════════════════════════════════════════════════════════════════════
  crf_seq  <- reactiveVal(0L)
  crf_rows <- reactiveVal(list())
  crf_uid  <- function() { n <- crf_seq() + 1L; crf_seq(n); n }

  crf_init <- function() {
    cfg <- rv$trial_config
    if (is.null(cfg)) return(invisible())
    s <- th_crf_schedule(cfg, rv$raw_redcap)
    crf_rows(lapply(seq_len(nrow(s)), function(i) list(
      uid = crf_uid(), timepoint = s$timepoint[i], form = s$form[i], field = s$field[i],
      event = s$event[i], anchor = s$anchor[i], offset = s$offset[i], grace = s$grace[i],
      kind = s$kind[i])))
  }

  crf_sync <- function() lapply(crf_rows(), function(r) {
    g <- function(k, d) { v <- input[[paste0("crf_", k, "_", r$uid)]]; if (is.null(v)) d else v }
    list(uid = r$uid, timepoint = g("tp", r$timepoint), form = g("form", r$form),
         field = g("field", r$field), event = g("event", r$event), anchor = g("anchor", r$anchor),
         offset = num(g("offset", r$offset), 0), grace = num(g("grace", r$grace)),
         kind = g("kind", r$kind))
  })

  output$settings_crf_ui <- renderUI({
    rows <- crf_rows()
    cfg  <- rv$trial_config
    cols <- names(rv$raw_redcap %||% list())
    evs  <- unique(c("", as.character(unlist(cfg$redcap_events %||% list()))))
    fcols <- grep("_complete$", cols, value = TRUE)
    if (!length(rows)) return(div(class = "sch-empty", "No forms scheduled — add one below."))
    # A blank Grace uses the default, so show the default as the placeholder
    grace_ph <- function(tag) {
      tag$children <- lapply(tag$children, function(ch) {
        if (inherits(ch, "shiny.tag") && identical(ch$name, "input"))
          ch$attribs$placeholder <- as.character(th_settings(cfg)$crf$grace_days %||% 14)
        ch
      })
      tag
    }
    lapply(rows, function(r) div(class = "crf-row",
      textInput(paste0("crf_tp_", r$uid), NULL, r$timepoint, placeholder = "e.g. Day 30"),
      textInput(paste0("crf_form_", r$uid), NULL, r$form, placeholder = "Form name"),
      selectizeInput(paste0("crf_field_", r$uid), NULL, choices = unique(c(r$field, fcols)),
                     selected = r$field, options = list(create = TRUE, placeholder = "…_complete")),
      selectizeInput(paste0("crf_event_", r$uid), NULL, choices = unique(c(r$event, evs)),
                     selected = r$event, options = list(create = TRUE, placeholder = "Any event")),
      selectInput(paste0("crf_anchor_", r$uid), NULL,
                  c("Randomisation" = "rand", "Operation" = "op", "Discharge" = "discharge"), r$anchor,
                  selectize = FALSE),
      numericInput(paste0("crf_offset_", r$uid), NULL, r$offset, min = 0),
      grace_ph(numericInput(paste0("crf_grace_", r$uid), NULL, if (is.na(r$grace)) NA else r$grace, min = 0)),
      selectInput(paste0("crf_kind_", r$uid), NULL, c("Site CRF" = "CRF", "Questionnaire" = "PROM"), r$kind,
                  selectize = FALSE),
      tags$button(type = "button", class = "sch-remove", title = "Remove form",
                  onclick = sprintf("Shiny.setInputValue('settings_crf_remove', %d, {priority:'event'})", r$uid),
                  HTML("&times;"))))
  })
  outputOptions(output, "settings_crf_ui", suspendWhenHidden = FALSE)

  output$settings_crf_status <- renderUI({
    cfg <- rv$trial_config
    if (is.null(cfg)) return(NULL)
    custom <- length(cfg$crf_schedule) > 0
    sm <- tryCatch(state$health()$summary, error = function(e) NULL)
    missing <- if (!is.null(rv$raw_redcap)) {
      s <- th_crf_schedule(cfg, rv$raw_redcap); unique(s$field[!s$present])
    } else character()
    div(class = "s-status",
        span(class = paste("ovr-pill", if (custom) "on"), if (custom) "Custom schedule" else "Automatic"),
        if (!is.null(sm)) span(sprintf("With the saved schedule: %d overdue CRFs and %d overdue questionnaires across %d participants.",
                                       sm$overdue_crfs, sm$overdue_proms, sm$n)),
        if (length(missing)) div(class = "s-warn",
                                 paste("Not in the current export, so not tracked:", paste(missing, collapse = ", "))))
  })

  observeEvent(input$settings_crf_add, {
    r <- crf_sync()
    r[[length(r) + 1]] <- list(uid = crf_uid(), timepoint = "", form = "", field = "", event = "",
                               anchor = "rand", offset = 0, grace = NA_real_, kind = "CRF")
    crf_rows(r)
  })
  observeEvent(input$settings_crf_remove, {
    id <- suppressWarnings(as.integer(input$settings_crf_remove))
    crf_rows(Filter(function(r) !identical(r$uid, id), crf_sync()))
  })
  observeEvent(input$settings_save_crf, {
    rows <- Filter(function(r) nzchar(trimws(r$field %||% "")), crf_sync())
    if (!length(rows)) return(warn("Add at least one form with a completion field, or reset to the automatic schedule."))
    out <- lapply(rows, function(r) list(
      timepoint = trimws(r$timepoint), form = if (nzchar(trimws(r$form))) trimws(r$form) else trimws(r$field),
      field = trimws(r$field), event = trimws(r$event %||% ""), anchor = r$anchor,
      offset = r$offset, grace = if (is.na(r$grace)) NA_real_ else r$grace, kind = r$kind))
    save_cfg(list(crf_schedule = out), sprintf("Saved a CRF schedule of %d forms.", length(out)))
  })
  observeEvent(input$settings_crf_reset, {
    if (isTRUE(save_cfg(list(crf_schedule = NULL), "Back to the automatic CRF schedule."))) crf_init()
  })

  observeEvent(input$settings_save_crf_rules, {
    save_monitoring("crf", list(
      grace_days = num(input$set_crf_grace, 14),
      op_expected_days = num(input$set_op_expected, 7),
      discharge_expected_days = num(input$set_dis_expected, 60)), "Due-date rules saved.")
  })

  # ════════════════════════════════════════════════════════════════════════
  # Recruitment & monitoring — target schedule
  # ════════════════════════════════════════════════════════════════════════
  ts_text <- function(cfg) {
    ts <- th_target_schedule(cfg)
    if (is.null(ts)) return("")
    paste(sprintf("%s, %s", format(ts$month_date, "%Y-%m"),
                  format(ts$cumulative_target, trim = TRUE, drop0trailing = TRUE)), collapse = "\n")
  }
  parse_month <- function(s) {
    s <- trimws(s)
    if (grepl("^[0-9]{4}-[0-9]{1,2}$", s)) s <- paste0(s, "-01")
    d <- suppressWarnings(as.Date(s, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"), optional = TRUE))
    if (is.na(d)) d <- suppressWarnings(as.Date(paste("01", s), format = "%d %b %Y"))
    if (is.na(d)) d <- suppressWarnings(as.Date(paste("01", s), format = "%d %B %Y"))
    d
  }
  ts_parse <- function(txt) {
    lines <- trimws(strsplit(txt %||% "", "\n", fixed = TRUE)[[1]])
    lines <- lines[nzchar(lines)]
    if (!length(lines)) return(NULL)
    d <- as.Date(vapply(lines, function(l) {
      p <- trimws(strsplit(l, "[,;\t]")[[1]])
      if (length(p) < 2) { p <- strsplit(l, " +")[[1]]; p <- c(paste(head(p, -1), collapse = " "), tail(p, 1)) }
      as.character(parse_month(p[1]))
    }, "", USE.NAMES = FALSE))
    v <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", vapply(lines, function(l) tail(strsplit(l, "[,;\t ]+")[[1]], 1), "", USE.NAMES = FALSE))))
    bad <- is.na(d) | is.na(v)
    df <- data.frame(month_date = d[!bad], cumulative_target = v[!bad])
    list(df = df[order(df$month_date), , drop = FALSE], bad = lines[bad])
  }

  output$settings_target_preview <- renderUI({
    p <- ts_parse(input$set_target_schedule)
    if (is.null(p) || !nrow(p$df))
      return(div(class = "s-hint", "No target schedule — pace-against-target views will be hidden."))
    df <- p$df; w <- 320; h <- 150; pad <- 8
    x <- if (nrow(df) == 1) w / 2 else pad + (seq_len(nrow(df)) - 1) / (nrow(df) - 1) * (w - 2 * pad)
    y <- h - pad - df$cumulative_target / max(1, df$cumulative_target) * (h - 2 * pad)
    pts <- paste(sprintf("%.1f,%.1f", x, y), collapse = " ")
    tagList(
      div(class = "ts-preview", HTML(sprintf(
        '<svg width="%d" height="%d" viewBox="0 0 %d %d" role="img" aria-label="Target schedule preview"><polygon class="ts-area" points="%s %.1f,%d %.1f,%d"/><polyline class="ts-line" points="%s"/></svg>',
        w, h, w, h, pts, x[length(x)], h - pad, x[1], h - pad, pts))),
      div(class = "s-hint", sprintf("%d months, reaching %s by %s", nrow(df),
                                    format(max(df$cumulative_target), big.mark = ","),
                                    format(max(df$month_date), "%B %Y"))),
      if (length(p$bad)) div(class = "s-warn", sprintf("Can't read %d line%s: %s", length(p$bad),
                                                      if (length(p$bad) == 1) "" else "s",
                                                      paste(head(p$bad, 3), collapse = " · "))))
  })

  observeEvent(input$settings_save_target, {
    p <- ts_parse(input$set_target_schedule)
    if (!is.null(p) && length(p$bad)) return(warn("Some lines couldn't be read — fix or remove them before saving."))
    if (is.null(p) || !nrow(p$df)) {
      save_cfg(list(target_schedule = NULL), "Target schedule removed.")
    } else {
      df <- p$df
      save_cfg(list(target_schedule = list(month_date = format(df$month_date), cumulative_target = df$cumulative_target)),
               sprintf("Saved a %d-month target schedule.", nrow(df)),
               cfg_patch = list(target_schedule = df))
    }
  })

  # ════════════════════════════════════════════════════════════════════════
  # Recruitment & monitoring — pilot, sites & projections
  # ════════════════════════════════════════════════════════════════════════
  observeEvent(input$settings_save_pilot, {
    pl <- list(target = num(input$set_pilot_target), sites = num(input$set_pilot_sites),
               months = num(input$set_pilot_months))
    if (is.na(pl$target)) save_cfg(list(pilot = NULL), "Internal pilot removed.")
    else save_cfg(list(pilot = Filter(Negate(is.na), pl)), "Internal pilot saved.")
  })

  observeEvent(input$settings_save_siteproj, {
    pd <- rv$trial_config$projection_defaults %||% list()
    pd$target_sites  <- num(input$set_target_sites, pd$target_sites %||% 24)
    pd$sites_central <- num(input$set_sites_per_month, pd$sites_central %||% 2)
    m <- rv$trial_config$monitoring %||% list()
    m$projection <- list(window_weeks = num(input$set_proj_window, 12),
                         sims = th_settings(rv$trial_config)$projection$sims)
    save_cfg(list(projection_defaults = pd,
                  site_defaults = list(monthly_target = num(input$set_site_monthly, 2)),
                  monitoring = m), "Site and projection settings saved.")
  })

  # ════════════════════════════════════════════════════════════════════════
  # Recruitment & monitoring — working hours
  # ════════════════════════════════════════════════════════════════════════
  observeEvent(input$settings_save_hours, {
    ok_time <- function(s) grepl("^([01]?[0-9]|2[0-3]):[0-5][0-9]$", trimws(s %||% ""))
    if (!ok_time(input$set_wh_start) || !ok_time(input$set_wh_end))
      return(warn("Enter working hours as HH:MM, for example 08:00 and 18:00."))
    if (.th_hm(input$set_wh_start) >= .th_hm(input$set_wh_end))
      return(warn("The end of the working day must be after the start."))
    if (!length(input$set_wh_days)) return(warn("Pick at least one working day."))
    extra <- trimws(strsplit(input$set_wh_extra %||% "", "\n", fixed = TRUE)[[1]])
    extra <- sub("\\s.*$", "", extra[nzchar(extra)])
    bad <- extra[is.na(suppressWarnings(as.Date(extra, optional = TRUE)))]
    if (length(bad)) return(warn(paste("These aren't dates (use YYYY-MM-DD):", paste(bad, collapse = ", "))))
    save_monitoring("working_hours", list(
      start = trimws(input$set_wh_start), end = trimws(input$set_wh_end),
      days = as.list(input$set_wh_days), bank_holidays = isTRUE(input$set_wh_bh),
      extra_holidays = as.list(extra)), "Working hours saved.")
  })

  # ════════════════════════════════════════════════════════════════════════
  # Recruitment & monitoring — health thresholds and weights
  # ════════════════════════════════════════════════════════════════════════
  observeEvent(input$settings_save_thresholds, {
    fl <- list(
      cos_exclude = trimws(input$set_cos_exclude %||% ""),
      z_warn = num(input$set_z_warn, 1.96), z_alarm = num(input$set_z_alarm, 3.09),
      overdue_amber = num(input$set_over_amber, 10), overdue_red = num(input$set_over_red, 25),
      quiet_amber = num(input$set_quiet_amber, 30), quiet_red = num(input$set_quiet_red, 60),
      expected_attrition = num(input$set_exp_attr, 15), min_n = num(input$set_min_n, 5))
    if (fl$z_warn >= fl$z_alarm) return(warn("The alarm limit must be stricter than the warning limit."))
    if (fl$overdue_amber >= fl$overdue_red) return(warn("Overdue CRFs: amber must be below red."))
    if (fl$quiet_amber >= fl$quiet_red) return(warn("Quiet sites: amber must be fewer days than red."))
    w <- list(recruitment = num(input$set_w_recruit, 30), retention = num(input$set_w_retain, 30),
              data = num(input$set_w_data, 30), activity = num(input$set_w_activity, 10))
    if (sum(unlist(w)) <= 0) return(warn("At least one score weight must be above zero."))
    m <- rv$trial_config$monitoring %||% list()
    m$flags <- fl; m$weights <- w
    save_cfg(list(monitoring = m), "Health thresholds saved.")
  })

  # ════════════════════════════════════════════════════════════════════════
  # Reports & admin — export / import settings
  # ════════════════════════════════════════════════════════════════════════
  output$settings_export <- downloadHandler(
    filename = function() sprintf("%s_settings_%s.json", rv$trial_config$code %||% "trial",
                                  format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      jsonlite::write_json(load_overrides(rv$trial_config), file, auto_unbox = TRUE, pretty = TRUE)
    })

  pending_import <- reactiveVal(NULL)
  # Trial-specific content that should never be copied between trials
  NO_IMPORT <- c("short_name", "name", "trial_target", "modifications", "report_content",
                 "portfolio_review", "report_template_paths", "data_dir", "return_rates_dir", "logo_file")

  observeEvent(input$settings_import, {
    f <- input$settings_import
    req(f)
    ov <- tryCatch(jsonlite::fromJSON(f$datapath, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.list(ov) || !length(ov) || is.null(names(ov)))
      return(showNotification("That file isn't a settings export (an overrides.json file).", type = "error", duration = 8))
    keep <- setdiff(names(ov), NO_IMPORT)
    if (!length(keep)) return(warn("There's nothing in that file that can be copied to this trial."))
    pending_import(ov[keep])
    showModal(modalDialog(
      title = "Import settings?", easyClose = TRUE,
      footer = tagList(modalButton("Cancel"), actionButton("settings_import_go", "Import", class = "btn-primary-sm")),
      div(style = "font-size:13px;line-height:1.6;",
          sprintf("This copies %d setting group%s into %s: ", length(keep), if (length(keep) == 1) "" else "s",
                  rv$trial_config$short_name %||% "this trial"),
          tags$b(paste(keep, collapse = ", ")), ".",
          div(class = "s-hint", style = "margin-top:8px;",
              "Names, targets, data paths, report text and modifications stay as they are."))))
  })

  observeEvent(input$settings_import_go, {
    ov <- pending_import(); req(ov)
    removeModal()
    cfg <- rv$trial_config
    if (!require_role(rv, "manager")) return()
    cur <- load_overrides(cfg)
    for (k in names(ov)) cur[[k]] <- ov[[k]]
    save_overrides(cfg, cur)
    new <- apply_overrides(cfg, ov)
    rv$trial_config <- new
    apply_trial_globals(new)
    apply_trial_colours(new$colors %||% list())
    rv$settings_changed <- Sys.time()
    pending_import(NULL)
    crf_init()
    showNotification(HTML("&#x2714; Settings imported."), type = "message", duration = 4)
  })

  # ════════════════════════════════════════════════════════════════════════
  # Load every field when a trial is opened
  # ════════════════════════════════════════════════════════════════════════
  observeEvent(rv$trial_config$code, {
    cfg <- rv$trial_config
    if (is.null(cfg)) return()
    st <- th_settings(cfg)
    updateNumericInput(session, "set_crf_grace",    value = st$crf$grace_days)
    updateNumericInput(session, "set_op_expected",  value = st$crf$op_expected_days)
    updateNumericInput(session, "set_dis_expected", value = st$crf$discharge_expected_days)
    updateTextAreaInput(session, "set_target_schedule", value = ts_text(cfg))
    pl <- cfg$pilot %||% list()
    updateNumericInput(session, "set_pilot_target", value = num(pl$target))
    updateNumericInput(session, "set_pilot_sites",  value = num(pl$sites))
    updateNumericInput(session, "set_pilot_months", value = num(pl$months))
    pd <- cfg$projection_defaults %||% list()
    updateNumericInput(session, "set_target_sites",    value = num(pd$target_sites, 24))
    updateNumericInput(session, "set_sites_per_month", value = num(pd$sites_central, 2))
    updateNumericInput(session, "set_site_monthly",    value = num(cfg$site_defaults$monthly_target, 2))
    updateNumericInput(session, "set_proj_window",     value = st$projection$window_weeks)
    wh <- st$working_hours
    updateTextInput(session, "set_wh_start", value = wh$start)
    updateTextInput(session, "set_wh_end",   value = wh$end)
    updateCheckboxGroupInput(session, "set_wh_days", selected = wh$days)
    updateCheckboxInput(session, "set_wh_bh", value = isTRUE(wh$bank_holidays))
    updateTextAreaInput(session, "set_wh_extra", value = paste(wh$extra_holidays, collapse = "\n"))
    fl <- st$flags
    updateTextInput(session, "set_cos_exclude", value = fl$cos_exclude)
    updateSelectInput(session, "set_z_warn",  selected = as.character(fl$z_warn))
    updateSelectInput(session, "set_z_alarm", selected = as.character(fl$z_alarm))
    updateNumericInput(session, "set_over_amber",  value = fl$overdue_amber)
    updateNumericInput(session, "set_over_red",    value = fl$overdue_red)
    updateNumericInput(session, "set_quiet_amber", value = fl$quiet_amber)
    updateNumericInput(session, "set_quiet_red",   value = fl$quiet_red)
    updateNumericInput(session, "set_exp_attr",    value = fl$expected_attrition)
    updateNumericInput(session, "set_min_n",       value = fl$min_n)
    w <- st$weights
    updateSliderInput(session, "set_w_recruit",  value = w$recruitment)
    updateSliderInput(session, "set_w_retain",   value = w$retention)
    updateSliderInput(session, "set_w_data",     value = w$data)
    updateSliderInput(session, "set_w_activity", value = w$activity)
    labs <- cfg$cos_type_labels
    cos_rows(lapply(names(labs), function(k)
      list(uid = cos_uid(), code = k, label = as.character(unlist(labs[[k]]))[1])))
    crf_init()
  })
}
