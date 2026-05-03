trial_settings_server <- function(input, output, session, state) {
  rv <- state$rv

  # ── Colour presets ────────────────────────────────────────────────────────
  presets <- list(
    navy_teal     = list(primary = "#1B4F6B", secondary = "#2EC4A5", accent = "#F59E0B"),
    indigo_violet = list(primary = "#312E81", secondary = "#8B5CF6", accent = "#F59E0B"),
    emerald       = list(primary = "#064E3B", secondary = "#10B981", accent = "#FBBF24"),
    slate_coral   = list(primary = "#334155", secondary = "#F97316", accent = "#06B6D4"),
    burgundy      = list(primary = "#7F1D1D", secondary = "#DC2626", accent = "#F59E0B"),
    ocean         = list(primary = "#0C4A6E", secondary = "#06B6D4", accent = "#F97316")
  )

  apply_preset <- function(preset) {
    updateTextInput(session, "set_col_primary",   value = preset$primary)
    updateTextInput(session, "set_col_secondary", value = preset$secondary)
    updateTextInput(session, "set_col_accent",    value = preset$accent)
  }

  observeEvent(input$preset_navy_teal,     apply_preset(presets$navy_teal))
  observeEvent(input$preset_indigo_violet, apply_preset(presets$indigo_violet))
  observeEvent(input$preset_emerald,       apply_preset(presets$emerald))
  observeEvent(input$preset_slate_coral,   apply_preset(presets$slate_coral))
  observeEvent(input$preset_burgundy,      apply_preset(presets$burgundy))
  observeEvent(input$preset_ocean,         apply_preset(presets$ocean))

  # ── Live preview ──────────────────────────────────────────────────────────
  output$color_preview_bar <- renderUI({
    p <- input$set_col_primary   %||% "#1B4F6B"
    s <- input$set_col_secondary %||% "#2EC4A5"
    a <- input$set_col_accent    %||% "#F59E0B"

    # Update preview swatches
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

  # ── Populate fields when trial is loaded ──────────────────────────────────
  observeEvent(rv$trial_config, {
    cfg <- rv$trial_config
    if (is.null(cfg)) return()

    cols <- cfg$colors %||% list(primary = "#1B4F6B", secondary = "#2EC4A5", accent = "#F59E0B")
    updateTextInput(session, "set_col_primary",   value = cols$primary)
    updateTextInput(session, "set_col_secondary", value = cols$secondary)
    updateTextInput(session, "set_col_accent",    value = cols$accent)

    feat <- cfg$features %||% list()
    updateCheckboxInput(session, "set_feat_projections", value = isTRUE(feat$projections))
    updateCheckboxInput(session, "set_feat_pilot",       value = isTRUE(feat$pilot_criteria))
    updateCheckboxInput(session, "set_feat_postal",      value = isTRUE(feat$postal_tracking))
    updateCheckboxInput(session, "set_feat_returns",     value = isTRUE(feat$return_rates))
    updateCheckboxInput(session, "set_feat_consort",     value = isTRUE(feat$consort_flow))
    updateCheckboxInput(session, "set_feat_baseline",    value = isTRUE(feat$baseline_table))

    updateTextInput(session,    "set_short_name", value = cfg$short_name %||% "")
    updateTextInput(session,    "set_full_name",  value = cfg$name %||% "")
    updateNumericInput(session, "set_target",     value = cfg$trial_target %||% 100)
    rd <- cfg$report_defaults %||% list()
    updateTextInput(session, "set_ci",      value = rd$ci %||% "")
    updateTextInput(session, "set_sponsor", value = rd$sponsor %||% "")
  })

  # ── Config file path display ──────────────────────────────────────────────
  output$settings_config_path <- renderUI({
    cfg <- rv$trial_config
    if (is.null(cfg)) return(span("No trial selected."))
    path <- file.path(cfg$trial_dir, "config.R")
    div(HTML(paste0("Config file: <code>", path, "</code>")))
  })

  # ── Save colours ─────────────────────────────────────────────────────────
  observeEvent(input$settings_save_colors, {
    cfg <- rv$trial_config
    if (is.null(cfg)) return()

    new_colors <- list(
      primary   = input$set_col_primary   %||% "#1B4F6B",
      secondary = input$set_col_secondary %||% "#2EC4A5",
      accent    = input$set_col_accent    %||% "#F59E0B"
    )

    save_config_field(cfg, "colors", new_colors)

    # Update in-memory config and apply colours immediately (no restart)
    rv$trial_config$colors <- new_colors
    apply_trial_colours(new_colors)

    showNotification(HTML("&#x2714; Colours saved and applied."),
                     type = "message", duration = 4)
  })

  # ── Save features ────────────────────────────────────────────────────────
  observeEvent(input$settings_save_features, {
    cfg <- rv$trial_config
    if (is.null(cfg)) return()

    # Update identity fields
    save_config_field(cfg, "short_name",    input$set_short_name %||% cfg$short_name)
    save_config_field(cfg, "name",          input$set_full_name  %||% cfg$name)
    save_config_field(cfg, "trial_target",  paste0(as.integer(input$set_target %||% 100), "L"))

    # Update features
    save_config_field(cfg, "features", list(
      postal_tracking  = isTRUE(input$set_feat_postal),
      return_rates     = isTRUE(input$set_feat_returns),
      projections      = isTRUE(input$set_feat_projections),
      pilot_criteria   = isTRUE(input$set_feat_pilot),
      consort_flow     = isTRUE(input$set_feat_consort),
      baseline_table   = isTRUE(input$set_feat_baseline)
    ))

    # Apply target immediately
    TRIAL_TARGET <<- as.integer(input$set_target %||% 100)

    # Toggle tab visibility immediately
    if (isTRUE(input$set_feat_postal)) shinyjs::show("go_postal_wrap") else shinyjs::hide("go_postal_wrap")
    if (isTRUE(input$set_feat_returns)) shinyjs::show("go_returns_wrap") else shinyjs::hide("go_returns_wrap")

    showNotification(HTML("&#x2714; Settings saved."), type = "message", duration = 4)
  })
}


# ── Helper: update a field in the trial's config.R ────────────────────────────
# This does a targeted find-and-replace in the config file rather than
# rewriting the whole thing, so comments and manual edits are preserved.

save_config_field <- function(cfg, field_name, new_value) {
  config_path <- file.path(cfg$trial_dir, "config.R")
  if (!file.exists(config_path)) return()

  lines <- readLines(config_path, warn = FALSE)

  # Format the new value as R code
  format_value <- function(val) {
    if (is.list(val)) {
      # Format as list(key = value, ...)
      items <- mapply(function(k, v) {
        if (is.logical(v)) sprintf("    %-18s= %s", k, toupper(as.character(v)))
        else if (is.numeric(v)) sprintf("    %-18s= %s", k, v)
        else sprintf('    %-18s= "%s"', k, v)
      }, names(val), val, SIMPLIFY = FALSE)
      paste0("list(\n", paste(items, collapse = ",\n"), "\n  )")
    } else if (is.numeric(new_value) || grepl("^\\d+L?$", as.character(new_value))) {
      as.character(new_value)
    } else {
      sprintf('"%s"', gsub('"', '\\\\"', as.character(val)))
    }
  }

  formatted <- format_value(new_value)

  # Strategy: find the line that starts the field assignment, replace it
  # For simple fields: field_name = value,
  # For list fields: field_name = list(...) spanning multiple lines

  pattern <- sprintf("^(\\s*)%s\\s*=", field_name)
  match_idx <- grep(pattern, lines)

  if (length(match_idx) == 0) {
    # Field not found — append before closing paren
    close_idx <- max(grep("^\\)", lines))
    if (length(close_idx) > 0) {
      indent <- "  "
      new_line <- sprintf("%s%s = %s,", indent, field_name, formatted)
      lines <- append(lines, new_line, after = close_idx - 1)
    }
  } else {
    idx <- match_idx[1]
    indent <- sub("^(\\s*).*", "\\1", lines[idx])

    if (is.list(new_value)) {
      # Find the end of the list block (closing paren + possible comma)
      depth <- 0; end_idx <- idx
      for (i in idx:length(lines)) {
        depth <- depth + nchar(gsub("[^(]", "", lines[i])) - nchar(gsub("[^)]", "", lines[i]))
        if (depth <= 0) { end_idx <- i; break }
      }
      replacement <- sprintf("%s%s = %s,", indent, field_name, formatted)
      lines <- c(lines[1:(idx - 1)], replacement, lines[(end_idx + 1):length(lines)])
    } else {
      lines[idx] <- sprintf("%s%s = %s,", indent, field_name, formatted)
    }
  }

  writeLines(lines, config_path)
}
