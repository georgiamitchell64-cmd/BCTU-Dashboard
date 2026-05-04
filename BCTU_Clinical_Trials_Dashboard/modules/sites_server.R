sites_server <- function(input, output, session, state) {
  rv <- state$rv

  # ── Auto-fill city when a known UK hospital is selected ──────────────────
  observeEvent(input$ns_name, {
    req(nzchar(trimws(input$ns_name %||% "")))
    city <- hospital_city(input$ns_name)
    if (!is.na(city)) {
      updateTextInput(session, "ns_city", value = city)
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # ── Manage table ──────────────────────────────────────────────────────────
  output$manage_table <- renderReactable({
    df <- rv$sites
    if (nrow(df) == 0) return(empty_reactable("No sites \u2014 load REDCap CSV or add manually."))

    # Ensure new columns exist for older saved data
    if (!"country"    %in% names(df)) df$country    <- NA_character_
    if (!"siv_booked" %in% names(df)) df$siv_booked <- FALSE
    if (!"siv_date"   %in% names(df)) df$siv_date   <- as.Date(NA)

    df <- df %>% mutate(
      Status = vapply(status, status_pill_html, character(1)),

      mo_edit = paste0(
        '<input type="number" class="editable-num" min="0" value="', monthly_target,
        '" onchange="Shiny.setInputValue(&quot;mo_tgt_edit&quot;,{id:&quot;', site_id,
        '&quot;,val:this.value},{priority:&quot;event&quot;})">'),
      city_edit = paste0(
        '<input type="text" class="editable-num" style="width:110px" value="', coalesce(city, ""),
        '" onchange="Shiny.setInputValue(&quot;city_edit&quot;,{id:&quot;', site_id,
        '&quot;,val:this.value},{priority:&quot;event&quot;})">'),
      region_edit = paste0(
        '<input type="text" class="editable-num" style="width:100px" value="', coalesce(region, ""),
        '" onchange="Shiny.setInputValue(&quot;region_edit&quot;,{id:&quot;', site_id,
        '&quot;,val:this.value},{priority:&quot;event&quot;})">'),
      country_edit = paste0(
        '<input type="text" class="editable-num" style="width:120px" value="', coalesce(country, ""),
        '" onchange="Shiny.setInputValue(&quot;country_edit&quot;,{id:&quot;', site_id,
        '&quot;,val:this.value},{priority:&quot;event&quot;})">'),

      site_id_edit = paste0(
        '<input type="text" class="editable-num" style="width:85px" value="', site_id,
        '" data-old="', site_id,
        '" onchange="Shiny.setInputValue(&quot;site_id_edit&quot;,',
        '{old_id:this.dataset.old,new_id:this.value},{priority:&quot;event&quot;})">'),

      target_edit = paste0(
        '<input type="number" class="editable-num" min="0" value="', target,
        '" onchange="Shiny.setInputValue(&quot;target_edit&quot;,{id:&quot;', site_id,
        '&quot;,val:this.value},{priority:&quot;event&quot;})">'),

      open_date_edit = paste0(
        '<input type="date" class="editable-num" style="width:130px" value="',
        coalesce(as.character(site_open_date), ""),
        '" onchange="Shiny.setInputValue(&quot;open_date_edit&quot;,{id:&quot;', site_id,
        '&quot;,val:this.value},{priority:&quot;event&quot;})">'),

      # SIV booked checkbox
      siv_booked_edit = paste0(
        '<input type="checkbox" ', ifelse(isTRUE(siv_booked), 'checked ', ''),
        'onchange="Shiny.setInputValue(&quot;siv_booked_edit&quot;,{id:&quot;', site_id,
        '&quot;,val:this.checked},{priority:&quot;event&quot;})" ',
        'style="width:18px;height:18px;cursor:pointer;accent-color:#2EC4A5;">'),

      # SIV date
      siv_date_edit = paste0(
        '<input type="date" class="editable-num" style="width:130px" value="',
        coalesce(as.character(siv_date), ""),
        '" onchange="Shiny.setInputValue(&quot;siv_date_edit&quot;,{id:&quot;', site_id,
        '&quot;,val:this.value},{priority:&quot;event&quot;})">')
    )

    reactable(
      df %>% select(site_id_edit, site_name, city_edit, region_edit, country_edit,
                    Status, siv_booked_edit, siv_date_edit,
                    mo_edit, target_edit, randomised, open_date_edit),
      striped = TRUE, highlight = TRUE, compact = TRUE,
      selection = "single", onClick = "select",
      defaultColDef = colDef(style = list(fontFamily = "Outfit", fontSize = "12.5px")),
      columns = list(
        site_id_edit    = colDef(name = "Site ID",        html = TRUE, minWidth = 100),
        site_name       = colDef(name = "Site",           minWidth = 160),
        city_edit       = colDef(name = "City",           html = TRUE, minWidth = 120),
        region_edit     = colDef(name = "Region",         html = TRUE, minWidth = 110),
        country_edit    = colDef(name = "Country",        html = TRUE, minWidth = 130),
        Status          = colDef(name = "Status",         html = TRUE, minWidth = 120),
        siv_booked_edit = colDef(name = "SIV booked",     html = TRUE, minWidth = 100, align = "center"),
        siv_date_edit   = colDef(name = "SIV date",       html = TRUE, minWidth = 140),
        mo_edit         = colDef(name = "Mo. target",     html = TRUE, minWidth = 100, align = "center"),
        target_edit     = colDef(name = "Overall target", html = TRUE, minWidth = 110, align = "center"),
        randomised      = colDef(name = "Randomised",     align = "center"),
        open_date_edit  = colDef(name = "Open date",      html = TRUE, minWidth = 150)
      )
    )
  })

  # ── Edit handlers ─────────────────────────────────────────────────────────
  observeEvent(input$mo_tgt_edit, {
    req(input$mo_tgt_edit$id)
    idx <- which(rv$sites$site_id == input$mo_tgt_edit$id)
    if (length(idx)) {
      rv$sites$monthly_target[idx] <- as.integer(input$mo_tgt_edit$val)
      showNotification(paste("Monthly target updated for", input$mo_tgt_edit$id), type = "message", duration = 2)
    }
  })

  # Re-geocode when city changes — use stored country for context
  observeEvent(input$city_edit, {
    req(input$city_edit$id)
    idx <- which(rv$sites$site_id == input$city_edit$id)
    if (length(idx)) {
      city_val <- str_to_title(trimws(input$city_edit$val))
      rv$sites$city[idx] <- city_val
      ctry <- rv$sites$country[idx]
      ll <- geocode_location(city_val, ctry)
      rv$sites$lat[idx] <- ll$lat
      rv$sites$lon[idx] <- ll$lon
      showNotification(paste("City updated for", input$city_edit$id), type = "message", duration = 2)
    }
  })

  observeEvent(input$region_edit, {
    req(input$region_edit$id)
    idx <- which(rv$sites$site_id == input$region_edit$id)
    if (length(idx)) {
      rv$sites$region[idx] <- trimws(input$region_edit$val)
      showNotification(paste("Region updated for", input$region_edit$id), type = "message", duration = 2)
    }
  })

  # Re-geocode when country changes
  observeEvent(input$country_edit, {
    req(input$country_edit$id)
    idx <- which(rv$sites$site_id == input$country_edit$id)
    if (length(idx)) {
      ctry_val <- trimws(input$country_edit$val)
      rv$sites$country[idx] <- ctry_val
      city_val <- rv$sites$city[idx]
      if (!is.na(city_val) && nzchar(city_val)) {
        ll <- geocode_location(city_val, ctry_val)
        rv$sites$lat[idx] <- ll$lat
        rv$sites$lon[idx] <- ll$lon
      }
      showNotification(paste("Country updated for", input$country_edit$id), type = "message", duration = 2)
    }
  })

  observeEvent(input$site_id_edit, {
    req(input$site_id_edit$old_id, input$site_id_edit$new_id)
    old_id <- trimws(input$site_id_edit$old_id)
    new_id <- trimws(input$site_id_edit$new_id)
    if (!nzchar(new_id) || new_id == old_id) return()
    if (new_id %in% rv$sites$site_id) {
      showNotification("Site ID already exists.", type = "error")
      return()
    }
    idx <- which(rv$sites$site_id == old_id)
    if (length(idx)) rv$sites$site_id[idx] <- new_id
    rv$log$site_id[rv$log$site_id == old_id] <- new_id
    showNotification(paste("Site ID:", old_id, "\u2192", new_id), type = "message", duration = 3)
  })

  observeEvent(input$target_edit, {
    req(input$target_edit$id)
    idx <- which(rv$sites$site_id == input$target_edit$id)
    if (length(idx)) {
      rv$sites$target[idx] <- as.integer(input$target_edit$val)
      showNotification(paste("Overall target updated for", input$target_edit$id), type = "message", duration = 2)
    }
  })

  observeEvent(input$open_date_edit, {
    req(input$open_date_edit$id)
    req(nzchar(input$open_date_edit$val %||% ""))
    idx <- which(rv$sites$site_id == input$open_date_edit$id)
    if (length(idx)) {
      new_date <- tryCatch(as.Date(input$open_date_edit$val), error = function(e) NA_Date_)
      if (!is.na(new_date)) {
        rv$sites$site_open_date[idx] <- new_date
        showNotification(paste("Open date updated for", input$open_date_edit$id), type = "message", duration = 2)
      }
    }
  })

  # SIV booked checkbox
  observeEvent(input$siv_booked_edit, {
    req(input$siv_booked_edit$id)
    idx <- which(rv$sites$site_id == input$siv_booked_edit$id)
    if (length(idx)) {
      rv$sites$siv_booked[idx] <- isTRUE(input$siv_booked_edit$val)
      showNotification(paste("SIV booked updated for", input$siv_booked_edit$id),
                       type = "message", duration = 2)
    }
  })

  # SIV date
  observeEvent(input$siv_date_edit, {
    req(input$siv_date_edit$id)
    idx <- which(rv$sites$site_id == input$siv_date_edit$id)
    if (length(idx)) {
      new_val <- input$siv_date_edit$val
      if (!nzchar(new_val %||% "")) {
        rv$sites$siv_date[idx] <- as.Date(NA)
      } else {
        new_date <- tryCatch(as.Date(new_val), error = function(e) NA_Date_)
        rv$sites$siv_date[idx] <- new_date
        # If a SIV date is set and the checkbox isn't ticked, auto-tick it
        if (!is.na(new_date) && !isTRUE(rv$sites$siv_booked[idx])) {
          rv$sites$siv_booked[idx] <- TRUE
        }
      }
      showNotification(paste("SIV date updated for", input$siv_date_edit$id),
                       type = "message", duration = 2)
    }
  })

  # ── Add / delete site ─────────────────────────────────────────────────────
  observeEvent(input$add_site, {
    req(input$ns_name)

    # Use city for map lookup; fall back to hospital name if city not entered
    lookup_name <- if (nzchar(trimws(input$ns_city %||% ""))) input$ns_city else input$ns_name
    country_val <- trimws(input$ns_country %||% "")
    ll <- geocode_location(lookup_name, country_val)

    new_id <- if (nzchar(trimws(input$ns_id %||% ""))) trimws(input$ns_id) else next_site_id(rv$sites)

    siv_date_val <- tryCatch({
      v <- input$ns_siv_date
      if (is.null(v) || length(v) == 0 || !nzchar(as.character(v))) as.Date(NA)
      else as.Date(v)
    }, error = function(e) as.Date(NA))

    rv$sites <- bind_rows(rv$sites, tibble(
      site_id        = new_id,
      site_name      = trimws(input$ns_name),
      city           = if (nzchar(trimws(input$ns_city %||% ""))) str_to_title(input$ns_city) else NA_character_,
      region         = if (nzchar(trimws(input$ns_region %||% ""))) input$ns_region else NA_character_,
      country        = if (nzchar(country_val)) country_val else NA_character_,
      status         = input$ns_status,
      site_open_date = tryCatch(as.Date(input$ns_open), error = function(e) NA_Date_),
      siv_booked     = isTRUE(input$ns_siv_booked),
      siv_date       = siv_date_val,
      monthly_target = as.integer(input$ns_mo_tgt),
      target         = as.integer(input$ns_tgt),
      randomised     = as.integer(input$ns_rand),
      lat = ll$lat, lon = ll$lon
    ))
    showNotification(paste("Site", new_id, "added."), type = "message")
    cfg <- rv$trial_config
    log_activity("site_added",
                 sprintf("Added site <strong>%s</strong>",
                         htmltools::htmlEscape(trimws(input$ns_name))),
                 username = rv$username,
                 trial_code = if (!is.null(cfg)) cfg$code else NULL)

    updateSelectizeInput(session, "ns_name",       selected = "")
    updateTextInput(session,      "ns_id",         value = "")
    updateTextInput(session,      "ns_city",       value = "")
    updateTextInput(session,      "ns_region",     value = "")
    updateNumericInput(session,   "ns_rand",       value = 0)
    updateCheckboxInput(session,  "ns_siv_booked", value = FALSE)
  })

  # ── Bulk add sites ────────────────────────────────────────────────────────
  # Parses a paste string OR an uploaded CSV. Each row becomes a site row
  # with sensible defaults. Skips duplicates (matched by site_name).

  observeEvent(input$bulk_add_sites, {
    if (!require_role(rv, "manager")) return()
    showModal(modalDialog(
      title = div(style = "display:flex;align-items:center;gap:10px;",
                  span(style = "font-size:18px;color:#6366F1;", HTML("&#x1F4CB;")),
                  span("Bulk add sites")),
      size = "l", easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("bulk_add_preview", "Preview",
                     class = "btn",
                     style = "background:#FFFFFF;color:#1B4F6B;
                              border:1px solid #DDE5EE;font-weight:500;"),
        actionButton("bulk_add_go", "Add sites",
                     class = "btn btn-primary",
                     style = "background:#6366F1;border-color:#6366F1;font-weight:600;")
      ),

      tabsetPanel(
        id = "bulk_add_mode",
        tabPanel("Paste",
          div(style = "padding:14px 0;",
              div(style = "font-size:12.5px;color:#475569;line-height:1.7;
                           margin-bottom:10px;",
                  HTML("One site per line. Extra columns are optional, separated by
                        <code>|</code> (pipe). Format:")),
              tags$pre(style = "background:#F8FAFD;border:1px solid #EEF2F7;
                                padding:10px 12px;border-radius:6px;font-size:11.5px;
                                color:#475569;line-height:1.6;",
                       "Site name | City | Country | Status | Monthly target | Overall target",
                       "\nQueen Elizabeth Hospital Birmingham | Birmingham | United Kingdom",
                       "\nLeeds General Infirmary | Leeds | United Kingdom | Recruiting | 3 | 60",
                       "\nManchester Royal Infirmary"),
              textAreaInput("bulk_paste", label = NULL,
                            placeholder = "Paste site list here…",
                            rows = 10, width = "100%"))),
        tabPanel("CSV upload",
          div(style = "padding:14px 0;",
              div(style = "font-size:12.5px;color:#475569;line-height:1.7;
                           margin-bottom:10px;",
                  HTML("CSV with at least a <code>site_name</code> column.
                        Optional columns: <code>city</code>, <code>region</code>,
                        <code>country</code>, <code>status</code>,
                        <code>monthly_target</code>, <code>target</code>.")),
              fileInput("bulk_csv", label = NULL, accept = ".csv",
                        buttonLabel = "Choose CSV"))),
        tabPanel("Defaults",
          div(style = "padding:14px 0;",
              div(style = "font-size:12.5px;color:#475569;margin-bottom:14px;",
                  "Applied to every site that doesn't specify these explicitly."),
              div(style = "display:grid;grid-template-columns:1fr 1fr 1fr;gap:14px;",
                  selectInput("bulk_def_status", "Default status",
                              choices = c("Identified", "Set-up", "Open",
                                          "Recruiting", "Closed"),
                              selected = "Identified"),
                  numericInput("bulk_def_monthly", "Default monthly target",
                               value = 0, min = 0),
                  numericInput("bulk_def_target", "Default overall target",
                               value = 0, min = 0)))
        )
      ),

      div(style = "margin-top:16px;",
          uiOutput("bulk_preview_ui"))
    ))
  })

  .parse_bulk_paste <- function(txt) {
    if (is.null(txt) || !nzchar(trimws(txt))) return(data.frame())
    lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
    lines <- trimws(lines)
    lines <- lines[nzchar(lines)]
    if (!length(lines)) return(data.frame())

    rows <- lapply(lines, function(ln) {
      parts <- trimws(strsplit(ln, "|", fixed = TRUE)[[1]])
      data.frame(
        site_name      = parts[1],
        city           = if (length(parts) >= 2) parts[2] else NA_character_,
        country        = if (length(parts) >= 3) parts[3] else NA_character_,
        status         = if (length(parts) >= 4) parts[4] else NA_character_,
        monthly_target = if (length(parts) >= 5) suppressWarnings(as.integer(parts[5])) else NA_integer_,
        target         = if (length(parts) >= 6) suppressWarnings(as.integer(parts[6])) else NA_integer_,
        stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
  }

  .parse_bulk_csv <- function(path) {
    if (is.null(path) || !file.exists(path)) return(data.frame())
    df <- tryCatch(read.csv(path, stringsAsFactors = FALSE,
                            check.names = FALSE),
                   error = function(e) data.frame())
    if (!nrow(df)) return(df)
    # Lowercase column names for matching
    names(df) <- tolower(names(df))
    if (!"site_name" %in% names(df)) {
      showNotification("CSV is missing a site_name column.",
                       type = "warning", duration = 6)
      return(data.frame())
    }
    cols <- c("site_name", "city", "region", "country", "status",
              "monthly_target", "target")
    for (c in setdiff(cols, names(df))) df[[c]] <- NA
    df[, cols, drop = FALSE]
  }

  bulk_parsed <- reactive({
    mode <- input$bulk_add_mode
    if (identical(mode, "Paste")) .parse_bulk_paste(input$bulk_paste)
    else if (identical(mode, "CSV upload")) {
      f <- input$bulk_csv
      if (is.null(f)) data.frame() else .parse_bulk_csv(f$datapath)
    } else data.frame()
  })

  output$bulk_preview_ui <- renderUI({
    parsed <- bulk_parsed()
    existing <- rv$sites$site_name %||% character(0)

    if (!nrow(parsed)) {
      return(div(style = "font-size:12px;color:#94A3B8;font-style:italic;
                          padding:8px 0;",
                 "Paste a site list or upload a CSV — preview will appear here."))
    }

    valid <- !is.na(parsed$site_name) & nzchar(trimws(parsed$site_name))
    parsed <- parsed[valid, , drop = FALSE]
    dups   <- parsed$site_name %in% existing
    n_new  <- sum(!dups)
    n_dup  <- sum(dups)

    chip <- function(n, label, bg, fg) {
      span(style = sprintf("display:inline-flex;align-items:center;gap:5px;
                            background:%s;color:%s;padding:3px 9px;border-radius:999px;
                            font-size:11px;font-weight:600;margin-right:6px;",
                           bg, fg),
           sprintf("%d %s", n, label))
    }

    div(style = "background:#FFFFFF;border:1px solid #EEF2F7;border-radius:10px;
                 padding:10px 14px;",
        div(style = "display:flex;justify-content:space-between;align-items:center;
                     margin-bottom:8px;",
            div(chip(n_new, "new",      "#ECFDF5", "#15803D"),
                if (n_dup) chip(n_dup, "duplicate", "#FEF3C7", "#92400E"),
                if (sum(!valid)) chip(sum(!valid), "skipped", "#FEE2E2", "#B91C1C")),
            span(style = "font-size:11px;color:#94A3B8;",
                 sprintf("%d total parsed", nrow(parsed) + sum(!valid)))),
        div(style = "max-height:160px;overflow-y:auto;font-size:11.5px;",
            lapply(seq_len(nrow(parsed)), function(i) {
              r <- parsed[i, ]
              is_dup <- dups[i]
              div(style = sprintf("padding:5px 0;border-top:1px solid #EEF2F7;
                                   color:%s;",
                                  if (is_dup) "#94A3B8" else "#0F172A"),
                  span(style = "font-weight:500;", r$site_name),
                  if (!is.na(r$city) && nzchar(r$city))
                    span(style = "color:#64748B;",
                         sprintf(" · %s", r$city)),
                  if (is_dup)
                    span(style = "color:#92400E;float:right;",
                         "already exists"))
            })))
  })

  observeEvent(input$bulk_add_preview, {
    # No-op: previews update live; this just nudges users
  })

  observeEvent(input$bulk_add_go, {
    if (!require_role(rv, "manager")) return()
    parsed <- bulk_parsed()
    if (!nrow(parsed)) {
      showNotification("Nothing to add — paste site names or upload a CSV.",
                       type = "warning")
      return()
    }
    valid <- !is.na(parsed$site_name) & nzchar(trimws(parsed$site_name))
    parsed <- parsed[valid, , drop = FALSE]
    existing <- rv$sites$site_name %||% character(0)
    new_rows <- parsed[!parsed$site_name %in% existing, , drop = FALSE]
    if (!nrow(new_rows)) {
      showNotification("All listed sites already exist.",
                       type = "warning", duration = 5)
      return()
    }

    def_status  <- input$bulk_def_status %||% "Identified"
    def_monthly <- as.integer(input$bulk_def_monthly %||% 0)
    def_target  <- as.integer(input$bulk_def_target %||% 0)

    add_blocks <- lapply(seq_len(nrow(new_rows)), function(i) {
      r <- new_rows[i, ]
      country_val <- if (is.na(r$country) || !nzchar(r$country))
        "United Kingdom" else r$country
      lookup <- if (!is.na(r$city) && nzchar(r$city)) r$city else r$site_name
      ll <- tryCatch(geocode_location(lookup, country_val),
                     error = function(e) list(lat = NA_real_, lon = NA_real_))
      tibble(
        site_id        = next_site_id(rv$sites),
        site_name      = trimws(r$site_name),
        city           = if (!is.na(r$city) && nzchar(r$city))
                           str_to_title(r$city) else NA_character_,
        region         = NA_character_,
        country        = country_val,
        status         = if (!is.na(r$status) && nzchar(r$status))
                           r$status else def_status,
        site_open_date = as.Date(NA),
        siv_booked     = FALSE,
        siv_date       = as.Date(NA),
        monthly_target = if (!is.na(r$monthly_target))
                           r$monthly_target else def_monthly,
        target         = if (!is.na(r$target)) r$target else def_target,
        randomised     = 0L,
        lat            = ll$lat,
        lon            = ll$lon
      )
    })

    # next_site_id is computed against rv$sites — but we're adding multiple
    # in one batch. Re-id sequentially against the running total.
    base <- nrow(rv$sites)
    for (i in seq_along(add_blocks)) {
      rv$sites <- bind_rows(rv$sites, add_blocks[[i]])
    }

    removeModal()
    cfg <- rv$trial_config
    log_activity("sites_bulk_added",
                 sprintf("Bulk-added <strong>%d</strong> sites",
                         nrow(new_rows)),
                 username = rv$username,
                 trial_code = if (!is.null(cfg)) cfg$code else NULL,
                 metadata = list(skipped = nrow(parsed) - nrow(new_rows)))
    showNotification(
      sprintf("Added %d %s (%d skipped as duplicates).",
              nrow(new_rows),
              if (nrow(new_rows) == 1) "site" else "sites",
              nrow(parsed) - nrow(new_rows)),
      type = "message", duration = 6)
  })

  observeEvent(input$delete_site, {
    if (!require_role(rv, "manager")) return()
    sel <- input$manage_table__reactable__selected
    req(sel)
    if (sel < 1 || sel > nrow(rv$sites)) return()
    deleted_id   <- rv$sites$site_id[sel]
    deleted_name <- rv$sites$site_name[sel]
    showNotification(paste("Site", deleted_id, "removed."), type = "warning")
    rv$sites <- rv$sites[-sel, , drop = FALSE]
    cfg <- rv$trial_config
    log_activity("site_deleted",
                 sprintf("Removed site <strong>%s</strong>",
                         htmltools::htmlEscape(deleted_name %||% deleted_id)),
                 username = rv$username,
                 trial_code = if (!is.null(cfg)) cfg$code else NULL)
  })
}
