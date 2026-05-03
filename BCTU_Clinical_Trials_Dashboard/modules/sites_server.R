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

    updateSelectizeInput(session, "ns_name",       selected = "")
    updateTextInput(session,      "ns_id",         value = "")
    updateTextInput(session,      "ns_city",       value = "")
    updateTextInput(session,      "ns_region",     value = "")
    updateNumericInput(session,   "ns_rand",       value = 0)
    updateCheckboxInput(session,  "ns_siv_booked", value = FALSE)
  })

  observeEvent(input$delete_site, {
    sel <- input$manage_table__reactable__selected
    req(sel)
    if (sel < 1 || sel > nrow(rv$sites)) return()
    showNotification(paste("Site", rv$sites$site_id[sel], "removed."), type = "warning")
    rv$sites <- rv$sites[-sel, , drop = FALSE]
  })
}
