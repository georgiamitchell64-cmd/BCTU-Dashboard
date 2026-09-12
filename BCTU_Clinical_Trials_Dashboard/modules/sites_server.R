sites_server <- function(input, output, session, state) {
  rv <- state$rv

  editing_orig_id  <- reactiveVal(NULL)   # site_id being edited (NULL = adding)
  pending_delete_id <- reactiveVal(NULL)

  # ── Site health (shared th_build via state$health) ──────────────────────
  output$site_health_ui <- renderUI({
    H <- state$health()
    if (is.null(H) || is.null(H$sites) || !nrow(H$sites)) return(NULL)
    tagList(
      div(class = "th-section",
        div(class = "th-section-head",
          div(tags$h3("Site health"),
              div(class = "th-sub-t",
                  "Each site's change-of-status and overdue-CRF rates against its size. A site outside the funnel is unusually high for its size, not just small. Click a site for details."))),
        th_funnel_widget(H, "th-funnel-sites")),
      div(class = "th-section",
        div(class = "th-section-head",
          div(tags$h3("Site scorecards"),
              div(class = "th-sub-t",
                  "Scored on recruitment against target, retention, CRF returns and recent activity. Weights and thresholds are in Settings → Recruitment & monitoring."))),
        th_site_cards(H)))
  })

  # Open date per site: the Sites tab's, or — for a site that has recruited but
  # has none recorded, usually one created from REDCap — its first
  # randomisation, flagged as an estimate. The same rule as the TMG report.
  .open_dates <- function(df) {
    od  <- suppressWarnings(as.Date(df$site_open_date))
    est <- rep(FALSE, nrow(df))
    H <- tryCatch(state$health(), error = function(e) NULL)
    P <- if (is.null(H)) NULL else H$participants
    if (!is.null(P) && nrow(P)) {
      n <- suppressWarnings(as.numeric(df$randomised))
      for (i in which(is.na(od) & !is.na(n) & n > 0 & !is.na(df$site_name))) {
        r <- P$rand[!is.na(P$site) & P$site == df$site_name[i]]
        if (length(r)) {
          od[i]  <- as.Date(as.POSIXct(min(r), origin = "1970-01-01", tz = "UTC"))
          est[i] <- TRUE
        }
      }
    }
    list(date = od, est = est)
  }

  # ── Summary stats tiles ─────────────────────────────────────────────────
  output$sites_summary_stats <- renderUI({
    df <- rv$sites
    if (nrow(df) == 0) return(NULL)
    n_recruiting <- sum(df$status == "Recruiting", na.rm = TRUE)
    n_setup      <- sum(df$status %in% c("Set-up", "Identified"), na.rm = TRUE)
    n_paused     <- sum(df$status == "Paused", na.rm = TRUE)
    n_closed     <- sum(df$status == "Closed", na.rm = TRUE)
    total_rand   <- sum(df$randomised, na.rm = TRUE)
    src          <- ifelse(is.na(df$source) | df$source == "", "auto", df$source)
    n_manual     <- sum(src == "manual")
    # A missing open date is only a data gap for a site that has actually
    # opened. Identified and Set-up sites have not opened yet, so leaving the
    # date blank there is expected and must not raise an "Incomplete" flag.
    opened_statuses <- c("Open", "Recruiting", "Paused", "Closed")
    missing_open    <- is.na(df$site_open_date) & df$status %in% opened_statuses
    n_flagged       <- sum(is.na(df$city) | missing_open, na.rm = TRUE)

    # Actual monthly average recruits per site — each site's randomisations over
    # the months it has been open, averaged across the sites that have an open
    # date recorded. A site without one has no denominator, so it is left out
    # rather than counted as zero.
    per_site_rates <- .actual_monthly_per_site(df$randomised, .open_dates(df)$date)
    avg_per_site   <- if (any(!is.na(per_site_rates)))
                        mean(per_site_rates, na.rm = TRUE) else NA_real_
    avg_label      <- if (is.na(avg_per_site)) "—" else
                        formatC(avg_per_site, format = "f", digits = 1)

    make_stat <- function(value, label, color) {
      div(class = "sites-stat",
          div(class = "sites-stat-v", style = sprintf("color:%s;", color), value),
          div(class = "sites-stat-l", label))
    }
    div(class = "sites-stats-row",
        make_stat(nrow(df),      "Total sites", "#1B1B1B"),
        make_stat(n_recruiting,  "Recruiting",  "#3AAA35"),
        make_stat(n_setup,       "In set-up",   "#8A8A8C"),
        make_stat(n_paused,      "Paused",      "#F07F3C"),
        make_stat(n_closed,      "Closed",      "#58595B"),
        make_stat(avg_label,     "Avg recruits / site / month", "#00788E"),
        make_stat(total_rand, recruit_term("Past", rv$trial_config), "#1B1B1B"),
        if (n_flagged > 0) make_stat(n_flagged, "Incomplete", "#C20019"))
  })

  # ── Monthly recruitment per site ────────────────────────────────────────
  # Each site's randomisations per month since it opened — the same figure as
  # the "Avg recruits / site / month" tile and the TMG report's monthly-average
  # column (.actual_monthly_per_site) — against its monthly target, with the
  # last 90 days alongside so a site that is slowing down shows up.
  output$sites_monthly_ui <- renderUI({
    df <- rv$sites
    if (is.null(df) || !nrow(df)) return(NULL)
    section <- function(...) div(class = "th-section",
      div(class = "th-section-head",
        div(tags$h3("Monthly recruitment per site"),
            div(class = "th-sub-t",
                "Randomisations per month since each site opened, against its monthly target, with the last 90 days alongside. Click a site for its details."))),
      ...)
    jsq  <- function(x) gsub("'", "\\\\'", gsub("\\\\", "\\\\\\\\", as.character(x)))
    num1 <- function(x) if (is.na(x)) "—" else formatC(x, format = "f", digits = 1)
    H <- tryCatch(state$health(), error = function(e) NULL)
    P <- if (is.null(H)) NULL else H$participants

    name <- as.character(df$site_name)
    opn  <- .open_dates(df)
    od   <- opn$date
    rate <- .actual_monthly_per_site(df$randomised, od)
    tgt  <- suppressWarnings(as.numeric(df$monthly_target))
    days_open <- as.numeric(Sys.Date() - od)
    last90 <- if (is.null(P) || !nrow(P)) rep(NA_integer_, nrow(df)) else {
      from <- .th_today_s(H$today) - 90 * .TH_DAY
      vapply(name, function(s) sum(P$site == s & P$rand > from), integer(1), USE.NAMES = FALSE)
    }
    # Pace over the last 90 days, or since opening for a site open less than that
    recent <- ifelse(!is.na(days_open) & days_open >= 30 & !is.na(last90),
                     last90 / (pmin(days_open, 90) / 30.44), NA_real_)

    ok <- !is.na(rate)
    if (!any(ok)) return(section(th_empty_note(
      "Add an open date to each site to see its monthly recruitment.")))
    d <- data.frame(site = name, status = as.character(df$status), rate = rate, target = tgt,
                    recent = recent, last90 = last90, est = opn$est,
                    stringsAsFactors = FALSE)[ok, , drop = FALSE]
    d$ratio <- ifelse(!is.na(d$target) & d$target > 0, d$rate / d$target, NA_real_)
    d <- d[order(-d$rate, d$site), , drop = FALSE]

    # ── Headline figures ──
    avg_rate   <- mean(d$rate)
    avg_target <- if (any(!is.na(d$target))) mean(d$target, na.rm = TRUE) else NA_real_
    avg_recent <- if (any(!is.na(d$recent))) mean(d$recent, na.rm = TRUE) else NA_real_
    n_tgt <- sum(!is.na(d$ratio)); n_met <- sum(d$ratio >= 1, na.rm = TRUE)
    low   <- d$site[!is.na(d$ratio) & d$ratio < 0.6]
    open_now <- df$status %in% c("Open", "Recruiting")
    trial_pace <- if (all(is.na(last90))) NA_real_ else sum(last90, na.rm = TRUE) / (90 / 30.44)
    trial_tgt  <- sum(tgt[open_now], na.rm = TRUE)
    kpi <- function(l, v, s, cls = NULL) div(class = paste(c("sm-kpi", cls), collapse = " "),
      div(class = "sm-kpi-l", l), div(class = "sm-kpi-v", v), div(class = "sm-kpi-s", s))
    kpis <- div(class = "sm-kpis",
      kpi("Average per site", num1(avg_rate),
          if (is.na(avg_target)) "per month since opening"
          else sprintf("per month since opening · target %s", num1(avg_target))),
      kpi("Last 90 days", num1(avg_recent),
          if (is.na(avg_recent)) "no recent data"
          else HTML(sprintf("<span class='%s'>%s</span> per site per month",
                            if (avg_recent >= avg_rate) "sm-up" else "sm-down",
                            if (avg_recent >= avg_rate) "&#9650; faster than overall" else "&#9660; slower than overall"))),
      kpi("At or above target", if (n_tgt) sprintf("%d of %d", n_met, n_tgt) else "—",
          if (length(low)) sprintf("%d well below (under 60%%): %s", length(low),
                                   paste(utils::head(low, 3), collapse = ", "))
          else "no site is well below target",
          if (length(low)) "warn"),
      kpi("Whole trial, last 90 days", num1(trial_pace),
          if (trial_tgt > 0) sprintf("per month · open sites' targets add up to %s", num1(trial_tgt))
          else "per month"))

    # ── One row per site: bar = rate, line = target, ring = last 90 days ──
    mx <- max(c(d$rate, d$target, d$recent), na.rm = TRUE)
    if (!is.finite(mx) || mx <= 0) mx <- 1
    at <- function(v) sprintf("%.1f%%", 100 * min(v, mx) / mx)
    rows <- lapply(seq_len(nrow(d)), function(i) {
      r <- d[i, ]
      cls <- if (is.na(r$ratio)) "" else if (r$ratio >= 1) "met" else if (r$ratio < 0.6) "low" else "near"
      tags$button(type = "button", class = paste("sm-row", cls),
        onclick = sprintf("Shiny.setInputValue('th_site_open',{site:'%s',n:Math.random()},{priority:'event'})",
                          jsq(r$site)),
        title = sprintf("%s: %s a month since opening%s%s", r$site, num1(r$rate),
                        if (!is.na(r$target)) sprintf(" (target %s)", num1(r$target)) else "",
                        if (!is.na(r$recent)) sprintf(", %s over the last 90 days", num1(r$recent)) else ""),
        span(class = "sm-site", r$site,
             span(class = "sm-status", if (isTRUE(r$est)) paste(r$status, "· open date estimated*") else r$status)),
        span(class = "sm-bar",
             span(class = "sm-bar-f", style = sprintf("width:%s;", at(r$rate))),
             if (!is.na(r$target)) span(class = "sm-tgt", style = sprintf("left:%s;", at(r$target))),
             if (!is.na(r$recent)) span(class = "sm-rec", style = sprintf("left:%s;", at(r$recent)))),
        span(class = "sm-v", num1(r$rate),
             if (!is.na(r$target)) span(class = "sm-of", sprintf(" / %s", num1(r$target)))),
        span(class = "sm-r", num1(r$recent)))
    })
    n_skip <- sum(!ok & df$status %in% c("Open", "Recruiting", "Paused", "Closed"))
    section(
      kpis,
      div(class = "sm-legend",
          span(span(class = "sm-lg-bar"), "per month since opening"),
          span(span(class = "sm-lg-tgt"), "monthly target"),
          span(span(class = "sm-lg-rec"), "last 90 days"),
          span(span(class = "sm-lg-bar low"), "under 60% of target"),
          span(span(class = "sm-lg-bar near"), "60–99%")),
      div(class = "sm-rows",
          div(class = "sm-row sm-head", span("Site"), span("Per month"),
              span(class = "sm-v", "Rate / target"), span(class = "sm-r", "Last 90 days")),
          rows),
      div(class = "sm-foot",
          "Per month is randomisations divided by the calendar months a site has been open, counting its opening month — the same figure as the TMG report.",
          if (any(d$est)) " * No open date on the Sites tab, so the first randomisation is used — add the real date to correct it.",
          if (n_skip) sprintf(" %d open site%s without an open date %s left out.", n_skip,
                              if (n_skip == 1) "" else "s", if (n_skip == 1) "is" else "are")))
  })

  # ── Filtered view (search box + status chips, both wired from sites.R JS) ─
  sites_filtered <- reactive({
    df <- rv$sites
    if (is.null(df) || !nrow(df)) return(df)
    q <- tolower(trimws(input$sites_search %||% ""))
    if (nzchar(q)) {
      hay <- tolower(paste(dplyr::coalesce(df$site_name, ""), dplyr::coalesce(df$site_id, ""),
                           dplyr::coalesce(df$city, ""), dplyr::coalesce(df$region, ""),
                           dplyr::coalesce(df$country, "")))
      df <- df[grepl(q, hay, fixed = TRUE), , drop = FALSE]
    }
    sf <- input$sites_status_filter
    if (!is.null(sf) && length(sf)) df <- df[df$status %in% sf, , drop = FALSE]
    df
  })

  .status_cls <- function(s) switch(s %||% "",
    "Recruiting" = "sr-st-rec", "Open" = "sr-st-open", "Set-up" = "sr-st-setup",
    "Paused" = "sr-st-paused", "Closed" = "sr-st-closed", "sr-st-id")

  # Render one site row (read-only; left-click edits, right-click → menu).
  .site_row <- function(s) {
    gv <- function(x, d = NULL) if (is.null(x) || length(x) == 0 || is.na(x[1])) d else x[1]
    status <- gv(s$status, "Identified")
    parts  <- c(gv(s$city), gv(s$country)); loc <- paste(parts[nzchar(parts)], collapse = ", ")
    sid    <- gv(s$site_id, "")
    metric <- function(v, l) div(class = "sr-metric",
                                 div(class = "sr-metric-v", if (is.null(v)) "—" else v),
                                 div(class = "sr-metric-l", l))
    div(class = "site-row", `data-id` = sid,
        onclick      = sprintf("siteOpenEdit('%s')", sid),
        oncontextmenu = sprintf("siteShowCtx(event,'%s')", sid),
        div(class = "sr-main",
            span(class = paste("sr-status", .status_cls(status)), status),
            div(class = "sr-name-wrap",
                div(class = "sr-name", gv(s$site_name, sid)),
                div(class = "sr-sub",
                    paste0(sid, if (nzchar(loc)) paste0("  ·  ", loc) else "")))),
        div(class = "sr-metrics",
            metric(gv(s$randomised, 0), "randomised"),
            metric(gv(s$monthly_target), "mo. target"),
            metric(gv(s$target), "overall")),
        tags$button(class = "sr-kebab", type = "button", title = "Actions",
                    onclick = sprintf("event.stopPropagation(); siteShowCtx(event,'%s')", sid),
                    HTML("&#8942;")))
  }

  # ── Grouped tables: manual vs auto-populated ────────────────────────────
  output$sites_groups_ui <- renderUI({
    df <- sites_filtered()
    if (is.null(df) || !nrow(rv$sites))
      return(div(class = "sites-empty",
                 "No sites yet — add one with “+ Add site” or load a REDCap CSV."))
    if (!nrow(df))
      return(div(class = "sites-empty", "No sites match your search or filters."))

    src    <- ifelse(is.na(df$source) | df$source == "", "auto", df$source)
    manual <- df[src == "manual", , drop = FALSE]
    auto   <- df[src != "manual", , drop = FALSE]

    grp <- function(icon, title, sub, rows_df) {
      if (!nrow(rows_df)) return(NULL)
      tagList(
        div(class = "sites-group-head",
            div(class = "sgh-l", HTML(icon),
                span(class = "sgh-title", title),
                span(class = "sgh-count", nrow(rows_df))),
            span(class = "sgh-sub", sub)),
        div(class = "sites-group",
            lapply(seq_len(nrow(rows_df)), function(i) .site_row(rows_df[i, ]))))
    }
    tagList(
      grp('<svg width="15" height="15" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M16 21v-2a4 4 0 0 0-4-4H6a4 4 0 0 0-4 4v2"/><circle cx="9" cy="7" r="4"/><path d="M19 8v6M22 11h-6"/></svg>',
          "Added on the Sites tab", "Entered by hand — usually identified or in set-up, and not in the REDCap export yet", manual),
      grp('<svg width="15" height="15" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><ellipse cx="12" cy="5" rx="9" ry="3"/><path d="M3 5v14a9 3 0 0 0 18 0V5"/><path d="M3 12a9 3 0 0 0 18 0"/></svg>',
          "From the REDCap export", "Created automatically from the export's data access groups", auto)
    )
  })

  # Auto-fill city when a known UK hospital is chosen in the modal.
  observeEvent(input$se_name, {
    req(nzchar(trimws(input$se_name %||% "")))
    if (nzchar(trimws(input$se_city %||% ""))) return()
    city <- hospital_city(input$se_name)
    if (!is.na(city)) updateTextInput(session, "se_city", value = city)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # ── Open the add / edit modal ───────────────────────────────────────────
  observeEvent(input$add_site_open, {
    editing_orig_id(NULL)
    showModal(site_edit_modal(NULL, is_new = TRUE))
  })

  observeEvent(input$site_edit_open, {
    id  <- input$site_edit_open$id
    # NA-safe exact match. A plain `site_id == id` subset turns any NA site_id
    # into a phantom all-NA row, which would open the modal blank.
    idx <- which(!is.na(rv$sites$site_id) & rv$sites$site_id == id)
    if (!length(idx)) {
      showNotification("Couldn't find that site to edit.", type = "warning")
      return()
    }
    editing_orig_id(id)
    showModal(site_edit_modal(rv$sites[idx[1], , drop = FALSE], is_new = FALSE))
  })

  # ── Save (add or update) ────────────────────────────────────────────────
  observeEvent(input$site_edit_save, {
    name <- trimws(input$se_name %||% "")
    if (!nzchar(name)) { showNotification("Enter a site name.", type = "warning"); return() }

    orig   <- editing_orig_id()
    is_new <- is.null(orig)
    new_id <- trimws(input$se_id %||% "")
    if (!nzchar(new_id)) new_id <- if (is_new) next_site_id(rv$sites) else orig

    other_ids <- rv$sites$site_id[rv$sites$site_id != (orig %||% "")]
    if (new_id %in% other_ids) {
      showNotification("That Site ID already exists.", type = "error"); return()
    }

    city    <- trimws(input$se_city %||% "")
    country <- trimws(input$se_country %||% "")
    # Geocode the hospital first, and only fall back to the city. Geocoding the
    # city gave every site in a town the identical centroid, so their map
    # markers landed exactly on top of one another. .uk_hospital_coords holds
    # per-hospital coordinates, so the site name resolves to the actual site.
    ll <- tryCatch({
      by_site <- if (nzchar(name)) hospital_latlon(name) else
                   list(lat = NA_real_, lon = NA_real_)
      if (!is.na(by_site$lat)) by_site
      else geocode_location(if (nzchar(city)) city else name, country)
    }, error = function(e) list(lat = NA_real_, lon = NA_real_))
    # Clearing a dateInput sends a zero-length value, not NULL or "". The old
    # guard ran that through nzchar(), producing logical(0), and `if` on a
    # zero-length condition errors — which crashed the save when either date
    # was left blank. Treat anything empty, NA or unparseable as "no date".
    as_d <- function(x) {
      if (is.null(x) || length(x) == 0) return(as.Date(NA))
      x <- x[1]
      if (is.na(x) || !nzchar(as.character(x))) return(as.Date(NA))
      d <- tryCatch(suppressWarnings(as.Date(x)), error = function(e) as.Date(NA))
      if (length(d) == 0) as.Date(NA) else d[1]
    }
    src <- if (is_new) "manual" else {
      sidx <- which(!is.na(rv$sites$site_id) & rv$sites$site_id == orig)
      s <- if (length(sidx)) rv$sites$source[sidx[1]] else NA_character_
      if (!is.na(s) && nzchar(s)) s else "manual"
    }

    row <- tibble(
      site_id        = new_id,
      site_name      = name,
      city           = if (nzchar(city)) str_to_title(city) else NA_character_,
      region         = { r <- trimws(input$se_region %||% ""); if (nzchar(r)) r else NA_character_ },
      country        = if (nzchar(country)) country else NA_character_,
      status         = input$se_status %||% "Identified",
      site_open_date = as_d(input$se_open),
      monthly_target = as.integer(input$se_mo_tgt %||% 0),
      target         = as.integer(input$se_tgt %||% 0),
      randomised     = as.integer(input$se_rand %||% 0),
      lat            = ll$lat,
      lon            = ll$lon,
      source         = src
    )

    cfg <- rv$trial_config
    if (is_new) {
      rv$sites <- bind_rows(rv$sites, row)
      log_activity("site_added",
                   sprintf("Added site <strong>%s</strong>", htmltools::htmlEscape(name)),
                   username = rv$username, trial_code = if (!is.null(cfg)) cfg$code else NULL)
      showNotification(sprintf("Site “%s” added.", name), type = "message", duration = 4)
    } else {
      idx <- which(rv$sites$site_id == orig)
      if (length(idx)) {
        for (col in names(row)) rv$sites[[col]][idx] <- row[[col]][1]
        if (!identical(orig, new_id))
          rv$log$site_id[rv$log$site_id == orig] <- new_id
      }
      showNotification(sprintf("“%s” updated.", name), type = "message", duration = 3)
    }
    editing_orig_id(NULL)
    removeModal()
  })

  # ── Delete (from the modal or the right-click menu) ─────────────────────
  .confirm_delete <- function(id) {
    idx <- which(!is.na(rv$sites$site_id) & rv$sites$site_id == id)
    nm  <- if (length(idx)) (rv$sites$site_name[idx[1]] %||% id) else id
    pending_delete_id(id)
    showModal(modalDialog(
      title = div(style = "color:#C20019;", HTML("&#x26A0; Delete site?")),
      size = "s", easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("site_delete_confirm", "Yes, delete",
                     class = "btn btn-danger",
                     style = "background:#C20019;border-color:#C20019;font-weight:600;")),
      div(style = "font-size:13px;line-height:1.6;",
          HTML(sprintf("Permanently remove <strong>%s</strong>? This can't be undone.",
                       htmltools::htmlEscape(nm))))))
  }

  observeEvent(input$site_delete_ask, {
    if (!require_role(rv, "manager")) return()
    .confirm_delete(input$site_delete_ask$id)
  })
  observeEvent(input$site_edit_delete, {
    id <- editing_orig_id(); if (is.null(id)) return()
    removeModal()
    if (!require_role(rv, "manager")) return()
    .confirm_delete(id)
  })
  observeEvent(input$site_delete_confirm, {
    id <- pending_delete_id(); req(id)
    idx <- which(rv$sites$site_id == id)
    if (length(idx)) {
      nm <- rv$sites$site_name[idx] %||% id
      rv$sites <- rv$sites[-idx, , drop = FALSE]
      cfg <- rv$trial_config
      log_activity("site_deleted",
                   sprintf("Removed site <strong>%s</strong>", htmltools::htmlEscape(nm)),
                   username = rv$username, trial_code = if (!is.null(cfg)) cfg$code else NULL)
      showNotification(sprintf("Site “%s” removed.", nm), type = "warning", duration = 4)
    }
    pending_delete_id(NULL)
    removeModal()
  })

  # ── Bulk add sites (paste / CSV) ────────────────────────────────────────
  observeEvent(input$bulk_add_sites, {
    if (!require_role(rv, "manager")) return()
    showModal(modalDialog(
      title = div(style = "display:flex;align-items:center;gap:10px;",
                  span(style = "font-size:18px;color:#1B1B1B;", HTML("&#x1F4CB;")),
                  span("Bulk add sites")),
      size = "l", easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("bulk_add_go", "Add sites",
                     class = "btn btn-primary",
                     style = "background:#1B1B1B;border-color:#1B1B1B;font-weight:600;")
      ),
      tabsetPanel(
        id = "bulk_add_mode",
        tabPanel("Paste",
          div(style = "padding:14px 0;",
              div(style = "font-size:12.5px;color:#4A4A4A;line-height:1.7;margin-bottom:10px;",
                  HTML("One site per line. Extra columns optional, separated by <code>|</code> (pipe):")),
              tags$pre(style = "background:#F8FAFD;border:1px solid #EEF2F7;padding:10px 12px;border-radius:6px;font-size:11.5px;color:#4A4A4A;line-height:1.6;",
                       "Site name | City | Country | Status | Monthly target | Overall target",
                       "\nLeeds General Infirmary | Leeds | United Kingdom | Recruiting | 3 | 60",
                       "\nManchester Royal Infirmary"),
              textAreaInput("bulk_paste", label = NULL, placeholder = "Paste site list here…",
                            rows = 10, width = "100%"))),
        tabPanel("CSV upload",
          div(style = "padding:14px 0;",
              div(style = "font-size:12.5px;color:#4A4A4A;line-height:1.7;margin-bottom:10px;",
                  HTML("CSV with at least a <code>site_name</code> column. Optional: <code>city</code>, <code>region</code>, <code>country</code>, <code>status</code>, <code>monthly_target</code>, <code>target</code>.")),
              fileInput("bulk_csv", label = NULL, accept = ".csv", buttonLabel = "Choose CSV"))),
        tabPanel("Defaults",
          div(style = "padding:14px 0;",
              div(style = "font-size:12.5px;color:#4A4A4A;margin-bottom:14px;",
                  "Applied to every site that doesn't specify these explicitly."),
              div(style = "display:grid;grid-template-columns:1fr 1fr 1fr;gap:14px;",
                  selectInput("bulk_def_status", "Default status",
                              choices = c("Identified", "Set-up", "Open", "Recruiting", "Closed"),
                              selected = "Identified"),
                  numericInput("bulk_def_monthly", "Default monthly target", value = 0, min = 0),
                  numericInput("bulk_def_target", "Default overall target", value = 0, min = 0))))
      ),
      div(style = "margin-top:16px;", uiOutput("bulk_preview_ui"))
    ))
  })

  .parse_bulk_paste <- function(txt) {
    if (is.null(txt) || !nzchar(trimws(txt))) return(data.frame())
    lines <- trimws(strsplit(txt, "\n", fixed = TRUE)[[1]]); lines <- lines[nzchar(lines)]
    if (!length(lines)) return(data.frame())
    do.call(rbind, lapply(lines, function(ln) {
      parts <- trimws(strsplit(ln, "|", fixed = TRUE)[[1]])
      data.frame(
        site_name      = parts[1],
        city           = if (length(parts) >= 2) parts[2] else NA_character_,
        country        = if (length(parts) >= 3) parts[3] else NA_character_,
        status         = if (length(parts) >= 4) parts[4] else NA_character_,
        monthly_target = if (length(parts) >= 5) suppressWarnings(as.integer(parts[5])) else NA_integer_,
        target         = if (length(parts) >= 6) suppressWarnings(as.integer(parts[6])) else NA_integer_,
        stringsAsFactors = FALSE)
    }))
  }
  .parse_bulk_csv <- function(path) {
    if (is.null(path) || !file.exists(path)) return(data.frame())
    df <- tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
                   error = function(e) data.frame())
    if (!nrow(df)) return(df)
    names(df) <- tolower(names(df))
    if (!"site_name" %in% names(df)) {
      showNotification("CSV is missing a site_name column.", type = "warning", duration = 6)
      return(data.frame())
    }
    cols <- c("site_name", "city", "region", "country", "status", "monthly_target", "target")
    for (c in setdiff(cols, names(df))) df[[c]] <- NA
    df[, cols, drop = FALSE]
  }
  bulk_parsed <- reactive({
    mode <- input$bulk_add_mode
    if (identical(mode, "Paste")) .parse_bulk_paste(input$bulk_paste)
    else if (identical(mode, "CSV upload")) {
      f <- input$bulk_csv; if (is.null(f)) data.frame() else .parse_bulk_csv(f$datapath)
    } else data.frame()
  })

  output$bulk_preview_ui <- renderUI({
    parsed <- bulk_parsed()
    existing <- rv$sites$site_name %||% character(0)
    if (!nrow(parsed))
      return(div(style = "font-size:12px;color:#8A8A8C;font-style:italic;padding:8px 0;",
                 "Paste a site list or upload a CSV — preview will appear here."))
    valid  <- !is.na(parsed$site_name) & nzchar(trimws(parsed$site_name))
    parsed <- parsed[valid, , drop = FALSE]
    dups   <- parsed$site_name %in% existing
    chip <- function(n, label, bg, fg)
      span(style = sprintf("display:inline-flex;gap:5px;background:%s;color:%s;padding:3px 9px;border-radius:999px;font-size:11px;font-weight:600;margin-right:6px;", bg, fg),
           sprintf("%d %s", n, label))
    div(style = "background:#FFFFFF;border:1px solid #EEF2F7;border-radius:10px;padding:10px 14px;",
        div(style = "margin-bottom:8px;",
            chip(sum(!dups), "new", "#ECFDF5", "#15803D"),
            if (sum(dups)) chip(sum(dups), "duplicate", "#FEF3C7", "#92400E")),
        div(style = "max-height:160px;overflow-y:auto;font-size:11.5px;",
            lapply(seq_len(nrow(parsed)), function(i) {
              r <- parsed[i, ]
              div(style = sprintf("padding:5px 0;border-top:1px solid #EEF2F7;color:%s;",
                                  if (dups[i]) "#8A8A8C" else "#1B1B1B"),
                  span(style = "font-weight:500;", r$site_name),
                  if (!is.na(r$city) && nzchar(r$city)) span(style = "color:#58595B;", sprintf(" · %s", r$city)),
                  if (dups[i]) span(style = "color:#92400E;float:right;", "already exists"))
            })))
  })

  observeEvent(input$bulk_add_go, {
    if (!require_role(rv, "manager")) return()
    parsed <- bulk_parsed()
    if (!nrow(parsed)) { showNotification("Nothing to add.", type = "warning"); return() }
    valid  <- !is.na(parsed$site_name) & nzchar(trimws(parsed$site_name))
    parsed <- parsed[valid, , drop = FALSE]
    existing <- rv$sites$site_name %||% character(0)
    new_rows <- parsed[!parsed$site_name %in% existing, , drop = FALSE]
    if (!nrow(new_rows)) { showNotification("All listed sites already exist.", type = "warning", duration = 5); return() }

    def_status  <- input$bulk_def_status %||% "Identified"
    def_monthly <- as.integer(input$bulk_def_monthly %||% 0)
    def_target  <- as.integer(input$bulk_def_target %||% 0)
    n_new <- nrow(new_rows)

    add_blocks <- withProgress(
      message = "Adding sites", detail = sprintf("Geocoding %d location%s…", n_new, if (n_new == 1) "" else "s"),
      value = 0,
      lapply(seq_len(n_new), function(i) {
        r <- new_rows[i, ]
        country_val <- if (is.na(r$country) || !nzchar(r$country)) "United Kingdom" else r$country
        lookup <- if (!is.na(r$city) && nzchar(r$city)) r$city else r$site_name
        incProgress(1 / n_new, detail = sprintf("(%d/%d) %s", i, n_new, trimws(r$site_name)))
        ll <- tryCatch(geocode_location(lookup, country_val), error = function(e) list(lat = NA_real_, lon = NA_real_))
        tibble(
          site_id = next_site_id(rv$sites), site_name = trimws(r$site_name),
          city = if (!is.na(r$city) && nzchar(r$city)) str_to_title(r$city) else NA_character_,
          region = NA_character_, country = country_val,
          status = if (!is.na(r$status) && nzchar(r$status)) r$status else def_status,
          site_open_date = as.Date(NA), siv_booked = FALSE, siv_date = as.Date(NA),
          monthly_target = if (!is.na(r$monthly_target)) r$monthly_target else def_monthly,
          target = if (!is.na(r$target)) r$target else def_target,
          randomised = 0L, lat = ll$lat, lon = ll$lon, source = "manual")
      }))
    for (i in seq_along(add_blocks)) rv$sites <- bind_rows(rv$sites, add_blocks[[i]])

    removeModal()
    cfg <- rv$trial_config
    log_activity("sites_bulk_added",
                 sprintf("Bulk-added <strong>%d</strong> sites", nrow(new_rows)),
                 username = rv$username, trial_code = if (!is.null(cfg)) cfg$code else NULL)
    showNotification(sprintf("Added %d %s.", nrow(new_rows), if (nrow(new_rows) == 1) "site" else "sites"),
                     type = "message", duration = 5)
  })
}
