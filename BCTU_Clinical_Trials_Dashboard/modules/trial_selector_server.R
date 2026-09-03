trial_selector_server <- function(input, output, session, state) {
  rv <- state$rv

  # Add home-mode class on init so sidebar/topbar are hidden on the home screen.
  shinyjs::runjs("document.body.classList.add('home-mode')")

  # Ensure home outputs render even when their wrapper div is display:none —
  # we toggle home tabs via JS, which Shiny's auto-suspend can't detect.
  .force_render <- function(ids) {
    for (id in ids) outputOptions(output, id, suspendWhenHidden = FALSE)
  }

  # ══════════════════════════════════════════════════════════════════════════

  # TRIAL CARD GRID
  # ══════════════════════════════════════════════════════════════════════════

  # \u2500\u2500 Per-trial recruitment count (cheap: reads latest CSV, counts unique IDs) \u2500\u2500
  .recruited_count <- function(cfg) {
    data_dir <- cfg$data_dir %||% file.path(getwd(), "trials", cfg$code, "data")
    csv <- tryCatch(find_latest_csv(data_dir), error = function(e) NULL)
    if (is.null(csv)) return(0L)
    raw <- tryCatch(read_redcap_file(csv), error = function(e) NULL)
    if (is.null(raw) || !"record_id" %in% names(raw)) return(0L)
    id_col <- cfg$redcap_fields$record_id %||% "record_id"
    if (!id_col %in% names(raw)) id_col <- "record_id"
    length(unique(baseline_rows(raw, cfg)[[id_col]]))
  }

  .status_pill <- function(pct) {
    if (pct >= 0.5)      tags$span(class = "home-status on-track",
                                   tags$span(class = "dot"), "On track")
    else if (pct >= 0.25) tags$span(class = "home-status warning",
                                    tags$span(class = "dot"), "Behind")
    else                  tags$span(class = "home-status warning",
                                    tags$span(class = "dot"), "Below 25%")
  }

  # New: redesigned status pill (.stat-pill) for rich-card layout
  .stat_pill_v2 <- function(pct, size = "md") {
    if (pct >= 0.5) {
      cls <- "on-track"; lbl <- "On track"
    } else if (pct >= 0.25) {
      cls <- "warning"; lbl <- "Behind pace"
    } else {
      cls <- "warning"; lbl <- "Below 25%"
    }
    tags$span(class = paste("stat-pill", cls, if (size == "sm") "sm"),
              tags$span(class = "stat-pill-dot"),
              lbl)
  }

  # Pick a stable colour pair for a trial mark (logo tile)
  .trial_mark_colors <- function(cfg) {
    pal <- list(
      list("#1B4F6B", "#2EC4A5"),
      list("#0E7490", "#67E8F9"),
      list("#7C2D12", "#FED7AA"),
      list("#BE185D", "#FBCFE8"),
      list("#0F766E", "#5EEAD4"),
      list("#4338CA", "#A78BFA"),
      list("#B45309", "#FBBF24")
    )
    code <- cfg$short_name %||% cfg$code %||% "X"
    idx  <- (sum(utf8ToInt(toupper(code))) %% length(pal)) + 1
    pal[[idx]]
  }

  # Resolve the URL path to a trial's logo in www/trial_logos/, if one was
  # copied there at startup (see app.R). Returns NULL when there's no logo
  # so callers can fall back to the gradient initials tile.
  .trial_logo_url <- function(cfg) {
    code <- cfg$code %||% ""
    if (!nzchar(code)) return(NULL)
    dir <- file.path(getwd(), "www", "trial_logos")
    if (!dir.exists(dir)) return(NULL)
    for (ext in c("png", "svg", "jpg", "jpeg", "webp", "gif")) {
      f <- file.path(dir, paste0(code, ".", ext))
      if (file.exists(f)) return(paste0("trial_logos/", code, ".", ext))
    }
    NULL
  }

  .trial_mark <- function(cfg, size = 44) {
    cols <- .trial_mark_colors(cfg)
    code <- toupper(cfg$short_name %||% cfg$code %||% "?")
    logo_url <- .trial_logo_url(cfg)
    if (!is.null(logo_url)) {
      # Real logo — render the image inside a square frame so the visual
      # weight matches the gradient initials tile used elsewhere.
      return(tags$div(
        class = "trial-mark2 trial-mark2-img",
        style = sprintf(
          "width:%spx;height:%spx;background:#fff;border:1px solid #E2E8EE;
           display:flex;align-items:center;justify-content:center;
           border-radius:8px;overflow:hidden;",
          size, size),
        tags$img(src = logo_url,
                 style = "max-width:88%;max-height:88%;object-fit:contain;",
                 alt = code)
      ))
    }
    tags$div(class = "trial-mark2",
             style = sprintf(
               "width:%spx;height:%spx;font-size:%spx;background:linear-gradient(135deg,%s 0%%,%s 140%%);",
               size, size, round(size * 0.32), cols[[1]], cols[[2]]),
             substr(code, 1, 4))
  }

  # All trials in the system (admin view).
  all_trials_data <- reactive({
    input$wiz_create
    input$confirm_delete_trial
    rv$home_membership_changed
    rv$settings_changed
    trials <- discover_trials()
    rv$available_trials <- trials
    lapply(names(trials), function(code) {
      cfg <- trials[[code]]
      n <- .recruited_count(cfg)
      target <- cfg$trial_target %||% 0L
      list(
        code     = code,
        cfg      = cfg,
        n        = n,
        target   = target,
        pct      = if (target > 0) min(1, n / target) else 0,
        category = trial_category(cfg)
      )
    })
  })

  # Trials the current user can see (filtered by membership).
  trials_data <- reactive({
    rows <- all_trials_data()
    visible <- user_visible_trials(rv$username)
    Filter(function(r) r$code %in% visible, rows)
  })

  # Profile name in topbar
  output$home_profile_name <- renderText({
    rv$username %||% "Profile"
  })

  # "Add Trial" button (admin only)
  output$home_add_button_ui <- renderUI({
    if (isTRUE(rv$portfolio_role == "admin")) {
      tags$button(class = "sec-head2-act",
                  onclick = "Shiny.setInputValue('open_wizard', Math.random(), {priority:'event'})",
                  style = "padding:6px 10px;border:1px solid #E2E8EE;border-radius:8px;background:#fff;",
                  HTML("&#43; Add trial"))
    }
  })

  # Hide tabs the user shouldn't see (admin-only ones).
  observe({
    is_admin <- isTRUE(rv$portfolio_role == "admin")
    shinyjs::runjs(sprintf(
      "document.querySelectorAll('.home-root .htab').forEach(function(t){
         var label = t.textContent.trim();
         // strip count badges (e.g. 'My Trials3')
         label = label.replace(/[0-9]+$/, '').trim();
         if (label === 'All Trials' || label === 'Activity' || label === 'Sites') {
           t.style.display = %s ? '' : 'none';
         }
       });", if (is_admin) "true" else "false"))
  })

  # \u2500\u2500 Rich trial card (new redesign \u2014 .rcard) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  .render_trial_card <- function(r, is_tm) {
    cfg     <- r$cfg
    ci      <- cfg$report_defaults$ci      %||% "\u2014"
    sponsor <- cfg$report_defaults$sponsor %||% "\u2014"
    phase   <- cfg$phase                   %||% "\u2014"
    cat     <- r$category                  %||% "\u2014"
    pct_w   <- sprintf("%d%%", round(r$pct * 100))
    bar_col <- .trial_mark_colors(cfg)[[1]]

    full_name <- cfg$full_name %||% cfg$name %||% (cfg$short_name %||% toupper(r$code))

    last_rand <- cfg$last_rand %||% "\u2014"
    sites_open <- cfg$sites_open %||% NA_integer_
    sites_total <- cfg$sites_total %||% NA_integer_
    queries <- cfg$open_queries %||% 0

    sites_txt <- if (!is.na(sites_open) && !is.na(sites_total))
      tagList(sites_open, tags$span(class = "rcard-foot-sub", sprintf(" / %d", sites_total)))
    else "\u2014"

    tags$button(class = "rcard",
        onclick = sprintf("Shiny.setInputValue('select_trial', '%s', {priority:'event'})", r$code),

        div(class = "rcard-head",
            .trial_mark(cfg),
            div(class = "rcard-titleblock",
                div(class = "rcard-code", cfg$short_name %||% toupper(r$code)),
                div(class = "rcard-name", full_name)),
            .stat_pill_v2(r$pct)
        ),

        div(class = "rcard-meta",
            div(class = "rcard-meta-row",
                tags$span(class = "rcard-meta-k", "CI"),
                tags$span(class = "rcard-meta-v", ci)),
            div(class = "rcard-meta-row",
                tags$span(class = "rcard-meta-k", "Sponsor"),
                tags$span(class = "rcard-meta-v", sponsor)),
            div(class = "rcard-meta-row",
                tags$span(class = "rcard-meta-k", "Phase"),
                tags$span(class = "rcard-meta-v", paste(phase, "\u00b7", cat)))
        ),

        div(class = "rcard-progressblock",
            div(class = "rcard-progress-top",
                tags$span(class = "rcard-recruited", format(r$n, big.mark = ",")),
                tags$span(class = "rcard-target",
                          sprintf("/ %s recruited", format(r$target, big.mark = ","))),
                tags$span(class = "rcard-pct", pct_w)),
            div(class = "rcard-bar",
                div(class = "rcard-bar-fill",
                    style = sprintf("width:%s;background:%s;", pct_w, bar_col)))
        ),

        div(class = "rcard-foot",
            div(class = "rcard-foot-cell",
                tags$span(class = "rcard-foot-k", "Sites"),
                tags$span(class = "rcard-foot-v", sites_txt)),
            div(class = "rcard-foot-cell",
                tags$span(class = "rcard-foot-k", "Last rand."),
                tags$span(class = "rcard-foot-v", last_rand)),
            div(class = "rcard-foot-cell",
                tags$span(class = "rcard-foot-k", "Queries"),
                tags$span(class = paste("rcard-foot-v", if (queries > 10) "warn" else ""),
                          as.character(queries)))
        )
    )
  }

  # \u2500\u2500 Category divider (new design \u2014 .cat-head) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  .category_header <- function(cat, n) {
    icon <- TRIAL_CATEGORY_ICONS[[cat]] %||% TRIAL_CATEGORY_ICONS[["Other"]]
    div(class = "cat-head",
        div(class = "cat-icon", HTML(icon)),
        div(class = "cat-label", cat),
        div(class = "cat-count",
            paste(n, if (n == 1) "trial" else "trials")),
        div(class = "cat-divider"))
  }

  # Order categories in TRIAL_CATEGORIES order, with Uncategorised last.
  .ordered_categories <- function(cats) {
    pref <- c(TRIAL_CATEGORIES, TRIAL_CATEGORY_FALLBACK)
    known <- intersect(pref, cats)
    extra <- setdiff(cats, pref)
    c(known, sort(extra))
  }

  output$trial_cards_ui <- renderUI({
    rows  <- tryCatch(trials_data(), error = function(e) {
      message("trial_cards rows err: ", e$message)
      return(list())
    })
    is_tm <- isTRUE(rv$portfolio_role == "admin")

    add_card <- if (is_tm) {
      tags$button(class = "add-card",
          onclick = "Shiny.setInputValue('open_wizard', Math.random(), {priority:'event'})",
          div(class = "add-card-plus", HTML("&#43;")),
          div(class = "add-card-l1", "Add a new trial"),
          div(class = "add-card-l2", "Wizard takes ~3 min"))
    }

    if (length(rows) == 0 && is.null(add_card)) {
      return(div(class = "empty-tab",
                 div(class = "empty-tab-i", HTML("&#x2299;")),
                 div(class = "empty-tab-t", "No trials yet"),
                 div(class = "empty-tab-s", "No trials available — ask an admin to add you to a trial.")))
    }

    if (length(rows) == 0) {
      return(div(class = "rgrid", add_card))
    }

    cats_seen <- unique(vapply(rows, function(r) r$category, character(1)))
    blocks <- lapply(.ordered_categories(cats_seen), function(cat) {
      group <- Filter(function(r) identical(r$category, cat), rows)
      tagList(
        .category_header(cat, length(group)),
        div(class = "rgrid",
            lapply(group, function(r) .render_trial_card(r, is_tm)))
      )
    })

    if (!is.null(add_card)) {
      blocks <- c(blocks, list(
        div(class = "rgrid", style = "margin-top:14px;", add_card)
      ))
    }

    do.call(tagList, blocks)
  })

  # ── Topbar pieces: profile initials, my-trials count, activity dot ────
  output$home_user_initials <- renderText({
    nm <- rv$username %||% "U"
    parts <- strsplit(nm, "\\s+")[[1]]
    if (length(parts) >= 2) toupper(paste0(substr(parts[1], 1, 1), substr(parts[2], 1, 1)))
    else                    toupper(substr(nm, 1, 2))
  })

  output$home_my_trials_count <- renderText({
    rows <- tryCatch(trials_data(), error = function(e) list())
    as.character(length(rows))
  })

  output$home_activity_dot <- renderUI({
    feed <- tryCatch(activity_feed(), error = function(e) NULL)
    if (!is.null(feed) && nrow(feed) > 0) tags$span(class = "htab-dot")
  })

  output$home_trials_section_title <- renderText({
    rows <- tryCatch(trials_data(), error = function(e) list())
    sprintf("%d active %s", length(rows),
            if (length(rows) == 1) "trial" else "trials")
  })

  # ── Portfolio summary strip (My Trials hero header) ───────────────────
  output$home_summary_strip_ui <- renderUI({
    rows <- tryCatch(trials_data(), error = function(e) list())
    if (length(rows) == 0) return(NULL)

    n_trials <- length(rows)
    pcts <- vapply(rows, function(r) r$pct, numeric(1))
    on_track <- sum(pcts >= 0.5)
    at_risk  <- sum(pcts < 0.25)

    sites_open <- sum(vapply(rows, function(r) r$cfg$sites_open %||% 0L, integer(1)))
    sites_total <- sum(vapply(rows, function(r) r$cfg$sites_total %||% 0L, integer(1)))
    this_week <- sum(vapply(rows, function(r) r$cfg$this_week %||% 0L, integer(1)))
    queries  <- sum(vapply(rows, function(r) r$cfg$open_queries %||% 0L, integer(1)))

    greet_hr <- as.integer(format(Sys.time(), "%H"))
    greeting <- if (greet_hr < 12) "Good morning" else if (greet_hr < 18) "Good afternoon" else "Good evening"
    fname <- strsplit(rv$username %||% "there", "\\s+")[[1]][1]

    div(class = "psum",
        div(class = "psum-head",
            div(class = "psum-eye", "Portfolio · last 7 days"),
            div(class = "psum-greet",
                sprintf("%s, %s — ", greeting, fname),
                tags$span(class = "psum-greet-sub",
                          sprintf("%d %s across %d %s",
                                  this_week,
                                  if (this_week == 1) "new participant this week" else "new participants this week",
                                  n_trials,
                                  if (n_trials == 1) "trial" else "trials")))
        ),
        div(class = "psum-grid",
            div(class = "psum-stat accent",
                div(class = "psum-stat-k", "Active trials"),
                div(class = "psum-stat-v", n_trials),
                div(class = "psum-stat-s",
                    sprintf("%d on track at 50%%+ of target", on_track))),
            div(class = "psum-stat pos",
                div(class = "psum-stat-k", "This week"),
                div(class = "psum-stat-v", sprintf("+%d", this_week)),
                div(class = "psum-stat-s", "recruited across portfolio")),
            div(class = paste("psum-stat", if (queries > 50) "warn"),
                div(class = "psum-stat-k", "Open queries"),
                div(class = "psum-stat-v", queries),
                div(class = "psum-stat-s", "across all trials"))
        ),
        div(class = "psum-trials",
            div(class = "psum-trials-head",
                tags$span(class = "psum-trials-eye", "Per-trial recruitment & sites"),
                tags$span(class = "psum-trials-leg",
                          tags$span(class = "psum-trials-leg-sw"),
                          " recruited vs target")),
            div(class = "psum-trials-list",
                lapply(rows, function(r) {
                  cfg <- r$cfg
                  pct <- r$pct
                  s_open <- cfg$sites_open %||% 0L
                  s_total <- cfg$sites_total %||% 0L
                  s_pct <- if (s_total > 0) min(1, s_open / s_total) else 0
                  bar_col <- .trial_mark_colors(cfg)[[1]]
                  dot_col <- if (pct >= 0.5) "#10B981" else if (pct >= 0.25) "#F59E0B" else "#EF4444"
                  div(class = "psum-trow",
                      div(class = "psum-trow-id",
                          tags$span(class = "psum-trow-dot",
                                    style = sprintf("background:%s;", dot_col)),
                          tags$span(class = "psum-trow-code",
                                    cfg$short_name %||% toupper(r$code))),
                      div(class = "psum-trow-met",
                          div(class = "psum-trow-met-k", "Recruited"),
                          div(class = "psum-trow-met-v",
                              format(r$n, big.mark = ","),
                              tags$span(class = "psum-trow-met-of",
                                        sprintf(" / %s", format(r$target, big.mark = ",")))),
                          div(class = "psum-trow-bar",
                              div(class = "psum-trow-bar-f",
                                  style = sprintf("width:%d%%;background:%s;",
                                                  round(pct * 100), bar_col))),
                          div(class = "psum-trow-met-s",
                              sprintf("%d%% of target", round(pct * 100)))),
                      div(class = "psum-trow-met",
                          div(class = "psum-trow-met-k", "Sites"),
                          div(class = "psum-trow-met-v",
                              s_open,
                              tags$span(class = "psum-trow-met-of",
                                        sprintf(" / %d", s_total))),
                          div(class = "psum-trow-bar",
                              div(class = "psum-trow-bar-f",
                                  style = sprintf("width:%d%%;background:#27384A;opacity:.55;",
                                                  round(s_pct * 100)))),
                          div(class = "psum-trow-met-s",
                              sprintf("%d pending open", max(0L, s_total - s_open))))
                  )
                })))
    )
  })

  # ── Activity preview (small list under My Trials) ─────────────────────
  output$home_activity_preview_ui <- renderUI({
    feed <- tryCatch(activity_feed(), error = function(e) NULL)
    if (is.null(feed) || nrow(feed) == 0) {
      return(div(class = "act-list",
                 div(style = "padding:20px;text-align:center;color:#64748B;font-size:12.5px;",
                     "No recent activity yet.")))
    }
    trials <- discover_trials()
    rows <- head(feed, 6)
    div(class = "act-list",
        lapply(seq_len(nrow(rows)), function(i) {
          r <- rows[i, ]
          ev <- as.character(r$event_type %||% "info")
          map <- list(
            trial_created    = list(c = "#10B981", l = "TRIAL"),
            trial_deleted    = list(c = "#EF4444", l = "TRIAL"),
            site_added       = list(c = "#3B82F6", l = "SITE"),
            sites_bulk_added = list(c = "#3B82F6", l = "SITE"),
            site_deleted     = list(c = "#EF4444", l = "SITE"),
            csv_uploaded     = list(c = "#0EA5E9", l = "DATA"),
            amendment_added  = list(c = "#A855F7", l = "AMEND"),
            amendment_edited = list(c = "#A855F7", l = "AMEND"),
            settings_saved   = list(c = "#64748B", l = "SET"),
            membership_changed = list(c = "#F59E0B", l = "USER"),
            portfolio_role_changed = list(c = "#F59E0B", l = "ROLE")
          )
          k <- map[[ev]] %||% list(c = "#10B981", l = "INFO")
          tcode <- as.character(r$trial_code %||% "—")
          tshort <- if (!is.null(trials[[tcode]]))
            (trials[[tcode]]$short_name %||% toupper(tcode)) else tcode
          when <- as.character(r$happened_at %||% r$timestamp %||% "")
          when_s <- if (nchar(when)) format(as.POSIXct(when), "%d %b %H:%M") else ""
          div(class = "act-row",
              div(class = "act-tag",
                  style = sprintf("background:%s;", k$c), k$l),
              div(class = "act-trial", tshort),
              div(class = "act-text", as.character(r$summary %||% r$description %||% "")),
              div(class = "act-time", when_s))
        }))
  })

  # ── Quick actions: New trial → wizard ─────────────────────────────────
  observeEvent(input$qa_new_trial, {
    if (isTRUE(rv$portfolio_role == "admin")) {
      session$sendCustomMessage("trigger_open_wizard", list())
      shinyjs::runjs("Shiny.setInputValue('open_wizard', Math.random(), {priority:'event'});")
    } else {
      showModal(modalDialog(
        title = "Admins only",
        easyClose = TRUE, footer = modalButton("OK"),
        "Only portfolio admins can create new trials."
      ))
    }
  })

  # ── Quick actions: Run a report → trial picker → reports module ───────
  observeEvent(input$qa_run_report, {
    rows <- tryCatch(trials_data(), error = function(e) list())
    if (length(rows) == 0) {
      showModal(modalDialog(
        title = "No trials available",
        easyClose = TRUE, footer = modalButton("Close"),
        "You need at least one trial to run a report."))
      return()
    }
    choices <- setNames(
      vapply(rows, function(r) r$code, character(1)),
      vapply(rows, function(r) r$cfg$short_name %||% toupper(r$code), character(1))
    )
    showModal(modalDialog(
      title = "Run a report",
      size = "s", easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("qa_run_report_go", "Open report builder",
                     class = "btn btn-primary",
                     style = "background:#1B4F6B;border-color:#1B4F6B;")),
      div(style = "padding:6px 0;",
          tags$label(style = "font-size:11px;font-weight:600;color:#64748B;
                              text-transform:uppercase;letter-spacing:.5px;",
                     "Trial"),
          selectInput("qa_run_report_trial", label = NULL,
                      choices = choices, width = "100%"),
          div(style = "font-size:12px;color:#64748B;margin-top:8px;",
              "Opens the report builder where you'll choose format
               (Word / PDF / HTML) and sections."))
    ))
  })

  observeEvent(input$qa_run_report_go, {
    code <- input$qa_run_report_trial
    removeModal()
    if (!is.null(code) && nzchar(code)) {
      shinyjs::runjs(sprintf(
        "Shiny.setInputValue('select_trial', '%s', {priority:'event'});", code))
      # Switch to reports tab once trial is loaded
      shinyjs::runjs("setTimeout(function(){
        var btn = document.getElementById('nav_reports');
        if (btn) btn.click();
      }, 600);")
    }
  })

  # ── Quick actions: Switch theme ───────────────────────────────────────
  observeEvent(input$qa_switch_theme, {
    showModal(modalDialog(
      title = "Switch theme",
      size = "s", easyClose = TRUE,
      footer = modalButton("Close"),
      div(style = "display:grid;gap:10px;padding:6px 0;",
          tags$button(class = "qa-tile",
                      onclick = "Shiny.setInputValue('qa_theme_pick','light',{priority:'event'})",
                      div(class = "qa-icon", HTML("&#x2600;")),
                      div(class = "qa-text",
                          div(class = "qa-label", "Light"),
                          div(class = "qa-desc", "Default BCTU navy & teal"))),
          tags$button(class = "qa-tile",
                      onclick = "Shiny.setInputValue('qa_theme_pick','dark',{priority:'event'})",
                      div(class = "qa-icon", HTML("&#x263D;")),
                      div(class = "qa-text",
                          div(class = "qa-label", "Dark"),
                          div(class = "qa-desc", "Reduced glare for evening work"))),
          tags$button(class = "qa-tile",
                      onclick = "Shiny.setInputValue('qa_theme_pick','system',{priority:'event'})",
                      div(class = "qa-icon", HTML("&#x1F5A5;")),
                      div(class = "qa-text",
                          div(class = "qa-label", "System"),
                          div(class = "qa-desc", "Follow OS preference"))))
    ))
  })

  observeEvent(input$qa_theme_pick, {
    pick <- input$qa_theme_pick
    removeModal()
    shinyjs::runjs(sprintf("
      document.documentElement.setAttribute('data-theme','%s');
      try { localStorage.setItem('bctu_theme','%s'); } catch(e){}
    ", pick, pick))
    showNotification(sprintf("Theme set to %s.", pick), duration = 2)
  })

  # ── Quick actions: Portfolio settings → manage users ──────────────────
  observeEvent(input$qa_portfolio_settings, {
    if (isTRUE(rv$portfolio_role == "admin")) {
      open_user_management()
    } else {
      showModal(modalDialog(
        title = "Admins only",
        easyClose = TRUE, footer = modalButton("OK"),
        "Only portfolio admins can change portfolio settings."))
    }
  })

  # ── Help button ───────────────────────────────────────────────────────
  observeEvent(input$home_help_open, {
    showModal(modalDialog(
      title = "Help",
      size = "m", easyClose = TRUE, footer = modalButton("Close"),
      div(style = "font-size:13px;line-height:1.6;color:#27384A;",
          tags$p(tags$strong("BCTU Clinical Trials Dashboard")),
          tags$p("Click any trial card to open its dashboard. Use the tabs at the top
                  to switch between My Trials, Portfolio, All Trials, Sites and Activity."),
          tags$p("Quick actions row:"),
          tags$ul(
            tags$li(tags$strong("New trial"), " — wizard to spin up a new dashboard (admin)."),
            tags$li(tags$strong("Run a report"), " — pick a trial and open the report builder."),
            tags$li(tags$strong("Switch theme"), " — light, dark, or system."),
            tags$li(tags$strong("Portfolio settings"), " — manage users, roles and access (admin).")),
          tags$p(style = "color:#64748B;font-size:12px;",
                 "Need more help? Contact the BCTU support team."))
    ))
  })

  # \u2500\u2500 Overview tab \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  output$home_overview_ui <- renderUI({
    rows <- tryCatch(
      if (isTRUE(rv$portfolio_role == "admin")) all_trials_data() else trials_data(),
      error = function(e) {
        message("home_overview rows error: ", e$message)
        list()
      })
    if (length(rows) == 0) {
      return(div(class = "home-empty",
                 div(class = "icon", HTML("&#x1F4CA;")),
                 div("No trials yet \u2014 create one to see portfolio stats.")))
    }

    # \u2500\u2500 Pre-compute v2 status for each trial (avoids reading raw twice) \u2500\u2500
    # Each row picks up $status_v2 \u2208 {on, warn, risk, setup, closed}
    rows <- lapply(rows, function(r) {
      r$status_v2 <- tryCatch(trial_status_v2(r),
                              error = function(e) "setup")
      r
    })

    # Aggregate status counts \u2014 these are countable across trials
    n_on    <- sum(vapply(rows, function(r) r$status_v2 == "on",    logical(1)))
    n_warn  <- sum(vapply(rows, function(r) r$status_v2 == "warn",  logical(1)))
    n_risk  <- sum(vapply(rows, function(r) r$status_v2 == "risk",  logical(1)))
    n_setup <- sum(vapply(rows, function(r) r$status_v2 == "setup", logical(1)))
    n_closed<- sum(vapply(rows, function(r) r$status_v2 == "closed",logical(1)))
    n_trials <- length(rows)
    # Average progress (mean of per-trial %), not summed/blended ratios
    avg_pct <- if (n_trials > 0)
      round(mean(vapply(rows, function(r) r$pct %||% 0, numeric(1))) * 100)
    else 0L

    # \u2500\u2500 Smart Insights cards (inlined \u2014 was a nested uiOutput) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
    smart_section <- tryCatch({
      summary <- compute_portfolio_summary(discover_trials())
      if (is.null(summary) || !length(summary$per_trial)) NULL
      else {
        suggestion <- function(emoji, title_html, hint, key, accent = "#6366F1") {
          div(class = "home-insight-card",
              onclick = sprintf("Shiny.setInputValue('home_insight_query', '%s', {priority:'event'})", key),
              style = sprintf("background:#FFFFFF;border:1px solid #EEF2F7;border-radius:14px;
                               padding:14px 16px;cursor:pointer;transition:all .15s;
                               border-left:3px solid %s;", accent),
              div(style = "display:flex;align-items:flex-start;gap:10px;",
                  div(style = sprintf("font-size:18px;color:%s;line-height:1;
                                       margin-top:2px;", accent), HTML(emoji)),
                  div(style = "flex:1;min-width:0;",
                      div(style = "font-size:13px;font-weight:600;color:#0F172A;
                                   line-height:1.4;", HTML(title_html)),
                      div(style = "font-size:11px;color:#64748B;margin-top:4px;",
                          hint))))
        }
        cards <- list()
        cats <- unique(vapply(summary$per_trial, function(s) s$category, character(1)))
        if (length(cats) > 0) {
          cards[[length(cards)+1]] <- suggestion("&#x1F4CA;",
            sprintf("How are <strong>%s</strong> trials recruiting?", cats[1]),
            "See breakdown of trials in this category",
            paste0("category::", cats[1]), "#6366F1")
        }
        if (summary$n_below + summary$n_stalled > 0) {
          n_attn <- summary$n_below + summary$n_stalled
          cards[[length(cards)+1]] <- suggestion("&#x26A0;",
            sprintf("<strong>%d %s</strong> need attention", n_attn,
                    if (n_attn == 1) "trial" else "trials"),
            "View trials below pace or stalled",
            "needs_attention", "#F59E0B")
        } else {
          cards[[length(cards)+1]] <- suggestion("&#x2728;",
            "Portfolio is <strong>healthy</strong>",
            "No stalled or critically lagging trials",
            "healthy", "#10B981")
        }
        if (summary$n_lagging > 0) {
          cards[[length(cards)+1]] <- suggestion("&#x1F4CD;",
            sprintf("<strong>%d %s</strong> open but not recruiting",
                    summary$n_lagging,
                    if (summary$n_lagging == 1) "site" else "sites"),
            "Across all trials in your portfolio",
            "lagging_sites", "#F43F5E")
        }
        if (length(summary$per_trial) >= 2) {
          cards[[length(cards)+1]] <- suggestion("&#x1F50D;",
            "<strong>Compare</strong> two trials side by side",
            "Pick any two trials to compare",
            "compare", "#8B5CF6")
        }
        div(
          tags$style(HTML(".home-insight-card:hover{transform:translateY(-1px);
                          box-shadow:0 6px 18px rgba(99,102,241,0.10);}")),
          div(style = "font-size:11px;font-weight:600;color:#64748B;
                       text-transform:uppercase;letter-spacing:.6px;margin-bottom:10px;",
              "Smart Insights"),
          div(style = "display:grid;grid-template-columns:repeat(auto-fill, minmax(240px, 1fr));
                       gap:12px;", cards))
      }
    }, error = function(e) {
      message("smart_section error: ", e$message)
      div(style = "background:#FFFBEB;border:1px solid #FDE68A;border-radius:10px;
                   padding:12px 14px;font-size:12px;color:#78350F;",
          sprintf("Smart Insights couldn't load: %s", e$message))
    })
    cats_seen <- unique(vapply(rows, function(r) r$category, character(1)))

    # ── KPI cards (portfolio health, not summed totals) ────────────────────
    kpi <- function(lbl, value, sub, trend = NULL, accent = "var(--bctu-deep)") {
      div(class = "pf-kpi",
        div(class = "pf-kpi-lbl", lbl),
        div(class = "pf-kpi-v", style = sprintf("color:%s", accent), value),
        div(class = "pf-kpi-sub",
            tags$span(sub),
            if (!is.null(trend)) tags$span(class = "pf-kpi-trend", trend)))
    }
    kpi_row <- div(class = "pf-kpi-row",
      kpi("Active trials", n_trials,
          if (n_trials == 1) "live trial" else "live trials",
          NULL, "var(--bctu-deep)"),
      kpi("On track",
          tagList(n_on, tags$span(style = "font-size:16px;color:var(--muted);font-weight:500;",
                                  sprintf(" / %d", n_trials))),
          "at or above target pace", NULL, "var(--green-dk)"),
      kpi("Need attention",
          tagList(n_warn + n_risk,
                  tags$span(style = "font-size:16px;color:var(--muted);font-weight:500;",
                            sprintf(" / %d", n_trials))),
          sprintf("%d behind · %d stalled", n_warn, n_risk),
          NULL, "var(--amber-dk)"),
      kpi("Avg. recruitment progress", paste0(avg_pct, "%"),
          "mean across all trials", NULL, "var(--bctu-deep)"))

    # ── Recruitment-by-trial: horizontal bars, sorted by % ────────────────
    sorted_rows <- rows[order(-vapply(rows, function(r) r$pct %||% 0, numeric(1)))]
    rec_rows <- lapply(sorted_rows, function(r) {
      cfg  <- r$cfg
      pct  <- max(0, min(1, r$pct %||% 0))
      pct_w <- sprintf("%.0f%%", pct * 100)
      lbl  <- trial_status_label(r$status_v2)
      div(class = "pf-rec-row",
          onclick = sprintf("Shiny.setInputValue('select_trial','%s',{priority:'event'})", r$code),
          div(div(class = "pf-rec-name", cfg$short_name %||% toupper(r$code)),
              div(class = "pf-rec-cat", r$category)),
          div(class = "pf-rec-bar",
              div(class = sprintf("pf-rec-bar-f s-%s", lbl$cls),
                  style = sprintf("width:%s;", pct_w))),
          div(class = "pf-rec-count",
              tags$b(format(r$n, big.mark = ",")),
              if (r$target > 0)
                tags$span(style = "color:var(--muted);font-weight:500;",
                          sprintf(" / %s", format(r$target, big.mark = ","))),
              tags$br(),
              tags$span(style = "font-size:10.5px;color:var(--muted);",
                        sprintf("%s of target", pct_w))),
          div(class = sprintf("pf-rec-pill s-%s", lbl$cls), lbl$text)
      )
    })

    # ── Status donut (server-rendered SVG) ────────────────────────────────
    donut_svg <- local({
      slices <- list(
        list(n = n_on,    col = "#10B981", lbl = "On track"),
        list(n = n_warn,  col = "#F59E0B", lbl = "Behind"),
        list(n = n_risk,  col = "#EF4444", lbl = "Stalled"),
        list(n = n_setup, col = "#3B82F6", lbl = "Set-up"),
        list(n = n_closed,col = "#A693AF", lbl = "Closed"))
      slices <- Filter(function(s) s$n > 0, slices)
      circ <- 377  # 2 * pi * 60
      offset <- 0
      paths <- vapply(slices, function(s) {
        dash <- round(circ * s$n / max(1, n_trials), 1)
        out <- sprintf(
'<circle cx="80" cy="80" r="60" stroke="%s" stroke-width="22" fill="none"
         stroke-dasharray="%s %s" stroke-dashoffset="%s"/>',
          s$col, dash, circ, -offset)
        offset <<- offset + dash
        out
      }, character(1))
      HTML(sprintf(
'<svg viewBox="0 0 160 160" width="160" height="160" style="transform:rotate(-90deg)">
  <circle cx="80" cy="80" r="60" stroke="#F4ECF1" stroke-width="22" fill="none"/>
  %s
</svg>
<div class="pf-donut-ctr"><b>%d</b><small>TRIALS</small></div>',
        paste(paths, collapse = ""), n_trials))
    })

    donut_legend <- div(class = "pf-donut-legend",
      .pf_leg_row("On track", n_on,    n_trials, "#10B981"),
      .pf_leg_row("Behind",   n_warn,  n_trials, "#F59E0B"),
      .pf_leg_row("Stalled",  n_risk,  n_trials, "#EF4444"),
      .pf_leg_row("Set-up",   n_setup, n_trials, "#3B82F6"),
      .pf_leg_row("Closed",   n_closed,n_trials, "#A693AF"))

    # ── By-category panel — average progress + status mix per category ───
    cat_rows_html <- lapply(.ordered_categories(cats_seen), function(cat) {
      group <- Filter(function(r) identical(r$category, cat), rows)
      icon  <- TRIAL_CATEGORY_ICONS[[cat]] %||% TRIAL_CATEGORY_ICONS[["Other"]]
      cat_avg <- round(mean(vapply(group, function(r) r$pct %||% 0, numeric(1))) * 100)
      mix_parts <- c(
        if (sum(vapply(group, function(r) r$status_v2 == "on",    logical(1))) > 0)
          sprintf("%d on track", sum(vapply(group, function(r) r$status_v2 == "on",    logical(1)))),
        if (sum(vapply(group, function(r) r$status_v2 == "warn",  logical(1))) > 0)
          sprintf("%d behind",   sum(vapply(group, function(r) r$status_v2 == "warn",  logical(1)))),
        if (sum(vapply(group, function(r) r$status_v2 == "risk",  logical(1))) > 0)
          sprintf("%d stalled",  sum(vapply(group, function(r) r$status_v2 == "risk",  logical(1)))),
        if (sum(vapply(group, function(r) r$status_v2 == "setup", logical(1))) > 0)
          sprintf("%d set-up",   sum(vapply(group, function(r) r$status_v2 == "setup", logical(1)))),
        if (sum(vapply(group, function(r) r$status_v2 == "closed",logical(1))) > 0)
          sprintf("%d closed",   sum(vapply(group, function(r) r$status_v2 == "closed",logical(1)))))
      div(class = "pf-cat-row",
        div(class = "pf-cat-icon", HTML(icon)),
        div(div(class = "pf-cat-name", cat),
            div(class = "pf-cat-sub",
                paste(length(group),
                      if (length(group) == 1) "trial" else "trials",
                      "·", paste(mix_parts, collapse = " · ")))),
        div(class = "pf-cat-bar",
            div(class = "pf-cat-bar-f",
                style = sprintf("width:%d%%;", cat_avg))),
        div(class = "pf-cat-n", paste0(cat_avg, "%")))
    })

    tagList(
      smart_section,
      kpi_row,
      div(class = "pf-two-col",
        div(class = "pf-panel",
          div(class = "pf-panel-head",
            tags$span(class = "pf-panel-title", "Recruitment by trial"),
            tags$span(class = "pf-panel-meta", "sorted by % of target · click any trial to open")),
          div(class = "pf-rec-list", rec_rows)),
        div(class = "pf-panel",
          div(class = "pf-panel-head",
            tags$span(class = "pf-panel-title", "Trial status")),
          div(class = "pf-donut-flex",
            div(class = "pf-donut-wrap", donut_svg),
            donut_legend))),
      div(class = "pf-panel",
        div(class = "pf-panel-head",
          tags$span(class = "pf-panel-title", "By category"),
          tags$span(class = "pf-panel-meta",
                    "Mean progress and status mix per category")),
        div(class = "pf-cat-grid", cat_rows_html))
    )
  })

  # Tiny helper for the donut legend rows
  .pf_leg_row <- function(name, n, total, color) {
    pct <- if (total > 0) round(100 * n / total) else 0
    div(class = "pf-leg-row",
        div(class = "pf-leg-l",
            div(class = "pf-leg-dot", style = sprintf("background:%s;", color)),
            tags$span(class = "pf-leg-name", name)),
        div(tags$span(class = "pf-leg-n", n),
            tags$span(class = "pf-leg-pct", sprintf("%d%%", pct))))
  }

  # ── Portfolio insight computation (cached, refreshes on data changes) ────
  portfolio_summary <- reactive({
    rv$home_membership_changed
    rv$settings_changed
    trials <- discover_trials()
    compute_portfolio_summary(trials)
  })

  # ── Smart Insights row at the top of Overview ────────────────────────────
  output$home_smart_insights_ui <- renderUI({
    summary <- tryCatch(portfolio_summary(), error = function(e) {
      message("portfolio_summary error: ", e$message)
      NULL
    })
    if (is.null(summary) || length(summary$per_trial) == 0) return(NULL)

    # Build natural-language suggestion cards
    suggestion <- function(emoji, title_html, hint, key, accent = "#6366F1") {
      div(class = "home-insight-card",
          onclick = sprintf("Shiny.setInputValue('home_insight_query', '%s', {priority:'event'})", key),
          style = sprintf("background:#FFFFFF;border:1px solid #EEF2F7;border-radius:14px;
                           padding:14px 16px;cursor:pointer;transition:all .15s;
                           border-left:3px solid %s;", accent),
          div(style = "display:flex;align-items:flex-start;gap:10px;",
              div(style = sprintf("font-size:18px;color:%s;line-height:1;
                                   margin-top:2px;", accent),
                  HTML(emoji)),
              div(style = "flex:1;min-width:0;",
                  div(style = "font-size:13px;font-weight:600;color:#0F172A;
                               line-height:1.4;",
                      HTML(title_html)),
                  div(style = "font-size:11px;color:#64748B;margin-top:4px;",
                      hint))))
    }

    cards <- list()

    # Suggestion: how is each category doing?
    cats <- unique(vapply(summary$per_trial, function(s) s$category, character(1)))
    if (length(cats) > 0) {
      cat_first <- cats[1]
      cards <- c(cards, list(
        suggestion("&#x1F4CA;",
          sprintf("How are <strong>%s</strong> trials recruiting?", cat_first),
          "See breakdown of trials in this category",
          paste0("category::", cat_first),
          "#6366F1")
      ))
    }

    # Suggestion: trials needing attention
    if (summary$n_below > 0 || summary$n_stalled > 0) {
      n_attn <- summary$n_below + summary$n_stalled
      cards <- c(cards, list(
        suggestion("&#x26A0;",
          sprintf("<strong>%d %s</strong> need attention",
                  n_attn, if (n_attn == 1) "trial" else "trials"),
          "View trials below pace or stalled",
          "needs_attention",
          "#F59E0B")
      ))
    } else {
      cards <- c(cards, list(
        suggestion("&#x2728;",
          "Portfolio is <strong>healthy</strong>",
          "No stalled or critically lagging trials",
          "healthy",
          "#10B981")
      ))
    }

    # Suggestion: lagging sites
    if (summary$n_lagging > 0) {
      cards <- c(cards, list(
        suggestion("&#x1F4CD;",
          sprintf("<strong>%d %s</strong> open but not recruiting",
                  summary$n_lagging,
                  if (summary$n_lagging == 1) "site" else "sites"),
          "Across all trials in your portfolio",
          "lagging_sites",
          "#F43F5E")
      ))
    }

    # Suggestion: compare trials
    if (length(summary$per_trial) >= 2) {
      cards <- c(cards, list(
        suggestion("&#x1F50D;",
          "<strong>Compare</strong> two trials side by side",
          "Pick any two trials to compare",
          "compare",
          "#8B5CF6")
      ))
    }

    div(
      tags$style(HTML("
        .home-insight-card:hover {
          transform: translateY(-1px);
          box-shadow: 0 6px 18px rgba(99,102,241,0.10);
        }
      ")),
      div(style = "font-size:11px;font-weight:600;color:#64748B;
                   text-transform:uppercase;letter-spacing:.6px;
                   margin-bottom:10px;",
          "Smart Insights"),
      div(style = "display:grid;grid-template-columns:repeat(auto-fill, minmax(240px, 1fr));
                   gap:12px;",
          cards)
    )
  })

  # ── Per-trial rows with insight chips ────────────────────────────────────
  output$home_per_trial_rows_ui <- renderUI({
    summary <- tryCatch(portfolio_summary(), error = function(e) NULL)
    rdata <- all_trials_data()
    if (is.null(summary)) {
      # Fallback: render rows from rdata only, no insight chips
      rows_list <- lapply(rdata, function(r)
        list(code = r$code,
             short = r$cfg$short_name %||% toupper(r$code),
             category = r$category, top = NULL, insights = list()))
    } else {
      rows_list <- summary$per_trial
    }
    if (!length(rows_list) || !length(rdata)) {
      return(div(style = "padding:14px 0;color:#94A3B8;font-style:italic;
                          font-size:12.5px;",
                 "No trials yet."))
    }
    rdata_lookup <- setNames(rdata, vapply(rdata, function(r) r$code, character(1)))

    rows <- lapply(rows_list, function(s) {
      r <- rdata_lookup[[s$code]]
      if (is.null(r)) return(NULL)
      pct_w <- sprintf("%.0f%%", r$pct * 100)

      sev_chip <- if (!is.null(s$top)) {
        sev <- s$top$severity
        col <- switch(sev,
                      alert   = list(bg = "#FEF2F2", fg = "#B91C1C"),
                      warning = list(bg = "#FFFBEB", fg = "#B45309"),
                      info    = list(bg = "#F0FDF4", fg = "#15803D"))
        span(style = sprintf("display:inline-flex;align-items:center;gap:5px;
                              background:%s;color:%s;padding:3px 9px;border-radius:999px;
                              font-size:10.5px;font-weight:600;",
                             col$bg, col$fg),
             HTML(s$top$icon), s$top$title)
      } else NULL

      div(style = "padding:14px 0;border-bottom:1px solid #EEF2F7;cursor:pointer;",
          onclick = sprintf("Shiny.setInputValue('home_insight_query','trial::%s',{priority:'event'})",
                            s$code),
          div(style = "display:grid;grid-template-columns:1.4fr 1fr 0.5fr;gap:14px;
                       align-items:center;",
              div(div(style = "font-weight:600;color:#0F172A;font-size:14px;", s$short),
                  div(style = "font-size:11px;color:#64748B;", s$category)),
              div(class = "home-progress", style = "margin:0;width:100%;",
                  div(class = "home-progress-fill", style = sprintf("width:%s;", pct_w))),
              div(style = "font-size:13px;color:#475569;text-align:right;",
                  sprintf("%d / %d", r$n, r$target))),
          if (!is.null(sev_chip))
            div(style = "margin-top:8px;", sev_chip))
    })
    div(rows)
  })

  # ── Per-category rows (clickable) ────────────────────────────────────────
  output$home_per_category_rows_ui <- renderUI({
    rows <- all_trials_data()
    cats_seen <- unique(vapply(rows, function(r) r$category, character(1)))
    cat_rows <- lapply(.ordered_categories(cats_seen), function(cat) {
      group <- Filter(function(r) identical(r$category, cat), rows)
      icon  <- TRIAL_CATEGORY_ICONS[[cat]] %||% TRIAL_CATEGORY_ICONS[["Other"]]
      div(style = "display:grid;grid-template-columns:auto 1fr auto;
                   gap:14px;align-items:center;
                   padding:12px 0;border-bottom:1px solid #EEF2F7;cursor:pointer;",
          onclick = sprintf("Shiny.setInputValue('home_insight_query','category::%s',{priority:'event'})",
                            cat),
          div(class = "home-category-icon", HTML(icon)),
          div(style = "font-weight:600;color:#0F172A;font-size:13.5px;", cat),
          div(style = "font-size:13px;color:var(--accent);font-weight:600;",
              paste(length(group),
                    if (length(group) == 1) "trial" else "trials")))
    })
    div(cat_rows)
  })

  # ── Insight query handler (opens drill-down modal) ───────────────────────
  observeEvent(input$home_insight_query, {
    q <- input$home_insight_query
    if (is.null(q) || !nzchar(q)) return()

    # category::Surgery
    if (startsWith(q, "category::")) {
      cat <- sub("^category::", "", q)
      summary <- compute_category_summary(discover_trials(), cat)
      showModal(.insight_category_modal(cat, summary))
      return()
    }

    # trial::tonic
    if (startsWith(q, "trial::")) {
      code <- sub("^trial::", "", q)
      cfgs <- discover_trials()
      cfg <- cfgs[[code]]
      if (is.null(cfg)) return()
      s <- trial_insight_summary(cfg)
      showModal(.insight_trial_modal(s))
      return()
    }

    if (q == "needs_attention") {
      summary <- portfolio_summary()
      showModal(.insight_attention_modal(summary))
      return()
    }
    if (q == "healthy") {
      summary <- portfolio_summary()
      showModal(.insight_healthy_modal(summary))
      return()
    }
    if (q == "lagging_sites") {
      summary <- portfolio_summary()
      showModal(.insight_lagging_modal(summary))
      return()
    }
    if (q == "compare") {
      showModal(.insight_compare_modal(rv))
      return()
    }
  })

  observeEvent(input$home_insight_compare_go, {
    a <- input$home_insight_compare_a
    b <- input$home_insight_compare_b
    if (is.null(a) || is.null(b) || identical(a, b)) {
      showNotification("Pick two different trials.", type = "warning")
      return()
    }
    cfgs <- discover_trials()
    sa <- trial_insight_summary(cfgs[[a]])
    sb <- trial_insight_summary(cfgs[[b]])
    removeModal()
    showModal(.insight_compare_result_modal(sa, sb))
  })

  # \u2500\u2500 All Trials table (grouped by category) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Cached enriched-row data for All Trials \u2014 pulls site counts + recent
  # activity per trial. Heavier than trials_data() because it reads each
  # trial's CSV + SQLite, so we memoise on data fingerprints.
  all_trials_enriched <- reactive({
    rv$home_membership_changed
    rv$settings_changed
    rows <- tryCatch(all_trials_data(), error = function(e) list())
    lapply(rows, function(r) {
      sites_df <- tryCatch(.read_trial_sites(r$cfg), error = function(e) NULL)
      raw      <- tryCatch(.read_trial_raw(r$cfg),   error = function(e) NULL)
      r$status_v2     <- tryCatch(trial_status_v2(r, sites_df, raw),
                                  error = function(e) "setup")
      r$site_counts   <- tryCatch(trial_site_counts(r$cfg, sites_df),
                                  error = function(e) list(open=0L,total=0L,not_recruiting=0L))
      r$recent        <- tryCatch(trial_recent_activity(r$cfg, raw),
                                  error = function(e) list(label="\u2014", detail="", cls="cold"))
      r
    })
  })

  output$all_trials_count_lbl <- renderText({
    rows <- tryCatch(all_trials_enriched(), error = function(e) list())
    n <- length(rows)
    sprintf("%d %s", n, if (n == 1) "trial" else "trials")
  })

  output$home_all_trials_ui <- renderUI({
    rows <- tryCatch(all_trials_enriched(), error = function(e) list())
    if (length(rows) == 0) {
      return(div(class = "home-empty",
                 div(class = "icon", HTML("&#x1F4CB;")),
                 div("No trials yet.")))
    }

    # \u2500\u2500 Apply toolbar filters \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
    filt <- input$all_trials_filter %||% "all"
    if (!identical(filt, "all")) {
      rows <- Filter(function(r) identical(r$status_v2, filt), rows)
    }
    search <- tolower(trimws(input$all_trials_search %||% ""))
    if (nzchar(search)) {
      rows <- Filter(function(r) {
        cfg <- r$cfg
        hay <- tolower(paste(
          cfg$short_name %||% r$code, cfg$name %||% "",
          cfg$report_defaults$ci %||% "", cfg$report_defaults$sponsor %||% "",
          r$category %||% "", collapse = " "))
        grepl(search, hay, fixed = TRUE)
      }, rows)
    }

    # \u2500\u2500 Sort within each group \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
    sort_by <- input$all_trials_sort %||% "pct"
    sort_rows <- function(xs) {
      if (!length(xs)) return(xs)
      key <- switch(sort_by,
        "name"   = vapply(xs, function(r) tolower(r$cfg$short_name %||% r$code), character(1)),
        "recent" = -vapply(xs, function(r) {
                     # warm > warm-light > cool > cold
                     match(r$recent$cls, c("warm","warm-light","cool","cold"), nomatch = 5)
                   }, integer(1)),
        "status" = vapply(xs, function(r) {
                     # risk > warn > setup > on > closed
                     match(r$status_v2, c("risk","warn","setup","on","closed"),
                           nomatch = 9)
                   }, integer(1)),
                 -vapply(xs, function(r) r$pct %||% 0, numeric(1)))
      xs[order(key)]
    }

    if (length(rows) == 0) {
      return(div(class = "home-empty",
                 div(class = "icon", HTML("&#x1F50D;")),
                 div("No trials match the current filters.")))
    }

    cats_seen <- unique(vapply(rows, function(r) r$category, character(1)))
    blocks <- lapply(.ordered_categories(cats_seen), function(cat) {
      group <- sort_rows(Filter(function(r) identical(r$category, cat), rows))
      icon  <- TRIAL_CATEGORY_ICONS[[cat]] %||% TRIAL_CATEGORY_ICONS[["Other"]]

      # Category status mix for the group header
      mix <- function(code, label) {
        n <- sum(vapply(group, function(r) r$status_v2 == code, logical(1)))
        if (n == 0) NULL
        else tags$span(tagList(tags$b(n), " ", label))
      }
      head_stats <- div(class = "at-cat-head-stats",
        mix("on", "on track"), mix("warn", "behind"),
        mix("risk", "stalled"), mix("setup", "set-up"),
        mix("closed", "closed"))

      row_html <- lapply(group, function(r) {
        cfg <- r$cfg
        lbl <- trial_status_label(r$status_v2)
        pct <- max(0, min(1, r$pct %||% 0))
        site <- r$site_counts
        site_sub <- if (site$total == 0) "no sites yet"
                    else if (site$open == site$total) "all recruiting"
                    else if (site$not_recruiting > 0)
                      tags$span(tags$span(class = "neg", site$not_recruiting),
                                " not recruiting")
                    else
                      sprintf("%d in set-up", site$total - site$open)
        tags$div(class = sprintf("at-trial-row s-%s", lbl$cls),
                 onclick = sprintf("Shiny.setInputValue('select_trial','%s',{priority:'event'})", r$code),
          # Col 1 \u2014 Name
          div(class = "at-name",
            div(class = "at-name-row",
                tags$b(cfg$short_name %||% toupper(r$code)),
                tags$span(class = "at-tag", r$category %||% "\u2014")),
            tags$div(class = "at-name-full", cfg$name %||% "")),
          # Col 2 \u2014 Recruitment progress
          div(class = "at-progress",
            div(class = "at-bar-wrap",
                div(class = sprintf("at-bar-f s-%s", lbl$cls),
                    style = sprintf("width:%.0f%%;", pct * 100))),
            div(class = "at-bar-label",
                tags$span(tags$b(format(r$n, big.mark = ",")),
                          if (r$target > 0)
                            sprintf(" / %s recruited", format(r$target, big.mark = ","))
                          else " recruited"),
                tags$span(sprintf("%.0f%%", pct * 100)))),
          # Col 3 \u2014 Sites
          div(class = "at-stat",
            div(class = "at-stat-l", "Sites"),
            div(class = "at-stat-v",
                site$open,
                tags$span(style = "font-size:11px;color:var(--muted);font-weight:500;",
                          sprintf(" / %d", site$total))),
            div(class = "at-stat-s", site_sub)),
          # Col 4 \u2014 CI / Sponsor
          div(class = "at-meta",
            div(class = "role", "CI"),
            cfg$report_defaults$ci %||% tags$span(style = "color:var(--muted-2)", "\u2014"),
            tags$div(style = "margin-top:4px",
                     tags$span(class = "role", "Sponsor"), " ",
                     cfg$report_defaults$sponsor %||% tags$span(style = "color:var(--muted-2)", "\u2014"))),
          # Col 5 \u2014 Recent activity
          div(class = "at-recent",
            tags$span(class = sprintf("at-dot %s", r$recent$cls), r$recent$label),
            if (nzchar(r$recent$detail))
              tags$span(style = "font-size:10px;color:var(--muted-2)", r$recent$detail)),
          # Col 6 \u2014 Status pill + arrow
          div(class = "at-action",
            tags$div(class = sprintf("at-status s-%s", lbl$cls), lbl$text),
            tags$div(class = "at-arrow", HTML("&rarr;")))
        )
      })

      tagList(
        div(class = "at-cat-head",
          div(class = "at-cat-head-icon", HTML(icon)),
          div(class = "at-cat-head-title", cat),
          div(class = "at-cat-head-count",
              paste(length(group),
                    if (length(group) == 1) "trial" else "trials")),
          head_stats
        ),
        div(class = "at-trial-list", row_html)
      )
    })

    do.call(tagList, blocks)
  })

  # \u2500\u2500 Activity placeholder \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Reactive feed — invalidates when any of the obvious sources changes.
  activity_feed <- reactive({
    rv$home_membership_changed
    rv$settings_changed
    invalidateLater(60 * 1000)   # gentle background refresh
    list_activity(limit = 200,
                  trial_code = if (isTRUE(input$activity_trial_filter == "__all__"))
                    NULL else input$activity_trial_filter,
                  event_type = if (isTRUE(input$activity_event_filter == "__all__"))
                    NULL else input$activity_event_filter)
  })

  output$home_activity_ui <- renderUI({
    trials <- discover_trials()
    trial_choices <- c("All trials" = "__all__",
                       setNames(names(trials),
                                vapply(trials, function(t)
                                  t$short_name %||% toupper(t$code),
                                  character(1))))
    event_choices <- c("All events"          = "__all__",
                       "Trial created"       = "trial_created",
                       "Trial deleted"       = "trial_deleted",
                       "Site added"          = "site_added",
                       "Sites bulk-added"    = "sites_bulk_added",
                       "Site deleted"        = "site_deleted",
                       "CSV uploaded"        = "csv_uploaded",
                       "Amendment added"     = "amendment_added",
                       "Amendment edited"    = "amendment_edited",
                       "Amendment removed"   = "amendment_removed",
                       "Settings saved"      = "settings_saved",
                       "Membership changed"  = "membership_changed",
                       "Portfolio role"      = "portfolio_role_changed")

    feed <- activity_feed()
    rows <- if (is.null(feed) || !nrow(feed)) {
      div(class = "home-empty",
          div(class = "icon", HTML("&#x1F514;")),
          div(style = "font-size:15px;color:#0F172A;font-weight:500;margin-bottom:6px;",
              "No activity yet"),
          div("Trial creations, site changes, uploads and amendments will show up here."))
    } else {
      div(lapply(seq_len(nrow(feed)), function(i)
        render_activity_row(feed[i, ], trials)))
    }

    div(
      div(style = "display:grid;grid-template-columns:1fr 1fr;gap:14px;
                   margin-bottom:16px;",
          div(tags$label(style = "font-size:11px;font-weight:600;color:#64748B;
                                  text-transform:uppercase;letter-spacing:.5px;",
                         "Trial"),
              selectInput("activity_trial_filter", label = NULL,
                          choices = trial_choices,
                          selected = "__all__", width = "100%")),
          div(tags$label(style = "font-size:11px;font-weight:600;color:#64748B;
                                  text-transform:uppercase;letter-spacing:.5px;",
                         "Event type"),
              selectInput("activity_event_filter", label = NULL,
                          choices = event_choices,
                          selected = "__all__", width = "100%"))),
      div(style = "background:#FFFFFF;border:1px solid #EEF2F7;border-radius:14px;
                   padding:6px 22px;",
          rows)
    )
  })

  # ── Sites tab (cross-trial site performance) ─────────────────────────────
  cross_sites_long <- reactive({
    rv$home_membership_changed
    rv$settings_changed
    cross_trial_sites(discover_trials())
  })

  output$home_sites_ui <- renderUI({
    long <- tryCatch(cross_sites_long(), error = function(e) {
      message("cross_sites_long err: ", e$message)
      NULL
    })
    if (is.null(long) || !nrow(long)) {
      return(div(class = "home-empty",
                 div(class = "icon", HTML("&#x1F3E5;")),
                 div("No site data yet — open each trial once so its sites table populates.")))
    }

    cats <- unique(long$category)
    cat_choices <- c("All categories" = "__all__",
                     setNames(cats, cats))

    div(
      # Filter row
      div(style = "display:grid;grid-template-columns:1.2fr 2fr;gap:14px;
                   margin-bottom:18px;align-items:end;",
          div(tags$label(style = "font-size:11px;font-weight:600;color:#64748B;
                                  text-transform:uppercase;letter-spacing:.5px;",
                         "Filter by category"),
              selectInput("sites_category", label = NULL,
                          choices = cat_choices,
                          selected = "__all__", width = "100%")),
          div(tags$label(style = "font-size:11px;font-weight:600;color:#64748B;
                                  text-transform:uppercase;letter-spacing:.5px;",
                         "Look up a site"),
              selectizeInput("sites_lookup", label = NULL,
                             choices = c("", sort(unique(long$site_name))),
                             selected = "",
                             options = list(placeholder = "Type a hospital name…",
                                            allowEmptyOption = TRUE),
                             width = "100%"))
      ),

      # Two columns: top sites (left), site detail (right)
      div(style = "display:grid;grid-template-columns:1.2fr 1.4fr;gap:18px;",
          uiOutput("sites_top_ui"),
          uiOutput("sites_detail_ui"))
    )
  })

  # Top 5 sites for the chosen category
  output$sites_top_ui <- renderUI({
    long <- cross_sites_long()
    cat  <- input$sites_category %||% "__all__"
    in_scope <- if (identical(cat, "__all__")) long else long[long$category == cat, ]

    if (!nrow(in_scope)) {
      return(div(style = "background:#FFFFFF;border:1px solid #EEF2F7;border-radius:14px;
                          padding:24px;",
                 div(style = "font-size:13px;color:#64748B;",
                     "No sites in this category yet.")))
    }

    agg <- aggregate_sites(in_scope)
    agg <- agg[order(-agg$total_randomised), ]
    top <- head(agg, 5)

    rows <- lapply(seq_len(nrow(top)), function(i) {
      r <- top[i, ]
      pct <- if (r$total_target > 0) min(1, r$total_randomised / r$total_target) else 0
      pct_w <- sprintf("%.0f%%", pct * 100)
      div(style = "display:grid;grid-template-columns:auto 1fr auto;
                   gap:14px;align-items:center;
                   padding:14px 0;border-bottom:1px solid #EEF2F7;cursor:pointer;",
          onclick = sprintf("Shiny.setInputValue('sites_lookup','%s',{priority:'event'});
                             $('#sites_lookup').val('%s').trigger('change');",
                            gsub("'", "\\\\'", r$site_name),
                            gsub("'", "\\\\'", r$site_name)),
          div(style = "width:30px;height:30px;border-radius:9px;background:#F5F3FF;
                       color:#6366F1;display:flex;align-items:center;justify-content:center;
                       font-weight:700;font-size:13px;", i),
          div(div(style = "font-weight:600;color:#0F172A;font-size:13.5px;",
                  r$site_name),
              div(style = "font-size:11px;color:#64748B;",
                  sprintf("%d %s · %d recruited",
                          r$n_trials,
                          if (r$n_trials == 1) "trial" else "trials",
                          r$total_randomised)),
              div(class = "home-progress", style = "margin-top:6px;",
                  div(class = "home-progress-fill", style = sprintf("width:%s;", pct_w)))),
          div(style = "font-size:13px;font-weight:600;color:var(--accent);", pct_w))
    })

    div(style = "background:#FFFFFF;border:1px solid #EEF2F7;border-radius:14px;
                 padding:8px 24px 6px;",
        div(style = "font-size:11px;font-weight:600;color:var(--muted);
                     text-transform:uppercase;letter-spacing:.6px;
                     padding:14px 0 4px;",
            sprintf("Top %d sites%s",
                    nrow(top),
                    if (cat == "__all__") "" else paste(" — ", cat))),
        div(rows))
  })

  # Site detail card — shows when a site is selected
  output$sites_detail_ui <- renderUI({
    long <- cross_sites_long()
    site <- input$sites_lookup
    if (is.null(site) || !nzchar(site)) {
      return(div(style = "background:#FFFFFF;border:1px dashed #DDE5EE;border-radius:14px;
                          padding:32px;text-align:center;color:#64748B;font-size:13px;",
                 div(style = "font-size:30px;margin-bottom:10px;opacity:.4;",
                     HTML("&#x1F50D;")),
                 div("Pick a site from the list, top-5 panel, or search above to see how
                      it's performing across trials.")))
    }

    site_rows <- long[long$site_name == site, ]
    if (!nrow(site_rows)) {
      return(div(class = "home-empty", "Site not found."))
    }

    trial_blocks <- lapply(seq_len(nrow(site_rows)), function(i) {
      r <- site_rows[i, ]
      pct <- if (r$target > 0) min(1, r$randomised / r$target) else 0
      pct_w <- sprintf("%.0f%%", pct * 100)
      mt <- if (is.na(r$monthly_target) || r$monthly_target == 0) "—"
            else paste(r$monthly_target, "/ month")

      div(style = "padding:14px 0;border-bottom:1px solid #EEF2F7;",
          div(style = "display:flex;justify-content:space-between;align-items:center;
                       margin-bottom:8px;",
              div(div(style = "font-weight:600;color:#0F172A;font-size:14px;",
                      r$trial_short),
                  div(style = "font-size:11px;color:#64748B;",
                      sprintf("%s · status: %s", r$category,
                              r$status %||% "—"))),
              .status_pill(pct)),
          div(class = "home-progress", style = "margin:8px 0 6px;",
              div(class = "home-progress-fill",
                  style = sprintf("width:%s;", pct_w))),
          div(style = "display:flex;justify-content:space-between;
                       font-size:12px;color:#475569;",
              span(sprintf("%d / %d recruited", r$randomised, r$target)),
              span(sprintf("Monthly target: %s", mt)),
              span(style = "font-weight:600;color:var(--accent);", pct_w))
      )
    })

    div(style = "background:#FFFFFF;border:1px solid #EEF2F7;border-radius:14px;
                 padding:18px 24px 10px;",
        div(style = "display:flex;align-items:center;gap:10px;margin-bottom:6px;",
            div(style = "width:34px;height:34px;border-radius:10px;background:#F5F3FF;
                         color:#6366F1;display:flex;align-items:center;justify-content:center;
                         font-size:14px;", HTML("&#x1F3E5;")),
            div(div(style = "font-weight:600;color:#0F172A;font-size:15px;letter-spacing:-.2px;",
                    site),
                div(style = "font-size:11.5px;color:#64748B;",
                    sprintf("Active in %d %s",
                            nrow(site_rows),
                            if (nrow(site_rows) == 1) "trial" else "trials")))),

        div(style = "font-size:11px;font-weight:600;color:var(--muted);
                     text-transform:uppercase;letter-spacing:.6px;
                     padding:14px 0 4px;",
            "Per-trial recruitment"),
        div(trial_blocks))
  })

  # \u2500\u2500 Topbar dropdown \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  output$home_dropdown_ui <- renderUI({
    is_admin <- isTRUE(rv$portfolio_role == "admin")
    items <- list(
      div(class = "home-dropdown-item",
          onclick = "Shiny.setInputValue('home_change_password', Math.random(), {priority:'event'})",
          "Change Password")
    )
    if (is_admin) {
      items <- c(items, list(
        div(class = "home-dropdown-item",
            onclick = "Shiny.setInputValue('home_manage_users', Math.random(), {priority:'event'})",
            "Manage Users"),
        div(class = "home-dropdown-item",
            onclick = "Shiny.setInputValue('home_backup_restore', Math.random(), {priority:'event'})",
            "Backup / Restore")
      ))
    }
    items <- c(items, list(
      div(class = "home-dropdown-divider"),
      div(class = "home-dropdown-item",
          onclick = "Shiny.setInputValue('home_sign_out', Math.random(), {priority:'event'})",
          "Sign Out")
    ))
    div(class = "home-dropdown", do.call(tagList, items))
  })

  observeEvent(input$home_change_password, {
    if (is.null(rv$username) || !nzchar(rv$username)) return()
    showModal(modalDialog(
      title = "Change password",
      size = "s", easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("change_password_go", "Update password",
                     class = "btn btn-primary",
                     style = "background:#6366F1;border-color:#6366F1;font-weight:600;")
      ),
      div(style = "padding:6px 0;font-size:12.5px;color:#475569;line-height:1.6;
                   margin-bottom:12px;",
          sprintf("Updating password for %s", rv$username)),
      passwordInput("change_pw_current", "Current password",
                    width = "100%"),
      passwordInput("change_pw_new",     "New password",
                    placeholder = "At least 6 characters", width = "100%"),
      passwordInput("change_pw_confirm", "Confirm new password",
                    width = "100%")
    ))
  })

  observeEvent(input$change_password_go, {
    cur     <- input$change_pw_current %||% ""
    new_pw  <- input$change_pw_new     %||% ""
    confirm <- input$change_pw_confirm %||% ""

    if (!verify_password(rv$username, cur)) {
      showNotification("Current password is incorrect.", type = "error")
      return()
    }
    if (nchar(new_pw) < 6) {
      showNotification("New password must be at least 6 characters.",
                       type = "warning")
      return()
    }
    if (!identical(new_pw, confirm)) {
      showNotification("New passwords don't match.", type = "warning")
      return()
    }
    set_password(rv$username, new_pw)
    removeModal()
    showNotification("Password updated.", type = "message", duration = 4)
  })
  observeEvent(input$home_sign_out, {
    session$reload()
  })

  # \u2500\u2500 Manage Users (admin only) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  observeEvent(input$home_manage_users, {
    if (!isTRUE(rv$portfolio_role == "admin")) return()
    open_user_management()
  })

  # \u2500\u2500 Backup / Restore (admin only) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  observeEvent(input$home_backup_restore, {
    if (!isTRUE(rv$portfolio_role == "admin")) return()
    showModal(modalDialog(
      title = div(style = "display:flex;align-items:center;gap:10px;",
                  span(style = "font-size:18px;color:#6366F1;", HTML("&#x1F4E6;")),
                  span("Backup & Restore")),
      size = "m", easyClose = TRUE, footer = modalButton("Close"),

      div(style = "background:#F8FAFD;border:1px solid #EEF2F7;border-radius:10px;
                   padding:14px 16px;margin-bottom:18px;font-size:12.5px;
                   color:#475569;line-height:1.7;",
          HTML("Bundles every trial's <code>config.R</code>, <code>overrides.json</code>,
                and per-trial SQLite, plus the shared user database. <strong>REDCap CSV
                exports are not included</strong> \u2014 keep those local."),
          tags$br(), tags$br(),
          HTML("Restoring overwrites local copies with the contents of the zip.")),

      div(style = "display:grid;grid-template-columns:1fr 1fr;gap:14px;",
          # Backup
          div(style = "background:#FFFFFF;border:1px solid #EEF2F7;border-radius:10px;
                       padding:16px;",
              div(style = "font-size:13px;font-weight:600;color:#0F172A;margin-bottom:6px;",
                  HTML("&#x2B07; Backup")),
              div(style = "font-size:11.5px;color:#64748B;margin-bottom:14px;",
                  "Download the current portfolio as a single zip."),
              downloadButton("portfolio_backup_dl", "Download backup (.zip)",
                             class = "btn btn-sm",
                             style = "background:#6366F1;color:#fff;border:none;
                                      width:100%;padding:8px;font-weight:500;")),
          # Restore
          div(style = "background:#FFFFFF;border:1px solid #FECACA;border-radius:10px;
                       padding:16px;",
              div(style = "font-size:13px;font-weight:600;color:#0F172A;margin-bottom:6px;",
                  HTML("&#x21BB; Restore")),
              div(style = "font-size:11.5px;color:#64748B;margin-bottom:10px;",
                  "Pick a previous backup. ", tags$strong("Will overwrite current data."), ""),
              fileInput("portfolio_backup_file", label = NULL,
                        buttonLabel = "Choose .zip", accept = ".zip",
                        placeholder = "No file selected"),
              actionButton("portfolio_restore_go", "Restore from selected zip",
                           class = "btn btn-sm",
                           style = "background:#FFFFFF;color:#B91C1C;
                                    border:1px solid #FECACA;width:100%;
                                    padding:8px;font-weight:500;")
          ))
    ))
  })

  output$portfolio_backup_dl <- downloadHandler(
    filename = function() {
      sprintf("BCTU_portfolio_backup_%s.zip", format(Sys.time(), "%Y%m%d_%H%M"))
    },
    content = function(file) {
      tryCatch(write_portfolio_backup(file),
        error = function(e) {
          showNotification(paste("Backup failed:", e$message),
                           type = "error", duration = 8)
        })
    }
  )

  observeEvent(input$portfolio_restore_go, {
    if (!isTRUE(rv$portfolio_role == "admin")) return()
    f <- input$portfolio_backup_file
    if (is.null(f) || !nrow(f)) {
      showNotification("Choose a backup .zip first.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = div(style = "color:#B91C1C;",
                  HTML("&#x26A0; Confirm restore")),
      size = "s", easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("portfolio_restore_confirm", "Yes, restore",
                     class = "btn btn-danger",
                     style = "background:#DC2626;border-color:#DC2626;font-weight:600;")
      ),
      div(style = "padding:6px 0;font-size:13px;line-height:1.7;",
          HTML(sprintf("Restoring <strong>%s</strong> will overwrite every
                        trial's config, overrides, and SQLite, plus the shared
                        user database. <strong>This cannot be undone.</strong>",
                       htmltools::htmlEscape(f$name))))
    ))
  })

  observeEvent(input$portfolio_restore_confirm, {
    if (!isTRUE(rv$portfolio_role == "admin")) return()
    f <- input$portfolio_backup_file
    if (is.null(f) || !nrow(f)) { removeModal(); return() }
    res <- tryCatch(restore_portfolio_backup(f$datapath),
      error = function(e) list(error = e$message))
    removeModal()
    if (!is.null(res$error)) {
      showNotification(paste("Restore failed:", res$error),
                       type = "error", duration = 10)
      return()
    }
    showNotification(
      sprintf("Restored %d files (%d skipped). Reload the page to see changes.",
              res$restored, res$skipped),
      type = "message", duration = 12)
    rv$home_membership_changed <- Sys.time()
  })

  # ── User management console (master-detail) ──────────────────────────────
  mu_selected_user <- reactiveVal(NULL)
  mu_last_temp      <- reactiveVal(NULL)   # list(user, pw) — one-time temp display
  mu_mode           <- reactiveVal("detail")  # "detail" | "create"

  # Open the console: default-select the first user, clear transient state.
  open_user_management <- function() {
    users <- tryCatch(list_all_users(), error = function(e) data.frame())
    mu_selected_user(if (nrow(users)) users$fullname[1] else NULL)
    mu_last_temp(NULL)
    mu_mode("detail")
    shinyjs::runjs("Shiny.setInputValue('mu_search', '');")
    showModal(manage_users_modal())
  }

  # Enter / leave the "create user" form.
  observeEvent(input$mu_new_user, {
    mu_selected_user(NULL); mu_last_temp(NULL); mu_mode("create")
  })
  observeEvent(input$mu_cancel_create, { mu_mode("detail") })

  # Build the create-user form shown in the detail pane.
  .mu_create_form <- function() {
    div(class = "mu-create",
        div(class = "mu-detail-head",
            span(class = "mu-avatar mu-avatar-lg", HTML("&#43;")),
            div(div(class = "mu-detail-name", "New user"),
                div(class = "mu-detail-sub",
                    "Create a profile and set their starting password"))),
        div(class = "mu-section",
            div(class = "mu-section-label", "Details"),
            div(class = "mu-form-grid",
                div(class = "mu-field", tags$label("Full name *"),
                    textInput("mu_new_name", NULL, placeholder = "e.g. Jane Smith", width = "100%")),
                div(class = "mu-field", tags$label("Email *"),
                    textInput("mu_new_email", NULL, placeholder = "jane.smith@bham.ac.uk", width = "100%")),
                div(class = "mu-field", tags$label("Job title"),
                    textInput("mu_new_jobtitle", NULL, placeholder = "e.g. Trial Manager", width = "100%")),
                div(class = "mu-field", tags$label("Portfolio role"),
                    selectInput("mu_new_portrole", NULL,
                                c("Member" = "member", "Admin" = "admin"), width = "100%")))),
        div(class = "mu-section",
            div(class = "mu-section-label", "Starting password"),
            div(class = "mu-field",
                passwordInput("mu_new_initpw", NULL,
                              placeholder = "Leave blank to auto-generate", width = "100%")),
            div(class = "mu-hint",
                "Leave blank and a temporary one is generated and shown to you. Either way the user sets their own password at first login.")),
        div(class = "mu-create-actions",
            actionButton("mu_cancel_create", "Cancel", class = "mu-btn mu-btn-ghost"),
            actionButton("mu_create_user", HTML("&#43; Create user"), class = "mu-btn mu-btn-navy")))
  }

  observeEvent(input$mu_create_user, {
    name   <- trimws(input$mu_new_name %||% "")
    email  <- trimws(input$mu_new_email %||% "")
    jobt   <- trimws(input$mu_new_jobtitle %||% "")
    prole  <- input$mu_new_portrole %||% "member"
    initpw <- input$mu_new_initpw %||% ""

    if (!nzchar(name)) { showNotification("Enter a full name.", type = "warning"); return() }
    users <- list_all_users()
    if (tolower(name) %in% tolower(users$fullname)) {
      showNotification("A user with that name already exists.", type = "warning"); return()
    }
    if (!.is_valid_email(email)) {
      showNotification("Enter a valid email address.", type = "warning"); return()
    }
    if (!is.null(find_profile_by_email(email))) {
      showNotification("That email is already linked to a profile.", type = "warning"); return()
    }
    if (nzchar(initpw) && nchar(initpw) < 6) {
      showNotification("Password must be at least 6 characters.", type = "warning"); return()
    }

    ok <- tryCatch({
      db_save_profile(name, role = if (nzchar(jobt)) jobt else "Member",
                      password = NULL, email = email)
      TRUE
    }, error = function(e) {
      showNotification(paste("Couldn't create user:", e$message), type = "error", duration = 8)
      FALSE
    })
    if (!ok) return()

    # Set the starting password (typed or auto-generated) and force a change at
    # first login. Promote to admin if requested.
    res <- tryCatch(
      admin_reset_password(name, admin_fullname = rv$username,
                           new_password = if (nzchar(initpw)) initpw else NULL),
      error = function(e) list(success = FALSE))
    if (identical(prole, "admin")) set_portfolio_role(name, "admin")

    log_activity("user_created",
                 sprintf("Created user <strong>%s</strong>", htmltools::htmlEscape(name)),
                 username = rv$username)

    mu_selected_user(name)
    mu_mode("detail")
    mu_last_temp(if (isTRUE(res$success)) list(user = name, pw = res$temp_password) else NULL)
    rv$home_membership_changed <- Sys.time()
    showNotification(sprintf("Created %s.", name), type = "message", duration = 4)
  })

  .mu_initials <- function(name) {
    parts <- strsplit(trimws(name), "\\s+")[[1]]
    parts <- parts[nzchar(parts)]
    if (!length(parts)) return("?")
    toupper(paste0(substr(parts[1], 1, 1),
                   if (length(parts) > 1) substr(parts[length(parts)], 1, 1) else ""))
  }

  # Left rail: searchable list of users.
  output$mu_user_list_ui <- renderUI({
    rv$home_membership_changed   # refresh trigger
    users <- list_all_users()
    if (nrow(users) == 0) return(div(class = "mu-empty", "No users yet."))

    q <- tolower(trimws(input$mu_search %||% ""))
    if (nzchar(q)) users <- users[grepl(q, tolower(users$fullname), fixed = TRUE), , drop = FALSE]
    if (nrow(users) == 0) return(div(class = "mu-empty", "No users match your search."))

    sel <- mu_selected_user()
    lapply(seq_len(nrow(users)), function(i) {
      u    <- users[i, ]
      nmem <- nrow(list_user_memberships_for(u$fullname))
      is_admin <- identical(u$portfolio_role, "admin")
      tags$button(
        type = "button",
        class = paste("mu-user-item", if (identical(u$fullname, sel)) "active" else ""),
        onclick = sprintf("Shiny.setInputValue('mu_select', {user:'%s', n:Math.random()}, {priority:'event'})",
                          gsub("'", "\\\\'", u$fullname)),
        span(class = "mu-avatar", .mu_initials(u$fullname)),
        span(class = "mu-user-meta",
             span(class = "mu-user-name", u$fullname),
             span(class = "mu-user-sub",
                  sprintf("%s · %s", if (is_admin) "Admin" else "Member",
                          if (nmem == 1) "1 trial" else paste0(nmem, " trials")))),
        if (is_admin) span(class = "mu-badge mu-badge-admin", "Admin")
      )
    })
  })

  observeEvent(input$mu_select, {
    mu_selected_user(input$mu_select$user)
    mu_last_temp(NULL)   # don't carry a temp password across users
    mu_mode("detail")
  })

  observeEvent(input$home_set_portrole, {
    u <- input$home_set_portrole$user
    r <- input$home_set_portrole$role
    set_portfolio_role(u, r)
    rv$home_membership_changed <- Sys.time()
    # Refresh own session if you changed your own role
    if (identical(u, rv$username)) rv$portfolio_role <- r
    log_activity("portfolio_role_changed",
                 sprintf("Set <strong>%s</strong> portfolio role to <strong>%s</strong>",
                         htmltools::htmlEscape(u), htmltools::htmlEscape(r)),
                 username = rv$username)
    showNotification(sprintf("%s is now %s.", u, r),
                     type = "message", duration = 3)
  })

  # Right pane: the selected user's role, per-trial access and password actions.
  output$mu_detail_ui <- renderUI({
    rv$home_membership_changed
    if (identical(mu_mode(), "create")) return(.mu_create_form())
    u <- mu_selected_user()
    if (is.null(u))
      return(div(class = "mu-detail-empty",
                 HTML('<svg width="34" height="34" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5"><path d="M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2"/><circle cx="9" cy="7" r="4"/></svg>'),
                 div("Select a user, or create a new one.")))

    users <- list_all_users()
    urow  <- users[users$fullname == u, , drop = FALSE]
    if (!nrow(urow)) return(div(class = "mu-detail-empty", "User not found."))

    is_admin  <- identical(urow$portfolio_role[1], "admin")
    has_pw    <- tryCatch(profile_has_password(u),        error = function(e) FALSE)
    reset_req <- tryCatch(is_password_reset_required(u),  error = function(e) FALSE)
    trials    <- discover_trials()
    mems      <- list_user_memberships_for(u)
    # A list (not a named vector) so mem_lookup[[tcode]] returns NULL — not a
    # "subscript out of bounds" error — for trials the user isn't a member of.
    mem_lookup <- setNames(as.list(mems$trial_role), mems$trial_code)

    portrole_btn <- function(val, label) {
      tags$button(type = "button",
        class = paste("mu-seg-btn", if ((val == "admin") == is_admin) "active" else ""),
        onclick = sprintf("Shiny.setInputValue('home_set_portrole',{user:'%s',role:'%s',n:Math.random()},{priority:'event'})",
                          gsub("'", "\\\\'", u), val),
        label)
    }

    trial_rows <- if (!length(trials)) {
      div(class = "mu-hint", "No trials in the portfolio yet.")
    } else lapply(names(trials), function(tcode) {
      cfg <- trials[[tcode]]; current <- mem_lookup[[tcode]] %||% "none"
      div(class = "mu-trial-row",
          div(class = "mu-trial-name",
              span(class = paste("mu-trial-dot", if (current != "none") "on" else "")),
              tags$strong(cfg$short_name %||% toupper(tcode))),
          tags$select(class = "mu-select",
            onchange = sprintf("Shiny.setInputValue('home_set_mem',{user:'%s',trial:'%s',role:this.value,n:Math.random()},{priority:'event'})",
                              gsub("'", "\\\\'", u), tcode),
            tags$option(value = "none",         selected = if (current == "none") NA else NULL,         "No access"),
            tags$option(value = "manager",      selected = if (current == "manager") NA else NULL,      "Manager"),
            tags$option(value = "coordinator",  selected = if (current == "coordinator") NA else NULL,  "Coordinator"),
            tags$option(value = "statistician", selected = if (current == "statistician") NA else NULL, "Statistician"),
            tags$option(value = "readonly",     selected = if (current == "readonly") NA else NULL,     "Read-only")))
    })

    temp <- mu_last_temp()
    temp_box <- if (!is.null(temp) && identical(temp$user, u))
      div(class = "mu-temp",
          div(class = "mu-temp-label", "Temporary password — copy and share securely, then it's gone:"),
          div(class = "mu-temp-pw", temp$pw),
          div(class = "mu-temp-note", "The user must set their own password the next time they sign in.")) else NULL

    pw_status <- if (!has_pw) span(class = "mu-pill mu-pill-warn", "No password set")
      else if (reset_req)    span(class = "mu-pill mu-pill-warn", "Reset pending — must change at next login")
      else                   span(class = "mu-pill mu-pill-ok",   "Password set")

    tagList(
      div(class = "mu-detail-head",
          span(class = "mu-avatar mu-avatar-lg", .mu_initials(u)),
          div(div(class = "mu-detail-name", u),
              div(class = "mu-detail-sub",
                  sprintf("%s%s", urow$role[1] %||% "",
                          if (!is.na(urow$created[1] %||% NA) && nzchar(urow$created[1] %||% ""))
                            paste0(" · added ", urow$created[1]) else "")))),

      div(class = "mu-section",
          div(class = "mu-section-label", "Portfolio role"),
          div(class = "mu-seg", portrole_btn("member", "Member"), portrole_btn("admin", "Admin")),
          div(class = "mu-hint",
              if (is_admin) "Admins see and manage every trial in the portfolio."
              else "Members see only the trials granted below.")),

      div(class = "mu-section",
          div(class = "mu-section-label", "Trial access"),
          if (is_admin) div(class = "mu-hint",
              "This user is an admin and already has manager access to every trial."),
          div(class = "mu-trial-list", trial_rows)),

      div(class = "mu-section",
          div(class = "mu-section-label", "Security"),
          div(class = "mu-pw-status", pw_status),
          temp_box,
          actionButton("mu_reset_pw", HTML("&#8634; Reset to a temporary password"),
                       class = "mu-btn mu-btn-amber mu-btn-block"),
          div(class = "mu-pw-set",
              tags$label("Or set a specific password"),
              div(class = "mu-pw-set-row",
                  passwordInput("mu_new_pw", label = NULL,
                                placeholder = "New password (min 6 chars)", width = "100%"),
                  actionButton("mu_set_pw", "Set", class = "mu-btn mu-btn-navy"))),
          div(class = "mu-hint",
              "Both options force the user to choose their own password next time they sign in. Existing passwords are encrypted and can never be viewed."))
    )
  })

  observeEvent(input$mu_reset_pw, {
    u <- mu_selected_user(); if (is.null(u)) return()
    res <- tryCatch(admin_reset_password(u, admin_fullname = rv$username),
                    error = function(e) list(success = FALSE, message = e$message))
    if (isTRUE(res$success)) {
      mu_last_temp(list(user = u, pw = res$temp_password))
      rv$home_membership_changed <- Sys.time()
      showNotification(sprintf("Temporary password generated for %s.", u),
                       type = "message", duration = 4)
    } else {
      showNotification(res$message %||% "Couldn't reset password.", type = "error", duration = 6)
    }
  })

  observeEvent(input$mu_set_pw, {
    u <- mu_selected_user(); if (is.null(u)) return()
    pw <- input$mu_new_pw %||% ""
    if (nchar(pw) < 6) {
      showNotification("Password must be at least 6 characters.", type = "warning")
      return()
    }
    res <- tryCatch(admin_reset_password(u, admin_fullname = rv$username, new_password = pw),
                    error = function(e) list(success = FALSE, message = e$message))
    if (isTRUE(res$success)) {
      mu_last_temp(NULL)
      rv$home_membership_changed <- Sys.time()
      showNotification(sprintf("Password set for %s — they'll choose their own at next login.", u),
                       type = "message", duration = 5)
    } else {
      showNotification(res$message %||% "Couldn't set password.", type = "error", duration = 6)
    }
  })

  observeEvent(input$home_set_mem, {
    u <- input$home_set_mem$user
    t <- input$home_set_mem$trial
    r <- input$home_set_mem$role
    if (r == "none") {
      revoke_membership(u, t)
      log_activity("membership_changed",
                   sprintf("Revoked <strong>%s</strong>'s access",
                           htmltools::htmlEscape(u)),
                   username = rv$username, trial_code = t)
    } else {
      grant_membership(u, t, r)
      log_activity("membership_changed",
                   sprintf("Set <strong>%s</strong> as <strong>%s</strong>",
                           htmltools::htmlEscape(u), htmltools::htmlEscape(r)),
                   username = rv$username, trial_code = t)
    }
    rv$home_membership_changed <- Sys.time()
  })


  # ══════════════════════════════════════════════════════════════════════════
  # TRIAL SELECTION (existing logic)
  # ══════════════════════════════════════════════════════════════════════════

  observeEvent(input$select_trial, {
    code <- input$select_trial
    cfg  <- rv$available_trials[[code]]
    if (is.null(cfg)) return()

    # Permission check
    role_here <- user_trial_role(rv$username, code)
    if (is.null(role_here)) {
      showNotification(
        sprintf("You don't have access to %s. Ask an admin to add you.",
                cfg$short_name %||% toupper(code)),
        type = "warning", duration = 6)
      return()
    }

    rv$trial_config <- cfg
    rv$trial_code   <- code
    rv$trial_role   <- role_here

    apply_trial_globals(cfg)
    # Lazy-seed the trial's report templates if they're not yet on disk
    # (legacy trials that pre-date the per-trial report templates flow).
    tryCatch(seed_trial_report_templates(cfg, overwrite = FALSE),
             error = function(e) message("Template seed failed: ", e$message))
    apply_trial_role_visibility(role_here)
    if (!dir.exists(dirname(DB_PATH))) dir.create(dirname(DB_PATH), recursive = TRUE)
    db_init()

    rv$sites <- db_load_sites()
    rv$log   <- db_load_log()

    trial_name <- cfg$short_name %||% toupper(code)
    runjs(sprintf("$('.topbar-title').text('%s')", trial_name))
    runjs(sprintf("document.title = '%s Dashboard'", trial_name))

    shinyjs::hide("trial_selector_panel")
    shinyjs::show("dashboard_panel")
    shinyjs::show("sidebar_nav_section")    # Show the sidebar nav
    shinyjs::show("topbar_wrap")            # Show the topbar
    shinyjs::show("topnav_wrap")            # Show the top tab bar
    shinyjs::runjs("document.body.classList.remove('home-mode')")

    rv$trigger_data_load <- Sys.time()

    # Apply feature flags — show/hide tabs based on config
    feat <- cfg$features %||% list()
    if (isTRUE(feat$postal_tracking)) {
      shinyjs::show("go_postal_wrap")
      shinyjs::show("tn_postal")
    } else {
      shinyjs::hide("go_postal_wrap")
      shinyjs::hide("tn_postal")
    }
    if (isTRUE(feat$return_rates)) {
      shinyjs::show("go_returns_wrap")
      shinyjs::show("tn_returns")
    } else {
      shinyjs::hide("go_returns_wrap")
      shinyjs::hide("tn_returns")
    }
    # PROMs section on the Data tab — defaults to TRUE for legacy configs
    if (isTRUE(feat$participant_questionnaires %||% TRUE)) {
      shinyjs::show("participant_questionnaire_section")
      shinyjs::show("participant_questionnaire_grid")
    } else {
      shinyjs::hide("participant_questionnaire_section")
      shinyjs::hide("participant_questionnaire_grid")
    }

    # Apply trial colours dynamically
    .theme_key <- cfg$theme %||% "custom"
    .sidebar_variant <- if (.theme_key %in% names(TRIAL_THEMES))
      TRIAL_THEMES[[.theme_key]]$sidebar else "dark"
    apply_trial_colours(
      cfg$colors %||% list(primary = "#1B4F6B", secondary = "#2EC4A5", accent = "#F59E0B"),
      sidebar = .sidebar_variant
    )
  })


  # ══════════════════════════════════════════════════════════════════════════
  # WIZARD: step navigation
  # ══════════════════════════════════════════════════════════════════════════

  # Slugify a follow-up timepoint label into a redcap_events role key, e.g.
  # "Day 30" -> "day_30", "6 months" -> "6_months". Shared by the dynamic
  # event-name fields and the create handler.
  .wiz_tp_slug <- function(s) {
    s <- tolower(trimws(s))
    s <- gsub("[^a-z0-9]+", "_", s)
    gsub("^_+|_+$", "", s)
  }

  # ── Multi-work-package wizard panel ─────────────────────────────────────
  # Toggling the checkbox shows/hides the WP count + dynamic name fields, and
  # the "separate export per work package" option on the Data step.
  observeEvent(input$wiz_is_multi_wp, {
    on <- isTRUE(input$wiz_is_multi_wp)
    shinyjs::toggle("wiz_wp_panel",         condition = on)
    shinyjs::toggle("wiz_multi_export_wrap", condition = on)
    if (!on) updateCheckboxInput(session, "wiz_multi_export", value = FALSE)
  }, ignoreNULL = FALSE)

  # Separate-export-per-WP toggle: show the per-WP folder fields and hide the
  # single data-folder field when on.
  observeEvent(input$wiz_multi_export, {
    on <- isTRUE(input$wiz_multi_export) && isTRUE(input$wiz_is_multi_wp)
    shinyjs::toggle("wiz_wp_export_panel",    condition = on)
    shinyjs::toggle("wiz_single_export_wrap", condition = !on)
  }, ignoreNULL = FALSE)

  # One data-folder field per work package (multi-export). Preserves typed
  # values across re-render via isolate(input[[id]]).
  output$wiz_wp_export_fields_ui <- renderUI({
    if (!isTRUE(input$wiz_is_multi_wp)) return(NULL)
    n <- suppressWarnings(as.integer(input$wiz_n_wps))
    if (is.na(n) || n < 1) n <- 1
    if (n > 10) n <- 10
    rows <- lapply(seq_len(n), function(i) {
      id  <- paste0("wiz_wp_data_", i)
      nm  <- trimws(input[[paste0("wiz_wp_name_", i)]] %||% "")
      lbl <- if (nzchar(nm)) sprintf("WKP%d · %s", i, nm) else paste0("WKP", i)
      div(class = "nt-field",
          tags$label(lbl, style = "font-size:12px;font-weight:600;"),
          textInput(id, label = NULL,
                    value = isolate(input[[id]]) %||% "",
                    placeholder = sprintf("K:/BCTU/Teams/MyTeam/MyTrial/WKP%d", i),
                    width = "100%"))
    })
    tagList(
      div(class = "nt-group-label", style = "margin-top:6px;", "Export folder per work package"),
      rows)
  })

  # One REDCap-event-name box per chosen follow-up timepoint (step 3). The role
  # key is the slugified label, matched by the create handler.
  output$wiz_tp_fields_ui <- renderUI({
    tps <- input$wiz_timepoints
    if (is.null(tps) || !length(tps)) {
      return(div(class = "nt-hint", "No follow-up timepoints selected — Baseline only."))
    }
    seen <- character(0)
    rows <- lapply(tps, function(tp) {
      key <- .wiz_tp_slug(tp)
      if (!nzchar(key) || key %in% seen) return(NULL)
      seen <<- c(seen, key)
      id  <- paste0("wiz_tp_evt_", key)
      div(style = "display:grid;grid-template-columns:150px 1fr;gap:10px;align-items:center;margin-bottom:8px;",
          tags$label(tp, style = "font-size:12px;font-weight:600;color:var(--ov-navy);
                                  background:var(--ov-rail);border:1px solid var(--ov-line);
                                  border-radius:6px;padding:7px 10px;text-align:center;margin:0;"),
          textInput(id, label = NULL,
                    value = isolate(input[[id]]) %||% "",
                    placeholder = paste0(key, "_arm_1"),
                    width = "100%"))
    })
    div(style = "margin-top:10px;", rows)
  })

  # Capture toggles on the Field-mapping step: default the procedure group on
  # for Surgery and off otherwise, and show/hide each optional group.
  observeEvent(input$wiz_category, {
    is_surgery <- identical(input$wiz_category, "Surgery")
    updateCheckboxInput(session, "wiz_cap_procedure", value = is_surgery)
  })
  observeEvent(input$wiz_cap_procedure, {
    shinyjs::toggle("wiz_procedure_fields", condition = isTRUE(input$wiz_cap_procedure))
  }, ignoreNULL = FALSE)
  observeEvent(input$wiz_cap_demographics, {
    shinyjs::toggle("wiz_demographics_fields", condition = isTRUE(input$wiz_cap_demographics))
  }, ignoreNULL = FALSE)

  # Render N labelled text inputs (WKP1 ... WKPN) when multi-WP is enabled.
  # Re-rendering preserves user-typed values via input[[id]] lookup so users
  # don't lose edits when bumping the count up or down.
  output$wiz_wp_fields_ui <- renderUI({
    if (!isTRUE(input$wiz_is_multi_wp)) return(NULL)
    n <- suppressWarnings(as.integer(input$wiz_n_wps))
    if (is.na(n) || n < 1) n <- 1
    if (n > 10) n <- 10
    rows <- lapply(seq_len(n), function(i) {
      id  <- paste0("wiz_wp_name_", i)
      lbl <- paste0("WKP", i, " name")
      div(style = "display:grid;grid-template-columns:80px 1fr;gap:10px;
                   align-items:center;margin-bottom:6px;",
          tags$label(paste0("WKP", i),
                     style = "font-size:12px;font-weight:600;color:#1B4F6B;
                              background:#EEF3F8;border-radius:6px;
                              padding:6px 10px;text-align:center;margin:0;"),
          textInput(id, label = NULL,
                    value = isolate(input[[id]]) %||% "",
                    placeholder = "e.g. Surgery cohort",
                    width = "100%"))
    })
    div(style = "margin-top:4px;", rows)
  })

  wiz_step  <- reactiveVal(1L)
  WIZ_TOTAL <- 6L

  # Step-1 validation, shared by Next and the clickable stepper.
  .wiz_validate_step1 <- function() {
    sn <- trimws(input$wiz_short_name %||% "")
    if (!nzchar(sn)) {
      showNotification("Please enter a short name for the trial.", type = "warning")
      return(FALSE)
    }
    code <- tolower(gsub("[^a-zA-Z0-9]", "", sn))
    if (code %in% names(rv$available_trials)) {
      showNotification(paste0("A trial with code '", code, "' already exists."),
                       type = "warning")
      return(FALSE)
    }
    TRUE
  }

  # Show step n (1..WIZ_TOTAL) and sync footer buttons. Used by open, next,
  # prev and the stepper.
  show_step <- function(n) {
    n <- max(1L, min(WIZ_TOTAL, as.integer(n)))
    for (i in seq_len(WIZ_TOTAL))
      shinyjs::toggle(paste0("wiz_step_", i), condition = (i == n))
    shinyjs::toggle("wiz_prev",   condition = n > 1L)
    shinyjs::toggle("wiz_next",   condition = n < WIZ_TOTAL)
    shinyjs::toggle("wiz_create", condition = n == WIZ_TOTAL)
    wiz_step(n)
    shinyjs::runjs("var m=document.querySelector('.nt-main'); if(m){m.scrollTop=0;} window.scrollTo(0,0);")
  }

  # Open the full-page setup (swap out the home selector) and reset the form.
  open_new_trial <- function() {
    shinyjs::reset("new_trial_form")
    shinyjs::hide("trial_selector_panel")
    shinyjs::show("new_trial_panel")
    show_step(1L)
  }
  close_new_trial <- function() {
    shinyjs::hide("new_trial_panel")
    shinyjs::show("trial_selector_panel")
  }

  observeEvent(input$open_wizard, open_new_trial())
  observeEvent(input$wiz_cancel,  close_new_trial())

  # Stepper rail — one row per step (done / active / upcoming). Visited steps
  # are clickable to jump back and forth.
  output$wiz_step_indicator <- renderUI({
    step <- wiz_step()
    rows <- lapply(NT_STEPS, function(s) {
      i <- s$n
      state  <- if (i == step) "active" else if (i < step) "done" else "upcoming"
      marker <- if (i < step) HTML("&#x2713;") else as.character(i)
      tags$button(
        type = "button",
        class = paste("nt-step-item", state),
        onclick = sprintf("Shiny.setInputValue('wiz_goto', %d, {priority:'event'})", i),
        span(class = "nt-step-marker", marker),
        span(class = "nt-step-text",
             span(class = "nt-step-title", s$title),
             span(class = "nt-step-blurb", s$blurb)))
    })
    div(class = "nt-stepper",
        div(class = "nt-stepper-head", sprintf("Step %d of %d", step, WIZ_TOTAL)),
        div(class = "nt-step-list", rows))
  })

  observeEvent(input$wiz_goto, {
    target <- suppressWarnings(as.integer(input$wiz_goto))
    if (is.na(target)) return()
    if (wiz_step() == 1L && target > 1L && !.wiz_validate_step1()) return()
    show_step(target)
  })

  observeEvent(input$wiz_next, {
    step <- wiz_step()
    if (step == 1L && !.wiz_validate_step1()) return()
    if (step < WIZ_TOTAL) show_step(step + 1L)
  })
  observeEvent(input$wiz_prev, {
    if (wiz_step() > 1L) show_step(wiz_step() - 1L)
  })

  # Review summary
  output$wiz_review_summary <- renderUI({
    sn   <- trimws(input$wiz_short_name %||% "")
    code <- tolower(gsub("[^a-zA-Z0-9]", "", sn))

    row <- function(label, value) {
      div(style = "display:flex;justify-content:space-between;padding:5px 0;
                    border-bottom:1px solid rgba(46,196,165,.15);font-size:13px;",
          span(style = "color:#64748B;font-weight:500;", label),
          span(style = "color:#1B4F6B;font-weight:600;", value))
    }

    multi_export <- isTRUE(input$wiz_is_multi_wp) && isTRUE(input$wiz_multi_export)
    data_loc <- if (multi_export)
      "Separate export per work package"
    else if (nzchar(input$wiz_data_path %||% ""))
      input$wiz_data_path else paste0("trials/", code, "/data/")
    rr_loc <- if (nzchar(input$wiz_rr_path %||% ""))
      input$wiz_rr_path else "\u2014"
    logo_loc <- if (nzchar(input$wiz_logo_path %||% ""))
      input$wiz_logo_path else "\u2014"

    # Follow-up timepoints chosen on step 3.
    tps <- input$wiz_timepoints %||% character(0)
    tp_label <- if (length(tps)) paste(tps, collapse = " \u00b7 ") else "Baseline only"

    # Capture summary for the field-mapping step.
    caps <- c("Demographics"[isTRUE(input$wiz_cap_demographics)],
              "Procedure dates"[isTRUE(input$wiz_cap_procedure)])
    caps <- caps[!is.na(caps)]
    cap_label <- if (length(caps)) paste(caps, collapse = ", ") else "Core fields only"

    features_on <- c()
    if (isTRUE(input$wiz_feat_projections)) features_on <- c(features_on, "Projections")
    if (isTRUE(input$wiz_feat_postal))      features_on <- c(features_on, "Postal")
    if (isTRUE(input$wiz_feat_returns))     features_on <- c(features_on, "Return rates")
    if (isTRUE(input$wiz_feat_pilot))       features_on <- c(features_on, "Pilot criteria")
    if (isTRUE(input$wiz_feat_consort))     features_on <- c(features_on, "CONSORT")
    if (isTRUE(input$wiz_feat_baseline))    features_on <- c(features_on, "Baseline table")
    if (length(features_on) == 0) features_on <- "None"

    type_label <- c(
      "randomised"    = "Randomised (open label)",
      "single_blind"  = "Randomised (single blind)",
      "double_blind"  = "Randomised (double blind)",
      "observational" = "Observational",
      "single_arm"    = "Single-arm / cohort",
      "platform"      = "Platform / umbrella",
      "other"         = "Other"
    )[input$wiz_trial_type %||% "randomised"]

    # Collect the structured WP names from the form, if multi-WP is enabled.
    wp_names <- character(0)
    if (isTRUE(input$wiz_is_multi_wp)) {
      n <- suppressWarnings(as.integer(input$wiz_n_wps))
      if (!is.na(n) && n > 0) {
        wp_names <- vapply(seq_len(min(n, 10L)), function(i) {
          v <- trimws(input[[paste0("wiz_wp_name_", i)]] %||% "")
          if (nzchar(v)) sprintf("WKP%d: %s", i, v) else sprintf("WKP%d", i)
        }, character(1))
      }
    }
    wp_label <- if (length(wp_names)) paste(wp_names, collapse = " \u00b7 ") else "Single (no work packages)"

    tagList(
      row("Trial code",          code),
      row("Short name",          sn),
      row("Full name",           input$wiz_full_name %||% "\u2014"),
      row("Category",            input$wiz_category %||% "Other"),
      row("Trial type",          type_label %||% "Randomised"),
      row("Work packages",       wp_label),
      row("Target",              as.character(input$wiz_target %||% 100)),
      row("CI",                  input$wiz_ci %||% "\u2014"),
      row("REDCap data",         data_loc),
      row("Return rates folder", rr_loc),
      row("Trial logo",          logo_loc),
      row("Baseline event",      input$wiz_ev_baseline %||% "\u2014"),
      row("Follow-up timepoints", tp_label),
      row("Captures",            cap_label),
      row("Features",            paste(features_on, collapse = ", "))
    )
  })


  # ══════════════════════════════════════════════════════════════════════════
  # DELETE TRIAL
  # ══════════════════════════════════════════════════════════════════════════

  observeEvent(input$delete_trial, {
    code <- input$delete_trial
    cfg  <- rv$available_trials[[code]]
    if (is.null(cfg)) return()

    showModal(modalDialog(
      title = div(style = "display:flex;align-items:center;gap:10px;color:#DC2626;",
                  span(style = "font-size:22px;", HTML("&#x26A0;")),
                  span("Delete trial?")),
      div(style = "padding:8px 0;",
          HTML(sprintf("Are you sure you want to delete <strong>%s</strong>?",
                       cfg$short_name %||% toupper(code))),
          tags$br(), tags$br(),
          div(style = "background:#FEF2F2;border-left:3px solid #DC2626;padding:12px 14px;
                       border-radius:6px;font-size:13px;color:#7F1D1D;line-height:1.6;",
              HTML("This will permanently delete:"),
              tags$ul(style = "margin:6px 0 0 16px;",
                      tags$li("The trial config file"),
                      tags$li("Any sites, randomisation logs, and dashboard data"),
                      tags$li("Uploaded REDCap exports stored in the trial folder")),
              tags$br(),
              tags$strong("This cannot be undone.")
          )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_trial", "Yes, delete trial",
                     class = "btn btn-danger",
                     style = "background:#DC2626;border-color:#DC2626;font-weight:600;")
      ),
      easyClose = TRUE,
      size = "m"
    ))

    # Store the code being deleted
    rv$pending_delete <- code
  })

  observeEvent(input$confirm_delete_trial, {
    code <- rv$pending_delete
    if (is.null(code)) return()

    trial_dir <- file.path(getwd(), "trials", code)
    success <- tryCatch({
      unlink(trial_dir, recursive = TRUE, force = TRUE)
      TRUE
    }, error = function(e) FALSE)

    # Also remove the logo if it was copied to www/
    logo_path <- file.path(getwd(), "www", "trial_logos", paste0(code, ".jpg"))
    if (file.exists(logo_path)) file.remove(logo_path)

    removeModal()

    if (success) {
      showNotification(sprintf("Trial '%s' deleted.", code),
                       type = "message", duration = 5)
      # Refresh trial list
      rv$available_trials <- discover_trials()
    } else {
      showNotification("Could not delete trial folder. It may be in use.",
                       type = "error", duration = 8)
    }

    rv$pending_delete <- NULL
  })


  # ══════════════════════════════════════════════════════════════════════════
  # WIZARD: create the trial
  # ══════════════════════════════════════════════════════════════════════════

  observeEvent(input$wiz_create, {
    sn   <- trimws(input$wiz_short_name %||% "")
    code <- tolower(gsub("[^a-zA-Z0-9]", "", sn))

    if (!nzchar(code)) {
      showNotification("Trial short name is required.", type = "error")
      return()
    }

    trials_dir <- file.path(getwd(), "trials")
    trial_dir  <- file.path(trials_dir, code)

    if (dir.exists(trial_dir)) {
      showNotification(paste0("Folder already exists: trials/", code), type = "error")
      return()
    }

    # Create folder structure
    dir.create(file.path(trial_dir, "www"),     recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(trial_dir, "data"),    recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(trial_dir, "reports"), recursive = TRUE, showWarnings = FALSE)

    # Helper: safely quote a string for R code
    rq <- function(x) {
      x <- x %||% ""
      if (!nzchar(trimws(x))) return("NULL")
      sprintf('"%s"', gsub('"', '\\\\"', x))
    }

    # Build data_dir line
    data_dir_line <- if (nzchar(trimws(input$wiz_data_path %||% ""))) {
      sprintf('  data_dir = "%s",', gsub("\\\\", "/", input$wiz_data_path))
    } else {
      "  data_dir = NULL,    # uses trials/<code>/data/"
    }

    # Build return_rates_dir line
    rr_dir_line <- if (nzchar(trimws(input$wiz_rr_path %||% ""))) {
      sprintf('  return_rates_dir = "%s",', gsub("\\\\", "/", input$wiz_rr_path))
    } else {
      "  return_rates_dir = NULL,"
    }

    # Build logo_file line
    logo_line <- if (nzchar(trimws(input$wiz_logo_path %||% ""))) {
      sprintf('  logo_file = "%s",', gsub("\\\\", "/", input$wiz_logo_path))
    } else {
      "  logo_file = NULL,"
    }

    # Build sub_forms events
    sf_raw <- trimws(input$wiz_ev_subforms %||% "")
    sf_vec <- if (nzchar(sf_raw)) {
      parts <- trimws(strsplit(sf_raw, ",")[[1]])
      parts <- parts[nzchar(parts)]
      if (length(parts) > 0) sprintf('c(%s)', paste(sprintf('"%s"', parts), collapse = ", "))
      else "NULL"
    } else "NULL"

    # Build optional field lines, gated by the capture toggles on step 4 so a
    # non-surgical trial doesn't get operation fields, etc.
    opt_field <- function(name, input_id, enabled = TRUE) {
      val <- if (isTRUE(enabled)) trimws(input[[input_id]] %||% "") else ""
      if (nzchar(val)) sprintf('    %-28s= "%s",', name, val)
      else             sprintf('    %-28s= NULL,', name)
    }
    cap_proc <- isTRUE(input$wiz_cap_procedure)
    cap_demo <- isTRUE(input$wiz_cap_demographics)
    opt_fields_block <- paste(
      opt_field("operation_date",     "wiz_fld_op_date",        cap_proc),
      opt_field("operation_datetime", "wiz_fld_op_date",        cap_proc),
      opt_field("discharge_date",     "wiz_fld_discharge_date", cap_proc),
      opt_field("age",                "wiz_fld_age",            cap_demo),
      opt_field("sex",                "wiz_fld_sex",            cap_demo),
      opt_field("ethnicity",          "wiz_fld_ethnicity",      cap_demo),
      sep = "\n")

    # Build the redcap_events block: baseline + each chosen follow-up timepoint
    # (keyed by its slug) + sub-form events.
    ev_lines  <- sprintf("    %-10s = %s,", "baseline", rq(input$wiz_ev_baseline))
    seen_keys <- "baseline"
    for (tp in (input$wiz_timepoints %||% character(0))) {
      key <- .wiz_tp_slug(tp)
      if (!nzchar(key) || key %in% seen_keys) next
      seen_keys <- c(seen_keys, key)
      evname    <- input[[paste0("wiz_tp_evt_", key)]] %||% ""
      ev_lines  <- c(ev_lines, sprintf("    %-10s = %s,", key, rq(evname)))
    }
    ev_lines <- c(ev_lines, sprintf("    %-10s = %s", "sub_forms", sf_vec))
    events_block <- paste(ev_lines, collapse = "\n")

    # Build work_packages line from the structured wizard form. Each WP is
    # stored as a character with the literal label "WKP<n>: <name>" so
    # downstream UI can split label/name cleanly.
    wp_names_raw <- character(0)
    if (isTRUE(input$wiz_is_multi_wp)) {
      n <- suppressWarnings(as.integer(input$wiz_n_wps))
      if (!is.na(n) && n > 0) {
        wp_names_raw <- vapply(seq_len(min(n, 10L)), function(i) {
          v <- trimws(input[[paste0("wiz_wp_name_", i)]] %||% "")
          if (nzchar(v)) sprintf("WKP%d: %s", i, v) else sprintf("WKP%d", i)
        }, character(1))
      }
    }
    wp_line <- if (length(wp_names_raw) > 0) {
      sprintf('  work_packages = c(%s),',
              paste(sprintf('"%s"', gsub('"', '\\\\"', wp_names_raw)),
                    collapse = ", "))
    } else "  work_packages = NULL,"

    # Build the data-paths block. When a trial keeps a separate export per work
    # package, write work_package_data_dirs (aligned to work_packages) so the
    # loader reads, tags and combines each WP's newest CSV.
    multi_export <- isTRUE(input$wiz_is_multi_wp) &&
                    isTRUE(input$wiz_multi_export) &&
                    length(wp_names_raw) > 0
    if (multi_export) {
      wp_dirs <- vapply(seq_along(wp_names_raw), function(i)
        gsub("\\\\", "/", trimws(input[[paste0("wiz_wp_data_", i)]] %||% "")),
        character(1))
      data_block <- paste(
        "  data_dir = NULL,    # per-work-package exports below",
        sprintf("  work_package_data_dirs = c(%s),",
                paste(sprintf('"%s"', wp_dirs), collapse = ", ")),
        rr_dir_line, sep = "\n")
    } else {
      data_block <- paste(data_dir_line, rr_dir_line, sep = "\n")
    }

    trial_type_val <- input$wiz_trial_type %||% "randomised"

    config_text <- sprintf('# ===========================================================================
# Trial Configuration: %s
# ===========================================================================
# Auto-generated by the dashboard wizard on %s
# Edit this file to fine-tune settings.
# ===========================================================================

trial_config <- list(

  # -- Identity --
  code         = "%s",
  name         = %s,
  short_name   = "%s",
  trial_target = %dL,
  category     = "%s",

  # -- Design --
  trial_type   = "%s",   # randomised / single_blind / double_blind / observational / single_arm / platform / other
%s

  # -- Branding --
%s
  colors = list(
    primary   = "%s",
    secondary = "%s",
    accent    = "%s"
  ),

  # -- Data paths --
%s

  # -- REDCap events --
  redcap_events = list(
%s
  ),

  # -- REDCap field mappings --
  redcap_fields = list(
    record_id               = "%s",
    site_name               = "%s",
    randomisation_datetime  = "%s",

%s

    follow_up_instruments = list(),
    cos_type              = %s
  ),

  # -- COS type labels (standard defaults) --
  cos_type_labels = c(
    "1" = "Death", "2" = "No Operation", "3" = "Part withdrawal",
    "4" = "Complete withdrawal", "5" = "Lost to follow-up"
  ),

  ethnicity_labels       = NULL,
  white_ethnicity_codes  = NULL,
  target_schedule        = NULL,
  participant_table_layout = NULL,

  projection_defaults = list(
    rate_central = 3.0, rate_optimistic = 4.0, rate_pessimistic = 2.0,
    sites_central = 2.0, sites_optimistic = 3.0, sites_pessimistic = 1.0,
    target_sites = 24
  ),

  report_defaults = list(
    ci      = %s,
    sponsor = %s
  ),

  # -- Feature flags --
  features = list(
    postal_tracking          = %s,
    return_rates             = %s,
    projections              = %s,
    pilot_criteria           = %s,
    consort_flow             = %s,
    baseline_table           = %s,
    participant_questionnaires = %s
  )
)
',
      toupper(sn),
      format(Sys.Date(), "%d %B %Y"),
      code,
      rq(input$wiz_full_name),
      sn,
      as.integer(input$wiz_target %||% 100),
      input$wiz_category %||% "Other",
      trial_type_val,
      wp_line,
      logo_line,
      input$wiz_col_primary %||% "#1B4F6B",
      input$wiz_col_secondary %||% "#2EC4A5",
      input$wiz_col_accent %||% "#F59E0B",
      data_block,
      events_block,
      input$wiz_fld_record_id %||% "record_id",
      input$wiz_fld_site %||% "site_name",
      input$wiz_fld_rand_dt %||% "rand_dttm_s",
      opt_fields_block,
      rq(input$wiz_fld_cos_type),
      rq(input$wiz_ci),
      rq(input$wiz_sponsor),
      if (isTRUE(input$wiz_feat_postal))         "TRUE" else "FALSE",
      if (isTRUE(input$wiz_feat_returns))        "TRUE" else "FALSE",
      if (isTRUE(input$wiz_feat_projections))    "TRUE" else "FALSE",
      if (isTRUE(input$wiz_feat_pilot))          "TRUE" else "FALSE",
      if (isTRUE(input$wiz_feat_consort))        "TRUE" else "FALSE",
      if (isTRUE(input$wiz_feat_baseline))       "TRUE" else "FALSE",
      if (isTRUE(input$wiz_feat_questionnaires %||% TRUE)) "TRUE" else "FALSE"
    )

    # Write config file
    tryCatch({
      writeLines(config_text, file.path(trial_dir, "config.R"))

      # Seed the trial's reports/ folder with copies of the canonical Rmd
      # templates. Trial managers can edit them via Trial Settings → Report
      # templates without touching other trials.
      seed_trial_report_templates(
        list(trial_dir = trial_dir, code = code),
        overwrite = FALSE)

      # Copy logo to www/trial_logos/ if a path was provided
      logo_src <- trimws(input$wiz_logo_path %||% "")
      if (nzchar(logo_src) && file.exists(logo_src)) {
        logos_dir <- file.path(getwd(), "www", "trial_logos")
        dir.create(logos_dir, recursive = TRUE, showWarnings = FALSE)
        ext <- tolower(tools::file_ext(logo_src))
        if (!ext %in% c("png", "jpg", "jpeg", "svg")) ext <- "png"
        dest <- file.path(logos_dir, paste0(code, ".", ext))
        tryCatch(file.copy(logo_src, dest, overwrite = TRUE),
                 error = function(e) message("Logo copy failed: ", e$message))
      }

      close_new_trial()
      showNotification(
        HTML(sprintf("Trial <strong>%s</strong> created successfully!<br>
                      Folder: <code>trials/%s/</code><br>
                      Place your REDCap CSV exports in the data folder and select the trial to begin.",
                     sn, code)),
        type = "message", duration = 10
      )
      # Auto-grant manager membership to every admin (incl. the creator)
      all_users <- list_all_users()
      for (admin in all_users$fullname[all_users$portfolio_role == "admin"]) {
        grant_membership(admin, code, "manager")
      }
      # Refresh trial list and home cards
      rv$available_trials <- discover_trials()
      rv$home_membership_changed <- Sys.time()
      log_activity("trial_created",
                   sprintf("Created trial <strong>%s</strong>", htmltools::htmlEscape(sn)),
                   username = rv$username, trial_code = code)
    }, error = function(e) {
      showNotification(paste("Error creating trial:", e$message), type = "error", duration = 10)
    })
  })

  # ── Smart Notifications drawer ───────────────────────────────────────────
  notifications <- reactive({
    rv$home_membership_changed
    rv$settings_changed
    rv$notif_dismissed_marker
    build_notifications(discover_trials(), rv$username)
  })

  output$notif_badge_ui <- renderUI({
    n <- length(notifications())
    if (n == 0) return(NULL)
    span(class = "notif-bell-badge", n)
  })

  output$notif_drawer_ui <- renderUI({
    notes <- notifications()
    if (!length(notes)) {
      return(div(style = "padding:40px 20px;text-align:center;color:#94A3B8;",
                 div(style = "font-size:32px;margin-bottom:10px;opacity:.4;",
                     HTML("&#x2728;")),
                 div(style = "font-size:13.5px;color:#0F172A;font-weight:500;
                              margin-bottom:5px;", "All clear"),
                 div(style = "font-size:12px;",
                     "No active notifications. Anything flagged in
                      Smart Insights will show up here.")))
    }
    div(lapply(notes, render_notification_row))
  })

  observeEvent(input$notif_open, {
    shinyjs::runjs("document.body.classList.add('notif-open')")
  })
  observeEvent(input$notif_close_drawer, {
    shinyjs::runjs("document.body.classList.remove('notif-open')")
  })

  observeEvent(input$notif_dismiss, {
    key <- input$notif_dismiss
    if (is.null(key) || !nzchar(key)) return()
    dismiss_notification(rv$username, key)
    rv$notif_dismissed_marker <- Sys.time()
  })

  observeEvent(input$notif_clear_all, {
    clear_all_dismissed(rv$username)   # idempotent reset of dismissals
    # Then dismiss everything currently active so the bell drops to 0
    for (n in notifications()) dismiss_notification(rv$username, n$key)
    rv$notif_dismissed_marker <- Sys.time()
    showNotification("Notifications cleared.", type = "message", duration = 3)
  })

  # Force-render every home output — they live inside JS-toggled divs which
  # Shiny would otherwise suspend.
  .force_render(c(
    "trial_cards_ui", "home_overview_ui", "home_all_trials_ui",
    "home_sites_ui", "home_activity_ui",
    "home_add_button_ui", "home_dropdown_ui", "home_profile_name",
    "sites_top_ui", "sites_detail_ui",
    "notif_badge_ui", "notif_drawer_ui",
    "mu_user_list_ui", "mu_detail_ui"
  ))
}
