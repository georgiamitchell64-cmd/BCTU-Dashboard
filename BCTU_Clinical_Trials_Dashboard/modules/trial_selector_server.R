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
    baseline <- cfg$redcap_events$baseline %||% "baseline_arm_1"
    if ("redcap_event_name" %in% names(raw)) {
      length(unique(raw[[id_col]][raw$redcap_event_name == baseline]))
    } else {
      length(unique(raw[[id_col]]))
    }
  }

  .status_pill <- function(pct) {
    if (pct >= 0.5)      tags$span(class = "home-status on-track",
                                   tags$span(class = "dot"), "On track")
    else if (pct >= 0.25) tags$span(class = "home-status warning",
                                    tags$span(class = "dot"), "Behind")
    else                  tags$span(class = "home-status at-risk",
                                    tags$span(class = "dot"), "At risk")
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
      tags$button(class = "home-add-btn",
                  onclick = "Shiny.setInputValue('open_wizard', Math.random(), {priority:'event'})",
                  HTML("&#43; Add Trial"))
    }
  })

  # Hide tabs the user shouldn't see (admin-only ones).
  observe({
    is_admin <- isTRUE(rv$portfolio_role == "admin")
    shinyjs::runjs(sprintf(
      "document.querySelectorAll('.home-tab').forEach(function(t){
         var label = t.textContent.trim();
         if (label === 'All Trials' || label === 'Activity' || label === 'Sites') {
           t.style.display = %s ? '' : 'none';
         }
       });", if (is_admin) "true" else "false"))
  })

  # \u2500\u2500 My Trials cards (grouped by portfolio category) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  .render_trial_card <- function(r, is_tm) {
    cfg <- r$cfg
    ci      <- cfg$report_defaults$ci      %||% "\u2014"
    sponsor <- cfg$report_defaults$sponsor %||% "\u2014"
    pct_w   <- sprintf("%.0f%%", r$pct * 100)

    div(class = "home-card",
        onclick = sprintf("Shiny.setInputValue('select_trial', '%s', {priority:'event'})", r$code),

        div(class = "home-card-head",
            div(class = "home-card-title",
                cfg$short_name %||% toupper(r$code)),
            .status_pill(r$pct)
        ),
        div(class = "home-card-meta",
            HTML(sprintf("CI: %s &middot; Sponsor: %s", ci, sponsor))),

        div(class = "home-progress",
            div(class = "home-progress-fill", style = sprintf("width:%s;", pct_w))),
        div(class = "home-progress-row",
            span(tags$strong(r$n), " / ", r$target, " recruited"),
            span(pct_w))
    )
  }

  .category_header <- function(cat, n) {
    icon <- TRIAL_CATEGORY_ICONS[[cat]] %||% TRIAL_CATEGORY_ICONS[["Other"]]
    div(class = "home-category",
        div(class = "home-category-icon", HTML(icon)),
        div(class = "home-category-label", cat),
        div(class = "home-category-count",
            paste(n, if (n == 1) "trial" else "trials")),
        div(class = "home-category-divider"))
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
      div(class = "home-card home-add-card",
          onclick = "Shiny.setInputValue('open_wizard', Math.random(), {priority:'event'})",
          div(class = "plus", HTML("&#43;")),
          div(class = "label", "Add New Trial"))
    }

    if (length(rows) == 0 && is.null(add_card)) {
      return(div(class = "home-empty",
                 div(class = "icon", HTML("&#x1F4CB;")),
                 div("No trials available yet.")))
    }

    if (length(rows) == 0) {
      return(div(class = "home-grid", add_card))
    }

    cats_seen <- unique(vapply(rows, function(r) r$category, character(1)))
    blocks <- lapply(.ordered_categories(cats_seen), function(cat) {
      group <- Filter(function(r) identical(r$category, cat), rows)
      tagList(
        .category_header(cat, length(group)),
        div(class = "home-grid",
            lapply(group, function(r) .render_trial_card(r, is_tm)))
      )
    })

    # The "Add New Trial" card sits in its own footer row after all categories.
    if (!is.null(add_card)) {
      blocks <- c(blocks, list(
        div(class = "home-category",
            div(class = "home-category-icon",
                style = "background:#F8FAFD;color:#94A3B8;",
                HTML("&#43;")),
            div(class = "home-category-label",
                style = "color:var(--muted);font-weight:500;",
                "Add a new trial"),
            div(class = "home-category-divider")),
        div(class = "home-grid", add_card)
      ))
    }

    do.call(tagList, blocks)
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
    n_trials  <- length(rows)
    n_at_risk <- sum(vapply(rows, function(r) r$pct < 0.25, logical(1)))
    n_on      <- sum(vapply(rows, function(r) r$pct >= 0.5, logical(1)))
    cats_seen <- unique(vapply(rows, function(r) r$category, character(1)))

    stat <- function(label, value, sub = NULL) {
      div(class = "home-stat",
          div(class = "home-stat-label", label),
          div(class = "home-stat-value", value),
          if (!is.null(sub)) div(class = "home-stat-sub", sub))
    }

    # Per-trial recruitment status (each trial standalone, not summed)
    trial_rows <- lapply(rows, function(r) {
      pct_w <- sprintf("%.0f%%", r$pct * 100)
      cfg <- r$cfg
      div(style = "display:grid;grid-template-columns:1.5fr 1fr 0.6fr 0.4fr;
                   gap:14px;align-items:center;
                   padding:14px 0;border-bottom:1px solid #EEF2F7;",
          # Trial name + category
          div(div(style = "font-weight:600;color:#0F172A;font-size:14px;",
                  cfg$short_name %||% toupper(r$code)),
              div(style = "font-size:11px;color:#64748B;", r$category)),
          # Mini progress bar
          div(class = "home-progress", style = "margin:0;width:100%;",
              div(class = "home-progress-fill", style = sprintf("width:%s;", pct_w))),
          # Counts
          div(style = "font-size:13px;color:#475569;text-align:right;",
              sprintf("%d / %d", r$n, r$target)),
          # Status pill
          div(style = "text-align:right;", .status_pill(r$pct))
      )
    })

    # Per-category trial counts (no recruitment summing)
    cat_rows <- lapply(.ordered_categories(cats_seen), function(cat) {
      group   <- Filter(function(r) identical(r$category, cat), rows)
      icon    <- TRIAL_CATEGORY_ICONS[[cat]] %||% TRIAL_CATEGORY_ICONS[["Other"]]
      div(style = "display:grid;grid-template-columns:auto 1fr auto;
                   gap:14px;align-items:center;
                   padding:12px 0;border-bottom:1px solid #EEF2F7;",
          div(class = "home-category-icon", HTML(icon)),
          div(style = "font-weight:600;color:#0F172A;font-size:13.5px;", cat),
          div(style = "font-size:13px;color:var(--accent);font-weight:600;",
              paste(length(group),
                    if (length(group) == 1) "trial" else "trials")))
    })

    tagList(
      smart_section,

      div(class = "home-stat-grid", style = "margin-top:18px;",
          stat("Trials in portfolio", n_trials,
               if (n_trials == 1) "live trial" else "live trials"),
          stat("On track", n_on, "at 50%+ of target"),
          stat("At risk", n_at_risk, "below 25% of target"),
          stat("Categories", length(cats_seen),
               if (length(cats_seen) == 1) "category" else "distinct categories")
      ),

      div(style = "display:grid;grid-template-columns:2fr 1fr;gap:18px;margin-top:8px;",
          div(style = "background:#FFFFFF;border:1px solid #EEF2F7;border-radius:14px;
                       padding:8px 24px 4px;",
              div(style = "font-size:11px;font-weight:600;color:var(--muted);
                           text-transform:uppercase;letter-spacing:.6px;
                           padding:14px 0 4px;",
                  "Per-trial progress"),
              div(trial_rows)),
          div(style = "background:#FFFFFF;border:1px solid #EEF2F7;border-radius:14px;
                       padding:8px 24px 4px;",
              div(style = "font-size:11px;font-weight:600;color:var(--muted);
                           text-transform:uppercase;letter-spacing:.6px;
                           padding:14px 0 4px;",
                  "By category"),
              div(cat_rows)))
    )
  })

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
  output$home_all_trials_ui <- renderUI({
    rows <- tryCatch(all_trials_data(), error = function(e) {
      message("all_trials rows err: ", e$message)
      list()
    })
    if (length(rows) == 0) {
      return(div(class = "home-empty",
                 div(class = "icon", HTML("&#x1F4CB;")),
                 div("No trials yet.")))
    }

    cats_seen <- unique(vapply(rows, function(r) r$category, character(1)))
    blocks <- lapply(.ordered_categories(cats_seen), function(cat) {
      group <- Filter(function(r) identical(r$category, cat), rows)
      body <- lapply(group, function(r) {
        cfg <- r$cfg
        tags$tr(class = "clickable",
                onclick = sprintf("Shiny.setInputValue('select_trial','%s',{priority:'event'})", r$code),
                tags$td(tags$strong(cfg$short_name %||% toupper(r$code))),
                tags$td(cfg$report_defaults$ci      %||% "\u2014"),
                tags$td(cfg$report_defaults$sponsor %||% "\u2014"),
                tags$td(sprintf("%d / %d", r$n, r$target)),
                tags$td(.status_pill(r$pct)))
      })
      tagList(
        .category_header(cat, length(group)),
        tags$table(class = "home-table",
          tags$thead(tags$tr(
            tags$th("Trial"), tags$th("CI"), tags$th("Sponsor"),
            tags$th("Recruitment"), tags$th("Status"))),
          tags$tbody(body))
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
    showModal(manage_users_modal())
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

  output$manage_users_table_ui <- renderUI({
    rv$home_membership_changed   # refresh trigger
    users <- list_all_users()
    trials <- discover_trials()

    if (nrow(users) == 0) {
      return(div(class = "home-empty", "No users yet."))
    }

    rows <- lapply(seq_len(nrow(users)), function(i) {
      u <- users[i, ]
      mems <- list_user_memberships_for(u$fullname)
      mem_pills <- if (nrow(mems) == 0) {
        span(style = "font-size:11.5px;color:#94A3B8;font-style:italic;", "No trials")
      } else {
        lapply(seq_len(nrow(mems)), function(j) {
          m <- mems[j, ]
          tcode <- m$trial_code
          tname <- trials[[tcode]]$short_name %||% toupper(tcode)
          span(style = "display:inline-flex;align-items:center;gap:4px;
                        background:#F1F5F9;color:#0F172A;
                        padding:2px 8px;border-radius:999px;font-size:11px;
                        margin-right:5px;margin-bottom:4px;",
               tname,
               span(style = "color:#64748B;font-size:10px;",
                    sprintf("(%s)", m$trial_role)))
        })
      }

      div(style = "display:grid;grid-template-columns:1.4fr 0.8fr 2.4fr auto;
                   gap:14px;align-items:center;padding:12px 0;
                   border-bottom:1px solid #EEF2F7;",
          div(div(style = "font-weight:600;color:#0F172A;", u$fullname),
              div(style = "font-size:11px;color:#64748B;", u$role)),
          tags$select(id = paste0("portrole_", i),
            class = "form-control",
            style = "font-size:12px;padding:5px 8px;height:32px;",
            onchange = sprintf("Shiny.setInputValue('home_set_portrole',
                                {user:'%s', role:this.value, n:Math.random()},
                                {priority:'event'})",
                               gsub("'", "\\\\'", u$fullname)),
            tags$option(value = "member",
                        selected = if (u$portfolio_role == "member") NA else NULL,
                        "Member"),
            tags$option(value = "admin",
                        selected = if (u$portfolio_role == "admin") NA else NULL,
                        "Admin")),
          div(mem_pills),
          actionButton(paste0("edit_mem_", i),
                       label = HTML("&#9881; Edit"),
                       class = "btn btn-sm btn-outline-secondary",
                       onclick = sprintf("Shiny.setInputValue('home_edit_user',
                                          {user:'%s', n:Math.random()},
                                          {priority:'event'})",
                                         gsub("'", "\\\\'", u$fullname)))
      )
    })

    div(rows)
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

  observeEvent(input$home_edit_user, {
    u <- input$home_edit_user$user
    showModal(edit_memberships_modal(u))
  })

  output$edit_memberships_body <- renderUI({
    u <- isolate(input$home_edit_user$user)
    if (is.null(u)) return(NULL)
    rv$home_membership_changed
    trials <- discover_trials()
    mems <- list_user_memberships_for(u)
    mem_lookup <- setNames(mems$trial_role, mems$trial_code)

    rows <- lapply(names(trials), function(tcode) {
      cfg <- trials[[tcode]]
      current <- mem_lookup[[tcode]] %||% "none"
      div(style = "display:grid;grid-template-columns:1fr 1fr;gap:14px;
                   align-items:center;padding:10px 0;
                   border-bottom:1px solid #EEF2F7;",
          div(tags$strong(cfg$short_name %||% toupper(tcode)),
              tags$br(),
              span(style = "font-size:11px;color:#64748B;", cfg$name %||% "")),
          tags$select(
            class = "form-control",
            style = "font-size:13px;padding:6px 10px;",
            onchange = sprintf("Shiny.setInputValue('home_set_mem',
                                {user:'%s', trial:'%s', role:this.value, n:Math.random()},
                                {priority:'event'})",
                               gsub("'", "\\\\'", u), tcode),
            tags$option(value = "none",
                        selected = if (current == "none") NA else NULL,
                        "No access"),
            tags$option(value = "manager",
                        selected = if (current == "manager") NA else NULL,
                        "Manager"),
            tags$option(value = "coordinator",
                        selected = if (current == "coordinator") NA else NULL,
                        "Coordinator"),
            tags$option(value = "statistician",
                        selected = if (current == "statistician") NA else NULL,
                        "Statistician"),
            tags$option(value = "readonly",
                        selected = if (current == "readonly") NA else NULL,
                        "Read-only"))
      )
    })
    tagList(
      div(style = "font-size:13px;color:#64748B;margin-bottom:12px;",
          sprintf("Set per-trial access for %s", u)),
      div(rows)
    )
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
    apply_trial_role_visibility(role_here)
    if (!dir.exists(dirname(DB_PATH))) dir.create(dirname(DB_PATH), recursive = TRUE)
    db_init()

    rv$sites <- db_load_sites()
    rv$log   <- db_load_log()

    trial_name <- cfg$short_name %||% toupper(code)
    runjs(sprintf("$('.topbar-title').text('%s Site Tracker')", trial_name))
    runjs(sprintf("document.title = '%s Dashboard'", trial_name))

    shinyjs::hide("trial_selector_panel")
    shinyjs::show("dashboard_panel")
    shinyjs::show("sidebar_nav_section")    # Show the sidebar nav
    shinyjs::show("topbar_wrap")            # Show the topbar
    shinyjs::runjs("document.body.classList.remove('home-mode')")

    rv$trigger_data_load <- Sys.time()

    # Apply feature flags — show/hide tabs based on config
    feat <- cfg$features %||% list()
    if (isTRUE(feat$postal_tracking)) shinyjs::show("go_postal_wrap") else shinyjs::hide("go_postal_wrap")
    if (isTRUE(feat$return_rates))    shinyjs::show("go_returns_wrap") else shinyjs::hide("go_returns_wrap")

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

  wiz_step <- reactiveVal(1L)
  WIZ_TOTAL <- 6L

  # Open wizard modal
  observeEvent(input$open_wizard, {
    wiz_step(1L)
    showModal(new_trial_wizard_ui())
    shinyjs::hide("wiz_prev")
    shinyjs::hide("wiz_create")
    shinyjs::show("wiz_next")
  })

  # Step indicator
  output$wiz_step_indicator <- renderUI({
    step <- wiz_step()
    dots <- lapply(seq_len(WIZ_TOTAL), function(i) {
      active <- if (i == step) "background:#2EC4A5;" else "background:#DDE5EE;"
      span(style = paste0("width:10px;height:10px;border-radius:50%;display:inline-block;
                            margin:0 3px;transition:background .2s;", active))
    })
    div(style = "text-align:center;margin-bottom:18px;", dots)
  })

  # Next step
  observeEvent(input$wiz_next, {
    step <- wiz_step()

    # Validate step 1
    if (step == 1L) {
      sn <- trimws(input$wiz_short_name %||% "")
      if (!nzchar(sn)) {
        showNotification("Please enter a short name for the trial.", type = "warning")
        return()
      }
      # Check if trial code already exists
      code <- tolower(gsub("[^a-zA-Z0-9]", "", sn))
      if (code %in% names(rv$available_trials)) {
        showNotification(paste0("A trial with code '", code, "' already exists."), type = "warning")
        return()
      }
    }

    if (step < WIZ_TOTAL) {
      shinyjs::hide(paste0("wiz_step_", step))
      wiz_step(step + 1L)
      shinyjs::show(paste0("wiz_step_", step + 1L))

      # Button visibility
      shinyjs::show("wiz_prev")
      if (step + 1L == WIZ_TOTAL) {
        shinyjs::hide("wiz_next")
        shinyjs::show("wiz_create")
      }
    }
  })

  # Previous step
  observeEvent(input$wiz_prev, {
    step <- wiz_step()
    if (step > 1L) {
      shinyjs::hide(paste0("wiz_step_", step))
      wiz_step(step - 1L)
      shinyjs::show(paste0("wiz_step_", step - 1L))

      shinyjs::show("wiz_next")
      shinyjs::hide("wiz_create")
      if (step - 1L == 1L) shinyjs::hide("wiz_prev")
    }
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

    data_loc <- if (input$wiz_data_source == "network" && nzchar(input$wiz_data_path %||% ""))
      input$wiz_data_path else paste0("trials/", code, "/data/")

    features_on <- c()
    if (isTRUE(input$wiz_feat_projections)) features_on <- c(features_on, "Projections")
    if (isTRUE(input$wiz_feat_postal))      features_on <- c(features_on, "Postal")
    if (isTRUE(input$wiz_feat_returns))     features_on <- c(features_on, "Return rates")
    if (isTRUE(input$wiz_feat_pilot))       features_on <- c(features_on, "Pilot criteria")
    if (isTRUE(input$wiz_feat_consort))     features_on <- c(features_on, "CONSORT")
    if (isTRUE(input$wiz_feat_baseline))    features_on <- c(features_on, "Baseline table")
    if (length(features_on) == 0) features_on <- "None"

    tagList(
      row("Trial code",          code),
      row("Short name",          sn),
      row("Full name",           input$wiz_full_name %||% "\u2014"),
      row("Category",            input$wiz_category %||% "Other"),
      row("Target",              as.character(input$wiz_target %||% 100)),
      row("CI",                  input$wiz_ci %||% "\u2014"),
      row("Data location",       data_loc),
      row("Baseline event",      input$wiz_ev_baseline %||% "\u2014"),
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
    data_dir_line <- if (input$wiz_data_source == "network" && nzchar(input$wiz_data_path %||% "")) {
      sprintf('  data_dir = "%s",', gsub("\\\\", "/", input$wiz_data_path))
    } else {
      "  data_dir = NULL,    # uses trials/<code>/data/"
    }

    # Build sub_forms events
    sf_raw <- trimws(input$wiz_ev_subforms %||% "")
    sf_vec <- if (nzchar(sf_raw)) {
      parts <- trimws(strsplit(sf_raw, ",")[[1]])
      parts <- parts[nzchar(parts)]
      if (length(parts) > 0) sprintf('c(%s)', paste(sprintf('"%s"', parts), collapse = ", "))
      else "NULL"
    } else "NULL"

    # Build optional field lines
    opt_field <- function(name, input_id) {
      val <- trimws(input[[input_id]] %||% "")
      if (nzchar(val)) sprintf('    %-28s= "%s",', name, val) else sprintf('    %-28s= NULL,', name)
    }

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

  # -- Branding --
  logo_file = NULL,
  colors = list(
    primary   = "%s",
    secondary = "%s",
    accent    = "%s"
  ),

  # -- Data source --
%s

  # -- REDCap events --
  redcap_events = list(
    baseline  = %s,
    discharge = %s,
    day_30    = %s,
    day_90    = %s,
    sub_forms = %s
  ),

  # -- REDCap field mappings --
  redcap_fields = list(
    record_id               = "%s",
    site_name               = "%s",
    randomisation_datetime  = "%s",

%s
%s
%s
%s
%s
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
    postal_tracking  = %s,
    return_rates     = %s,
    projections      = %s,
    pilot_criteria   = %s,
    consort_flow     = %s,
    baseline_table   = %s
  )
)
',
      toupper(sn),
      format(Sys.Date(), "%%d %%B %%Y"),
      code,
      rq(input$wiz_full_name),
      sn,
      as.integer(input$wiz_target %||% 100),
      input$wiz_category %||% "Other",
      input$wiz_col_primary %||% "#1B4F6B",
      input$wiz_col_secondary %||% "#2EC4A5",
      input$wiz_col_accent %||% "#F59E0B",
      data_dir_line,
      rq(input$wiz_ev_baseline),
      rq(input$wiz_ev_discharge),
      rq(input$wiz_ev_day30),
      rq(input$wiz_ev_day90),
      sf_vec,
      input$wiz_fld_record_id %||% "record_id",
      input$wiz_fld_site %||% "site_name",
      input$wiz_fld_rand_dt %||% "rand_dttm_s",
      opt_field("operation_date",   "wiz_fld_op_date"),
      opt_field("operation_datetime", "wiz_fld_op_date"),  # same field, both formats
      opt_field("discharge_date",   "wiz_fld_discharge_date"),
      opt_field("age",              "wiz_fld_age"),
      opt_field("sex",              "wiz_fld_sex"),
      opt_field("ethnicity",        "wiz_fld_ethnicity"),
      rq(input$wiz_fld_cos_type),
      rq(input$wiz_ci),
      rq(input$wiz_sponsor),
      if (isTRUE(input$wiz_feat_postal))      "TRUE" else "FALSE",
      if (isTRUE(input$wiz_feat_returns))     "TRUE" else "FALSE",
      if (isTRUE(input$wiz_feat_projections)) "TRUE" else "FALSE",
      if (isTRUE(input$wiz_feat_pilot))       "TRUE" else "FALSE",
      if (isTRUE(input$wiz_feat_consort))     "TRUE" else "FALSE",
      if (isTRUE(input$wiz_feat_baseline))    "TRUE" else "FALSE"
    )

    # Write config file
    tryCatch({
      writeLines(config_text, file.path(trial_dir, "config.R"))
      removeModal()
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
    "notif_badge_ui", "notif_drawer_ui"
  ))
}
