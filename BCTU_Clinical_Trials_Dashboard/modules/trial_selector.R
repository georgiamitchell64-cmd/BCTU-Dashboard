trial_selector_ui <- function() {
  div(id = "trial_selector_panel", class = "home-root",

      # ── Global rules: collapse sidebar / page padding while on home ──────
      tags$style(HTML("
        /* Hide every form of bslib sidebar */
        body.home-mode #main_sidebar,
        body.home-mode .bslib-sidebar-layout > aside,
        body.home-mode .bslib-sidebar-layout > .sidebar,
        body.home-mode aside.sidebar { display:none !important; }

        /* Collapse the grid so the main column fills the viewport */
        body.home-mode .bslib-sidebar-layout {
          display: block !important;
          grid-template-columns: 0 !important;
          --_sidebar-width: 0 !important;
        }
        body.home-mode .bslib-sidebar-layout > .main,
        body.home-mode .bslib-sidebar-layout > main,
        body.home-mode bslib-sidebar-layout > .main {
          width: 100% !important;
          max-width: 100% !important;
          padding: 0 !important;
          margin: 0 !important;
          grid-column: 1 / -1 !important;
        }

        /* Kill the page-padding wrapper from build_app_ui */
        body.home-mode #app_main_wrap { padding: 0 !important; }
        body.home-mode .home-root,
        body.home-mode .home-root + * { width: 100%; }
      ")),

      # ── Home-screen styles (scoped via .home-root) ───────────────────────
      tags$style(HTML("
        .home-root {
          --bg:       #FAFBFD;
          --surface:  #FFFFFF;
          --border:   #EEF2F7;
          --text:     #0F172A;
          --muted:    #64748B;
          --faint:    #94A3B8;
          --accent:   #6366F1;
          --accent-2: #8B5CF6;
          --mint:     #10B981;
          --amber:    #F59E0B;
          --rose:     #F43F5E;
          background: var(--bg);
          width: 100%;
          min-height: 100vh;
          margin: 0;
          padding: 0;
          color: var(--text);
          position: relative;
          z-index: 1;
        }
        .home-topbar {
          display:flex; justify-content:space-between; align-items:center;
          padding:18px 32px; background:var(--surface);
          border-bottom:1px solid var(--border);
        }
        .home-brand {
          display:flex; align-items:center; gap:10px;
          font-size:15px; font-weight:600; letter-spacing:-0.2px;
        }
        .home-brand .brand-mark {
          width:30px; height:30px; border-radius:9px;
          background: linear-gradient(135deg, var(--accent), var(--accent-2));
          display:flex; align-items:center; justify-content:center; color:white;
          font-size:14px; font-weight:700;
        }
        .home-profile {
          position:relative; cursor:pointer; padding:8px 12px;
          border-radius:9px; transition:background .15s;
          font-size:14px; color:var(--text);
        }
        .home-profile:hover { background:#F1F5F9; }
        .home-profile .caret { color:var(--faint); margin-left:4px; }
        .home-profile .home-dropdown {
          display:none; position:absolute; right:0; top:42px;
          background:var(--surface); border:1px solid var(--border);
          border-radius:12px; padding:6px; width:200px;
          box-shadow: 0 10px 30px rgba(15,23,42,0.08);
          z-index:50;
        }
        .home-profile.open .home-dropdown { display:block; }
        .home-dropdown-item {
          padding:9px 12px; font-size:13px; border-radius:7px;
          cursor:pointer; color:var(--text);
        }
        .home-dropdown-item:hover { background:#F1F5F9; }
        .home-dropdown-divider {
          height:1px; background:var(--border); margin:4px 0;
        }

        .home-container { padding: 28px 32px 60px; max-width:1280px; margin:0 auto; }

        .home-tabs {
          display:flex; gap:6px; margin-bottom:24px;
          border-bottom:1px solid var(--border);
        }
        .home-tab {
          cursor:pointer; padding:10px 14px;
          font-size:13px; font-weight:500; color:var(--muted);
          border-bottom:2px solid transparent; margin-bottom:-1px;
          transition:color .15s, border-color .15s;
        }
        .home-tab:hover { color:var(--text); }
        .home-tab.active {
          color:var(--accent); border-bottom-color:var(--accent);
          font-weight:600;
        }

        .home-section-head {
          display:flex; justify-content:space-between; align-items:center;
          margin-bottom:18px;
        }
        .home-section-head h2 {
          font-size:22px; font-weight:600; letter-spacing:-0.4px; margin:0;
        }
        .home-section-head .subtitle {
          font-size:13px; color:var(--muted); margin-top:3px;
        }

        .home-add-btn {
          background:var(--accent); color:white; border:none;
          padding:10px 16px; border-radius:10px; font-size:13px;
          font-weight:500; cursor:pointer; transition:background .15s;
          font-family:inherit;
        }
        .home-add-btn:hover { background:#4F46E5; }

        .home-grid {
          display:grid;
          grid-template-columns:repeat(auto-fill, minmax(280px, 1fr));
          gap:16px;
        }

        .home-card {
          background:var(--surface); border:1px solid var(--border);
          border-radius:14px; padding:20px;
          box-shadow: 0 1px 2px rgba(15,23,42,0.03);
          cursor:pointer; transition: transform .15s, box-shadow .15s, border-color .15s;
          position:relative;
        }
        .home-card:hover {
          transform: translateY(-2px);
          box-shadow: 0 8px 24px rgba(99,102,241,0.10);
          border-color: #DCD6FE;
        }
        .home-card-head {
          display:flex; justify-content:space-between; align-items:flex-start;
          margin-bottom:12px;
        }
        .home-card-title {
          font-size:16px; font-weight:600; letter-spacing:-0.2px;
        }
        .home-card-meta {
          font-size:11.5px; color:var(--muted); line-height:1.55;
          margin-bottom:14px;
        }
        .home-progress {
          height:6px; background:#F1F5F9; border-radius:999px;
          overflow:hidden; margin:10px 0 8px;
        }
        .home-progress-fill {
          height:100%; border-radius:999px;
          background: linear-gradient(90deg, var(--accent), var(--accent-2));
          transition: width .4s ease;
        }
        .home-progress-row {
          display:flex; justify-content:space-between; align-items:baseline;
          font-size:11.5px; color:var(--muted);
        }
        .home-progress-row strong { color:var(--text); font-weight:600; }

        .home-status {
          display:inline-flex; align-items:center; gap:5px;
          font-size:10.5px; font-weight:600; padding:3px 8px;
          border-radius:999px; text-transform:uppercase; letter-spacing:.4px;
        }
        .home-status.on-track { background:#ECFDF5; color:var(--mint); }
        .home-status.warning  { background:#FEF3C7; color:#92400E; }
        .home-status.at-risk  { background:#FFE4E6; color:#9F1239; }
        .home-status .dot {
          width:6px; height:6px; border-radius:50%;
          background:currentColor;
        }

        .home-add-card {
          background:transparent; border:2px dashed #DCD6FE;
          display:flex; flex-direction:column; align-items:center;
          justify-content:center; min-height:170px;
          color:var(--accent); cursor:pointer;
          transition: background .15s, border-color .15s;
        }
        .home-add-card:hover {
          background:#F5F3FF; border-color:var(--accent);
        }
        .home-add-card .plus {
          font-size:30px; font-weight:300; line-height:1; margin-bottom:6px;
        }
        .home-add-card .label { font-size:13px; font-weight:500; }

        .home-card-delete {
          position:absolute; top:12px; right:12px;
          width:26px; height:26px; border-radius:50%;
          border:none; background:transparent;
          color:var(--faint); font-size:16px; cursor:pointer;
          display:flex; align-items:center; justify-content:center;
          transition: background .15s, color .15s;
        }
        .home-card-delete:hover { background:#FFE4E6; color:var(--rose); }

        .home-stat-grid {
          display:grid; grid-template-columns:repeat(auto-fit, minmax(220px, 1fr));
          gap:14px; margin-bottom:24px;
        }
        .home-stat {
          background:var(--surface); border:1px solid var(--border);
          border-radius:14px; padding:18px 20px;
        }
        .home-stat-label {
          font-size:11px; font-weight:600; color:var(--muted);
          text-transform:uppercase; letter-spacing:.6px; margin-bottom:8px;
        }
        .home-stat-value {
          font-size:28px; font-weight:700; letter-spacing:-0.6px;
          color:var(--text);
        }
        .home-stat-sub {
          font-size:12px; color:var(--muted); margin-top:4px;
        }

        .home-table {
          width:100%; border-collapse:collapse; background:var(--surface);
          border:1px solid var(--border); border-radius:14px; overflow:hidden;
        }
        .home-table th {
          text-align:left; padding:14px 18px; font-size:11px;
          font-weight:600; color:var(--muted);
          text-transform:uppercase; letter-spacing:.6px;
          background:#FAFBFD; border-bottom:1px solid var(--border);
        }
        .home-table td {
          padding:14px 18px; font-size:13.5px;
          border-bottom:1px solid var(--border);
        }
        .home-table tr:last-child td { border-bottom:none; }
        .home-table tr.clickable { cursor:pointer; transition:background .12s; }
        .home-table tr.clickable:hover { background:#F8FAFD; }

        .home-category {
          margin-top: 26px; margin-bottom: 14px;
          display:flex; align-items:center; gap:10px;
        }
        .home-category:first-of-type { margin-top: 0; }
        .home-category-icon {
          width: 30px; height: 30px; border-radius: 9px;
          background: #F5F3FF; color: var(--accent);
          display:flex; align-items:center; justify-content:center;
          font-size: 14px;
        }
        .home-category-label {
          font-size: 14px; font-weight: 600; color: var(--text);
          letter-spacing:-0.1px;
        }
        .home-category-count {
          font-size: 11px; color: var(--muted);
          background: #F1F5F9; padding: 2px 9px; border-radius: 999px;
          font-weight: 500;
        }
        .home-category-divider {
          flex: 1; height: 1px; background: var(--border);
          margin-left: 6px;
        }

        .home-empty {
          text-align:center; padding:60px 20px; color:var(--muted);
          font-size:14px;
        }
        .home-empty .icon {
          font-size:32px; margin-bottom:12px; opacity:.4;
        }
      ")),

      # ── JS for tab switching + dropdown toggle ───────────────────────────
      tags$script(HTML("
        document.addEventListener('click', function(e){
          var prof = document.querySelector('.home-profile');
          if (prof && prof.contains(e.target)) {
            prof.classList.toggle('open');
          } else if (prof) {
            prof.classList.remove('open');
          }
        });
        function homeShowTab(tab, el) {
          ['my','overview','all','sites','activity'].forEach(function(t){
            var n = document.getElementById('home_tab_'+t);
            if (n) n.style.display = (t===tab) ? '' : 'none';
          });
          document.querySelectorAll('.home-tab').forEach(function(t){
            t.classList.remove('active');
          });
          if (el) el.classList.add('active');
          Shiny.setInputValue('home_active_tab', tab, {priority:'event'});
        }
      ")),

      # ── Topbar ───────────────────────────────────────────────────────────
      div(class = "home-topbar",
          div(class = "home-brand",
              span(class = "brand-mark", "B"),
              span("BCTU Trials")),
          div(style = "display:flex;align-items:center;gap:6px;",
              # Bell icon + count badge
              div(class = "home-bell",
                  onclick = "Shiny.setInputValue('notif_open', Math.random(), {priority:'event'})",
                  style = "position:relative;cursor:pointer;width:36px;height:36px;
                           border-radius:9px;display:flex;align-items:center;justify-content:center;
                           color:#475569;transition:background .15s;",
                  onmouseover = "this.style.background='#F1F5F9';",
                  onmouseout  = "this.style.background='transparent';",
                  HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="none"
                        stroke="currentColor" stroke-width="2" stroke-linecap="round"
                        stroke-linejoin="round"><path d="M6 8a6 6 0 0 1 12 0c0 7 3 9 3 9H3s3-2 3-9"/>
                        <path d="M10.3 21a1.94 1.94 0 0 0 3.4 0"/></svg>'),
                  uiOutput("notif_badge_ui", inline = TRUE)
              ),
              div(class = "home-profile",
                  textOutput("home_profile_name", inline = TRUE),
                  span(class = "caret", HTML("&#9662;")),
                  uiOutput("home_dropdown_ui", inline = FALSE)
              )
          )
      ),

      # Notification drawer (off-canvas, slides from right)
      tags$style(HTML("
        .notif-drawer-bg {
          position:fixed; inset:0; background:rgba(15,23,42,0.35);
          z-index:1080; opacity:0; pointer-events:none;
          transition:opacity .2s;
        }
        .notif-drawer {
          position:fixed; top:0; right:0; bottom:0; width:380px;
          background:#FFFFFF; box-shadow:-12px 0 30px rgba(15,23,42,0.12);
          z-index:1090; transform:translateX(100%);
          transition:transform .25s ease;
          display:flex; flex-direction:column;
        }
        body.notif-open .notif-drawer-bg { opacity:1; pointer-events:auto; }
        body.notif-open .notif-drawer { transform:translateX(0); }
        .notif-drawer-head {
          padding:18px 20px; border-bottom:1px solid #EEF2F7;
          display:flex; justify-content:space-between; align-items:center;
        }
        .notif-drawer-body { flex:1; overflow-y:auto; padding:14px 16px; }
        .notif-bell-badge {
          position:absolute; top:4px; right:4px; min-width:16px; height:16px;
          padding:0 4px; border-radius:999px; background:#F43F5E; color:#FFF;
          font-size:9.5px; font-weight:700; display:flex; align-items:center;
          justify-content:center; line-height:1;
        }
      ")),
      div(class = "notif-drawer-bg",
          onclick = "document.body.classList.remove('notif-open')"),
      div(class = "notif-drawer",
          div(class = "notif-drawer-head",
              div(div(style = "font-weight:600;color:#0F172A;font-size:15px;",
                      "Smart Notifications"),
                  div(style = "font-size:11.5px;color:#64748B;",
                      "Auto-generated from trial data")),
              div(
                  actionLink("notif_clear_all", "Clear all",
                             style = "font-size:11.5px;color:#64748B;
                                      text-decoration:none;margin-right:14px;"),
                  tags$button(onclick = "document.body.classList.remove('notif-open')",
                              style = "background:transparent;border:none;color:#94A3B8;
                                       font-size:18px;cursor:pointer;line-height:1;",
                              HTML("&times;")))
          ),
          div(class = "notif-drawer-body",
              uiOutput("notif_drawer_ui"))),

      # ── Container with tabs ──────────────────────────────────────────────
      div(class = "home-container",

          div(class = "home-tabs",
              div(class = "home-tab active",
                  onclick = "homeShowTab('my', this)", "My Trials"),
              div(class = "home-tab",
                  onclick = "homeShowTab('overview', this)", "Overview"),
              div(class = "home-tab",
                  onclick = "homeShowTab('all', this)", "All Trials"),
              div(class = "home-tab",
                  onclick = "homeShowTab('sites', this)", "Sites"),
              div(class = "home-tab",
                  onclick = "homeShowTab('activity', this)", "Activity")
          ),

          # My Trials
          div(id = "home_tab_my",
              div(class = "home-section-head",
                  div(tags$h2("My Trials"),
                      div(class = "subtitle",
                          "Trials you're currently working on")),
                  uiOutput("home_add_button_ui", inline = TRUE)
              ),
              uiOutput("trial_cards_ui")
          ),

          # Overview
          div(id = "home_tab_overview", style = "display:none;",
              div(class = "home-section-head",
                  div(tags$h2("Portfolio Overview"),
                      div(class = "subtitle",
                          "Aggregate recruitment and site activity across all trials"))
              ),
              uiOutput("home_overview_ui")
          ),

          # All Trials
          div(id = "home_tab_all", style = "display:none;",
              div(class = "home-section-head",
                  div(tags$h2("All Trials"),
                      div(class = "subtitle",
                          "Every trial in the BCTU portfolio"))
              ),
              uiOutput("home_all_trials_ui")
          ),

          # Sites
          div(id = "home_tab_sites", style = "display:none;",
              div(class = "home-section-head",
                  div(tags$h2("Site performance"),
                      div(class = "subtitle",
                          "Top sites by category, plus look-up for any specific site"))
              ),
              uiOutput("home_sites_ui")
          ),

          # Activity
          div(id = "home_tab_activity", style = "display:none;",
              div(class = "home-section-head",
                  div(tags$h2("Activity"),
                      div(class = "subtitle",
                          "Recent randomisations, site changes, and data uploads"))
              ),
              uiOutput("home_activity_ui")
          )
      )
  )
}


# ── Smart Insights drill-down modals ────────────────────────────────────────
.insight_modal_header <- function(emoji, title, subtitle = NULL) {
  div(style = "display:flex;align-items:center;gap:12px;",
      div(style = "width:38px;height:38px;border-radius:10px;background:#F5F3FF;
                   color:#6366F1;display:flex;align-items:center;justify-content:center;
                   font-size:18px;",
          HTML(emoji)),
      div(div(style = "font-weight:600;color:#0F172A;font-size:16px;", title),
          if (!is.null(subtitle))
            div(style = "font-size:11.5px;color:#64748B;", subtitle))
  )
}

.insight_category_modal <- function(category, summary) {
  if (is.null(summary)) {
    return(modalDialog(title = paste(category, "— no data"), easyClose = TRUE,
                       footer = modalButton("Close"),
                       div(style = "padding:14px;color:#64748B;",
                           "No trials in this category yet.")))
  }
  modalDialog(
    size = "l", easyClose = TRUE, footer = modalButton("Close"),
    title = .insight_modal_header("&#x1F4CA;",
              paste(category, "portfolio overview"),
              sprintf("%d %s",
                      summary$n_trials,
                      if (summary$n_trials == 1) "trial" else "trials")),

    # Smart summary paragraph
    div(style = "background:#F8FAFD;border:1px solid #EEF2F7;border-radius:10px;
                 padding:14px 18px;font-size:13px;color:#0F172A;line-height:1.7;
                 margin-bottom:16px;",
        summary$summary),

    # Stats row
    div(style = "display:grid;grid-template-columns:repeat(3, 1fr);gap:12px;
                 margin-bottom:18px;",
        div(style = "background:#FFFFFF;border:1px solid #EEF2F7;border-radius:10px;
                     padding:12px 14px;",
            div(style = "font-size:10px;font-weight:600;color:#64748B;
                         text-transform:uppercase;letter-spacing:.5px;",
                "Active sites"),
            div(style = "font-size:22px;font-weight:700;color:#0F172A;",
                summary$n_sites)),
        div(style = "background:#FFFBEB;border:1px solid #FDE68A;border-radius:10px;
                     padding:12px 14px;",
            div(style = "font-size:10px;font-weight:600;color:#92400E;
                         text-transform:uppercase;letter-spacing:.5px;",
                "Warnings"),
            div(style = "font-size:22px;font-weight:700;color:#78350F;",
                summary$n_warnings)),
        div(style = "background:#FEF2F2;border:1px solid #FECACA;border-radius:10px;
                     padding:12px 14px;",
            div(style = "font-size:10px;font-weight:600;color:#B91C1C;
                         text-transform:uppercase;letter-spacing:.5px;",
                "Alerts"),
            div(style = "font-size:22px;font-weight:700;color:#7F1D1D;",
                summary$n_alerts))),

    # Per-trial cards
    div(style = "font-size:11px;font-weight:600;color:#64748B;
                 text-transform:uppercase;letter-spacing:.5px;margin-bottom:8px;",
        "Trials in this category"),
    div(lapply(summary$per_trial, function(s) {
      div(style = "padding:14px 16px;background:#FFFFFF;border:1px solid #EEF2F7;
                   border-radius:10px;margin-bottom:10px;",
          div(style = "display:flex;justify-content:space-between;align-items:baseline;
                       margin-bottom:8px;",
              div(style = "font-weight:600;color:#0F172A;font-size:14px;", s$short),
              div(style = "font-size:11px;color:#64748B;",
                  sprintf("%d %s",
                          length(s$insights),
                          if (length(s$insights) == 1) "insight" else "insights"))),
          if (length(s$insights) > 0)
            render_insights_panel(s$insights)
          else
            div(style = "font-size:12px;color:#94A3B8;font-style:italic;",
                "No insights — upload a REDCap export to populate.")
      )
    }))
  )
}

.insight_trial_modal <- function(s) {
  modalDialog(
    size = "l", easyClose = TRUE, footer = modalButton("Close"),
    title = .insight_modal_header("&#x1F50E;",
              paste(s$short, "— Smart Insights"),
              s$category),
    if (length(s$insights) > 0)
      render_insights_panel(s$insights)
    else
      div(style = "padding:18px;color:#94A3B8;font-style:italic;",
          "No insights yet — upload a REDCap export.")
  )
}

.insight_attention_modal <- function(summary) {
  flagged <- Filter(function(t)
    !is.null(t$top) && t$top$severity %in% c("warning", "alert"),
    summary$per_trial)

  modalDialog(
    size = "l", easyClose = TRUE, footer = modalButton("Close"),
    title = .insight_modal_header("&#x26A0;",
              "Trials needing attention",
              sprintf("%d %s flagged",
                      length(flagged),
                      if (length(flagged) == 1) "trial" else "trials")),

    if (length(flagged) == 0)
      div(style = "padding:14px;color:#15803D;font-size:13px;",
          HTML("&#x2728; Nothing flagged — every trial is healthy."))
    else
      div(lapply(flagged, function(s) {
        div(style = "padding:14px 16px;background:#FFFFFF;border:1px solid #EEF2F7;
                     border-radius:10px;margin-bottom:10px;",
            div(style = "display:flex;justify-content:space-between;align-items:baseline;
                         margin-bottom:8px;",
                div(style = "font-weight:600;color:#0F172A;font-size:14px;", s$short),
                div(style = "font-size:11px;color:#64748B;", s$category)),
            render_insights_panel(s$insights))
      }))
  )
}

.insight_healthy_modal <- function(summary) {
  modalDialog(
    size = "m", easyClose = TRUE, footer = modalButton("Close"),
    title = .insight_modal_header("&#x2728;",
              "Portfolio is healthy", "No critical issues detected"),
    div(style = "padding:14px;font-size:13px;color:#0F172A;line-height:1.7;",
        sprintf("All %d %s in your portfolio are recruiting actively without
                 critical alerts. Latest insights surface no stalled trials.",
                summary$n_total,
                if (summary$n_total == 1) "trial" else "trials"))
  )
}

.insight_lagging_modal <- function(summary) {
  rows <- list()
  for (s in summary$per_trial) {
    lag <- Filter(function(i) grepl("open but not recruiting", i$title), s$insights)
    if (length(lag)) {
      rows[[length(rows) + 1]] <- list(short = s$short,
                                       category = s$category,
                                       insight = lag[[1]])
    }
  }

  modalDialog(
    size = "l", easyClose = TRUE, footer = modalButton("Close"),
    title = .insight_modal_header("&#x1F4CD;",
              "Sites open but not recruiting",
              "Across the whole portfolio"),
    if (length(rows) == 0)
      div(style = "padding:14px;color:#15803D;",
          "No lagging sites detected.")
    else
      div(lapply(rows, function(r) {
        div(style = "padding:14px 16px;background:#FFFFFF;border:1px solid #EEF2F7;
                     border-radius:10px;margin-bottom:10px;",
            div(style = "font-weight:600;color:#0F172A;font-size:14px;margin-bottom:6px;",
                sprintf("%s · %s", r$short, r$category)),
            render_insights_panel(list(r$insight)))
      }))
  )
}

.insight_compare_modal <- function(rv) {
  trials <- rv$available_trials %||% discover_trials()
  if (length(trials) < 2) {
    return(modalDialog(title = "Compare trials", easyClose = TRUE,
                       footer = modalButton("Close"),
                       div("Need at least two trials to compare.")))
  }
  choices <- setNames(names(trials),
                      vapply(trials, function(t)
                        t$short_name %||% toupper(t$code), character(1)))
  modalDialog(
    size = "m", easyClose = TRUE,
    title = .insight_modal_header("&#x1F50D;", "Compare two trials"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("home_insight_compare_go", "Compare",
                   class = "btn btn-primary",
                   style = "background:#6366F1;border-color:#6366F1;font-weight:600;")
    ),
    div(style = "display:grid;grid-template-columns:1fr 1fr;gap:14px;",
        selectInput("home_insight_compare_a", "Trial A", choices = choices,
                    selected = choices[1]),
        selectInput("home_insight_compare_b", "Trial B", choices = choices,
                    selected = choices[min(2, length(choices))]))
  )
}

.insight_compare_result_modal <- function(sa, sb) {
  side <- function(s, accent) {
    div(style = sprintf("background:#FFFFFF;border:1px solid #EEF2F7;border-radius:12px;
                         padding:14px 16px;border-top:3px solid %s;", accent),
        div(style = "display:flex;justify-content:space-between;align-items:baseline;
                     margin-bottom:8px;",
            div(style = "font-weight:600;color:#0F172A;font-size:15px;", s$short),
            div(style = "font-size:11px;color:#64748B;", s$category)),
        div(style = "display:flex;gap:8px;margin-bottom:10px;font-size:11px;",
            span(style = "background:#FEF2F2;color:#B91C1C;padding:3px 8px;border-radius:999px;
                          font-weight:600;",
                 sprintf("%d alerts", s$n_alert)),
            span(style = "background:#FFFBEB;color:#B45309;padding:3px 8px;border-radius:999px;
                          font-weight:600;",
                 sprintf("%d warnings", s$n_warning)),
            span(style = "background:#F0FDF4;color:#15803D;padding:3px 8px;border-radius:999px;
                          font-weight:600;",
                 sprintf("%d info", s$n_info))),
        if (length(s$insights) > 0) render_insights_panel(s$insights)
        else div(style = "font-size:12px;color:#94A3B8;font-style:italic;",
                 "No insights yet."))
  }
  modalDialog(
    size = "l", easyClose = TRUE, footer = modalButton("Close"),
    title = .insight_modal_header("&#x1F50D;",
              sprintf("%s vs %s", sa$short, sb$short)),
    div(style = "display:grid;grid-template-columns:1fr 1fr;gap:14px;",
        side(sa, "#6366F1"),
        side(sb, "#8B5CF6"))
  )
}


# ── "Manage Users" modal (admin only) ────────────────────────────────────────
manage_users_modal <- function() {
  modalDialog(
    title = div(style = "display:flex;align-items:center;gap:10px;",
                span(style = "font-size:18px;color:#6366F1;", HTML("&#9881;")),
                span(style = "font-weight:600;", "Manage Users")),
    size = "l", easyClose = TRUE,
    footer = modalButton("Close"),

    div(style = "display:grid;grid-template-columns:1.4fr 0.8fr 2.4fr auto;
                 gap:14px;padding:8px 0;border-bottom:2px solid #EEF2F7;
                 font-size:11px;font-weight:600;color:#64748B;
                 text-transform:uppercase;letter-spacing:.5px;",
        div("User"),
        div("Portfolio role"),
        div("Trial access"),
        div("")),

    uiOutput("manage_users_table_ui"),

    div(style = "margin-top:18px;font-size:12px;color:#64748B;line-height:1.6;",
        HTML("<strong>Admin</strong> sees and manages every trial.
              <strong>Member</strong> sees only the trials granted below."))
  )
}

# ── "Edit memberships" modal (admin only) ────────────────────────────────────
edit_memberships_modal <- function(fullname) {
  modalDialog(
    title = div(style = "display:flex;align-items:center;gap:10px;",
                span(style = "font-weight:600;", "Trial access")),
    size = "m", easyClose = TRUE,
    footer = modalButton("Done"),

    uiOutput("edit_memberships_body")
  )
}


# ── "Add New Trial" wizard modal ──────────────────────────────────────────────

new_trial_wizard_ui <- function() {

  step_label <- function(num, title) {
    div(style = "margin-bottom:14px;",
        div(style = "display:flex;align-items:center;gap:10px;margin-bottom:8px;",
            span(style = "width:26px;height:26px;border-radius:50%;background:#6366F1;color:#fff;
                          display:inline-flex;align-items:center;justify-content:center;
                          font-size:12px;font-weight:700;", num),
            span(style = "font-size:14px;font-weight:600;color:#0F172A;", title)
        )
    )
  }

  help_text <- function(txt) {
    div(style = "font-size:11px;color:#64748B;margin-top:-6px;margin-bottom:10px;
                 line-height:1.5;font-style:italic;", txt)
  }

  modalDialog(
    title = div(style = "display:flex;align-items:center;gap:10px;",
                span(style = "font-size:22px;", HTML("&#x2795;")),
                span("Add a New Trial")),
    size = "l",
    easyClose = TRUE,
    footer = div(
      style = "display:flex;justify-content:space-between;width:100%;",
      modalButton("Cancel"),
      div(
        actionButton("wiz_prev", HTML("&larr; Back"),
                     class = "btn btn-outline-secondary",
                     style = "margin-right:8px;"),
        actionButton("wiz_next", HTML("Next &rarr;"),
                     class = "btn btn-primary",
                     style = "background:#6366F1;border-color:#6366F1;margin-right:8px;"),
        shinyjs::hidden(
          actionButton("wiz_create", HTML("&#x2714; Create Trial"),
                       class = "btn btn-success",
                       style = "background:#10B981;border-color:#10B981;font-weight:600;")
        )
      )
    ),

    uiOutput("wiz_step_indicator"),

    div(id = "wiz_step_1",
        step_label("1", "Trial basics"),
        help_text("These are the essential details. Everything else has sensible defaults you can tweak later."),
        div(style = "display:grid;grid-template-columns:1fr 1fr;gap:14px;",
            textInput("wiz_short_name", "Short name (e.g. TONIC, LOCI)", placeholder = "MYTRIAL"),
            numericInput("wiz_target", "Recruitment target", value = 100, min = 1)
        ),
        textInput("wiz_full_name", "Full trial name",
                  placeholder = "e.g. A randomised trial comparing X vs Y in patients with Z",
                  width = "100%"),
        div(style = "display:grid;grid-template-columns:1fr 1fr;gap:14px;",
            textInput("wiz_ci", "Chief Investigator", placeholder = "e.g. Prof Jane Smith"),
            textInput("wiz_sponsor", "Sponsor", placeholder = "e.g. University of Birmingham")
        ),
        selectInput("wiz_category", "Portfolio category",
                    choices = TRIAL_CATEGORIES, selected = "Surgery",
                    width = "100%")
    ),

    shinyjs::hidden(div(id = "wiz_step_2",
        step_label("2", "Data source"),
        help_text("Where does this trial's REDCap data live?"),
        radioButtons("wiz_data_source", NULL,
                     choices = c("Inside the app folder (recommended for testing)" = "local",
                                 "Network drive path (e.g. K: drive)" = "network"),
                     selected = "local", inline = FALSE),
        conditionalPanel(
          condition = "input.wiz_data_source == 'network'",
          textInput("wiz_data_path", "Full folder path to REDCap CSV exports",
                    placeholder = "K:/BCTU/Teams/MyTeam/MyTrial/Data", width = "100%"),
          help_text("Use forward slashes. The app needs read access to this folder.")
        )
    )),

    shinyjs::hidden(div(id = "wiz_step_3",
        step_label("3", "REDCap events"),
        help_text("Map your REDCap event names. Only Baseline is required."),
        div(style = "display:grid;grid-template-columns:1fr 1fr;gap:12px;",
            textInput("wiz_ev_baseline",  "Baseline event *",  value = "baseline_arm_1"),
            textInput("wiz_ev_discharge", "Discharge event",   value = "discharge_arm_1"),
            textInput("wiz_ev_day30",     "Day 30 event",      placeholder = "day_30_arm_1"),
            textInput("wiz_ev_day90",     "Day 90 event",      placeholder = "day_90_arm_1")
        ),
        textInput("wiz_ev_subforms", "Sub-forms / SAE events (comma-separated)",
                  value = "sub_forms_arm_1, ad_hoc_arm_1", width = "100%")
    )),

    shinyjs::hidden(div(id = "wiz_step_4",
        step_label("4", "REDCap field names"),
        help_text("Map the key variable names from your REDCap project. Only the first three are required."),
        div(style = "font-size:11px;font-weight:600;color:#0F172A;margin-bottom:6px;
                     text-transform:uppercase;letter-spacing:.5px;", "Required"),
        div(style = "display:grid;grid-template-columns:1fr 1fr 1fr;gap:12px;",
            textInput("wiz_fld_record_id", "Record ID",              value = "record_id"),
            textInput("wiz_fld_site",      "Site name",              value = "site_name"),
            textInput("wiz_fld_rand_dt",   "Randomisation datetime", value = "rand_dttm_s")
        ),
        tags$hr(style = "border:none;border-top:1px solid #EEF3F8;margin:12px 0;"),
        div(style = "font-size:11px;font-weight:600;color:#0F172A;margin-bottom:6px;
                     text-transform:uppercase;letter-spacing:.5px;", "Optional"),
        help_text("Leave blank if not applicable."),
        div(style = "display:grid;grid-template-columns:1fr 1fr 1fr;gap:12px;",
            textInput("wiz_fld_op_date",        "Operation date",   placeholder = "iop_op_end_dt"),
            textInput("wiz_fld_discharge_date", "Discharge date",   placeholder = "dis_discharge_day"),
            textInput("wiz_fld_age",            "Age",              placeholder = "cae_age"),
            textInput("wiz_fld_sex",            "Sex",              placeholder = "base_sex"),
            textInput("wiz_fld_ethnicity",      "Ethnicity",        placeholder = "base_ethnic_gp"),
            textInput("wiz_fld_cos_type",       "Change of status", placeholder = "cos_type")
        )
    )),

    shinyjs::hidden(div(id = "wiz_step_5",
        step_label("5", "Features"),
        help_text("Choose which dashboard tabs to show. You can change these later in the config file."),
        div(style = "display:grid;grid-template-columns:1fr 1fr;gap:6px 20px;margin-bottom:16px;",
            checkboxInput("wiz_feat_projections", "Recruitment projections",      value = TRUE),
            checkboxInput("wiz_feat_postal",      "Postal tracking tab",          value = FALSE),
            checkboxInput("wiz_feat_returns",     "Return rates tab",             value = FALSE),
            checkboxInput("wiz_feat_pilot",       "Pilot progression criteria",   value = FALSE),
            checkboxInput("wiz_feat_consort",     "CONSORT flow diagram",         value = FALSE),
            checkboxInput("wiz_feat_baseline",    "Baseline characteristics table", value = FALSE)
        )
    )),

    shinyjs::hidden(div(id = "wiz_step_6",
        step_label(HTML("&#x2714;"), "Review & create"),
        div(style = "background:#F5F3FF;border:1px solid #DCD6FE;border-radius:8px;padding:16px 18px;",
            uiOutput("wiz_review_summary")
        ),
        div(style = "margin-top:14px;font-size:12px;color:#64748B;line-height:1.6;",
            HTML("Click <strong>Create Trial</strong> to generate the configuration.
                  You can fine-tune settings later by editing
                  <code>trials/&lt;code&gt;/config.R</code>."))
    ))
  )
}
