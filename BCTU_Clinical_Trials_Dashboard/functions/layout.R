build_app_ui <- function() {
  fluidPage(
    theme = tonic_theme,

    tags$head(
      # Order matters: tokens + base components first, then page-level
      # overrides in the redesign sheets.
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com",
                crossorigin = ""),
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@400;500;600;700&display=swap"),
      tags$link(rel = "stylesheet", type = "text/css", href = "tonic_core.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "home_redesign.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "modules_redesign.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "panorama_overview.css"),
      tags$script(HTML("
        (function() {
          try {
            var t = localStorage.getItem('bctu_theme') || 'light';
            if (t === 'system') {
              t = (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) ? 'dark' : 'light';
            }
            document.documentElement.setAttribute('data-theme', t);
          } catch(e) { document.documentElement.setAttribute('data-theme','light'); }
        })();
        function setActiveNav(id) {
          document.querySelectorAll('.sidebar-nav-btn').forEach(function(b) {
            b.classList.remove('active-nav');
          });
          var el = document.getElementById(id);
          if (el) el.classList.add('active-nav');
        }
        function setActiveTab(id) {
          document.querySelectorAll('.topnav-tab').forEach(function(t) {
            t.classList.remove('topnav-active');
          });
          var el = document.getElementById(id);
          if (el) el.classList.add('topnav-active');
        }
        function setActiveWp(id) {
          document.querySelectorAll('.wp-pill').forEach(function(b) {
            b.classList.remove('on');
          });
          var el = document.getElementById(id);
          if (el) el.classList.add('on');
        }
        // Account menu (in-trial topbar) — toggle open, close on outside click.
        function toggleAccountMenu(e) {
          if (e) e.stopPropagation();
          var el = document.getElementById('topbar_account');
          if (el) el.classList.toggle('open');
        }
        document.addEventListener('click', function(e) {
          var a = document.getElementById('topbar_account');
          if (a && !a.contains(e.target)) a.classList.remove('open');
        });
      ")),
      tags$style(HTML("
        .wp-picker-bar {
          background:#FFFFFF; border-bottom:1px solid #E2E8EE;
          padding:8px 22px; display:flex; align-items:center; gap:8px;
          overflow-x:auto;
        }
        .wp-picker-bar .wp-eye {
          font-size:10.5px; font-weight:600; color:#64748B;
          text-transform:uppercase; letter-spacing:.6px; margin-right:6px;
          flex-shrink:0;
        }
        .wp-pill {
          padding:5px 14px; border-radius:999px;
          border:1px solid #DDE5EE; background:#fff;
          color:#475569; font-size:12px; font-weight:500;
          font-family:inherit; cursor:pointer; white-space:nowrap;
        }
        .wp-pill:hover { border-color:#1B4F6B; color:#1B4F6B; }
        .wp-pill.on {
          background:#1B4F6B; color:#fff; border-color:#1B4F6B;
          font-weight:600;
        }
      "))
    ),

    useShinyjs(),

    # Hidden nav buttons so observeEvent handlers still have valid IDs
    div(style = "display:none;",
        actionButton("go_overview",       "Overview",          class = "sidebar-nav-btn"),
        actionButton("go_charts",         "Charts",            class = "sidebar-nav-btn"),
        actionButton("go_reports",        "Reports",           class = "sidebar-nav-btn"),
        actionButton("go_modifications",  "Modifications",     class = "sidebar-nav-btn"),
        actionButton("go_randomisations", "Randomisations",    class = "sidebar-nav-btn"),
        actionButton("go_participants",   "Data",              class = "sidebar-nav-btn"),
        div(id = "go_returns_wrap",
            actionButton("go_returns",    "Return Rates",      class = "sidebar-nav-btn")),
        actionButton("go_sites",          "Manage Sites",      class = "sidebar-nav-btn"),
        actionButton("go_upload",         "Data / Export",     class = "sidebar-nav-btn"),
        # Postal Tracking and Accounts removed in the desktop build.
        # Hidden placeholders keep shinyjs::show()/hide() calls in
        # trial_selector_server.R and trial_settings_server.R harmless.
        div(id = "go_postal_wrap", style = "display:none"),
        div(id = "accounts_nav",   style = "display:none"),
        div(id = "settings_nav",
            actionButton("go_settings",   "Trial Settings",    class = "sidebar-nav-btn")),
        # Binding for the "All trials" home control rendered in the tab bar.
        actionButton("back_to_selector",  "All trials",        class = "sidebar-nav-btn"),
        # Hidden sidebar outputs so server code doesn't error
        textOutput("sb_name"),
        textOutput("sb_role"),
        # Hidden filter inputs (still used by server code)
        div(id = "sidebar_nav_section",
            textInput("search_txt", label = NULL, placeholder = "Search sites", width = "100%"),
            pickerInput("status_filter", label = NULL,
                        choices = c("Identified", "Set-up", "Open", "Recruiting", "Closed"),
                        selected = c("Identified", "Set-up", "Open", "Recruiting", "Closed"),
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, title = "Status filter")))
    ),

    # ── Welcome screen overlay (covers everything until user registers) ─────
    welcome_screen_ui(),

    div(id = "app_main_wrap", style = "padding:0;",

        # ═══ Top bar — hidden until trial is selected ═══════════════════════
        shinyjs::hidden(div(id = "topbar_wrap", class = "topbar",
            div(class = "topbar-left",
                tags$img(src = "BlackText-landscape.png",
                         class = "topbar-logo",
                         alt = "Birmingham Clinical Trials Unit"),
                div(class = "topbar-divider"),
                span(class = "topbar-title",
                     uiOutput("topbar_trial_label", inline = TRUE)),
                tags$div(style = "display:none;",
                     textOutput("topbar_view_badge", inline = TRUE))
            ),
            div(class = "topbar-right",
                div(class = "topbar-bell", title = "Notifications",
                    HTML('<svg width="16" height="16" viewBox="0 0 16 16" fill="none" aria-hidden="true"><path d="M4 6.5a4 4 0 018 0c0 2 1 3.5 1.5 4H2.5c.5-.5 1.5-2 1.5-4z" stroke="currentColor" stroke-width="1.2" fill="none" stroke-linejoin="round"/><path d="M6 10.5s.5 2 2 2 2-2 2-2" stroke="currentColor" stroke-width="1.2" stroke-linecap="round" fill="none"/></svg>'),
                    div(class = "topbar-bell-dot")
                ),
                # Account control — initials + name + caret, opening a small
                # menu (change password / sign out). Far clearer than a bare icon.
                div(id = "topbar_account", class = "topbar-account",
                    onclick = "toggleAccountMenu(event)",
                    span(class = "topbar-acct-avatar",
                         textOutput("topbar_acct_initials", inline = TRUE)),
                    span(class = "topbar-acct-info",
                         span(class = "topbar-acct-name",
                              textOutput("topbar_acct_name", inline = TRUE)),
                         span(class = "topbar-acct-role",
                              textOutput("topbar_role", inline = TRUE))),
                    span(class = "topbar-acct-caret", HTML("&#9662;")),
                    div(class = "topbar-acct-menu",
                        div(class = "tam-head", "Account"),
                        tags$button(class = "tam-item", type = "button",
                                    onclick = "Shiny.setInputValue('home_change_password', Math.random(), {priority:'event'})",
                                    HTML('<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="11" width="18" height="11" rx="2"/><path d="M7 11V7a5 5 0 0 1 10 0v4"/></svg>'),
                                    span("Change password")),
                        tags$button(class = "tam-item tam-danger", type = "button",
                                    onclick = "Shiny.setInputValue('home_sign_out', Math.random(), {priority:'event'})",
                                    HTML('<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M9 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h4"/><path d="M16 17l5-5-5-5"/><path d="M21 12H9"/></svg>'),
                                    span("Sign out"))),
                    tags$div(style = "display:none;",
                        textOutput("topbar_username", inline = TRUE))
                )
            )
        )),

        # ═══ Horizontal tab bar (hidden until trial is selected) ═══════════
        shinyjs::hidden(div(id = "topnav_wrap", class = "topnav-bar topnav-pills",
            # Prominent "back to all trials" control at the start of the nav —
            # the obvious way out of a trial, separated from the in-trial tabs.
            tags$button(id = "tn_home", class = "topnav-home", type = "button",
                        title = "Back to all trials",
                        onclick = "Shiny.setInputValue('back_to_selector', Math.random(), {priority:'event'});",
                        HTML('<svg width="15" height="15" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M3 12l9-9 9 9"/><path d="M5 10v10a1 1 0 0 0 1 1h4v-6h4v6h4a1 1 0 0 0 1-1V10"/></svg>'),
                        span("All trials")),
            tags$span(class = "topnav-sep"),
            tags$button(id = "tn_overview",       class = "topnav-tab topnav-active", type = "button",
                        onclick = "Shiny.setInputValue('go_overview', Math.random(), {priority:'event'}); setActiveTab('tn_overview');",
                        "Overview"),
            tags$button(id = "tn_participants",   class = "topnav-tab", type = "button",
                        onclick = "Shiny.setInputValue('go_participants', Math.random(), {priority:'event'}); setActiveTab('tn_participants');",
                        "Data"),
            tags$button(id = "tn_sites",          class = "topnav-tab", type = "button",
                        onclick = "Shiny.setInputValue('go_sites', Math.random(), {priority:'event'}); setActiveTab('tn_sites');",
                        "Sites"),
            tags$button(id = "tn_randomisations", class = "topnav-tab", type = "button",
                        onclick = "Shiny.setInputValue('go_randomisations', Math.random(), {priority:'event'}); setActiveTab('tn_randomisations');",
                        "Randomisations"),
            # Postal tracking tab removed in the desktop build. Hidden
            # placeholder keeps shinyjs::show("tn_postal") calls harmless.
            tags$span(id = "tn_postal", style = "display:none"),
            tags$button(id = "tn_returns",        class = "topnav-tab", type = "button",
                        onclick = "Shiny.setInputValue('go_returns', Math.random(), {priority:'event'}); setActiveTab('tn_returns');",
                        "Return rates"),
            tags$button(id = "tn_reports",        class = "topnav-tab topnav-reports", type = "button",
                        onclick = "Shiny.setInputValue('go_reports', Math.random(), {priority:'event'}); setActiveTab('tn_reports');",
                        "Reports"),
            tags$button(id = "tn_modifications",  class = "topnav-tab", type = "button",
                        onclick = "Shiny.setInputValue('go_modifications', Math.random(), {priority:'event'}); setActiveTab('tn_modifications');",
                        "Modifications"),
            tags$button(id = "tn_settings",       class = "topnav-tab", type = "button",
                        onclick = "Shiny.setInputValue('go_settings', Math.random(), {priority:'event'}); setActiveTab('tn_settings');",
                        "Settings")
        )),

        # ═══ Work-package picker (only shown for multi-WP trials) ═════════
        # Server-rendered: a row of pill buttons — "Overview" + one per WP.
        # When a trial has no work_packages defined, the wrapper is hidden.
        shinyjs::hidden(div(id = "wp_picker_wrap", class = "wp-picker-bar",
            uiOutput("wp_picker_ui")
        )),

        # ── Trial selector (shown first) ──────────────────────────────────────
        trial_selector_ui(),

        # ── New-trial setup (full-page wizard; hidden until "New trial") ──────
        new_trial_setup_ui(),

        # ── Dashboard panels (hidden until trial is selected) ─────────────────
        shinyjs::hidden(
          div(id = "dashboard_panel", style = "padding:20px 22px;",
              tabsetPanel(id = "active_tab", type = "hidden",
                          overview_tab_ui(),
                          charts_tab_ui(),
                          reports_tab_ui(),
                          modifications_tab_ui(),
                          randomisations_tab_ui(),
                          participants_tab_ui(),
                          sites_tab_ui(),
                          upload_tab_ui(),
                          return_rates_tab_ui(),
                          # Postal tracking and Accounts tabs removed in the
                          # desktop build.
                          trial_settings_tab_ui()
              )
          )
        )
    )
  )
}
