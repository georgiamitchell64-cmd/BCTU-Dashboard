build_app_ui <- function() {
  page_sidebar(
  theme    = tonic_theme,
  title    = NULL,
  fillable = FALSE,
  padding  = 0,

  sidebar = sidebar(
    id = "main_sidebar", open = "always",
    padding = 0,

    tags$head(
      tags$style(HTML(tonic_css)),
      tags$script(HTML("
        function setActiveNav(id) {
          document.querySelectorAll('.sidebar-nav-btn').forEach(function(b) {
            b.classList.remove('active-nav');
          });
          var el = document.getElementById(id);
          if (el) el.classList.add('active-nav');
        }
      "))
    ),

    useShinyjs(),

    div(class = "sidebar-logo",
        # Logo updates dynamically when trial is selected
        uiOutput("sidebar_logo_ui"),
        div(class = "sidebar-logo-info",
            div(class = "sidebar-user-name", textOutput("sb_name", inline = TRUE)),
            div(class = "sidebar-user-role", textOutput("sb_role", inline = TRUE))
        )
    ),

    # ── Sidebar navigation (hidden until trial is selected) ─────────────────
    shinyjs::hidden(div(id = "sidebar_nav_section",

      # Back to trial selector button
      div(id = "back_to_selector_wrap", style = "padding:0 14px 4px;",
          actionButton("back_to_selector",
                       label = HTML("&#x2190; Switch trial"),
                       class = "btn btn-sm",
                       style = "width:100%;background:rgba(255,255,255,.08);color:rgba(255,255,255,.7);
                                border:1px solid rgba(255,255,255,.12);font-size:11px;font-weight:500;
                                border-radius:5px;margin-bottom:4px;",
                       width = "100%")
      ),

      span(class = "nav-section-label", "Navigation"),
      nav_btn("go_overview",       "Overview",          "chart-line"),
      nav_btn("go_reports",        "Reports & Charts",  "chart-bar"),
      nav_btn("go_randomisations", "Randomisations",    "users"),
      nav_btn("go_participants",   "Participant Data",  "clipboard-list"),
      div(id = "go_returns_wrap",
          nav_btn("go_returns",    "Return Rates",      "clipboard-check")),
      nav_btn("go_sites",          "Manage Sites",      "hospital"),
      nav_btn("go_upload",         "Data / Export",     "folder-open"),
      div(id = "go_postal_wrap",
          nav_btn("go_postal",     "Postal Tracking",   "envelope")),
      div(id = "accounts_nav",
          nav_btn("go_accounts",   "Accounts",          "users-gear")
      ),
      div(id = "settings_nav", class = "tm-only",
          nav_btn("go_settings",   "Trial Settings",    "gear")
      ),

      tags$hr(style = "border-color:rgba(255,255,255,.08);margin:8px 16px"),
      span(class = "nav-section-label", "Filter"),
      div(style = "padding:0 14px 10px;",
          textInput("search_txt", label = NULL, placeholder = "Search sites",
                    width = "100%"),
          pickerInput("status_filter", label = NULL,
                      choices = c("Identified", "Set-up", "Open", "Recruiting", "Closed"),
                      selected = c("Identified", "Set-up", "Open", "Recruiting", "Closed"),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE, title = "Status filter",
                        style = "background:rgba(255,255,255,.09)!important;color:#fff!important;border-color:rgba(255,255,255,.14)!important;font-size:12px!important"
                      ))
      )
    ))
  ),

  # ── Welcome screen overlay (covers everything until user registers) ─────
  welcome_screen_ui(),

  div(style = "padding:20px 22px;",

      # Top bar — hidden until trial is selected
      shinyjs::hidden(div(id = "topbar_wrap", class = "topbar",
          div(style = "display:flex;align-items:center",
              span(class = "topbar-title", "Clinical Trials Dashboard"),
              span(class = "topbar-badge", textOutput("topbar_view_badge", inline = TRUE))
          ),
          div(class = "topbar-user",
              span(class = "role-badge", textOutput("topbar_role", inline = TRUE)),
              textOutput("topbar_username", inline = TRUE)
          )
      )),

      # ── Trial selector (shown first) ──────────────────────────────────────
      trial_selector_ui(),

      # ── Dashboard panels (hidden until trial is selected) ─────────────────
      shinyjs::hidden(
        div(id = "dashboard_panel",
            tabsetPanel(id = "active_tab", type = "hidden",
                        overview_tab_ui(),
                        reports_tab_ui(),
                        randomisations_tab_ui(),
                        participants_tab_ui(),
                        sites_tab_ui(),
                        upload_tab_ui(),
                        return_rates_tab_ui(),
                        postal_tracking_tab_ui(),
                        trial_settings_tab_ui(),
                        accounts_tab_ui()
            )
        )
      )
  )
)
}
