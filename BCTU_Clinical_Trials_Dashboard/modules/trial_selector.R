trial_selector_ui <- function() {
  div(id = "trial_selector_panel", class = "home-root",

      # ── JS: tab switching, menus, My Trials toolbar ───────────────────────
      tags$script(HTML("
        document.addEventListener('click', function(e){
          var prof = document.querySelector('.home-root .userchip');
          var gear = document.querySelector('.home-root .home-settings');
          if (prof && prof.contains(e.target)) {
            prof.classList.toggle('open');
          } else if (prof) {
            prof.classList.remove('open');
          }
          if (gear && gear.contains(e.target)) {
            // rows marked data-keep keep the menu open (toggles)
            if (e.target.closest('.hs-item') && !e.target.closest('[data-keep]')) gear.classList.remove('open');
            else if (!e.target.closest('.hs-menu')) gear.classList.toggle('open');
          } else if (gear) {
            gear.classList.remove('open');
          }
        });
        function homeShowTab(tab, el) {
          ['my','overview','all','sites','people'].forEach(function(t){
            var n = document.getElementById('home_tab_'+t);
            if (!n) return;
            n.style.display = (t===tab) ? '' : 'none';
            // Tell Shiny, so outputs inside a tab that has just appeared
            // stop being suspended as hidden and render.
            if (window.jQuery) jQuery(n).trigger((t===tab) ? 'shown' : 'hidden');
          });
          document.querySelectorAll('.home-root .htab').forEach(function(t){
            t.classList.remove('active');
          });
          if (!el) el = document.getElementById('htab_' + tab);
          if (el) el.classList.add('active');
          if (window.Shiny) Shiny.setInputValue('home_active_tab', tab, {priority:'event'});
        }
      ")),

      div(class = "home-shell",

          # ── Top bar ──────────────────────────────────────────────────────
          div(class = "home-topbar",
              # Settings menu (theme, optional tabs, people & access)
              div(class = "home-settings",
                  tags$button(class = "tb-icon", title = "Settings", type = "button",
                              `aria-label` = "Settings",
                              tags$svg(width = "16", height = "16", viewBox = "0 0 24 24",
                                       fill = "none", stroke = "currentColor",
                                       `stroke-width` = "2", `stroke-linecap` = "round",
                                       `stroke-linejoin` = "round",
                                       tags$circle(cx = "12", cy = "12", r = "3"),
                                       tags$path(d = "M19.4 15a1.65 1.65 0 0 0 .33 1.82l.06.06a2 2 0 1 1-2.83 2.83l-.06-.06a1.65 1.65 0 0 0-1.82-.33 1.65 1.65 0 0 0-1 1.51V21a2 2 0 1 1-4 0v-.09a1.65 1.65 0 0 0-1-1.51 1.65 1.65 0 0 0-1.82.33l-.06.06a2 2 0 1 1-2.83-2.83l.06-.06A1.65 1.65 0 0 0 4.6 15a1.65 1.65 0 0 0-1.51-1H3a2 2 0 1 1 0-4h.09a1.65 1.65 0 0 0 1.51-1 1.65 1.65 0 0 0-.33-1.82l-.06-.06a2 2 0 1 1 2.83-2.83l.06.06A1.65 1.65 0 0 0 9 4.6a1.65 1.65 0 0 0 1-1.51V3a2 2 0 1 1 4 0v.09a1.65 1.65 0 0 0 1 1.51 1.65 1.65 0 0 0 1.82-.33l.06-.06a2 2 0 1 1 2.83 2.83l-.06.06A1.65 1.65 0 0 0 19.4 9a1.65 1.65 0 0 0 1.51 1H21a2 2 0 1 1 0 4h-.09a1.65 1.65 0 0 0-1.51 1z"))),
                  uiOutput("home_settings_ui")),
              tags$img(src = "BlackText-landscape.png",
                       alt = "BCTU — Birmingham Clinical Trials Unit"),
              div(class = "topbar-spacer"),
              tags$button(class = "tb-icon", title = "Notifications",
                          onclick = "Shiny.setInputValue('notif_open', Math.random(), {priority:'event'})",
                          HTML("&#x1F514;"),
                          uiOutput("notif_badge_ui", inline = TRUE)),
              tags$button(class = "tb-icon", title = "Help",
                          onclick = "Shiny.setInputValue('home_help_open', Math.random(), {priority:'event'})",
                          "?"),
              div(class = "userchip",
                  div(class = "useravatar",
                      textOutput("home_user_initials", inline = TRUE)),
                  textOutput("home_profile_name", inline = TRUE),
                  span(class = "userchip-caret", HTML("&#9662;")),
                  uiOutput("home_dropdown_ui", inline = FALSE)
              )
          ),

          # ── Notification drawer (off-canvas, slides from right) ──────────
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
          ")),
          div(class = "notif-drawer-bg",
              onclick = "document.body.classList.remove('notif-open')"),
          div(class = "notif-drawer",
              div(class = "notif-drawer-head",
                  div(div(style = "font-weight:600;color:#1B1B1B;font-size:15px;",
                          "Smart Notifications"),
                      div(style = "font-size:11.5px;color:#58595B;",
                          "Auto-generated from trial data")),
                  div(
                      actionLink("notif_clear_all", "Clear all",
                                 style = "font-size:11.5px;color:#58595B;
                                          text-decoration:none;margin-right:14px;"),
                      tags$button(onclick = "document.body.classList.remove('notif-open')",
                                  style = "background:transparent;border:none;color:#8A8A8C;
                                           font-size:18px;cursor:pointer;line-height:1;",
                                  HTML("&times;")))
              ),
              div(class = "notif-drawer-body",
                  uiOutput("notif_drawer_ui"))),

          # ── Canvas with tabs ─────────────────────────────────────────────
          div(class = "home-canvas",

              # Portfolio and All Trials are optional (settings menu); Sites and
              # All Trials are admin-only. The server shows them. People & access
              # has no tab: it opens from the settings menu.
              div(class = "home-tabs",
                  tags$button(id = "htab_my", class = "htab active",
                              onclick = "homeShowTab('my', this)",
                              "My Trials",
                              span(class = "htab-count",
                                   textOutput("home_my_trials_count", inline = TRUE))),
                  shinyjs::hidden(tags$button(id = "htab_overview", class = "htab",
                                              onclick = "homeShowTab('overview', this)", "Portfolio")),
                  shinyjs::hidden(tags$button(id = "htab_all", class = "htab",
                                              onclick = "homeShowTab('all', this)", "All Trials")),
                  shinyjs::hidden(tags$button(id = "htab_sites", class = "htab",
                                              onclick = "homeShowTab('sites', this)", "Sites"))
              ),

              # My Trials: header, toolbar, tiles
              div(id = "home_tab_my",
                  div(class = "mt-head",
                      div(class = "mt-title-wrap",
                          tags$h1(class = "mt-title", "My Trials",
                                  span(class = "mt-badge",
                                       textOutput("home_my_trials_badge", inline = TRUE))),
                          div(class = "mt-sub", "Click a trial to open its dashboard")),
                      uiOutput("home_add_button_ui", inline = TRUE)),
                  div(class = "mt-toolbar",
                      div(class = "mt-search",
                          tags$svg(width = "14", height = "14", viewBox = "0 0 24 24",
                                   fill = "none", stroke = "currentColor", `stroke-width` = "2",
                                   tags$circle(cx = "11", cy = "11", r = "7"),
                                   tags$path(d = "M20 20l-3-3")),
                          tags$input(type = "search", id = "my_search_box",
                                     placeholder = "Search trials, CIs, sponsors",
                                     `aria-label` = "Search trials", autocomplete = "off",
                                     oninput = "Shiny.setInputValue('my_search', this.value)")),
                      uiOutput("home_my_category_ui", inline = TRUE),
                      div(class = "mt-toolbar-spacer"),
                      span(class = "mt-count", textOutput("home_my_trials_showing", inline = TRUE))),
                  uiOutput("my_trials_table_ui")
              ),

              # Portfolio (was Overview)
              div(id = "home_tab_overview", style = "display:none;",
                  div(class = "sec-head2",
                      div(tags$h2("Portfolio"),
                          div(class = "sec-head2-sub",
                              "Aggregate recruitment and site activity across all trials"))
                  ),
                  uiOutput("home_overview_ui")
              ),

              # All Trials
              div(id = "home_tab_all", style = "display:none;",
                  div(class = "sec-head2",
                      div(tags$h2("All Trials"),
                          div(class = "sec-head2-sub",
                              "Every trial in the BCTU portfolio · click any to open its dashboard"))
                  ),

                  # Toolbar — search + status filter + sort + view + count.
                  # The chip and view-toggle states are mirrored to hidden Shiny
                  # inputs so the server can re-render the list on change.
                  div(class = "at-toolbar",
                    div(class = "at-search-wrap",
                        tags$svg(width = "14", height = "14", viewBox = "0 0 24 24",
                                 fill = "none", stroke = "currentColor",
                                 `stroke-width` = "2",
                                 tags$circle(cx = "11", cy = "11", r = "7"),
                                 tags$path(d = "M20 20l-3-3")),
                        textInput("all_trials_search", label = NULL,
                                  placeholder = "Search by trial, CI, sponsor…",
                                  width = "100%")),
                    div(class = "at-pill-group", id = "all_trials_filter_group",
                        tags$button(class = "at-pill on", `data-key` = "all",   "All"),
                        tags$button(class = "at-pill",    `data-key` = "on",    "On track"),
                        tags$button(class = "at-pill",    `data-key` = "warn",  "Behind"),
                        tags$button(class = "at-pill",    `data-key` = "risk",  "Stalled"),
                        tags$button(class = "at-pill",    `data-key` = "setup", "Set-up"),
                        tags$button(class = "at-pill",    `data-key` = "closed","Closed")),
                    selectInput("all_trials_sort", label = NULL,
                                choices = c(
                                  "Sort: Recruitment %"   = "pct",
                                  "Sort: Trial name"      = "name",
                                  "Sort: Recent activity" = "recent",
                                  "Sort: Status (worst first)" = "status"),
                                selected = "pct", width = "180px"),
                    tags$div(class = "at-toolbar-spacer"),
                    tags$span(class = "at-result-count",
                              textOutput("all_trials_count_lbl", inline = TRUE))
                  ),
                  # Tiny JS to mirror chip clicks into a Shiny input
                  tags$script(HTML("
                    document.addEventListener('click', function(ev){
                      var btn = ev.target.closest('#all_trials_filter_group .at-pill');
                      if (!btn) return;
                      document.querySelectorAll('#all_trials_filter_group .at-pill')
                        .forEach(function(b){ b.classList.remove('on'); });
                      btn.classList.add('on');
                      Shiny.setInputValue('all_trials_filter',
                        btn.getAttribute('data-key'), {priority:'event'});
                    });
                  ")),

                  uiOutput("home_all_trials_ui")
              ),

              # Sites
              div(id = "home_tab_sites", style = "display:none;",
                  div(class = "sec-head2",
                      div(tags$h2("Site performance"),
                          div(class = "sec-head2-sub",
                              "Top sites by category, plus look-up for any specific site"))
                  ),
                  uiOutput("home_sites_ui")
              ),

              # People & access (admin; opened from the settings menu)
              people_tab_ui()
          )
      )
  )
}
