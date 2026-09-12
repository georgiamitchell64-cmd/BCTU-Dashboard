trial_selector_ui <- function() {
  div(id = "trial_selector_panel", class = "home-root",

      # ── JS: tab switching, dropdown toggle, notification drawer ───────────
      tags$script(HTML("
        document.addEventListener('click', function(e){
          var prof = document.querySelector('.home-root .userchip');
          if (prof && prof.contains(e.target)) {
            prof.classList.toggle('open');
          } else if (prof) {
            prof.classList.remove('open');
          }
        });
        function homeShowTab(tab, el) {
          ['my','overview','all','sites','activity','people'].forEach(function(t){
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
          if (el) el.classList.add('active');
          if (window.Shiny) Shiny.setInputValue('home_active_tab', tab, {priority:'event'});
        }
      ")),

      div(class = "home-shell",

          # ── Top bar ──────────────────────────────────────────────────────
          # Logo size + filter are owned by .home-topbar img in home_redesign.css
          # so the BCTU colours render at their native magenta/pink.
          div(class = "home-topbar",
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

              div(class = "home-tabs",
                  tags$button(class = "htab active",
                              onclick = "homeShowTab('my', this)",
                              "My Trials",
                              span(class = "htab-count",
                                   textOutput("home_my_trials_count", inline = TRUE))),
                  tags$button(class = "htab",
                              onclick = "homeShowTab('overview', this)", "Portfolio"),
                  tags$button(class = "htab",
                              onclick = "homeShowTab('all', this)", "All Trials"),
                  tags$button(class = "htab",
                              onclick = "homeShowTab('sites', this)", "Sites"),
                  tags$button(class = "htab",
                              onclick = "homeShowTab('activity', this)",
                              "Activity",
                              uiOutput("home_activity_dot", inline = TRUE)),
                  shinyjs::hidden(tags$button(id = "htab_people", class = "htab",
                                              onclick = "homeShowTab('people', this)", "People"))
              ),

              # My Trials
              div(id = "home_tab_my",
                  uiOutput("home_summary_strip_ui"),

                  # Quick actions row
                  div(class = "qa-row",
                      tags$button(class = "qa-tile primary",
                                  onclick = "Shiny.setInputValue('qa_new_trial', Math.random(), {priority:'event'})",
                                  div(class = "qa-icon", HTML("&#43;")),
                                  div(class = "qa-text",
                                      div(class = "qa-label", "New trial"),
                                      div(class = "qa-desc", "Spin up a dashboard"))),
                      tags$button(class = "qa-tile",
                                  onclick = "Shiny.setInputValue('qa_run_report', Math.random(), {priority:'event'})",
                                  div(class = "qa-icon", HTML("&#x2913;")),
                                  div(class = "qa-text",
                                      div(class = "qa-label", "Run a report"),
                                      div(class = "qa-desc", "Generate TMG / TSC"))),
                      tags$button(class = "qa-tile",
                                  onclick = "Shiny.setInputValue('qa_switch_theme', Math.random(), {priority:'event'})",
                                  div(class = "qa-icon", HTML("&#x2197;")),
                                  div(class = "qa-text",
                                      div(class = "qa-label", "Switch theme"),
                                      div(class = "qa-desc", "Light, dark, system"))),
                      tags$button(class = "qa-tile",
                                  onclick = "Shiny.setInputValue('qa_portfolio_settings', Math.random(), {priority:'event'})",
                                  div(class = "qa-icon", HTML("&#9881;")),
                                  div(class = "qa-text",
                                      div(class = "qa-label", "Portfolio settings"),
                                      div(class = "qa-desc", "Members, defaults, audit")))
                  ),

                  # Header for the trials grid
                  div(class = "sec-head2",
                      div(tags$h2(textOutput("home_trials_section_title", inline = TRUE)),
                          div(class = "sec-head2-sub",
                              "Click any card to open the trial dashboard")),
                      uiOutput("home_add_button_ui", inline = TRUE)
                  ),
                  uiOutput("trial_cards_ui"),

                  # Recent activity preview
                  div(class = "sec-head2",
                      div(tags$h2("Recent activity"),
                          div(class = "sec-head2-sub",
                              "Across all your trials")),
                      tags$button(class = "sec-head2-act",
                                  onclick = "homeShowTab('activity', document.querySelectorAll('.home-root .htab')[4])",
                                  "See all →")
                  ),
                  uiOutput("home_activity_preview_ui")
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

              # Activity
              people_tab_ui(),

              div(id = "home_tab_activity", style = "display:none;",
                  div(class = "sec-head2",
                      div(tags$h2("Activity"),
                          div(class = "sec-head2-sub",
                              "Recent randomisations, site changes, and data uploads"))
                  ),
                  uiOutput("home_activity_ui")
              )
          )
      )
  )
}

