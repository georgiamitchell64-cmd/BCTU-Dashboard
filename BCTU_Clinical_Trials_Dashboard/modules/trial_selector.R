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
          ['my','overview','all','sites','activity'].forEach(function(t){
            var n = document.getElementById('home_tab_'+t);
            if (n) n.style.display = (t===tab) ? '' : 'none';
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
                              uiOutput("home_activity_dot", inline = TRUE))
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


# ── "User management" modal (admin only) ──────────────────────────────────────
# Master-detail console: searchable user list on the left, a detail panel on the
# right with portfolio role, per-trial access and password actions. Passwords are
# never shown — only reset (to a temporary the user must change) or set.
manage_users_modal <- function() {
  modalDialog(
    title = NULL, footer = NULL, size = "l", easyClose = TRUE,
    div(class = "mu-root",
      div(class = "mu-head",
          div(class = "mu-head-titles",
              div(class = "mu-title",
                  HTML('<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2"/><circle cx="9" cy="7" r="4"/><path d="M23 21v-2a4 4 0 0 0-3-3.87"/><path d="M16 3.13a4 4 0 0 1 0 7.75"/></svg>'),
                  span("User management")),
              div(class = "mu-sub",
                  "Access and credentials across the portfolio")),
          div(class = "mu-head-actions",
              div(class = "mu-search",
                  HTML('<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="11" cy="11" r="7"/><path d="M20 20l-3-3"/></svg>'),
                  tags$input(type = "text", id = "mu_search",
                             class = "mu-search-input",
                             placeholder = "Search users…",
                             oninput = "Shiny.setInputValue('mu_search', this.value)")),
              tags$button(type = "button", class = "mu-btn mu-btn-navy mu-add-btn",
                          onclick = "Shiny.setInputValue('mu_new_user', Math.random(), {priority:'event'})",
                          HTML("&#43; New user")))),
      div(class = "mu-body",
          div(class = "mu-list", uiOutput("mu_user_list_ui")),
          div(class = "mu-detail", uiOutput("mu_detail_ui"))),
      div(class = "mu-foot",
          span(class = "mu-foot-note",
               HTML('&#128274; Passwords are encrypted — they can never be viewed, only reset or set.')),
          modalButton("Close"))
    )
  )
}
