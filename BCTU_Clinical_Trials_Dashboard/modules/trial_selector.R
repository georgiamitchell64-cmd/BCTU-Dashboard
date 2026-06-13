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
                    width = "100%"),

        # Trial type
        selectInput("wiz_trial_type", "Trial type",
                    choices = c(
                      "Randomised (open label)"   = "randomised",
                      "Randomised (single blind)" = "single_blind",
                      "Randomised (double blind)" = "double_blind",
                      "Observational"             = "observational",
                      "Single-arm / cohort"       = "single_arm",
                      "Platform / umbrella"       = "platform",
                      "Other"                     = "other"
                    ),
                    selected = "randomised", width = "100%"),

        # Multi-work-package toggle. When ticked, exposes a count input and
        # a dynamically rendered set of fields (WKP1, WKP2, ...) so the user
        # fills them in like a modern form.
        div(style = "background:#F8FAFD;border:1px solid #E2E8EE;border-radius:10px;
                     padding:14px 16px;margin-top:8px;",
            checkboxInput("wiz_is_multi_wp",
                          "This is a platform trial or has multiple work packages",
                          value = FALSE),
            help_text("Tick if the trial has multiple sub-projects (e.g. Panorama).
                       The dashboard will show per-WP tabs and a roll-up overview."),
            shinyjs::hidden(div(id = "wiz_wp_panel",
                div(style = "display:grid;grid-template-columns:160px 1fr;gap:12px;
                             align-items:center;margin-bottom:10px;",
                    tags$label("How many work packages?",
                               style = "font-size:12px;font-weight:600;color:#0F172A;
                                        margin:0;"),
                    numericInput("wiz_n_wps", label = NULL,
                                 value = 2, min = 1, max = 10, step = 1,
                                 width = "100px")),
                # Server-rendered: one labelled field per WP.
                uiOutput("wiz_wp_fields_ui")
            ))
        )
    ),

    shinyjs::hidden(div(id = "wiz_step_2",
        step_label("2", "Data paths"),
        help_text("Paste the K: drive (or network) paths to your trial data. Use forward slashes. Leave blank to use the local app folder."),

        div(class = "s-field", style = "margin-bottom:14px;",
            tags$label(style = "font-size:12px;font-weight:600;color:#0F172A;",
                       HTML("REDCap data CSV folder &#x1F4C1;")),
            textInput("wiz_data_path",  label = NULL,
                      placeholder = "K:/BCTU/Teams/MyTeam/MyTrial/Data",
                      width = "100%"),
            help_text("Folder containing your REDCap CSV exports (the app picks the newest file).")),

        div(class = "s-field", style = "margin-bottom:14px;",
            tags$label(style = "font-size:12px;font-weight:600;color:#0F172A;",
                       HTML("Return rates CSV folder &#x1F4C1;")),
            textInput("wiz_rr_path", label = NULL,
                      placeholder = "K:/BCTU/Teams/MyTeam/MyTrial/ReturnRates",
                      width = "100%"),
            help_text("Folder containing return-rate CSVs. Leave blank if not applicable.")),

        div(class = "s-field", style = "margin-bottom:14px;",
            tags$label(style = "font-size:12px;font-weight:600;color:#0F172A;",
                       HTML("Trial logo file &#x1F5BC;")),
            textInput("wiz_logo_path", label = NULL,
                      placeholder = "K:/BCTU/Teams/MyTeam/MyTrial/logo.png",
                      width = "100%"),
            help_text("Path to a .png or .jpg logo file. Displayed in the topbar and reports."))
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
        help_text("Choose which dashboard tabs and sections to show. You can change these later in Trial Settings."),
        div(style = "display:grid;grid-template-columns:1fr 1fr;gap:6px 20px;margin-bottom:16px;",
            checkboxInput("wiz_feat_projections",     "Recruitment projections",        value = TRUE),
            checkboxInput("wiz_feat_questionnaires",  "Patient-completed questionnaires (PROMs)", value = TRUE),
            checkboxInput("wiz_feat_postal",          "Postal tracking tab",            value = FALSE),
            checkboxInput("wiz_feat_returns",         "Return rates tab",               value = FALSE),
            checkboxInput("wiz_feat_pilot",           "Pilot progression criteria",     value = FALSE),
            checkboxInput("wiz_feat_consort",         "CONSORT flow diagram",           value = FALSE),
            checkboxInput("wiz_feat_baseline",        "Baseline characteristics table", value = FALSE)
        ),
        help_text("Uncheck PROMs for trials where patients don't complete questionnaires (e.g. observational, registry, biomarker-only).")
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
