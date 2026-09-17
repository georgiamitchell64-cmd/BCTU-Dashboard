# ─────────────────────────────────────────────────────────────────────────────
# People & access — the admin workspace on the home screen, opened from the
# settings menu (gear, top left). Styles: www/people.css.
# Server: modules/people_server.R.
# ─────────────────────────────────────────────────────────────────────────────

people_tab_ui <- function() {
  div(id = "home_tab_people", style = "display:none;",
    div(class = "pp-root",
      div(class = "pp-head",
          div(div(class = "pp-title", "People & access"),
              div(class = "pp-sub",
                  "Who can use the dashboard, which trials they can open, and whether they can sign in.")),
          tags$button(type = "button", class = "pp-btn pp-btn-primary",
                      onclick = "Shiny.setInputValue('pp_new_user', Math.random(), {priority:'event'})",
                      HTML("&#43; New person"))),

      uiOutput("pp_kpis"),

      div(class = "pp-toolbar",
          div(class = "pp-search",
              tags$input(type = "search", id = "pp_search_box", placeholder = "Search by name, email or job title",
                         `aria-label` = "Search people", autocomplete = "off",
                         oninput = "Shiny.setInputValue('pp_search', this.value)")),
          div(class = "pp-chips", role = "group", `aria-label` = "Show",
              lapply(list(c("all", "Everyone"), c("admins", "Admins"),
                          c("attention", "Awaiting a password"), c("noaccess", "No trial access")), function(f)
                tags$button(type = "button", class = paste("pp-chip", if (f[1] == "all") "on"),
                            `data-f` = f[1], f[2]))),
          div(class = "pp-view", role = "group", `aria-label` = "View",
              tags$button(type = "button", class = "pp-seg on", `data-v` = "people", "People"),
              tags$button(type = "button", class = "pp-seg", `data-v` = "matrix", "Access matrix"))),

      div(id = "pp_view_people", class = "pp-body",
          div(class = "pp-list", uiOutput("pp_list_ui")),
          div(class = "pp-detail", uiOutput("pp_detail_ui"))),

      div(id = "pp_view_matrix", class = "pp-matrix-wrap", style = "display:none;",
          div(class = "pp-matrix-note",
              "Change anyone's access to any trial here. Admins always have full access to every trial."),
          uiOutput("pp_matrix_ui")),

      tags$details(class = "pp-guide",
        tags$summary("What each role can do"),
        div(class = "pp-guide-grid",
            div(tags$b("Admin"), tags$span("Portfolio-wide. Sees every trial, manages people, creates and archives trials, and runs backups.")),
            div(tags$b("Manager"), tags$span("For one trial. Changes its settings, sites, report templates and data uploads, and can download participant data.")),
            div(tags$b("Coordinator, Statistician, Read-only"),
                tags$span("For one trial. Can open its dashboards and generate reports, but can't change settings or download participant data. The three currently have the same permissions; the name records the person's job on the trial.")),
            div(tags$b("No access"), tags$span("The trial doesn't appear for them at all."))))
    ),

    tags$script(HTML("
      function ppFilter(f) {
        $('.pp-chip').removeClass('on');
        $('.pp-chip[data-f=\"' + f + '\"]').addClass('on');
        Shiny.setInputValue('pp_filter', f, {priority: 'event'});
      }
      $(document).on('click', '.pp-chip', function() { ppFilter($(this).data('f')); });
      $(document).on('click', '.pp-seg', function() {
        var v = $(this).data('v');
        $('.pp-seg').removeClass('on'); $(this).addClass('on');
        $('#pp_view_people').toggle(v === 'people');
        $('#pp_view_matrix').toggle(v === 'matrix');
      });
    "))
  )
}
