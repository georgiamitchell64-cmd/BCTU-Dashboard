sites_tab_ui <- function() {
  tabPanel("sites",

    # ── Row interactions: left-click to edit, right-click context menu,
    #    live search + status-chip filtering ───────────────────────────────
    tags$script(HTML("
      function siteOpenEdit(id){
        Shiny.setInputValue('site_edit_open', {id:id, n:Math.random()}, {priority:'event'});
      }
      function siteShowCtx(e, id){
        e.preventDefault(); e.stopPropagation();
        var m = document.getElementById('site_ctx');
        if(!m) return;
        m.dataset.id = id;
        var x = Math.min(e.clientX, window.innerWidth - 190);
        var y = Math.min(e.clientY, window.innerHeight - 110);
        m.style.left = x + 'px'; m.style.top = y + 'px';
        m.classList.add('show');
      }
      function siteCtxAction(action){
        var m = document.getElementById('site_ctx'); if(!m) return;
        var id = m.dataset.id; m.classList.remove('show');
        if(action === 'edit') siteOpenEdit(id);
        else Shiny.setInputValue('site_delete_ask', {id:id, n:Math.random()}, {priority:'event'});
      }
      document.addEventListener('click', function(){
        var m = document.getElementById('site_ctx'); if(m) m.classList.remove('show');
      });
      document.addEventListener('contextmenu', function(e){
        if(!e.target.closest('.site-row')){ var m=document.getElementById('site_ctx'); if(m) m.classList.remove('show'); }
      });
      function siteSearch(v){ Shiny.setInputValue('sites_search', v); }
      function siteChip(btn){
        btn.classList.toggle('on');
        var act = [];
        document.querySelectorAll('.sites-toolbar .fchip.on').forEach(function(b){ act.push(b.getAttribute('data-status')); });
        Shiny.setInputValue('sites_status_filter', act);
      }
    ")),

    div(class = "sites-shell",
      tags$section(class = "sites-canvas",

        # ── Toolbar ────────────────────────────────────────────────────
        div(class = "sites-toolbar",
            div(class = "st-left",
                div(class = "searchwrap",
                    span(class = "sw-ic", HTML("&#x2315;")),
                    tags$input(type = "text", class = "searchinput",
                               id = "sites_search_input", oninput = "siteSearch(this.value)",
                               placeholder = "Search sites, cities, codes…")),
                div(class = "fchips",
                    tags$button(class = "fchip", `data-status` = "Recruiting", onclick = "siteChip(this)",
                                span(class = "fchip-dot", style = "background:#10B981;"), "Recruiting"),
                    tags$button(class = "fchip", `data-status` = "Set-up", onclick = "siteChip(this)",
                                span(class = "fchip-dot", style = "background:#94A3B8;"), "Set-up"),
                    tags$button(class = "fchip", `data-status` = "Paused", onclick = "siteChip(this)",
                                span(class = "fchip-dot", style = "background:#F59E0B;"), "Paused"),
                    tags$button(class = "fchip", `data-status` = "Closed", onclick = "siteChip(this)",
                                span(class = "fchip-dot", style = "background:#64748B;"), "Closed")
                )
            ),
            div(class = "st-right",
                actionButton("bulk_add_sites", HTML("&#x2191; Import CSV"),
                             class = "btn-ghost-sm"),
                actionButton("add_site_open", "+ Add site",
                             class = "btn-primary-sm")
            )
        ),

        # ── Summary stats ──────────────────────────────────────────────
        uiOutput("sites_summary_stats"),

        # ── Right-click context menu (shared) ─────────────────────────
        div(id = "site_ctx", class = "site-ctx",
            tags$button(class = "site-ctx-item", type = "button",
                        onclick = "siteCtxAction('edit')",
                        HTML('<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M11 4H4a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7"/><path d="M18.5 2.5a2.12 2.12 0 0 1 3 3L12 15l-4 1 1-4z"/></svg>'),
                        span("Edit site")),
            tags$button(class = "site-ctx-item danger", type = "button",
                        onclick = "siteCtxAction('delete')",
                        HTML('<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M3 6h18"/><path d="M8 6V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2"/><path d="M19 6l-1 14a2 2 0 0 1-2 2H8a2 2 0 0 1-2-2L5 6"/></svg>'),
                        span("Delete site"))
        ),

        # ── Grouped site tables (manual / auto) ────────────────────────
        uiOutput("sites_groups_ui")
      )
    )
  )
}


# ── Add / edit site modal ─────────────────────────────────────────────────────
# row = a one-row data frame of the site being edited, or NULL when adding.
site_edit_modal <- function(row = NULL, is_new = FALSE) {
  g  <- function(f, default = NULL) {
    if (is.null(row) || is.null(row[[f]])) return(default)
    v <- row[[f]]; if (length(v) && is.na(v[1])) return(default); v
  }
  # NA, not NULL: Shiny fills a dateInput with today's date when it is given
  # no initial value, which silently invents an open/SIV date for a new site.
  gd <- function(f) { v <- g(f); if (is.null(v)) NA else tryCatch(as.Date(v), error = function(e) NA) }
  src <- g("source", "manual")

  modalDialog(
    title = div(class = "se-title",
                span(HTML(if (is_new)
                  '<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round"><path d="M12 5v14M5 12h14"/></svg>'
                  else
                  '<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M11 4H4a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7"/><path d="M18.5 2.5a2.12 2.12 0 0 1 3 3L12 15l-4 1 1-4z"/></svg>')),
                span(if (is_new) "Add a site" else (g("site_name", "Edit site"))),
                if (!is_new) span(class = paste0("se-src-badge ", if (identical(src, "manual")) "manual" else "auto"),
                                  if (identical(src, "manual")) "Manually added" else "From REDCap")),
    size = "l", easyClose = TRUE,
    footer = div(class = "se-foot",
      if (!is_new)
        actionButton("site_edit_delete", HTML("&#x1F5D1; Delete site"),
                     class = "btn-danger-sm", style = "margin-right:auto;"),
      modalButton("Cancel"),
      actionButton("site_edit_save", if (is_new) "Add site" else "Save changes",
                   class = "btn btn-primary",
                   style = "background:#1B4F6B;border-color:#1B4F6B;font-weight:600;")
    ),

    div(class = "se-form",
      div(class = "se-group-label", "Identity"),
      div(class = "se-grid",
        div(class = "se-field se-span2",
            tags$label("Site / Trust name"),
            selectizeInput("se_name", label = NULL,
                           choices  = c(stats::setNames(g("site_name", ""), g("site_name", "")), get_hospital_names()),
                           selected = g("site_name", ""), width = "100%",
                           options  = list(create = TRUE, placeholder = "Type or select hospital…",
                                           createOnBlur = TRUE))),
        div(class = "se-field",
            tags$label("Site ID"),
            textInput("se_id", label = NULL, value = g("site_id", ""),
                      placeholder = if (is_new) "Auto-generated" else "", width = "100%"))
      ),
      div(class = "se-grid se-grid3",
        div(class = "se-field", tags$label("City"),
            textInput("se_city", label = NULL, value = g("city", ""), placeholder = "e.g. Leeds", width = "100%")),
        div(class = "se-field", tags$label("Region / State"),
            textInput("se_region", label = NULL, value = g("region", ""), placeholder = "e.g. Yorkshire", width = "100%")),
        div(class = "se-field", tags$label("Country"),
            selectizeInput("se_country", label = NULL, choices = site_countries,
                           selected = g("country", "United Kingdom"),
                           options = list(create = TRUE), width = "100%"))
      ),

      div(class = "se-group-label", "Status & set-up"),
      div(class = "se-grid se-grid3",
        div(class = "se-field", tags$label("Status"),
            selectInput("se_status", label = NULL,
                        choices = c("Identified", "Set-up", "Open", "Recruiting", "Paused", "Closed"),
                        selected = g("status", "Identified"), width = "100%")),
        div(class = "se-field", tags$label("Open date"),
            dateInput("se_open", label = NULL, value = gd("site_open_date"), width = "100%")),
        div(class = "se-field", tags$label("SIV date"),
            dateInput("se_siv_date", label = NULL, value = gd("siv_date"), width = "100%"))
      ),
      div(class = "se-field se-check",
          checkboxInput("se_siv_booked", "Site initiation visit (SIV) booked",
                        value = isTRUE(g("siv_booked", FALSE)))),

      div(class = "se-group-label", "Recruitment"),
      div(class = "se-grid se-grid3",
        div(class = "se-field", tags$label("Monthly target"),
            numericInput("se_mo_tgt", label = NULL, value = as.integer(g("monthly_target", NA_integer_)), min = 0, width = "100%")),
        div(class = "se-field", tags$label("Overall target"),
            numericInput("se_tgt", label = NULL, value = as.integer(g("target", NA_integer_)), min = 0, width = "100%")),
        div(class = "se-field", tags$label("Randomised"),
            numericInput("se_rand", label = NULL, value = as.integer(g("randomised", 0)), min = 0, width = "100%"))
      ),
      div(class = "se-hint",
          "Enter a City and Country to place the site on the map. Non-UK locations are looked up via OpenStreetMap.")
    )
  )
}
