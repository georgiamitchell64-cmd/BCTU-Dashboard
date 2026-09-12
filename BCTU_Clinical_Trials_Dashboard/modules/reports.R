# ─────────────────────────────────────────────────────────────────────────────
# Reports — report builder in two panels:
#   set up and generate (left) · live preview (right). Reports always cover
#   the whole trial, so there is no reporting-period step.
# Styles: www/reports.css. Server: modules/reports_server.R.
# ─────────────────────────────────────────────────────────────────────────────

.rb_step <- function(n, title, ..., note = NULL, id = NULL)
  div(class = "rb-step", id = id,
      div(class = "rb-step-h", span(class = "rb-step-n", sprintf("%02d", n)), span(class = "rb-step-t", title)),
      ...,
      if (!is.null(note)) div(class = "rb-step-note", note))

reports_tab_ui <- function() {
  tabPanel("reports",
    # Fonts used inside the document preview pages
    tags$link(href = paste0("https://fonts.googleapis.com/css2",
                            "?family=Inter:wght@400;500;600;700",
                            "&family=Source+Serif+4:wght@400;600;700",
                            "&family=JetBrains+Mono:wght@400;500&display=swap"),
              rel = "stylesheet"),

    div(class = "rb-app",
      div(class = "rb-work3",

        # ═══ Set up ═══════════════════════════════════════════════════════
        tags$aside(class = "rb-setup", `aria-label` = "Report set-up",
          uiOutput("rb_trial_card"),

          .rb_step(1, "Report type", uiOutput("rb_type_cards")),

          .rb_step(2, "Generate",
            div(class = "rb-ready", uiOutput("rb_readiness")),
            uiOutput("rb_format_ui"),
            downloadButton("rb_download", "Generate report", class = "rb-generate"),
            uiOutput("rb_gen_note")),

          .rb_step(3, "Options", id = "rb_opts_step",
            checkboxInput("include_withdrawn", "Include withdrawn participants", FALSE),
            div(id = "rb_opt_appendix",
                checkboxInput("report_appendix", "Add the data appendix", FALSE)),
            radioButtons("completeness_style", "CRF completeness shown as",
                         c("Heatmap" = "heatmap", "List" = "flat", "Both" = "both"), "heatmap", inline = TRUE)),

          .rb_step(4, "People",
            textInput("prepared_by", "Prepared by", ""),
            textInput("reviewed_by", "Reviewed by", "")),

          .rb_step(5, "Content", uiOutput("rb_content_ui")),

          .rb_step(6, "Recent reports", uiOutput("rb_recent_ui"))
        ),

        # ═══ Preview ══════════════════════════════════════════════════════
        div(class = "rb-canvas-wrap",
          tags$main(class = "rb-canvas",
            div(class = "rb-toolbar",
                div(class = "rb-tb-titles",
                    div(class = "rb-tb-title", textOutput("rb_canvas_title", inline = TRUE)),
                    div(class = "rb-tb-sub", textOutput("rb_canvas_meta", inline = TRUE))),
                div(class = "rb-tb-spacer"),
                uiOutput("rb_preview_status", inline = TRUE),
                div(class = "rb-zoom",
                    tags$button(class = "rb-tb-btn", type = "button", `aria-label` = "Zoom out",
                                onclick = "rbZoom(-0.1)", HTML("&minus;")),
                    span(id = "rb_zoom_val", "100%"),
                    tags$button(class = "rb-tb-btn", type = "button", `aria-label` = "Zoom in",
                                onclick = "rbZoom(0.1)", "+")),
                tags$button(class = "rb-tb-btn", type = "button", onclick = "rbPrint()", "Print")),
            div(class = "rb-pages", uiOutput("rb_document_preview"))),

          # Slide-over editor, opened from Set up → Content
          tags$aside(id = "rb_panel", class = "rb-panel", `aria-label` = "Report content editor",
            div(class = "rb-panel-head",
                tags$h3(id = "rb_panel_title", "Sections"),
                tags$button(id = "rb_panel_close", class = "rb-panel-close", type = "button",
                            `aria-label` = "Close editor", HTML("&times;"))),
            div(class = "rb-panel-body", uiOutput("rb_builder_body")))
        )
      )
    ),

    tags$script(HTML("
      function rbZoom(d) {
        var p = $('.rb-pages');
        var z = Math.min(1.5, Math.max(0.3, (parseFloat(p.data('zoom')) || 1) + d));
        p.data('zoom', z).css({ transform: 'scale(' + z + ')', 'transform-origin': 'top center' });
        $('#rb_zoom_val').text(Math.round(z * 100) + '%');
      }
      // Print the report itself (the preview frame) rather than the whole app
      function rbPrint() {
        var f = document.querySelector('#rb_document_preview iframe');
        if (f && f.contentWindow) { f.contentWindow.focus(); f.contentWindow.print(); }
        else window.print();
      }
      var RB_PANEL_TITLES = { sections: 'Sections', narrative: 'Narrative',
                              amend: 'Amendments', portfolio: 'Portfolio review' };
      $(document).on('click', '.rb-btab', function() {
        var key = this.id.replace('rb_btab_', '');
        var open = !$(this).hasClass('active');
        $('.rb-btab').removeClass('active');
        $('#rb_panel').toggleClass('open', open);
        if (open) { $(this).addClass('active'); $('#rb_panel_title').text(RB_PANEL_TITLES[key] || key); }
        Shiny.setInputValue('rb_active_btab', open ? key : null, { priority: 'event' });
      });
      $(document).on('click', '#rb_panel_close', function() {
        $('.rb-btab').removeClass('active');
        $('#rb_panel').removeClass('open');
        Shiny.setInputValue('rb_active_btab', null, { priority: 'event' });
      });
      // Jump to Settings → Reports & admin (report text and templates)
      function rbOpenReportSettings() {
        Shiny.setInputValue('go_settings', Math.random(), { priority: 'event' });
        if (window.setActiveTab) setActiveTab('tn_settings');
        setTimeout(function() { $('.settings-item[data-section=reports]').trigger('click'); }, 400);
      }
    "))
  )
}
