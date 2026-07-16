# =============================================================================
# Modifications tab — UI
# Tracks trial modifications using the new IRAS / HRA classification
# (non-substantial, and substantial categories A / B / C). Persists to
# overrides.json under cfg$modifications.
# =============================================================================

MOD_CATEGORIES <- c(
  "Minor / Non-substantial"            = "non_substantial",
  "Modification of Important Detail"   = "important_detail",
  "Substantial — Category A"           = "substantial_a",
  "Substantial — Category B"           = "substantial_b",
  "Substantial — Category C"           = "substantial_c"
)

MOD_STATUSES <- c("Draft",
                  "Locked for submission",
                  "Submitted",
                  "Under review",
                  "Awaiting further information (RFI)",
                  "Approved",
                  "Approved with conditions",
                  "Implemented",
                  "Rejected / unfavourable",
                  "Withdrawn")

MOD_REVIEW_BODIES <- c(
  "REC (Research Ethics Committee)",
  "HRA / HCRW Approval",
  "MHRA — Medicines (CTIMP)",
  "MHRA — Devices",
  "NHS / HSC R&D",
  "CAG (Confidentiality Advisory Group)",
  "ARSAC (radioactive substances)",
  "HMPPS (prison research)",
  "Devolved nations review"
)

MOD_CHANGE_TYPES <- c(
  "Protocol amendment",
  "Change to participant information / consent",
  "Change to recruitment or eligibility",
  "Change to site list (add/remove sites)",
  "Change to investigator / sponsor personnel",
  "Change to data flows / data items",
  "Change to investigational product",
  "Change to safety reporting",
  "Change to statistical methods / SAP",
  "Administrative / clarification only"
)

MOD_DOCUMENTS <- c(
  "Protocol",
  "Participant Information Sheet (PIS)",
  "Informed Consent Form (ICF)",
  "Investigator Brochure (IB)",
  "GP letter",
  "Patient-facing materials",
  "Site agreement",
  "CRFs / data collection forms",
  "Statistical Analysis Plan",
  "Recruitment materials",
  "Sponsor / funder documents",
  "Modification Tool PDF",
  "Covering letter (MHRA)",
  "Red-lined (tracked) document version",
  "Clean document version",
  "CAG modification form",
  "ARSAC Notice of Substantial Modification",
  "Preliminary Research Assessment (PRA) form",
  "Sponsor declaration / authorisation",
  "Other"
)

modifications_tab_ui <- function() {
  tabPanel("modifications",
    div(class = "md-view",

      # ── Summary strip (total + category chips + pipeline) ────────────────
      uiOutput("mod_summary_strip"),

      # ── Toolbar (search + status filter + ghost CSV + primary +Add) ──────
      div(class = "md-toolbar",
        div(class = "md-search-wrap",
          textInput("mod_search", label = NULL,
                    placeholder = "Search ref or title…", width = "100%")
        ),
        div(class = "md-status-wrap",
          selectInput("mod_status_filter", label = NULL,
                      choices = c("All statuses" = "all",
                                  setNames(MOD_STATUSES, MOD_STATUSES)),
                      selected = "all", width = "100%")
        ),
        div(class = "md-grow"),
        downloadButton("mod_export_csv", HTML("&darr; Export CSV"),
                       class = "md-btn-ghost"),
        actionButton("mod_add_open", HTML("+ Add modification"),
                     class = "md-btn-primary")
      ),

      # ── Table card ───────────────────────────────────────────────────────
      div(class = "md-table-card",
        withSpinner(reactableOutput("mod_table"), type = 4, color = col_teal)
      ),

      # ── Footer note ──────────────────────────────────────────────────────
      div(class = "md-footer",
          "Modifications saved per-trial to overrides.json · feeds the TMG report amendments section · classified per IRAS / HRA framework"),

      # ── Slide-over editor (hidden by default) ────────────────────────────
      shinyjs::hidden(div(id = "mod_editor_scrim", class = "md-editor-scrim",
        # Click outside the panel closes it
        tags$script(HTML("
          (function(){
            var s = document.getElementById('mod_editor_scrim');
            if (!s || s.__bound) return; s.__bound = true;
            s.addEventListener('click', function(e){
              if (e.target === s) {
                Shiny.setInputValue('mod_editor_close', Math.random(), {priority:'event'});
              }
            });
          })();
        ")),
        tags$aside(class = "md-editor",
          tags$header(class = "md-editor-head",
            div(
              div(class = "md-editor-eye",
                  uiOutput("mod_editor_eyebrow", inline = TRUE)),
              div(class = "md-editor-cat",
                  uiOutput("mod_editor_cat", inline = TRUE))
            ),
            actionButton("mod_editor_close_btn", HTML("&times;"),
                         class = "md-editor-close",
                         style = "background:none;border:none;")
          ),
          div(class = "md-editor-body",
            div(style = "font-size:11px;color:var(--ov-muted);margin-bottom:14px;line-height:1.5;",
                HTML("Classify per the <b>IRAS / HRA modification framework</b>.
                      <b>Minor</b> = no notification required.
                      <b>Important Detail</b> = notification to REC/MHRA.
                      <b>Substantial</b> = formal review required (Cat A/B/C).")),

        div(class = "form-grid g2", style = "grid-template-columns:1fr 1fr;",
          div(class = "form-field",
              textInput("mod_ref", "Reference",
                        placeholder = "MOD-001")),
          div(class = "form-field",
              pickerInput("mod_category", "Category",
                          choices = MOD_CATEGORIES,
                          selected = "non_substantial",
                          options = list(`dropup-auto` = FALSE,
                                         container = "body",
                                         `dropdown-align-right` = "auto",
                                         size = 8)))
        ),

        div(class = "form-field",
            textInput("mod_title", "Title",
                      placeholder = "e.g. Protocol v3 — add Glasgow site")),

        div(class = "form-field",
            textAreaInput("mod_description", "Description / rationale",
                          rows = 3,
                          placeholder = "What is changing, and why?")),

        div(class = "form-field",
            pickerInput("mod_change_types", "Change types",
                        choices = MOD_CHANGE_TYPES,
                        multiple = TRUE,
                        options = list(
                          container = "body",
                          `actions-box` = TRUE,
                          `live-search` = TRUE,
                          `selected-text-format` = "count > 2",
                          title = "Up to 10 change types (per Modification Tool)"))),

        div(class = "form-field",
            pickerInput("mod_review_bodies", "Review bodies notified",
                        choices = MOD_REVIEW_BODIES,
                        multiple = TRUE,
                        options = list(
                          container = "body",
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 2",
                          title = "Select review bodies"))),

        div(class = "form-field",
            pickerInput("mod_documents", "Affected documents",
                        choices = MOD_DOCUMENTS,
                        multiple = TRUE,
                        options = list(
                          container = "body",
                          `actions-box` = TRUE,
                          `live-search` = TRUE,
                          `selected-text-format` = "count > 2",
                          title = "Select affected documents"))),

        div(class = "form-grid g2", style = "grid-template-columns:1fr 1fr;",
          div(class = "form-field",
              dateInput("mod_date_prepared", "Date prepared",
                        value = Sys.Date(), format = "dd M yyyy")),
          div(class = "form-field",
              pickerInput("mod_status", "Status",
                          choices = MOD_STATUSES, selected = "Draft",
                          options = list(`dropup-auto` = FALSE,
                                         container = "body",
                                         size = 8)))
        ),

        div(class = "form-grid g2", style = "grid-template-columns:1fr 1fr;",
          div(class = "form-field",
              dateInput("mod_date_submitted", "Submitted to HRA / IRAS",
                        value = NULL, format = "dd M yyyy")),
          div(class = "form-field",
              dateInput("mod_date_rfi", "RFI received (35-day window)",
                        value = NULL, format = "dd M yyyy"))
        ),

        div(class = "form-grid g2", style = "grid-template-columns:1fr 1fr;",
          div(class = "form-field",
              dateInput("mod_date_approved", "Approval received",
                        value = NULL, format = "dd M yyyy")),
          div(class = "form-field",
              textInput("mod_sponsor_authorised_by", "Sponsor authorised by",
                        placeholder = "Name / role"))
        ),

        div(class = "form-grid g2", style = "grid-template-columns:1fr 1fr;",
          div(class = "form-field",
              textInput("mod_iras_ref", "IRAS / HRA reference",
                        placeholder = "e.g. 328678")),
          div(class = "form-field",
              textInput("mod_rec_ref", "REC reference",
                        placeholder = "e.g. 25/0268"))
        ),

        div(class = "form-grid g2", style = "grid-template-columns:1fr 1fr;",
          div(class = "form-field",
              textInput("mod_mhra_ref", "MHRA reference (CTIMP)",
                        placeholder = "optional")),
          div(class = "form-field",
              dateInput("mod_date_implemented", "Implemented at sites",
                        value = NULL, format = "dd M yyyy"))
        ),

        div(class = "form-field",
            textAreaInput("mod_notes", "Notes",
                          rows = 2,
                          placeholder = "Any additional context, conditions on approval, etc."))
          ),  # /md-editor-body

          tags$footer(class = "md-editor-foot",
            actionButton("mod_remove",    HTML("Remove"),       class = "md-btn-danger"),
            actionButton("mod_duplicate", HTML("Duplicate"),    class = "md-btn-ghost"),
            div(class = "md-grow"),
            actionButton("mod_clear",     HTML("Cancel"),       class = "md-btn-ghost"),
            actionButton("mod_save_new",    HTML("+ Add modification"),
                         class = "md-btn-primary",
                         style = "display:none;"),
            actionButton("mod_save_update", HTML("Save changes"),
                         class = "md-btn-primary")
          )
        )  # /md-editor aside
      ))   # /md-editor-scrim hidden
    )
  )
}
