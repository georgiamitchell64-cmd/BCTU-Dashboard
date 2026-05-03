# =============================================================================
# Auto-detect modal: review / amend / apply REDCap field mappings
# =============================================================================
# After a CSV upload, autodetect_redcap() proposes mappings for events,
# fields and instruments. This module renders those proposals in a modal
# with two views (Review and Amend) and merges the chosen values into the
# active trial config in-memory.
# =============================================================================

# Logical roles we present in the UI. Each role label displays nicely and
# corresponds to a detected$fields[[name]] / detected$events[[name]] entry.
.AUTODETECT_FIELD_ROLES <- list(
  list(role = "record_id",              label = "Record ID",              required = TRUE),
  list(role = "site_name",              label = "Site name",              required = TRUE),
  list(role = "randomisation_datetime", label = "Randomisation datetime", required = TRUE),
  list(role = "operation_date",         label = "Operation date",         required = FALSE),
  list(role = "discharge_date",         label = "Discharge date",         required = FALSE),
  list(role = "age",                    label = "Age",                    required = FALSE),
  list(role = "sex",                    label = "Sex",                    required = FALSE),
  list(role = "ethnicity",              label = "Ethnicity",              required = FALSE),
  list(role = "nela_score",             label = "NELA score",             required = FALSE),
  list(role = "cos_type",               label = "Change-of-status type",  required = FALSE)
)

.AUTODETECT_EVENT_ROLES <- list(
  list(role = "baseline",  label = "Baseline",  required = TRUE),
  list(role = "discharge", label = "Discharge", required = FALSE),
  list(role = "day_30",    label = "Day 30",    required = FALSE),
  list(role = "day_90",    label = "Day 90",    required = FALSE)
)

#' Pull the detected value for a logical field role, or NULL.
.autodetect_field <- function(detected, role) {
  if (role == "record_id") return(detected$record_id)
  detected$fields[[role]]
}

#' Pull the detected event for a logical event role, or NULL.
.autodetect_event <- function(detected, role) {
  detected$events[[role]]
}

#' Pretty cell for a detected value (or em-dash if NULL/empty).
.autodetect_value_cell <- function(v) {
  if (is.null(v) || !nzchar(v)) {
    span(style = "color:#94A3B8;font-style:italic;", HTML("&mdash; not found"))
  } else {
    tags$code(style = "background:#EEF3F8;padding:2px 8px;border-radius:4px;
                       color:#1B4F6B;font-size:12px;", v)
  }
}

#' Build the modal contents shown after a successful CSV upload.
#' `detected` = result of autodetect_redcap()
#' `cfg`      = active trial config (used to show what's already mapped)
#' `ns`       = session$ns from the calling module (for input IDs)
autodetect_modal_ui <- function(detected, cfg, ns = identity) {

  csv_cols    <- if (!is.null(detected$record_id) && !is.null(detected$n_cols)) {
    # detected doesn't store the full column list — rebuild from fields/events.
    # The wider list is added by the caller (see upload_server).
    detected$.all_cols %||% character(0)
  } else character(0)

  event_vals <- detected$events$all %||% character(0)

  # ── Header summary ─────────────────────────────────────────────────────────
  n_events_found <- sum(!sapply(.AUTODETECT_EVENT_ROLES, function(r)
    is.null(.autodetect_event(detected, r$role))))
  n_fields_found <- sum(!sapply(.AUTODETECT_FIELD_ROLES, function(r)
    is.null(.autodetect_field(detected, r$role))))
  n_instruments  <- length(detected$instruments %||% character(0))

  summary_pill <- function(n, label) {
    div(style = "display:inline-flex;align-items:center;gap:6px;background:#F1F5F9;
                 padding:6px 12px;border-radius:8px;font-size:12px;color:#1B4F6B;
                 font-weight:600;margin-right:8px;",
        span(style = "font-size:16px;font-weight:700;", n),
        span(label))
  }

  # ── Review view (default) ──────────────────────────────────────────────────
  review_view <- div(id = ns("autodetect_view_review"),
    div(style = "margin-bottom:14px;",
        summary_pill(n_events_found, "events"),
        summary_pill(n_fields_found, "fields"),
        summary_pill(n_instruments,  "instruments")),

    div(style = "font-size:11px;font-weight:600;color:#1B4F6B;
                 text-transform:uppercase;letter-spacing:.5px;margin:14px 0 6px;",
        "REDCap events"),
    .autodetect_review_table(
      lapply(.AUTODETECT_EVENT_ROLES, function(r) list(
        label    = r$label,
        required = r$required,
        value    = .autodetect_event(detected, r$role)))),

    div(style = "font-size:11px;font-weight:600;color:#1B4F6B;
                 text-transform:uppercase;letter-spacing:.5px;margin:18px 0 6px;",
        "Field mappings"),
    .autodetect_review_table(
      lapply(.AUTODETECT_FIELD_ROLES, function(r) list(
        label    = r$label,
        required = r$required,
        value    = .autodetect_field(detected, r$role)))),

    if (n_instruments > 0) tagList(
      div(style = "font-size:11px;font-weight:600;color:#1B4F6B;
                   text-transform:uppercase;letter-spacing:.5px;margin:18px 0 6px;",
          "Detected instruments"),
      div(style = "display:flex;flex-wrap:wrap;gap:6px;",
          lapply(detected$instruments, function(i)
            tags$code(style = "background:#EEF3F8;padding:2px 8px;border-radius:4px;
                               color:#1B4F6B;font-size:12px;", i)))
    )
  )

  # ── Amend view (hidden by default) ─────────────────────────────────────────
  amend_view <- shinyjs::hidden(div(id = ns("autodetect_view_amend"),
    div(style = "background:#FFF7ED;border:1px solid #FED7AA;border-radius:8px;
                 padding:10px 12px;font-size:12px;color:#7C2D12;margin-bottom:14px;",
        HTML("&#9998; Pick the correct column for each role. Leave on “(not used)” to skip.")),

    div(style = "font-size:11px;font-weight:600;color:#1B4F6B;
                 text-transform:uppercase;letter-spacing:.5px;margin:0 0 6px;",
        "REDCap events"),
    .autodetect_amend_grid(.AUTODETECT_EVENT_ROLES,
      values = lapply(.AUTODETECT_EVENT_ROLES, function(r)
        .autodetect_event(detected, r$role)),
      choices_pool = event_vals,
      ns = ns, id_prefix = "autodetect_evt_"),

    div(style = "font-size:11px;font-weight:600;color:#1B4F6B;
                 text-transform:uppercase;letter-spacing:.5px;margin:18px 0 6px;",
        "Field mappings"),
    .autodetect_amend_grid(.AUTODETECT_FIELD_ROLES,
      values = lapply(.AUTODETECT_FIELD_ROLES, function(r)
        .autodetect_field(detected, r$role)),
      choices_pool = csv_cols,
      ns = ns, id_prefix = "autodetect_fld_")
  ))

  tagList(review_view, amend_view)
}

# ── Internal helpers ─────────────────────────────────────────────────────────

# Read-only review row: label | sample-value-pill | required marker
.autodetect_review_table <- function(rows) {
  tags$table(style = "width:100%;border-collapse:collapse;font-size:13px;",
    tags$tbody(lapply(rows, function(r) {
      tags$tr(style = "border-bottom:1px solid #EEF3F8;",
        tags$td(style = "padding:7px 0;width:40%;color:#475569;",
                r$label,
                if (isTRUE(r$required))
                  span(style = "color:#DC2626;margin-left:4px;font-weight:700;", "*")),
        tags$td(style = "padding:7px 0;",
                .autodetect_value_cell(r$value)))
    }))
  )
}

# Editable grid: each role gets a selectInput populated from `choices_pool`.
.autodetect_amend_grid <- function(roles, values, choices_pool, ns, id_prefix) {
  div(style = "display:grid;grid-template-columns:1fr 1fr;gap:10px 16px;",
    Map(function(r, v) {
      sel_choices <- c("(not used)" = "", sort(unique(choices_pool %||% character(0))))
      # If detected value isn't in pool (e.g. role-pattern matched something
      # unusual), inject it so the dropdown still selects it.
      if (!is.null(v) && nzchar(v) && !(v %in% sel_choices)) {
        sel_choices <- c(setNames(v, v), sel_choices)
      }
      div(
        tags$label(style = "font-size:11px;color:#475569;font-weight:600;
                            display:block;margin-bottom:3px;",
                   r$label,
                   if (isTRUE(r$required))
                     span(style = "color:#DC2626;margin-left:3px;", "*")),
        selectInput(ns(paste0(id_prefix, r$role)),
                    label    = NULL,
                    choices  = sel_choices,
                    selected = v %||% "",
                    width    = "100%")
      )
    }, roles, values)
  )
}

#' Augment a trial config with applied autodetect values.
#' Only fills slots that are NULL/empty in the existing config — existing
#' mappings are never overwritten unless `force = TRUE`.
merge_detected_into_config <- function(cfg, applied_fields = list(),
                                       applied_events = list(),
                                       detected_instruments = NULL,
                                       force = FALSE) {
  cfg$redcap_fields <- cfg$redcap_fields %||% list()
  cfg$redcap_events <- cfg$redcap_events %||% list()

  fill <- function(slot, name, value) {
    if (is.null(value) || (is.character(value) && !nzchar(value))) return(slot)
    if (force || is.null(slot[[name]]) ||
        (is.character(slot[[name]]) && !nzchar(slot[[name]]))) {
      slot[[name]] <- value
    }
    slot
  }

  for (n in names(applied_fields))
    cfg$redcap_fields <- fill(cfg$redcap_fields, n, applied_fields[[n]])

  for (n in names(applied_events))
    cfg$redcap_events <- fill(cfg$redcap_events, n, applied_events[[n]])

  # Follow-up instruments: only fill if currently empty.
  fu <- cfg$redcap_fields$follow_up_instruments
  if ((is.null(fu) || length(fu) == 0) && length(detected_instruments) > 0) {
    cfg$redcap_fields$follow_up_instruments <- as.list(detected_instruments)
  }

  cfg
}
