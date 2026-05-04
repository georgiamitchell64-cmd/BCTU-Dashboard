# =============================================================================
# Report sections registry (Stage 10)
# =============================================================================
# Each section is a list with:
#   id      — stable identifier persisted in templates
#   label   — display name in the builder UI
#   group   — UI grouping ("Header", "Recruitment", "Safety", etc.)
#   render  — function(ctx) returning HTML (character or htmltools tag list)
# `ctx` carries everything a render function might want:
#   list(rv, cfg, report_data, period_label, prepared_by, reviewed_by,
#        meeting_date, custom_text)
#
# Keep render functions defensive — return a small "no data" notice rather
# than throwing when something is missing.
# =============================================================================

# ── Helpers ────────────────────────────────────────────────────────────────
.rs_h <- function(level, text)
  sprintf("<h%d style='font-family:Inter,sans-serif;color:#0F172A;
                       letter-spacing:-0.3px;margin:18px 0 10px;'>%s</h%d>",
          level, htmltools::htmlEscape(text), level)

.rs_subtle <- function(text)
  sprintf("<p style='color:#64748B;font-size:12.5px;margin:4px 0 14px;'>%s</p>",
          htmltools::htmlEscape(text))

.rs_box <- function(content, accent = "#6366F1")
  sprintf("<div style='background:#FFFFFF;border:1px solid #EEF2F7;border-left:3px solid %s;
                       border-radius:10px;padding:14px 18px;margin-bottom:12px;'>%s</div>",
          accent, content)

.rs_kv_grid <- function(pairs) {
  cells <- vapply(seq_along(pairs), function(i) {
    sprintf("<div style='padding:8px 0;border-bottom:1px solid #EEF2F7;'>
              <div style='font-size:10.5px;color:#64748B;font-weight:600;
                          text-transform:uppercase;letter-spacing:.5px;margin-bottom:2px;'>%s</div>
              <div style='font-size:14px;color:#0F172A;font-weight:500;'>%s</div>
            </div>",
            htmltools::htmlEscape(names(pairs)[i]),
            htmltools::htmlEscape(as.character(pairs[[i]])))
  }, character(1))
  sprintf("<div style='display:grid;grid-template-columns:1fr 1fr;gap:10px 24px;'>%s</div>",
          paste(cells, collapse = ""))
}

# ── Section render functions ───────────────────────────────────────────────

.rs_render_header <- function(ctx) {
  cfg <- ctx$cfg
  paste0(
    "<div style='border-bottom:2px solid #6366F1;padding-bottom:14px;margin-bottom:18px;'>",
    sprintf("<div style='font-size:11px;font-weight:600;color:#6366F1;
                         text-transform:uppercase;letter-spacing:.6px;'>%s Report</div>",
            htmltools::htmlEscape(ctx$template_label %||% "Trial")),
    sprintf("<h1 style='font-size:26px;color:#0F172A;margin:6px 0 10px;
                        letter-spacing:-0.5px;'>%s</h1>",
            htmltools::htmlEscape(cfg$short_name %||% "Trial")),
    sprintf("<div style='font-size:13px;color:#475569;margin-bottom:6px;'>%s</div>",
            htmltools::htmlEscape(cfg$name %||% "")),
    .rs_kv_grid(list(
      "Reporting period"   = ctx$period_label %||% "—",
      "Date generated"     = format(Sys.Date(), "%d %B %Y"),
      "Prepared by"        = ctx$prepared_by %||% "—",
      "Reviewed by"        = ctx$reviewed_by %||% "—",
      "Chief Investigator" = cfg$report_defaults$ci %||% "—",
      "Sponsor"            = cfg$report_defaults$sponsor %||% "—"
    )),
    "</div>"
  )
}

.rs_render_recruitment_summary <- function(ctx) {
  rv <- ctx$rv; cfg <- ctx$cfg
  target <- cfg$trial_target %||% 0L

  raw <- rv$raw_redcap
  n_baseline <- 0L
  if (!is.null(raw) && nrow(raw) && "redcap_event_name" %in% names(raw)) {
    bevt <- cfg$redcap_events$baseline %||% "baseline_arm_1"
    id_col <- cfg$redcap_fields$record_id %||% "record_id"
    n_baseline <- length(unique(raw[[id_col]][raw$redcap_event_name == bevt]))
  }
  pct <- if (target > 0) min(1, n_baseline / target) else 0

  paste0(
    .rs_h(2, "Recruitment summary"),
    sprintf("<div style='display:grid;grid-template-columns:repeat(3, 1fr);gap:12px;
                         margin-bottom:14px;'>
              <div style='background:#FFFFFF;border:1px solid #EEF2F7;border-radius:10px;padding:14px;'>
                <div style='font-size:10.5px;color:#64748B;text-transform:uppercase;
                            letter-spacing:.5px;font-weight:600;'>Recruited</div>
                <div style='font-size:26px;color:#0F172A;font-weight:700;'>%d</div>
              </div>
              <div style='background:#FFFFFF;border:1px solid #EEF2F7;border-radius:10px;padding:14px;'>
                <div style='font-size:10.5px;color:#64748B;text-transform:uppercase;
                            letter-spacing:.5px;font-weight:600;'>Target</div>
                <div style='font-size:26px;color:#0F172A;font-weight:700;'>%d</div>
              </div>
              <div style='background:#FFFFFF;border:1px solid #EEF2F7;border-radius:10px;padding:14px;'>
                <div style='font-size:10.5px;color:#64748B;text-transform:uppercase;
                            letter-spacing:.5px;font-weight:600;'>Progress</div>
                <div style='font-size:26px;color:#6366F1;font-weight:700;'>%.0f%%</div>
              </div>
            </div>",
            n_baseline, target, pct * 100)
  )
}

.rs_render_site_summary <- function(ctx) {
  sites <- ctx$rv$sites
  if (is.null(sites) || !nrow(sites)) {
    return(paste0(.rs_h(2, "Sites"),
                  .rs_subtle("No site data available.")))
  }
  rows <- vapply(seq_len(nrow(sites)), function(i) {
    s <- sites[i, ]
    sprintf("<tr>
              <td style='padding:8px 12px;'>%s</td>
              <td style='padding:8px 12px;color:#475569;'>%s</td>
              <td style='padding:8px 12px;text-align:right;font-variant-numeric:tabular-nums;'>%s / %s</td>
            </tr>",
            htmltools::htmlEscape(as.character(s$site_name %||% "—")),
            htmltools::htmlEscape(as.character(s$status %||% "—")),
            as.character(s$randomised %||% 0),
            as.character(s$target %||% "—"))
  }, character(1))
  paste0(
    .rs_h(2, "Sites"),
    "<table style='width:100%;border-collapse:collapse;font-size:13px;
                    background:#FFFFFF;border:1px solid #EEF2F7;border-radius:10px;
                    overflow:hidden;'>",
    "<thead><tr style='background:#FAFBFD;'>
       <th style='text-align:left;padding:10px 12px;font-size:10.5px;color:#64748B;
                  text-transform:uppercase;letter-spacing:.5px;'>Site</th>
       <th style='text-align:left;padding:10px 12px;font-size:10.5px;color:#64748B;
                  text-transform:uppercase;letter-spacing:.5px;'>Status</th>
       <th style='text-align:right;padding:10px 12px;font-size:10.5px;color:#64748B;
                  text-transform:uppercase;letter-spacing:.5px;'>Randomised</th>
     </tr></thead>",
    "<tbody>", paste(rows, collapse = ""), "</tbody></table>"
  )
}

.rs_render_demographics <- function(ctx) {
  raw <- ctx$rv$raw_redcap; cfg <- ctx$cfg
  if (is.null(raw) || !nrow(raw))
    return(paste0(.rs_h(2, "Demographics"),
                  .rs_subtle("No data available.")))
  cols <- cfg$participant_breakdowns
  if (is.null(cols) || !length(cols)) {
    det <- detect_breakdown_columns(raw, cfg)
    cols <- default_breakdown_cols(det)
  }
  if (!length(cols)) return("")

  blocks <- lapply(cols, function(c) {
    bd <- compute_breakdown(raw, c, cfg)
    if (is.null(bd)) return("")
    seg_rows <- vapply(bd$segments, function(s) {
      pct_w <- max(0, min(1, s$pct)) * 100
      sprintf("<tr>
                <td style='padding:5px 0;font-size:12px;'>%s</td>
                <td style='padding:5px 0;text-align:right;font-size:12px;
                           color:#475569;font-variant-numeric:tabular-nums;'>%d (%.0f%%)</td>
              </tr>",
              htmltools::htmlEscape(s$label), s$n, pct_w)
    }, character(1))
    sprintf("<div style='background:#FFFFFF;border:1px solid #EEF2F7;border-radius:10px;
                         padding:14px 16px;'>
              <div style='font-weight:600;color:#0F172A;font-size:13.5px;margin-bottom:4px;'>%s</div>
              <div style='font-size:11.5px;color:#64748B;margin-bottom:10px;'>%s</div>
              <table style='width:100%%;border-collapse:collapse;'><tbody>%s</tbody></table>
            </div>",
            htmltools::htmlEscape(bd$label),
            htmltools::htmlEscape(bd$headline),
            paste(seg_rows, collapse = ""))
  })

  paste0(
    .rs_h(2, "Demographics"),
    sprintf("<div style='display:grid;grid-template-columns:repeat(auto-fill, minmax(280px, 1fr));
                         gap:12px;'>%s</div>",
            paste(blocks, collapse = ""))
  )
}

.rs_render_smart_insights <- function(ctx) {
  cfg <- ctx$cfg
  insights <- tryCatch(
    compute_insights(ctx$rv$raw_redcap, ctx$rv$sites, cfg),
    error = function(e) list())
  if (!length(insights))
    return(paste0(.rs_h(2, "Smart insights"),
                  .rs_subtle("No insights available.")))

  blocks <- vapply(insights, function(i) {
    accent <- switch(i$severity,
                     alert   = "#B91C1C",
                     warning = "#B45309",
                     info    = "#15803D")
    .rs_box(sprintf("<div style='font-weight:600;color:#0F172A;font-size:13.5px;
                                  margin-bottom:4px;'>%s</div>
                      <div style='font-size:12.5px;color:#475569;line-height:1.5;'>%s</div>%s",
                    htmltools::htmlEscape(i$title),
                    htmltools::htmlEscape(i$body),
                    if (!is.null(i$value))
                      sprintf("<div style='margin-top:6px;font-size:12px;color:%s;
                                            font-weight:700;'>%s</div>",
                              accent, htmltools::htmlEscape(i$value)) else ""),
            accent)
  }, character(1))

  paste0(.rs_h(2, "Smart insights"), paste(blocks, collapse = ""))
}

.rs_render_safety_summary <- function(ctx) {
  raw <- ctx$rv$raw_redcap
  if (is.null(raw) || !nrow(raw))
    return(paste0(.rs_h(2, "Safety & regulatory"),
                  .rs_subtle("No data available.")))

  # Best-effort counters using common REDCap column patterns.
  count_col <- function(pat) {
    m <- grep(pat, names(raw), ignore.case = TRUE, value = TRUE)
    if (!length(m)) return(NA_integer_)
    suppressWarnings(sum(!is.na(raw[[m[1]]]) & raw[[m[1]]] != "" &
                         raw[[m[1]]] != "0"))
  }
  n_sae <- count_col("^sae_")
  n_dev <- count_col("deviation|protocol_dev")
  n_wd  <- count_col("withdraw|^cos_type$")

  pairs <- list()
  if (!is.na(n_sae)) pairs[["Serious adverse events"]] <- n_sae
  if (!is.na(n_dev)) pairs[["Protocol deviations"]]    <- n_dev
  if (!is.na(n_wd))  pairs[["Withdrawals (COS)"]]      <- n_wd

  if (!length(pairs))
    return(paste0(.rs_h(2, "Safety & regulatory"),
                  .rs_subtle("No safety columns detected in the export.")))

  paste0(.rs_h(2, "Safety & regulatory"),
         .rs_box(.rs_kv_grid(pairs), accent = "#F59E0B"))
}

.rs_render_amendments <- function(ctx) {
  cfg <- ctx$cfg
  items <- cfg$amendments
  if (is.null(items) || !length(items)) {
    return(paste0(
      .rs_h(2, "Amendments"),
      .rs_subtle("No amendments tracked. Add them via the Amendments card on the Reports tab.")))
  }

  rows <- vapply(items, function(a) {
    accent <- if (identical(a$type, "Substantial")) "#B91C1C" else "#6366F1"
    sprintf("<tr>
              <td style='padding:9px 12px;font-weight:500;'>%s</td>
              <td style='padding:9px 12px;color:%s;font-size:11.5px;
                          font-weight:600;text-transform:uppercase;letter-spacing:.4px;'>%s</td>
              <td style='padding:9px 12px;color:#475569;'>%s</td>
              <td style='padding:9px 12px;color:#475569;'>%s</td>
              <td style='padding:9px 12px;color:#0F172A;line-height:1.5;'>%s</td>
            </tr>",
            htmltools::htmlEscape(as.character(a$ref %||% "—")),
            accent,
            htmltools::htmlEscape(as.character(a$type %||% "—")),
            htmltools::htmlEscape(as.character(a$date %||% "—")),
            htmltools::htmlEscape(as.character(a$status %||% "—")),
            htmltools::htmlEscape(as.character(a$description %||% "")))
  }, character(1))

  paste0(
    .rs_h(2, "Amendments"),
    "<table style='width:100%;border-collapse:collapse;font-size:13px;
                    background:#FFFFFF;border:1px solid #EEF2F7;border-radius:10px;
                    overflow:hidden;'>",
    "<thead><tr style='background:#FAFBFD;'>",
    "<th style='text-align:left;padding:10px 12px;font-size:10.5px;color:#64748B;
                text-transform:uppercase;letter-spacing:.5px;'>Reference</th>",
    "<th style='text-align:left;padding:10px 12px;font-size:10.5px;color:#64748B;
                text-transform:uppercase;letter-spacing:.5px;'>Type</th>",
    "<th style='text-align:left;padding:10px 12px;font-size:10.5px;color:#64748B;
                text-transform:uppercase;letter-spacing:.5px;'>Date</th>",
    "<th style='text-align:left;padding:10px 12px;font-size:10.5px;color:#64748B;
                text-transform:uppercase;letter-spacing:.5px;'>Status</th>",
    "<th style='text-align:left;padding:10px 12px;font-size:10.5px;color:#64748B;
                text-transform:uppercase;letter-spacing:.5px;'>Description</th>",
    "</tr></thead>",
    "<tbody>", paste(rows, collapse = ""), "</tbody></table>"
  )
}

.rs_render_next_period <- function(ctx) {
  paste0(
    .rs_h(2, "Plans for next reporting period"),
    .rs_box(
      if (!is.null(ctx$next_period_text) && nzchar(ctx$next_period_text))
        sprintf("<p style='margin:0;font-size:13px;line-height:1.7;color:#0F172A;'>%s</p>",
                htmltools::htmlEscape(ctx$next_period_text))
      else
        "<em style='color:#94A3B8;'>Add plans via the report builder before generating.</em>",
      "#10B981"))
}

.rs_render_custom_text <- function(ctx) {
  txt <- ctx$custom_text
  if (is.null(txt) || !nzchar(txt)) return("")
  paste0(.rs_h(2, "Notes"),
         .rs_box(sprintf("<p style='margin:0;font-size:13px;line-height:1.7;color:#0F172A;
                                     white-space:pre-wrap;'>%s</p>",
                          htmltools::htmlEscape(txt))))
}

# ── Section registry ───────────────────────────────────────────────────────
REPORT_SECTIONS <- list(
  list(id = "header",              label = "Header & metadata",
       group = "Cover",             render = .rs_render_header),
  list(id = "recruitment_summary", label = "Recruitment summary",
       group = "Recruitment",       render = .rs_render_recruitment_summary),
  list(id = "smart_insights",      label = "Smart insights",
       group = "Recruitment",       render = .rs_render_smart_insights),
  list(id = "site_summary",        label = "Site summary",
       group = "Sites",             render = .rs_render_site_summary),
  list(id = "demographics",        label = "Demographics breakdown",
       group = "Participants",      render = .rs_render_demographics),
  list(id = "safety_summary",      label = "Safety & regulatory",
       group = "Safety",            render = .rs_render_safety_summary),
  list(id = "amendments",          label = "Amendments",
       group = "Regulatory",        render = .rs_render_amendments),
  list(id = "custom_text",         label = "Custom text / notes",
       group = "Free text",         render = .rs_render_custom_text),
  list(id = "next_period",         label = "Plans for next period",
       group = "Free text",         render = .rs_render_next_period)
)

report_section_by_id <- function(id) {
  for (s in REPORT_SECTIONS) if (identical(s$id, id)) return(s)
  NULL
}

# ── Default templates ──────────────────────────────────────────────────────
REPORT_TEMPLATES <- list(
  TMG = list(
    label = "TMG (Trial Management Group)",
    description = "Internal management report — recruitment, sites, insights.",
    sections = c("header", "recruitment_summary", "smart_insights",
                 "site_summary", "safety_summary", "custom_text")
  ),
  TSC = list(
    label = "TSC (Trial Steering Committee)",
    description = "External oversight — recruitment, demographics, safety, amendments.",
    sections = c("header", "recruitment_summary", "demographics",
                 "site_summary", "safety_summary", "amendments",
                 "next_period", "custom_text")
  ),
  NIHR = list(
    label = "NIHR funder update",
    description = "Concise sponsor/funder report — recruitment vs target, plans.",
    sections = c("header", "recruitment_summary", "site_summary",
                 "amendments", "next_period")
  )
)
