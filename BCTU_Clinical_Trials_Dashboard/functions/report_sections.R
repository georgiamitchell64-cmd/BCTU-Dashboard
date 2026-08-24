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
  # Embed the trial logo as a data URI so the downloaded report is self-contained.
  logo_html <- ""
  lf <- cfg$logo_file
  if (!is.null(lf) && nzchar(lf) && file.exists(lf)) {
    uri <- tryCatch(knitr::image_uri(lf), error = function(e) NULL)
    if (!is.null(uri) && nzchar(uri))
      logo_html <- sprintf("<img src='%s' alt='' style='max-height:56px;max-width:220px;
                            object-fit:contain;display:block;margin-bottom:12px;'/>", uri)
  }
  paste0(
    "<div style='border-bottom:2px solid #6366F1;padding-bottom:14px;margin-bottom:18px;'>",
    logo_html,
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

.rs_render_consort <- function(ctx) {
  cfg <- ctx$cfg
  raw <- ctx$rv$raw_redcap
  if (is.null(raw) || !nrow(raw))
    return(paste0(.rs_h(2, "CONSORT flow diagram"),
                  .rs_subtle("No participant data available.")))
  counts <- tryCatch(consort_counts_live(raw, cfg), error = function(e) NULL)
  if (is.null(counts))
    return(paste0(.rs_h(2, "CONSORT flow diagram"),
                  .rs_subtle("CONSORT diagram unavailable.")))
  paste0(.rs_h(2, "CONSORT flow diagram"), consort_html(counts, cfg))
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

# Render an events tibble (from safety_events.R) as a report table — the
# standard Participant/Site/Type columns plus any mapped extra (x__) columns
# and the reason/notes narrative.
.rs_events_table <- function(df, title = "") {
  if (is.null(df) || !nrow(df)) return("")
  xcols    <- grep("^x__", names(df), value = TRUE)
  is_event <- "term" %in% names(df)
  show_reason <- "narrative" %in% names(df) &&
    any(!is.na(df$narrative) & nzchar(trimws(df$narrative)))
  heads <- c("Participant", "Site",
             if (is_event) "Type", sub("^x__", "", xcols),
             if (show_reason) "Reason / notes")
  th <- paste0("<tr>", paste(sprintf("<th>%s</th>", htmltools::htmlEscape(heads)),
                             collapse = ""), "</tr>")
  body <- vapply(seq_len(nrow(df)), function(i) {
    r    <- df[i, ]
    vals <- c(as.character(r$record_id), as.character(r$site %||% ""),
              if (is_event) as.character(r$term %||% ""),
              vapply(xcols, function(c) as.character(r[[c]] %||% ""), character(1)),
              if (show_reason) as.character(r$narrative %||% ""))
    vals <- ifelse(is.na(vals) | !nzchar(trimws(vals)), "—", vals)
    paste0("<tr>", paste(sprintf("<td>%s</td>", htmltools::htmlEscape(vals)),
                         collapse = ""), "</tr>")
  }, character(1))
  paste0(if (nzchar(title)) .rs_h(3, title) else "",
         "<table>", th, paste(body, collapse = ""), "</table>")
}

.rs_render_complications <- function(ctx) {
  df <- tryCatch(complication_events(ctx$rv$raw_redcap), error = function(e) NULL)
  if (is.null(df) || !nrow(df))
    return(paste0(.rs_h(2, "Complications"),
                  .rs_subtle("No complications recorded, or no complication columns mapped in Trial Settings.")))
  paste0(.rs_h(2, "Complications"),
         .rs_subtle(sprintf("%d participant%s with a recorded complication.",
                            nrow(df), if (nrow(df) == 1) "" else "s")),
         .rs_events_table(df, ""))
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

  # Detailed SAE + withdrawal tables (with reason and any mapped extra columns
  # — death/causality/expectedness, cos/reason, etc.).
  sae_tbl <- tryCatch(.rs_events_table(sae_events(raw), "Serious adverse events"),
                      error = function(e) "")
  wd_tbl  <- tryCatch(.rs_events_table(withdrawal_events(raw),
                                       "Withdrawals / change of status"),
                      error = function(e) "")

  paste0(.rs_h(2, "Safety & regulatory"),
         .rs_box(.rs_kv_grid(pairs), accent = "#F59E0B"),
         sae_tbl, wd_tbl)
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

# ── Portfolio review (BCTU Trial Update Summary v4.0) ─────────────────────
# Pulls fixed metadata from cfg$portfolio_review (set on Settings → Portfolio
# review) and variable meeting dates / RAG / narrative from the report builder
# (set on Reports → Portfolio panel, threaded through ctx$portfolio).

.pr_pick <- function(ctx, key, fallback = "—") {
  pr <- ctx$cfg$portfolio_review %||% list()
  v  <- pr[[key]]
  if (is.null(v) || !nzchar(as.character(v))) return(fallback)
  as.character(v)
}
.pr_var <- function(ctx, key, fallback = "—") {
  v <- (ctx$portfolio %||% list())[[key]]
  if (is.null(v) || !nzchar(as.character(v))) return(fallback)
  as.character(v)
}
.pr_table <- function(rows) {
  cells <- vapply(rows, function(r) {
    sprintf("<tr>
              <td style='padding:8px 12px;background:#F8FAFC;width:42%%;
                         font-size:11.5px;font-weight:600;color:#0F172A;
                         border:1px solid #E2E8EE;'>%s</td>
              <td style='padding:8px 12px;font-size:12px;color:#0F172A;
                         border:1px solid #E2E8EE;'>%s</td>
            </tr>",
            htmltools::htmlEscape(r[[1]]),
            htmltools::htmlEscape(r[[2]]))
  }, character(1))
  paste0("<table style='width:100%;border-collapse:collapse;margin:6px 0 14px;'>",
         "<tbody>", paste(cells, collapse = ""), "</tbody></table>")
}

.pr_info_row <- function(label, value) {
  sprintf(
    "<div class='pf-info-row'>
       <div class='pf-info-cell label'>%s</div>
       <div class='pf-info-cell value'>%s</div>
     </div>",
    htmltools::htmlEscape(label),
    htmltools::htmlEscape(value %||% "—"))
}

.pr_checkbox <- function(checked, size = "") {
  cls <- paste("pf-checkbox", size, if (isTRUE(checked)) "checked" else "")
  sprintf("<span class='%s'>%s</span>",
          trimws(gsub("\\s+", " ", cls)),
          if (isTRUE(checked)) "&#10003;" else "")
}

.rs_render_pr_trial_summary <- function(ctx) {
  cfg <- ctx$cfg
  left_rows <- list(
    list("Trial Name (acronym):",
         paste0(cfg$name %||% cfg$short_name %||% "—",
                " (", toupper(cfg$short_name %||% cfg$code %||% "—"), ")")),
    list("CI:",           cfg$report_defaults$ci %||% .pr_pick(ctx, "ci")),
    list("Trial Type:",   .pr_pick(ctx, "trial_type")),
    list("Sponsor:",      cfg$report_defaults$sponsor %||% .pr_pick(ctx, "sponsor")),
    list("Grant start date:", .pr_pick(ctx, "grant_start")),
    list("Sample size:",  as.character(cfg$trial_target %||% .pr_pick(ctx, "sample_size")))
  )
  right_rows <- list(
    list("Team Leader:",     .pr_pick(ctx, "team_leader")),
    list("Funder:",          .pr_pick(ctx, "funder")),
    list("Intervention:",    .pr_pick(ctx, "intervention")),
    list("Trial Coordinator:", .pr_pick(ctx, "coordinator")),
    list("Grant end date:",  .pr_pick(ctx, "grant_end")),
    list("Date first patient recruited:", .pr_pick(ctx, "first_patient_date"))
  )
  rows_html <- function(rows)
    paste(vapply(rows, function(r) .pr_info_row(r[[1]], r[[2]]),
                 character(1)), collapse = "")

  header_block <- sprintf(
    "<div class='pf-header-block'>
       <div class='pf-banner'>Trial Update Summary</div>
       <div class='pf-info-grid'>%s</div>
       <div class='pf-info-grid'>%s</div>
     </div>",
    rows_html(left_rows), rows_html(right_rows))

  stage <- .pr_var(ctx, "stage", .pr_pick(ctx, "stage", "Recruiting"))
  statuses <- c("In set-up", "Recruiting", "In Follow-up", "Analysis")
  status_items <- vapply(statuses, function(s) {
    sprintf("<label class='pf-check-item'>%s<span>%s</span></label>",
            .pr_checkbox(identical(tolower(stage), tolower(s))),
            htmltools::htmlEscape(s))
  }, character(1))
  status_row <- sprintf("<div class='pf-status-row'>%s</div>",
                        paste(status_items, collapse = ""))

  summary_txt <- .pr_pick(ctx, "brief_summary", "")
  summary_html <- if (nzchar(summary_txt) && summary_txt != "—")
    htmltools::htmlEscape(summary_txt)
  else
    "<em style='color:#94A3B8;'>Add a brief summary on Settings &rarr; Portfolio review.</em>"
  summary_block <- sprintf(
    "<div class='pf-summary'>
       <div class='pf-summary-label'>Brief summary of trial:</div>
       <div class='pf-summary-text'>%s</div>
     </div>", summary_html)

  paste0(header_block, status_row, summary_block)
}

.pr_yn_row <- function(label, yes, date_val = NULL, na_opt = FALSE, na_val = FALSE) {
  is_yes <- isTRUE(yes)
  is_na  <- isTRUE(na_val)
  is_no  <- !is_yes && !is_na
  date_html <- if (!is.null(date_val))
    sprintf("<span class='pf-yn-date'>Date: <strong>%s</strong></span>",
            htmltools::htmlEscape(date_val %||% "—")) else ""
  na_html <- if (isTRUE(na_opt))
    sprintf("<label class='pf-check-item small'>%s<span>N/A</span></label>",
            .pr_checkbox(is_na, "sm")) else ""
  sprintf(
    "<div class='pf-yn-row'>
       <div class='pf-yn-label'>%s</div>
       <div class='pf-yn-answer'>
         <label class='pf-check-item small'>%s<span>No</span></label>
         <label class='pf-check-item small'>%s<span>Yes</span></label>
         %s%s
       </div>
     </div>",
    htmltools::htmlEscape(label),
    .pr_checkbox(is_no,  "sm"),
    .pr_checkbox(is_yes, "sm"),
    na_html, date_html)
}

.pr_meeting_row <- function(label, prev, nxt) {
  sprintf(
    "<div class='pf-meeting-row'>
       <div class='pf-meeting-label'>%s</div>
       <div class='pf-meeting-dates'>
         <span>Previous: <strong>%s</strong></span>
         <span>Next: <strong>%s</strong></span>
       </div>
     </div>",
    htmltools::htmlEscape(label),
    htmltools::htmlEscape(prev %||% "—"),
    htmltools::htmlEscape(nxt %||% "—"))
}

.pr_truthy <- function(x) {
  if (is.null(x)) return(FALSE)
  if (is.logical(x)) return(isTRUE(x))
  s <- tolower(as.character(x))
  nzchar(s) && !s %in% c("no", "n", "false", "0", "—", "-", "n/a", "na")
}

.rs_render_pr_review_progress <- function(ctx) {
  approvals_yes <- .pr_truthy(.pr_pick(ctx, "approvals_in_place", NULL)) ||
                   nzchar(.pr_pick(ctx, "approvals_date", "")) &&
                   .pr_pick(ctx, "approvals_date") != "—"
  open_yes <- .pr_truthy(.pr_pick(ctx, "open_to_recruitment", NULL)) ||
              .pr_pick(ctx, "open_recruitment_date") != "—"
  first_yes <- .pr_pick(ctx, "first_patient_date") != "—"
  pilot_val  <- .pr_pick(ctx, "pilot_phase_ended", "")
  pilot_yes  <- tolower(pilot_val) %in% c("yes", "true", "y")
  pilot_na   <- tolower(pilot_val) %in% c("n/a", "na", "not applicable")

  rows <- paste0(
    .pr_yn_row("All approvals in place", approvals_yes,
               .pr_pick(ctx, "approvals_date")),
    .pr_yn_row("Open to recruitment", open_yes,
               .pr_pick(ctx, "open_recruitment_date")),
    .pr_yn_row("First patient recruited", first_yes,
               .pr_pick(ctx, "first_patient_date")),
    .pr_yn_row("Pilot phase ended", pilot_yes,
               na_opt = TRUE, na_val = pilot_na),
    "<div class='pf-divider'></div>",
    .pr_meeting_row("TMG meeting dates",
                    .pr_var(ctx, "tmg_prev"), .pr_var(ctx, "tmg_next")),
    .pr_meeting_row("TSC meeting dates",
                    .pr_var(ctx, "tsc_prev"), .pr_var(ctx, "tsc_next")),
    .pr_meeting_row("DMEC meeting dates",
                    .pr_var(ctx, "dmc_prev"), .pr_var(ctx, "dmc_next")),
    "<div class='pf-divider'></div>",
    sprintf(
      "<div class='pf-further-row'>
         <div class='pf-further-label'>Further information</div>
         <div class='pf-further-text'>%s</div>
       </div>",
      htmltools::htmlEscape(.pr_var(ctx, "further_info", "—"))))

  sprintf(
    "<div class='pf-section'>
       <div class='pf-section-banner'>Review of Progress</div>
       <div class='pf-progress-grid'>%s</div>
     </div>", rows)
}

.rs_render_pr_rag_status <- function(ctx) {
  rag <- tolower(.pr_var(ctx, "rag_status", ""))
  rag_defs <- list(
    list(key = "red",   bg = "#FEE2E2", border = "#EF4444", text = "#991B1B",
         label = "Red — Significantly behind schedule; delivery at risk"),
    list(key = "amber", bg = "#FEF3C7", border = "#F59E0B", text = "#92400E",
         label = "Amber — Behind schedule; areas of concern identified"),
    list(key = "green", bg = "#D1FAE5", border = "#10B981", text = "#065F46",
         label = "Green — On track; no major problems identified"))

  items <- vapply(rag_defs, function(d) {
    active <- identical(rag, d$key)
    item_style <- if (active)
      sprintf("background:%s;border-left:3px solid %s;", d$bg, d$border) else ""
    dot_style <- sprintf("background:%s;", if (active) d$border else "var(--rb-line, #E2E8EE)")
    text_style <- if (active)
      sprintf("color:%s;font-weight:600;", d$text) else ""
    sprintf(
      "<div class='pf-rag-item%s' style='%s'>
         <div class='pf-rag-dot' style='%s'></div>
         <div class='pf-rag-text' style='%s'>%s</div>
       </div>",
      if (active) " active" else "",
      item_style, dot_style, text_style,
      htmltools::htmlEscape(d$label))
  }, character(1))

  note <- .pr_var(ctx, "rag_note", "")
  note_html <- if (nzchar(note) && note != "—")
    sprintf("<div style='margin-top:8px;padding:8px 10px;border:1px solid #E2E8EE;
                          border-radius:4px;background:#FFFFFF;font-size:10.5px;
                          color:#27384A;line-height:1.5;'>%s</div>",
            htmltools::htmlEscape(note)) else ""

  sprintf(
    "<div class='pf-section'>
       <div class='pf-section-banner'>RAG Status</div>
       <div class='pf-rag-grid'>%s</div>
       %s
     </div>",
    paste(items, collapse = ""), note_html)
}

.rs_render_pr_recruitment <- function(ctx) {
  cfg <- ctx$cfg
  rd  <- ctx$report_data %||% list()
  recruited <- rd$recruited %||% .pr_pick(ctx, "recruited", NA)
  target    <- cfg$trial_target %||% .pr_pick(ctx, "sample_size", NA)
  sites_open  <- rd$sites_open  %||% .pr_pick(ctx, "sites_open",  NA)
  sites_total <- rd$sites_total %||% .pr_pick(ctx, "sites_total", NA)

  fmt_int <- function(x) {
    if (is.null(x) || is.na(x) || !nzchar(as.character(x)) ||
        as.character(x) == "—") return("—")
    n <- suppressWarnings(as.numeric(x))
    if (is.na(n)) as.character(x) else format(n, big.mark = ",")
  }
  pct <- if (is.numeric(recruited) && is.numeric(target) && target > 0)
    sprintf("%d%%", round(100 * recruited / target)) else "—"

  stats <- sprintf(
    "<div class='pf-recruit-stats'>
       <div class='pf-recruit-stat'><span class='k'>Randomised to date</span><span class='v'>%s</span></div>
       <div class='pf-recruit-stat'><span class='k'>Target</span><span class='v'>%s</span></div>
       <div class='pf-recruit-stat'><span class='k'>%% of target</span><span class='v'>%s</span></div>
       <div class='pf-recruit-stat'><span class='k'>Sites open</span><span class='v'>%s / %s</span></div>
     </div>",
    fmt_int(recruited), fmt_int(target), pct,
    fmt_int(sites_open), fmt_int(sites_total))

  placeholder <- "<div class='pf-chart-placeholder'>
       <div class='pf-chart-placeholder-icon'>
         <svg width='32' height='32' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='1.5' stroke-linecap='round' stroke-linejoin='round'>
           <rect x='3' y='3' width='18' height='18' rx='2'/>
           <polyline points='3 15 8 10 13 13 21 5'/>
         </svg>
       </div>
       <div class='pf-chart-placeholder-text'>Recruitment graph</div>
       <div class='pf-chart-placeholder-sub'>Cumulative recruitment with projection to grant end</div>
     </div>"

  sprintf(
    "<div class='pf-section'>
       <div class='pf-section-banner'>Recruitment</div>
       <div class='pf-chart-container'>%s%s</div>
     </div>", placeholder, stats)
}

.rs_render_pr_milestones <- function(ctx) {
  ms_list <- ctx$cfg$portfolio_review$milestones %||% list()
  if (!length(ms_list)) {
    ms_list <- list(
      list(label = "Trial approvals submitted/in place",
           date_proposed = "", achieved = "",
           date_achieved = .pr_pick(ctx, "approvals_date")),
      list(label = "First site open to recruitment",
           date_proposed = "", achieved = "",
           date_achieved = .pr_pick(ctx, "open_recruitment_date")),
      list(label = "Recruitment milestone",
           date_proposed = .pr_var(ctx, "ms_recruitment",
                                   .pr_pick(ctx, "ms_recruitment")),
           achieved = "", date_achieved = ""),
      list(label = "Other",
           date_proposed = .pr_var(ctx, "ms_other",
                                   .pr_pick(ctx, "ms_other")),
           achieved = "", date_achieved = ""))
  }

  body_rows <- vapply(ms_list, function(m) {
    achieved <- tolower(as.character(m$achieved %||% ""))
    is_yes <- achieved %in% c("yes", "true", "y", "1") ||
              (nzchar(m$date_achieved %||% "") && m$date_achieved != "—")
    pill_cls <- if (is_yes) "green" else "grey"
    pill_txt <- if (is_yes) "Yes" else "Pending"
    sprintf(
      "<tr>
         <td>%s</td>
         <td class='mono'>%s</td>
         <td><span class='pf-pill %s'>%s</span></td>
         <td class='mono'>%s</td>
       </tr>",
      htmltools::htmlEscape(m$label %||% "—"),
      htmltools::htmlEscape(m$date_proposed %||% "—"),
      pill_cls, pill_txt,
      htmltools::htmlEscape(if (nzchar(m$date_achieved %||% "")) m$date_achieved else "—"))
  }, character(1))

  dc_raw <- .pr_var(ctx, "ms_data_capture", .pr_pick(ctx, "ms_data_capture"))
  dc_num <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", dc_raw)))
  dc_pct <- if (length(dc_num) && !is.na(dc_num)) max(0, min(100, dc_num)) else 0
  dc_label <- if (length(dc_num) && !is.na(dc_num))
    sprintf("%d%%", round(dc_pct)) else "—"

  dc_row <- sprintf(
    "<tr>
       <td>%% Data capture (CRFs received vs expected)</td>
       <td></td>
       <td colspan='2'>
         <div class='pf-data-capture'>
           <div class='pf-dc-bar'><div class='pf-dc-fill' style='width:%s%%;'></div></div>
           <span class='pf-dc-label'>%s</span>
         </div>
       </td>
     </tr>", dc_pct, dc_label)

  sprintf(
    "<div class='pf-section'>
       <div class='pf-section-banner'>Key (Funder) Milestones</div>
       <table class='pf-table'>
         <thead><tr>
           <th>Milestone</th><th>Date proposed</th>
           <th>Achieved</th><th>Date achieved</th>
         </tr></thead>
         <tbody>%s%s</tbody>
       </table>
     </div>", paste(body_rows, collapse = ""), dc_row)
}

.rs_render_pr_database <- function(ctx) {
  items <- list(
    list("Final CRF sign-off",                  .pr_pick(ctx, "db_crf_signoff")),
    list("Functional Requirement Specification", .pr_pick(ctx, "db_func_spec")),
    list("Database Requirement Specification",   .pr_pick(ctx, "db_req_spec")),
    list("Release for testing",                  .pr_pick(ctx, "db_release_test")),
    list("Final release date",                   .pr_pick(ctx, "db_final_release"))
  )
  rows <- vapply(items, function(r)
    sprintf("<div class='pf-kv-row'>
               <span class='pf-kv-label'>%s</span>
               <span class='pf-kv-value'>%s</span>
             </div>",
            htmltools::htmlEscape(r[[1]]),
            htmltools::htmlEscape(r[[2]] %||% "—")),
    character(1))
  sprintf(
    "<div class='pf-section'>
       <div class='pf-section-banner'>Database Development</div>
       <div class='pf-kv-grid'>%s</div>
     </div>", paste(rows, collapse = ""))
}

.rs_render_pr_finance <- function(ctx) {
  fin_status <- tolower(.pr_var(ctx, "fin_status",
                                .pr_pick(ctx, "fin_status", "")))
  fin_cls <- if (grepl("over", fin_status)) "red" else
             if (grepl("under", fin_status)) "amber" else
             if (grepl("track|on-?track", fin_status)) "green" else "grey"
  fin_label <- if (grepl("over", fin_status)) "Overspent" else
               if (grepl("under", fin_status)) "Underspent" else
               if (grepl("track", fin_status)) "On track" else
               .pr_var(ctx, "fin_status", .pr_pick(ctx, "fin_status", "Not set"))

  row <- function(label, value)
    sprintf("<div class='pf-staff-row'>
               <span class='pf-staff-label'>%s</span>
               <span class='pf-staff-value'>%s</span>
             </div>", label, value)

  rows <- paste0(
    row("Staffing status",
        htmltools::htmlEscape(.pr_var(ctx, "fin_staffing_status",
                                       .pr_pick(ctx, "fin_staffing_status")))),
    row("Staffing awarded (FTE &amp; duration)",
        htmltools::htmlEscape(.pr_pick(ctx, "fin_staffing_awarded"))),
    row("Current financial status",
        sprintf("<span class='pf-pill %s'>%s</span>", fin_cls,
                htmltools::htmlEscape(fin_label))))

  sprintf(
    "<div class='pf-section'>
       <div class='pf-section-banner'>Finance and Staffing</div>
       <div class='pf-staffing-grid'>%s</div>
     </div>", rows)
}

.pr_to_bullets <- function(txt) {
  if (is.null(txt) || !nzchar(txt) || txt == "—") return("")
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  lines <- trimws(sub("^[-*•]\\s*", "", lines))
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return("")
  items <- vapply(lines, function(l)
    sprintf("<li>%s</li>", htmltools::htmlEscape(l)), character(1))
  sprintf("<ul class='pf-issues-list'>%s</ul>", paste(items, collapse = ""))
}

.rs_render_pr_issues <- function(ctx) {
  concerns <- .pr_to_bullets(.pr_var(ctx, "issues_concerns", ""))
  remedial <- .pr_to_bullets(.pr_var(ctx, "issues_remedial", ""))
  empty_state <- "<em style='color:#94A3B8;font-size:10.5px;'>None recorded.</em>"
  if (!nzchar(concerns)) concerns <- empty_state
  if (!nzchar(remedial)) remedial <- empty_state

  sprintf(
    "<div class='pf-section'>
       <div class='pf-section-banner alert'>Major Issues of Concern</div>
       <div class='pf-issues-block'>
         <div class='pf-issues-col'>
           <div class='pf-issues-heading'>Issues of concern</div>%s
         </div>
         <div class='pf-issues-col'>
           <div class='pf-issues-heading'>Current / planned remedial action</div>%s
         </div>
       </div>
     </div>", concerns, remedial)
}

# ── Section registry ───────────────────────────────────────────────────────
REPORT_SECTIONS <- list(
  list(id = "header",              label = "Header & metadata",
       group = "Cover",             render = .rs_render_header),
  list(id = "recruitment_summary", label = "Recruitment summary",
       group = "Recruitment",       render = .rs_render_recruitment_summary),
  list(id = "smart_insights",      label = "Smart insights",
       group = "Recruitment",       render = .rs_render_smart_insights),
  list(id = "consort",             label = "CONSORT flow diagram",
       group = "Participants",      render = .rs_render_consort),
  list(id = "site_summary",        label = "Site summary",
       group = "Sites",             render = .rs_render_site_summary),
  list(id = "demographics",        label = "Demographics breakdown",
       group = "Participants",      render = .rs_render_demographics),
  list(id = "safety_summary",      label = "Safety & regulatory",
       group = "Safety",            render = .rs_render_safety_summary),
  list(id = "complications",       label = "Complications",
       group = "Safety",            render = .rs_render_complications),
  list(id = "amendments",          label = "Amendments",
       group = "Regulatory",        render = .rs_render_amendments),
  list(id = "custom_text",         label = "Custom text / notes",
       group = "Free text",         render = .rs_render_custom_text),
  list(id = "next_period",         label = "Plans for next period",
       group = "Free text",         render = .rs_render_next_period),

  # ── Portfolio Review (BCTU Trial Update Summary v4.0) ──────────────────
  list(id = "pr_trial_summary",    label = "Trial Update Summary",
       group = "Portfolio review", render = .rs_render_pr_trial_summary),
  list(id = "pr_review_progress",  label = "Review of Progress",
       group = "Portfolio review", render = .rs_render_pr_review_progress),
  list(id = "pr_rag_status",       label = "RAG Status",
       group = "Portfolio review", render = .rs_render_pr_rag_status),
  list(id = "pr_recruitment",      label = "Recruitment",
       group = "Portfolio review", render = .rs_render_pr_recruitment),
  list(id = "pr_milestones",       label = "Key (Funder) Milestones",
       group = "Portfolio review", render = .rs_render_pr_milestones),
  list(id = "pr_database",         label = "Database Development",
       group = "Portfolio review", render = .rs_render_pr_database),
  list(id = "pr_finance",          label = "Finance and Staffing",
       group = "Portfolio review", render = .rs_render_pr_finance),
  list(id = "pr_issues",           label = "Major Issues of Concern",
       group = "Portfolio review", render = .rs_render_pr_issues)
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
                 "consort", "site_summary", "safety_summary", "complications",
                 "custom_text")
  ),
  iTMG = list(
    label = "iTMG (Independent Trial Management Group)",
    description = "Independent management report — same content as the TMG report, labelled iTMG.",
    sections = c("header", "recruitment_summary", "smart_insights",
                 "consort", "site_summary", "safety_summary", "complications",
                 "custom_text")
  ),
  TSC = list(
    label = "TSC (Trial Steering Committee)",
    description = "External oversight — recruitment, demographics, safety, amendments.",
    sections = c("header", "recruitment_summary", "demographics", "consort",
                 "site_summary", "safety_summary", "complications", "amendments",
                 "next_period", "custom_text")
  ),
  `TSC Interim` = list(
    label = "TSC Interim v0.1",
    description = "TSC interim progress report — the TMG layout plus baseline characteristics, protocol deviations and monthly recruitment by site; recruiting sites only.",
    sections = c("header", "recruitment_summary", "demographics", "consort",
                 "site_summary", "safety_summary", "complications", "amendments",
                 "next_period", "custom_text")
  ),
  NIHR = list(
    label = "NIHR funder update",
    description = "Concise sponsor/funder report — recruitment vs target, plans.",
    sections = c("header", "recruitment_summary", "site_summary",
                 "amendments", "next_period")
  ),
  Portfolio = list(
    label = "Portfolio review",
    description = "BCTU Trial Update Summary (v4.0) — trial summary, progress, RAG, milestones, finance, issues.",
    sections = c("pr_trial_summary", "pr_review_progress", "pr_rag_status",
                 "pr_recruitment", "pr_milestones", "pr_database",
                 "pr_finance", "pr_issues")
  )
)
