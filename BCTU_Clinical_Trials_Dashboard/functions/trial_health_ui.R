# =============================================================================
# Trial health — UI builders for the Overview, Data, Sites and Randomisations tabs
# =============================================================================
# Cards are plain htmltools built from th_build() (functions/trial_health.R).
# The interactive charts are JSON widgets mounted by www/trial_health.js —
# funnel, crfgrid, punchcard and trajectory — using the same embed-JSON-next-
# to-a-div pattern as the trial replay.
# =============================================================================

.th_rag_word  <- c(red = "At risk", amber = "Watch", green = "Healthy", new = "New site")
.th_score_col <- function(s) {
  if (length(s) != 1 || is.na(s)) "#8A8A8C" else if (s >= 75) "#00ACA9" else if (s >= 50) "#F07F3C" else "#E30513"
}
.th_month <- function(d) if (is.null(d) || !length(d) || is.na(d)) "—" else format(as.Date(d), "%b %Y")
.th_iso   <- function(d) format(as.Date(d), "%Y-%m-%d")
.th_num1  <- function(x) format(round(x, 1), nsmall = 0, drop0trailing = TRUE)

th_empty_note <- function(msg) div(class = "th-empty", msg)

.th_widget <- function(kind, id, payload) {
  json <- jsonlite::toJSON(payload, auto_unbox = TRUE, dataframe = "rows",
                           na = "null", null = "null", digits = 4)
  json <- gsub("</", "<\\/", json, fixed = TRUE)
  tagList(div(id = id, class = paste0("th-widget th-", kind), `data-kind` = kind),
          tags$script(type = "application/json", id = paste0(id, "-data"), HTML(json)))
}

# Tab-switch link used by the "needs attention" items
.th_go <- function(label, input_id, tab_btn) {
  tags$button(type = "button", class = "th-link",
              onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority:'event'}); if (window.setActiveTab) setActiveTab('%s');",
                                input_id, tab_btn),
              label, HTML(" &rarr;"))
}

th_score_ring <- function(score, size = 76, stroke = 8, sub = NULL, track = "#EFEFEF") {
  r <- (size - stroke) / 2; circ <- 2 * pi * r
  val <- if (length(score) != 1 || is.na(score)) 0 else max(0, min(100, score))
  c0 <- size / 2
  div(class = "th-ring", style = sprintf("width:%dpx;height:%dpx;", size, size),
      HTML(sprintf(paste0(
        '<svg width="%d" height="%d" viewBox="0 0 %d %d" aria-hidden="true">',
        '<circle cx="%.1f" cy="%.1f" r="%.1f" fill="none" stroke="%s" stroke-width="%d"/>',
        '<circle cx="%.1f" cy="%.1f" r="%.1f" fill="none" stroke="%s" stroke-width="%d" stroke-linecap="round"',
        ' stroke-dasharray="%.1f %.1f" transform="rotate(-90 %.1f %.1f)"/></svg>'),
        size, size, size, size, c0, c0, r, track, stroke,
        c0, c0, r, .th_score_col(score), stroke, circ * val / 100, circ, c0, c0)),
      div(class = "th-ring-v", if (is.na(val) || length(score) != 1 || is.na(score)) "—" else score,
          if (!is.null(sub)) span(sub)))
}

th_sparkline <- function(v, w = 120, h = 28, col = "#1B1B1B") {
  v <- as.numeric(v)
  if (!length(v)) return(NULL)
  n <- length(v); mx <- max(1, v)
  x <- if (n == 1) w / 2 else (seq_len(n) - 1) / (n - 1) * (w - 4) + 2
  y <- h - 3 - v / mx * (h - 6)
  pts <- paste(sprintf("%.1f,%.1f", x, y), collapse = " ")
  HTML(sprintf(paste0(
    '<svg class="th-spark" width="%d" height="%d" viewBox="0 0 %d %d" aria-hidden="true">',
    '<polygon points="%s %.1f,%d %.1f,%d" fill="%s" opacity=".12"/>',
    '<polyline points="%s" fill="none" stroke="%s" stroke-width="1.6" stroke-linejoin="round"/>',
    '<circle cx="%.1f" cy="%.1f" r="2.2" fill="%s"/></svg>'),
    w, h, w, h, pts, x[n], h, x[1], h, col, pts, col, x[n], y[n], col))
}

# =============================================================================
# Overview — trial health card
# =============================================================================
th_health_card <- function(H) {
  head <- div(class = "pov-card-head",
    div(tags$h3("Trial health"),
        span(class = "pov-card-sub",
             "Recruitment, retention, data returns and site activity, scored from the latest export. Thresholds are in Settings → Recruitment & monitoring.")))
  if (is.null(H) || !nrow(H$participants))
    return(tags$section(class = "pov-card th-card", head,
                        th_empty_note("Load a REDCap export to see how the trial is doing.")))
  sm <- H$summary; comp <- sm$components; fl <- H$settings$flags
  word <- if (is.na(sm$score)) "Not enough data yet" else if (sm$score >= 75) "On track"
          else if (sm$score >= 50) "Needs attention" else "At risk"

  bar <- function(lbl, v, hint) div(class = "th-comp",
    div(class = "th-comp-top", span(lbl), span(class = "th-comp-v", if (is.na(v)) "—" else round(v))),
    div(class = "th-comp-track",
        div(class = "th-comp-fill",
            style = sprintf("width:%.0f%%;background:%s;", if (is.na(v)) 0 else v, .th_score_col(v)))),
    div(class = "th-comp-hint", hint))

  S <- H$sites
  open_n <- if (is.null(S)) 0 else sum(!S$status %in% c("Closed", "Paused") & S$months >= 1)
  bars <- div(class = "th-comps",
    bar("Recruitment", comp[["recruitment"]],
        if (!is.na(sm$target_by_now)) sprintf("%d randomised vs %d expected by now", sm$n, round(sm$target_by_now))
        else sprintf("%d randomised", sm$n)),
    bar("Retention", comp[["retention"]],
        sprintf("%d with a change of status (%s), deaths excluded", sm$flagged,
                if (is.na(sm$flagged_pct)) "—" else sprintf("%.0f%%", sm$flagged_pct))),
    bar("Data returns", comp[["data"]],
        if (sm$expected_crfs) sprintf("%d of %d expected CRFs entered", sm$complete_crfs, sm$expected_crfs)
        else "No CRFs due yet"),
    bar("Site activity", comp[["activity"]],
        sprintf("Open sites that recruited in the last %d days (%d open)", fl$quiet_amber, open_n)))

  pr <- H$projection
  stat <- function(v, l, cls = NULL) div(class = "th-hs", div(class = paste("th-hs-v", cls), v), div(class = "th-hs-l", l))
  stats <- div(class = "th-hero-stats",
    stat(format(sm$n, big.mark = ","), sprintf("randomised of %s", format(sm$target, big.mark = ","))),
    if (!is.na(sm$ahead))
      stat(sprintf("%+d", as.integer(sm$ahead)), "vs target schedule", if (sm$ahead < 0) "bad" else "good"),
    stat(if (is.na(sm$completeness)) "—" else sprintf("%.0f%%", sm$completeness), "of expected CRFs in"),
    if (!is.null(pr))
      stat(.th_month(pr$finish$p50),
           if (!is.na(pr$prob_by_end)) sprintf("finish at current pace · %.0f%% by %s", 100 * pr$prob_by_end, .th_month(pr$end_date))
           else "finish at current pace",
           if (!is.na(pr$prob_by_end) && pr$prob_by_end < 0.5) "bad"))

  # Needs attention: worst sites, urgent participants, overdue CRFs
  items <- list()
  if (!is.null(S)) for (i in head(which(S$rag %in% c("red", "amber")), 3))
    items[[length(items) + 1]] <- div(class = paste("th-att", paste0("rag-", S$rag[i])),
      div(class = "th-att-t", S$site[i], span(class = paste("th-rag", paste0("rag-", S$rag[i])), .th_rag_word[[S$rag[i]]])),
      div(class = "th-att-b", S$reasons[[i]][1] %||% ""))
  hi <- if (is.null(H$issues)) 0 else sum(H$issues$priority == "High")
  if (hi) items[[length(items) + 1]] <- div(class = "th-att rag-red",
    div(class = "th-att-t", sprintf("%d participant%s need urgent follow-up", hi, if (hi == 1) "" else "s")),
    div(class = "th-att-b", "Overdue CRFs or missing operation / discharge dates"))
  if (!is.null(pr) && !is.na(pr$prob_by_end) && pr$prob_by_end < 0.5)
    items[[length(items) + 1]] <- div(class = paste("th-att", if (pr$prob_by_end < 0.2) "rag-red" else "rag-amber"),
      div(class = "th-att-t", sprintf("Current pace misses %s by %s", format(pr$target, big.mark = ","), .th_month(pr$end_date))),
      div(class = "th-att-b", sprintf("At %s a month the target is reached around %s (%.0f%% chance by %s).",
                                      .th_num1(pr$per_month), .th_month(pr$finish$p50),
                                      100 * pr$prob_by_end, .th_month(pr$end_date))))
  if (!length(items)) items <- list(div(class = "th-att rag-green",
    div(class = "th-att-t", "Nothing needs attention"),
    div(class = "th-att-b", "All sites are within limits.")))

  tags$section(class = "pov-card th-card", head,
    div(class = "th-hero",
      th_score_ring(sm$score, size = 104, stroke = 10, sub = "/ 100", track = "rgba(255,255,255,.12)"),
      div(class = "th-hero-word",
          div(class = "th-hero-state", style = sprintf("color:%s;", .th_score_col(sm$score)), word),
          div(class = "th-hero-note", "Overall health, weighing recruitment, retention, data returns and site activity.")),
      stats),
    div(class = "th-health",
      bars,
      div(class = "th-attn",
          div(class = "th-attn-h", "Needs attention"),
          items,
          div(class = "th-attn-links",
              .th_go("Site scorecards", "go_sites", "tn_sites"),
              .th_go("Data worklist", "go_participants", "tn_participants"),
              .th_go("Recruitment trends", "go_randomisations", "tn_randomisations")))))
}

# Real sparkline for an Overview KPI (monthly series)
th_kpi_spark <- function(H, what = c("rand", "sites", "cum")) {
  what <- match.arg(what)
  pt <- H$patterns
  if (is.null(pt)) return(NULL)
  v <- switch(what,
    rand  = pt$monthly,
    cum   = cumsum(pt$monthly),
    sites = {
      S <- H$sites
      if (is.null(S)) return(NULL)
      m <- as.Date(paste0(pt$months, "-01")); o <- .th_as_date(S$open)
      vapply(m, function(x) sum(!is.na(o) & o <= x + 31), numeric(1))
    })
  th_sparkline(tail(v, 12), w = 80, h = 32, col = trial_palette()[["primary"]])
}

# =============================================================================
# Sites — scorecards and drill-down
# =============================================================================
th_site_cards <- function(H) {
  S <- H$sites
  if (is.null(S) || !nrow(S)) return(th_empty_note("No sites with participants yet."))
  pt <- H$patterns
  metric <- function(l, v, cls = NULL) div(class = paste("th-m", cls), div(class = "th-m-v", v), div(class = "th-m-l", l))
  sub <- function(l, v) div(class = "th-sub", span(l),
    div(class = "th-sub-track", div(style = sprintf("width:%.0f%%;background:%s;", if (is.na(v)) 0 else v, .th_score_col(v)))))
  cards <- lapply(seq_len(nrow(S)), function(i) {
    r <- S[i, ]; why <- S$reasons[[i]]
    monthly <- pt$by_site[[r$site]]$monthly %||% integer()
    div(class = paste("th-scard", paste0("rag-", r$rag)), role = "button", tabindex = "0",
        `data-site` = r$site, title = paste("Open", r$site),
        onclick = "Shiny.setInputValue('th_site_open', {site: this.dataset.site, n: Math.random()}, {priority: 'event'})",
        onkeydown = "if (event.key === 'Enter' || event.key === ' ') { event.preventDefault(); this.click(); }",
      div(class = "th-scard-head",
          div(class = "th-scard-name", span(r$site),
              span(class = paste("th-rag", paste0("rag-", r$rag)), .th_rag_word[[r$rag]])),
          th_score_ring(r$score, size = 46, stroke = 5)),
      div(class = "th-scard-metrics",
          metric("Randomised", r$n),
          metric("Per month", if (is.na(r$rate)) "—" else
                 tagList(.th_num1(r$rate), span(class = "th-m-of", paste0("/", .th_num1(r$target_rate))))),
          metric("Change of status", if (r$n) sprintf("%d · %.0f%%", r$cos_n, 100 * r$cos_rate) else "—",
                 if (r$n >= H$settings$flags$min_n && !is.na(r$z_cos) && r$z_cos >= H$settings$flags$z_warn) "bad"),
          metric("CRFs in", if (is.na(r$complete_pct)) "—" else sprintf("%.0f%%", r$complete_pct),
                 if (!is.na(r$overdue_pct) && r$overdue_pct >= H$settings$flags$overdue_amber) "bad")),
      div(class = "th-scard-spark", th_sparkline(monthly, w = 150, h = 30),
          span(sprintf("%d in the last 90 days", sum(tail(monthly, 3))))),
      div(class = "th-scard-subs", sub("Recruit", r$sc_recruit), sub("Retain", r$sc_retain),
          sub("Data", r$sc_data), sub("Activity", r$sc_activity)),
      if (length(why)) tags$ul(class = "th-reasons", lapply(head(why, 3), tags$li)))
  })
  div(class = "th-scards", cards)
}

th_site_detail <- function(H, site) {
  S <- H$sites; i <- match(site, S$site)
  if (is.na(i)) return(th_empty_note("This site has no data in the current view."))
  r <- S[i, ]; P <- H$participants[H$participants$site == site, , drop = FALSE]
  today_s <- .th_today_s(H$today)
  crf <- H$crf[H$crf$site == site, , drop = FALSE]
  od  <- crf[crf$status == "overdue", , drop = FALSE]
  fmt_d <- function(x) ifelse(is.na(x), "—", format(.th_as_date(x), "%d %b %Y"))
  rows <- lapply(seq_len(nrow(P)), function(k) {
    p <- P[k, ]; o <- od[od$id == p$id, , drop = FALSE]
    st <- if (!is.na(p$exit_at) && p$exit_at <= today_s) p$exit_label else if (!is.na(p$part_at)) "Part withdrawal" else "Active"
    tags$tr(class = if (nrow(o)) "th-row-bad",
            tags$td(p$id), tags$td(fmt_d(p$rand)), tags$td(fmt_d(p$op)), tags$td(fmt_d(p$dis)),
            tags$td(st), tags$td(if (nrow(o)) paste(sprintf("%s %s (%dd)", o$timepoint, o$form, as.integer(o$days_overdue)), collapse = "; ") else "—"))
  })
  monthly <- H$patterns$by_site[[site]]$monthly %||% integer()
  tagList(
    div(class = "th-detail-head",
        th_score_ring(r$score, size = 64, stroke = 7),
        div(div(class = "th-detail-name", site,
                span(class = paste("th-rag", paste0("rag-", r$rag)), .th_rag_word[[r$rag]])),
            div(class = "th-detail-sub",
                sprintf("Open since %s · %d randomised · %s a month (target %s) · last randomisation %s",
                        if (is.na(r$open)) "—" else format(.th_as_date(r$open), "%d %b %Y"), r$n,
                        if (is.na(r$rate)) "—" else .th_num1(r$rate), .th_num1(r$target_rate),
                        if (is.na(r$last)) "never" else format(.th_as_date(r$last), "%d %b %Y"))))),
    if (length(S$reasons[[i]])) tags$ul(class = "th-reasons th-reasons-lg", lapply(S$reasons[[i]], tags$li)),
    div(class = "th-detail-grid",
        div(class = "th-detail-box", div(class = "th-detail-h", "Monthly randomisations"),
            th_sparkline(monthly, w = 300, h = 60)),
        div(class = "th-detail-box", div(class = "th-detail-h", "Change of status"),
            if (any(!is.na(P$exit_code) | !is.na(P$part_at)))
              tags$ul(class = "th-plain", lapply(which(!is.na(P$exit_code) | !is.na(P$part_at)), function(k)
                tags$li(tags$b(P$id[k]), " — ",
                        if (!is.na(P$exit_code[k])) paste0(P$exit_label[k], ", ", fmt_d(P$exit_at[k])) else
                          paste0("Part withdrawal, ", fmt_d(P$part_at[k])))))
            else div(class = "th-muted", "None recorded")),
        div(class = "th-detail-box", div(class = "th-detail-h", "CRF returns"),
            div(class = "th-muted", sprintf("%d of %d expected CRFs entered · %d overdue · %d due now",
                                            r$done, r$expected, r$overdue, r$due_soon)))),
    div(class = "th-table-wrap",
        tags$table(class = "th-table",
          tags$thead(tags$tr(tags$th("Participant"), tags$th("Randomised"), tags$th("Surgery"),
                             tags$th("Discharged"), tags$th("Status"), tags$th("Overdue forms"))),
          tags$tbody(rows))))
}

# =============================================================================
# JSON widgets (mounted by www/trial_health.js)
# =============================================================================

th_crfgrid_widget <- function(H, id = "th-crfgrid") {
  crf <- H$crf; sched <- H$schedule[H$schedule$present %in% TRUE, , drop = FALSE]
  P <- H$participants
  if (!nrow(crf) || !nrow(sched)) return(th_empty_note("No CRF schedule could be built from this export. Set one up in Settings → Visits & CRFs."))
  code <- c(complete = "c", overdue = "o", due = "d", not_due = "n", not_required = "x", awaiting = "a")
  key <- paste(crf$id, crf$fid)
  s_map <- setNames(code[crf$status], key)
  d_map <- setNames(crf$days_overdue, key)
  e_map <- setNames(crf$estimated, key)
  ord <- order(P$site, P$id)
  rows <- lapply(ord, function(i) {
    k <- paste(P$id[i], sched$fid)
    s <- s_map[k]; s[is.na(s)] <- "-"
    list(id = P$id[i], site = P$site[i], stage = P$stage[i], s = paste(s, collapse = ""),
         d = I(unname(d_map[k])), e = paste(ifelse(e_map[k] %in% TRUE, "1", "0"), collapse = ""))
  })
  .th_widget("crfgrid", id, list(
    forms = data.frame(fid = sched$fid, tp = sched$timepoint, form = sched$form, kind = sched$kind,
                       stringsAsFactors = FALSE),
    rows = rows))
}

th_punchcard_widget <- function(H, id = "th-punchcard") {
  pt <- H$patterns
  if (is.null(pt)) return(th_empty_note("No randomisations yet."))
  wh <- pt$working_hours
  .th_widget("punchcard", id, list(
    heat = lapply(1:7, function(r) I(as.integer(pt$heat[r, ]))),
    by_wday = I(unname(as.integer(pt$by_wday))), by_hour = I(unname(as.integer(pt$by_hour))),
    start = .th_hm(wh$start), end = .th_hm(wh$end), work_days = I(match(wh$days, .TH_WDAYS)),
    cats = as.list(pt$category), n = pt$n, n_timed = pt$n_timed))
}

th_trajectory_widget <- function(H, id = "th-trajectory") {
  pr <- H$projection
  if (is.null(pr)) return(th_empty_note("Projections need at least three randomisations over two complete weeks."))
  d <- function(x) if (is.null(x) || !length(x) || all(is.na(x))) NULL else .th_iso(x)
  .th_widget("trajectory", id, list(
    weeks = I(.th_iso(pr$weeks)), actual = I(pr$actual),
    pweeks = I(.th_iso(pr$proj_weeks)),
    pace = I(round(pr$pace, 1)), trend = I(round(pr$trend, 1)), sites = I(round(pr$sites, 1)),
    band = lapply(pr$band, function(x) I(round(x, 1))),
    target = pr$target, n_now = pr$n_now,
    schedule = if (!is.null(pr$schedule))
      list(t = I(.th_iso(pr$schedule$month_date)), v = I(pr$schedule$cumulative_target)) else NULL,
    finish = lapply(pr$finish, d), prob = pr$prob_by_end, end_date = d(pr$end_date),
    per_month = round(pr$per_month, 1), growth = round(pr$growth_month, 1),
    lambda = round(pr$lambda_week, 4), last_rand = d(pr$last_rand),
    sites_per_month = round(pr$sites_per_month, 1), open_sites = pr$open_sites,
    to_open = pr$to_open, window = pr$window_weeks, today = .th_iso(H$today)))
}

# =============================================================================
# Randomisations — KPI strip
# =============================================================================
th_rand_kpis <- function(H) {
  pt <- H$patterns
  if (is.null(pt)) return(th_empty_note("No randomisations yet — upload a REDCap export."))
  cat <- pt$category; timed <- max(1, pt$n_timed)
  pc <- function(k) 100 * sum(cat[k]) / timed
  hot <- which(pt$heat == max(pt$heat), arr.ind = TRUE)[1, ]
  kpi <- function(l, v, s = NULL, cls = NULL) div(class = paste("th-kpi", cls),
    div(class = "th-kpi-l", l), div(class = "th-kpi-v", v), if (!is.null(s)) div(class = "th-kpi-s", s))
  tr <- pt$trend_pct
  div(class = "th-kpis",
    kpi("Randomised", pt$n, sprintf("%d with a recorded time", pt$n_timed)),
    kpi("Last 90 days", pt$recent_90,
        if (is.na(tr)) "no earlier period to compare"
        else HTML(sprintf("<span class='%s'>%s%.0f%%</span> vs the 90 days before",
                          if (tr >= 0) "th-good" else "th-bad", if (tr >= 0) "▲ " else "▼ ", abs(tr)))),
    kpi("In hours", sprintf("%.0f%%", pc("In hours")),
        sprintf("%s–%s, %s", pt$working_hours$start, pt$working_hours$end,
                paste(pt$working_hours$days[c(1, length(pt$working_hours$days))], collapse = "–"))),
    kpi("Weekday out of hours", sprintf("%.0f%%", pc("Weekday out of hours")), "evenings and nights"),
    kpi("Weekends & bank holidays", sprintf("%.0f%%", pc(c("Weekend", "Bank holiday"))),
        sprintf("%d on bank holidays", cat[["Bank holiday"]])),
    kpi("Busiest slot", if (pt$n_timed) sprintf("%s %02d:00", .TH_WDAYS[hot[1]], hot[2] - 1) else "—",
        if (pt$n_timed) sprintf("%d randomisations in that hour", max(pt$heat)) else NULL))
}

# =============================================================================
# Participant drill-down — every scheduled form with its status
# =============================================================================
th_participant_detail <- function(H, id) {
  P <- H$participants; i <- match(id, P$id)
  if (is.na(i)) return(th_empty_note("This participant isn't in the current view."))
  p   <- P[i, ]
  crf <- H$crf[H$crf$id == id, , drop = FALSE]
  iss <- if (is.null(H$issues)) NULL else H$issues[H$issues$id == id, , drop = FALSE]
  fmt_d <- function(x) ifelse(is.na(x), "—", format(.th_as_date(x), "%d %b %Y"))
  lab <- c(complete = "Complete", overdue = "Overdue", due = "Due now", not_due = "Not due yet",
           not_required = "Not required", awaiting = "Awaiting visit date")
  rows <- lapply(seq_len(nrow(crf)), function(k) {
    f <- crf[k, ]
    tags$tr(class = if (f$status == "overdue") "th-row-bad",
      tags$td(f$timepoint),
      tags$td(f$form, if (f$kind != "CRF") span(class = "th-muted", " · questionnaire")),
      tags$td(fmt_d(f$event_at),
              if (isTRUE(f$estimated) && f$status != "complete") span(class = "th-muted", " (estimated)")),
      tags$td(span(class = paste0("th-st th-st-", f$status), lab[[f$status]]),
              if (f$status == "overdue") span(class = "th-muted", sprintf(" by %d days", as.integer(f$days_overdue)))))
  })
  has_iss <- !is.null(iss) && nrow(iss) > 0
  tagList(
    div(class = "th-detail-head",
        div(div(class = "th-detail-name", p$id,
                if (has_iss) span(class = paste0("th-prio th-prio-", iss$priority[1]),
                                  paste(iss$priority[1], "priority"))),
            div(class = "th-detail-sub", sprintf("%s · %s", p$site, p$stage)))),
    if (has_iss) tags$ul(class = "th-reasons th-reasons-lg",
                         lapply(strsplit(iss$issues[1], " · ", fixed = TRUE)[[1]], tags$li)),
    div(class = "th-detail-grid",
        div(class = "th-detail-box", div(class = "th-detail-h", "Randomised"), fmt_d(p$rand)),
        div(class = "th-detail-box", div(class = "th-detail-h", "Surgery · discharge"),
            paste(fmt_d(p$op), "·", fmt_d(p$dis))),
        div(class = "th-detail-box", div(class = "th-detail-h", "Change of status"),
            if (!is.na(p$exit_code)) paste0(p$exit_label, ", ", fmt_d(p$exit_at))
            else if (!is.na(p$part_at)) paste0("Part withdrawal, ", fmt_d(p$part_at)) else "None")),
    div(class = "th-table-wrap",
        tags$table(class = "th-table",
          tags$thead(tags$tr(tags$th("Timepoint"), tags$th("Form"), tags$th("Visit date"), tags$th("Status"))),
          tags$tbody(rows))))
}
