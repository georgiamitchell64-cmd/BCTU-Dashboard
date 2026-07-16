# =============================================================================
# Smart Insights — deterministic, rule-based summaries (Stage 8)
# =============================================================================
# Pure functions. No API calls. Given a trial's raw REDCap export and sites
# table, return a list of insight objects shaped like:
#   list(severity = "info"|"warning"|"alert",
#        icon = "<HTML entity>",
#        title = "Headline",
#        body  = "One-line explanation",
#        value = "optional bold supporting number")
#
# Rules return NULL to skip themselves when there isn't enough data.
# =============================================================================

.insight <- function(severity, icon, title, body, value = NULL) {
  list(severity = severity, icon = icon, title = title,
       body = body, value = value)
}

# Pull baseline rows from the raw REDCap frame.
.baseline_rows <- function(raw, cfg) {
  if (is.null(raw)) return(data.frame())
  if (!nrow(raw))   return(raw[0, , drop = FALSE])
  baseline_evt <- cfg$redcap_events$baseline %||% "baseline_arm_1"
  if ("redcap_event_name" %in% names(raw)) {
    raw[raw$redcap_event_name == baseline_evt, , drop = FALSE]
  } else {
    raw
  }
}

# Vector of randomisation dates, NA-stripped.
.rand_dates <- function(raw, cfg) {
  base <- .baseline_rows(raw, cfg)
  if (!nrow(base)) return(as.Date(character(0)))
  rand_col <- cfg$redcap_fields$randomisation_datetime %||% "rand_dttm_s"
  if (!rand_col %in% names(base)) return(as.Date(character(0)))
  d <- suppressWarnings(as.Date(base[[rand_col]]))
  d[!is.na(d)]
}

# ── Rule 1: Recruitment pace (last 30 days vs target) ───────────────────────
insight_pace <- function(raw, sites, cfg) {
  dates <- .rand_dates(raw, cfg)
  if (length(dates) == 0) return(NULL)

  recent <- sum(dates >= (Sys.Date() - 30))
  open_sites <- if (!is.null(sites) && nrow(sites)) {
    sum(sites$status %in% c("Open", "Recruiting"), na.rm = TRUE)
  } else 0L
  expected <- open_sites * (cfg$projection_defaults$rate_central %||% 3)

  if (expected <= 0) {
    return(.insight("info", "&#x1F4C8;", "Recruitment pace",
                   sprintf("%d randomisations in the last 30 days.", recent),
                   value = sprintf("%d", recent)))
  }

  ratio <- recent / expected
  if (ratio >= 0.9) {
    .insight("info", "&#x1F4C8;", "Recruitment pace on track",
             sprintf("Last 30 days: %d (target ~%d at %d open sites).",
                     recent, round(expected), open_sites),
             value = sprintf("%.0f%%", ratio * 100))
  } else if (ratio >= 0.5) {
    .insight("warning", "&#x1F4C9;", "Recruitment pace below target",
             sprintf("Last 30 days: %d vs target ~%d. At %.0f%% of central projection.",
                     recent, round(expected), ratio * 100),
             value = sprintf("%.0f%%", ratio * 100))
  } else {
    .insight("alert", "&#x26A0;", "Recruitment well below target",
             sprintf("Only %d randomisations in the last 30 days, vs target ~%d.",
                     recent, round(expected)),
             value = sprintf("%.0f%%", ratio * 100))
  }
}

# ── Rule 2: Stalled trial ───────────────────────────────────────────────────
insight_stalled <- function(raw, cfg) {
  dates <- .rand_dates(raw, cfg)
  if (length(dates) == 0) {
    return(.insight("warning", "&#x23F8;", "No randomisations yet",
                   "No baseline records with a randomisation date in the export.",
                   value = "0"))
  }
  days_since <- as.integer(Sys.Date() - max(dates))
  if (days_since >= 30) {
    .insight("alert", "&#x23F8;",
             sprintf("Stalled — %d days since last randomisation", days_since),
             "Trial appears paused. Check if sites are still actively recruiting.",
             value = sprintf("%d days", days_since))
  } else if (days_since >= 14) {
    .insight("warning", "&#x23F1;",
             sprintf("Quiet — %d days since last randomisation", days_since),
             "Recruitment has slowed. May warrant a check-in with sites.",
             value = sprintf("%d days", days_since))
  } else {
    .insight("info", "&#x1F4C5;",
             sprintf("Last randomisation %d %s ago",
                     days_since, if (days_since == 1) "day" else "days"),
             "Recruitment is currently active.",
             value = sprintf("%d", length(dates)))
  }
}

# ── Rule 3: Top performer (last 30d) ────────────────────────────────────────
insight_top_site <- function(raw, cfg) {
  base <- .baseline_rows(raw, cfg)
  if (!nrow(base)) return(NULL)
  rand_col <- cfg$redcap_fields$randomisation_datetime %||% "rand_dttm_s"
  site_col <- cfg$redcap_fields$site_name %||% "site_name"
  if (!all(c(rand_col, site_col) %in% names(base))) return(NULL)

  base$.d <- suppressWarnings(as.Date(base[[rand_col]]))
  recent  <- base[!is.na(base$.d) & base$.d >= (Sys.Date() - 30), , drop = FALSE]
  if (!nrow(recent)) return(NULL)

  by_site <- sort(table(recent[[site_col]]), decreasing = TRUE)
  if (length(by_site) == 0) return(NULL)

  top_name <- names(by_site)[1]
  top_n    <- as.integer(by_site[1])

  .insight("info", "&#x1F3C6;",
           "Top performing site (last 30 days)",
           sprintf("%s contributed %d of the last 30 days' randomisations.",
                   top_name, top_n),
           value = sprintf("%d", top_n))
}

# ── Rule 4: Lagging open sites ──────────────────────────────────────────────
insight_lagging_sites <- function(raw, sites, cfg) {
  if (is.null(sites) || !nrow(sites)) return(NULL)
  recruiting <- sites[sites$status %in% c("Open", "Recruiting"), ]
  if (!nrow(recruiting)) return(NULL)

  base <- .baseline_rows(raw, cfg)
  if (!nrow(base)) return(NULL)
  rand_col <- cfg$redcap_fields$randomisation_datetime %||% "rand_dttm_s"
  site_col <- cfg$redcap_fields$site_name %||% "site_name"
  if (!all(c(rand_col, site_col) %in% names(base))) return(NULL)

  base$.d <- suppressWarnings(as.Date(base[[rand_col]]))
  recent  <- base[!is.na(base$.d) & base$.d >= (Sys.Date() - 60), , drop = FALSE]
  active_sites <- unique(recent[[site_col]])
  lagging <- recruiting$site_name[!recruiting$site_name %in% active_sites]
  if (length(lagging) == 0) return(NULL)

  .insight(if (length(lagging) > 3) "alert" else "warning",
           "&#x1F4CD;",
           sprintf("%d %s open but not recruiting",
                   length(lagging),
                   if (length(lagging) == 1) "site" else "sites"),
           sprintf("No randomisations in the last 60 days at: %s.",
                   paste(head(lagging, 3), collapse = ", ")),
           value = sprintf("%d", length(lagging)))
}

# ── Rule 5: Data completeness on baseline ───────────────────────────────────
insight_completeness <- function(raw, cfg) {
  base <- .baseline_rows(raw, cfg)
  if (!nrow(base)) return(NULL)

  age_col <- cfg$redcap_fields$age %||% "dem_age"
  sex_col <- cfg$redcap_fields$sex %||% "dem_sex"
  cols <- intersect(c(age_col, sex_col), names(base))
  if (!length(cols)) return(NULL)

  recent <- if (nrow(base) > 50) tail(base, 50) else base
  miss_pct <- mean(rowSums(is.na(recent[, cols, drop = FALSE]) |
                             recent[, cols, drop = FALSE] == "") > 0)

  if (miss_pct >= 0.25) {
    .insight("warning", "&#x1F50D;",
             "Baseline data incomplete",
             sprintf("%.0f%% of recent baseline records are missing %s.",
                     miss_pct * 100, paste(cols, collapse = " or ")),
             value = sprintf("%.0f%%", miss_pct * 100))
  } else if (miss_pct == 0) {
    .insight("info", "&#x2705;", "Baseline data complete",
             sprintf("All %d recent baseline records have %s populated.",
                     nrow(recent), paste(cols, collapse = " and ")))
  } else NULL
}

# ── Aggregator ──────────────────────────────────────────────────────────────
compute_insights <- function(raw_redcap, sites, cfg) {
  safe <- function(expr) tryCatch(expr, error = function(e) {
    message("Insight rule error: ", e$message); NULL
  })
  rules <- list(
    safe(insight_pace(raw_redcap, sites, cfg)),
    safe(insight_stalled(raw_redcap, cfg)),
    safe(insight_top_site(raw_redcap, cfg)),
    safe(insight_lagging_sites(raw_redcap, sites, cfg)),
    safe(insight_completeness(raw_redcap, cfg))
  )
  Filter(Negate(is.null), rules)
}

# ── Render one insight card ─────────────────────────────────────────────────
render_insight_card <- function(ins) {
  colours <- switch(ins$severity,
    info    = list(bg = "#F0FDF4", border = "#BBF7D0",
                   icon_bg = "#DCFCE7", icon_fg = "#15803D",
                   title = "#14532D"),
    warning = list(bg = "#FFFBEB", border = "#FDE68A",
                   icon_bg = "#FEF3C7", icon_fg = "#B45309",
                   title = "#78350F"),
    alert   = list(bg = "#FEF2F2", border = "#FECACA",
                   icon_bg = "#FEE2E2", icon_fg = "#B91C1C",
                   title = "#7F1D1D")
  )

  div(style = sprintf("background:%s;border:1px solid %s;border-radius:12px;
                       padding:14px 16px;display:flex;align-items:flex-start;gap:12px;",
                      colours$bg, colours$border),
      div(style = sprintf("width:34px;height:34px;border-radius:9px;background:%s;
                           color:%s;display:flex;align-items:center;justify-content:center;
                           font-size:16px;flex-shrink:0;",
                          colours$icon_bg, colours$icon_fg),
          HTML(ins$icon)),
      div(style = "flex:1;min-width:0;",
          div(style = "display:flex;justify-content:space-between;align-items:baseline;
                       gap:10px;margin-bottom:3px;",
              div(style = sprintf("font-size:13.5px;font-weight:600;color:%s;",
                                  colours$title),
                  ins$title),
              if (!is.null(ins$value))
                span(style = sprintf("font-size:13px;font-weight:700;color:%s;
                                      flex-shrink:0;", colours$icon_fg),
                     ins$value)),
          div(style = "font-size:12px;color:#475569;line-height:1.5;",
              ins$body))
  )
}

render_insights_panel <- function(insights) {
  if (length(insights) == 0) {
    return(div(style = "padding:18px;font-size:12.5px;color:#64748B;
                        font-style:italic;",
               "No insights yet — upload a REDCap export to populate the trial."))
  }
  div(style = "display:grid;grid-template-columns:repeat(auto-fill, minmax(280px, 1fr));
               gap:12px;",
      lapply(insights, render_insight_card))
}
