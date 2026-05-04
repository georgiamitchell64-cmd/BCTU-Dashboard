# =============================================================================
# Participant breakdowns — customisable demographic cards
# =============================================================================
# Detects usable columns from the uploaded REDCap CSV (rv$raw_redcap), picks
# sensible defaults, and renders breakdown cards. The user's column choice
# persists to overrides.json under `participant_breakdowns`.
# =============================================================================

# Columns we should never treat as demographic breakdowns
.PB_SKIP_PATTERNS <- c(
  "^record_id$", "^redcap_", "_complete$", "^rand_dttm",
  "_dt$", "_date$", "_dttm", "^site_name$", "^site_id$",
  "_timestamp$"
)

# Heuristic: detect columns suitable for breakdowns from a raw frame.
# Returns a data.frame: column, label, type ("numeric" | "categorical"),
# n_unique, n_missing.
detect_breakdown_columns <- function(raw, cfg = NULL) {
  if (is.null(raw) || !nrow(raw)) return(data.frame())

  # Filter to baseline rows (most demographics live there)
  base <- raw
  if (!is.null(cfg) && "redcap_event_name" %in% names(raw)) {
    bevt <- cfg$redcap_events$baseline %||% "baseline_arm_1"
    base <- raw[raw$redcap_event_name == bevt, , drop = FALSE]
  }

  cols <- names(base)
  is_skip <- vapply(cols, function(c)
    any(vapply(.PB_SKIP_PATTERNS, function(p)
      grepl(p, c, ignore.case = TRUE), logical(1))),
    logical(1))
  cols <- cols[!is_skip]

  if (!length(cols)) return(data.frame())

  rows <- lapply(cols, function(c) {
    v <- base[[c]]
    if (is.null(v)) return(NULL)

    # Treat empty strings as NA
    if (is.character(v) || is.factor(v)) v[v == ""] <- NA

    n_total   <- length(v)
    n_missing <- sum(is.na(v))
    n_present <- n_total - n_missing
    if (n_present < 3) return(NULL)

    # Try numeric coercion silently
    v_num <- suppressWarnings(as.numeric(v))
    n_num <- sum(!is.na(v_num))
    is_numeric_like <- n_num >= n_present * 0.8 && n_num >= 3

    if (is_numeric_like) {
      n_unique <- length(unique(v_num[!is.na(v_num)]))
      # Numerics with very few values (e.g. 1/2 sex codes) are categorical
      type <- if (n_unique > 6) "numeric" else "categorical"
    } else {
      n_unique <- length(unique(v[!is.na(v)]))
      if (n_unique > 30) return(NULL)  # too many distinct strings
      type <- "categorical"
    }

    data.frame(
      column    = c,
      label     = .pretty_label(c),
      type      = type,
      n_unique  = n_unique,
      n_missing = n_missing,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

.pretty_label <- function(col) {
  # base_age_at_rand → Age at rand
  s <- gsub("[._]", " ", col)
  s <- sub("^(dem|cae|base|baseline)\\s+", "", s, ignore.case = TRUE)
  paste0(toupper(substring(s, 1, 1)), substring(s, 2))
}

# Default NHS 19-code ethnicity scheme (used when no trial-level mapping set).
.NHS_ETHNICITY_LABELS <- c(
  "1"  = "White British",
  "2"  = "White Irish",
  "3"  = "Any other White",
  "4"  = "White & Black Caribbean",
  "5"  = "White & Black African",
  "6"  = "White & Asian",
  "7"  = "Any other Mixed",
  "8"  = "Indian",
  "9"  = "Pakistani",
  "10" = "Bangladeshi",
  "11" = "Any other Asian",
  "12" = "Caribbean",
  "13" = "African",
  "14" = "Any other Black",
  "15" = "Chinese",
  "16" = "Arab",
  "17" = "Any other ethnic group",
  "18" = "Not stated",
  "19" = "Unknown"
)

# Resolve coded values to human labels using cfg, with sensible fallbacks.
# Looks for cfg[[paste0(col, "_labels")]] first; falls back to NHS scheme
# for columns whose name contains "ethnic".
.resolve_value_labels <- function(values, col, cfg = NULL) {
  if (is.null(values) || !length(values)) return(values)
  values <- as.character(values)

  # Per-column mapping from cfg, e.g. cfg$dem_ethnicity_labels
  mapping <- NULL
  if (!is.null(cfg)) {
    candidate_keys <- c(paste0(col, "_labels"),
                        sub("^(dem|cae|base|baseline)_", "", col),
                        "ethnicity_labels")
    for (k in candidate_keys) {
      if (!is.null(cfg[[k]]) && length(cfg[[k]]) > 0) {
        mapping <- cfg[[k]]; break
      }
    }
  }

  # Fallback: NHS scheme if column looks like ethnicity and values are 1-19
  if (is.null(mapping) && grepl("ethnic", col, ignore.case = TRUE)) {
    if (all(values %in% c(names(.NHS_ETHNICITY_LABELS), NA))) {
      mapping <- .NHS_ETHNICITY_LABELS
    }
  }

  if (is.null(mapping)) return(values)

  # Mapping may be a named character vector or named list
  mapping <- unlist(mapping)
  out <- mapping[values]
  out[is.na(out)] <- values[is.na(out)]   # keep raw value if not in mapping
  unname(out)
}

# Compute breakdown data for one column.
# Returns a list with: type, label, total, missing, headline, segments
# (list of {label, n, pct}).
compute_breakdown <- function(raw, col, cfg = NULL,
                              numeric_breaks = NULL,
                              max_segments = 8) {
  if (is.null(raw) || !nrow(raw) || !col %in% names(raw)) return(NULL)

  base <- raw
  if (!is.null(cfg) && "redcap_event_name" %in% names(raw)) {
    bevt <- cfg$redcap_events$baseline %||% "baseline_arm_1"
    base <- raw[raw$redcap_event_name == bevt, , drop = FALSE]
  }

  v <- base[[col]]
  if (is.character(v) || is.factor(v)) v[v == ""] <- NA

  v_num <- suppressWarnings(as.numeric(v))
  is_numeric_like <- mean(!is.na(v_num)) >= 0.8 &&
                     length(unique(v_num[!is.na(v_num)])) > 6
  total   <- length(v)
  missing <- sum(is.na(v))

  if (is_numeric_like) {
    vals <- v_num[!is.na(v_num)]
    breaks <- numeric_breaks %||% c(-Inf,
                                    quantile(vals, c(.25, .5, .75), names = FALSE),
                                    Inf)
    breaks <- sort(unique(breaks))
    bins <- cut(vals, breaks = breaks, include.lowest = TRUE, right = FALSE,
                dig.lab = 4)
    tab  <- table(bins)
    segments <- lapply(seq_along(tab), function(i) {
      list(label = names(tab)[i],
           n = as.integer(tab[i]),
           pct = if (length(vals)) tab[i] / length(vals) else 0)
    })
    headline <- sprintf("Median %.1f · Mean %.1f", median(vals), mean(vals))
    return(list(type = "numeric", label = .pretty_label(col),
                column = col, total = total, missing = missing,
                headline = headline, segments = segments,
                values_min = min(vals), values_max = max(vals)))
  }

  # Categorical
  vals <- v[!is.na(v)]
  vals <- as.character(vals)
  vals <- .resolve_value_labels(vals, col, cfg)
  tab  <- sort(table(vals), decreasing = TRUE)
  if (length(tab) > max_segments) {
    top <- tab[seq_len(max_segments - 1)]
    other_n <- sum(tab) - sum(top)
    tab <- c(top, Other = other_n)
  }

  segments <- lapply(seq_along(tab), function(i) {
    list(label = names(tab)[i],
         n = as.integer(tab[i]),
         pct = if (length(vals)) tab[i] / length(vals) else 0)
  })
  headline <- sprintf("%d categories · %d records",
                     min(length(unique(v[!is.na(v)])), 99),
                     length(vals))
  list(type = "categorical", label = .pretty_label(col),
       column = col, total = total, missing = missing,
       headline = headline, segments = segments)
}

# Pick sensible defaults if the user hasn't configured anything yet.
default_breakdown_cols <- function(detected) {
  if (!nrow(detected)) return(character(0))
  # Prefer demographic-y names first, then up to 3 columns total.
  pri_pat <- "(?i)age|sex|gender|ethnic|nela|bmi"
  primary <- detected$column[grepl(pri_pat, detected$column)]
  rest <- setdiff(detected$column, primary)
  picks <- c(primary, rest)
  head(picks, 3)
}

# ── Rendering ──────────────────────────────────────────────────────────────
.bd_palette <- c("#6366F1", "#8B5CF6", "#06B6D4", "#10B981",
                 "#F59E0B", "#F43F5E", "#0EA5E9", "#84CC16")

render_breakdown_card <- function(bd) {
  if (is.null(bd)) return(NULL)
  segs <- bd$segments

  # Bars
  bar_rows <- lapply(seq_along(segs), function(i) {
    s <- segs[[i]]
    col <- .bd_palette[((i - 1) %% length(.bd_palette)) + 1]
    pct <- max(0, min(1, s$pct))
    div(style = "margin-bottom:8px;",
        div(style = "display:flex;justify-content:space-between;font-size:11.5px;
                     color:#475569;margin-bottom:3px;",
            span(style = "font-weight:500;color:#0F172A;", s$label),
            span(sprintf("%d  ·  %.0f%%", s$n, pct * 100))),
        div(style = "height:7px;background:#F1F5F9;border-radius:999px;
                     overflow:hidden;",
            div(style = sprintf("height:100%%;width:%.1f%%;background:%s;
                                 border-radius:999px;transition:width .3s;",
                                pct * 100, col))))
  })

  div(style = "background:#FFFFFF;border:1px solid #EEF3F8;border-radius:12px;
               padding:16px 18px;",
      div(style = "display:flex;justify-content:space-between;align-items:baseline;
                   margin-bottom:4px;",
          div(style = "font-weight:600;color:#0F172A;font-size:14px;
                       letter-spacing:-0.1px;",
              bd$label),
          span(style = "font-size:10px;text-transform:uppercase;letter-spacing:.5px;
                        color:#94A3B8;font-weight:600;", bd$type)),
      div(style = "font-size:11.5px;color:#64748B;margin-bottom:14px;",
          bd$headline,
          if (bd$missing > 0)
            span(style = "color:#94A3B8;",
                 sprintf("  ·  %d missing", bd$missing))),
      div(bar_rows))
}

render_breakdowns_grid <- function(breakdowns) {
  breakdowns <- Filter(Negate(is.null), breakdowns)
  if (!length(breakdowns)) {
    return(div(style = "padding:30px 20px;text-align:center;color:#94A3B8;
                        font-size:13px;font-style:italic;",
               div(style = "font-size:24px;margin-bottom:8px;opacity:.4;",
                   HTML("&#x1F4CA;")),
               div("No demographic breakdowns selected."),
               div(style = "font-size:11px;margin-top:4px;",
                   "Click “Configure” to pick columns from the uploaded CSV.")))
  }
  div(style = "display:grid;grid-template-columns:repeat(auto-fill, minmax(280px, 1fr));
               gap:14px;",
      lapply(breakdowns, render_breakdown_card))
}
