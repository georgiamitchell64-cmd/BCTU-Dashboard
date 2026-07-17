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

# ── Participant-level value extraction ───────────────────────────────────────
# A REDCap export is one row per participant-EVENT, so reading a demographic
# straight off the baseline-event rows silently loses every participant whose
# value was entered on a different event/row (consent events, repeat-instrument
# rows, multi-arm event names). These helpers collapse the frame to ONE value
# per participant: the first non-empty value, scanning the mapped baseline
# event's rows first and every other row after, so counts always cover the
# whole cohort.

.pb_id_col <- function(raw, cfg = NULL) {
  id <- cfg$redcap_fields$record_id %||% "record_id"
  if (!id %in% names(raw))
    id <- if ("record_id" %in% names(raw)) "record_id" else names(raw)[1]
  id
}

# Build once, reuse for every column: a list of row indices per participant,
# ordered so baseline-event rows are scanned first. `%in%` (not `==`) so
# multi-event baseline mappings and NA event names are handled.
pb_participant_index <- function(raw, cfg = NULL) {
  ids <- trimws(as.character(raw[[.pb_id_col(raw, cfg)]]))
  ok  <- !is.na(ids) & nzchar(ids)
  ord <- seq_len(nrow(raw))
  if ("redcap_event_name" %in% names(raw)) {
    bevt <- cfg$redcap_events$baseline %||% "baseline_arm_1"
    is_base <- raw$redcap_event_name %in% bevt
    ord <- order(!is_base)          # stable: baseline rows first
  }
  ord <- ord[ok[ord]]
  split(ord, ids[ord])
}

# One value per participant for `col` (character; NA when never recorded).
pb_first_values <- function(raw, col, idx) {
  v  <- raw[[col]]
  vc <- trimws(as.character(v))
  has <- !is.na(v) & nzchar(vc)
  vapply(idx, function(r) {
    nz <- r[has[r]]
    if (length(nz)) vc[nz[1]] else NA_character_
  }, character(1))
}

# Heuristic: detect columns suitable for breakdowns from a raw frame.
# Returns a data.frame: column, label, type ("numeric" | "categorical"),
# n_unique, n_missing. Counts are per PARTICIPANT (values coalesced across
# events), not per row.
detect_breakdown_columns <- function(raw, cfg = NULL) {
  if (is.null(raw) || !nrow(raw)) return(data.frame())

  idx <- pb_participant_index(raw, cfg)

  cols <- names(raw)
  is_skip <- vapply(cols, function(c)
    any(vapply(.PB_SKIP_PATTERNS, function(p)
      grepl(p, c, ignore.case = TRUE), logical(1))),
    logical(1))
  cols <- cols[!is_skip]

  if (!length(cols)) return(data.frame())

  rows <- lapply(cols, function(c) {
    v <- pb_first_values(raw, c, idx)

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

# Format a cut() bin label like "[-Inf,35)" into "< 35", "35–55", "68+".
.format_bin_label <- function(lbl) {
  lbl <- as.character(lbl)
  m <- regmatches(lbl,
    regexpr("^[\\[(](-?Inf|[0-9.e+\\-]+),\\s*(-?Inf|[0-9.e+\\-]+)[\\])]$",
            lbl, perl = TRUE))
  if (!length(m) || !nzchar(m)) return(lbl)
  inner <- substr(m, 2, nchar(m) - 1)
  parts <- strsplit(inner, ",")[[1]]
  lo <- trimws(parts[1]); hi <- trimws(parts[2])
  fmt_n <- function(x) {
    n <- suppressWarnings(as.numeric(x))
    if (is.na(n)) return(x)
    if (n == round(n)) as.character(as.integer(n)) else sprintf("%.1f", n)
  }
  if (lo == "-Inf") return(paste0("< ",   fmt_n(hi)))
  if (hi == "Inf")  return(paste0(fmt_n(lo), "+"))
  paste0(fmt_n(lo), "–", fmt_n(hi))
}

# Built-in suggestions for common column patterns.
# Applied when a column has no explicit mapping and matches a pattern.
.KNOWN_CODE_SUGGESTIONS <- list(
  list(pattern = "(?i)(^|_)(sex|gender)(_|$)",
       labels  = c("1" = "Male", "2" = "Female", "3" = "Other / prefer not to say")),
  list(pattern = "(?i)yes_no|(_yn$)|(^yn_)",
       labels  = c("0" = "No", "1" = "Yes")),
  list(pattern = "(?i)(smoker|smoking|smoke)",
       labels  = c("0" = "No", "1" = "Yes", "2" = "Ex-smoker")),
  list(pattern = "(?i)(asthma|diabetes|hypertens|comorbid|cardiac|renal)",
       labels  = c("0" = "No", "1" = "Yes"))
)

.suggest_code_labels <- function(col, values) {
  values <- setdiff(as.character(values), NA_character_)
  for (s in .KNOWN_CODE_SUGGESTIONS) {
    if (grepl(s$pattern, col, perl = TRUE) &&
        all(values %in% names(s$labels))) return(s$labels)
  }
  NULL
}

# Returns TRUE if all non-NA values in the vector look like small integers
# (i.e. likely coded values with no label mapping).
.looks_like_codes <- function(v) {
  vals <- v[!is.na(v) & nzchar(as.character(v))]
  if (!length(vals)) return(FALSE)
  v_int <- suppressWarnings(as.integer(as.character(vals)))
  all(!is.na(v_int)) && all(v_int >= 0) && length(unique(v_int)) <= 10
}

# Find columns in `det` (detect_breakdown_columns result) that appear to be
# numeric codes with no resolved label mapping in cfg or built-in suggestions.
# Returns a list of lists: col, label, values, suggested (named char vec or NULL).
find_unmapped_code_cols <- function(raw, cfg, det) {
  if (is.null(raw) || !nrow(raw) || !nrow(det)) return(list())
  idx <- pb_participant_index(raw, cfg)
  results <- list()
  for (i in seq_len(nrow(det))) {
    r <- det[i, ]
    if (r$type != "categorical") next
    col <- r$column
    if (!col %in% names(raw)) next
    v <- pb_first_values(raw, col, idx)
    if (!.looks_like_codes(v)) next
    # Skip if cfg already has a label mapping for this column
    mapping <- cfg$column_labels[[col]] %||% NULL
    if (!is.null(mapping) && length(mapping) > 0) next
    # Skip ethnicity — handled separately by NHS scheme
    if (grepl("ethnic", col, ignore.case = TRUE)) next
    uniq_vals <- sort(unique(v[!is.na(v)]))
    suggested <- .suggest_code_labels(col, uniq_vals)
    results[[length(results) + 1]] <- list(
      col       = col,
      label     = r$label,
      values    = uniq_vals,
      suggested = suggested
    )
  }
  results
}

# Like find_unmapped_code_cols(), but returns EVERY coded categorical column
# (including ones already labelled) with each value's current label pre-filled —
# existing override wins, then a built-in suggestion, then blank. This powers
# the "edit / rename groupings" view so saved labels can be changed later.
find_editable_code_cols <- function(raw, cfg, det) {
  if (is.null(raw) || !nrow(raw) || is.null(det) || !nrow(det)) return(list())
  idx <- pb_participant_index(raw, cfg)
  has <- function(x, k) !is.null(x) && k %in% names(x)
  results <- list()
  for (i in seq_len(nrow(det))) {
    r <- det[i, ]
    if (r$type != "categorical") next
    col <- r$column
    if (!col %in% names(raw)) next
    v <- pb_first_values(raw, col, idx)
    if (!.looks_like_codes(v)) next
    uniq_vals <- sort(unique(v[!is.na(v)]))
    if (!length(uniq_vals)) next

    existing  <- cfg$column_labels[[col]]; if (is.null(existing)) existing <- list()
    suggested <- .suggest_code_labels(col, uniq_vals)
    if (is.null(suggested) && grepl("ethnic", col, ignore.case = TRUE))
      suggested <- .NHS_ETHNICITY_LABELS
    if (is.null(suggested)) suggested <- character(0)

    prefill <- setNames(vapply(uniq_vals, function(val) {
      e <- if (has(existing, val)) existing[[val]] else NULL
      if (!is.null(e) && nzchar(as.character(e))) return(as.character(e))
      s <- if (has(suggested, val)) suggested[[val]] else NULL
      if (!is.null(s) && nzchar(as.character(s))) return(as.character(s))
      ""
    }, character(1)), uniq_vals)

    results[[length(results) + 1]] <- list(
      col       = col,
      label     = r$label,
      values    = uniq_vals,
      labelled  = length(existing) > 0,
      suggested = as.list(prefill))
  }
  results
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

  # 1. User-defined per-column labels saved in overrides.json → cfg$column_labels
  mapping <- NULL
  if (!is.null(cfg) && !is.null(cfg$column_labels[[col]]) &&
      length(cfg$column_labels[[col]]) > 0) {
    mapping <- unlist(cfg$column_labels[[col]])
  }

  # 2. Legacy per-column mapping from cfg, e.g. cfg$dem_ethnicity_labels
  if (is.null(mapping) && !is.null(cfg)) {
    candidate_keys <- c(paste0(col, "_labels"),
                        sub("^(dem|cae|base|baseline)_", "", col),
                        "ethnicity_labels")
    for (k in candidate_keys) {
      if (!is.null(cfg[[k]]) && length(cfg[[k]]) > 0) {
        mapping <- cfg[[k]]; break
      }
    }
  }

  # 3. NHS ethnicity fallback
  if (is.null(mapping) && grepl("ethnic", col, ignore.case = TRUE)) {
    if (all(values %in% c(names(.NHS_ETHNICITY_LABELS), NA))) {
      mapping <- .NHS_ETHNICITY_LABELS
    }
  }

  # 4. Built-in suggestions for common patterns (sex, yes/no, etc.)
  if (is.null(mapping)) {
    mapping <- .suggest_code_labels(col, values)
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

  # One value per PARTICIPANT, coalesced across every row/event (baseline
  # rows first). Previously this read only the baseline-event rows, so any
  # participant whose demographics were recorded on another event/row was
  # silently missing from the counts.
  idx <- pb_participant_index(raw, cfg)
  v   <- pb_first_values(raw, col, idx)

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
      list(label = .format_bin_label(names(tab)[i]),
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
  headline <- sprintf("%d categories · %d participants",
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
