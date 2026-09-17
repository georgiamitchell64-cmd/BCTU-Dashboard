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
  base <- baseline_rows(raw, cfg)

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
      # Numerics with very few values (e.g. 1/2 sex codes) are categorical,
      # and so is a coded column with an actual label list however many
      # distinct codes it has (e.g. a 19-code ethnicity scheme).
      type <- if (n_unique > 6 && !.pb_has_value_mapping(v, c, cfg)) "numeric" else "categorical"
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

# Common REDCap abbreviations, spelled out in card titles and labels.
.PB_ABBREVIATIONS <- c(gp = "group", grp = "group", mort = "mortality", nela = "NELA",
                       bmi = "BMI", asa = "ASA", nrs = "NRS", pts = "points",
                       yrs = "years", rand = "randomisation", imd = "IMD", eq5d = "EQ-5D")

.pretty_label <- function(col) {
  # base_age_at_rand → Age at randomisation; base_ethnic_gp → Ethnic group
  s <- gsub("[._]", " ", col)
  s <- sub("^(dem|cae|base|baseline)\\s+", "", s, ignore.case = TRUE)
  w <- strsplit(s, "\\s+")[[1]]
  hit <- tolower(w) %in% names(.PB_ABBREVIATIONS)
  w[hit] <- .PB_ABBREVIATIONS[tolower(w[hit])]
  s <- paste(w, collapse = " ")
  paste0(toupper(substring(s, 1, 1)), substring(s, 2))
}

# A breakdown's title: the trial's own name for it, else the worked-out one
.bd_title <- function(col, cfg = NULL) {
  t <- as.character(unlist(cfg$breakdown_titles[[col]]))
  if (length(t) && !is.na(t[1]) && nzchar(trimws(t[1]))) trimws(t[1]) else .pretty_label(col)
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
  base <- baseline_rows(raw, cfg)
  results <- list()
  for (i in seq_len(nrow(det))) {
    r <- det[i, ]
    if (r$type != "categorical") next
    col <- r$column
    if (!col %in% names(base)) next
    v <- as.character(base[[col]])
    v[v == ""] <- NA
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
  base <- baseline_rows(raw, cfg)
  has <- function(x, k) !is.null(x) && k %in% names(x)
  results <- list()
  for (i in seq_len(nrow(det))) {
    r <- det[i, ]
    if (r$type != "categorical") next
    col <- r$column
    if (!col %in% names(base)) next
    v <- as.character(base[[col]]); v[v == ""] <- NA
    if (!.looks_like_codes(v)) next
    uniq_vals <- sort(unique(v[!is.na(v)]))
    if (!length(uniq_vals)) next

    existing  <- .pb_lookup_saved_mapping(col, cfg); if (is.null(existing)) existing <- list()
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

# ── Codebook ─────────────────────────────────────────────────────────────────
# Every coded column in the export, whether or not it was picked up as a
# demographic breakdown. A REDCap export carries the numbers but not what they
# mean, so this is the surface a trial manager labels once (aetiology,
# severity, withdrawal level, yes/no fields) and every view then reads through
# cfg$column_labels.
.CODEBOOK_SKIP_PATTERNS <- c(
  "^record_id$", "^redcap_", "_complete$", "_timestamp$",
  "_dt$", "_date$", "_dttm", "^site_id$", "_id$", "_instance$"
)

detect_coded_columns <- function(raw, cfg = NULL, max_codes = 25) {
  if (is.null(raw) || !is.data.frame(raw) || !nrow(raw)) return(list())
  base <- baseline_rows(raw, cfg)
  if (!nrow(base)) base <- raw

  cols <- names(base)
  skip <- vapply(cols, function(c)
    any(vapply(.CODEBOOK_SKIP_PATTERNS, function(p)
      grepl(p, c, ignore.case = TRUE), logical(1))), logical(1))
  cols <- cols[!skip]

  has <- function(x, k) !is.null(x) && k %in% names(x)
  out <- list()
  for (col in cols) {
    v <- as.character(base[[col]]); v[v == ""] <- NA
    vals <- v[!is.na(v)]
    if (!length(vals)) next
    v_int <- suppressWarnings(as.integer(vals))
    if (any(is.na(v_int))) next                       # not a coded field
    uniq <- sort(unique(v_int))
    if (!length(uniq) || length(uniq) > max_codes) next
    if (length(uniq) == length(vals) && length(vals) > 3) next   # looks like an id

    existing  <- .pb_lookup_saved_mapping(col, cfg)
    if (is.null(existing)) existing <- list()
    suggested <- .suggest_code_labels(col, as.character(uniq))
    if (is.null(suggested) && grepl("ethnic", col, ignore.case = TRUE))
      suggested <- .NHS_ETHNICITY_LABELS
    if (is.null(suggested)) suggested <- character(0)

    prefill <- setNames(vapply(as.character(uniq), function(val) {
      e <- if (has(existing, val)) existing[[val]] else NULL
      if (!is.null(e) && nzchar(as.character(e))) return(as.character(e))
      sg <- if (has(suggested, val)) suggested[[val]] else NULL
      if (!is.null(sg) && nzchar(as.character(sg))) return(as.character(sg)) 
      ""
    }, character(1)), as.character(uniq))

    out[[length(out) + 1]] <- list(
      col       = col,
      label     = .pretty_label(col),
      values    = as.character(uniq),
      n         = length(vals),
      labelled  = length(existing) > 0,
      suggested = as.list(prefill))
  }
  out
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

# A trial's own saved mapping for a column — overrides.json's column_labels,
# or a legacy per-column key such as cfg$ethnicity_labels — with no fallback
# to a built-in scheme. Shared by resolve_value_labels() (what's actually
# shown) and the codebook editor (what counts as "already labelled", so it
# doesn't offer a generic suggestion that would overwrite a trial's real
# mapping if saved).
.pb_lookup_saved_mapping <- function(col, cfg = NULL) {
  # 1. User-defined per-column labels saved in overrides.json → cfg$column_labels
  if (!is.null(cfg) && !is.null(cfg$column_labels[[col]]) &&
      length(cfg$column_labels[[col]]) > 0) {
    return(unlist(cfg$column_labels[[col]]))
  }

  # 2. Legacy per-column mapping from cfg, e.g. cfg$dem_ethnicity_labels
  if (!is.null(cfg)) {
    candidate_keys <- c(paste0(col, "_labels"),
                        sub("^(dem|cae|base|baseline)_", "", col),
                        "ethnicity_labels")
    for (k in candidate_keys) {
      if (!is.null(cfg[[k]]) && length(cfg[[k]]) > 0) return(cfg[[k]])
    }
  }
  NULL
}

# Resolve coded values to human labels using cfg, with sensible fallbacks.
# Looks for cfg[[paste0(col, "_labels")]] first; falls back to NHS scheme
# for columns whose name contains "ethnic".
.resolve_value_labels <- function(values, col, cfg = NULL) {
  if (is.null(values) || !length(values)) return(values)
  values <- as.character(values)

  mapping <- .pb_lookup_saved_mapping(col, cfg)

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

# Whether resolve_value_labels() would actually translate at least one of a
# column's distinct values — i.e. a real code list exists for it, from the
# trial's overrides, a legacy per-column mapping, or (for an ethnicity-named
# column) the NHS fallback. Used to keep a coded column categorical however
# many distinct codes it has: the numeric-vs-categorical heuristic below is
# for genuinely continuous fields (age, BMI, NELA score), and was wrongly
# catching a 19-code ethnicity scheme — more than its 6-unique-value cutoff
# — and showing it as a numeric range instead of resolving the labels.
.pb_has_value_mapping <- function(values, col, cfg = NULL) {
  vals <- unique(as.character(values[!is.na(values)]))
  if (!length(vals)) return(FALSE)
  any(.resolve_value_labels(vals, col, cfg) != vals)
}

# Grouping for a numeric breakdown, or NULL. Defaults: NELA predicted
# mortality splits at 5% (the minimisation groups in functions/baseline_table.R)
# and age at 75. "Customise demographics" on the Data tab saves a trial's own
# choices as cfg$breakdown_cuts = list(<column> = list(cut = c(...), unit, title));
# an entry with no cut switches grouping off for that column.
.parse_cuts <- function(x) {
  v <- suppressWarnings(as.numeric(unlist(strsplit(as.character(unlist(x)), "[,;[:space:]]+"))))
  sort(unique(v[!is.na(v)]))
}
.role_col <- function(role, default) {
  if (!exists("fld", mode = "function")) return(default)
  tryCatch(fld(role, default = default), error = function(e) default)
}

breakdown_cut <- function(col, cfg = NULL) {
  own <- cfg$breakdown_cuts[[col]]
  if (!is.null(own)) {
    cuts <- .parse_cuts(own$cut)
    if (!length(cuts)) return(NULL)
    u <- as.character(unlist(own$unit)); t <- as.character(unlist(own$title))
    return(list(cut = cuts, unit = if (length(u) && !is.na(u[1])) u[1] else "",
                title = if (length(t) && !is.na(t[1]) && nzchar(t[1])) t[1] else "Groups"))
  }
  if (identical(col, .role_col("nela_score", "base_nela_score_mort")))
    return(list(cut = 5, unit = "%", title = "Minimisation groups"))
  if (identical(col, .role_col("age", "cae_age")))
    return(list(cut = 75, unit = "", title = "Age groups"))
  NULL
}

# Counts in each group: one cut makes "Under x" and "x or over" (x < cut,
# x >= cut); several make "a to under b" bands between them.
.breakdown_split <- function(vals, spec) {
  if (is.null(spec) || !length(spec$cut)) return(NULL)
  cuts <- spec$cut; u <- spec$unit %||% ""; k <- length(cuts)
  f <- function(x) paste0(.bd_num(x), u)
  counts <- as.integer(table(cut(vals, c(-Inf, cuts, Inf), right = FALSE)))
  labels <- c(paste("Under", f(cuts[1])),
              if (k > 1) sprintf("%s to under %s", vapply(cuts[-k], f, ""), vapply(cuts[-1], f, "")),
              paste(f(cuts[k]), "or over"))
  list(cut = cuts, unit = u, title = spec$title %||% "Groups", n = length(vals),
       labels = labels, counts = counts, under = counts[1], over = counts[length(counts)])
}

# Compute breakdown data for one column.
# Returns a list with: type, label, total, missing, headline, segments
# (list of {label, n, pct}).
#   numeric_bins – "quartile" (the report's bands) or "pretty" (round-number
#                  bands, used on the Data tab so the spread actually shows).
compute_breakdown <- function(raw, col, cfg = NULL,
                              numeric_breaks = NULL,
                              max_segments = 8,
                              numeric_bins = c("quartile", "pretty")) {
  numeric_bins <- match.arg(numeric_bins)
  if (is.null(raw) || !nrow(raw) || !col %in% names(raw)) return(NULL)

  base <- baseline_rows(raw, cfg)

  v <- base[[col]]
  if (is.character(v) || is.factor(v)) v[v == ""] <- NA

  v_num <- suppressWarnings(as.numeric(v))
  is_numeric_like <- mean(!is.na(v_num)) >= 0.8 &&
                     length(unique(v_num[!is.na(v_num)])) > 6 &&
                     !.pb_has_value_mapping(v, col, cfg)
  total   <- length(v)
  missing <- sum(is.na(v))

  if (is_numeric_like) {
    vals <- v_num[!is.na(v_num)]
    # Quartile bands hold ~25% each by construction; round-number bands show
    # where participants actually sit.
    breaks <- numeric_breaks %||% switch(numeric_bins,
      quartile = c(-Inf, quantile(vals, c(.25, .5, .75), names = FALSE), Inf),
      pretty   = pretty(range(vals), n = 5))
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
    hb <- pretty(range(vals), n = 10)
    hc <- as.integer(table(cut(vals, breaks = hb, include.lowest = TRUE, right = FALSE)))
    return(list(type = "numeric", label = .bd_title(col, cfg),
                column = col, total = total, missing = missing,
                headline = headline, segments = segments,
                values_min = min(vals), values_max = max(vals),
                stats = list(n = length(vals), median = stats::median(vals),
                             q1 = stats::quantile(vals, .25, names = FALSE),
                             q3 = stats::quantile(vals, .75, names = FALSE),
                             min = min(vals), max = max(vals), mean = mean(vals)),
                hist = list(breaks = hb, counts = hc),
                mini = .breakdown_split(vals, breakdown_cut(col, cfg))))
  }

  # Categorical
  raw_vals <- as.character(v[!is.na(v)])
  vals <- .resolve_value_labels(raw_vals, col, cfg)
  # Coded values the labels don't cover (e.g. only 4 of a 19-code scheme named)
  unlabelled <- unique(raw_vals[vals == raw_vals & grepl("^-?[0-9]+$", raw_vals)])
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
  list(type = "categorical", label = .bd_title(col, cfg),
       column = col, total = total, missing = missing,
       headline = headline, segments = segments,
       unlabelled = unlabelled,
       partly_labelled = length(unlabelled) > 0 && any(vals != raw_vals))
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
.bd_palette <- c("#0057BF", "#1B1B1B", "#00ACA9", "#3AAA35",
                 "#F07F3C", "#C59A00", "#2581C4", "#84CC16")

# Demographic cards are trial charts, so they take the trial's colours: its
# three brand colours, then lighter tints of them, then grey for "Other".
.bd_colours <- function(n) {
  pal <- tryCatch(trial_palette(),
                  error = function(e) c(primary = "#1B1B1B", secondary = "#00788E", accent = "#C59A00"))
  tint <- function(hex, w) {
    c <- grDevices::col2rgb(hex)[, 1]
    grDevices::rgb(c[1] + (255 - c[1]) * w, c[2] + (255 - c[2]) * w,
                   c[3] + (255 - c[3]) * w, maxColorValue = 255)
  }
  base <- c(pal[["primary"]], pal[["secondary"]], pal[["accent"]],
            tint(pal[["primary"]], .45), tint(pal[["secondary"]], .45), tint(pal[["accent"]], .45),
            tint(pal[["primary"]], .7), "#9A9A9C")
  rep_len(base, n)
}

# Whole numbers without decimals, everything else to one place
.bd_num <- function(x) {
  if (is.null(x) || !length(x) || is.na(x)) return("—")
  if (abs(x - round(x)) < 1e-9) format(round(x), big.mark = ",") else sprintf("%.1f", x)
}

# A coded value with no name reads as "Code 7" rather than a bare number
.bd_seg_label <- function(s, bd) {
  if (isTRUE(bd$partly_labelled) && s$label %in% (bd$unlabelled %||% character(0)))
    paste("Code", s$label) else s$label
}

# One headline figure per breakdown, shown above the cards.
render_demographics_strip <- function(breakdowns) {
  bds <- Filter(function(b) !is.null(b) && b$type %in% c("numeric", "categorical"), breakdowns)
  if (!length(bds)) return(NULL)
  kpi <- function(bd) {
    if (identical(bd$type, "numeric") && !is.null(bd$mini) && length(bd$mini$cut) == 1) {
      # A single split (NELA 5%, age 75) leads with the share at or above it
      m <- bd$mini; cut <- paste0(.bd_num(m$cut), m$unit)
      div(class = "dm-kpi",
          div(class = "dm-kpi-l", bd$label),
          div(class = "dm-kpi-v", sprintf("%.0f%%", 100 * m$over / max(1, m$n)),
              span(class = "dm-kpi-u", paste0("≥", cut))),
          div(class = "dm-kpi-s", sprintf("%d under %s · median %s", m$under, cut,
                                          .bd_num(bd$stats$median))))
    } else if (identical(bd$type, "numeric")) {
      st <- bd$stats
      div(class = "dm-kpi",
          div(class = "dm-kpi-l", bd$label),
          div(class = "dm-kpi-v", .bd_num(st$median), span(class = "dm-kpi-u", "median")),
          div(class = "dm-kpi-s", sprintf("IQR %s–%s · range %s–%s", .bd_num(st$q1),
                                          .bd_num(st$q3), .bd_num(st$min), .bd_num(st$max))))
    } else {
      top <- bd$segments[[1]]
      lab <- .bd_seg_label(top, bd)
      div(class = "dm-kpi",
          div(class = "dm-kpi-l", bd$label),
          div(class = "dm-kpi-v", sprintf("%.0f%%", 100 * top$pct)),
          div(class = "dm-kpi-s", title = lab, lab))
    }
  }
  div(class = "dm-kpis", lapply(bds, kpi))
}

.bd_categorical_body <- function(bd) {
  segs <- bd$segments
  cols <- .bd_colours(length(segs))
  lab  <- function(s) .bd_seg_label(s, bd)
  stack <- div(class = "dm-stack", role = "img",
    `aria-label` = paste(vapply(segs, function(s) sprintf("%s %.0f%%", lab(s), 100 * s$pct), ""),
                         collapse = ", "),
    lapply(seq_along(segs), function(i) {
      s <- segs[[i]]
      span(class = "dm-stack-seg", title = sprintf("%s: %d (%.0f%%)", lab(s), s$n, 100 * s$pct),
           style = sprintf("width:%.2f%%;background:%s;", 100 * max(0, min(1, s$pct)), cols[i]))
    }))
  rows <- lapply(seq_along(segs), function(i) {
    s <- segs[[i]]
    no_name <- isTRUE(bd$partly_labelled) && s$label %in% bd$unlabelled
    div(class = "dm-row",
        span(class = "dm-sw", style = sprintf("background:%s;", cols[i])),
        span(class = "dm-row-l", title = lab(s), lab(s), if (no_name) span(class = "dm-tag", "no name")),
        span(class = "dm-row-n", s$n),
        span(class = "dm-row-p", sprintf("%.0f%%", 100 * s$pct)))
  })
  n_unl <- if (isTRUE(bd$partly_labelled)) length(bd$unlabelled) else 0
  tagList(stack, div(class = "dm-rows", rows),
          if (n_unl > 0)
            div(class = "dm-foot",
                sprintf("%d code%s here %s no name yet. ", n_unl, if (n_unl == 1) "" else "s",
                        if (n_unl == 1) "has" else "have"),
                tags$a(href = "#", class = "dm-link",
                       # Opens Customise demographics on this column
                       onclick = sprintf("Shiny.setInputValue('bd_cfg_focus','%s'); document.getElementById('configure_breakdowns').click(); return false;",
                                         gsub("'", "\\\\'", bd$column)),
                       "Name them")))
}

.bd_numeric_body <- function(bd) {
  st <- bd$stats; br <- bd$hist$breaks; cn <- bd$hist$counts
  W <- 300; H <- 72
  x_at <- function(x) (x - br[1]) / (br[length(br)] - br[1]) * W
  top  <- max(cn, 1)
  col  <- .bd_colours(1)[1]
  bars <- vapply(seq_along(cn), function(i) {
    h <- if (cn[i] > 0) max(2, cn[i] / top * (H - 4)) else 0
    x0 <- x_at(br[i]); x1 <- x_at(br[i + 1])
    sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" fill="%s"><title>%s to %s: %d</title></rect>',
            x0 + 1, H - h, max(0, x1 - x0 - 2), h, col, .bd_num(br[i]), .bd_num(br[i + 1]), cn[i])
  }, "")
  # Groups (NELA <5% / ≥5%, age under / over 75, or a trial's own bands): the
  # split itself, and a line on the histogram at each cut
  m <- bd$mini
  cuts_in  <- if (is.null(m)) numeric(0) else m$cut[m$cut >= br[1] & m$cut <= br[length(br)]]
  cut_line <- paste(vapply(cuts_in, function(cv)
    sprintf('<line class="dm-cut" x1="%.1f" x2="%.1f" y1="0" y2="%d" vector-effect="non-scaling-stroke"/>',
            x_at(cv), x_at(cv), H), ""), collapse = "")
  cut_lbl  <- if (!is.null(m) && length(m$cut) == 1 && length(cuts_in))
    span(class = "dm-cut-lbl", style = sprintf("left:%.2f%%;", 100 * x_at(m$cut) / W),
         paste(paste0(.bd_num(m$cut), m$unit), "cut-off"))
  mini_ui  <- if (!is.null(m)) {
    k <- length(m$counts)
    cols  <- if (k == 2) .bd_colours(2)[2:1] else .bd_colours(k)
    share <- function(n) 100 * n / max(1, m$n)
    div(class = "dm-mini",
        div(class = "dm-mini-t", m$title),
        div(class = "dm-stack", role = "img",
            `aria-label` = paste(sprintf("%s: %d", m$labels, m$counts), collapse = ", "),
            lapply(seq_len(k), function(i)
              span(class = "dm-stack-seg",
                   title = sprintf("%s: %d (%.0f%%)", m$labels[i], m$counts[i], share(m$counts[i])),
                   style = sprintf("width:%.2f%%;background:%s;", share(m$counts[i]), cols[i])))),
        div(class = "dm-rows", lapply(seq_len(k), function(i)
          div(class = "dm-row",
              span(class = "dm-sw", style = sprintf("background:%s;", cols[i])),
              span(class = "dm-row-l", m$labels[i]),
              span(class = "dm-row-n", m$counts[i]),
              span(class = "dm-row-p", sprintf("%.0f%%", share(m$counts[i])))))))
  }
  svg <- sprintf(paste0(
    '<svg class="dm-hist" viewBox="0 0 %d %d" preserveAspectRatio="none" role="img" aria-label="%s">',
    '<rect class="dm-iqr" x="%.1f" y="0" width="%.1f" height="%d"/>%s',
    '<line class="dm-med" x1="%.1f" x2="%.1f" y1="0" y2="%d" vector-effect="non-scaling-stroke"/>%s</svg>'),
    W, H, htmltools::htmlEscape(sprintf("%s: median %s, middle half %s to %s", bd$label,
                                        .bd_num(st$median), .bd_num(st$q1), .bd_num(st$q3))),
    x_at(st$q1), x_at(st$q3) - x_at(st$q1), H, paste(bars, collapse = ""),
    x_at(st$median), x_at(st$median), H, cut_line)
  ticks <- if (length(br) > 7) br[seq(1, length(br), by = 2)] else br
  stat  <- function(l, v) div(class = "dm-stat", div(class = "dm-stat-v", v), div(class = "dm-stat-l", l))
  tagList(
    mini_ui,
    div(class = "dm-hist-wrap", HTML(svg), cut_lbl,
        div(class = "dm-axis", lapply(ticks, function(t)
          span(style = sprintf("left:%.2f%%;", 100 * x_at(t) / W), .bd_num(t))))),
    div(class = "dm-legend", span(class = "dm-lg-iqr"), "middle half",
        span(class = "dm-lg-med"), "median"),
    div(class = "dm-stats",
        stat("Median", .bd_num(st$median)),
        stat("IQR", sprintf("%s–%s", .bd_num(st$q1), .bd_num(st$q3))),
        stat("Range", sprintf("%s–%s", .bd_num(st$min), .bd_num(st$max))),
        stat("Mean", .bd_num(st$mean))))
}

render_breakdown_card <- function(bd) {
  if (is.null(bd)) return(NULL)
  if (identical(bd$type, "absent"))
    return(div(class = "dm-card dm-card-absent",
               div(class = "dm-card-head", div(class = "dm-card-t", bd$label)),
               div(class = "dm-empty", sprintf("The column “%s” isn't in this export.", bd$column))))
  n_ok <- bd$total - bd$missing
  div(class = "dm-card",
      div(class = "dm-card-head",
          div(class = "dm-card-t", bd$label),
          div(class = "dm-card-n", sprintf("n = %d", n_ok),
              if (bd$missing > 0)
                span(class = "dm-miss", sprintf(" · %d missing (%.0f%%)", bd$missing,
                                                100 * bd$missing / max(1, bd$total))))),
      if (identical(bd$type, "numeric")) .bd_numeric_body(bd) else .bd_categorical_body(bd))
}

render_breakdowns_grid <- function(breakdowns) {
  breakdowns <- Filter(Negate(is.null), breakdowns)
  if (!length(breakdowns))
    return(div(class = "dm-none",
               div("No demographic breakdowns selected."),
               div(class = "dm-none-s", "Use “Configure” below to pick columns from the export.")))
  div(class = "dm-grid", lapply(breakdowns, render_breakdown_card))
}
