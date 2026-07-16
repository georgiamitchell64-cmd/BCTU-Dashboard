# =============================================================================
# TONIC Trial — Flat completeness table helper
# =============================================================================
# Shared helper used by the Shiny app, the TMG (HTML) Rmd, and the TSC (Word)
# Rmd. It defines a protocol-ordered list of forms + timepoints, then looks
# up live numbers from the return-rate CSV (rd$crf_data or a tidied version).
#
# Public functions:
#   flat_completeness_spec()
#     → tibble with columns: form_display, timepoint_display, csv_form, csv_event
#
#   flat_completeness_df(crf_df, rate_type = c("pct_due", "pct_expected"))
#     → tibble ready for display, columns:
#         form_display, timepoint_display, n_expected, entered, pct
#
# Rows listed in the protocol but not yet in the export show NA values.
# The caller decides how to render NA ("—" is the usual choice).
# =============================================================================

flat_completeness_spec <- function() {
  tibble::tribble(
    ~form_display,               ~timepoint_display,   ~csv_form,               ~csv_event,
    "Consent & Eligibility",     "Screening",          "Consent & Eligibility", "Baseline",
    "Baseline Data",             "Baseline",           "Baseline Data",         "Baseline",
    "Baseline Nutrition",        "Baseline",           "Baseline Nutrition",    "Baseline",
    "Randomisation",             "Baseline",           "Randomisation",         "Baseline",
    "Contact Details",           "Baseline",           "Contact Details",       "Baseline",
    "Index Operation",           "Baseline",           "Index Operation",       "Baseline",
    "Discharge",                 "Discharge",          "Discharge",             "Discharge",
    "Surgical Complications",    "Discharge",          NA_character_,           NA_character_,
    "Surgical Complications",    "Day 30 post-op",     NA_character_,           NA_character_,
    "Surgical Complications",    "Day 90 post-op",     NA_character_,           NA_character_,
    "Nutritional Outcomes",      "Discharge",          "Nutritional Outcomes",  "Discharge",
    "Nutritional Outcomes",      "Day 30 post-op",     NA_character_,           NA_character_,
    "PN Complications",          "Discharge",          NA_character_,           NA_character_,
    "PN Complications",          "Day 30 post-op",     NA_character_,           NA_character_,
    "PN Complications",          "Day 90 post-op",     NA_character_,           NA_character_,
    "Line Complications",        "Discharge",          NA_character_,           NA_character_,
    "Line Complications",        "Day 30 post-op",     NA_character_,           NA_character_,
    "Line Complications",        "Day 90 post-op",     NA_character_,           NA_character_,
    "Post Operation",            "Day 30 post-op",     "Post Operation",        "Day 30",
    "Post Operation",            "Day 90 post-op",     "Post Operation",        "Day 90",
    "HRUQ",                      "Baseline",           "HRUQ",                  "Baseline",
    "HRUQ",                      "Day 30 post-op",     "HRUQ",                  "Day 30",
    "HRUQ",                      "Day 90 post-op",     "HRUQ",                  "Day 90",
    "Barthel Index",             "Discharge",          "Barthel Index",         "Discharge",
    "Barthel Index",             "Day 30 post-op",     "Barthel Index",         "Day 30",
    "Barthel Index",             "Day 90 post-op",     "Barthel Index",         "Day 90",
    "EQ-5D-5L",                  "Baseline",           "EQ-5D",                 "Baseline",
    "EQ-5D-5L",                  "Discharge",          "EQ-5D",                 "Discharge",
    "EQ-5D-5L",                  "Day 30 post-op",     "EQ-5D",                 "Day 30",
    "EQ-5D-5L",                  "Day 90 post-op",     "EQ-5D",                 "Day 90",
    "PRO-diGI",                  "Discharge",          "PRO-diGI",              "Discharge",
    "PRO-diGI",                  "Day 30 post-op",     "PRO-diGI",              "Day 30",
    "PRO-diGI",                  "Day 90 post-op",     "PRO-diGI",              "Day 90",
    "QoR-15",                    "Discharge",          "QoR-15",                "Discharge",
    "QoR-15",                    "Day 30 post-op",     "QoR-15",                "Day 30",
    "QoR-15",                    "Day 90 post-op",     "QoR-15",                "Day 90",
    "Treatment Satisfaction VAS","Day 30 post-op",     "Patient Satisfaction",  "Day 30",
    "Treatment Satisfaction VAS","Day 90 post-op",     "Patient Satisfaction",  "Day 90"
  )
}

# ─────────────────────────────────────────────────────────────────────────────
#' Build a display-ready flat completeness data frame
#'
#' @param crf_df    The CRF data frame, either the raw CSV (with columns
#'                  Site, Event, Form, Expected, Due, Entered, ...) or a
#'                  tidied version. This function is robust to either.
#' @param rate_type "pct_due" or "pct_expected" — which denominator to use
#' @return tibble with columns: form_display, timepoint_display,
#'                              n_expected (due or expected depending on rate_type),
#'                              entered, pct
# ─────────────────────────────────────────────────────────────────────────────
flat_completeness_df <- function(crf_df, rate_type = "pct_due") {

  if (is.null(crf_df) || nrow(crf_df) == 0) {
    spec <- flat_completeness_spec()
    return(tibble::tibble(
      form_display      = spec$form_display,
      timepoint_display = spec$timepoint_display,
      n_expected        = NA_integer_,
      entered           = NA_integer_,
      pct               = NA_real_
    ))
  }

  # Filter to .Overall rows (if that column exists) — otherwise use all rows
  df <- if ("Site" %in% names(crf_df)) {
    crf_df[crf_df$Site == ".Overall", , drop = FALSE]
  } else if ("site" %in% names(crf_df)) {
    crf_df[crf_df$site == ".Overall", , drop = FALSE]
  } else {
    crf_df
  }

  # Normalise column names to lowercase for lookup
  colmap <- setNames(names(df), tolower(names(df)))
  get_col <- function(nm) df[[colmap[[nm]]]]

  to_num <- function(x) suppressWarnings(as.numeric(gsub("%", "", as.character(x))))

  # Build a lookup by form + event
  spec <- flat_completeness_spec()
  n_rows <- nrow(spec)

  n_expected <- integer(n_rows)
  entered    <- integer(n_rows)
  pct        <- numeric(n_rows)

  for (i in seq_len(n_rows)) {
    csv_form  <- spec$csv_form[i]
    csv_event <- spec$csv_event[i]

    if (is.na(csv_form) || is.na(csv_event)) {
      n_expected[i] <- NA_integer_
      entered[i]    <- NA_integer_
      pct[i]        <- NA_real_
      next
    }

    match_rows <- which(get_col("form") == csv_form &
                        as.character(get_col("event")) == csv_event)

    if (length(match_rows) == 0) {
      n_expected[i] <- NA_integer_
      entered[i]    <- NA_integer_
      pct[i]        <- NA_real_
      next
    }

    r <- match_rows[1]
    ex <- as.integer(to_num(get_col("expected"))[r])
    du <- as.integer(to_num(get_col("due"))[r])
    en <- as.integer(to_num(get_col("entered"))[r])

    n_expected[i] <- if (rate_type == "pct_due") du else ex
    entered[i]    <- en

    denom <- if (rate_type == "pct_due") du else ex
    pct[i] <- if (!is.na(denom) && denom > 0 && !is.na(en)) {
      round(en / denom * 100, 1)
    } else {
      NA_real_
    }
  }

  tibble::tibble(
    form_display      = spec$form_display,
    timepoint_display = spec$timepoint_display,
    n_expected        = n_expected,
    entered           = entered,
    pct               = pct
  )
}

# ─────────────────────────────────────────────────────────────────────────────
#' Render the flat table as HTML (for the TMG Rmd report)
#'
#' @param df           Output of flat_completeness_df()
#' @param rate_type    "pct_due" or "pct_expected" (controls column header)
#' @return character HTML string
# ─────────────────────────────────────────────────────────────────────────────
flat_completeness_html <- function(df, rate_type = "pct_due") {

  # Styling: group rows by form — show name only in first row of each group,
  # thicker top border when the form changes.
  pct_style <- function(p) {
    if (is.na(p))        "color:#94A3B8"
    else if (p >= 90)    "color:#0F6E56;font-weight:700"
    else if (p >= 70)    "color:#D97706;font-weight:700"
    else                 "color:#C0392B;font-weight:700"
  }

  expected_header <- if (rate_type == "pct_due") "Forms due" else "Forms expected"

  rows_html <- vapply(seq_len(nrow(df)), function(i) {
    form_txt <- if (i == 1 || df$form_display[i] != df$form_display[i - 1]) {
      df$form_display[i]
    } else {
      ""
    }
    border_top <- if (i > 1 && df$form_display[i] != df$form_display[i - 1]) {
      "border-top:2px solid #DDE5EE;"
    } else ""

    exp_txt <- if (is.na(df$n_expected[i])) "\u2014" else format(df$n_expected[i], big.mark = ",")
    ent_txt <- if (is.na(df$entered[i]))    "\u2014" else format(df$entered[i],    big.mark = ",")
    pct_txt <- if (is.na(df$pct[i]))        "\u2014" else paste0(round(df$pct[i], 0), "%")

    sprintf(
      paste0(
        '<tr style="%s">',
        '<td style="font-weight:600;color:#1B4F6B">%s</td>',
        '<td style="color:#334155">%s</td>',
        '<td style="text-align:center">%s</td>',
        '<td style="text-align:center">%s</td>',
        '<td style="text-align:center;%s">%s</td>',
        '</tr>'
      ),
      border_top,
      form_txt,
      df$timepoint_display[i],
      exp_txt,
      ent_txt,
      pct_style(df$pct[i]),
      pct_txt
    )
  }, character(1))

  paste0(
    '<table class="rt" style="width:100%;border-collapse:collapse;font-size:11px">',
    '<thead><tr style="background:#F4F6F8;border-bottom:2px solid #1B4F6B;color:#1B4F6B">',
    '<th style="text-align:left;padding:8px 10px">Form name</th>',
    '<th style="text-align:left;padding:8px 10px">Time-point</th>',
    '<th style="text-align:center;padding:8px 10px">', expected_header, '</th>',
    '<th style="text-align:center;padding:8px 10px">Forms received</th>',
    '<th style="text-align:center;padding:8px 10px">% Return rate</th>',
    '</tr></thead><tbody>',
    paste(rows_html, collapse = ""),
    '</tbody></table>',
    '<div style="margin-top:10px;font-size:9px;color:#6b7c8d">',
    'Rows marked \u2014 are forms listed in the protocol that are not yet in the current data export. ',
    '<span style="color:#0F6E56;font-weight:600">\u226590%</span> on track &middot; ',
    '<span style="color:#D97706;font-weight:600">70\u201389%</span> attention needed &middot; ',
    '<span style="color:#C0392B;font-weight:600">&lt;70%</span> action required',
    '</div>'
  )
}

# ─────────────────────────────────────────────────────────────────────────────
#' Render the flat table as a flextable (for the TSC Word report)
#'
#' @param df           Output of flat_completeness_df()
#' @param rate_type    "pct_due" or "pct_expected" (controls column header)
#' @return flextable object
# ─────────────────────────────────────────────────────────────────────────────
flat_completeness_flextable <- function(df, rate_type = "pct_due") {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("flextable package required for flat_completeness_flextable()")
  }

  expected_header <- if (rate_type == "pct_due") "Forms due" else "Forms expected"

  # Deduplicate form names — only show in first row of each group
  df_display <- df
  prev_form  <- ""
  for (i in seq_len(nrow(df_display))) {
    if (df_display$form_display[i] == prev_form) {
      df_display$form_display[i] <- ""
    } else {
      prev_form <- df_display$form_display[i]
    }
  }

  tbl <- data.frame(
    `Form name`       = df_display$form_display,
    `Time-point`      = df_display$timepoint_display,
    ex  = ifelse(is.na(df$n_expected), "\u2014", format(df$n_expected, big.mark = ",")),
    en  = ifelse(is.na(df$entered),    "\u2014", format(df$entered,    big.mark = ",")),
    pc  = ifelse(is.na(df$pct),        "\u2014", paste0(round(df$pct, 0), "%")),
    check.names      = FALSE,
    stringsAsFactors = FALSE
  )
  names(tbl)[3:5] <- c(expected_header, "Forms received", "% Return rate")

  tonic_navy <- "#1B4F6B"

  ft <- flextable::flextable(tbl)
  ft <- flextable::bg(ft,     part = "header", bg = tonic_navy)
  ft <- flextable::color(ft,  part = "header", color = "white")
  ft <- flextable::bold(ft,   part = "header")
  ft <- flextable::fontsize(ft, part = "header", size = 10)
  ft <- flextable::fontsize(ft, part = "body",   size = 9)
  ft <- flextable::padding(ft, padding = 4)
  ft <- flextable::align(ft,  j = 3:5, align = "center", part = "all")
  ft <- flextable::bold(ft,   j = 1, part = "body")
  ft <- flextable::color(ft,  j = 1, color = tonic_navy, part = "body")

  # Colour the % column based on value
  for (i in seq_len(nrow(df))) {
    p <- df$pct[i]
    if (is.na(p)) {
      ft <- flextable::color(ft, i = i, j = 5, color = "#94A3B8")
    } else if (p >= 90) {
      ft <- flextable::color(ft, i = i, j = 5, color = "#0F6E56")
      ft <- flextable::bold(ft,  i = i, j = 5, bold = TRUE)
    } else if (p >= 70) {
      ft <- flextable::color(ft, i = i, j = 5, color = "#D97706")
      ft <- flextable::bold(ft,  i = i, j = 5, bold = TRUE)
    } else {
      ft <- flextable::color(ft, i = i, j = 5, color = "#C0392B")
      ft <- flextable::bold(ft,  i = i, j = 5, bold = TRUE)
    }
  }

  # Add horizontal rule between form groups (where form_display is non-empty)
  group_starts <- which(nzchar(df_display$form_display))
  for (gs in group_starts) {
    if (gs > 1) {
      ft <- flextable::hline(ft, i = gs - 1,
                              border = officer::fp_border(color = "#CBD5E1", width = 1))
    }
  }

  ft <- flextable::border_outer(ft,
          border = officer::fp_border(color = "#CBD5E1", width = 0.5))
  ft <- flextable::width(ft, j = 1, width = 1.8)
  ft <- flextable::width(ft, j = 2, width = 1.4)
  ft <- flextable::width(ft, j = 3, width = 0.9)
  ft <- flextable::width(ft, j = 4, width = 1.0)
  ft <- flextable::width(ft, j = 5, width = 1.0)

  ft
}
