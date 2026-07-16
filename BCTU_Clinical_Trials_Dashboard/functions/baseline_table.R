# =============================================================================
# TONIC Trial — Baseline characteristics table ("Table 1")
# =============================================================================
# Shared helper used by the TMG (HTML) and TSC (Word) Rmds.
#
# Public functions:
#   baseline_characteristics_df(rd)
#     → tibble with columns: section, label, sublabel, stat
#       where `section` groups rows (minimisation / demographics / general),
#       `label` is the characteristic name (shown only on first row of group),
#       `sublabel` is the category, and `stat` is the formatted value.
#
#   baseline_characteristics_html(df, total_n)        → HTML string (TMG)
#   baseline_characteristics_flextable(df, total_n)   → flextable (TSC)
#
# Data contract: `rd` must contain `baseline_df` — a per-participant data
# frame with any of these columns (missing ones skip their row):
#   cae_age               — age in years (numeric)
#   base_sex              — 1=Male, 2=Female
#   base_ethnic_gp        — 1..19 (NHS ethnic group codes)
#   base_nela_score_mort  — NELA predicted mortality % (numeric)
#   nut_b_nrs_group       — "0-3 Low risk" / "4 At risk" / "5-7 High risk"
#   nut_b_must_score      — 1=0 (Low), 2=1 (Medium), 3=≥2 (High)
#   base_residence        — 1=Own home, 2=Rehab, 3=Residential, 4=Nursing
#   site_name             — for site breakdown in minimisation block
# =============================================================================

# ── Sex coding ───────────────────────────────────────────────────────────────
.sex_labels <- c("1" = "Male", "2" = "Female")

# ── Ethnicity coding (NHS 19-category) ───────────────────────────────────────
.eth_labels <- c(
  "1"="Asian or Asian British - Indian",
  "2"="Asian or Asian British - Pakistani",
  "3"="Asian or Asian British - Bangladeshi",
  "4"="Asian or Asian British - Chinese",
  "5"="Asian or Asian British - Any other Asian background",
  "6"="Black, Black British, Caribbean or African - Caribbean",
  "7"="Black, Black British, Caribbean or African - African",
  "8"="Black, Black British, Caribbean or African - Any other Black background",
  "9"="Mixed - White and Black Caribbean",
  "10"="Mixed - White and Black African",
  "11"="Mixed - White and Asian",
  "12"="Mixed - Any other Mixed background",
  "13"="White - British",
  "14"="White - Irish",
  "15"="White - Gypsy or Irish Traveller",
  "16"="White - Roma",
  "17"="White - Any other White background",
  "18"="Other - Arab",
  "19"="Other - Any other ethnic group"
)

# ── Residence coding ─────────────────────────────────────────────────────────
.residence_labels <- c(
  "1" = "Own home",
  "2" = "Rehabilitation",
  "3" = "Residential home",
  "4" = "Nursing home"
)

# ── MUST coding ──────────────────────────────────────────────────────────────
.must_labels <- c(
  "1" = "0 (Low risk)",
  "2" = "1 (Medium risk)",
  "3" = "\u22652 (High risk)"
)

# ── Helpers ──────────────────────────────────────────────────────────────────
.fmt_n_pct <- function(n, total) {
  if (is.na(n) || total == 0) return("\u2014")
  pct <- round(n / total * 100, 0)
  sprintf("%d (%d%%)", n, pct)
}

.fmt_mean_sd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("\u2014")
  sprintf("%.1f (%.1f)", mean(x), sd(x))
}

.fmt_range <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("\u2014")
  sprintf("%d, %d", as.integer(min(x)), as.integer(max(x)))
}

.fmt_missing <- function(x) {
  if (length(x) == 0) return("0")
  as.character(sum(is.na(x)))
}

# =============================================================================
# Main builder — returns long-form tibble of display rows
# =============================================================================
baseline_characteristics_df <- function(rd) {

  df <- if (is.null(rd$baseline_df)) data.frame() else rd$baseline_df
  total_n <- nrow(df)

  # Resolve trial-specific REDCap column names from the active config.
  # Falls back to TONIC defaults so existing call sites keep working.
  c_nela <- fld("nela_score",  default = "base_nela_score_mort")
  c_nrs  <- fld("nrs_group",   default = "nut_b_nrs_group")
  c_age  <- fld("age",         default = "cae_age")
  c_sex  <- fld("sex",         default = "base_sex")
  c_eth  <- fld("ethnicity",   default = "base_ethnic_gp")
  c_res  <- fld("residence",   default = "base_residence")
  c_must <- fld("must_score",  default = "nut_b_must_score")

  rows <- list()
  add  <- function(section, label, sublabel, stat) {
    rows[[length(rows) + 1]] <<- list(
      section = section, label = label, sublabel = sublabel, stat = stat
    )
  }

  # ── MINIMISATION VARIABLES ─────────────────────────────────────────────────
  # NELA Mortality Score
  if (c_nela %in% names(df)) {
    x <- suppressWarnings(as.numeric(df[[c_nela]]))
    n_under <- sum(x < 5,  na.rm = TRUE)
    n_over  <- sum(x >= 5, na.rm = TRUE)
    valid   <- sum(!is.na(x))
    add("Minimisation variables", "NELA Mortality Score", "<5%",
        .fmt_n_pct(n_under, valid))
    add("Minimisation variables", "NELA Mortality Score", "\u22655%",
        .fmt_n_pct(n_over, valid))
    if (sum(is.na(x)) > 0) {
      add("Minimisation variables", "NELA Mortality Score", "Missing",
          as.character(sum(is.na(x))))
    }
  }

  # Nutrition Risk Score (NRS)
  if (c_nrs %in% names(df)) {
    nrs <- as.character(df[[c_nrs]])
    n_under <- sum(grepl("^0-3", nrs), na.rm = TRUE)
    n_over  <- sum(grepl("^(4|5-7)", nrs), na.rm = TRUE)
    valid   <- n_under + n_over
    n_miss  <- total_n - valid
    add("Minimisation variables", "Nutrition Risk Score (NRS)", "<3",
        .fmt_n_pct(n_under, valid))
    add("Minimisation variables", "Nutrition Risk Score (NRS)", "\u22653",
        .fmt_n_pct(n_over,  valid))
    if (n_miss > 0) {
      add("Minimisation variables", "Nutrition Risk Score (NRS)", "Missing",
          as.character(n_miss))
    }
  }

  # Site
  if ("site_name" %in% names(df)) {
    valid <- df$site_name[!is.na(df$site_name) & nzchar(df$site_name)]
    if (length(valid) > 0) {
      tab <- sort(table(valid), decreasing = TRUE)
      for (nm in names(tab)) {
        add("Minimisation variables", "Site", nm,
            .fmt_n_pct(as.integer(tab[[nm]]), length(valid)))
      }
    }
  }

  # ── PARTICIPANT DEMOGRAPHICS ───────────────────────────────────────────────
  # Age
  if (c_age %in% names(df)) {
    age <- suppressWarnings(as.numeric(df[[c_age]]))
    add("Participant demographics", "Age (years)", "Mean (SD)",
        .fmt_mean_sd(age))
    add("Participant demographics", "Age (years)", "Range (min, max)",
        .fmt_range(age))
    add("Participant demographics", "Age (years)", "Missing",
        .fmt_missing(age))
  }

  # Gender
  if (c_sex %in% names(df)) {
    sx    <- as.character(df[[c_sex]])
    labs  <- .sex_labels[sx]
    valid <- sum(!is.na(labs))
    for (nm in unname(.sex_labels)) {
      n <- sum(labs == nm, na.rm = TRUE)
      add("Participant demographics", "Gender, n (%)", nm,
          .fmt_n_pct(n, valid))
    }
    n_miss <- sum(is.na(labs))
    if (n_miss > 0) {
      add("Participant demographics", "Gender, n (%)", "Missing",
          as.character(n_miss))
    }
  }

  # Ethnic group — only show categories that actually appear
  if (c_eth %in% names(df)) {
    eg    <- as.character(df[[c_eth]])
    labs  <- .eth_labels[eg]
    valid <- sum(!is.na(labs))
    if (valid > 0) {
      tab <- sort(table(labs), decreasing = TRUE)
      for (nm in names(tab)) {
        add("Participant demographics", "Ethnic Group, n (%)", nm,
            .fmt_n_pct(as.integer(tab[[nm]]), valid))
      }
    }
    n_miss <- sum(is.na(labs))
    if (n_miss > 0) {
      add("Participant demographics", "Ethnic Group, n (%)", "Missing",
          as.character(n_miss))
    }
  }

  # ── GENERAL ────────────────────────────────────────────────────────────────
  # Place of residence
  if (c_res %in% names(df)) {
    res   <- as.character(df[[c_res]])
    labs  <- .residence_labels[res]
    valid <- sum(!is.na(labs))
    for (nm in unname(.residence_labels)) {
      n <- sum(labs == nm, na.rm = TRUE)
      add("General", "Participant's place of residence", nm,
          .fmt_n_pct(n, valid))
    }
    n_miss <- sum(is.na(labs))
    if (n_miss > 0) {
      add("General", "Participant's place of residence", "Missing",
          as.character(n_miss))
    }
  }

  # MUST score
  if (c_must %in% names(df)) {
    ms    <- as.character(df[[c_must]])
    labs  <- .must_labels[ms]
    valid <- sum(!is.na(labs))
    for (nm in unname(.must_labels)) {
      n <- sum(labs == nm, na.rm = TRUE)
      add("General", "MUST Score", nm,
          .fmt_n_pct(n, valid))
    }
    n_miss <- sum(is.na(labs))
    if (n_miss > 0) {
      add("General", "MUST Score", "Missing",
          as.character(n_miss))
    }
  }

  # ── Convert to tibble ──────────────────────────────────────────────────────
  if (length(rows) == 0) {
    return(tibble::tibble(section = character(), label = character(),
                           sublabel = character(), stat = character(),
                           total_n = integer()))
  }

  tbl <- do.call(rbind, lapply(rows, function(r) {
    data.frame(section = r$section, label = r$label,
               sublabel = r$sublabel, stat = r$stat,
               stringsAsFactors = FALSE)
  }))
  tbl$total_n <- total_n
  tibble::as_tibble(tbl)
}

# =============================================================================
# HTML renderer (TMG report)
# =============================================================================
baseline_characteristics_html <- function(df, total_n = NULL) {

  if (nrow(df) == 0) {
    return('<div style="padding:12px;color:#64748B;font-style:italic">No baseline data available.</div>')
  }

  if (is.null(total_n)) total_n <- df$total_n[1]

  # Deduplicate repeated label values within a section (show label only on first row)
  df_disp <- df
  df_disp$label_show <- df_disp$label
  for (i in seq_len(nrow(df_disp))) {
    if (i > 1 && df_disp$label_show[i] == df_disp$label_show[i - 1]) {
      df_disp$label_show[i] <- ""
    }
  }

  sections <- unique(df_disp$section)
  section_html <- vapply(sections, function(sec) {

    sec_rows <- df_disp[df_disp$section == sec, , drop = FALSE]

    rows_html <- vapply(seq_len(nrow(sec_rows)), function(i) {
      border_top <- if (i > 1 && nzchar(sec_rows$label_show[i])) {
        "border-top:1px solid #E2E8F0;"
      } else ""
      sprintf(
        paste0('<tr style="%s">',
               '<td style="padding:6px 10px;font-weight:600;color:#1B4F6B;width:32%%;">%s</td>',
               '<td style="padding:6px 10px;color:#475569;width:38%%;">%s</td>',
               '<td style="padding:6px 10px;text-align:right;color:#334155;font-weight:500;width:30%%;">%s</td>',
               '</tr>'),
        border_top, sec_rows$label_show[i], sec_rows$sublabel[i], sec_rows$stat[i]
      )
    }, character(1))

    paste0(
      '<tr style="background:#F4F6F8;">',
      '<td colspan="3" style="padding:8px 10px;font-weight:700;color:#1B4F6B;',
      'text-transform:uppercase;letter-spacing:0.5px;font-size:10px;',
      'border-top:2px solid #1B4F6B;">', sec, '</td></tr>',
      paste(rows_html, collapse = "")
    )
  }, character(1))

  paste0(
    '<table class="rt" style="width:100%;border-collapse:collapse;font-size:11px;">',
    '<thead><tr style="background:#1B4F6B;color:#FFFFFF;">',
    '<th colspan="2" style="padding:8px 10px;text-align:left;">Characteristic</th>',
    '<th style="padding:8px 10px;text-align:right;">n = ', total_n, '</th>',
    '</tr></thead><tbody>',
    paste(section_html, collapse = ""),
    '</tbody></table>'
  )
}

# =============================================================================
# Flextable renderer (TSC Word report)
# =============================================================================
baseline_characteristics_flextable <- function(df, total_n = NULL) {

  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("flextable package required for baseline_characteristics_flextable()")
  }

  if (nrow(df) == 0) return(NULL)

  if (is.null(total_n)) total_n <- df$total_n[1]

  # Deduplicate labels within section groups
  df_disp <- df
  df_disp$label_show <- df_disp$label
  for (i in seq_len(nrow(df_disp))) {
    if (i > 1 && df_disp$label_show[i] == df_disp$label_show[i - 1] &&
        df_disp$section[i] == df_disp$section[i - 1]) {
      df_disp$label_show[i] <- ""
    }
  }

  # Interleave section-header rows with data rows
  build_rows <- list()
  for (sec in unique(df_disp$section)) {
    # Section header row (label spans the first two columns via merging later)
    build_rows[[length(build_rows) + 1]] <- data.frame(
      Characteristic = sec, Category = "", Value = "",
      is_section = TRUE, stringsAsFactors = FALSE
    )
    sec_rows <- df_disp[df_disp$section == sec, , drop = FALSE]
    for (i in seq_len(nrow(sec_rows))) {
      build_rows[[length(build_rows) + 1]] <- data.frame(
        Characteristic = sec_rows$label_show[i],
        Category       = sec_rows$sublabel[i],
        Value          = sec_rows$stat[i],
        is_section     = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }

  tbl <- do.call(rbind, build_rows)
  section_idx <- which(tbl$is_section)

  # Rename the value column to show total n
  names(tbl)[3] <- paste0("n = ", total_n)

  ft <- flextable::flextable(tbl[, 1:3])
  ft <- flextable::bg(ft,     part = "header", bg = "#1B4F6B")
  ft <- flextable::color(ft,  part = "header", color = "white")
  ft <- flextable::bold(ft,   part = "header")
  ft <- flextable::fontsize(ft, part = "header", size = 10)
  ft <- flextable::fontsize(ft, part = "body",   size = 9)
  ft <- flextable::padding(ft, padding = 4)
  ft <- flextable::align(ft,  j = 3,  align = "right", part = "all")
  ft <- flextable::bold(ft,   j = 1,  part = "body")
  ft <- flextable::color(ft,  j = 1,  color = "#1B4F6B", part = "body")

  # Style the section-header rows
  for (i in section_idx) {
    ft <- flextable::bg(ft,   i = i, bg = "#F4F6F8")
    ft <- flextable::bold(ft, i = i, bold = TRUE)
    ft <- flextable::color(ft, i = i, color = "#1B4F6B")
    # Merge the three cells into one
    ft <- flextable::merge_at(ft, i = i, j = 1:3)
  }

  ft <- flextable::border_outer(ft,
          border = officer::fp_border(color = "#CBD5E1", width = 0.5))
  ft <- flextable::border_inner_h(ft,
          border = officer::fp_border(color = "#E2E8F0", width = 0.5))

  ft <- flextable::width(ft, j = 1, width = 2.1)
  ft <- flextable::width(ft, j = 2, width = 2.4)
  ft <- flextable::width(ft, j = 3, width = 1.4)

  ft
}
