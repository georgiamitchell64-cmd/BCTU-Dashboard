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

.fmt_median_iqr <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("—")
  q <- stats::quantile(x, c(.25, .5, .75), names = FALSE)
  sprintf("%.1f (%.1f, %.1f)", q[2], q[1], q[3])
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
    add("Participant demographics", "Age (years)", "Median (IQR)",
        .fmt_median_iqr(age))
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
    return('<div style="padding:12px;color:#58595B;font-style:italic">No baseline data available.</div>')
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
        "border-top:1px solid #E3E3E3;"
      } else ""
      sprintf(
        paste0('<tr style="%s">',
               '<td style="padding:6px 10px;font-weight:600;color:#1B1B1B;width:32%%;">%s</td>',
               '<td style="padding:6px 10px;color:#4A4A4A;width:38%%;">%s</td>',
               '<td style="padding:6px 10px;text-align:right;color:#3C3C3B;font-weight:500;width:30%%;">%s</td>',
               '</tr>'),
        border_top, sec_rows$label_show[i], sec_rows$sublabel[i], sec_rows$stat[i]
      )
    }, character(1))

    paste0(
      '<tr style="background:#F4F6F8;">',
      '<td colspan="3" style="padding:8px 10px;font-weight:700;color:#1B1B1B;',
      'text-transform:uppercase;letter-spacing:0.5px;font-size:10px;',
      'border-top:2px solid #1B1B1B;">', sec, '</td></tr>',
      paste(rows_html, collapse = "")
    )
  }, character(1))

  paste0(
    '<table class="rt" style="width:100%;border-collapse:collapse;font-size:11px;">',
    '<thead><tr style="background:#1B1B1B;color:#FFFFFF;">',
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
  ink <- "#1B1B1B"; muted <- "#6B6B6D"

  # A standard "Table 1": each characteristic on a bold row with its
  # categories indented beneath, grouped under full-width section rows, and
  # one Total column. ", n (%)" moves into the footnote.
  rows <- list(); kind <- character(0)
  push <- function(a, b, k) {
    rows[[length(rows) + 1]] <<- data.frame(char = a, val = b, stringsAsFactors = FALSE)
    kind <<- c(kind, k)
  }
  for (sec in unique(df$section)) {
    push(sec, "", "section")
    s <- df[df$section == sec, , drop = FALSE]
    for (lab in unique(s$label)) {
      r <- s[s$label == lab, , drop = FALSE]
      push(sub(",\\s*n \\(%\\)$", "", lab), "", "label")
      for (i in seq_len(nrow(r))) {
        # "Missing 0" is noise; only show missing counts that exist
        if (identical(r$sublabel[i], "Missing") && identical(trimws(r$stat[i]), "0")) next
        push(r$sublabel[i], r$stat[i], if (identical(r$sublabel[i], "Missing")) "missing" else "item")
      }
    }
  }
  tbl <- do.call(rbind, rows)
  rows_of <- function(k) which(kind == k)

  ft <- flextable::flextable(tbl, col_keys = c("char", "val"))
  ft <- flextable::set_header_labels(ft, char = "Characteristic",
          val = sprintf("Total\n(N = %s)", format(total_n, big.mark = ",")))
  ft <- flextable::font(ft, fontname = "Arial", part = "all")
  ft <- flextable::fontsize(ft, size = 9, part = "all")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::color(ft, color = ink, part = "all")
  ft <- flextable::align(ft, j = "val", align = "right", part = "all")
  ft <- flextable::valign(ft, valign = "bottom", part = "header")
  ft <- flextable::padding(ft, padding.top = 2, padding.bottom = 2,
                           padding.left = 4, padding.right = 4, part = "all")

  # Section rows span the table, shaded
  for (i in rows_of("section")) ft <- flextable::merge_at(ft, i = i, j = 1:2, part = "body")
  ft <- flextable::bg(ft, i = rows_of("section"), bg = "#F2F2F2", part = "body")
  ft <- flextable::bold(ft, i = rows_of("section"), part = "body")
  ft <- flextable::padding(ft, i = rows_of("section"), padding.top = 4, padding.bottom = 3, part = "body")
  # Characteristics bold; categories indented; missing counts muted
  ft <- flextable::bold(ft, i = rows_of("label"), j = "char", part = "body")
  ft <- flextable::padding(ft, i = c(rows_of("item"), rows_of("missing")), j = "char",
                           padding.left = 16, part = "body")
  if (length(rows_of("missing"))) {
    ft <- flextable::italic(ft, i = rows_of("missing"), part = "body")
    ft <- flextable::color(ft, i = rows_of("missing"), color = muted, part = "body")
  }

  # Horizontal rules only: heavy above the header and at the foot, thin under
  # the header, hairlines between sections — no vertical lines
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline_top(ft, border = officer::fp_border(color = ink, width = 1.25), part = "header")
  ft <- flextable::hline_bottom(ft, border = officer::fp_border(color = ink, width = 0.75), part = "header")
  ft <- flextable::hline_bottom(ft, border = officer::fp_border(color = ink, width = 1.25), part = "body")
  between <- rows_of("section")[rows_of("section") > 1] - 1
  if (length(between))
    ft <- flextable::hline(ft, i = between, border = officer::fp_border(color = "#BFBFBF", width = 0.5),
                           part = "body")

  ft <- flextable::width(ft, j = "char", width = 4.6)
  ft <- flextable::width(ft, j = "val",  width = 1.7)
  ft <- flextable::set_table_properties(ft, layout = "fixed")

  # Keep each characteristic with its categories across a page break
  if ("keep_with_next" %in% getNamespaceExports("flextable"))
    ft <- flextable::keep_with_next(ft, i = c(rows_of("section"), rows_of("label")),
                                    value = TRUE, part = "body")

  # Footnote: what the figures are, and the abbreviations the table uses
  abbr <- c(NELA = "National Emergency Laparotomy Audit", NRS = "Nutritional Risk Screening",
            MUST = "Malnutrition Universal Screening Tool", SD = "standard deviation",
            IQR = "interquartile range")
  txt  <- paste(tbl$char, collapse = " ")
  used <- names(abbr)[vapply(names(abbr), function(a) grepl(paste0("\\b", a, "\\b"), txt, perl = TRUE),
                             logical(1))]
  note <- paste0("Data are n (%) unless stated otherwise. Percentages are of participants with a ",
                 "recorded value; missing values are shown separately.",
                 if (length(used)) paste0(" ", paste(sprintf("%s, %s", used, abbr[used]), collapse = "; "), "."))
  ft <- flextable::add_footer_lines(ft, note)
  ft <- flextable::font(ft, fontname = "Arial", part = "footer")
  ft <- flextable::fontsize(ft, size = 7.5, part = "footer")
  ft <- flextable::color(ft, color = muted, part = "footer")
  ft <- flextable::padding(ft, padding.top = 4, part = "footer")
  ft
}
