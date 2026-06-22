# ─────────────────────────────────────────────────────────────────────────────
# safety_events.R
# ----------------------------------------------------------------------------
# Pulls per-event detail rows (SAE, deviation, withdrawal, pregnancy) out of
# the loaded REDCap data. The existing parse_safety() only returns boolean
# flags ("did this row complete the SAE form?"); these helpers return the
# actual data — onset date, submission date, severity, narrative, status —
# for the inline drill-down on the Data tab.
#
# All field lookups go through fld() so the same code works for any trial.
# When a field isn't found in the loaded export, the corresponding column
# comes back NA and the UI renders an em-dash rather than failing.
# ─────────────────────────────────────────────────────────────────────────────

#' Safe getter — returns the column if present, NAs otherwise.
.safety_col <- function(df, col, n = nrow(df), as = NA) {
  if (is.null(col) || !nzchar(col) || !(col %in% names(df))) {
    return(rep(as, n))
  }
  df[[col]]
}

#' User-mapped extra detail columns for a section ("sae" / "withdrawal" /
#' "complication"), defined in Trial Settings and stored in
#' cfg$detail_fields[[section]] as a list of list(col=, header=). Lets trials
#' surface extra REDCap columns (e.g. death, causality, expectedness, reason)
#' without code changes.
detail_fields_for <- function(section, cfg = current_trial_config()) {
  fl <- (cfg %||% list())$detail_fields[[section]]
  if (is.null(fl) || !length(fl)) return(list())
  out <- lapply(fl, function(x) {
    col <- x$col %||% x[["col"]]
    if (is.null(col) || !nzchar(col)) return(NULL)
    list(col = col, header = x$header %||% x[["header"]] %||% col)
  })
  Filter(Negate(is.null), out)
}

#' Append mapped extra columns to an events tibble. Each becomes an "x__<header>"
#' column so renderers can pick them up generically and show the header.
.append_detail_cols <- function(tib, df, extra) {
  n <- nrow(df)
  for (e in extra) tib[[paste0("x__", e$header)]] <-
    as.character(.safety_col(df, e$col, n, NA_character_))
  tib
}

#' Coerce a vector of date-ish strings into Date. REDCap exports vary:
#' "2026-05-17", "17/05/2026", "2026-05-17 13:42", with mixed quality.
#' Returns NA for unparseable values rather than throwing.
.parse_date_loose <- function(x) {
  if (is.null(x)) return(as.Date(integer(0), origin = "1970-01-01"))
  x_chr <- trimws(as.character(x))
  x_chr[!nzchar(x_chr)] <- NA_character_
  # Drop time portion if present
  x_chr <- sub("[ T].*$", "", x_chr)
  out <- suppressWarnings(as.Date(x_chr, format = "%Y-%m-%d"))
  uk  <- is.na(out) & !is.na(x_chr)
  if (any(uk)) {
    out[uk] <- suppressWarnings(as.Date(x_chr[uk], format = "%d/%m/%Y"))
  }
  us  <- is.na(out) & !is.na(x_chr)
  if (any(us)) {
    out[us] <- suppressWarnings(as.Date(x_chr[us], format = "%m/%d/%Y"))
  }
  out
}

#' Generic event-detail extractor.
#' @param raw_df          rv$raw_redcap
#' @param complete_field  logical field that flags this event was completed
#'                        (typically `<form>_complete` with value 2 = complete)
#' @param spec            list of column names by role: list(
#'                          term, severity, relatedness, status,
#'                          onset_date, report_date, narrative
#'                        ) — each may be NULL or "" if not configured
#' @param event_label     short label for the event type (e.g. "SAE")
#' @return  tibble — one row per event, columns:
#'           record_id, site, event_type, term, severity, relatedness,
#'           status, onset_date, report_date, lag_days, narrative
extract_events <- function(raw_df, complete_field, spec, event_label = "Event",
                           extra = list()) {
  if (is.null(raw_df) || nrow(raw_df) == 0) {
    return(tibble::tibble(
      record_id = character(0), site = character(0), event_type = character(0),
      term = character(0), severity = character(0), relatedness = character(0),
      status = character(0),
      onset_date = as.Date(integer(0), origin = "1970-01-01"),
      report_date = as.Date(integer(0), origin = "1970-01-01"),
      lag_days = integer(0), narrative = character(0)
    ))
  }

  comp_vals <- suppressWarnings(as.integer(.safety_col(raw_df, complete_field)))
  keep <- !is.na(comp_vals) & comp_vals == 2L
  if (!any(keep)) {
    return(tibble::tibble(
      record_id = character(0), site = character(0), event_type = character(0),
      term = character(0), severity = character(0), relatedness = character(0),
      status = character(0),
      onset_date = as.Date(integer(0), origin = "1970-01-01"),
      report_date = as.Date(integer(0), origin = "1970-01-01"),
      lag_days = integer(0), narrative = character(0)
    ))
  }

  df <- raw_df[keep, , drop = FALSE]
  n  <- nrow(df)

  onset  <- .parse_date_loose(.safety_col(df, spec$onset_date,  n, NA_character_))
  report <- .parse_date_loose(.safety_col(df, spec$report_date, n, NA_character_))
  lag    <- as.integer(report - onset)

  tib <- tibble::tibble(
    record_id   = as.character(.safety_col(df, "record_id",     n, NA)),
    site        = as.character(.safety_col(df, "site_dag",      n, NA)),
    event_type  = event_label,
    term        = as.character(.safety_col(df, spec$term,        n, NA_character_)),
    severity    = as.character(.safety_col(df, spec$severity,    n, NA_character_)),
    relatedness = as.character(.safety_col(df, spec$relatedness, n, NA_character_)),
    status      = as.character(.safety_col(df, spec$status,      n, NA_character_)),
    onset_date  = onset,
    report_date = report,
    lag_days    = lag,
    narrative   = as.character(.safety_col(df, spec$narrative,   n, NA_character_))
  )

  # Translate coded severity / status to labels via the trial's column_labels
  # (e.g. SAE severity 1/2/3 → Mild/Moderate/Severe). Unmapped values are kept
  # as-is, so this is a no-op for trials without a mapping.
  cfg <- current_trial_config()
  if (exists(".resolve_value_labels")) {
    if (!is.null(spec$severity) && nzchar(spec$severity %||% ""))
      tib$severity <- .resolve_value_labels(tib$severity, spec$severity, cfg)
    if (!is.null(spec$status) && nzchar(spec$status %||% ""))
      tib$status   <- .resolve_value_labels(tib$status,   spec$status,   cfg)
  }

  .append_detail_cols(tib, df, extra)
}

#' Pull SAE rows. Field names resolved via fld().
sae_events <- function(raw_df) {
  extract_events(
    raw_df,
    complete_field = fld("sae_complete",   default = "sae_complete"),
    spec = list(
      term        = fld("sae_term",        default = NULL),
      severity    = fld("sae_severity",    default = NULL),
      relatedness = fld("sae_relatedness", default = NULL),
      status      = fld("sae_status",      default = NULL),
      onset_date  = fld("sae_onset_date",  default = NULL),
      report_date = fld("sae_report_date", default = NULL),
      narrative   = fld("sae_narrative",   default = NULL)
    ),
    event_label = "SAE",
    extra = detail_fields_for("sae")   # e.g. death, causality, expectedness, reason
  )
}

#' Pull deviation rows.
deviation_events <- function(raw_df) {
  extract_events(
    raw_df,
    complete_field = fld("deviation_complete", default = "deviation_complete"),
    spec = list(
      term        = fld("deviation_term",        default = NULL),
      severity    = fld("deviation_severity",    default = NULL),
      relatedness = fld("deviation_relatedness", default = NULL),
      status      = fld("deviation_status",      default = NULL),
      onset_date  = fld("deviation_date",        default = NULL),
      report_date = fld("deviation_report_date", default = NULL),
      narrative   = fld("deviation_narrative",   default = NULL)
    ),
    event_label = "Deviation"
  )
}

#' Pull pregnancy notification rows.
preg_notif_events <- function(raw_df) {
  extract_events(
    raw_df,
    complete_field = fld("pregnancy_notification_complete",
                         default = "pregnancy_notification_complete"),
    spec = list(
      term        = fld("preg_notif_term",        default = NULL),
      severity    = NULL,
      relatedness = NULL,
      status      = fld("preg_notif_status",      default = NULL),
      onset_date  = fld("preg_notif_date",        default = NULL),
      report_date = fld("preg_notif_report_date", default = NULL),
      narrative   = fld("preg_notif_narrative",   default = NULL)
    ),
    event_label = "Pregnancy notification"
  )
}

#' Pull pregnancy outcome rows.
preg_out_events <- function(raw_df) {
  extract_events(
    raw_df,
    complete_field = fld("pregnancy_outcome_complete",
                         default = "pregnancy_outcome_complete"),
    spec = list(
      term        = fld("preg_out_outcome",     default = NULL),
      severity    = NULL,
      relatedness = NULL,
      status      = fld("preg_out_status",      default = NULL),
      onset_date  = fld("preg_out_date",        default = NULL),
      report_date = fld("preg_out_report_date", default = NULL),
      narrative   = fld("preg_out_narrative",   default = NULL)
    ),
    event_label = "Pregnancy outcome"
  )
}

#' Pull withdrawal rows. Withdrawals don't follow the *_complete pattern —
#' a row counts as a withdrawal when `cos_type` is non-empty.
withdrawal_events <- function(raw_df) {
  if (is.null(raw_df) || nrow(raw_df) == 0) {
    return(tibble::tibble(
      record_id = character(0), site = character(0), event_type = character(0),
      term = character(0), severity = character(0), relatedness = character(0),
      status = character(0),
      onset_date = as.Date(integer(0), origin = "1970-01-01"),
      report_date = as.Date(integer(0), origin = "1970-01-01"),
      lag_days = integer(0), narrative = character(0)
    ))
  }
  cos_col  <- fld("cos_type", default = "cos_type")
  date_col <- fld("cos_date", default = NULL)
  reason   <- fld("cos_reason", default = NULL)

  cos_raw <- .safety_col(raw_df, cos_col)
  keep <- !is.na(cos_raw) & nchar(trimws(as.character(cos_raw))) > 0 &
          as.character(cos_raw) != "NA"
  if (!any(keep)) {
    return(extract_events(raw_df[0, ], complete_field = NULL, spec = list(),
                          event_label = "Withdrawal"))
  }
  df <- raw_df[keep, , drop = FALSE]
  n  <- nrow(df)

  cos_code  <- as.character(.safety_col(df, cos_col, n))
  cos_label <- if (exists("cos_type_labels"))
    dplyr::recode(cos_code, !!!cos_type_labels,
                  .default = paste("Code:", cos_code))
  else cos_code
  onset <- .parse_date_loose(.safety_col(df, date_col, n, NA_character_))

  tib <- tibble::tibble(
    record_id   = as.character(.safety_col(df, "record_id", n, NA)),
    site        = as.character(.safety_col(df, "site_dag",  n, NA)),
    event_type  = "Withdrawal",
    term        = cos_label,
    severity    = cos_code,                # used to colour the pill by code
    relatedness = NA_character_,
    status      = NA_character_,
    onset_date  = onset,
    report_date = as.Date(rep(NA, n), origin = "1970-01-01"),
    lag_days    = rep(NA_integer_, n),
    narrative   = as.character(.safety_col(df, reason, n, NA_character_))
  )
  # Extra mapped columns (e.g. cos, additional reason fields), defined in
  # Trial Settings → Detail fields.
  .append_detail_cols(tib, df, detail_fields_for("withdrawal"))
}

#' Pull complication rows — entirely driven by the columns mapped in
#' Trial Settings → Detail fields (section "complication"). A row counts when
#' any mapped complication column is non-empty.
complication_events <- function(raw_df) {
  extras <- detail_fields_for("complication")
  empty  <- tibble::tibble(record_id = character(0), site = character(0))
  if (is.null(raw_df) || !nrow(raw_df) || !length(extras)) return(empty)
  cols    <- vapply(extras, `[[`, character(1), "col")
  present <- cols[cols %in% names(raw_df)]
  if (!length(present)) return(empty)
  keep <- Reduce(`|`, lapply(present, function(c) {
    v <- trimws(as.character(raw_df[[c]])); !is.na(v) & nzchar(v) & v != "NA" & v != "0"
  }))
  if (!any(keep)) return(empty)
  df  <- raw_df[keep, , drop = FALSE]; n <- nrow(df)
  tib <- tibble::tibble(
    record_id = as.character(.safety_col(df, "record_id", n, NA)),
    site      = as.character(.safety_col(df, "site_dag",  n, NA)))
  .append_detail_cols(tib, df, extras)
}

# ─── Severity pill rendering ────────────────────────────────────────────────
# We don't know a priori what severity scale a trial uses (Mild/Moderate/...,
# 1/2/3, Grade I–V, etc.) so we rank the observed distinct values and assign
# colours from a calm-to-alarm gradient. The lowest-ranked value gets green,
# the highest gets red, anything in between is amber-shaded.

# Default colour ramp (low → high). Re-used by withdrawal too.
.severity_palette <- list(
  bg = c("#D1FAE5", "#FEF3C7", "#FED7AA", "#FECACA", "#FCA5A5"),
  fg = c("#065F46", "#92400E", "#9A3412", "#991B1B", "#7F1D1D")
)

#' Build a lookup from distinct severity values to colour indices.
#' Tries to detect numeric ordering first; falls back to alphabetical order
#' of the observed values. Returns a function (sev) -> list(bg, fg).
build_severity_palette <- function(values) {
  vals <- unique(stats::na.omit(as.character(values)))
  vals <- vals[nchar(trimws(vals)) > 0]
  if (length(vals) == 0) {
    return(function(v) list(bg = "#F1F5F9", fg = "#475569"))
  }
  # Sort: numeric if all parse, else by a learned ranking of common words,
  # else alphabetical.
  num <- suppressWarnings(as.numeric(vals))
  if (!any(is.na(num))) {
    ord <- order(num)
  } else {
    # Heuristic ranking for common scale words
    rank_words <- c("none" = 0, "trivial" = 1, "minor" = 1, "minimal" = 1,
                    "mild" = 2, "low" = 2, "grade 1" = 2, "grade i" = 2,
                    "moderate" = 3, "medium" = 3, "grade 2" = 3, "grade ii" = 3,
                    "severe" = 4, "high" = 4, "grade 3" = 4, "grade iii" = 4,
                    "serious" = 5, "life-threatening" = 5, "lifethreatening" = 5,
                    "grade 4" = 5, "grade iv" = 5,
                    "fatal" = 6, "death" = 6, "grade 5" = 6, "grade v" = 6)
    ranks <- rank_words[tolower(trimws(vals))]
    if (all(!is.na(ranks))) ord <- order(ranks) else ord <- order(vals)
  }
  ranked <- vals[ord]
  n <- length(ranked)
  # Map ranked positions onto the 5-step palette
  idx <- if (n <= 1) 1L else round(seq(1, length(.severity_palette$bg), length.out = n))
  lookup <- setNames(as.list(idx), ranked)

  function(v) {
    v_chr <- as.character(v)
    i <- lookup[[v_chr]]
    if (is.null(i)) return(list(bg = "#F1F5F9", fg = "#475569"))
    list(bg = .severity_palette$bg[[i]], fg = .severity_palette$fg[[i]])
  }
}

#' Render a single severity value as an HTML pill, using a palette function
#' built by build_severity_palette().
severity_pill <- function(value, palette_fn) {
  v <- value
  if (is.na(v) || !nzchar(trimws(as.character(v)))) {
    return('<span style="color:#94A3B8">—</span>')
  }
  pal <- palette_fn(v)
  sprintf(
    '<span style="display:inline-block;font-size:10px;font-weight:600;padding:2px 8px;border-radius:9px;background:%s;color:%s">%s</span>',
    pal$bg, pal$fg, htmltools::htmlEscape(as.character(v))
  )
}

#' Render a status value as a coloured dot + text. Heuristic: words containing
#' "open" / "pending" → amber, "review" → blue, "closed" / "resolved" → green.
status_dot <- function(value) {
  if (is.na(value) || !nzchar(trimws(as.character(value)))) {
    return('<span style="color:#94A3B8">—</span>')
  }
  v <- tolower(trimws(as.character(value)))
  col <- if (grepl("fatal|death|died|deceased", v))     "#DC2626"
         else if (grepl("open|pending|new|ongoing|active", v)) "#F59E0B"
         else if (grepl("review|investig", v))          "#3B82F6"
         else if (grepl("closed|resolv|complete", v))   "#10B981"
         else "#94A3B8"
  sprintf(
    '<span style="display:inline-flex;align-items:center;gap:5px;font-size:11px;font-weight:600"><span style="width:8px;height:8px;border-radius:50%%;background:%s"></span>%s</span>',
    col, htmltools::htmlEscape(as.character(value))
  )
}

#' Render a reporting-lag in days with red highlight when > threshold.
lag_html <- function(days, threshold = 5) {
  if (is.na(days)) return('<span style="color:#94A3B8">—</span>')
  cls <- if (days > threshold) "color:#DC2626;font-weight:600" else "color:#64748B"
  sprintf('<span style="font-size:11px;%s">%d day%s</span>',
          cls, as.integer(days), if (abs(days) == 1) "" else "s")
}
