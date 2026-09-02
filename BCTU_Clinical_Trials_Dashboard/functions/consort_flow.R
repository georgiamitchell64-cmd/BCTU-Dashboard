# =============================================================================
# TONIC Trial — CONSORT flow diagram (built with the `consort` R package)
# =============================================================================
# Produces a standard single-arm CONSORT diagram:
#
#   Enrolment:      Assessed for eligibility → Excluded
#   Allocation:     Randomised → Did not receive intervention (no-op)
#   Follow-up:      Lost / withdrew after randomisation
#   Analysis:       Analysed at Day 30 and Day 90
#
# Uses consort's manual add_box()/add_side_box() pipeline — aggregated counts,
# no per-participant expansion needed.
#
# Public functions:
#   consort_counts(rd, screening_xlsx_path = NULL)   →  list of counts
#   consort_object(counts)                           →  consort package object
#   consort_svg(counts)                              →  SVG string (HTML)
#   consort_png(counts, filepath, width, height)     →  PNG file (Word)
# =============================================================================

suppressPackageStartupMessages({
  library(grid)
  # The `consort` package is only needed for the legacy consort_object/svg/png
  # (Word) renderers. The dashboard + HTML report use the self-contained
  # consort_html() below, so make the package optional — the app still runs if
  # a deployment doesn't have it installed.
  if (requireNamespace("consort", quietly = TRUE)) library(consort)
})

# ── helper ───────────────────────────────────────────────────────────────────
.safe_int <- function(x, default = 0L) {
  if (is.null(x) || length(x) == 0) return(as.integer(default))
  v <- suppressWarnings(as.integer(x))
  if (is.na(v)) return(as.integer(default))
  v
}

# =============================================================================
# 1. Derive counts
# =============================================================================
consort_counts <- function(rd, screening_xlsx_path = NULL) {

  counts <- list(
    screened              = 0L,
    excluded              = 0L,
    randomised            = 0L,
    withdrawn_complete    = 0L,
    withdrawn_partial     = 0L,
    withdrawn_death       = 0L,
    lost_followup         = 0L,
    no_operation          = 0L,
    received_surgery      = 0L,
    awaiting_surgery      = 0L,
    reached_day30         = 0L,
    reached_day90         = 0L
  )

  counts$randomised <- .safe_int(rd$kpis$total_randomised, 0)

  fdf <- rd$filtered_df

  if (!is.null(fdf) && nrow(fdf) > 0 && "cos_type" %in% names(fdf)) {
    counts$withdrawn_death    <- sum(fdf$cos_type == 1, na.rm = TRUE)
    counts$no_operation       <- sum(fdf$cos_type == 2, na.rm = TRUE)
    counts$withdrawn_partial  <- sum(fdf$cos_type == 3, na.rm = TRUE)
    counts$withdrawn_complete <- sum(fdf$cos_type == 4, na.rm = TRUE)
    counts$lost_followup      <- sum(fdf$cos_type == 5, na.rm = TRUE)
  }

  if (!is.null(fdf) && "op_date" %in% names(fdf)) {
    counts$received_surgery <- sum(!is.na(fdf$op_date), na.rm = TRUE)
  }

  withdrawn_total <- counts$withdrawn_complete +
                     counts$withdrawn_death +
                     counts$withdrawn_partial
  counts$awaiting_surgery <- max(
    0L,
    counts$randomised - counts$received_surgery -
      withdrawn_total - counts$no_operation - counts$lost_followup
  )

  if (!is.null(fdf) && "op_date" %in% names(fdf)) {
    today  <- Sys.Date()
    has_op <- !is.na(fdf$op_date)
    counts$reached_day30 <- sum(has_op & (fdf$op_date + 30) <= today, na.rm = TRUE)
    counts$reached_day90 <- sum(has_op & (fdf$op_date + 90) <= today, na.rm = TRUE)
  }

  # Screened from screening Excel
  if (!is.null(screening_xlsx_path) && file.exists(screening_xlsx_path)) {
    tryCatch({
      sheet <- readxl::read_excel(screening_xlsx_path)
      col <- grep("^How many participants have you screened",
                  names(sheet), value = TRUE)[1]
      if (!is.na(col) && nzchar(col)) {
        vals <- suppressWarnings(as.integer(sheet[[col]]))
        counts$excluded <- sum(vals, na.rm = TRUE)
      }
    }, error = function(e) {
      message("consort_counts: screening file error — ", e$message)
    })
  }

  counts$screened <- counts$excluded + counts$randomised
  counts
}

# ── format a number inside a box label ───────────────────────────────────────
.n <- function(label, n) sprintf("%s (n = %d)", label, n)

# ── compose a multi-line bullet list for side boxes ──────────────────────────
.bullets <- function(...) {
  items <- list(...)
  items <- Filter(function(x) !is.null(x) && x[[2]] > 0, items)
  if (length(items) == 0) return(NULL)
  total <- sum(vapply(items, `[[`, numeric(1), 2))
  lines <- vapply(items, function(x) sprintf("\u2022 %s (n = %d)", x[[1]], x[[2]]),
                  character(1))
  paste0("Reason (n = ", total, "):\n", paste(lines, collapse = "\n"))
}

# =============================================================================
# 2. Build the consort object (manual add_box pipeline)
# =============================================================================
consort_object <- function(counts) {

  # ── TONIC colour palette for the diagram ───────────────────────────────
  # Cream = neutral / calm (enrolment, surgery-ready)
  # Light teal = progress / milestones (randomised, received intervention, analysed)
  # Light amber = side-boxes (exclusions, losses) — visual "nudge"
  # Navy border on every box for a consistent TONIC brand outline.
  fill_cream <- grid::gpar(fill = "#FFF8F0", col = "#1B4F6B", lwd = 1.5)
  fill_teal  <- grid::gpar(fill = "#E0F7F3", col = "#1B4F6B", lwd = 1.5)
  fill_amber <- grid::gpar(fill = "#FFF6E5", col = "#B8860B", lwd = 1.5)
  fill_navy  <- grid::gpar(fill = "#1B4F6B", col = "#1B4F6B", lwd = 1.5)

  text_navy  <- grid::gpar(cex = 0.85, fontfamily = "sans", col = "#1B4F6B")
  text_white <- grid::gpar(cex = 0.85, fontfamily = "sans", col = "#FFFFFF", fontface = "bold")
  text_amber <- grid::gpar(cex = 0.8,  fontfamily = "sans", col = "#6B4A0B")

  # Set the defaults for any internal textbox calls (e.g. label_box)
  old_opts <- options(
    txt_gp = text_navy,
    box_gp = fill_cream
  )
  on.exit(options(old_opts), add = TRUE)

  # ── Enrolment ──────────────────────────────────────────────────────────
  g <- consort::add_box(
    txt     = .n("Assessed for eligibility", counts$screened),
    box_gp  = fill_cream,
    txt_gp  = text_navy
  )

  if (counts$excluded > 0) {
    g <- consort::add_side_box(
      g,
      txt    = .n("Excluded", counts$excluded),
      box_gp = fill_amber,
      txt_gp = text_amber
    )
  }

  # ── Randomisation (milestone — teal) ───────────────────────────────────
  g <- consort::add_box(
    g,
    txt    = .n("Randomised", counts$randomised),
    box_gp = fill_teal,
    txt_gp = text_navy
  )

  # ── Allocation side-box: did not receive intervention (amber) ──────────
  alloc_bullets <- .bullets(
    list("Did not have operation", counts$no_operation),
    list("Died before surgery",    counts$withdrawn_death),
    list("Withdrew consent",       counts$withdrawn_complete)
  )
  if (!is.null(alloc_bullets)) {
    g <- consort::add_side_box(
      g,
      txt    = paste0("Did not receive intervention\n", alloc_bullets),
      box_gp = fill_amber,
      txt_gp = text_amber
    )
  }

  # ── Received intervention (milestone — teal) ───────────────────────────
  g <- consort::add_box(
    g,
    txt    = .n("Received intervention (surgery)", counts$received_surgery),
    box_gp = fill_teal,
    txt_gp = text_navy
  )

  # ── Follow-up side-box: post-randomisation losses (amber) ──────────────
  fu_bullets <- .bullets(
    list("Lost to follow-up",  counts$lost_followup),
    list("Partial withdrawal", counts$withdrawn_partial)
  )
  if (!is.null(fu_bullets)) {
    g <- consort::add_side_box(
      g,
      txt    = fu_bullets,
      box_gp = fill_amber,
      txt_gp = text_amber
    )
  }

  # ── Analysis boxes (milestone — teal) ──────────────────────────────────
  g <- consort::add_box(
    g,
    txt    = .n("Analysed at Day 30", counts$reached_day30),
    box_gp = fill_teal,
    txt_gp = text_navy
  )

  g <- consort::add_box(
    g,
    txt    = .n("Analysed at Day 90", counts$reached_day90),
    box_gp = fill_teal,
    txt_gp = text_navy
  )

  # ── Phase labels on the left (navy fill, white bold text) ──────────────
  # Temporarily switch box defaults so the label boxes get the navy treatment
  options(box_gp = fill_navy, txt_gp = text_white)
  label_indices <- c(
    "1" = "Enrolment",
    "2" = "Allocation",
    "3" = "Follow-up",
    "4" = "Analysis"
  )
  g <- consort::add_label_box(g, txt = label_indices)

  g
}

# =============================================================================
# 3. SVG string (for HTML reports)
# =============================================================================
# Uses the grViz route since the grid renderer produces fixed-pixel output
# that doesn't scale well. DiagrammeR + DiagrammeRsvg handles this cleanly.
# If those packages aren't available, falls back to rendering grid → svglite.
# =============================================================================
consort_svg <- function(counts, title = "CONSORT flow diagram",
                        width = 8, height = 10) {

  obj <- consort_object(counts)

  # Try the preferred route: grViz → svg
  svg_str <- tryCatch({
    if (requireNamespace("DiagrammeR",    quietly = TRUE) &&
        requireNamespace("DiagrammeRsvg", quietly = TRUE)) {
      gr  <- graphics::plot(obj, grViz = TRUE)
      DiagrammeRsvg::export_svg(gr)
    } else NULL
  }, error = function(e) NULL)

  # Fall-back: render to a grid SVG via svglite
  if (is.null(svg_str) || !nzchar(svg_str)) {
    if (!requireNamespace("svglite", quietly = TRUE)) {
      return("<p style='color:#B8421F'>CONSORT SVG unavailable \u2014 install svglite or DiagrammeRsvg.</p>")
    }

    tmp <- tempfile(fileext = ".svg")
    svglite::svglite(tmp, width = width, height = height, bg = "transparent")
    on.exit(try(dev.off(), silent = TRUE), add = TRUE)
    print(obj)
    dev.off()

    svg_str <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  }

  # Final touch-ups: arrow/connector strokes default to black on some
  # renderers — swap to navy so they match the TONIC palette. We leave
  # box fills alone (they were set per-box via box_gp in consort_object).
  svg_str <- gsub('stroke="#000000"', 'stroke="#1B4F6B"', svg_str, fixed = TRUE)
  svg_str <- gsub('stroke:#000000',   'stroke:#1B4F6B',   svg_str, fixed = TRUE)

  svg_str
}

# =============================================================================
# 4. PNG file (for Word reports)
# =============================================================================
consort_png <- function(counts, filepath, width = 8, height = 10, dpi = 150) {

  obj <- consort_object(counts)

  grDevices::png(
    filename = filepath,
    width    = width,
    height   = height,
    units    = "in",
    res      = dpi,
    bg       = "white"
  )
  on.exit(try(dev.off(), silent = TRUE), add = TRUE)

  print(obj)

  invisible(filepath)
}

# =============================================================================
# 5. Live counts from a REDCap export (dashboard + HTML report)
# =============================================================================
# Unlike consort_counts() (which needs a prepared report-data object), this
# derives the flow straight from raw_redcap + the trial config, and breaks
# withdrawals down by their change-of-status type so it's clear what kind they
# are. Returns a plain list — no package dependencies.
consort_counts_live <- function(raw, cfg = current_trial_config()) {
  out <- list(randomised = 0L, received = 0L, in_followup = 0L,
              n_withdrawn = 0L, has_surgery = FALSE,
              withdrawals = data.frame(label = character(), n = integer(),
                                       stringsAsFactors = FALSE))
  if (is.null(raw) || !nrow(raw)) return(out)
  cfg <- cfg %||% list()

  id_col   <- cfg$redcap_fields$record_id %||% "record_id"
  if (!id_col %in% names(raw)) id_col <- names(raw)[1]
  ids      <- raw[[id_col]]
  rand_col <- cfg$redcap_fields$randomisation_datetime %||% "rand_dttm_s"
  bevt     <- cfg$redcap_events$baseline %||% "baseline_arm_1"

  if (rand_col %in% names(raw)) {
    v <- trimws(as.character(raw[[rand_col]]))
    keep <- !is.na(v) & nzchar(v) & v != "NA"
    out$randomised <- length(unique(ids[keep]))
  } else if ("redcap_event_name" %in% names(raw)) {
    out$randomised <- length(unique(baseline_rows(raw, cfg)[[id_col]]))
  } else {
    out$randomised <- length(unique(ids))
  }

  # Withdrawals broken down by change-of-status code → trial's own labels.
  cos_col <- cfg$redcap_fields$cos_type %||% "cos_type"
  if (!is.null(cos_col) && cos_col %in% names(raw)) {
    cv    <- trimws(as.character(raw[[cos_col]]))
    valid <- !is.na(cv) & nzchar(cv) & cv != "NA" & cv != "0"
    if (any(valid)) {
      labs  <- cfg$cos_type_labels
      codes <- sort(unique(cv[valid]))
      rows  <- lapply(codes, function(cd) {
        lab <- if (!is.null(labs) && cd %in% names(labs)) labs[[cd]] else paste("Type", cd)
        data.frame(label = lab, n = length(unique(ids[valid & cv == cd])),
                   stringsAsFactors = FALSE)
      })
      out$withdrawals <- do.call(rbind, rows)
      out$n_withdrawn <- length(unique(ids[valid]))
    }
  }

  # Received intervention — only meaningful when the trial captures an op date.
  op_col <- cfg$redcap_fields$operation_date
  if (!is.null(op_col) && !is.na(op_col) && nzchar(op_col) && op_col %in% names(raw)) {
    out$has_surgery <- TRUE
    ov <- trimws(as.character(raw[[op_col]]))
    out$received <- length(unique(ids[!is.na(ov) & nzchar(ov) & ov != "NA"]))
  }

  out$in_followup <- max(0L, out$randomised - out$n_withdrawn)
  out
}

# =============================================================================
# 6. Self-contained HTML CONSORT diagram (dashboard + HTML report)
# =============================================================================
# Inline-styled so it renders identically on the dashboard and inside the
# downloaded report HTML. Withdrawals branch off as an amber card listing each
# type and its count.
consort_html <- function(counts, cfg = NULL) {
  navy <- "#1B4F6B"; teal_bg <- "#E0F7F3"; amber_bg <- "#FFF7E6"; amber_br <- "#E0A93B"
  esc  <- function(x) htmltools::htmlEscape(x)

  box <- function(label, n) sprintf(
    "<div style='background:%s;border:1.5px solid %s;border-radius:12px;
                 padding:13px 20px;min-width:260px;text-align:center;
                 box-shadow:0 1px 3px rgba(0,0,0,.05);'>
       <div style='font-size:13px;font-weight:600;color:%s;'>%s</div>
       <div style='font-size:23px;font-weight:800;color:%s;line-height:1.1;margin-top:2px;'>%s</div>
     </div>",
    teal_bg, navy, navy, esc(label), navy, format(n, big.mark = ","))
  conn <- "<div style='width:2px;height:24px;background:#9FB6C4;margin:2px auto;'></div>"

  wd <- counts$withdrawals
  wd_card <- ""
  if (!is.null(wd) && nrow(wd) > 0) {
    items <- paste(vapply(seq_len(nrow(wd)), function(i)
      sprintf("<div style='display:flex;justify-content:space-between;gap:18px;
                           font-size:12px;color:#7A5B12;padding:3px 0;
                           border-top:1px solid rgba(224,169,59,.3);'>
                 <span>%s</span><strong>%d</strong></div>",
              esc(wd$label[i]), wd$n[i]), character(1)), collapse = "")
    wd_card <- sprintf(
      "<div style='align-self:center;'>
         <div style='font-size:11px;color:#9FB6C4;text-align:center;margin-bottom:4px;'>&larr; discontinued</div>
         <div style='background:%s;border:1.5px solid %s;border-radius:12px;padding:12px 16px;min-width:240px;'>
           <div style='font-size:11.5px;font-weight:700;color:#7A5B12;text-transform:uppercase;
                       letter-spacing:.4px;'>Withdrawn / discontinued (n = %d)</div>%s</div>
       </div>", amber_bg, amber_br, counts$n_withdrawn, items)
  }

  rows <- box("Randomised", counts$randomised)
  if (isTRUE(counts$has_surgery))
    rows <- paste0(rows, conn, box("Received intervention", counts$received))
  rows <- paste0(rows, conn, box("In follow-up", counts$in_followup))
  main_col <- sprintf("<div style='display:flex;flex-direction:column;align-items:center;'>%s</div>", rows)

  sprintf(
    "<div style='font-family:Inter,system-ui,sans-serif;display:flex;
                 align-items:center;justify-content:center;gap:30px;flex-wrap:wrap;
                 padding:8px 0;'>%s%s</div>",
    main_col, wd_card)
}
