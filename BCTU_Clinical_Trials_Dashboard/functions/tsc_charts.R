# =============================================================================
# TSC report — charts for the Word document
# =============================================================================
# Each chart takes the list from prepare_report_data() (rd) and writes a PNG to
# `filepath`, returning the path invisibly for knitr::include_graphics(). They
# are sized for a portrait Word page (6.3 in of text width), drawn at 300 dpi
# with ragg so the text stays crisp in print, and set in Arial — the report's
# own font. Colours follow the trial palette, as on the dashboard.

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

# Kept for templates that still refer to them
tonic_navy  <- "#1B1B1B"
tonic_teal  <- "#00ACA9"
tonic_muted <- "#6B6B6D"
tonic_grid  <- "#E6E6E3"

.tsc_or <- function(a, b) {
  if (is.null(a) || !length(a) || is.na(a[1]) || !nzchar(as.character(a[1]))) b else a[1]
}

.tsc_font <- function() {
  fams <- tryCatch(systemfonts::system_fonts()$family, error = function(e) character(0))
  if ("Arial" %in% fams) "Arial" else "sans"
}

# The trial's colours where the app has them; neutral fallbacks otherwise
.tsc_pal <- function() {
  p <- tryCatch(trial_palette(), error = function(e) NULL)
  list(primary   = .tsc_or(p[["primary"]],   "#1B1B1B"),
       secondary = .tsc_or(p[["secondary"]], "#00788E"),
       ink = "#1B1B1B", ink2 = "#3C3C3B", muted = "#6B6B6D", grid = "#E6E6E3",
       target = "#8A8A8C", warn = "#C59A00", bad = "#C20019", none = "#9A9A9C")
}

# --- Theme -----------------------------------------------------------------
theme_tonic <- function(base_size = 9.5, base_family = .tsc_font()) {
  pal <- .tsc_pal()
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      text                 = element_text(colour = pal$ink),
      plot.title           = element_text(face = "bold", size = base_size + 2,
                                          colour = pal$ink, margin = margin(b = 3)),
      plot.subtitle        = element_text(size = base_size, colour = pal$muted,
                                          margin = margin(b = 8)),
      plot.title.position  = "plot",
      plot.caption         = element_text(size = base_size - 1.5, colour = pal$muted,
                                          hjust = 0, margin = margin(t = 6)),
      plot.caption.position = "plot",
      axis.title           = element_text(size = base_size - 0.5, colour = pal$muted),
      axis.text            = element_text(size = base_size - 1, colour = pal$ink2),
      axis.line.x          = element_line(colour = "#BDBDBD", linewidth = 0.4),
      axis.ticks.x         = element_line(colour = "#BDBDBD", linewidth = 0.4),
      axis.ticks.length    = unit(2, "pt"),
      panel.grid.major.y   = element_line(colour = pal$grid, linewidth = 0.3),
      panel.grid.major.x   = element_blank(),
      panel.grid.minor     = element_blank(),
      legend.position      = "top",
      legend.justification = "left",
      legend.title         = element_blank(),
      legend.text          = element_text(size = base_size - 1, colour = pal$ink2),
      legend.key.size      = unit(9, "pt"),
      legend.margin        = margin(0, 0, 2, 0),
      legend.box.spacing   = unit(2, "pt"),
      plot.margin          = margin(6, 10, 6, 6)
    )
}

.tsc_save <- function(p, filepath, width, height, dpi) {
  dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
  ggsave(filepath, p, width = width, height = height, units = "in", dpi = dpi,
         bg = "white", device = dev)
  invisible(filepath)
}

# Month ticks that stay readable however long the trial has run
.tsc_month_breaks <- function(from, to) {
  n    <- length(seq(from, to, by = "month"))
  step <- if (n <= 9) 1 else if (n <= 18) 2 else if (n <= 36) 3 else 6
  seq(from, to, by = paste(step, "months"))
}

.tsc_this_month <- function() as.Date(format(Sys.Date(), "%Y-%m-01"))

# --- 1. Cumulative recruitment (actual vs target) --------------------------
chart_cumulative_recruitment <- function(rd, filepath,
                                         width = 6.3, height = 3.1, dpi = 300) {
  sf <- rd$schedule_full
  if (is.null(sf) || nrow(sf) == 0) return(invisible(NULL))
  pal <- .tsc_pal()
  sf  <- sf[order(sf$month_date), , drop = FALSE]
  sf$actual <- ifelse(sf$month_date <= .tsc_this_month(), sf$cumulative_actual, NA_real_)
  now <- sf[!is.na(sf$actual), , drop = FALSE]
  now <- now[nrow(now), , drop = FALSE]

  # The headline figure goes in the subtitle, so nothing overlaps the lines
  sub <- "Randomised participants against the protocol target"
  if (nrow(now)) {
    t <- now$cumulative_target
    sub <- if (!is.na(t) && t > 0)
      sprintf("%s randomised so far · %d%% of the %s the protocol expects by now",
              format(now$actual, big.mark = ","), round(100 * now$actual / t),
              format(round(t), big.mark = ","))
    else sprintf("%s randomised so far", format(now$actual, big.mark = ","))
  }

  p <- ggplot(sf, aes(x = month_date)) +
    geom_ribbon(aes(ymin = 0, ymax = actual), fill = pal$primary, alpha = 0.10, na.rm = TRUE) +
    geom_line(aes(y = cumulative_target, colour = "Protocol target"),
              linewidth = 0.6, linetype = "22") +
    geom_line(aes(y = actual, colour = "Randomised"), linewidth = 1, na.rm = TRUE) +
    geom_point(data = now, aes(y = actual), colour = pal$primary, size = 2) +
    scale_colour_manual(values = c("Randomised" = pal$primary, "Protocol target" = pal$target),
                        breaks = c("Randomised", "Protocol target")) +
    scale_x_date(breaks = .tsc_month_breaks(min(sf$month_date), max(sf$month_date)),
                 date_labels = "%b %y", expand = expansion(mult = c(0.01, 0.02))) +
    scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.06))) +
    labs(title = "Cumulative recruitment", subtitle = sub, x = NULL, y = NULL) +
    theme_tonic()
  .tsc_save(p, filepath, width, height, dpi)
}

# --- 2. Monthly recruitment against the monthly target ---------------------
chart_monthly_recruitment <- function(rd, filepath,
                                      width = 6.3, height = 2.9, dpi = 300) {
  pal <- .tsc_pal()
  mo  <- rd$monthly_recruit
  cm  <- .tsc_this_month()
  first  <- if (!is.null(mo) && nrow(mo) > 0) min(mo$month_date) else cm
  months <- seq(first, cm, by = "month")
  df <- data.frame(month_date = months)
  df$n <- if (!is.null(mo) && nrow(mo) > 0)
    mo$n[match(df$month_date, mo$month_date)] else NA_integer_
  df$n[is.na(df$n)] <- 0L
  df$in_progress <- df$month_date == cm

  # Monthly target = the step in the protocol's cumulative target
  sf <- rd$schedule_full
  df$target <- NA_real_
  if (!is.null(sf) && nrow(sf) && all(c("month_date", "cumulative_target") %in% names(sf))) {
    sf <- sf[order(sf$month_date), , drop = FALSE]
    per_month <- c(sf$cumulative_target[1], diff(sf$cumulative_target))
    df$target <- per_month[match(df$month_date, sf$month_date)]
  }
  has_target <- any(!is.na(df$target) & df$target > 0)

  p <- ggplot(df, aes(x = month_date, y = n)) +
    geom_col(aes(fill = in_progress), width = 24, show.legend = FALSE) +
    geom_text(aes(label = ifelse(n > 0, n, "")), vjust = -0.45, size = 2.8,
              colour = pal$ink, family = .tsc_font())
  if (has_target)
    p <- p + geom_step(aes(y = target, colour = "Monthly target (protocol)"),
                       direction = "mid", linewidth = 0.6, linetype = "22", na.rm = TRUE) +
      scale_colour_manual(values = c("Monthly target (protocol)" = pal$target))
  p <- p +
    scale_fill_manual(values = c("FALSE" = pal$primary, "TRUE" = scales::alpha(pal$primary, 0.45))) +
    scale_x_date(breaks = .tsc_month_breaks(first, cm), date_labels = "%b %y",
                 expand = expansion(add = 20)) +
    scale_y_continuous(breaks = breaks_pretty(), expand = expansion(mult = c(0, 0.15))) +
    labs(title = "Monthly recruitment",
         subtitle = sprintf("Participants randomised each month · %s is still in progress (lighter bar)",
                            format(cm, "%B")),
         x = NULL, y = NULL) +
    theme_tonic()
  .tsc_save(p, filepath, width, height, dpi)
}

# --- 3. Recruitment rate by site against its monthly target ----------------
# Average randomisations per month since each site opened (the same figure as
# the dashboard's Sites tab and the TMG report), with the site's monthly target
# marked. Sites without a rate fall back to their total randomised.
chart_site_recruitment <- function(rd, filepath,
                                   width = 6.3, height = NULL, dpi = 300) {
  pal <- .tsc_pal()
  st  <- rd$site_status
  ok  <- !is.null(st) && nrow(st) > 0 && all(c("actual_monthly", "randomisations") %in% names(st))
  df  <- if (ok) st[!is.na(st$actual_monthly) & st$randomisations > 0, , drop = FALSE] else NULL

  if (is.null(df) || !nrow(df)) {
    ss <- rd$site_summary
    if (is.null(ss) || !nrow(ss)) return(invisible(NULL))
    ss$site_name <- factor(ss$site_name, levels = ss$site_name[order(ss$randomisations)])
    if (is.null(height)) height <- max(2.4, nrow(ss) * 0.26 + 1.4)
    p <- ggplot(ss, aes(x = randomisations, y = site_name)) +
      geom_col(fill = pal$primary, width = 0.62) +
      geom_text(aes(label = randomisations), hjust = -0.25, size = 2.8, family = .tsc_font()) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(title = "Randomisations by site",
           subtitle = "Total randomised · add site open dates on the Sites tab to show monthly rates",
           x = NULL, y = NULL) +
      theme_tonic() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(colour = pal$grid, linewidth = 0.3))
    return(.tsc_save(p, filepath, width, height, dpi))
  }

  df$target <- suppressWarnings(as.numeric(df$monthly_target))
  df$ratio  <- ifelse(!is.na(df$target) & df$target > 0, df$actual_monthly / df$target, NA_real_)
  bands <- c("At or above target", "60–99% of target", "Below 60% of target", "No target set")
  df$band <- factor(ifelse(is.na(df$ratio), bands[4],
                    ifelse(df$ratio >= 1, bands[1], ifelse(df$ratio >= 0.6, bands[2], bands[3]))),
                    levels = bands)
  est <- if ("open_estimated" %in% names(df)) !is.na(df$open_estimated) & df$open_estimated else rep(FALSE, nrow(df))
  df$label <- paste0(df$site_name, ifelse(est, "*", ""))
  df$label <- factor(df$label, levels = df$label[order(df$actual_monthly)])
  if (is.null(height)) height <- max(2.4, nrow(df) * 0.26 + 1.5)

  p <- ggplot(df, aes(y = label)) +
    geom_col(aes(x = actual_monthly, fill = band), width = 0.62) +
    geom_errorbar(aes(xmin = target, xmax = target), width = 0.8, linewidth = 0.7,
                  colour = pal$ink, na.rm = TRUE) +
    geom_text(aes(x = pmax(actual_monthly, ifelse(is.na(target), 0, target)),
                  label = sprintf("%.1f  (%d)", actual_monthly, as.integer(randomisations))),
              hjust = -0.2, size = 2.7, colour = pal$ink2, family = .tsc_font()) +
    scale_fill_manual(values = setNames(c(pal$primary, pal$warn, pal$bad, pal$none), bands),
                      drop = TRUE) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(title = "Recruitment rate by site",
         subtitle = "Average randomisations per month since each site opened · the black mark is the site's monthly target",
         caption = paste0("Figures in brackets are the total randomised.",
                          if (any(est)) " * No open date recorded, so the first randomisation is used." else ""),
         x = NULL, y = NULL) +
    theme_tonic() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = pal$grid, linewidth = 0.3),
          axis.line.x = element_blank(), axis.ticks.x = element_blank())
  .tsc_save(p, filepath, width, height, dpi)
}

# --- 4. Open centres over time ---------------------------------------------
# Sites open at the end of each month (step line) and sites opening that month
# (bars), on one axis. Uses the Sites tab open dates, falling back to each
# site's first randomisation when none are recorded.
#
# Counts only centres the REDCap export knows about (site_status$in_redcap),
# and drops closed or withdrawn ones: a site typed into the Sites tab during
# set-up has an open date but no trial data behind it, and counting those
# overstated how many centres were actually open.
.tsc_open_sites_rows <- function(st) {
  if (is.null(st) || !nrow(st)) return(st)
  keep <- rep(TRUE, nrow(st))
  if ("in_redcap" %in% names(st)) keep <- keep & !is.na(st$in_redcap) & st$in_redcap
  if ("stage" %in% names(st))
    keep <- keep & !grepl("closed|withdrawn", as.character(st$stage), ignore.case = TRUE)
  st[keep, , drop = FALSE]
}

chart_open_sites <- function(rd, filepath,
                             width = 6.3, height = 2.8, dpi = 300) {
  pal <- .tsc_pal()
  cm  <- .tsc_this_month()
  st  <- .tsc_open_sites_rows(rd$site_status)
  od  <- if (!is.null(st) && nrow(st) && "open_date" %in% names(st))
    suppressWarnings(as.Date(st$open_date)) else as.Date(character(0))
  od  <- od[!is.na(od) & od <= Sys.Date()]
  if (!length(od) && !is.null(rd$site_summary) && "first_rand_date" %in% names(rd$site_summary))
    od <- stats::na.omit(as.Date(rd$site_summary$first_rand_date))

  if (!length(od)) {
    p <- ggplot() +
      annotate("text", x = 0, y = 0, label = "No site open dates recorded yet",
               colour = pal$muted, size = 3.2, family = .tsc_font()) +
      labs(title = "Open centres over time", x = NULL, y = NULL) +
      theme_void(base_family = .tsc_font()) +
      theme(plot.title = element_text(face = "bold", size = 11.5))
    return(.tsc_save(p, filepath, width, 1.4, dpi))
  }

  months <- seq(as.Date(format(min(od), "%Y-%m-01")), cm, by = "month")
  key    <- format(od, "%Y-%m")
  df <- data.frame(month = months,
                   new = vapply(format(months, "%Y-%m"), function(m) sum(key == m), integer(1),
                                USE.NAMES = FALSE))
  df$open <- cumsum(df$new)
  opened  <- df[df$new > 0, , drop = FALSE]

  p <- ggplot(df, aes(x = month)) +
    geom_col(aes(y = new, fill = "Sites opening that month"), width = 20) +
    geom_step(aes(y = open, colour = "Sites open"), linewidth = 1, direction = "hv") +
    geom_point(data = opened, aes(y = open), colour = pal$primary, size = 1.8) +
    geom_text(data = opened, aes(y = open, label = open), vjust = -0.9, size = 2.7,
              colour = pal$ink, family = .tsc_font()) +
    scale_fill_manual(values = c("Sites opening that month" = scales::alpha(pal$secondary, 0.45))) +
    scale_colour_manual(values = c("Sites open" = pal$primary)) +
    scale_x_date(breaks = .tsc_month_breaks(min(months), cm), date_labels = "%b %y",
                 expand = expansion(add = 15)) +
    scale_y_continuous(breaks = breaks_pretty(), expand = expansion(mult = c(0, 0.18))) +
    labs(title = "Open centres over time",
         subtitle = sprintf("%d site%s open", max(df$open), if (max(df$open) == 1) "" else "s"),
         x = NULL, y = NULL) +
    theme_tonic()
  .tsc_save(p, filepath, width, height, dpi)
}

# --- Helper: render all charts into a tmp directory, return named list ----
render_all_tsc_charts <- function(rd, outdir = tempfile("tsc_charts_")) {
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  list(
    cumulative = chart_cumulative_recruitment(rd, file.path(outdir, "cumulative.png")),
    monthly    = chart_monthly_recruitment(rd, file.path(outdir, "monthly.png")),
    site       = chart_site_recruitment(rd, file.path(outdir, "site.png")),
    open_sites = chart_open_sites(rd, file.path(outdir, "open_sites.png")),
    gantt      = chart_gantt(rd, file.path(outdir, "gantt.png"))
  )
}

# --- 5. Gantt chart (project timeline) -----------------------------------
# Matches the TMG HTML version: 7 phases from Jun 2024 to Feb 2029, with a
# vertical "Now" marker. Phase start/end dates are fixed in the protocol.
chart_gantt <- function(rd, filepath,
                        width = 6.3, height = 3.1, dpi = 300) {

  # Fixed phase schedule (month offsets from Jun 2024, matching TMG logic)
  trial_start <- as.Date("2024-06-01")
  add_months <- function(start, n) seq.Date(start, length.out = 2, by = paste(n, "months"))[2]

  phases <- data.frame(
    phase  = c("Study setup", "Recruitment pilot (6mo)", "Full trial (21mo)",
               "Treatment / follow-up", "Data cleaning",
               "Analysis & writeup", "Close out"),
    start  = c(add_months(trial_start, 0),   # Jun 2024
               add_months(trial_start, 21),  # Mar 2026 (pilot starts)
               add_months(trial_start, 27),  # Sep 2026 (full trial)
               add_months(trial_start, 21),  # Mar 2026 (FU parallel)
               add_months(trial_start, 49),  # Jul 2028
               add_months(trial_start, 51),  # Sep 2028
               add_months(trial_start, 54)), # Dec 2028
    end    = c(add_months(trial_start, 21),  # up to Mar 2026
               add_months(trial_start, 27),  # Sep 2026
               add_months(trial_start, 48),  # Jun 2028
               add_months(trial_start, 51),  # Sep 2028
               add_months(trial_start, 52),  # Oct 2028
               add_months(trial_start, 55),  # Jan 2029
               add_months(trial_start, 56)), # Feb 2029
    colour = c("#85B7EB", .tsc_pal()$primary, .tsc_pal()$secondary, "#FAC775",
               "#97C459", "#F0997B", "#ED93B1"),
    stringsAsFactors = FALSE
  )
  # Enforce row order top-to-bottom
  phases$phase <- factor(phases$phase, levels = rev(phases$phase))

  today <- Sys.Date()
  timeline_start <- trial_start
  timeline_end   <- add_months(trial_start, 57)
  pal <- .tsc_pal()

  p <- ggplot(phases) +
    geom_rect(aes(xmin = start, xmax = end,
                  ymin = as.numeric(phase) - 0.32,
                  ymax = as.numeric(phase) + 0.32,
                  fill = phase),
              colour = NA, alpha = 0.9) +
    geom_vline(xintercept = today,
               colour = pal$bad, linewidth = 0.6, linetype = "22") +
    annotate("text", x = today, y = length(levels(phases$phase)) + 0.65,
             label = "Now", colour = pal$bad, fontface = "bold",
             size = 2.8, hjust = -0.15, family = .tsc_font()) +
    # Colours keyed by each row's own phase (levels are reversed for display)
    scale_fill_manual(values = setNames(phases$colour, as.character(phases$phase))) +
    scale_y_continuous(breaks = seq_len(nlevels(phases$phase)),
                       labels = levels(phases$phase),
                       expand = expansion(mult = c(0.06, 0.14))) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %y",
                 limits = c(timeline_start, timeline_end),
                 expand = expansion(mult = c(0.01, 0.02))) +
    labs(title = "Project timeline",
         subtitle = "Protocol phases, with today marked",
         x = NULL, y = NULL) +
    theme_tonic() +
    theme(legend.position = "none",
          axis.text.y = element_text(colour = pal$ink),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = pal$grid, linewidth = 0.3))

  .tsc_save(p, filepath, width, height, dpi)
}
