# =============================================================================
# TONIC TSC Report — ggplot2 chart helpers (PNG output for Word embedding)
# =============================================================================
# Each function takes a slice of rd (the list from prepare_report_data)
# and writes a PNG to the supplied filepath. Returns the filepath invisibly
# so it can be chained into an Rmd chunk.
#
# Styled to match TONIC brand: navy (#1B4F6B) + teal (#2EC4A5), Outfit font
# if available, sans fallback otherwise.

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

# --- Theme -----------------------------------------------------------------
tonic_navy <- "#1B4F6B"
tonic_teal <- "#2EC4A5"
tonic_muted <- "#6b7c8d"
tonic_grid  <- "#e0e8ef"

theme_tonic <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      text              = element_text(colour = "#2C3E50"),
      plot.title        = element_text(colour = tonic_navy, face = "bold",
                                       size = base_size + 1, margin = margin(b = 6)),
      plot.subtitle     = element_text(colour = tonic_muted, size = base_size - 1,
                                       margin = margin(b = 10)),
      axis.title        = element_text(colour = tonic_muted, size = base_size - 1),
      axis.text         = element_text(colour = tonic_muted, size = base_size - 2),
      panel.grid.major  = element_line(colour = tonic_grid, linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position   = "bottom",
      legend.title      = element_blank(),
      legend.text       = element_text(size = base_size - 1, colour = tonic_muted),
      legend.key.size   = unit(0.4, "cm"),
      plot.margin       = margin(8, 12, 8, 8)
    )
}

# --- 1. Cumulative recruitment (actual vs target) --------------------------
chart_cumulative_recruitment <- function(rd, filepath,
                                          width = 6.5, height = 3.5, dpi = 150) {
  sf <- rd$schedule_full
  if (is.null(sf) || nrow(sf) == 0) return(invisible(NULL))

  current_month <- as.Date(format(Sys.Date(), "%Y-%m-01"))
  sf$actual_display <- ifelse(sf$month_date <= current_month,
                               sf$cumulative_actual, NA_real_)

  p <- ggplot(sf, aes(x = month_date)) +
    geom_ribbon(aes(ymin = 0, ymax = actual_display),
                fill = tonic_teal, alpha = 0.15, na.rm = TRUE) +
    geom_line(aes(y = cumulative_target, linetype = "Target"),
              colour = tonic_muted, linewidth = 0.6) +
    geom_line(aes(y = actual_display, linetype = "Actual"),
              colour = tonic_navy, linewidth = 1.1, na.rm = TRUE) +
    geom_point(aes(y = actual_display),
               colour = tonic_navy, size = 2, na.rm = TRUE) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_linetype_manual(values = c("Actual" = "solid", "Target" = "dashed")) +
    labs(title = "Cumulative recruitment",
         subtitle = "Actual randomisations vs protocol target",
         x = NULL, y = "Cumulative participants") +
    theme_tonic()

  ggsave(filepath, p, width = width, height = height, dpi = dpi, bg = "white")
  invisible(filepath)
}

# --- 2. Monthly recruitment bars -------------------------------------------
chart_monthly_recruitment <- function(rd, filepath,
                                       width = 6.5, height = 3, dpi = 150) {
  mo <- rd$monthly_recruit
  current_month <- as.Date(format(Sys.Date(), "%Y-%m-01"))

  # Start from the first recruitment month; extend 2 months beyond current
  # for forward context. If no data yet, show current month + 2 ahead.
  first_month <- if (!is.null(mo) && nrow(mo) > 0) min(mo$month_date) else current_month
  window_end  <- seq.Date(current_month, length.out = 2, by = "2 months")[2]

  all_m <- seq.Date(first_month, window_end, by = "month")
  df <- data.frame(month_date = all_m)
  if (!is.null(mo) && nrow(mo) > 0) {
    df <- merge(df, mo[, c("month_date", "n")], by = "month_date", all.x = TRUE)
  } else {
    df$n <- NA_integer_
  }
  df$n[is.na(df$n)] <- 0
  df$is_current <- df$month_date == current_month

  p <- ggplot(df, aes(x = month_date, y = n)) +
    geom_col(aes(fill = is_current),
             width = 22, show.legend = FALSE) +
    geom_text(aes(label = ifelse(n > 0, n, "")),
              vjust = -0.5, size = 3, colour = tonic_navy, fontface = "bold") +
    scale_fill_manual(values = c("TRUE" = tonic_teal, "FALSE" = tonic_navy)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y",
                 limits = c(first_month - 15, window_end + 15)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.20)),
                       breaks = function(limits) {
                         upper <- max(1, ceiling(limits[2]))
                         seq(0, upper, by = max(1, ceiling(upper / 6)))
                       }) +
    labs(title = "Monthly recruitment",
         subtitle = "Current month highlighted in teal",
         x = NULL, y = "Participants randomised") +
    theme_tonic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(filepath, p, width = width, height = height, dpi = dpi, bg = "white")
  invisible(filepath)
}

# --- 3. Randomisations by site vs monthly target ---------------------------
chart_site_recruitment <- function(rd, filepath,
                                    width = 6.5, height = NULL, dpi = 150) {
  ss  <- rd$site_summary
  sst <- rd$site_status
  if (is.null(ss) || nrow(ss) == 0) return(invisible(NULL))

  df <- ss[, c("site_name", "randomisations"), drop = FALSE]
  if (!is.null(sst) && nrow(sst) > 0 && "monthly_target" %in% names(sst)) {
    df <- merge(df, sst[, c("site_name", "monthly_target"), drop = FALSE],
                by = "site_name", all.x = TRUE)
  }
  if (!"monthly_target" %in% names(df)) df$monthly_target <- 2
  df$monthly_target[is.na(df$monthly_target)] <- 2

  df$site_name <- factor(df$site_name,
                          levels = df$site_name[order(df$randomisations)])

  # Long format for dodge
  long <- rbind(
    data.frame(site_name = df$site_name, value = df$randomisations,
               series = "Actual (total)", stringsAsFactors = FALSE),
    data.frame(site_name = df$site_name, value = df$monthly_target,
               series = "Monthly target", stringsAsFactors = FALSE)
  )
  long$series <- factor(long$series, levels = c("Actual (total)", "Monthly target"))

  if (is.null(height)) height <- max(2.5, nrow(df) * 0.32 + 1)

  p <- ggplot(long, aes(x = value, y = site_name, fill = series)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    scale_fill_manual(values = c("Actual (total)" = tonic_navy,
                                 "Monthly target" = scales::alpha(tonic_teal, 0.55))) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.08)),
                       breaks = function(limits) seq(0, ceiling(limits[2]), by = 1)) +
    labs(title = "Randomisations by site",
         subtitle = "Actual total vs monthly target",
         x = NULL, y = NULL) +
    theme_tonic()

  ggsave(filepath, p, width = width, height = height, dpi = dpi, bg = "white")
  invisible(filepath)
}

# --- 4. Open sites over time (bar chart) ----------------------------------
chart_open_sites <- function(rd, filepath,
                              width = 6.5, height = 3.2, dpi = 150) {
  ss <- rd$site_summary
  current_month <- as.Date(format(Sys.Date(), "%Y-%m-01"))

  if (is.null(ss) || nrow(ss) == 0 || !"first_rand_date" %in% names(ss)) {
    # Empty placeholder chart
    df <- data.frame(month = seq.Date(current_month, length.out = 2, by = "-3 months")[2],
                     new_sites = 0, cumulative = 0)
  } else {
    valid <- ss[!is.na(ss$first_rand_date), , drop = FALSE]
    if (nrow(valid) == 0) {
      df <- data.frame(month = current_month, new_sites = 0, cumulative = 0)
    } else {
      valid$month <- as.Date(format(valid$first_rand_date, "%Y-%m-01"))
      agg <- aggregate(list(new_sites = rep(1, nrow(valid))),
                        by = list(month = valid$month), FUN = sum)
      agg <- agg[order(agg$month), ]

      # Start from the first opening month; extend 2 months beyond current
      window_start <- min(agg$month)
      window_end   <- seq.Date(current_month, length.out = 2, by = "2 months")[2]
      all_m <- seq.Date(window_start, window_end, by = "month")
      df <- data.frame(month = all_m)
      df <- merge(df, agg, by = "month", all.x = TRUE)
      df$new_sites[is.na(df$new_sites)] <- 0
      df$cumulative <- cumsum(df$new_sites)
    }
  }
  df$is_current <- df$month == current_month

  # Primary geom: bars for new sites opening each month
  # Secondary: line for cumulative (use sec_axis so both read cleanly)
  max_cum <- max(df$cumulative, 1)
  max_new <- max(df$new_sites, 1)
  scale_factor <- max_cum / max_new

  p <- ggplot(df, aes(x = month)) +
    geom_col(aes(y = new_sites, fill = is_current),
             width = 22, show.legend = FALSE) +
    geom_line(aes(y = cumulative / scale_factor, group = 1),
              colour = tonic_navy, linewidth = 1, linetype = "dashed") +
    geom_point(aes(y = cumulative / scale_factor),
               colour = tonic_navy, size = 2.2) +
    geom_text(aes(y = new_sites, label = ifelse(new_sites > 0, new_sites, "")),
              vjust = -0.5, size = 3, colour = tonic_navy, fontface = "bold") +
    scale_fill_manual(values = c("TRUE" = tonic_teal, "FALSE" = tonic_navy)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
    scale_y_continuous(
      name = "New sites opening",
      expand = expansion(mult = c(0, 0.20)),
      breaks = function(limits) seq(0, ceiling(limits[2]), by = max(1, ceiling(max_new / 5))),
      sec.axis = sec_axis(~ . * scale_factor, name = "Cumulative sites open",
                          breaks = function(limits) seq(0, ceiling(limits[2]), by = max(1, ceiling(max_cum / 5))))
    ) +
    labs(title = "Open centres over time",
         subtitle = "Bars: new sites opening each month \u00b7 Dashed line: cumulative",
         x = NULL) +
    theme_tonic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(filepath, p, width = width, height = height, dpi = dpi, bg = "white")
  invisible(filepath)
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
                         width = 7, height = 3.5, dpi = 150) {

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
    colour = c("#85B7EB", "#1B4F6B", "#2EC4A5", "#FAC775",
               "#97C459", "#F0997B", "#ED93B1"),
    stringsAsFactors = FALSE
  )
  # Enforce row order top-to-bottom
  phases$phase <- factor(phases$phase, levels = rev(phases$phase))

  today <- Sys.Date()
  timeline_start <- trial_start
  timeline_end   <- add_months(trial_start, 57)

  p <- ggplot(phases) +
    geom_rect(aes(xmin = start, xmax = end,
                  ymin = as.numeric(phase) - 0.35,
                  ymax = as.numeric(phase) + 0.35,
                  fill = phase),
              colour = NA, alpha = 0.85) +
    geom_vline(xintercept = as.numeric(today),
               colour = "#C0392B", linewidth = 0.7, linetype = "dashed") +
    annotate("text", x = today, y = length(levels(phases$phase)) + 0.6,
             label = "Now", colour = "#C0392B", fontface = "bold",
             size = 3, hjust = 0.5) +
    scale_fill_manual(values = setNames(phases$colour, levels(phases$phase))) +
    scale_y_continuous(breaks = seq_len(nlevels(phases$phase)),
                       labels = levels(phases$phase),
                       expand = expansion(mult = c(0.08, 0.15))) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %y",
                 limits = c(timeline_start, timeline_end),
                 expand = expansion(mult = c(0.01, 0.02))) +
    labs(title = "Project timeline",
         subtitle = "Protocol phases with current progress marker",
         x = NULL, y = NULL) +
    theme_tonic() +
    theme(legend.position = "none",
          axis.text.y = element_text(face = "bold", colour = "#2C3E50"),
          axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = tonic_grid, linewidth = 0.3))

  ggsave(filepath, p, width = width, height = height, dpi = dpi, bg = "white")
  invisible(filepath)
}
