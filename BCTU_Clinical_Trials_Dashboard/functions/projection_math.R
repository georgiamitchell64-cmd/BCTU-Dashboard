# ── projection_math.R ─────────────────────────────────────────────────────────
#
# Helpers for the recruitment projection chart on the Overview tab.
#
# Public functions:
#   .build_projection_series(rand_dates, n_open_now, settings, trial_target)
#     → data.frame with columns:
#        month_date, month_label, plan, actual, central, pessimistic, optimistic
#
#   .projection_smart_defaults(raw_redcap, sites)
#     → list with rate_pessimistic/central/optimistic derived from actuals
#       when ≥3 months of randomisation data exist; NULL otherwise.
#
# Model:
#   Starting from "now" (first of this month), we project forward
#   month-by-month. Each month:
#     · new_sites = min(ramp_rate, target_sites - current_sites)
#     · current_sites += new_sites
#     · participants_this_month = current_sites × per_site_rate
#     · cumulative += participants_this_month
#   Stops early if cumulative hits trial_target.
# ─────────────────────────────────────────────────────────────────────────────

# ── Protocol plan schedule (mirrors prepare_report_data.R) ──────────────────
.projection_protocol_schedule <- function() {
  data.frame(
    month_date = as.Date(c(
      "2026-03-01","2026-04-01","2026-05-01","2026-06-01",
      "2026-07-01","2026-08-01","2026-09-01","2026-10-01","2026-11-01","2026-12-01",
      "2027-01-01","2027-02-01","2027-03-01","2027-04-01","2027-05-01","2027-06-01",
      "2027-07-01","2027-08-01","2027-09-01","2027-10-01","2027-11-01","2027-12-01",
      "2028-01-01","2028-02-01","2028-03-01","2028-04-01","2028-05-01","2028-06-01"
    )),
    cumulative_target = c(
      0,4,12,24,36,48,60,76,96,120,148,180,216,256,300,348,398,
      448,498,548,598,648,698,748,798,848,873,898
    ),
    stringsAsFactors = FALSE
  )
}

# ── Simulate forward from a given cumulative count ──────────────────────────
.simulate_ramp <- function(months_ahead, start_cum, start_sites, target_sites,
                           new_sites_per_month, per_site_rate, hard_cap = NULL) {
  cumulative <- numeric(months_ahead)
  cum   <- start_cum
  sites <- start_sites

  for (i in seq_len(months_ahead)) {
    new_sites <- min(new_sites_per_month, max(0, target_sites - sites))
    sites     <- sites + new_sites
    cum       <- cum + sites * per_site_rate
    if (!is.null(hard_cap) && cum > hard_cap) cum <- hard_cap
    cumulative[i] <- cum
  }
  cumulative
}

# ── Build the full series for the chart ─────────────────────────────────────
.build_projection_series <- function(rand_dates, n_open_now, settings,
                                     trial_target) {

  sched <- .projection_protocol_schedule()

  # Monthly actuals: count randomisations per month from the start
  this_month <- as.Date(format(Sys.Date(), "%Y-%m-01"))

  # Per-month cumulative actual over the full schedule range
  if (length(rand_dates) > 0) {
    rand_months <- as.Date(format(rand_dates, "%Y-%m-01"))
    # Tabulate per month across the schedule range
    monthly_n <- vapply(sched$month_date, function(m) sum(rand_months == m), integer(1))
    cum_actual <- cumsum(monthly_n)
    # Only show actual up to and including the current month
    cum_actual[sched$month_date > this_month] <- NA_real_
  } else {
    cum_actual <- rep(NA_real_, nrow(sched))
    # Show 0 for months ≤ today if there are no randomisations yet
    cum_actual[sched$month_date <= this_month] <- 0
  }

  # Anchor the projection at the last actual value (or 0 if none)
  last_actual_idx <- max(which(!is.na(cum_actual) & sched$month_date <= this_month), 0)
  if (last_actual_idx == 0) {
    # No data yet — project from month 0
    start_cum <- 0
    start_idx <- 1  # start projecting from the first month
  } else {
    start_cum <- cum_actual[last_actual_idx]
    start_idx <- last_actual_idx + 1  # project starting NEXT month
  }

  # Number of sites to assume we already have open: use the live count
  start_sites <- as.integer(n_open_now %||% 0)

  # Remaining months to fill
  months_ahead <- nrow(sched) - start_idx + 1
  if (months_ahead <= 0) {
    # Beyond schedule — return what we have
    return(data.frame(
      month_date  = sched$month_date,
      month_label = format(sched$month_date, "%b %y"),
      plan        = sched$cumulative_target,
      actual      = cum_actual,
      central     = rep(NA_real_, nrow(sched)),
      pessimistic = rep(NA_real_, nrow(sched)),
      optimistic  = rep(NA_real_, nrow(sched)),
      stringsAsFactors = FALSE
    ))
  }

  # Simulate each scenario, capping at trial_target (898) since the trial
  # cannot recruit beyond its ceiling
  central_tail <- .simulate_ramp(months_ahead, start_cum, start_sites,
                                  settings$target_sites,
                                  settings$sites_central, settings$rate_central,
                                  hard_cap = trial_target)
  pess_tail    <- .simulate_ramp(months_ahead, start_cum, start_sites,
                                  settings$target_sites,
                                  settings$sites_pessimistic, settings$rate_pessimistic,
                                  hard_cap = trial_target)
  opt_tail     <- .simulate_ramp(months_ahead, start_cum, start_sites,
                                  settings$target_sites,
                                  settings$sites_optimistic, settings$rate_optimistic,
                                  hard_cap = trial_target)

  # Assemble full-length vectors. Values before start_idx are NA for the
  # projection series so the lines begin at the anchor point.
  make_series <- function(tail_vec) {
    out <- rep(NA_real_, nrow(sched))
    if (start_idx > 1) out[start_idx - 1] <- start_cum  # connect projection to last actual
    out[start_idx:nrow(sched)] <- tail_vec
    out
  }

  data.frame(
    month_date  = sched$month_date,
    month_label = format(sched$month_date, "%b %y"),
    plan        = sched$cumulative_target,
    actual      = cum_actual,
    central     = make_series(central_tail),
    pessimistic = make_series(pess_tail),
    optimistic  = make_series(opt_tail),
    stringsAsFactors = FALSE
  )
}

# ── Smart defaults: derive per-site rates from actuals when ≥3 months ──────
.projection_smart_defaults <- function(raw_redcap, sites) {
  rand_col <- fld("randomisation_datetime", default = "rand_dttm_s")
  if (is.null(raw_redcap) || !(rand_col %in% names(raw_redcap))) return(NULL)
  rd <- suppressWarnings(as.Date(raw_redcap[[rand_col]]))
  rd <- rd[!is.na(rd)]
  if (length(rd) == 0) return(NULL)

  first   <- min(rd)
  elapsed <- as.numeric(difftime(Sys.Date(), first, units = "days")) / 30.44
  if (elapsed < 3) return(NULL)          # not enough data yet

  n_open <- sum(sites$status %in% c("Open", "Recruiting"), na.rm = TRUE)
  if (n_open == 0) return(NULL)

  per_site <- length(rd) / (elapsed * n_open)
  list(
    rate_pessimistic = max(0.1, round(per_site * 0.80, 1)),
    rate_central     = round(per_site, 1),
    rate_optimistic  = round(per_site * 1.20, 1)
  )
}
