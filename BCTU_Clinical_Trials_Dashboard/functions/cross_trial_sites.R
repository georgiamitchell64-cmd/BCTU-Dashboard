# =============================================================================
# Cross-trial site aggregation (Stage 7)
# =============================================================================
# Reads each trial's per-trial SQLite directly (bypassing the active DB_PATH
# global) and returns a long-format frame: one row per (site, trial).
# =============================================================================

cross_trial_sites <- function(trials = NULL) {
  if (is.null(trials)) trials <- discover_trials()
  if (length(trials) == 0) return(data.frame())

  rows <- list()
  for (code in names(trials)) {
    cfg <- trials[[code]]
    db   <- cfg$db_path %||% file.path(cfg$trial_dir, "data",
                                       paste0(code, ".sqlite"))
    if (!file.exists(db)) next

    con <- tryCatch(dbConnect(RSQLite::SQLite(), db), error = function(e) NULL)
    if (is.null(con)) next
    sites <- tryCatch(
      dbGetQuery(con,
        "SELECT site_id, site_name, status, monthly_target, target, randomised
         FROM sites"),
      error = function(e) NULL
    )
    dbDisconnect(con)
    if (is.null(sites) || nrow(sites) == 0) next

    sites$trial_code  <- code
    sites$trial_short <- cfg$short_name %||% toupper(code)
    sites$category    <- trial_category(cfg)
    rows[[code]] <- sites
  }

  if (length(rows) == 0) return(data.frame())
  out <- do.call(rbind, rows)
  out$randomised     <- as.integer(out$randomised %||% 0)
  out$target         <- as.integer(out$target %||% 0)
  out$monthly_target <- as.integer(out$monthly_target %||% 0)
  out
}

# Aggregate the long frame into one row per site_name.
# Returns: site_name, n_trials, total_randomised, total_target,
#          total_monthly_target, trial_codes (chr vec), categories (chr vec).
aggregate_sites <- function(long_df) {
  if (is.null(long_df) || !nrow(long_df)) return(data.frame())
  agg <- aggregate(
    cbind(randomised, target, monthly_target) ~ site_name,
    data = long_df, FUN = sum, na.rm = TRUE)
  meta <- aggregate(
    cbind(trial_codes  = trial_code,
          trial_shorts = trial_short,
          categories   = category) ~ site_name,
    data = long_df,
    FUN = function(x) paste(unique(x), collapse = ", "))
  n_t <- aggregate(trial_code ~ site_name, data = long_df,
                   FUN = function(x) length(unique(x)))
  names(n_t)[2] <- "n_trials"

  out <- merge(agg, meta, by = "site_name")
  out <- merge(out, n_t,  by = "site_name")
  names(out)[names(out) == "randomised"]      <- "total_randomised"
  names(out)[names(out) == "target"]          <- "total_target"
  names(out)[names(out) == "monthly_target"]  <- "total_monthly_target"
  out
}
