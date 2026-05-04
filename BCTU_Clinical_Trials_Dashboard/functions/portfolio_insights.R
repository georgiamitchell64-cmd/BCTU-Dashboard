# =============================================================================
# Portfolio-level Smart Insights (Stage 8 cont.)
# =============================================================================
# Builds on per-trial smart_insights.R. Aggregates across trials to produce:
#   - per-trial insight summary (top-severity insight for each trial)
#   - category-level insight summary
#   - portfolio-level "suggested questions" with structured answers
#
# Pure functions, no API calls. Input = list of cfgs from discover_trials();
# raw CSVs are read lazily per trial.
# =============================================================================

# ── Lightly-cached raw CSV per trial (one-shot per call) ────────────────────
.read_trial_raw <- function(cfg) {
  data_dir <- cfg$data_dir %||% file.path(getwd(), "trials", cfg$code, "data")
  csv <- tryCatch(find_latest_csv(data_dir), error = function(e) NULL)
  if (is.null(csv)) return(NULL)
  tryCatch(read_redcap_file(csv), error = function(e) NULL)
}

# ── Per-trial: top insight + count by severity ──────────────────────────────
trial_insight_summary <- function(cfg, raw = NULL, sites_df = NULL) {
  if (is.null(raw))      raw      <- .read_trial_raw(cfg)
  if (is.null(sites_df)) sites_df <- .read_trial_sites(cfg)

  insights <- tryCatch(compute_insights(raw, sites_df, cfg),
                       error = function(e) list())

  # Severity rank: alert > warning > info
  rank <- function(s) match(s, c("alert", "warning", "info"))
  if (length(insights) > 0) {
    sevs <- vapply(insights, function(i) rank(i$severity), integer(1))
    top  <- insights[[order(sevs)[1]]]
  } else {
    top <- NULL
  }

  list(
    code      = cfg$code,
    short     = cfg$short_name %||% toupper(cfg$code),
    category  = trial_category(cfg),
    insights  = insights,
    top       = top,
    n_alert   = sum(vapply(insights, function(i)
                  identical(i$severity, "alert"),   logical(1))),
    n_warning = sum(vapply(insights, function(i)
                  identical(i$severity, "warning"), logical(1))),
    n_info    = sum(vapply(insights, function(i)
                  identical(i$severity, "info"),    logical(1)))
  )
}

.read_trial_sites <- function(cfg) {
  db <- cfg$db_path %||% file.path(cfg$trial_dir, "data",
                                   paste0(cfg$code, ".sqlite"))
  if (!file.exists(db)) return(NULL)
  con <- tryCatch(dbConnect(RSQLite::SQLite(), db), error = function(e) NULL)
  if (is.null(con)) return(NULL)
  on.exit(dbDisconnect(con))
  tryCatch(
    dbGetQuery(con, "SELECT site_id, site_name, status, monthly_target,
                            target, randomised FROM sites"),
    error = function(e) NULL)
}

# ── Portfolio summary across all trials ─────────────────────────────────────
compute_portfolio_summary <- function(trials = NULL) {
  if (is.null(trials)) trials <- discover_trials()
  if (!length(trials)) return(list(per_trial = list(), suggestions = list()))

  per_trial <- lapply(trials, trial_insight_summary)

  # ── Suggestion 1: How many trials are below pace? ─────────────────────────
  below <- Filter(function(s) {
    !is.null(s$top) && s$top$severity %in% c("warning", "alert") &&
      grepl("pace|below|stalled|Stalled|paused", s$top$title, ignore.case = TRUE)
  }, per_trial)

  on_track <- Filter(function(s) {
    !is.null(s$top) && s$top$severity == "info" &&
      grepl("on track|active|complete", s$top$title, ignore.case = TRUE)
  }, per_trial)

  # ── Suggestion 2: Stalled trials ─────────────────────────────────────────
  stalled <- Filter(function(s) {
    any(vapply(s$insights, function(i)
      grepl("stalled|paused|no randomisations yet", i$title, ignore.case = TRUE),
      logical(1)))
  }, per_trial)

  # ── Suggestion 3: Lagging sites across portfolio ─────────────────────────
  lagging_total <- 0L
  for (s in per_trial) {
    lag_ins <- Filter(function(i)
      grepl("open but not recruiting", i$title), s$insights)
    if (length(lag_ins)) {
      n_str <- lag_ins[[1]]$value
      lagging_total <- lagging_total + suppressWarnings(as.integer(n_str))
    }
  }

  list(
    per_trial   = per_trial,
    n_total     = length(per_trial),
    n_below     = length(below),
    n_on_track  = length(on_track),
    n_stalled   = length(stalled),
    n_lagging   = lagging_total,
    stalled_codes = vapply(stalled, function(s) s$code, character(1)),
    below_codes   = vapply(below, function(s) s$code, character(1))
  )
}

# ── Category-level summary ──────────────────────────────────────────────────
compute_category_summary <- function(trials, category) {
  in_cat <- Filter(function(cfg) identical(trial_category(cfg), category), trials)
  if (!length(in_cat)) return(NULL)

  per_trial <- lapply(in_cat, trial_insight_summary)

  # Lift from sites: total active sites (no recruitment summing)
  n_sites_active <- 0L
  for (cfg in in_cat) {
    s <- .read_trial_sites(cfg)
    if (!is.null(s)) {
      n_sites_active <- n_sites_active +
        sum(s$status %in% c("Open", "Recruiting"), na.rm = TRUE)
    }
  }

  n_alerts <- sum(vapply(per_trial, function(t) t$n_alert, integer(1)))
  n_warnings <- sum(vapply(per_trial, function(t) t$n_warning, integer(1)))

  # Natural-language summary
  health_word <- if (n_alerts > 0) "needs attention"
                 else if (n_warnings > 0) "showing some warnings"
                 else "looking healthy"

  summary_text <- sprintf(
    "The %s portfolio has %d %s and is %s. %d %s currently open or recruiting.%s",
    category, length(in_cat),
    if (length(in_cat) == 1) "trial" else "trials",
    health_word,
    n_sites_active,
    if (n_sites_active == 1) "site is" else "sites are",
    if (n_alerts > 0)
      sprintf(" %d alert%s flagged across %s.",
              n_alerts, if (n_alerts == 1) "" else "s",
              if (length(in_cat) == 1) "the trial" else "trials")
    else ""
  )

  list(
    category   = category,
    per_trial  = per_trial,
    n_trials   = length(in_cat),
    n_sites    = n_sites_active,
    n_alerts   = n_alerts,
    n_warnings = n_warnings,
    summary    = summary_text
  )
}
