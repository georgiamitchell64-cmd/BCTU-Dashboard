# ── return_rates_data.R ──────────────────────────────────────────────────────
#
# Return rates come from one of two places:
#
#   1. A return-rate CSV from the REDCap export pipeline — the unit's own
#      figures. The newest file is used, found in this order:
#        a. the folder pasted in Trial Settings (cfg$return_rates_dir)
#        b. the trial's data folder: trials/<code>/data/*return rate*.csv
#        c. the legacy K: drive folder
#      Earlier files in the same folder give the trend over time.
#
#   2. The REDCap export itself. The trial-health engine (trial_health.R)
#      already knows every participant x scheduled form, so the same counts
#      are worked out from it — for any trial, with or without a CSV, and down
#      to the participants who still have a form to return.
#
# Both arrive in one shape, with ".Overall" rows for the whole trial:
#   site, timepoint, form, kind, expected, due, entered, overdue
# (kind — "CRF" or "PROM" — and overdue are only known from REDCap).
#
# CSV columns expected (matched by name, in any order):
#   Site, Event, Form, Expected, Due, Entered, "% Due Entered", "% Expected Entered"
# The two % columns are ignored: rates are recomputed as min(entered, due) / due
# so forms entered before their window opens never push a rate past 100%.
#
# ─────────────────────────────────────────────────────────────────────────────

RR_DIR <- "K:/BCTU/BCTU/Teams/Coloproctology/CURRENT TRIALS/TONIC/TONIC Meeting Organiser/TONIC TMG Report/TONIC_app/return rates"

# Matches "return_rate", "return rate", "return-rate" and "returnrate" so the
# folder picks up whatever the export was named.
RR_PATTERN <- "return[ _-]?rate.*\\.csv$"

# Rate bands: on target, caution, and below that the forms need chasing
RR_GOOD <- 90
RR_WARN <- 70

rr_pct  <- function(entered, due) ifelse(due > 0, 100 * pmin(entered, due) / due, NA_real_)
rr_band <- function(p) ifelse(is.na(p), "none", ifelse(p >= RR_GOOD, "good", ifelse(p >= RR_WARN, "warn", "bad")))

# ── Finding the files ───────────────────────────────────────────────────────
# When each file was exported: the YYYYMMDD-HHMMSS stamp in its name, or the
# file's modified time when there isn't one.
.rr_stamp <- function(files) {
  stamps <- sub(".*_(\\d{8}-\\d{6}).*", "\\1", basename(files))
  at <- suppressWarnings(as.POSIXct(stamps, format = "%Y%m%d-%H%M%S"))
  miss <- is.na(at)
  if (any(miss)) at[miss] <- file.mtime(files[miss])
  at
}

# Every return-rate file in a folder, oldest first
.rr_files <- function(dir, pattern = RR_PATTERN) {
  if (is.null(dir) || !nzchar(dir) || !dir.exists(dir)) return(character())
  files <- list.files(dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) return(character())
  files[order(.rr_stamp(files))]
}

.find_newest_rr_file <- function(dir, pattern = RR_PATTERN) {
  files <- .rr_files(dir, pattern)
  if (length(files)) files[length(files)] else NULL
}

# `dir` is the folder pasted in Trial Settings (cfg$return_rates_dir). When it
# is supplied it takes precedence, so the dashboard reads from where the user
# pointed it. We fall back to the local trial folder, then the legacy K: drive.
latest_return_rate_file <- function(dir = NULL, trial_code = NULL) {
  # 1. Configured folder from Trial Settings (highest priority)
  if (!is.null(dir) && nzchar(dir)) {
    found <- .find_newest_rr_file(dir)
    if (!is.null(found)) return(found)
  }

  # 2. Trial-specific local data folder
  if (!is.null(trial_code) && nzchar(trial_code)) {
    found <- .find_newest_rr_file(file.path("trials", trial_code, "data"))
    if (!is.null(found)) return(found)
  }

  # 3. Fall back to legacy K: drive
  .find_newest_rr_file(RR_DIR)
}

.rr_read <- function(path) {
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
           na.strings = c("NA", "", "N/A"))
}

# ── One shape for every source ──────────────────────────────────────────────
.RR_COLS <- list(
  site      = c("site", "sitename", "dag", "centre"),
  timepoint = c("event", "timepoint", "visit", "eventname"),
  form      = c("form", "instrument", "crf", "formname"),
  expected  = "expected",
  due       = "due",
  entered   = c("entered", "returned", "received", "completed"))

# A pipeline CSV in the standard shape. Columns are matched by name; a file
# whose headers aren't recognised is read in the pipeline's column order.
rr_standardise <- function(df) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(NULL)
  key  <- gsub("[^a-z]", "", tolower(names(df)))
  pick <- vapply(.RR_COLS, function(a) {
    i <- match(a, key); i <- i[!is.na(i)]
    if (length(i)) i[1] else NA_integer_
  }, integer(1))
  if (anyNA(pick)) {
    if (ncol(df) < 6) return(NULL)
    pick[] <- 1:6
  }
  chr <- function(k) trimws(as.character(df[[pick[[k]]]]))
  num <- function(k) { x <- suppressWarnings(as.numeric(df[[pick[[k]]]])); x[is.na(x)] <- 0; x }
  out <- data.frame(site = chr("site"), timepoint = chr("timepoint"), form = chr("form"),
                    kind = NA_character_, expected = num("expected"), due = num("due"),
                    entered = num("entered"), overdue = NA_real_, stringsAsFactors = FALSE)
  ok <- !is.na(out$site) & nzchar(out$site) & !is.na(out$form) & nzchar(out$form) &
        !is.na(out$timepoint) & nzchar(out$timepoint)
  out <- out[ok, , drop = FALSE]
  if (!nrow(out)) return(NULL)
  rr_add_overall(out)
}

# Totals by any grouping. Entered is capped at due on every row first, so one
# site's early entries can't make up for another site's missing forms.
rr_summarise <- function(d, by = character()) {
  if (is.null(d) || !nrow(d)) return(NULL)
  d$entered <- pmin(d$entered, d$due)
  key <- if (length(by)) do.call(paste, c(unname(as.list(d[by])), sep = "\r")) else rep("all", nrow(d))
  f   <- factor(key, levels = unique(key))
  out <- d[!duplicated(key), by, drop = FALSE]
  for (v in c("expected", "due", "entered")) out[[v]] <- as.numeric(tapply(d[[v]], f, sum))
  out$overdue <- if (all(is.na(d$overdue))) NA_real_
                 else as.numeric(tapply(d$overdue, f, sum, na.rm = TRUE))
  out$missing <- out$due - out$entered
  out$pct     <- rr_pct(out$entered, out$due)
  rownames(out) <- NULL
  out
}

# ".Overall" rows (the whole trial) for a file that only has the sites
rr_add_overall <- function(d) {
  if (any(d$site == ".Overall")) return(d)
  o <- rr_summarise(d, c("timepoint", "form", "kind"))
  o$site <- ".Overall"
  rbind(o[names(d)], d)
}

# Timepoints in visit order: screening, baseline, discharge, then by the
# number of days (Day 30, Week 6, Month 3 ...); anything else keeps the order
# it first appears in.
rr_tp_order <- function(tp) {
  u <- unique(as.character(tp[!is.na(tp)]))
  if (!length(u)) return(character())
  l <- tolower(u)
  m <- regexpr("[0-9]+(\\.[0-9]+)?", l)
  num <- rep(NA_real_, length(l))
  num[m > 0] <- as.numeric(regmatches(l, m))
  mult <- ifelse(grepl("week|wk", l), 7, ifelse(grepl("month|mth", l), 30.44,
          ifelse(grepl("year|yr", l), 365.25, 1)))
  rank <- num * mult
  rank[grepl("screen", l)] <- -3
  rank[grepl("baseline|random|enrol|consent", l)] <- -2
  rank[is.na(rank) & grepl("discharge|post.?op|surgery|operation|hospital", l)] <- -1
  rank[is.na(rank)] <- 1e6 + which(is.na(rank))
  u[order(rank)]
}

# ── From the REDCap export ──────────────────────────────────────────────────
# The trial-health CRF status (th_crf_status) has one row per participant x
# scheduled form. A form is due once its visit date has passed, like the
# pipeline's "Due"; participants who left before a visit aren't expected to
# return its forms.
rr_from_health <- function(H) {
  crf <- if (is.null(H)) NULL else H$crf
  if (is.null(crf) || !is.data.frame(crf) || !nrow(crf)) return(NULL)
  crf <- crf[crf$status != "not_required", , drop = FALSE]
  if (!nrow(crf)) return(NULL)
  today_s <- .th_today_s(H$today %||% Sys.Date())
  open <- !is.na(crf$event_at) & crf$event_at <= today_s
  d <- data.frame(site = crf$site, timepoint = crf$timepoint, form = crf$form,
                  kind = crf$kind, expected = 1, due = as.numeric(open),
                  entered = as.numeric(open & crf$status == "complete"),
                  overdue = as.numeric(crf$status == "overdue"), stringsAsFactors = FALSE)
  cols <- c("site", "timepoint", "form", "kind", "expected", "due", "entered", "overdue")
  s <- rr_summarise(d, c("site", "timepoint", "form", "kind"))
  o <- rr_summarise(d, c("timepoint", "form", "kind"))
  o$site <- ".Overall"
  rbind(o[cols], s[cols])
}

# Participants with a form due and not yet returned, longest overdue first.
# "due" = the visit has passed but the form is inside its grace period.
rr_outstanding <- function(H) {
  crf <- if (is.null(H)) NULL else H$crf
  if (is.null(crf) || !is.data.frame(crf) || !nrow(crf)) return(NULL)
  o <- crf[crf$status %in% c("due", "overdue"), , drop = FALSE]
  out <- data.frame(
    status = o$status, id = o$id, site = o$site, timepoint = o$timepoint, form = o$form,
    kind_code = o$kind, kind = ifelse(o$kind == "CRF", "Site CRF", "Questionnaire"),
    due_on = .th_as_date(o$event_at), estimated = o$estimated %in% TRUE,
    days_overdue = ifelse(is.na(o$days_overdue), 0, o$days_overdue),
    stringsAsFactors = FALSE)
  out <- out[order(-out$days_overdue, out$site, out$id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# ── Trend: the rates in every earlier file in the same folder ──────────────
# One row per file x timepoint (due, entered, pct). Cached on the files'
# names and modified times, so the 5-minute refresh doesn't re-read them.
.rr_hist_cache <- new.env(parent = emptyenv())

rr_history <- function(path, max_files = 60) {
  files <- utils::tail(.rr_files(dirname(path)), max_files)
  if (length(files) < 2) return(NULL)
  key <- paste(files, as.numeric(file.mtime(files)), collapse = "|")
  if (identical(.rr_hist_cache$key, key)) return(.rr_hist_cache$value)
  at <- .rr_stamp(files)
  rows <- lapply(seq_along(files), function(i) {
    d <- tryCatch(rr_standardise(.rr_read(files[i])), error = function(e) NULL)
    if (is.null(d)) return(NULL)
    t <- rr_summarise(d[d$site == ".Overall", , drop = FALSE], "timepoint")
    data.frame(at = at[i], file = basename(files[i]), timepoint = t$timepoint,
               due = t$due, entered = t$entered, pct = t$pct, stringsAsFactors = FALSE)
  })
  h <- do.call(rbind, rows)
  if (is.null(h) || length(unique(h$at)) < 2) h <- NULL
  .rr_hist_cache$key <- key
  .rr_hist_cache$value <- h
  h
}

# ── Load the newest file ────────────────────────────────────────────────────
load_return_rates <- function(dir = NULL, trial_code = NULL) {

  path <- latest_return_rate_file(dir, trial_code)
  if (is.null(path)) return(NULL)

  df <- tryCatch(rr_standardise(.rr_read(path)), error = function(e) {
    warning("Failed to read return rate CSV: ", conditionMessage(e))
    NULL
  })
  if (is.null(df)) return(NULL)

  # Where it came from and when — shown in the tab's header
  attr(df, "source_file") <- basename(path)
  attr(df, "loaded_at")   <- Sys.time()
  attr(df, "file_mtime")  <- file.mtime(path)
  attr(df, "exported_at") <- .rr_stamp(path)
  attr(df, "history")     <- tryCatch(rr_history(path), error = function(e) NULL)
  df
}
