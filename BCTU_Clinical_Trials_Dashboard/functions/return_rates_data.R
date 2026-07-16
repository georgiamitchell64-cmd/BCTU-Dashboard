# ── return_rates_data.R ──────────────────────────────────────────────────────
#
# Locates and loads the most recent return-rate CSV.
#
# Search order:
#   1. Configured folder: the path pasted in Trial Settings → "Return rates
#      CSV folder" (cfg$return_rates_dir), passed in as `dir`.
#   2. Trial-specific:    trials/<code>/data/*return rate*.csv
#   3. Legacy K: drive:   TONIC_return_rate_YYYYMMDD-HHMMSS.csv
#
# CSV columns expected:
#   Site, Event, Form, Expected, Due, Entered, "% Due Entered", "% Expected Entered"
#
# ─────────────────────────────────────────────────────────────────────────────

RR_DIR <- "K:/BCTU/BCTU/Teams/Coloproctology/CURRENT TRIALS/TONIC/TONIC Meeting Organiser/TONIC TMG Report/TONIC_app/return rates"

# ── Find the newest CSV in a given directory ────────────────────────────────
# Matches "return_rate", "return rate", "return-rate" and "returnrate" so the
# folder picks up whatever the export was named.
.find_newest_rr_file <- function(dir, pattern = "return[ _-]?rate.*\\.csv$") {
  if (!dir.exists(dir)) return(NULL)

  files <- list.files(dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) return(NULL)

  # Prefer timestamp from filename (YYYYMMDD-HHMMSS)
  stamps <- sub(".*_(\\d{8}-\\d{6}).*", "\\1", basename(files))
  parsed <- suppressWarnings(as.POSIXct(stamps, format = "%Y%m%d-%H%M%S"))

  if (all(is.na(parsed))) {
    parsed <- file.mtime(files)
  } else {
    parsed[is.na(parsed)] <- file.mtime(files[is.na(parsed)])
  }

  files[which.max(parsed)]
}

# ── Find file (config- and trial-aware) ────────────────────────────────────
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
    trial_data_dir <- file.path("trials", trial_code, "data")
    found <- .find_newest_rr_file(trial_data_dir)
    if (!is.null(found)) return(found)
  }

  # 3. Fall back to legacy K: drive
  .find_newest_rr_file(RR_DIR)
}

# ── Load it ──────────────────────────────────────────────────────────────────
load_return_rates <- function(dir = NULL, trial_code = NULL) {

  path <- latest_return_rate_file(dir, trial_code)
  if (is.null(path)) return(NULL)

  df <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
             na.strings = c("NA", "", "N/A")),
    error = function(e) {
      warning("Failed to read return rate CSV: ", conditionMessage(e))
      NULL
    }
  )

  # Stash the source path + timestamp as attributes — useful for display
  if (!is.null(df)) {
    attr(df, "source_file") <- basename(path)
    attr(df, "loaded_at")   <- Sys.time()
    attr(df, "file_mtime")  <- file.mtime(path)
  }

  df
}
