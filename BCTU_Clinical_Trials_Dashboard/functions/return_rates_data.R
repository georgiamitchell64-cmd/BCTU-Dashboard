# ── return_rates_data.R ──────────────────────────────────────────────────────
#
# Locates and loads the most recent TONIC return-rate CSV from the
# shared K: drive folder. Files are named TONIC_return_rate_YYYYMMDD-HHMMSS.csv
# and accumulate over time — this picks the newest by timestamp in the
# filename (falling back to file mtime if the filename doesn't parse).
#
# Usage:
#   rr_data <- reactive({
#     invalidateLater(5 * 60 * 1000)   # re-check every 5 min (optional)
#     load_return_rates()
#   })
#
# ─────────────────────────────────────────────────────────────────────────────

RR_DIR <- "K:/BCTU/BCTU/Teams/Coloproctology/CURRENT TRIALS/TONIC/TONIC Meeting Organiser/TONIC TMG Report/TONIC_app/return rates"

# ── Find the newest CSV ──────────────────────────────────────────────────────
latest_return_rate_file <- function(dir = RR_DIR) {

  if (!dir.exists(dir)) {
    warning("Return rates folder not found: ", dir)
    return(NULL)
  }

  files <- list.files(
    dir,
    pattern    = "^TONIC_return_rate.*\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(files) == 0) {
    warning("No TONIC_return_rate_*.csv files found in: ", dir)
    return(NULL)
  }

  # Prefer timestamp from filename (YYYYMMDD-HHMMSS)
  stamps <- sub(".*TONIC_return_rate_(\\d{8}-\\d{6}).*", "\\1", basename(files))
  parsed <- suppressWarnings(as.POSIXct(stamps, format = "%Y%m%d-%H%M%S"))

  if (all(is.na(parsed))) {
    # Fallback: file modification time
    parsed <- file.mtime(files)
  } else {
    # Fill any failed parses with mtime so nothing is dropped
    parsed[is.na(parsed)] <- file.mtime(files[is.na(parsed)])
  }

  files[which.max(parsed)]
}

# ── Load it ──────────────────────────────────────────────────────────────────
load_return_rates <- function(dir = RR_DIR) {

  path <- latest_return_rate_file(dir)
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
