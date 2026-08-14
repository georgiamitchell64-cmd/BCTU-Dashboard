# =============================================================================
# App paths — separates read-only program files from writable user data
# =============================================================================
# When the dashboard runs as a desktop (Electron) app, the program files live
# inside the application bundle, which is READ-ONLY and is replaced wholesale
# every time the app updates. Anything written there is either rejected by the
# OS or silently destroyed on the next update.
#
# So there are two distinct roots:
#
#   app_install_root()  Read-only. modules/, functions/, globals/, www/.
#                       Always the working directory.
#
#   app_data_root()     Writable, and survives app updates. Databases, trial
#                       configs, overrides, generated reports.
#                       Electron sets BCTU_DATA_ROOT to its userData path.
#                       In plain `shiny::runApp()` development both roots are
#                       the working directory, so nothing changes locally.
#
# Every writable path in the app must be derived from app_data_root(). If you
# add a new file the app writes to, route it through app_data_dir().
# =============================================================================

#' Root of the read-only application bundle.
app_install_root <- function() getwd()

#' Root for everything the app writes. Override with BCTU_DATA_ROOT.
app_data_root <- function() {
  root <- Sys.getenv("BCTU_DATA_ROOT", "")
  if (!nzchar(root)) root <- getwd()
  if (!dir.exists(root)) {
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
  }
  root
}

#' Build a path inside the writable data root, creating parent dirs on demand.
#' @param ... path components, as for file.path()
#' @param create_dir create the containing directory if missing (default TRUE)
app_data_dir <- function(..., create_dir = TRUE) {
  p <- file.path(app_data_root(), ...)
  if (isTRUE(create_dir)) {
    d <- if (grepl("\\.[A-Za-z0-9]{1,8}$", p)) dirname(p) else p
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  p
}

#' Writable trials directory. Trial configs and per-trial data live here.
#' Seeded from the bundle on first run by seed_user_data().
app_trials_dir <- function() app_data_dir("trials")

#' Copy the trial scaffold out of the read-only bundle into the writable root
#' on first launch. Only ever ADDS missing files, so a user's edited config or
#' database is never overwritten by an app update.
#'
#' Deliberately copies configuration and assets only (config.R, trial.json,
#' logos) and NOT *.sqlite — a fresh install starts with an empty database and
#' data is re-imported from the REDCap CSV export.
seed_user_data <- function(verbose = TRUE) {
  src <- file.path(app_install_root(), "trials")
  dst <- app_trials_dir()
  if (!dir.exists(src)) return(invisible(FALSE))
  if (identical(normalizePath(src, mustWork = FALSE),
                normalizePath(dst, mustWork = FALSE))) {
    return(invisible(FALSE))   # dev mode: same directory, nothing to seed
  }

  files <- list.files(src, recursive = TRUE, all.files = FALSE, full.names = FALSE)
  # Never seed databases — a fresh install starts clean.
  files <- files[!grepl("\\.sqlite$|\\.sqlite3$|\\.db$", files, ignore.case = TRUE)]

  copied <- 0L
  for (f in files) {
    to <- file.path(dst, f)
    if (file.exists(to)) next
    if (!dir.exists(dirname(to))) {
      dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
    }
    if (file.copy(file.path(src, f), to, overwrite = FALSE)) copied <- copied + 1L
  }
  if (verbose && copied > 0L) {
    message("Seeded ", copied, " trial config file(s) into ", dst)
  }
  invisible(copied > 0L)
}
