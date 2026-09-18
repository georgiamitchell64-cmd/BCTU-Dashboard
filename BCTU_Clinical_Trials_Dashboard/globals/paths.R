# =============================================================================
# Where the app reads, and where it writes
# =============================================================================
# The dashboard writes while it runs: SQLite databases under data/, a trial's
# overrides.json and its report templates under trials/<code>/, and a cache of
# the trial logos it serves. Run from a folder you can write to — a checkout
# opened in RStudio, or the desktop shortcut on a network drive — all of that
# stays exactly where it has always been, and nothing about an existing
# install changes.
#
# Installed, it can't: a packaged desktop build lives under Program Files or
# inside a signed .app, both read-only. The same files then live in a per-user
# folder instead, and the installed copy is read-only seed data.
#
# Resolution order:
#   1. BCTU_DATA_DIR    — set it to put the writable files wherever you like:
#                         a shared folder on K: for a whole team, or the
#                         per-user folder a packaged build passes in.
#   2. the app folder   — when it is writable. This is every current install,
#                         so nothing moves and nothing needs migrating.
#   3. tools::R_user_dir("BCTU-Dashboard", "data")  — %LOCALAPPDATA% on
#                         Windows, ~/Library/Application Support on a Mac.
#
# Whenever 1 or 3 is used, seed_data_root() copies the trials folder across on
# first run, so an installed copy still knows about the trials it shipped with.
# =============================================================================

.PATHS <- new.env(parent = emptyenv())

#' The folder the app's code lives in — fixed at startup, never written to
#' unless it happens to be the data root as well.
app_install_dir <- function() {
  if (is.null(.PATHS$install))
    .PATHS$install <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  .PATHS$install
}

#' Can we actually create a file here? dir.exists() and file.access() both say
#' yes for a folder Windows then refuses to write to (virtualised Program
#' Files, a read-only share), so this writes a file and deletes it again.
.dir_writable <- function(path) {
  if (is.null(path) || !nzchar(path) || !dir.exists(path)) return(FALSE)
  probe <- file.path(path, sprintf(".bctu-write-test-%s", Sys.getpid()))
  ok <- tryCatch({
    con <- file(probe, open = "w"); close(con); TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  unlink(probe, force = TRUE)
  isTRUE(ok)
}

#' The writable root: data/, trials/ and the logo cache all hang off this.
#' Worked out once per session — it does a real write test — and cached.
app_data_root <- function() {
  if (!is.null(.PATHS$root)) return(.PATHS$root)

  env <- trimws(Sys.getenv("BCTU_DATA_DIR", ""))
  root <- if (nzchar(env)) {
    if (!dir.exists(env))
      dir.create(env, recursive = TRUE, showWarnings = FALSE)
    normalizePath(env, winslash = "/", mustWork = FALSE)
  } else if (.dir_writable(app_install_dir())) {
    app_install_dir()
  } else {
    d <- tools::R_user_dir("BCTU-Dashboard", "data")
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    normalizePath(d, winslash = "/", mustWork = FALSE)
  }

  .PATHS$root <- root
  root
}

#' TRUE when the app writes into its own folder — the usual case, and the one
#' where nothing needs seeding.
app_data_root_is_install <- function()
  identical(app_data_root(), app_install_dir())

app_data_dir   <- function() file.path(app_data_root(), "data")
app_trials_dir <- function() file.path(app_data_root(), "trials")

#' Where trial logos are cached for serving. app.R maps this to the
#' "trial_logos/" URL with addResourcePath(), so the <img src> the UI writes
#' is the same wherever the folder actually sits.
trial_logo_dir <- function() file.path(app_data_root(), "www", "trial_logos")

#' Make sure the writable root has what the app needs. The trials folder is
#' copied from the installed copy the first time, so a packaged build still
#' knows about the trials it shipped with; data/ is only created, never
#' seeded, so the databases are built fresh by db_init() rather than carrying
#' whatever was in the installed copy.
seed_data_root <- function(quiet = FALSE) {
  root <- app_data_root()
  for (d in c(app_data_dir(), trial_logo_dir()))
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

  if (app_data_root_is_install()) return(invisible(root))

  # Seed trial by trial, not all-or-nothing. Checking only whether the trials
  # folder exists meant that once it did, a trial added in a later version
  # never appeared — the update installed, and the new trial was simply
  # missing with nothing said. Anything already there is left exactly as it
  # is: these folders hold the settings and report templates people have
  # edited, and an update must never write over those.
  dest <- app_trials_dir()
  src  <- file.path(app_install_dir(), "trials")
  if (dir.exists(src)) {
    if (!dir.exists(dest)) dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    shipped <- list.files(src, full.names = TRUE)
    fresh   <- shipped[!file.exists(file.path(dest, basename(shipped)))]
    if (length(fresh)) {
      copied <- file.copy(fresh, dest, recursive = TRUE, overwrite = FALSE)
      if (!quiet)
        message("Set up ", sum(copied), " trial folder(s) in ", dest)
    }
  }
  if (!quiet) message("Writing to ", root)
  invisible(root)
}
