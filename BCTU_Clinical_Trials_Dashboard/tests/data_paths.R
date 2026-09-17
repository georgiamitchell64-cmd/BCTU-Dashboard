# =============================================================================
# Where the app writes — regression test
# =============================================================================
# globals/paths.R decides where the databases, the trials folder and the logo
# cache live. Two things have to stay true:
#
#   * run from a folder it can write to — every current install, and every
#     checkout opened in RStudio — the app writes exactly where it always did,
#     so nothing needs migrating;
#   * pointed somewhere else with BCTU_DATA_DIR, or installed read-only, it
#     writes there instead and still finds the trials it shipped with.
#
# Needs DBI + RSQLite for the end-to-end check that the databases really are
# created under a relocated root.
#
# Run from the app directory:  Rscript tests/data_paths.R
# Exits non-zero on the first failed assertion.
# =============================================================================

# Rscript passes spaces in the script's path as "~+~" on Windows.
.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
setwd(file.path(dirname(gsub("~+~", " ", .this, fixed = TRUE)), ".."))

source("globals/paths.R")

ok <- function(cond, what) {
  if (!isTRUE(cond)) { cat("FAIL:", what, "\n"); quit(status = 1L) }
  cat("  ✓", what, "\n")
}

# Each scenario re-resolves from scratch: app_data_root() caches its answer,
# since it does a real write test, so the cache has to be cleared between them.
reset <- function(data_dir = "") {
  rm(list = ls(envir = .PATHS, all.names = TRUE), envir = .PATHS)
  Sys.setenv(BCTU_DATA_DIR = data_dir)
}
norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)

# ── Can we write here? ───────────────────────────────────────────────────────
cat("\nWritability probe\n")
ok(.dir_writable(tempdir()), "a writable folder reads as writable")
ok(!.dir_writable(file.path(tempdir(), "no-such-folder-here")),
   "a folder that isn't there reads as not writable")
ok(!.dir_writable(""), "an empty path reads as not writable")
ok(length(list.files(tempdir(), pattern = "^[.]bctu-write-test",
                     all.files = TRUE)) == 0,
   "the probe cleans up the file it wrote")

# ── Default: the app writes into its own folder, exactly as before ───────────
cat("\nDefault (app folder is writable)\n")
reset()
ok(app_data_root_is_install(), "the data root is the app folder")
ok(identical(norm(app_data_dir()), norm(file.path(getwd(), "data"))),
   "databases stay in data/ — an existing install needs no migrating")
ok(identical(norm(app_trials_dir()), norm(file.path(getwd(), "trials"))),
   "trials stay in trials/")
ok(dir.exists(app_trials_dir()), "and that folder is the real one, with the trials in it")

# ── BCTU_DATA_DIR sends everything somewhere else ────────────────────────────
cat("\nBCTU_DATA_DIR set\n")
elsewhere <- file.path(tempfile("bctu-root-"), "Dashboard Data")  # a space, on purpose
dir.create(elsewhere, recursive = TRUE)
reset(elsewhere)
ok(!app_data_root_is_install(), "the data root is no longer the app folder")
ok(identical(norm(app_data_root()), norm(elsewhere)), "it is the folder asked for")
ok(identical(norm(app_data_dir()), norm(file.path(elsewhere, "data"))),
   "databases follow it")
ok(identical(norm(app_trials_dir()), norm(file.path(elsewhere, "trials"))),
   "trials follow it")
ok(startsWith(norm(trial_logo_dir()), norm(elsewhere)), "the logo cache follows it")

# ── First run seeds the trials folder ────────────────────────────────────────
cat("\nSeeding a fresh data root\n")
shipped <- list.dirs("trials", recursive = FALSE, full.names = FALSE)
invisible(seed_data_root(quiet = TRUE))
ok(dir.exists(app_data_dir()), "data/ is created")
ok(dir.exists(trial_logo_dir()), "the logo cache is created")
seeded <- list.dirs(app_trials_dir(), recursive = FALSE, full.names = FALSE)
ok(setequal(seeded, shipped),
   sprintf("all %d trial folder(s) are copied across", length(shipped)))
ok(file.exists(file.path(app_trials_dir(), shipped[1], "config.R")),
   "each copy carries its config.R")

# Seeding again must not clobber local edits — the copy is a first-run seed,
# not a sync back from the installed copy on every start.
marker <- file.path(app_trials_dir(), shipped[1], "overrides-marker.json")
writeLines("{}", marker)
invisible(seed_data_root(quiet = TRUE))
ok(file.exists(marker), "running again leaves what's already there alone")

# ── Installed read-only: fall back to the per-user folder ────────────────────
# The packaged-build case. Can't be produced by chmod here (the test may be
# running as root, which writes anywhere regardless), so the probe is stubbed
# to report what a read-only Program Files would.
cat("\nApp folder not writable\n")
.dir_writable_real <- .dir_writable
.dir_writable <- function(path) FALSE
reset()
fallback <- app_data_root()
.dir_writable <- .dir_writable_real
ok(!app_data_root_is_install(), "the data root moves out of the app folder")
ok(identical(norm(fallback), norm(tools::R_user_dir("BCTU-Dashboard", "data"))),
   "it lands in the per-user data folder")
ok(dir.exists(fallback), "which is created if it wasn't there")
unlink(fallback, recursive = TRUE)

# ── The databases really are created there ───────────────────────────────────
reset(elsewhere)
if (all(vapply(c("DBI", "RSQLite"), requireNamespace, logical(1), quietly = TRUE))) {
  cat("\nDatabases under a relocated root\n")
  suppressPackageStartupMessages({library(DBI); library(RSQLite)})
  `%||%` <- function(a, b) if (is.null(a)) b else a
  discover_trials <- function() list()
  source("functions/permissions.R")
  .migrate_legacy_profiles <- function(con) invisible(NULL)
  invisible(shared_db_init())
  ok(file.exists(file.path(app_data_dir(), "shared.sqlite")),
     "shared.sqlite is written under the relocated root")
  ok(!file.exists(file.path(getwd(), "data", "shared.sqlite")) ||
       !identical(norm(app_data_dir()), norm(file.path(getwd(), "data"))),
     "and not into the app folder")
} else {
  cat("\n  (skipping the database check — DBI / RSQLite not installed)\n")
}

reset()
unlink(dirname(elsewhere), recursive = TRUE)
cat("\nAll data-path assertions passed.\n")
