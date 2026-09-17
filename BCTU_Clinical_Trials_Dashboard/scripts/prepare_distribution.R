# =============================================================================
# Stage a copy of the dashboard with no personal or participant data in it
# =============================================================================
# The working copy of this app accumulates real data: accounts and the
# activity log in data/shared.sqlite, participant postal tracking in
# data/postal_tracking.sqlite, per-trial databases, REDCap exports left in a
# trial's data folder. None of that belongs in something handed to someone
# else — a packaged installer, a zip for another team, a demo machine.
#
# This makes a clean copy. It never writes to the app folder it reads from.
#
#   Rscript scripts/prepare_distribution.R ../BCTU-Dashboard-dist
#   Rscript scripts/prepare_distribution.R <dir> --force   # reuse a non-empty dir
#
# What survives in a database is an allow-list, not a block-list: a table
# nobody has classified is emptied rather than shipped. Adding a table later
# therefore fails safe — it goes out empty until someone decides it is
# configuration and adds it to KEEP_TABLES.
#
# Accounts: the copy ships with none, and it doesn't need any. The first
# person to register on a fresh install becomes the admin
# (db_save_profile() in functions/permissions.R), and an admin implicitly
# manages every trial, so a stripped build bootstraps itself. Both halves of
# that — the stripping and the bootstrap — are covered by tests/distribution.R.
#
# Sourcing this file only defines the functions below; the staging run at the
# bottom happens when it is the script Rscript was pointed at.
# =============================================================================

# ── Tables worth shipping ────────────────────────────────────────────────────
# Trial configuration a fresh install is better off having. Everything else —
# profiles, trial_memberships, activity_events, activity_log, accounts,
# dismissed_notifications, home_prefs, postal_sent, every canon_* table of
# imported participant records, and the imports log — is emptied.
KEEP_TABLES <- c("sites", "projection_settings")

# Files that never belong in a distribution, whatever is in them.
DROP_PATTERNS <- c(
  "_mac[.]sqlite$",                # stray copies from another machine
  "^data/.*[.]csv$",               # exports dropped in the app's own data folder
  "^trials/[^/]+/data/.*[.]csv$"   # a trial's REDCap exports
)

#' Empty every table in every SQLite under `root` that isn't in `keep`.
#' Returns a data frame of what was cleared, one row per table.
strip_personal_data <- function(root, keep = KEEP_TABLES, quiet = FALSE) {
  stopifnot(dir.exists(root))
  root <- normalizePath(root, winslash = "/")
  rel  <- function(p) sub(paste0("^", root, "/"), "",
                          normalizePath(p, winslash = "/", mustWork = FALSE))
  out <- data.frame(file = character(), table = character(), rows = integer(),
                    stringsAsFactors = FALSE)

  for (db in list.files(root, pattern = "[.]sqlite$", recursive = TRUE, full.names = TRUE)) {
    con <- tryCatch(DBI::dbConnect(RSQLite::SQLite(), db), error = function(e) NULL)
    if (is.null(con)) { if (!quiet) cat("  ! could not open", rel(db), "\n"); next }
    cleared <- character(0)
    for (tb in setdiff(DBI::dbListTables(con), c(keep, "sqlite_sequence"))) {
      n <- tryCatch(DBI::dbGetQuery(con, sprintf('SELECT COUNT(*) AS n FROM "%s"', tb))$n,
                    error = function(e) 0L)
      if (n > 0) {
        ok <- tryCatch({ DBI::dbExecute(con, sprintf('DELETE FROM "%s"', tb)); TRUE },
                       error = function(e) FALSE)
        if (ok) {
          cleared <- c(cleared, sprintf("%s (%d)", tb, n))
          out <- rbind(out, data.frame(file = rel(db), table = tb, rows = as.integer(n),
                                       stringsAsFactors = FALSE))
        } else if (!quiet) cat("  ! could not clear", tb, "in", rel(db), "\n")
      }
    }
    # Reset AUTOINCREMENT counters so a fresh install starts from 1, and shrink
    # the file so deleted rows aren't still sitting in free pages on disk.
    tryCatch(DBI::dbExecute(con, "DELETE FROM sqlite_sequence"), error = function(e) NULL)
    tryCatch(DBI::dbExecute(con, "VACUUM"), error = function(e) NULL)
    DBI::dbDisconnect(con)
    if (!quiet)
      cat(sprintf("%-46s %s\n", rel(db),
                  if (length(cleared)) paste("cleared", paste(cleared, collapse = ", "))
                  else "nothing to clear"))
  }
  out
}

#' Delete the files under `root` that hold data rather than configuration.
#' Returns the paths removed, relative to root.
drop_data_files <- function(root, patterns = DROP_PATTERNS) {
  root <- normalizePath(root, winslash = "/")
  rel  <- function(p) sub(paste0("^", root, "/"), "",
                          normalizePath(p, winslash = "/", mustWork = FALSE))
  gone <- character(0)
  for (f in list.files(root, recursive = TRUE, full.names = TRUE, all.files = TRUE)) {
    r <- rel(f)
    if (any(vapply(patterns, function(p) grepl(p, r), logical(1)))) {
      unlink(f); gone <- c(gone, r)
    }
  }
  gone
}

#' What's left in each database, for a report nobody has to take on trust.
remaining_rows <- function(root) {
  root <- normalizePath(root, winslash = "/")
  rel  <- function(p) sub(paste0("^", root, "/"), "",
                          normalizePath(p, winslash = "/", mustWork = FALSE))
  out <- list()
  for (db in list.files(root, pattern = "[.]sqlite$", recursive = TRUE, full.names = TRUE)) {
    con <- tryCatch(DBI::dbConnect(RSQLite::SQLite(), db), error = function(e) NULL)
    if (is.null(con)) next
    kept <- character(0)
    for (tb in setdiff(DBI::dbListTables(con), "sqlite_sequence")) {
      n <- tryCatch(DBI::dbGetQuery(con, sprintf('SELECT COUNT(*) AS n FROM "%s"', tb))$n,
                    error = function(e) NA_integer_)
      if (!is.na(n) && n > 0) kept <- c(kept, sprintf("%s=%d", tb, n))
    }
    DBI::dbDisconnect(con)
    if (length(kept)) out[[rel(db)]] <- kept
  }
  out
}

# ── Staging run ──────────────────────────────────────────────────────────────
.invoked_directly <- function() {
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(f) > 0 &&
    identical(basename(sub("^--file=", "", f[1])), "prepare_distribution.R")
}

if (.invoked_directly()) {
  suppressPackageStartupMessages({library(DBI); library(RSQLite)})

  args  <- commandArgs(trailingOnly = TRUE)
  force <- "--force" %in% args
  out   <- args[!grepl("^--", args)][1]

  # Rscript passes spaces in the script's path as "~+~" on Windows.
  .this   <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
  app_dir <- normalizePath(file.path(dirname(gsub("~+~", " ", .this, fixed = TRUE)), ".."),
                           winslash = "/")

  if (is.na(out) || !nzchar(out)) {
    cat("Usage: Rscript scripts/prepare_distribution.R <output-dir> [--force]\n")
    quit(status = 1L)
  }

  # Refuse to damage the working copy
  out_abs <- suppressWarnings(normalizePath(out, winslash = "/", mustWork = FALSE))
  if (identical(out_abs, app_dir) || startsWith(paste0(out_abs, "/"), paste0(app_dir, "/"))) {
    cat("Refusing to stage into the app folder itself:\n  ", out_abs,
        "\nPick somewhere outside ", app_dir, "\n", sep = "")
    quit(status = 1L)
  }
  if (dir.exists(out_abs) &&
      length(list.files(out_abs, all.files = TRUE, no.. = TRUE)) && !force) {
    cat(out_abs, "already has something in it. Pass --force to reuse it.\n")
    quit(status = 1L)
  }

  cat("Staging a distribution copy\n  from ", app_dir, "\n  to   ", out_abs, "\n\n", sep = "")
  if (dir.exists(out_abs)) unlink(out_abs, recursive = TRUE)
  dir.create(out_abs, recursive = TRUE, showWarnings = FALSE)

  # "runtime" is the hundreds of MB of pandoc and Chrome that fetch_runtime.R
  # downloads. It is per-platform and re-fetched wherever the copy lands, so
  # copying it only makes the staged folder enormous.
  skip_top <- c(".git", ".Rproj.user", ".Rhistory", ".RData", "node_modules",
                "renv", "runtime")
  top <- setdiff(list.files(app_dir, all.files = TRUE, no.. = TRUE), skip_top)
  invisible(file.copy(file.path(app_dir, top), out_abs, recursive = TRUE))

  dropped <- drop_data_files(out_abs)
  if (length(dropped)) {
    cat("Removed", length(dropped), "file(s) that hold data rather than configuration:\n")
    for (d in dropped) cat("  -", d, "\n")
    cat("\n")
  }

  cleared <- strip_personal_data(out_abs)

  cat("\nCleared", sum(cleared$rows), "row(s) in total.\n\nWhat the copy still holds:\n")
  left <- remaining_rows(out_abs)
  if (!length(left)) cat("  (no rows in any database)\n")
  for (f in names(left)) cat(sprintf("  %-44s %s\n", f, paste(left[[f]], collapse = " ")))

  cat("\nDone. No accounts ship with this copy — the first person to register on",
      "\na fresh install becomes its admin.\n")
}
