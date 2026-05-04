# =============================================================================
# Backup / Restore (Track A)
# =============================================================================
# Bundles the entire portfolio state into a single zip:
#   shared.sqlite          — profiles, memberships, dismissed notifications
#   trials/<code>/config.R — every trial's config
#   trials/<code>/overrides.json — every trial's overrides (themes, mappings, etc.)
#   trials/<code>/data/<code>.sqlite — per-trial sites + activity log
#
# Restore overwrites local copies with the contents of the zip. Designed for
# "I rebuilt my laptop and want everything back" or "share this snapshot with
# a colleague to inspect".
#
# REDCap CSV exports are intentionally NOT bundled (they can be huge and
# contain participant-level data — keep those local-only).
# =============================================================================

# ── List all paths that should go into a backup ────────────────────────────
.backup_paths <- function(root = getwd()) {
  out <- character(0)

  shared <- file.path(root, "data", "shared.sqlite")
  if (file.exists(shared)) out <- c(out, shared)

  trials_dir <- file.path(root, "trials")
  if (dir.exists(trials_dir)) {
    for (sub in list.dirs(trials_dir, recursive = FALSE)) {
      cfg <- file.path(sub, "config.R")
      if (file.exists(cfg)) out <- c(out, cfg)

      ovr <- file.path(sub, "overrides.json")
      if (file.exists(ovr)) out <- c(out, ovr)

      # per-trial SQLite — sites + activity log
      sqlite <- file.path(sub, "data",
                          paste0(basename(sub), ".sqlite"))
      if (file.exists(sqlite)) out <- c(out, sqlite)
    }
  }
  out
}

# Write a backup zip to `target` (a connection or path).
write_portfolio_backup <- function(target) {
  paths <- .backup_paths()
  if (!length(paths)) {
    stop("Nothing to back up — no shared DB or trial configs found.",
         call. = FALSE)
  }
  # zip wants paths relative to wd for clean entries
  rel <- sub(paste0("^", normalizePath(getwd(), winslash = "/"), "/"), "",
             normalizePath(paths, winslash = "/"))
  # Add a small manifest so the restore side knows what version produced it
  manifest_path <- tempfile(fileext = ".json")
  on.exit(unlink(manifest_path), add = TRUE)
  jsonlite::write_json(
    list(generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
         file_count   = length(paths),
         files        = rel),
    manifest_path, auto_unbox = TRUE, pretty = TRUE)
  # Stage the manifest into the zip too
  manifest_target <- "BACKUP_MANIFEST.json"
  file.copy(manifest_path, manifest_target, overwrite = TRUE)
  on.exit(unlink(manifest_target), add = TRUE)

  zip_files <- c(manifest_target, rel)
  utils::zip(zipfile = target, files = zip_files, flags = "-r9X")
  invisible(target)
}

# Restore from a zip file path.
# Returns a list(restored = N, skipped = M, errors = chr) summary.
restore_portfolio_backup <- function(zip_path, root = getwd()) {
  if (!file.exists(zip_path)) stop("Backup file not found: ", zip_path)

  tmp <- tempfile("bctu_restore_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  utils::unzip(zip_path, exdir = tmp)

  manifest_file <- file.path(tmp, "BACKUP_MANIFEST.json")
  if (!file.exists(manifest_file)) {
    stop("Not a valid BCTU portfolio backup (missing manifest).",
         call. = FALSE)
  }
  manifest <- jsonlite::fromJSON(manifest_file, simplifyVector = FALSE)
  files <- unlist(manifest$files)

  restored <- 0L; skipped <- 0L; errs <- character(0)
  for (rel in files) {
    src <- file.path(tmp, rel)
    dst <- file.path(root, rel)
    if (!file.exists(src)) { skipped <- skipped + 1L; next }
    if (!dir.exists(dirname(dst)))
      dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    ok <- tryCatch({
      file.copy(src, dst, overwrite = TRUE); TRUE
    }, error = function(e) {
      errs <<- c(errs, sprintf("%s: %s", rel, e$message)); FALSE
    })
    if (ok) restored <- restored + 1L else skipped <- skipped + 1L
  }
  list(restored = restored, skipped = skipped,
       errors = errs, manifest = manifest)
}
