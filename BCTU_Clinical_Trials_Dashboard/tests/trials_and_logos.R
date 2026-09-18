# =============================================================================
# Shipped trials, and the logo each one shows
# =============================================================================
# Two things this guards:
#
#   * The build ships TONIC and PANORAMA. The example trials it used to carry
#     (atest, atrial, gogo, multiwp, test) were development fixtures and turned
#     up on real installs' My Trials page.
#   * A trial's logo resolves to a file that exists. config.R wrote the path
#     three ways — absolute, relative to the app folder, or not at all — and
#     the relative form only worked while the working directory was the app
#     folder, so on an installed build, where trials live in the writable data
#     root, every tile fell back to its initials.
#
#   Rscript tests/trials_and_logos.R
# =============================================================================

# Rscript passes spaces in the script's path as "~+~" on Windows.
.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
setwd(file.path(dirname(gsub("~+~", " ", .this, fixed = TRUE)), ".."))

need <- c("dplyr", "tibble", "rlang")
absent <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(absent)) {
  cat("SKIP: needs", paste(absent, collapse = ", "), "\n"); quit(status = 0L)
}
suppressPackageStartupMessages(for (p in need) library(p, character.only = TRUE))

for (f in c("globals/paths.R", "globals/constants.R", "globals/datasets.R",
            "globals/trial_config.R"))
  source(f)

ok <- function(cond, what) {
  if (!isTRUE(cond)) { cat("FAIL:", what, "\n"); quit(status = 1L) }
  cat("  ✓", what, "\n")
}

# ── What ships ────────────────────────────────────────────────────────────
shipped <- sort(basename(list.dirs("trials", recursive = FALSE)))
ok(identical(shipped, c("panorama", "tonic")),
   paste("trials/ holds only the real trials:", paste(shipped, collapse = ", ")))

trials <- discover_trials("trials")
ok(identical(sort(names(trials)), c("panorama", "tonic")),
   "and those are the trials the app discovers")

# ── Logos ─────────────────────────────────────────────────────────────────
lf <- trials$tonic$logo_file
ok(!is.null(lf) && file.exists(lf),
   paste("TONIC's logo resolves to a file that exists:", lf))
ok(identical(lf, normalizePath(lf, winslash = "/", mustWork = FALSE)),
   "as an absolute path, so it survives a change of working directory")

tmp <- file.path(tempdir(), paste0("trial_", as.integer(runif(1, 1e6, 9e6))))
dir.create(file.path(tmp, "www"), recursive = TRUE, showWarnings = FALSE)

ok(is.null(.trial_logo_file(NULL, tmp)),
   "a trial with no logo file resolves to NULL, not to a path that is not there")
ok(is.null(.trial_logo_file("trials/nowhere/www/logo.png", tmp)),
   "and so does a configured path that points at nothing")

png <- file.path(tmp, "www", "logo.png")
invisible(file.create(png))
ok(identical(.trial_logo_file(NULL, tmp), normalizePath(png, winslash = "/")),
   "dropping www/logo.png into a trial folder is enough to give it a logo")
ok(identical(.trial_logo_file("trials/whatever/www/logo.png", tmp),
             normalizePath(png, winslash = "/")),
   "a path written relative to the app folder finds the trial's own copy")

unlink(tmp, recursive = TRUE)
cat("PASS: tests/trials_and_logos.R\n")
