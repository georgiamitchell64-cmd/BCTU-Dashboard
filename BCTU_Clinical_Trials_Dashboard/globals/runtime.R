# =============================================================================
# The two things report export needs that aren't R packages
# =============================================================================
# Exporting a report to PDF or Word needs:
#
#   pandoc  — rmarkdown shells out to it for every Word report, and for the
#             HTML-to-DOCX conversion behind the TMG/iTMG download.
#   Chrome  — chromote drives a headless browser to print the styled HTML
#             reports to PDF, and to bake the JS-drawn charts into the DOCX.
#
# Opened from RStudio both usually come for free: RStudio bundles pandoc, and
# most people have Chrome. A double-click launch has neither, which is why
# "Pandoc not found" is the error people hit first, and a packaged build can't
# assume anything about the machine it lands on.
#
# So the app can carry its own copies. scripts/fetch_runtime.R downloads them
# into runtime/ next to the app:
#
#   runtime/pandoc/pandoc[.exe]
#   runtime/chrome/<platform folder>/chrome-headless-shell[.exe]
#
# Nothing here downloads anything or fails when the folder is missing: with no
# bundled copy the app looks for an installed pandoc and an installed Chrome
# exactly as it always has. Bundling only adds a first place to look.
#
#   BCTU_RUNTIME_DIR   look somewhere other than runtime/ next to the app.
# =============================================================================

#' Where the bundled binaries live. Read-only as far as the app is concerned —
#' it only ever executes them — so this hangs off the install folder, not the
#' writable data root.
runtime_dir <- function() {
  env <- trimws(Sys.getenv("BCTU_RUNTIME_DIR", ""))
  if (nzchar(env)) env else file.path(app_install_dir(), "runtime")
}

.exe <- function(name)
  if (.Platform$OS.type == "windows") paste0(name, ".exe") else name

#' The folder holding a bundled pandoc, or NULL. A folder is what
#' ensure_pandoc() wants, since RSTUDIO_PANDOC names a directory.
bundled_pandoc_dir <- function() {
  d <- file.path(runtime_dir(), "pandoc")
  if (dir.exists(d) && file.exists(file.path(d, .exe("pandoc")))) d else NULL
}

#' The bundled headless Chrome binary, or NULL.
#'
#' chrome-headless-shell unpacks into a platform-named folder and needs the
#' rest of that folder beside it (ICU data, the shared libraries), so the
#' whole thing is kept and only the binary is named here. Any depth of
#' subfolder is searched so the archive's own layout doesn't have to be
#' flattened, on any platform.
bundled_chrome <- function() {
  d <- file.path(runtime_dir(), "chrome")
  if (!dir.exists(d)) return(NULL)
  hit <- list.files(d, pattern = paste0("^", .exe("chrome-headless-shell"), "$"),
                    recursive = TRUE, full.names = TRUE)
  if (!length(hit)) return(NULL)
  normalizePath(hit[1], winslash = "/", mustWork = FALSE)
}

#' Point the tools at the bundled copies, where there are any. Called once at
#' startup, before anything tries to render.
#'
#' Neither variable is overwritten if it is already set: someone who has set
#' RSTUDIO_PANDOC or CHROMOTE_CHROME themselves meant it, and a packaged
#' launcher can set either to override what it ships with.
#'
#' Returns, invisibly, what it found — used by the startup message and by
#' tests/runtime_paths.R.
use_bundled_runtime <- function() {
  found <- list(pandoc = NULL, chrome = NULL)

  p <- bundled_pandoc_dir()
  if (!is.null(p)) {
    found$pandoc <- p
    if (!nzchar(Sys.getenv("RSTUDIO_PANDOC"))) Sys.setenv(RSTUDIO_PANDOC = p)
  }

  ch <- bundled_chrome()
  if (!is.null(ch)) {
    found$chrome <- ch
    if (!nzchar(Sys.getenv("CHROMOTE_CHROME"))) Sys.setenv(CHROMOTE_CHROME = ch)
  }

  invisible(found)
}

#' One line for the startup log, so it's obvious which copies are in play.
runtime_summary <- function() {
  f <- use_bundled_runtime()
  parts <- c(
    if (!is.null(f$pandoc)) "pandoc: bundled"
    else if (nzchar(Sys.getenv("RSTUDIO_PANDOC"))) "pandoc: from RSTUDIO_PANDOC"
    else "pandoc: looking on this machine",
    if (!is.null(f$chrome)) "Chrome: bundled"
    else "Chrome: looking on this machine")
  paste(parts, collapse = " · ")
}
