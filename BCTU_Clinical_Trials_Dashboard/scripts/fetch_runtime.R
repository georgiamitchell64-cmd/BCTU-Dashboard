# =============================================================================
# Download the pandoc and headless Chrome the app carries with it
# =============================================================================
# Report export needs two things that are not R packages: pandoc (every Word
# report, and the HTML-to-DOCX conversion) and a Chrome (chromote prints the
# styled HTML reports to PDF, and bakes the JS-drawn charts into the DOCX).
# Opened from RStudio they usually come for free. A double-click launch, or a
# packaged build on a machine that has neither, does not.
#
# This puts both inside the app, where globals/runtime.R looks first:
#
#   runtime/pandoc/pandoc[.exe]
#   runtime/chrome/chrome-headless-shell-<platform>/chrome-headless-shell[.exe]
#
#   Rscript scripts/fetch_runtime.R              # whatever is missing
#   Rscript scripts/fetch_runtime.R --force      # re-download both
#   Rscript scripts/fetch_runtime.R --pandoc     # just one of them
#   Rscript scripts/fetch_runtime.R --chrome
#
# Run it once per platform you package for — the binaries are platform
# specific, so a Windows installer has to be built with the Windows ones.
# runtime/ is gitignored: it is ~200MB of third-party binaries, fetched at
# build time rather than committed.
#
# Chrome is chrome-headless-shell from Chrome for Testing, not full Chrome:
# it is the build Google ships for exactly this, a fraction of the size, and
# printToPDF is all the app asks of it.
#
# Sourcing this file only defines the functions; the fetch happens when it is
# the script Rscript was pointed at.
# =============================================================================

PANDOC_RELEASES <- "https://api.github.com/repos/jgm/pandoc/releases/latest"
CHROME_VERSIONS <-
  "https://googlechromelabs.github.io/chrome-for-testing/last-known-good-versions-with-downloads.json"

#' This machine, named the way each project names its downloads.
#' Returns list(os, arch, pandoc = <regex>, chrome = <platform key>).
runtime_platform <- function(sysname = Sys.info()[["sysname"]],
                             machine = Sys.info()[["machine"]]) {
  arm <- grepl("arm|aarch", machine, ignore.case = TRUE)
  switch(sysname,
    Windows = list(os = "windows", arch = "x86_64",
                   pandoc = "windows-x86_64[.]zip$", chrome = "win64"),
    Darwin  = if (arm)
      list(os = "mac", arch = "arm64",
           pandoc = "arm64-macOS[.]zip$",   chrome = "mac-arm64")
    else
      list(os = "mac", arch = "x86_64",
           pandoc = "x86_64-macOS[.]zip$",  chrome = "mac-x64"),
    list(os = "linux", arch = if (arm) "arm64" else "amd64",
         pandoc = if (arm) "linux-arm64[.]tar[.]gz$" else "linux-amd64[.]tar[.]gz$",
         chrome = "linux64")
  )
}

.download <- function(url, dest, what) {
  cat("  downloading", what, "\n    ", url, "\n")
  ok <- tryCatch({
    utils::download.file(url, dest, mode = "wb", quiet = TRUE); TRUE
  }, error = function(e) { cat("    failed:", conditionMessage(e), "\n"); FALSE })
  if (!ok || !file.exists(dest) || file.info(dest)$size == 0) return(FALSE)
  cat("    ", format(file.info(dest)$size / 1e6, digits = 3), "MB\n")
  TRUE
}

.unpack <- function(archive, exdir) {
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  if (grepl("[.]zip$", archive, ignore.case = TRUE))
    utils::unzip(archive, exdir = exdir)
  else
    utils::untar(archive, exdir = exdir)
  invisible(exdir)
}

#' Put the pandoc from `archive` into `dest`. pandoc is a single static
#' binary — the archive also carries man pages and data files it does not
#' need — so this finds the binary wherever the archive puts it (bin/ on
#' Linux and macOS, the top level on Windows) and copies just that.
install_pandoc_archive <- function(archive, dest, windows = .Platform$OS.type == "windows") {
  bin <- if (windows) "pandoc.exe" else "pandoc"
  tmp <- file.path(tempdir(), paste0("pandoc-unpack-", basename(tempfile())))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  .unpack(archive, tmp)

  hit <- list.files(tmp, pattern = paste0("^", bin, "$"), recursive = TRUE,
                    full.names = TRUE)
  if (!length(hit)) stop("no ", bin, " inside ", basename(archive), call. = FALSE)

  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  target <- file.path(dest, bin)
  file.copy(hit[1], target, overwrite = TRUE)
  Sys.chmod(target, "0755")
  target
}

#' Put the headless Chrome from `archive` into `dest`. Unlike pandoc this is
#' not one file: the binary needs the ICU data and shared libraries beside it,
#' so the archive's own folder is kept whole.
install_chrome_archive <- function(archive, dest, windows = .Platform$OS.type == "windows") {
  bin <- if (windows) "chrome-headless-shell.exe" else "chrome-headless-shell"
  if (dir.exists(dest)) unlink(dest, recursive = TRUE)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  .unpack(archive, dest)

  hit <- list.files(dest, pattern = paste0("^", bin, "$"), recursive = TRUE,
                    full.names = TRUE)
  if (!length(hit)) stop("no ", bin, " inside ", basename(archive), call. = FALSE)
  Sys.chmod(hit[1], "0755")
  # The shared libraries beside it need to be readable, and on macOS the
  # helper binaries in the bundle need to stay executable.
  for (f in list.files(dirname(hit[1]), recursive = TRUE, full.names = TRUE))
    if (grepl("[.](so|dylib)$|Helper$|chrome_crashpad_handler$", f)) Sys.chmod(f, "0755")
  hit[1]
}

#' The download URL for the newest pandoc for this platform.
pandoc_url <- function(plat = runtime_platform()) {
  rel <- jsonlite::fromJSON(PANDOC_RELEASES)
  assets <- rel$assets
  hit <- grep(plat$pandoc, assets$name)
  if (!length(hit))
    stop("no pandoc release asset matching ", plat$pandoc, call. = FALSE)
  list(url = assets$browser_download_url[hit[1]],
       name = assets$name[hit[1]], version = rel$tag_name)
}

#' The download URL for the current stable chrome-headless-shell.
chrome_url <- function(plat = runtime_platform()) {
  j <- jsonlite::fromJSON(CHROME_VERSIONS)
  d <- j$channels$Stable$downloads$`chrome-headless-shell`
  hit <- which(d$platform == plat$chrome)
  if (!length(hit))
    stop("no chrome-headless-shell for platform ", plat$chrome, call. = FALSE)
  list(url = d$url[hit[1]], name = basename(d$url[hit[1]]),
       version = j$channels$Stable$version)
}

# ── Fetch ────────────────────────────────────────────────────────────────────
.invoked_directly <- function() {
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  length(f) > 0 && identical(basename(sub("^--file=", "", f[1])), "fetch_runtime.R")
}

if (.invoked_directly()) {
  args  <- commandArgs(trailingOnly = TRUE)
  force <- "--force" %in% args
  only  <- intersect(c("--pandoc", "--chrome"), args)
  want  <- if (!length(only)) c("pandoc", "chrome") else sub("^--", "", only)

  .this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
  app   <- normalizePath(file.path(dirname(gsub("~+~", " ", .this, fixed = TRUE)), ".."),
                         winslash = "/")
  runtime <- file.path(app, "runtime")
  plat    <- runtime_platform()
  windows <- identical(plat$os, "windows")

  cat("Fetching the report-export runtime for ", plat$os, "-", plat$arch, "\n",
      "  into ", runtime, "\n\n", sep = "")

  failed <- character(0)

  if ("pandoc" %in% want) {
    dest <- file.path(runtime, "pandoc")
    have <- file.exists(file.path(dest, if (windows) "pandoc.exe" else "pandoc"))
    if (have && !force) {
      cat("pandoc already there (--force to re-download)\n")
    } else {
      cat("pandoc\n")
      res <- tryCatch({
        u <- pandoc_url(plat)
        cat("  version", u$version, "\n")
        arc <- file.path(tempdir(), u$name)
        if (!.download(u$url, arc, u$name)) stop("download failed")
        p <- install_pandoc_archive(arc, dest, windows)
        cat("  installed", p, "\n"); TRUE
      }, error = function(e) { cat("  FAILED:", conditionMessage(e), "\n"); FALSE })
      if (!isTRUE(res)) failed <- c(failed, "pandoc")
    }
  }

  if ("chrome" %in% want) {
    dest <- file.path(runtime, "chrome")
    bin  <- if (windows) "chrome-headless-shell.exe" else "chrome-headless-shell"
    have <- dir.exists(dest) &&
      length(list.files(dest, pattern = paste0("^", bin, "$"), recursive = TRUE))
    if (have && !force) {
      cat("chrome already there (--force to re-download)\n")
    } else {
      cat("chrome-headless-shell\n")
      res <- tryCatch({
        u <- chrome_url(plat)
        cat("  version", u$version, "\n")
        arc <- file.path(tempdir(), u$name)
        if (!.download(u$url, arc, u$name)) stop("download failed")
        p <- install_chrome_archive(arc, dest, windows)
        cat("  installed", p, "\n"); TRUE
      }, error = function(e) { cat("  FAILED:", conditionMessage(e), "\n"); FALSE })
      if (!isTRUE(res)) failed <- c(failed, "chrome")
    }
  }

  cat("\n")
  if (length(failed)) {
    cat("Could not fetch:", paste(failed, collapse = ", "),
        "\nThe app still works — it falls back to whatever is installed on the",
        "\nmachine. Re-run this when the network allows, or install pandoc and",
        "\nChrome normally.\n")
    quit(status = 1L)
  }
  cat("Done. The app will use these in preference to anything installed.\n")
}
