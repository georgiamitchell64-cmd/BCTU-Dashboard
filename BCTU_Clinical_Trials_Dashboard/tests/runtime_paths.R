# =============================================================================
# The bundled report-export runtime — regression test
# =============================================================================
# Report export needs pandoc and a headless Chrome, neither an R package.
# globals/runtime.R lets the app carry its own copies in runtime/, and
# scripts/fetch_runtime.R puts them there. Three things have to hold:
#
#   * with nothing bundled, nothing changes — the app looks for an installed
#     pandoc and an installed Chrome exactly as it always has;
#   * with copies bundled, they are found and preferred, and ensure_pandoc()
#     picks the bundled one over anything installed on the machine;
#   * a value someone set themselves is never overwritten.
#
# It also covers the part of the fetch script that puts a downloaded archive
# in the right place — where the path bugs live — using archives built here
# rather than downloaded, so this needs no network.
#
# Run from the app directory:  Rscript tests/runtime_paths.R
# Exits non-zero on the first failed assertion.
# =============================================================================

# Rscript passes spaces in the script's path as "~+~" on Windows.
.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
setwd(file.path(dirname(gsub("~+~", " ", .this, fixed = TRUE)), ".."))

source("globals/paths.R")
source("globals/runtime.R")
source("scripts/fetch_runtime.R")   # defines the functions, downloads nothing

ok <- function(cond, what) {
  if (!isTRUE(cond)) { cat("FAIL:", what, "\n"); quit(status = 1L) }
  cat("  ✓", what, "\n")
}
norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
reset <- function(dir = "", pandoc = "", chrome = "") {
  Sys.setenv(BCTU_RUNTIME_DIR = dir, RSTUDIO_PANDOC = pandoc, CHROMOTE_CHROME = chrome)
  if (!nzchar(pandoc)) Sys.unsetenv("RSTUDIO_PANDOC")
  if (!nzchar(chrome)) Sys.unsetenv("CHROMOTE_CHROME")
}
win <- .Platform$OS.type == "windows"
pandoc_bin <- if (win) "pandoc.exe" else "pandoc"
chrome_bin <- if (win) "chrome-headless-shell.exe" else "chrome-headless-shell"

# ── Nothing bundled: the app is unchanged ────────────────────────────────────
cat("\nNothing bundled\n")
empty <- tempfile("runtime-empty-"); dir.create(empty)
reset(empty)
ok(is.null(bundled_pandoc_dir()), "no bundled pandoc is reported")
ok(is.null(bundled_chrome()), "no bundled Chrome is reported")
found <- use_bundled_runtime()
ok(is.null(found$pandoc) && is.null(found$chrome), "nothing is found")
ok(!nzchar(Sys.getenv("RSTUDIO_PANDOC")), "RSTUDIO_PANDOC is left unset")
ok(!nzchar(Sys.getenv("CHROMOTE_CHROME")), "CHROMOTE_CHROME is left unset")
ok(grepl("looking on this machine", runtime_summary()),
   "the summary says it will look on the machine")

# ── Bundled copies are found and used ────────────────────────────────────────
cat("\nBoth bundled\n")
rt <- tempfile("runtime-full-"); dir.create(rt)
dir.create(file.path(rt, "pandoc"), recursive = TRUE)
writeLines("#!/bin/sh\necho pandoc", file.path(rt, "pandoc", pandoc_bin))
# Chrome unpacks into a platform folder and is found at any depth
cdir <- file.path(rt, "chrome", "chrome-headless-shell-linux64")
dir.create(cdir, recursive = TRUE)
writeLines("#!/bin/sh\necho chrome", file.path(cdir, chrome_bin))
writeLines("x", file.path(cdir, "libEGL.so"))

reset(rt)
ok(identical(norm(bundled_pandoc_dir()), norm(file.path(rt, "pandoc"))),
   "the bundled pandoc folder is found")
ok(identical(norm(bundled_chrome()), norm(file.path(cdir, chrome_bin))),
   "the bundled Chrome is found inside its platform folder")
found <- use_bundled_runtime()
ok(identical(norm(Sys.getenv("RSTUDIO_PANDOC")), norm(file.path(rt, "pandoc"))),
   "RSTUDIO_PANDOC points at it")
ok(identical(norm(Sys.getenv("CHROMOTE_CHROME")), norm(file.path(cdir, chrome_bin))),
   "CHROMOTE_CHROME points at it")
ok(grepl("pandoc: bundled", runtime_summary()) &&
     grepl("Chrome: bundled", runtime_summary()),
   "the summary says both are bundled")

# ── A value someone set themselves wins ──────────────────────────────────────
cat("\nAlready set by hand\n")
reset(rt, pandoc = "/somewhere/of/my/own", chrome = "/my/own/chrome")
invisible(use_bundled_runtime())
ok(identical(Sys.getenv("RSTUDIO_PANDOC"), "/somewhere/of/my/own"),
   "an RSTUDIO_PANDOC that was already set is not overwritten")
ok(identical(Sys.getenv("CHROMOTE_CHROME"), "/my/own/chrome"),
   "a CHROMOTE_CHROME that was already set is not overwritten")

# ── ensure_pandoc() prefers the bundled copy ─────────────────────────────────
# The point of bundling: on a machine that also has pandoc installed, the copy
# shipped with the app is the one used, because it is the one we know works.
cat("\nensure_pandoc() with a bundled copy\n")
reset(rt)
`%||%` <- function(a, b) if (is.null(a)) b else a
suppressWarnings(suppressPackageStartupMessages(
  ok_pkgs <- all(vapply(c("dplyr", "stringr"), requireNamespace, logical(1), quietly = TRUE))))
if (ok_pkgs) {
  suppressPackageStartupMessages({library(dplyr); library(stringr)})
  source("functions/helpers.R")
  Sys.chmod(file.path(rt, "pandoc", pandoc_bin), "0755")
  ok(isTRUE(ensure_pandoc()), "ensure_pandoc() succeeds")
  ok(identical(norm(Sys.getenv("RSTUDIO_PANDOC")), norm(file.path(rt, "pandoc"))),
     "and resolves to the bundled copy, not one installed on the machine")
} else {
  cat("  (skipping — dplyr / stringr not installed)\n")
}

# ── Platform naming ──────────────────────────────────────────────────────────
# What each project calls this machine's download. Wrong here means the build
# fetches the wrong binary, which only shows up on that platform.
cat("\nPlatform naming\n")
p <- runtime_platform("Windows", "x86-64")
ok(p$chrome == "win64" && grepl("windows-x86_64", p$pandoc), "Windows x86_64")
p <- runtime_platform("Darwin", "arm64")
ok(p$chrome == "mac-arm64" && grepl("arm64-macOS", p$pandoc), "Apple silicon")
p <- runtime_platform("Darwin", "x86_64")
ok(p$chrome == "mac-x64" && grepl("x86_64-macOS", p$pandoc), "Intel Mac")
p <- runtime_platform("Linux", "x86_64")
ok(p$chrome == "linux64" && grepl("linux-amd64", p$pandoc), "Linux x86_64")
p <- runtime_platform("Linux", "aarch64")
ok(grepl("linux-arm64", p$pandoc), "Linux arm64")

# ── Unpacking what was downloaded ────────────────────────────────────────────
# Built here rather than downloaded, laid out the way the real archives are.
cat("\nUnpacking archives\n")
stage <- tempfile("archive-stage-"); dir.create(stage)

# pandoc: a single binary, under bin/ on Linux and macOS
src <- file.path(stage, "pandoc-9.9/bin"); dir.create(src, recursive = TRUE)
writeLines("#!/bin/sh\necho pandoc", file.path(src, pandoc_bin))
tar_path <- file.path(stage, "pandoc-9.9-linux-amd64.tar.gz")
old <- setwd(stage); utils::tar(tar_path, "pandoc-9.9", compression = "gzip"); setwd(old)
# Placed into a real runtime layout, so this also checks the two halves agree:
# what the fetch script writes is what the resolver goes looking for.
rtp <- tempfile("rt-pandoc-"); dir.create(rtp)
dest <- file.path(rtp, "pandoc")
placed <- install_pandoc_archive(tar_path, dest, windows = win)
ok(file.exists(file.path(dest, pandoc_bin)),
   "pandoc is lifted out of bin/ and put at the top of runtime/pandoc")
ok(identical(norm(placed), norm(file.path(dest, pandoc_bin))),
   "and the placed path is returned")
reset(rtp)
ok(identical(norm(bundled_pandoc_dir()), norm(dest)),
   "and the resolver finds what the fetch script placed")

# chrome: a folder that has to stay whole
cs <- file.path(stage, "chrome-headless-shell-linux64"); dir.create(cs, recursive = TRUE)
writeLines("#!/bin/sh\necho chrome", file.path(cs, chrome_bin))
writeLines("data", file.path(cs, "icudtl.dat"))
writeLines("lib",  file.path(cs, "libEGL.so"))
zip_path <- file.path(stage, "chrome-headless-shell-linux64.zip")
old <- setwd(stage)
zipped <- tryCatch({
  if (requireNamespace("zip", quietly = TRUE))
    zip::zip(zip_path, "chrome-headless-shell-linux64", mode = "mirror")
  else utils::zip(zip_path, "chrome-headless-shell-linux64", flags = "-r9Xq")
  TRUE
}, error = function(e) FALSE)
setwd(old)

if (zipped && file.exists(zip_path)) {
  rtc_root <- tempfile("rt-chrome-"); dir.create(rtc_root)
  cdest <- file.path(rtc_root, "chrome")
  placed <- install_chrome_archive(zip_path, cdest, windows = win)
  ok(file.exists(placed), "the Chrome binary is placed")
  ok(basename(placed) == chrome_bin, "under its own name")
  ok(file.exists(file.path(dirname(placed), "icudtl.dat")) &&
       file.exists(file.path(dirname(placed), "libEGL.so")),
     "with the data and library files it needs still beside it")
  reset(rtc_root)
  ok(identical(norm(bundled_chrome()), norm(placed)),
     "and the resolver finds what the fetch script placed")
} else {
  cat("  (skipping the Chrome archive check — no zip support here)\n")
}

reset()
cat("\nAll runtime assertions passed.\n")
