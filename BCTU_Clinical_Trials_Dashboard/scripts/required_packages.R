# =============================================================================
# Which packages does the dashboard actually need?
# =============================================================================
# The desktop installer bundles an R library, and anything missing from it is
# not a warning — the app dies on startup with "there is no package called X",
# which is a 10-minute rebuild to find out about. A hand-maintained list in the
# build workflow drifts the moment someone adds a library() call, so this reads
# the answer out of the source instead.
#
#   Rscript scripts/required_packages.R           # one per line
#   Rscript scripts/required_packages.R --check    # and fail if any is missing
#   Rscript scripts/required_packages.R --write    # refresh the manifest
#
# Scanning takes about five seconds, most of it knitting the report templates,
# which is too slow to do at every launch. So the answer is cached in
# globals/required_packages.txt, which is what the launcher and the build
# workflow read; tests/required_packages.R re-scans and fails if that file has
# gone stale, so the drift shows up in CI rather than in a dead installer.
#
# It walks the parse tree rather than grepping, because grepping for "pkg::"
# also finds things like flextable's ph.p2:: inside a string, and a package
# list with imaginary names in it fails the build just as hard as a short one.
# =============================================================================

# Packages that ship with R. Nothing to install, so nothing to bundle.
BASE_PACKAGES <- c(
  "base", "compiler", "datasets", "grDevices", "graphics", "grid", "methods",
  "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils"
)

#' Every package named in one parse tree: library(x), require(x),
#' requireNamespace("x"), and x::y / x:::y.
.packages_in_expr <- function(e, found = character(0)) {
  if (is.call(e)) {
    fn <- e[[1]]
    if (is.name(fn)) {
      nm <- as.character(fn)
      if (nm %in% c("::", ":::") && length(e) >= 2) {
        found <- c(found, tryCatch(as.character(e[[2]]), error = function(err) character(0)))
      } else if (nm %in% c("library", "require", "requireNamespace", "loadNamespace")) {
        # library(dplyr) and library("dplyr") both; skip library(x) where x is
        # a variable, which only appears in loops over a list of names.
        arg <- tryCatch(if (length(e) >= 2) e[[2]] else NULL,
                        error = function(err) NULL)
        if (is.character(arg)) found <- c(found, arg)
        else if (is.name(arg)) {
          ch <- as.character(arg)
          # character.only = TRUE means the argument is a variable, not a package
          char_only <- "character.only" %in% names(e) && isTRUE(e[["character.only"]])
          if (!char_only) found <- c(found, ch)
        }
      }
    }
    # An empty argument — the blank in x[i, ] — parses to a symbol that errors
    # the moment anything looks at it, so ask inside tryCatch and skip those.
    parts <- as.list(e)
    for (i in seq_along(parts)) {
      worth_it <- tryCatch(is.call(parts[[i]]) || is.name(parts[[i]]),
                           error = function(err) FALSE)
      if (worth_it) found <- .packages_in_expr(parts[[i]], found)
    }
  }
  found
}

#' Scan one .R or .Rmd file.
packages_in_file <- function(path) {
  exprs <- tryCatch({
    if (grepl("[.][Rr]md$", path)) {
      code <- knitr::purl(path, output = tempfile(fileext = ".R"),
                          quiet = TRUE, documentation = 0L)
      parse(code, keep.source = FALSE)
    } else {
      parse(path, keep.source = FALSE)
    }
  }, error = function(e) NULL)
  if (is.null(exprs)) return(character(0))
  found <- character(0)
  for (e in exprs) found <- .packages_in_expr(e, found)
  unique(found)
}

#' Every package the app needs, sorted, base packages removed.
required_packages <- function(root = ".", exclude = BASE_PACKAGES) {
  # Matches what the installer actually ships — electron/package.json drops
  # mockups, tests and report backups, so a package only a superseded backup
  # template mentions is not one the app needs.
  skip <- paste0("(^|/)([.]git|[.]Rproj[.]user|renv|packrat|node_modules|",
                 "planner|mockups|tests|backups)/")
  files <- list.files(root, pattern = "[.]([Rr]|[Rr]md)$", recursive = TRUE,
                      full.names = TRUE)
  files <- files[!grepl(skip, files)]
  found <- unique(unlist(lapply(files, packages_in_file)))
  found <- found[grepl("^[A-Za-z][A-Za-z0-9.]*$", found)]   # valid package names only
  sort(setdiff(found, exclude))
}

MANIFEST <- "globals/required_packages.txt"

#' The cached list. This is what runtime code should use.
manifest_packages <- function(root = ".") {
  f <- file.path(root, MANIFEST)
  if (!file.exists(f)) return(character(0))
  p <- trimws(readLines(f, warn = FALSE))
  sort(unique(p[nzchar(p) & !startsWith(p, "#")]))
}

# The directory matters, not just the name: tests/required_packages.R sources
# this file, and a basename-only check would fire the whole staging run there.
if (length(grep("^--file=.*scripts[/\\\\]required_packages[.]R$", commandArgs(FALSE)))) {
  .this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
  app   <- normalizePath(file.path(dirname(gsub("~+~", " ", .this, fixed = TRUE)), ".."),
                         winslash = "/")
  pkgs <- required_packages(app)

  if ("--check" %in% commandArgs(trailingOnly = TRUE)) {
    # Check the manifest, not a fresh scan: this runs against the bundled
    # library to prove every package the installer ships can actually be
    # loaded, and it must work without knitr and without the Rmd templates.
    pkgs <- manifest_packages(app)
    if (!length(pkgs)) { cat("No manifest at ", MANIFEST, "\n", sep = ""); quit(status = 1L) }
    missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
    for (p in pkgs) cat(if (p %in% missing) "MISSING " else "ok      ", p, "\n", sep = "")
    if (length(missing)) {
      cat("\n", length(missing), " package(s) missing: ",
          paste(missing, collapse = ", "), "\n", sep = "")
      quit(status = 1L)
    }
    cat("\nAll", length(pkgs), "packages present.\n")
  } else if ("--write" %in% commandArgs(trailingOnly = TRUE)) {
    f <- file.path(app, MANIFEST)
    writeLines(c(
      "# Every package the dashboard needs, read out of the source by",
      "# scripts/required_packages.R --write. Do not edit by hand: the build",
      "# workflow installs exactly this list, and tests/required_packages.R",
      "# fails if it no longer matches the code.",
      pkgs), f)
    cat("Wrote", length(pkgs), "packages to", MANIFEST, "\n")
  } else {
    cat(pkgs, sep = "\n")
    cat("\n")
  }
}
