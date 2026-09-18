# =============================================================================
# The package manifest must match the code
# =============================================================================
# globals/required_packages.txt is what the build workflow installs into the
# desktop build and what the launcher checks for. If someone adds a library()
# call and doesn't refresh it, the installer silently ships without that
# package and dies on startup with "there is no package called X" — which is
# how ggplot2 got missed. This fails here instead, in a second, in CI.
# =============================================================================

app <- normalizePath(file.path(dirname(sub("^--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), ".."), winslash = "/")
source(file.path(app, "scripts", "required_packages.R"))

pass <- 0L; fail <- character(0)
ok <- function(cond, label) {
  if (isTRUE(cond)) pass <<- pass + 1L else fail <<- c(fail, label)
}

if (!requireNamespace("knitr", quietly = TRUE)) {
  cat("SKIP required-packages tests: knitr is not installed\n")
  if ("--strict" %in% commandArgs(trailingOnly = TRUE)) quit(status = 1L)
  quit(status = 0L)
}

scanned  <- required_packages(app)
manifest <- manifest_packages(app)

missing_from_manifest <- setdiff(scanned, manifest)
stale_in_manifest     <- setdiff(manifest, scanned)

ok(length(missing_from_manifest) == 0,
   paste("the code uses packages the manifest doesn't list:",
         paste(missing_from_manifest, collapse = ", "),
         "\n    Run: Rscript scripts/required_packages.R --write"))
ok(length(stale_in_manifest) == 0,
   paste("the manifest lists packages the code no longer uses:",
         paste(stale_in_manifest, collapse = ", "),
         "\n    Run: Rscript scripts/required_packages.R --write"))

ok(length(manifest) > 20, "the manifest is not suspiciously short")

# The ones whose absence killed the first installer, and the ones only ever
# reached through :: — the cases a naive library()-only scan would miss.
for (p in c("ggplot2", "chromote", "zip", "ragg", "openxlsx", "pdftools",
            "DiagrammeR", "systemfonts", "rmarkdown")) {
  ok(p %in% manifest, paste0("the manifest includes ", p))
}

# Base packages ship with R; installing them is an error, not a no-op.
for (p in c("stats", "utils", "tools", "grid", "graphics", "grDevices", "methods")) {
  ok(!(p %in% manifest), paste0("the manifest leaves out the base package ", p))
}

# Anything here would be a parser bug, and would fail the build on a name that
# does not exist on CRAN.
bad <- manifest[!grepl("^[A-Za-z][A-Za-z0-9.]*$", manifest)]
ok(length(bad) == 0, paste("every name is a valid package name; bad:",
                           paste(bad, collapse = ", ")))

# The launcher must read the manifest rather than keep its own list.
launcher <- readLines(file.path(app, "desktop", "run_dashboard.R"), warn = FALSE)
ok(any(grepl("manifest_packages", launcher)),
   "desktop/run_dashboard.R reads the manifest")

if (length(fail)) {
  cat("\n", length(fail), " required-package assertion(s) failed:\n", sep = "")
  for (f in fail) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("All required-package assertions passed. (", length(manifest), " packages)\n", sep = "")
