# =============================================================================
# Test runner
# =============================================================================
# Runs every tests/*.R script in its own R process and prints one summary.
# Each script is self-contained (it sets its own working directory and sources
# what it needs), so a failure in one cannot mask or abort the others.
#
# A script that exits 0 after printing "SKIP:" — the guard some tests use when
# an optional package is absent — is reported as skipped, not passed, so a run
# that tested nothing cannot look green.
#
# Run from the app directory:  Rscript tests/run_all.R
#   --strict   treat a skipped script as a failure (use in CI)
#
# Exits non-zero if any script fails.
# =============================================================================

args      <- commandArgs(TRUE)
strict    <- "--strict" %in% args
this_file <- sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))
test_dir  <- dirname(this_file)
setwd(file.path(test_dir, ".."))

scripts <- setdiff(
  sort(list.files("tests", pattern = "[.][Rr]$", full.names = TRUE)),
  file.path("tests", basename(this_file))
)

if (!length(scripts)) {
  cat("No test scripts found in tests/.\n")
  quit(status = 1L)
}

rscript <- file.path(R.home("bin"), "Rscript")
results <- character(0)

for (script in scripts) {
  cat("\n", strrep("-", 70), "\n", script, "\n", strrep("-", 70), "\n", sep = "")
  out    <- system2(rscript, shQuote(script), stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  cat(out, sep = "\n")
  cat("\n")

  results[script] <-
    if (!identical(as.integer(status), 0L))       "FAIL"
    else if (any(grepl("^SKIP:", out)))           "skip"
    else                                          "pass"
}

cat("\n", strrep("=", 70), "\n", "Summary\n", strrep("=", 70), "\n", sep = "")
for (script in names(results))
  cat(sprintf("  %-6s %s\n", results[[script]], script))

failed  <- names(results)[results == "FAIL"]
skipped <- names(results)[results == "skip"]

if (length(failed)) {
  cat("\n", length(failed), " of ", length(results),
      " test scripts failed.\n", sep = "")
  quit(status = 1L)
}

if (length(skipped)) {
  cat("\n", length(skipped), " test script(s) skipped — a dependency is missing",
      ":\n", sep = "")
  for (script in skipped) cat("  ", script, "\n", sep = "")
  if (strict) {
    cat("\n--strict: a skipped script counts as a failure.\n")
    quit(status = 1L)
  }
}

cat("\n", length(results) - length(skipped), " of ", length(results),
    " test scripts passed.\n", sep = "")
