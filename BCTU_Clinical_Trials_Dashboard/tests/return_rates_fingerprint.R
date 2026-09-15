# =============================================================================
# Return-rate fingerprint
# =============================================================================
# The dashboard polls the return-rate folder every five minutes. It reads the
# CSV again only when return_rates_fingerprint() changes, so the fingerprint
# has to move for every change that matters and stay still for everything else.
#
# Run from the app directory:  Rscript tests/return_rates_fingerprint.R
# =============================================================================

setwd(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE),
                                                value = TRUE))), ".."))

source("functions/helpers.R")
source("functions/return_rates_data.R")

ok <- function(label, cond) {
  if (!isTRUE(cond)) { cat("  x", label, "\n"); quit(status = 1L) }
  cat("  ✓", label, "\n")
}

root <- file.path(tempdir(), "rr_fingerprint")
unlink(root, recursive = TRUE)
dir.create(root, recursive = TRUE)

write_export <- function(path, rows) {
  write.csv(
    data.frame(Site = rep("Site A", rows), Event = "Baseline", Form = "EQ5D",
               Expected = rows, Due = rows, Entered = rows),
    path, row.names = FALSE
  )
}

# ── Nothing there yet ────────────────────────────────────────────────────────
empty <- file.path(root, "empty")
dir.create(empty)
fp_none <- return_rates_fingerprint(dir = empty, trial_code = "tonic")
ok("an empty folder fingerprints without erroring",
   is.character(fp_none) && length(fp_none) == 1 && !is.na(fp_none))
ok("and gives the same answer when asked twice",
   identical(fp_none, return_rates_fingerprint(dir = empty, trial_code = "tonic")))
ok("a different trial with no export is still a different fingerprint",
   !identical(fp_none, return_rates_fingerprint(dir = empty, trial_code = "panorama")))

# ── An export appears ────────────────────────────────────────────────────────
live <- file.path(root, "live")
dir.create(live)
first <- file.path(live, "TONIC_return_rate_20260101-090000.csv")
write_export(first, 3)

fp1 <- return_rates_fingerprint(dir = live, trial_code = "tonic")
ok("an export present fingerprints differently from an empty folder",
   !identical(fp1, fp_none))
ok("re-checking an untouched folder does not move the fingerprint",
   identical(fp1, return_rates_fingerprint(dir = live, trial_code = "tonic")))

# ── The same file is rewritten with more rows ────────────────────────────────
write_export(first, 9)
Sys.setFileTime(first, Sys.time() + 60)
fp2 <- return_rates_fingerprint(dir = live, trial_code = "tonic")
ok("rewriting the export moves the fingerprint", !identical(fp1, fp2))

# ── A newer export lands alongside the old one ───────────────────────────────
second <- file.path(live, "TONIC_return_rate_20260201-090000.csv")
write_export(second, 5)
fp3 <- return_rates_fingerprint(dir = live, trial_code = "tonic")
ok("a newer export moves the fingerprint again", !identical(fp2, fp3))
ok("and the fingerprint tracks the file the loader would actually read",
   grepl(basename(latest_return_rate_file(dir = live, trial_code = "tonic")),
         fp3, fixed = TRUE))

# ── The loader still returns that file's contents ────────────────────────────
loaded <- load_return_rates(dir = live, trial_code = "tonic")
ok("the loader reads the newest export",
   is.data.frame(loaded) && nrow(loaded) == 5 &&
     identical(attr(loaded, "source_file"), basename(second)))

unlink(root, recursive = TRUE)
cat("\nAll return-rate fingerprint assertions passed.\n")
