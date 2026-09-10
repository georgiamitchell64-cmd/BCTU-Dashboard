# =============================================================================
# Column-name cleaning and Labels-export detection — regression test
# =============================================================================
# clean_df_names() substituted [^a-z0-9] BEFORE lower-casing, so every capital
# letter was deleted rather than folded: "Record ID" came out as "ecord". Raw
# REDCap exports are already lower-case, so it only showed on a file that was
# not — which is exactly the Labels export that reported "Could not find
# required columns".
#
# A Labels export can never load (four columns all called "Complete?" cannot be
# told apart), so it is rejected with a message that says how to re-export.
#
# Base R only — run from the app directory:  Rscript tests/labels_export.R
# Exits non-zero on the first failed assertion.
# =============================================================================

setwd(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE),
                                                value = TRUE))), ".."))

`%||%` <- function(a, b) if (is.null(a)) b else a

src  <- readLines("functions/helpers.R", warn = FALSE)
from <- grep("^clean_df_names <- function", src)[1]
to   <- grep("^next_site_id <- function", src)[1] - 1L
eval(parse(text = paste(src[from:to], collapse = "\n")))

ok <- function(cond, what) {
  if (!isTRUE(cond)) { cat("FAIL:", what, "\n"); quit(status = 1L) }
  cat("  ✓", what, "\n")
}

nm <- function(...) {
  x <- as.data.frame(matrix(NA, 1, length(c(...))))
  names(x) <- c(...)
  names(clean_df_names(x))
}

ok(identical(nm("Record ID"), "record_id"),
   "capitals are folded, not deleted: Record ID -> record_id")
ok(identical(nm("Event Name"), "event_name"), "Event Name -> event_name")
ok(identical(nm("record_id"), "record_id"), "a raw export's names are unchanged")
ok(identical(nm("  Date of discharge  "), "date_of_discharge"),
   "surrounding space and inner punctuation still collapse to underscores")

labels_cols <- c("Record ID", "Event Name", "Repeat Instrument", "Data Access Group",
                 "Age at admission", "Date participant screened", "Complete?",
                 "Complete?", "Complete?", "Complete?")
raw_cols <- c("record_id", "redcap_event_name", "rc_site_name", "cae_age",
              "screen_date", "pat_sig_date", "screening_complete",
              "consent_complete")
flat_cols <- c("record_id", "rc_site_name", "cae_age", "screening_complete")

ok(isTRUE(looks_like_labels_export(labels_cols)),
   "a Labels export is recognised by its question-text headings")
ok(!isTRUE(looks_like_labels_export(raw_cols)),
   "a raw longitudinal export is not")
ok(!isTRUE(looks_like_labels_export(flat_cols)),
   "a raw flat export with no event column is not")
ok(!isTRUE(looks_like_labels_export(character(0))),
   "an empty column set is not")

cat("\nAll labels-export assertions passed.\n")
