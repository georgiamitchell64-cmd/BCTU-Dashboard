# =============================================================================
# Convert a REDCap Codebook CSV export into a REDCap data dictionary CSV
# =============================================================================
#   Rscript scripts/convert_redcap_codebook.R <codebook.csv> [output.csv]
#
# Run from the BCTU_Clinical_Trials_Dashboard directory. The dashboard can read
# the Codebook export directly (Trial settings -> import codebook); this is for
# when a dictionary CSV is wanted as a file in its own right.
# =============================================================================

.here <- function(f) {
  cand <- c(f, file.path("..", f), file.path(dirname(sys.frame(1)$ofile %||% "."), "..", f))
  hit <- cand[file.exists(cand)]
  if (!length(hit)) stop("Cannot find ", f, " - run this from the dashboard directory.")
  hit[1]
}
`%||%` <- function(a, b) if (is.null(a)) b else a
source(.here("functions/codebook.R"))

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("Usage: Rscript convert_redcap_codebook.R <codebook.csv> [output.csv]")
inp  <- args[1]
outp <- if (length(args) > 1) args[2] else
  sub("\\.csv$", "_data_dictionary.csv", inp, ignore.case = TRUE)
dd <- redcap_codebook_to_dictionary(inp)
write_redcap_dictionary(dd, outp)
cat(sprintf("%d fields, %d with coded choices -> %s\n", nrow(dd),
            sum(nzchar(dd[["Choices, Calculations, OR Slider Labels"]])), outp))
