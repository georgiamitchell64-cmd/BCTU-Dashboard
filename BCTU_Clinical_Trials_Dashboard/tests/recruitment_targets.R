# =============================================================================
# Monthly recruitment targets — regression test
# =============================================================================
# Targets are entered by hand in Trial Settings → Recruitment targets and
# stored as report_content$monthly_targets. PANORAMA has no funder-agreed
# monthly profile, so with none entered the report must show recruitment
# without a plan rather than inventing a linear ramp from the sample size.
#
# This pins the editor's parser and the two report states.
#
# Base R only — run from the app directory:  Rscript tests/recruitment_targets.R
# Exits non-zero on the first failed assertion.
# =============================================================================

setwd(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE),
                                                value = TRUE))), ".."))

`%||%` <- function(a, b) if (is.null(a)) b else a

ok <- function(cond, what) {
  if (!isTRUE(cond)) { cat("FAIL:", what, "\n"); quit(status = 1L) }
  cat("  ✓", what, "\n")
}

# ── The editor's parser, lifted out of the Shiny module ──────────────────────
src  <- readLines("modules/trial_settings_server.R", warn = FALSE)
from <- grep("^  # A month written the way", src)[1]
to   <- grep("^  \\.rct_text <- function", src)[1] + 6L
ok(!is.na(from) && !is.na(to), "the targets parser is where the test expects it")
eval(parse(text = paste(sub("^  ", "", src[from:to]), collapse = "\n")))

one <- function(ln) {
  r <- .rct_parse_line(ln)
  if (is.null(r)) NA_character_ else paste(r$month, r$target)
}
ok(identical(one("2026-01, 5"), "2026-01 5"),   "YYYY-MM with a comma")
ok(identical(one("2026-02: 8"), "2026-02 8"),   "a colon separates too")
ok(identical(one("2026-03 8"),  "2026-03 8"),   "so does a plain space")
ok(identical(one("01/2026,7"),  "2026-01 7"),   "MM/YYYY")
ok(identical(one("Jan 2026, 12"), "2026-01 12"), "a month name")
ok(identical(one("Jan 26 3"),   "2026-01 3"),   "a two-digit year is this century")
ok(is.na(one("rubbish")),                        "a line with no number is skipped")
ok(is.na(one("")),                               "a blank line is skipped")

rows <- .rct_parse("2026-02, 8\n2026-01, 5\n2026-01, 6\nnot a target\n")
ok(length(rows) == 2, "duplicate months collapse to one entry")
ok(identical(rows[[1]]$month, "2026-01") && identical(rows[[1]]$target, 6),
   "entries come back in month order, the last value winning")
ok(identical(.rct_parse(""), list()), "an empty editor means no targets")
ok(identical(.rct_text(rows), "2026-01, 6\n2026-02, 8"),
   "saved targets round-trip back into the editor")

# ── The report's two states ──────────────────────────────────────────────────
# Evaluate the target block from the Rmd against a fixture, the way the
# report's setup chunk would.
rmd <- readLines("trials/panorama/reports/tmg_report.Rmd", warn = FALSE)
b   <- grep("^mt_entries <- rc\\$monthly_targets", rmd)[1]
e   <- grep("^month_lab  <- format\\(months_seq", rmd)[1]
ok(!is.na(b) && !is.na(e), "the report's target block is where the test expects it")

run <- function(entries) {
  env <- new.env()
  env$`%||%`     <- `%||%`
  env$rc         <- list(monthly_targets = entries)
  env$today      <- as.Date("2026-04-15")
  env$rec_dates  <- as.Date(c("2026-01-10", "2026-02-20"))
  env$d_screen   <- as.Date(c("2026-01-05", "2026-03-01"))
  eval(parse(text = paste(rmd[b:e], collapse = "\n")), envir = env)
  env
}

no_plan <- run(list())
ok(!isTRUE(no_plan$has_targets), "no entries means no targets")
ok(is.null(no_plan$cum_target),  "and no plan to measure against")
ok(length(no_plan$months_seq) == 4,
   "the months still span the data and the current month")

planned <- run(list(list(month = "2026-01", target = 5),
                    list(month = "2026-03", target = 8)))
ok(isTRUE(planned$has_targets), "entries mean there are targets")
ok(identical(planned$month_target, c(5, 0, 8, 0)),
   "a month with no target entered counts as none, not as interpolated")
ok(identical(planned$cum_target, c(5, 5, 13, 13)), "the plan is the running total")

cat("\nAll recruitment-target assertions passed.\n")
