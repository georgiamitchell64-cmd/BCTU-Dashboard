# =============================================================================
# Module start-up guard
# =============================================================================
# app.R starts every module server through start_module(). A module that throws
# must not take the others down with it, and — the part that used to be missing
# — the failure must be recorded rather than swallowed into the console.
#
# Run from the app directory:  Rscript tests/module_guard.R
# =============================================================================

setwd(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE),
                                                value = TRUE))), ".."))

need   <- "shiny"
absent <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(absent)) {
  cat("SKIP: needs", paste(absent, collapse = ", "), "\n"); quit(status = 0L)
}

source("functions/helpers.R")

ok <- function(label, cond) {
  if (!isTRUE(cond)) { cat("  x", label, "\n"); quit(status = 1L) }
  cat("  ✓", label, "\n")
}

started <- character(0)
note    <- function(name) started <<- c(started, name)

# A healthy module returns its value untouched.
value <- suppressMessages(start_module("Overview", { note("overview"); "ready" }))
ok("a module that starts returns its value", identical(value, "ready"))
ok("and is not recorded as a failure", length(module_failures()) == 0)

# A broken one is caught, and the modules after it still start.
broken <- suppressMessages(
  start_module("Reports", stop("no such column: consent_date"))
)
ok("a module that throws returns NULL instead of aborting",
   is.null(broken))

suppressMessages(start_module("Sites", note("sites")))
ok("the modules after a failure still start",
   identical(started, c("overview", "sites")))

# The failure is kept, with enough detail to act on.
failures <- module_failures()
ok("the failure is recorded", length(failures) == 1)
ok("under the module's name", identical(failures[[1]]$module, "Reports"))
ok("with the error message intact",
   identical(failures[[1]]$message, "no such column: consent_date"))
ok("and a timestamp", inherits(failures[[1]]$at, "POSIXct"))

# Failures accumulate newest first.
suppressMessages(start_module("Accounts", stop("permissions table is empty")))
failures <- module_failures()
ok("a second failure is recorded too", length(failures) == 2)
ok("newest first", identical(failures[[1]]$module, "Accounts"))

# Outside a Shiny session there is no notification to raise, and that must not
# itself throw — the guard is the last line of defence.
ok("the guard survives having no reactive domain",
   is.null(suppressMessages(start_module("Postal tracking", stop("boom")))))

cat("\nAll module-guard assertions passed.\n")
