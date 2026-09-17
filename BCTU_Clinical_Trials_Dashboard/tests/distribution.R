# =============================================================================
# Distribution build — regression test
# =============================================================================
# Two things have to hold for a copy handed to someone else:
#
#   * no accounts, no activity log, no participant records leave with it —
#     and because .migrate_legacy_profiles() reads profiles out of EVERY
#     database under data/ and trials/ at startup, clearing shared.sqlite
#     alone is not enough: a profile left in a trial database walks straight
#     back in on first run;
#   * it can still be used. A build with no accounts is only safe if the
#     first person to open it becomes the admin, so this checks the bootstrap
#     against a genuinely stripped database rather than assuming it.
#
# The account database here is built by the app's own shared_db_init() and
# filled with db_save_profile(), so what gets stripped is the real schema
# rather than an approximation of it.
#
# Run from the app directory:  Rscript tests/distribution.R
# Exits non-zero on the first failed assertion.
# =============================================================================

# Rscript passes spaces in the script's path as "~+~" on Windows.
.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
setwd(file.path(dirname(gsub("~+~", " ", .this, fixed = TRUE)), ".."))

if (!all(vapply(c("DBI", "RSQLite", "digest"), requireNamespace, logical(1), quietly = TRUE))) {
  cat("SKIP: DBI / RSQLite / digest not installed.\n"); quit(status = 0L)
}
suppressPackageStartupMessages({library(DBI); library(RSQLite)})

source("scripts/prepare_distribution.R")   # defines the functions, runs nothing

`%||%` <- function(a, b) if (is.null(a)) b else a
discover_trials <- function() list(tonic = list(code = "tonic"),
                                   panorama = list(code = "panorama"))
source("globals/paths.R")
source("functions/permissions.R")
.migrate_legacy_profiles <- function(con) invisible(NULL)

ok <- function(cond, what) {
  if (!isTRUE(cond)) { cat("FAIL:", what, "\n"); quit(status = 1L) }
  cat("  ✓", what, "\n")
}
rows <- function(db, tb) {
  con <- dbConnect(SQLite(), db); on.exit(dbDisconnect(con))
  if (!tb %in% dbListTables(con)) return(NA_integer_)
  dbGetQuery(con, sprintf('SELECT COUNT(*) AS n FROM "%s"', tb))$n
}
exec <- function(con, sql) invisible(dbExecute(con, sql))

# ── A miniature app folder, dirtied the way a real one gets dirtied ──────────
root <- tempfile("dist-fixture-"); dir.create(root)
on.exit(unlink(root, recursive = TRUE), add = TRUE)
dir.create(file.path(root, "data"), recursive = TRUE)
dir.create(file.path(root, "trials", "tonic", "data"), recursive = TRUE)

# The account database, built and filled by the app's own code
shared <- file.path(root, "data", "shared.sqlite")
SHARED_DB_PATH <- shared          # after sourcing, so the constant is ours
invisible(shared_db_init())
db_save_profile("A Person", "Trial Manager", password = "Original123!")
grant_membership("A Person", "tonic", "manager")
con <- dbConnect(SQLite(), shared)
exec(con, "CREATE TABLE IF NOT EXISTS activity_events
           (id INTEGER, username TEXT, summary TEXT)")
exec(con, "INSERT INTO activity_events VALUES (1,'A Person','uploaded an export')")
dbDisconnect(con)
ok(nrow(db_load_profiles()) == 1, "the fixture starts with a real account in it")

postal <- file.path(root, "data", "postal_tracking.sqlite")
con <- dbConnect(SQLite(), postal)
exec(con, "CREATE TABLE postal_sent (participant_id TEXT, modified_by TEXT)")
exec(con, "INSERT INTO postal_sent VALUES ('1001','A Person')")
dbDisconnect(con)

# A trial database: a profile that must go, sites and settings that must stay.
trial <- file.path(root, "trials", "tonic", "data", "tonic.sqlite")
con <- dbConnect(SQLite(), trial)
exec(con, "CREATE TABLE profiles (fullname TEXT)")
exec(con, "INSERT INTO profiles VALUES ('A Person')")
exec(con, "CREATE TABLE sites (site_name TEXT, randomised INTEGER)")
exec(con, "INSERT INTO sites VALUES ('Queen Elizabeth', 12)")
exec(con, "CREATE TABLE projection_settings (k TEXT, v TEXT)")
exec(con, "INSERT INTO projection_settings VALUES ('target','898')")
exec(con, "CREATE TABLE canon_participants (id TEXT)")
exec(con, "INSERT INTO canon_participants VALUES ('P1')")
dbDisconnect(con)

writeLines("record_id,age", file.path(root, "trials", "tonic", "data", "EXPORT.csv"))
writeLines("x", file.path(root, "data", "something.csv"))
invisible(file.copy(trial, file.path(root, "trials", "tonic", "data", "tonic_mac.sqlite")))
writeLines("cfg", file.path(root, "trials", "tonic", "config.R"))

# ── Files that hold data rather than configuration ──────────────────────────
cat("\nFiles dropped\n")
gone <- drop_data_files(root)
ok(any(grepl("EXPORT[.]csv$", gone)), "a trial's REDCap export is removed")
ok(any(grepl("^data/something[.]csv$", gone)), "a CSV in the app's data folder is removed")
ok(any(grepl("_mac[.]sqlite$", gone)), "a stray *_mac.sqlite copy is removed")
ok(file.exists(file.path(root, "trials", "tonic", "config.R")),
   "a trial's config.R is left alone")

# ── Rows cleared ─────────────────────────────────────────────────────────────
cat("\nRows cleared\n")
cleared <- strip_personal_data(root, quiet = TRUE)
ok(rows(shared, "profiles") == 0, "accounts are cleared from shared.sqlite")
ok(rows(shared, "trial_memberships") == 0, "trial memberships are cleared")
ok(rows(shared, "activity_events") == 0, "the activity log is cleared")
ok(rows(postal, "postal_sent") == 0, "participant postal tracking is cleared")
ok(rows(trial, "profiles") == 0,
   "accounts in a TRIAL database are cleared too — they'd be re-imported otherwise")
ok(rows(trial, "canon_participants") == 0, "imported participant records are cleared")
ok(rows(trial, "sites") == 1, "sites survive — they're trial configuration")
ok(rows(trial, "projection_settings") == 1, "projection settings survive")
ok(sum(cleared$rows) == 6, "the report counts every row it cleared")

# An unclassified table is emptied rather than shipped: adding one later fails
# safe instead of quietly leaking whatever goes into it.
con <- dbConnect(SQLite(), shared)
exec(con, "CREATE TABLE something_new_and_sensitive (x TEXT)")
exec(con, "INSERT INTO something_new_and_sensitive VALUES ('secret')")
dbDisconnect(con)
invisible(strip_personal_data(root, quiet = TRUE))
ok(rows(shared, "something_new_and_sensitive") == 0,
   "a table nobody classified is emptied, not shipped")

# ── The stripped build can still be used ─────────────────────────────────────
# A build with no accounts is only safe if it can make one. The first person to
# register becomes the admin, and an admin manages every trial implicitly.
cat("\nBootstrapping an admin on the stripped build\n")
invisible(shared_db_init())          # the startup path, against the stripped DB
ok(nrow(db_load_profiles()) == 0, "the stripped build starts with no accounts")

db_save_profile("First Person", "Trial Manager", password = "SetMeUp123!")
ok(user_is_admin("First Person"), "the first person to register becomes the admin")
ok(verify_password("First Person", "SetMeUp123!"), "their password works")
ok(!verify_password("First Person", "Original123!"),
   "the account that was stripped out is really gone")
ok(setequal(user_visible_trials("First Person"), c("tonic", "panorama")),
   "and they can see every trial without being granted anything")
ok(identical(user_trial_role("First Person", "tonic"), "manager"),
   "with manager rights on each of them")

db_save_profile("Second Person", "Coordinator", password = "AlsoMe123!")
ok(!user_is_admin("Second Person"), "the second person does not become an admin")
ok(length(user_visible_trials("Second Person")) == 0,
   "and sees nothing until an admin grants it")

cat("\nAll distribution assertions passed.\n")
