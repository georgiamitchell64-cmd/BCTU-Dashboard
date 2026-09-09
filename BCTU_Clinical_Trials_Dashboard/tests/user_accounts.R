# =============================================================================
# Account creation — regression test
# =============================================================================
# Walks the journey an admin actually takes for a new team member:
#   create the profile → set a starting password → grant a trial role →
#   first sign-in with the temporary password → forced change → sign in again.
#
# Runs against a throwaway SQLite so it never touches data/shared.sqlite.
# Needs DBI + RSQLite + digest, which the app already requires.
#
# Run from the app directory:  Rscript tests/user_accounts.R
# Exits non-zero on the first failed assertion.
# =============================================================================

setwd(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE),
                                                value = TRUE))), ".."))

ok_pkgs <- all(vapply(c("DBI", "RSQLite", "digest"), requireNamespace,
                      logical(1), quietly = TRUE))
if (!ok_pkgs) {
  cat("SKIP: DBI / RSQLite / digest not installed.\n")
  quit(status = 0L)
}
suppressPackageStartupMessages({library(DBI); library(RSQLite)})

`%||%` <- function(a, b) if (is.null(a)) b else a
discover_trials <- function() list(alpha = list(code = "alpha"),
                                   beta  = list(code = "beta"))

tmp <- tempfile(fileext = ".sqlite")
source("functions/permissions.R")
SHARED_DB_PATH <- tmp          # after sourcing, so the constant is ours
# The real init also imports profiles from any trial database it finds on
# disk. This test is about the code, not about whoever is in this checkout.
.migrate_legacy_profiles <- function(con) invisible(NULL)
invisible(shared_db_init())

ok <- function(cond, what) {
  if (!isTRUE(cond)) { cat("FAIL:", what, "\n"); quit(status = 1L) }
  cat("  ✓", what, "\n")
}

# ── The admin creates a profile ──────────────────────────────────────────────
db_save_profile("Ada Lovelace", role = "Trial Manager", password = NULL,
                email = "ada@example.ac.uk")
ok(nrow(list_all_users()) == 1, "the profile is created")
ok(identical(user_portfolio_role("Ada Lovelace"), "admin"),
   "the first profile bootstraps as admin")

db_save_profile("Grace Hopper", role = "Statistician", password = NULL,
                email = "grace@example.ac.uk")
ok(identical(user_portfolio_role("Grace Hopper"), "member"),
   "everyone after the first is a member")
ok(!profile_has_password("Grace Hopper"),
   "a profile created without a password has none yet")

# fullname is UNIQUE, so a second Grace must be refused rather than written.
dup <- tryCatch({ db_save_profile("Grace Hopper", role = "x"); "written" },
                error = function(e) "refused")
ok(identical(dup, "refused"), "a duplicate name is refused by the database")

ok(!is.null(find_profile_by_email("GRACE@example.ac.uk")),
   "email lookup ignores case")
ok(is.null(find_profile_by_email("nobody@example.ac.uk")),
   "an unknown email finds nothing")

# ── Starting password ────────────────────────────────────────────────────────
res <- admin_reset_password("Grace Hopper", admin_fullname = "Ada Lovelace")
ok(isTRUE(res$success), "the admin sets a starting password")
temp <- res$temp_password
ok(is.character(temp) && nchar(temp) > 6, "a temporary password is returned")
ok(profile_has_password("Grace Hopper"), "the profile now has a password")
ok(isTRUE(is_password_reset_required("Grace Hopper")),
   "and must be changed at first sign-in")

bad <- admin_reset_password("Nobody At All")
ok(!isTRUE(bad$success), "resetting an unknown profile fails cleanly")

# ── First sign-in ────────────────────────────────────────────────────────────
ok(verify_password("Grace Hopper", temp), "the temporary password signs in")
ok(!verify_password("Grace Hopper", "wrong-one"), "a wrong password does not")
ok(!verify_password("Nobody At All", temp), "nor does an unknown profile")

set_password("Grace Hopper", "her-own-password")
clear_password_reset_required("Grace Hopper")
ok(!isTRUE(is_password_reset_required("Grace Hopper")),
   "choosing a password clears the force-change flag")
ok(verify_password("Grace Hopper", "her-own-password"),
   "the new password signs in")
ok(!verify_password("Grace Hopper", temp),
   "and the temporary one no longer works")

# Two users with the same password must not share a hash.
db_save_profile("Alan Turing", role = "Member", password = "her-own-password")
con <- shared_db_connect()
h <- dbGetQuery(con, "SELECT fullname, password_hash, password_salt FROM profiles")
dbDisconnect(con)
gh <- h$password_hash[h$fullname == "Grace Hopper"]
at <- h$password_hash[h$fullname == "Alan Turing"]
ok(!identical(gh, at), "the same password hashes differently per user")
ok(all(nzchar(h$password_salt[!is.na(h$password_salt)])), "every hash is salted")

# ── Trial access ─────────────────────────────────────────────────────────────
ok(length(user_visible_trials("Grace Hopper")) == 0,
   "a new member sees no trials until granted one")
ok(identical(user_trial_role("Grace Hopper", "alpha"), NULL) ||
     is.na(user_trial_role("Grace Hopper", "alpha") %||% NA),
   "and holds no role on a trial she is not a member of")

grant_membership("Grace Hopper", "alpha", "coordinator")
ok(identical(user_visible_trials("Grace Hopper"), "alpha"),
   "granting a role makes the trial visible")
ok(identical(user_trial_role("Grace Hopper", "alpha"), "coordinator"),
   "at the role granted")

grant_membership("Grace Hopper", "alpha", "manager")
ok(identical(user_trial_role("Grace Hopper", "alpha"), "manager"),
   "re-granting changes the role rather than duplicating the row")
ok(nrow(list_trial_members("alpha")) ==
     length(unique(list_trial_members("alpha")$fullname)),
   "each member appears once on a trial")

grant_membership("Grace Hopper", "alpha", "wizard")
ok(identical(user_trial_role("Grace Hopper", "alpha"), "readonly"),
   "an unknown role falls back to read-only, never to something wider")

revoke_membership("Grace Hopper", "alpha")
ok(length(user_visible_trials("Grace Hopper")) == 0, "access can be revoked")

# ── Admins ───────────────────────────────────────────────────────────────────
ok(isTRUE(user_is_admin("Ada Lovelace")), "the admin is an admin")
ok(identical(user_trial_role("Ada Lovelace", "beta"), "manager"),
   "an admin manages every trial, membership row or not")
set_portfolio_role("Grace Hopper", "admin")
ok(setequal(user_visible_trials("Grace Hopper"), c("alpha", "beta")),
   "promotion to admin grants every trial")

unlink(tmp)
cat("\nAll account assertions passed.\n")
