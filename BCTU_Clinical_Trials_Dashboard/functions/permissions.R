# =============================================================================
# Permissions: shared user DB with portfolio + per-trial roles
# =============================================================================
# A single SQLite at data/shared.sqlite holds:
#   profiles            — one row per user (fullname unique)
#   trial_memberships   — which trials each user is a member of, and at what role
#
# Portfolio roles:  'admin'  — sees and manages everything
#                   'member' — sees only trials they're a member of
#
# Per-trial roles:  'manager'      — full edit rights (TM)
#                   'coordinator'  — edit sites, randomisations, uploads
#                   'statistician' — read + downloads
#                   'readonly'     — read only
#
# Admins are implicit 'manager' on every trial.
# =============================================================================

SHARED_DB_PATH <- file.path(getwd(), "data", "shared.sqlite")

shared_db_connect <- function() {
  if (!dir.exists(dirname(SHARED_DB_PATH)))
    dir.create(dirname(SHARED_DB_PATH), recursive = TRUE, showWarnings = FALSE)
  dbConnect(RSQLite::SQLite(), SHARED_DB_PATH)
}

PORTFOLIO_ROLES <- c("admin", "member")
TRIAL_ROLES     <- c("manager", "coordinator", "statistician", "readonly")

# Role-rank: higher = more privilege. Used by require_role() guards.
.ROLE_RANK <- c(readonly = 1, statistician = 2, coordinator = 3, manager = 4)

# Server-side guard for sensitive operations. Returns TRUE if the active
# user's per-trial role meets the minimum, FALSE otherwise (and surfaces a
# notification). Use at the top of an observer:
#   observeEvent(input$delete_site, {
#     if (!require_role(rv, "manager")) return()
#     ...
#   })
# Admins always pass (their per-trial role is set to "manager" implicitly
# via grant on every trial).
require_role <- function(rv, min = "manager") {
  cur  <- .ROLE_RANK[rv$trial_role %||% "readonly"]
  need <- .ROLE_RANK[[min]]
  if (is.na(cur) || cur < need) {
    showNotification("You don't have permission for this action.",
                     type = "error", duration = 5)
    return(FALSE)
  }
  TRUE
}

# ── Schema + migration ──────────────────────────────────────────────────────
shared_db_init <- function() {
  con <- shared_db_connect()
  on.exit(dbDisconnect(con))

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS profiles (
      id              INTEGER PRIMARY KEY AUTOINCREMENT,
      fullname        TEXT NOT NULL UNIQUE,
      role            TEXT NOT NULL,
      portfolio_role  TEXT NOT NULL DEFAULT 'member',
      created         TEXT NOT NULL
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS trial_memberships (
      fullname    TEXT NOT NULL,
      trial_code  TEXT NOT NULL,
      trial_role  TEXT NOT NULL DEFAULT 'readonly',
      granted_at  TEXT NOT NULL,
      PRIMARY KEY (fullname, trial_code)
    )
  ")

  # Migration: add portfolio_role + password_hash + password_salt + email to legacy profiles
  cols <- tryCatch(dbGetQuery(con, "PRAGMA table_info(profiles)")$name,
                   error = function(e) character(0))
  if (length(cols) > 0 && !"portfolio_role" %in% cols) {
    dbExecute(con, "ALTER TABLE profiles ADD COLUMN portfolio_role TEXT NOT NULL DEFAULT 'member'")
  }
  if (length(cols) > 0 && !"password_hash" %in% cols) {
    dbExecute(con, "ALTER TABLE profiles ADD COLUMN password_hash TEXT")
  }
  if (length(cols) > 0 && !"password_salt" %in% cols) {
    dbExecute(con, "ALTER TABLE profiles ADD COLUMN password_salt TEXT")
  }
  if (length(cols) > 0 && !"email" %in% cols) {
    dbExecute(con, "ALTER TABLE profiles ADD COLUMN email TEXT")
  }
  # Force-change flag — set by admin_reset_password() so the next login
  # interrupts the user with a "set a new password" panel before the
  # temporary password becomes their permanent one.
  if (length(cols) > 0 && !"password_reset_required" %in% cols) {
    dbExecute(con,
      "ALTER TABLE profiles ADD COLUMN password_reset_required INTEGER NOT NULL DEFAULT 0")
  }

  # Drop legacy one-time-code table from the email-based forgot-password flow
  # we no longer ship. Admins now reset passwords from the Accounts tab.
  dbExecute(con, "DROP TABLE IF EXISTS password_reset_codes")

  # Bring in any profiles still living in the legacy per-trial DBs
  .migrate_legacy_profiles(con)

  # If we have profiles but no admin, the earliest profile becomes admin.
  n_admins <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM profiles WHERE portfolio_role='admin'")$n
  n_total  <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM profiles")$n
  if (n_admins == 0 && n_total > 0) {
    dbExecute(con, "UPDATE profiles SET portfolio_role='admin'
                    WHERE id=(SELECT MIN(id) FROM profiles)")
  }

  # Admins implicitly access every trial — give them an explicit membership too,
  # so the membership table is the single source of truth for "who's on this trial".
  trial_codes <- tryCatch(names(discover_trials()), error = function(e) character(0))
  admins <- dbGetQuery(con, "SELECT fullname FROM profiles WHERE portfolio_role='admin'")$fullname
  for (a in admins) for (tc in trial_codes) {
    .grant_internal(con, a, tc, "manager")
  }
}

.migrate_legacy_profiles <- function(con_shared) {
  candidates <- list.files(app_data_dir("data"),
                           pattern = "\\.sqlite$", full.names = TRUE,
                           ignore.case = TRUE)
  candidates <- c(candidates,
                  list.files(app_trials_dir(),
                             pattern = "\\.sqlite$", full.names = TRUE,
                             recursive = TRUE, ignore.case = TRUE))
  candidates <- setdiff(candidates, SHARED_DB_PATH)

  for (path in candidates) {
    con_l <- tryCatch(dbConnect(RSQLite::SQLite(), path), error = function(e) NULL)
    if (is.null(con_l)) next
    legacy <- tryCatch(
      dbGetQuery(con_l, "SELECT fullname, role, created FROM profiles"),
      error = function(e) NULL
    )
    dbDisconnect(con_l)
    if (is.null(legacy) || nrow(legacy) == 0) next

    for (i in seq_len(nrow(legacy))) {
      p <- legacy[i, ]
      existing <- dbGetQuery(con_shared,
        "SELECT id FROM profiles WHERE fullname = ?", params = list(p$fullname))
      if (nrow(existing) == 0) {
        dbExecute(con_shared,
          "INSERT INTO profiles (fullname, role, portfolio_role, created)
           VALUES (?, ?, 'member', ?)",
          params = list(p$fullname, p$role, p$created))
      }
    }
  }
}

# ── Profile CRUD (shared DB) ────────────────────────────────────────────────
db_load_profiles <- function() {
  con <- shared_db_connect()
  on.exit(dbDisconnect(con))
  tryCatch(
    dbGetQuery(con, "SELECT id, fullname, role, portfolio_role, created
                     FROM profiles ORDER BY id"),
    error = function(e) data.frame(id = integer(), fullname = character(),
                                   role = character(), portfolio_role = character(),
                                   created = character())
  )
}

db_save_profile <- function(fullname, role, password = NULL, email = NULL) {
  con <- shared_db_connect()
  on.exit(dbDisconnect(con))
  # First profile registered becomes admin (bootstraps the system).
  n <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM profiles")$n
  portfolio_role <- if (n == 0) "admin" else "member"

  pw <- .hash_password(password)
  email_val <- if (is.null(email) || !nzchar(trimws(email))) NA_character_
               else trimws(tolower(email))
  dbExecute(con,
    "INSERT INTO profiles (fullname, role, portfolio_role, created,
                            password_hash, password_salt, email)
     VALUES (?, ?, ?, ?, ?, ?, ?)",
    params = list(fullname, role, portfolio_role,
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                  pw$hash, pw$salt, email_val))
}

# Basic email format check — a single @ with a dot in the domain.
.is_valid_email <- function(s) {
  if (is.null(s)) return(FALSE)
  s <- trimws(s)
  if (!nzchar(s)) return(FALSE)
  grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", s, perl = TRUE)
}

set_email <- function(fullname, email) {
  if (!.is_valid_email(email)) return(invisible(FALSE))
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  dbExecute(con, "UPDATE profiles SET email = ? WHERE fullname = ?",
            params = list(trimws(tolower(email)), fullname))
  invisible(TRUE)
}

find_profile_by_email <- function(email) {
  if (!.is_valid_email(email)) return(NULL)
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  row <- tryCatch(
    dbGetQuery(con,
      "SELECT fullname, email FROM profiles WHERE LOWER(email) = ?",
      params = list(trimws(tolower(email)))),
    error = function(e) NULL)
  if (is.null(row) || !nrow(row)) return(NULL)
  list(fullname = row$fullname[1], email = row$email[1])
}

# ── Password hashing ────────────────────────────────────────────────────────
# sha256(salt || password). Salt is 16 random bytes per user, hex-encoded.
# Adequate for low-stakes internal tool; not bcrypt, but stops casual snooping.
.random_salt <- function() {
  paste0(sprintf("%02x", as.integer(sample(0:255, 16, replace = TRUE))),
         collapse = "")
}

.hash_password <- function(password) {
  if (is.null(password) || !nzchar(password))
    return(list(hash = NA_character_, salt = NA_character_))
  salt <- .random_salt()
  hash <- digest::digest(paste0(salt, password),
                         algo = "sha256", serialize = FALSE)
  list(hash = hash, salt = salt)
}

verify_password <- function(fullname, password) {
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  row <- tryCatch(
    dbGetQuery(con,
      "SELECT password_hash, password_salt FROM profiles WHERE fullname = ?",
      params = list(fullname)),
    error = function(e) NULL)
  if (is.null(row) || !nrow(row)) return(FALSE)
  stored_hash <- row$password_hash[1]
  stored_salt <- row$password_salt[1]
  # No password set yet → reject. The login flow handles this case separately
  # by prompting the user to set a password.
  if (is.na(stored_hash) || is.na(stored_salt) ||
      !nzchar(stored_hash) || !nzchar(stored_salt)) return(FALSE)
  candidate <- digest::digest(paste0(stored_salt, password),
                              algo = "sha256", serialize = FALSE)
  identical(candidate, stored_hash)
}

profile_has_password <- function(fullname) {
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  row <- tryCatch(
    dbGetQuery(con,
      "SELECT password_hash FROM profiles WHERE fullname = ?",
      params = list(fullname)),
    error = function(e) NULL)
  if (is.null(row) || !nrow(row)) return(FALSE)
  h <- row$password_hash[1]
  !is.na(h) && nzchar(h)
}

set_password <- function(fullname, new_password) {
  if (is.null(new_password) || !nzchar(new_password)) return(invisible(FALSE))
  pw <- .hash_password(new_password)
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  dbExecute(con,
    "UPDATE profiles SET password_hash = ?, password_salt = ? WHERE fullname = ?",
    params = list(pw$hash, pw$salt, fullname))
  invisible(TRUE)
}

# ── Admin password reset ───────────────────────────────────────────────────
# No email — an admin types in (or auto-generates) a temporary password for
# another user. The flag forces a change on the user's next login so the
# temp password is never their permanent secret.

#' Generate a memorable temporary password — 3 short words + 2 digits.
#' (Easier to read out over the phone than a random string.)
.generate_temp_password <- function() {
  words <- c("river","sunset","quiet","apple","cedar","north","amber","tiger",
             "harbor","ivory","linen","maple","ocean","peach","raven","spruce",
             "thistle","violet","willow","zephyr")
  paste0(paste(sample(words, 2), collapse = "-"),
         sprintf("%02d", sample.int(99, 1)))
}

#' Reset another user's password (admin action). Returns the plaintext
#' temporary password so the caller can show it to the admin to relay.
#' @param target_fullname  user whose password is being reset
#' @param admin_fullname   admin doing the reset (used for audit logging)
#' @param new_password     optional explicit password; otherwise auto-generated
#' @return list(success = logical, temp_password = string, message = string)
admin_reset_password <- function(target_fullname,
                                 admin_fullname = NA_character_,
                                 new_password   = NULL) {
  if (is.null(target_fullname) || !nzchar(target_fullname)) {
    return(list(success = FALSE, temp_password = NA,
                message = "No target user supplied."))
  }
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  exists <- dbGetQuery(con,
    "SELECT 1 FROM profiles WHERE fullname = ?",
    params = list(target_fullname))
  if (nrow(exists) == 0) {
    return(list(success = FALSE, temp_password = NA,
                message = paste0("No profile named '", target_fullname, "'.")))
  }
  pw_plain <- if (!is.null(new_password) && nzchar(new_password))
    new_password else .generate_temp_password()
  pw <- .hash_password(pw_plain)
  dbExecute(con,
    "UPDATE profiles
       SET password_hash = ?, password_salt = ?, password_reset_required = 1
     WHERE fullname = ?",
    params = list(pw$hash, pw$salt, target_fullname))

  # Audit trail — uses log_activity() from functions/activity_log.R if loaded.
  if (exists("log_activity", mode = "function")) {
    tryCatch(
      log_activity("password_reset_by_admin",
        sprintf("Admin <strong>%s</strong> reset password for <strong>%s</strong>",
                htmltools::htmlEscape(admin_fullname %||% "(unknown)"),
                htmltools::htmlEscape(target_fullname))),
      error = function(e) message("activity log: ", e$message))
  }
  list(success = TRUE, temp_password = pw_plain,
       message = paste0("Temporary password set for ", target_fullname, "."))
}

#' Read the force-change flag.
is_password_reset_required <- function(fullname) {
  if (is.null(fullname) || !nzchar(fullname)) return(FALSE)
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  row <- tryCatch(
    dbGetQuery(con,
      "SELECT password_reset_required FROM profiles WHERE fullname = ?",
      params = list(fullname)),
    error = function(e) data.frame())
  if (nrow(row) == 0) return(FALSE)
  isTRUE(as.integer(row$password_reset_required[1]) == 1L)
}

#' Clear the force-change flag — called when the user picks a new password
#' on the post-login interrupt screen.
clear_password_reset_required <- function(fullname) {
  if (is.null(fullname) || !nzchar(fullname)) return(invisible(FALSE))
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  dbExecute(con,
    "UPDATE profiles SET password_reset_required = 0 WHERE fullname = ?",
    params = list(fullname))
  invisible(TRUE)
}

# ── Membership helpers ──────────────────────────────────────────────────────
user_portfolio_role <- function(fullname) {
  if (is.null(fullname) || !nzchar(fullname)) return("member")
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  out <- tryCatch(
    dbGetQuery(con, "SELECT portfolio_role FROM profiles WHERE fullname=?",
               params = list(fullname)),
    error = function(e) data.frame())
  if (nrow(out) == 0) "member" else out$portfolio_role[1]
}

user_is_admin <- function(fullname) {
  isTRUE(user_portfolio_role(fullname) == "admin")
}

user_trial_role <- function(fullname, trial_code) {
  if (is.null(fullname) || is.null(trial_code)) return(NULL)
  if (user_is_admin(fullname)) return("manager")
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  out <- tryCatch(
    dbGetQuery(con, "SELECT trial_role FROM trial_memberships
                     WHERE fullname=? AND trial_code=?",
               params = list(fullname, trial_code)),
    error = function(e) data.frame())
  if (nrow(out) == 0) NULL else out$trial_role[1]
}

user_can_access <- function(fullname, trial_code) {
  !is.null(user_trial_role(fullname, trial_code))
}

user_visible_trials <- function(fullname) {
  if (is.null(fullname) || !nzchar(fullname)) return(character(0))
  if (user_is_admin(fullname)) {
    return(tryCatch(names(discover_trials()), error = function(e) character(0)))
  }
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  out <- tryCatch(
    dbGetQuery(con, "SELECT trial_code FROM trial_memberships WHERE fullname=?",
               params = list(fullname)),
    error = function(e) data.frame())
  if (nrow(out) == 0) character(0) else out$trial_code
}

# Admin actions
.grant_internal <- function(con, fullname, trial_code, trial_role) {
  if (!trial_role %in% TRIAL_ROLES) trial_role <- "readonly"
  dbExecute(con,
    "INSERT OR REPLACE INTO trial_memberships
     (fullname, trial_code, trial_role, granted_at)
     VALUES (?, ?, ?, ?)",
    params = list(fullname, trial_code, trial_role,
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
}

grant_membership <- function(fullname, trial_code, trial_role = "readonly") {
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  .grant_internal(con, fullname, trial_code, trial_role)
}

revoke_membership <- function(fullname, trial_code) {
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  dbExecute(con,
    "DELETE FROM trial_memberships WHERE fullname=? AND trial_code=?",
    params = list(fullname, trial_code))
}

set_portfolio_role <- function(fullname, role) {
  if (!role %in% PORTFOLIO_ROLES) return(invisible(FALSE))
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  dbExecute(con, "UPDATE profiles SET portfolio_role=? WHERE fullname=?",
            params = list(role, fullname))
  # Newly promoted admin: grant manager on every trial.
  if (role == "admin") {
    trial_codes <- tryCatch(names(discover_trials()), error = function(e) character(0))
    for (tc in trial_codes) .grant_internal(con, fullname, tc, "manager")
  }
}

list_all_users <- function() {
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  dbGetQuery(con, "SELECT fullname, role, portfolio_role, created
                   FROM profiles ORDER BY fullname")
}

list_user_memberships_for <- function(fullname) {
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  tryCatch(
    dbGetQuery(con,
      "SELECT trial_code, trial_role, granted_at FROM trial_memberships
       WHERE fullname=? ORDER BY trial_code",
      params = list(fullname)),
    error = function(e) data.frame(trial_code = character(),
                                   trial_role = character(),
                                   granted_at = character()))
}

list_trial_members <- function(trial_code) {
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  dbGetQuery(con,
    "SELECT m.fullname, m.trial_role, m.granted_at, p.portfolio_role
     FROM trial_memberships m
     LEFT JOIN profiles p ON p.fullname = m.fullname
     WHERE m.trial_code = ?
     ORDER BY m.fullname",
    params = list(trial_code))
}
