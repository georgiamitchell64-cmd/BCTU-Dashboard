accounts_server <- function(input, output, session, state) {
  rv <- state$rv

  # ── Admin password reset ────────────────────────────────────────────────
  # Operates on the real `profiles` SQLite table (the canonical user store).
  # Refresh the target dropdown whenever the user lands on Accounts.
  refresh_pwadm_targets <- function() {
    profiles <- tryCatch(db_load_profiles(), error = function(e) NULL)
    if (is.null(profiles) || !nrow(profiles)) {
      updateSelectInput(session, "pwadm_target",
                        choices = c("No registered users" = ""))
      return()
    }
    choices <- setNames(profiles$fullname,
                        paste0(profiles$fullname, "  —  ", profiles$role))
    # Don't let an admin reset themselves through this UI (they'd lock
    # themselves into the force-change loop with the temp password).
    me <- rv$username %||% ""
    if (nzchar(me)) choices <- choices[choices != me]
    updateSelectInput(session, "pwadm_target", choices = choices)
  }

  observe({
    rv$username; rv$home_membership_changed   # anything that hints at a change
    refresh_pwadm_targets()
  })

  pwadm_result <- reactiveVal(NULL)

  output$pwadm_result_ui <- renderUI({
    r <- pwadm_result()
    if (is.null(r)) return(NULL)
    if (!isTRUE(r$success)) {
      return(div(class = "login-inline-msg error",
                 style = "margin-top:6px;",
                 span(class = "login-inline-msg-ic", HTML("&times;")),
                 span(r$message)))
    }
    div(style = "margin-top:10px;padding:14px 16px;
                 background:#F0FDF9;border:1.5px dashed #2EC4A5;
                 border-radius:10px;",
        div(style = "font-size:11px;font-weight:600;color:#1B4F6B;
                     text-transform:uppercase;letter-spacing:.6px;margin-bottom:6px;",
            sprintf("Temporary password for %s", r$target)),
        div(style = "font-family:'JetBrains Mono','Menlo',monospace;
                     font-size:22px;font-weight:700;color:#1B4F6B;
                     letter-spacing:1px;user-select:all;",
            r$temp_password),
        div(style = "font-size:12px;color:#0F766E;margin-top:8px;line-height:1.5;",
            HTML(paste0("Relay this to the user — Slack, in person, whatever you use. ",
                        "They'll be required to set their own password on next login. ",
                        "<strong>This is the only time you'll see it.</strong>"))))
  })

  observeEvent(input$pwadm_reset, {
    target <- input$pwadm_target %||% ""
    if (!nzchar(target)) {
      pwadm_result(list(success = FALSE,
                        message = "Pick a user from the dropdown first."))
      return()
    }
    # Only portfolio admins may run this. The tab itself is open to anyone
    # managing this trial, so say which right is missing rather than leaving
    # them clicking a button that never works.
    me <- rv$username %||% NA_character_
    if (!isTRUE(user_is_admin(me))) {
      pwadm_result(list(success = FALSE,
                        message = paste("Only a portfolio admin can reset passwords.",
                                        "Ask one to do it from Manage users on the home screen.")))
      return()
    }
    out <- admin_reset_password(target_fullname = target,
                                admin_fullname  = me)
    pwadm_result(c(out, list(target = target)))
    if (isTRUE(out$success)) {
      showNotification(sprintf("Password reset for %s.", target),
                       type = "message", duration = 4)
    }
  })

  output$perms_table_ui <- renderUI({
    yes <- "<span style='color:#059669;font-weight:700'>&check;</span>"
    no  <- "<span style='color:#DC2626'>&cross;</span>"
    # The four roles a person can hold on a trial (functions/permissions.R,
    # TRIAL_ROLES), not the job titles the old card listed.
    roles <- list(
      c("Manager",      yes, yes, yes, yes),
      c("Coordinator",  yes, yes, yes, no),
      c("Statistician", yes, no,  yes, no),
      c("Read only",    yes, no,  no,  no)
    )
    rows <- lapply(roles, function(r) {
      tags$tr(lapply(r, function(v) tags$td(HTML(v))))
    })
    tags$table(class = "perm-table",
      tags$thead(tags$tr(
        tags$th("Trial role"), tags$th("View"), tags$th("Edit data"),
        tags$th("Download"), tags$th("Trial settings")
      )),
      tags$tbody(rows))
  })

  # Everyone with a role on this trial. Read-only: roles are granted from the
  # home screen's Manage users console, which is the only place that writes to
  # the shared profile store.
  output$accounts_table <- renderReactable({
    rv$home_membership_changed          # refresh when a role is granted
    code <- rv$trial_code
    req(!is.null(code), nzchar(code))
    df <- tryCatch(list_trial_members(code), error = function(e) NULL)
    if (is.null(df) || !nrow(df)) {
      return(reactable(data.frame(`Who has access` = character(0),
                                  check.names = FALSE),
                       compact = TRUE))
    }
    pretty <- c(manager = "Manager", coordinator = "Coordinator",
                statistician = "Statistician", readonly = "Read only")
    df$Role <- unname(pretty[df$trial_role])
    df$Role[is.na(df$Role)] <- df$trial_role[is.na(df$Role)]
    df$Portfolio <- ifelse(df$portfolio_role %in% "admin", "Admin", "Member")
    df$Since <- substr(as.character(df$granted_at), 1, 10)
    df <- df[, c("fullname", "Role", "Portfolio", "Since")]
    names(df)[1] <- "Name"
    reactable(df, striped = TRUE, highlight = TRUE, compact = TRUE,
              defaultColDef = colDef(style = list(fontFamily = "Outfit",
                                                  fontSize = "12.5px")),
              columns = list(Portfolio = colDef(align = "center", width = 90),
                             Since     = colDef(align = "center", width = 100)))
  })
}
