accounts_server <- function(input, output, session, state) {
  rv <- state$rv
  auth <- state$auth

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
    rv$username; rv$accounts        # depend on anything that hints at change
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
    # Only admins may run this. Fall back open if the role checker isn't
    # available (e.g. legacy install) — the UI button is .tm-only-hidden
    # for non-managers anyway.
    me <- rv$username %||% NA_character_
    if (exists("user_is_admin", mode = "function")) {
      if (!isTRUE(user_is_admin(me))) {
        pwadm_result(list(success = FALSE,
                          message = "Only Trial Managers can reset passwords."))
        return()
      }
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
    roles <- list(
      c("Trial Manager", yes, yes, yes, yes, yes),
      c("CI",            yes, no,  no,  yes, no),
      c("Team Leader",   yes, no,  no,  yes, no),
      c("Guest",         yes, no,  no,  no,  no)
    )
    rows <- lapply(roles, function(r) {
      tags$tr(lapply(r, function(v) tags$td(HTML(v))))
    })
    tags$table(class = "perm-table",
      tags$thead(tags$tr(
        tags$th("Role"), tags$th("View"), tags$th("Edit"),
        tags$th("Download data"), tags$th("Download graphs"), tags$th("Manage accounts")
      )),
      tags$tbody(rows)
    )
  })

  output$accounts_table <- renderReactable({
    df <- rv$accounts
    req(!is.null(df), "role" %in% names(df), nrow(df) > 0)
    df %>%
      mutate(
        Edit     = ifelse(role == "Trial Manager", "\u2713", "\u2717"),
        DataDL   = ifelse(role == "Trial Manager", "\u2713", "\u2717"),
        GraphDL  = ifelse(role %in% c("Trial Manager", "CI", "Team Leader"), "\u2713", "\u2717"),
        Accounts = ifelse(role == "Trial Manager", "\u2713", "\u2717")
      ) %>%
      rename(`Data DL` = DataDL, `Graph DL` = GraphDL) %>%
      select(Username = user, Name = fullname, Role = role, Edit, `Data DL`, `Graph DL`, Accounts) %>%
      reactable(striped = TRUE, highlight = TRUE, compact = TRUE,
                selection = "single", onClick = "select",
                defaultColDef = colDef(style = list(fontFamily = "Outfit", fontSize = "12.5px")),
                columns = list(
                  Edit       = colDef(align = "center"),
                  `Data DL`  = colDef(align = "center"),
                  `Graph DL` = colDef(align = "center"),
                  Accounts   = colDef(align = "center")
                ))
  })

  observeEvent(input$accounts_table__reactable__selected, {
    sel <- input$accounts_table__reactable__selected
    req(sel)
    if (sel < 1 || sel > nrow(rv$accounts)) return()
    acc <- rv$accounts[sel, ]
    updateTextInput(session, "acc_name", value = acc$fullname)
    updateTextInput(session, "acc_user", value = acc$user)
    updateSelectInput(session, "acc_role", selected = acc$role)
    updateTextInput(session, "acc_pw", value = "")
  })

  observeEvent(input$create_account, {
    req(input$acc_name, input$acc_user, input$acc_pw)
    if (input$acc_user %in% rv$accounts$user) {
      showNotification("Username already exists. Use Update selected to edit.", type = "warning")
      return()
    }
    rv$accounts <- bind_rows(rv$accounts, data.frame(
      user = input$acc_user, password = input$acc_pw,
      role = input$acc_role, fullname = input$acc_name,
      admin = FALSE, stringsAsFactors = FALSE))
    showNotification(paste0("Account created: ", input$acc_name), type = "message", duration = 8)
    updateTextInput(session, "acc_name", value = "")
    updateTextInput(session, "acc_user", value = "")
    updateTextInput(session, "acc_pw", value = "")
  })

  observeEvent(input$update_account, {
    sel <- input$accounts_table__reactable__selected
    req(sel)
    if (sel < 1 || sel > nrow(rv$accounts)) return()
    req(input$acc_name, input$acc_user)
    rv$accounts$fullname[sel] <- input$acc_name
    rv$accounts$user[sel]     <- input$acc_user
    rv$accounts$role[sel]     <- input$acc_role
    if (nzchar(input$acc_pw)) rv$accounts$password[sel] <- input$acc_pw
    showNotification("Account updated.", type = "message")
  })

  observeEvent(input$remove_account, {
    sel <- input$accounts_table__reactable__selected
    req(sel)
    if (sel < 1 || sel > nrow(rv$accounts)) return()
    current_user <- tryCatch(auth$user, error = function(e) NULL)
    if (!is.null(current_user) && identical(rv$accounts$user[sel], current_user)) {
      showNotification("Cannot remove your own account.", type = "error")
      return()
    }
    removed_user <- rv$accounts$user[sel]
    rv$accounts <- rv$accounts[-sel, , drop = FALSE]
    showNotification(paste("Account removed:", removed_user), type = "warning")
  })
}
