welcome_server <- function(input, output, session, state) {
  rv <- state$rv
  selected_role <- reactiveVal("Trial Manager")

  # ── Role button toggle ──────────────────────────────────────────────────
  role_map <- c(role_tm = "Trial Manager", role_ci = "CI",
                role_tl = "Team Leader", role_guest = "Guest")

  lapply(names(role_map), function(btn_id) {
    observeEvent(input[[btn_id]], {
      selected_role(role_map[[btn_id]])
      # Update active state via JS
      runjs("$('.role-pick-btn').removeClass('active-role');")
      runjs(sprintf("$('#%s').addClass('active-role');", btn_id))
    })
  })

  # ── Show existing profiles or new-user form ─────────────────────────────
  output$welcome_profiles_ui <- renderUI({
    profiles <- db_load_profiles()
    if (nrow(profiles) == 0) {
      # No profiles yet — just show the new user form
      return(NULL)
    }

    # Show returning user buttons
    btns <- lapply(seq_len(nrow(profiles)), function(i) {
      p <- profiles[i, ]
      div(
        style = "display:flex;align-items:center;gap:12px;padding:12px 14px;
                 background:#F8FAFD;border:1.5px solid #DDE5EE;border-radius:12px;
                 cursor:pointer;transition:all .15s;margin-bottom:8px;text-align:left;",
        onclick = sprintf("Shiny.setInputValue('returning_user', %d, {priority:'event'})", i),
        onmouseover = "this.style.borderColor='#2EC4A5';this.style.background='#F0FDF9';",
        onmouseout  = "this.style.borderColor='#DDE5EE';this.style.background='#F8FAFD';",

        # Avatar circle
        div(style = sprintf("width:38px;height:38px;border-radius:50%%;flex-shrink:0;
                             background:linear-gradient(135deg,%s,%s);
                             display:flex;align-items:center;justify-content:center;
                             color:#fff;font-weight:700;font-size:14px;",
                            if (isTRUE(p$portfolio_role == "admin")) "#6366F1" else "#94A3B8",
                            if (isTRUE(p$portfolio_role == "admin")) "#8B5CF6" else "#CBD5E1"),
            toupper(substr(p$fullname, 1, 1))),

        div(
          div(style = "font-size:14px;font-weight:600;color:#1B4F6B;", p$fullname),
          div(style = "font-size:11px;color:#64748B;", p$role)
        )
      )
    })

    tagList(
      div(style = "font-size:13px;font-weight:600;color:#1B4F6B;margin-bottom:10px;",
          "Welcome back"),
      btns,
      tags$hr(style = "border:none;border-top:1px solid #EEF3F8;margin:16px 0;"),
      actionButton("show_new_user", "New user? Create a profile",
                   class = "btn btn-link",
                   style = "color:#2EC4A5;font-weight:500;font-size:13px;padding:0;
                            text-decoration:none;")
    )
  })

  # ── Initially hide new-user form if profiles exist ──────────────────────
  observe({
    profiles <- db_load_profiles()
    if (nrow(profiles) > 0) {
      shinyjs::hide("new_user_panel")
    }
  })

  observeEvent(input$show_new_user, {
    shinyjs::show("new_user_panel")
  })

  # ── Returning user clicked → show password prompt ───────────────────────
  pending_login <- reactiveVal(NULL)

  observeEvent(input$returning_user, {
    profiles <- db_load_profiles()
    idx <- input$returning_user
    if (idx < 1 || idx > nrow(profiles)) return()
    p <- profiles[idx, ]

    pending_login(list(
      fullname       = p$fullname,
      role           = p$role,
      portfolio_role = p$portfolio_role %||% "member",
      has_password   = profile_has_password(p$fullname)
    ))

    # Hide profile picker + new-user form, show password panel
    shinyjs::hide("welcome_profiles_ui")
    shinyjs::hide("new_user_panel")
    shinyjs::show("password_panel")
    updateTextInput(session, "login_password", value = "")
  })

  output$password_prompt_name <- renderText({
    p <- pending_login()
    if (is.null(p)) return("")
    paste0("Welcome back, ", p$fullname)
  })

  # If the chosen profile has no password yet (legacy migration), let the user
  # set one inline.
  output$login_set_password_panel <- renderUI({
    p <- pending_login()
    if (is.null(p) || isTRUE(p$has_password)) return(NULL)
    div(style = "background:#FFFBEB;border:1px solid #FDE68A;border-radius:10px;
                 padding:12px;margin-bottom:14px;font-size:12px;color:#78350F;
                 line-height:1.6;",
        HTML("<strong>Set a password to continue.</strong> Your profile was
              created before passwords were enabled. Pick one now and it'll
              be required from next time."),
        passwordInput("login_password_confirm", label = NULL,
                      placeholder = "Confirm password",
                      width = "100%"))
  })

  observeEvent(input$login_back, {
    pending_login(NULL)
    shinyjs::hide("password_panel")
    shinyjs::show("welcome_profiles_ui")
    profiles <- db_load_profiles()
    if (!nrow(profiles)) shinyjs::show("new_user_panel")
  })

  observeEvent(input$login_go, {
    p <- pending_login()
    if (is.null(p)) return()
    pw <- input$login_password %||% ""

    if (!nzchar(pw)) {
      showNotification("Enter your password.", type = "warning")
      return()
    }

    if (isTRUE(p$has_password)) {
      # Standard login
      if (!verify_password(p$fullname, pw)) {
        showNotification("Incorrect password.", type = "error")
        return()
      }
    } else {
      # First-time password set
      confirm <- input$login_password_confirm %||% ""
      if (nchar(pw) < 6) {
        showNotification("Pick at least 6 characters.", type = "warning")
        return()
      }
      if (!identical(pw, confirm)) {
        showNotification("Passwords don't match.", type = "warning")
        return()
      }
      set_password(p$fullname, pw)
    }

    rv$username       <- p$fullname
    rv$role           <- p$role
    rv$portfolio_role <- p$portfolio_role
    pending_login(NULL)
    complete_login(rv, session)
  })

  # ── New user: Get started ───────────────────────────────────────────────
  observeEvent(input$welcome_go, {
    name    <- trimws(input$welcome_name %||% "")
    pw      <- input$welcome_password %||% ""
    confirm <- input$welcome_password_confirm %||% ""

    if (!nzchar(name)) {
      showNotification("Please enter your name.", type = "warning")
      return()
    }
    if (nchar(pw) < 6) {
      showNotification("Choose a password of at least 6 characters.",
                       type = "warning")
      return()
    }
    if (!identical(pw, confirm)) {
      showNotification("Passwords don't match.", type = "warning")
      return()
    }

    role <- selected_role()
    db_save_profile(name, role, password = pw)

    rv$username       <- name
    rv$role           <- role
    rv$portfolio_role <- user_portfolio_role(name)

    complete_login(rv, session)
  })
}


# ── Helper: complete login and show home screen ──────────────────────────────
complete_login <- function(rv, session) {
  # Per-trial role visibility is applied later when a trial is selected
  # (see apply_trial_role_visibility).  At login we just hide the welcome.
  shinyjs::hide("welcome_screen")
}

# Called from trial_selector_server when a trial is opened.
apply_trial_role_visibility <- function(trial_role) {
  is_manager <- isTRUE(trial_role == "manager")
  if (is_manager) {
    shinyjs::runjs("$('.tm-only').show()")
    shinyjs::runjs("$('.dl-data-btn').show()")
    shinyjs::show("accounts_nav")
  } else {
    shinyjs::runjs("$('.tm-only').hide()")
    shinyjs::runjs("$('.dl-data-btn').hide()")
    shinyjs::hide("accounts_nav")
  }
}


# Profile storage now lives in functions/permissions.R (shared SQLite).
