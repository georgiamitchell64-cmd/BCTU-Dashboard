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
                            if (p$role == "Trial Manager") "#1B4F6B" else "#64748B",
                            if (p$role == "Trial Manager") "#2EC4A5" else "#94A3B8"),
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

  # ── Returning user clicked ──────────────────────────────────────────────
  observeEvent(input$returning_user, {
    profiles <- db_load_profiles()
    idx <- input$returning_user
    if (idx < 1 || idx > nrow(profiles)) return()

    p <- profiles[idx, ]
    rv$username <- p$fullname
    rv$role     <- p$role

    # Apply role UI and proceed
    complete_login(rv, session)
  })

  # ── New user: Get started ───────────────────────────────────────────────
  observeEvent(input$welcome_go, {
    name <- trimws(input$welcome_name %||% "")
    if (!nzchar(name)) {
      showNotification("Please enter your name.", type = "warning")
      return()
    }

    role <- selected_role()

    # Save profile
    db_save_profile(name, role)

    rv$username <- name
    rv$role     <- role

    complete_login(rv, session)
  })
}


# ── Helper: complete login and show trial selector ────────────────────────────
complete_login <- function(rv, session) {
  is_tm <- isTRUE(rv$role == "Trial Manager")

  # Apply role visibility
  if (is_tm) {
    shinyjs::runjs("$('.tm-only').show()")
    shinyjs::runjs("$('.dl-data-btn').show()")
    shinyjs::show("accounts_nav")
  } else {
    shinyjs::runjs("$('.tm-only').hide()")
    shinyjs::runjs("$('.dl-data-btn').hide()")
    shinyjs::hide("accounts_nav")
  }

  # Hide welcome, show main app
  shinyjs::hide("welcome_screen")
}


# ── SQLite profile storage ────────────────────────────────────────────────────

db_init_profiles <- function() {
  con <- db_connect()
  on.exit(dbDisconnect(con))
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS profiles (
      id       INTEGER PRIMARY KEY AUTOINCREMENT,
      fullname TEXT NOT NULL,
      role     TEXT NOT NULL,
      created  TEXT NOT NULL
    )
  ")
}

db_load_profiles <- function() {
  con <- db_connect()
  on.exit(dbDisconnect(con))
  tryCatch({
    dbGetQuery(con, "SELECT * FROM profiles ORDER BY id")
  }, error = function(e) {
    data.frame(id = integer(), fullname = character(),
               role = character(), created = character())
  })
}

db_save_profile <- function(fullname, role) {
  con <- db_connect()
  on.exit(dbDisconnect(con))
  dbExecute(con,
    "INSERT INTO profiles (fullname, role, created) VALUES (?, ?, ?)",
    params = list(fullname, role, format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  )
}
