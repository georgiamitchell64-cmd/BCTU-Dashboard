# ─────────────────────────────────────────────────────────────────────────────
# People & access — the admin workspace on the home screen (modules/people.R)
# and the per-trial "Team & access" card in Settings → Trial profile.
# Every change goes through functions/permissions.R and is logged to the
# activity feed. Passwords are never shown or stored in plain text: an admin can
# only issue a temporary password, which the person must change at sign-in.
# ─────────────────────────────────────────────────────────────────────────────

.PP_ROLE_LABELS <- c(manager = "Manager", coordinator = "Coordinator",
                     statistician = "Statistician", readonly = "Read-only")

.pp_initials <- function(name) {
  parts <- strsplit(trimws(name %||% ""), "\\s+")[[1]]
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return("?")
  toupper(paste0(substr(parts[1], 1, 1), if (length(parts) > 1) substr(parts[length(parts)], 1, 1) else ""))
}

# Escape a value for use inside a single-quoted JavaScript string
.pp_js <- function(x) gsub("'", "\\\\'", gsub("\\\\", "\\\\\\\\", as.character(x)))

.pp_role_select <- function(user, trial, current, input_id, cls = "pp-select") {
  opts <- c(none = "No access", .PP_ROLE_LABELS)
  tags$select(class = cls, `aria-label` = sprintf("%s's access to %s", user, trial),
    onchange = sprintf("Shiny.setInputValue('%s',{user:'%s',trial:'%s',role:this.value,n:Math.random()},{priority:'event'})",
                       input_id, .pp_js(user), .pp_js(trial)),
    lapply(names(opts), function(v)
      tags$option(value = v, selected = if (identical(v, current)) NA else NULL, opts[[v]])))
}

.pp_last_active <- function() {
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  tryCatch(dbGetQuery(con, "SELECT username, MAX(timestamp) AS last FROM activity_events
                            WHERE username IS NOT NULL GROUP BY username"),
           error = function(e) data.frame(username = character(), last = character()))
}

.pp_user_activity <- function(user, n = 6) {
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  tryCatch(dbGetQuery(con, "SELECT timestamp, trial_code, summary FROM activity_events
                            WHERE username = ? ORDER BY id DESC LIMIT ?",
                      params = list(user, as.integer(n))),
           error = function(e) data.frame())
}

.pp_ago <- function(ts) {
  if (is.null(ts) || is.na(ts) || !nzchar(ts)) return("no recorded activity")
  if (exists(".time_ago", mode = "function")) .time_ago(ts) else ts
}

people_server <- function(input, output, session, state) {
  rv <- state$rv
  is_admin <- reactive(isTRUE(rv$portfolio_role == "admin"))
  changed  <- function() rv$home_membership_changed <- Sys.time()
  notify   <- function(msg, type = "message", d = 4) showNotification(msg, type = type, duration = d)

  # The People tab is for admins only
  observe(shinyjs::toggle("htab_people", condition = is_admin()))

  # ── Data ─────────────────────────────────────────────────────────────────
  trials <- reactive({ rv$home_membership_changed; tryCatch(discover_trials(), error = function(e) list()) })
  mems   <- reactive({ rv$home_membership_changed; list_all_memberships() })
  people <- reactive({
    rv$home_membership_changed
    if (!is_admin()) return(NULL)
    u <- tryCatch(list_all_users_detail(), error = function(e) NULL)
    if (is.null(u) || !nrow(u)) return(u)
    m  <- mems()
    la <- .pp_last_active()
    u$is_admin    <- u$portfolio_role == "admin"
    u$n_trials    <- vapply(u$fullname, function(n) sum(m$fullname == n), integer(1))
    u$last_active <- la$last[match(u$fullname, la$username)]
    u$attention   <- !u$has_password | u$reset_required
    u$no_access   <- !u$is_admin & u$n_trials == 0
    u
  })

  selected <- reactiveVal(NULL)
  mode     <- reactiveVal("detail")        # "detail" | "create"
  temp_pw  <- reactiveVal(NULL)            # list(user, pw), shown once

  observeEvent(people(), {
    p <- people()
    if (!is.null(p) && nrow(p) && identical(mode(), "detail") &&
        (is.null(selected()) || !selected() %in% p$fullname)) selected(p$fullname[1])
  })

  # ── Summary and list ─────────────────────────────────────────────────────
  output$pp_kpis <- renderUI({
    p <- people(); if (is.null(p)) return(NULL)
    kpi <- function(v, l, f, warn = FALSE)
      tags$button(type = "button", class = paste("pp-kpi", if (warn && v > 0) "warn"),
                  onclick = sprintf("ppFilter('%s')", f),
                  div(class = "pp-kpi-v", v), div(class = "pp-kpi-l", l))
    div(class = "pp-kpis",
        kpi(nrow(p), "People", "all"),
        kpi(sum(p$is_admin), "Admins", "admins"),
        kpi(sum(p$attention), "Awaiting a password", "attention", warn = TRUE),
        kpi(sum(p$no_access), "No trial access", "noaccess", warn = TRUE))
  })

  filtered <- reactive({
    p <- people(); if (is.null(p) || !nrow(p)) return(p)
    f <- input$pp_filter %||% "all"
    p <- switch(f, admins = p[p$is_admin, ], attention = p[p$attention, ], noaccess = p[p$no_access, ], p)
    q <- tolower(trimws(input$pp_search %||% ""))
    if (nzchar(q)) {
      hay <- tolower(paste(p$fullname, p$role %||% "", p$email %||% ""))
      p <- p[grepl(q, hay, fixed = TRUE), , drop = FALSE]
    }
    p
  })

  output$pp_list_ui <- renderUI({
    p <- filtered()
    if (is.null(p) || !nrow(people() %||% data.frame())) return(div(class = "pp-empty", "No people yet."))
    if (!nrow(p)) return(div(class = "pp-empty", "Nobody matches."))
    sel <- selected()
    tagList(lapply(seq_len(nrow(p)), function(i) {
      r <- p[i, ]
      job <- if (is.na(r$role) || !nzchar(r$role)) NULL else r$role
      flag <- if (!r$has_password) "No password yet" else if (r$reset_required) "Must set a new password at sign-in" else NULL
      tags$button(type = "button", class = paste("pp-item", if (identical(r$fullname, sel)) "on"),
        onclick = sprintf("Shiny.setInputValue('pp_select',{user:'%s',n:Math.random()},{priority:'event'})", .pp_js(r$fullname)),
        span(class = "pp-avatar", .pp_initials(r$fullname)),
        span(class = "pp-item-meta",
             span(class = "pp-item-name", r$fullname),
             span(class = "pp-item-sub", paste(c(if (r$is_admin) "Admin" else
                    sprintf("%d trial%s", r$n_trials, if (r$n_trials == 1) "" else "s"), job), collapse = " · "))),
        if (!is.null(flag)) span(class = "pp-flag", title = flag, `aria-label` = flag, "!"))
    }))
  })

  observeEvent(input$pp_select, { selected(input$pp_select$user); mode("detail"); temp_pw(NULL) })
  observeEvent(input$pp_new_user, { if (!is_admin()) return(); mode("create"); temp_pw(NULL) })
  observeEvent(input$pp_cancel_create, mode("detail"))

  # ── Detail panel ─────────────────────────────────────────────────────────
  create_form <- function() {
    div(class = "pp-card",
      div(class = "pp-card-head",
          span(class = "pp-avatar lg", HTML("&#43;")),
          div(div(class = "pp-name", "New person"),
              div(class = "pp-meta", "They choose their own password the first time they sign in."))),
      div(class = "pp-section",
          div(class = "pp-grid",
              div(class = "pp-field", tags$label("Full name"), textInput("pp_new_name", NULL, placeholder = "e.g. Jane Smith", width = "100%")),
              div(class = "pp-field", tags$label("Email"), textInput("pp_new_email", NULL, placeholder = "jane.smith@bham.ac.uk", width = "100%")),
              div(class = "pp-field", tags$label("Job title"), textInput("pp_new_title", NULL, placeholder = "e.g. Trial Coordinator", width = "100%")),
              div(class = "pp-field", tags$label("Portfolio role"),
                  selectInput("pp_new_portrole", NULL, c("Member" = "member", "Admin" = "admin"), width = "100%"))),
          div(class = "pp-field", tags$label("Starting password (optional)"),
              passwordInput("pp_new_pw", NULL, placeholder = "Leave blank and one is made up for you", width = "100%"))),
      div(class = "pp-actions",
          actionButton("pp_cancel_create", "Cancel", class = "pp-btn"),
          actionButton("pp_create", HTML("&#43; Create"), class = "pp-btn pp-btn-primary")))
  }

  output$pp_detail_ui <- renderUI({
    if (!is_admin()) return(NULL)
    if (identical(mode(), "create")) return(create_form())
    p <- people(); u <- selected()
    if (is.null(p) || is.null(u) || !u %in% p$fullname)
      return(div(class = "pp-empty", "Select someone, or add a new person."))
    r   <- p[p$fullname == u, ][1, ]
    me  <- identical(u, rv$username)
    trs <- trials(); m <- mems()
    last_admin <- r$is_admin && sum(p$is_admin) <= 1
    role_of <- function(tc) { x <- m$trial_role[m$fullname == u & m$trial_code == tc]; if (length(x)) x[1] else "none" }

    seg <- function(val, label) tags$button(type = "button",
      class = paste("pp-seg-btn", if ((val == "admin") == r$is_admin) "on"),
      disabled = if (last_admin && val == "member") NA else NULL,
      onclick = sprintf("Shiny.setInputValue('pp_set_portrole',{user:'%s',role:'%s',n:Math.random()},{priority:'event'})", .pp_js(u), val),
      label)

    pw_pill <- if (!r$has_password) span(class = "pp-pill warn", "No password yet")
               else if (r$reset_required) span(class = "pp-pill warn", "Must set a new password at sign-in")
               else span(class = "pp-pill ok", "Password set")
    tp <- temp_pw()
    temp_box <- if (!is.null(tp) && identical(tp$user, u))
      div(class = "pp-temp",
          div(class = "pp-temp-l", "Temporary password. Pass it on securely; it won't be shown again."),
          div(class = "pp-temp-pw", tp$pw),
          div(class = "pp-temp-n", "They'll be asked to choose their own the next time they sign in."))

    act <- .pp_user_activity(u)
    activity <- if (!nrow(act)) div(class = "pp-hint", "No recorded activity yet.") else
      tagList(lapply(seq_len(nrow(act)), function(i)
        div(class = "pp-act",
            span(class = "pp-act-t", .pp_ago(act$timestamp[i])),
            span(class = "pp-act-s", HTML(act$summary[i]),
                 if (!is.na(act$trial_code[i]) && nzchar(act$trial_code[i]))
                   span(class = "pp-act-trial", toupper(act$trial_code[i]))))))

    tagList(
      div(class = "pp-card",
        div(class = "pp-card-head",
            span(class = "pp-avatar lg", .pp_initials(u)),
            div(div(class = "pp-name", u, if (r$is_admin) span(class = "pp-badge", "Admin"), if (me) span(class = "pp-you", "you")),
                div(class = "pp-meta", paste(c(if (!is.na(r$role) && nzchar(r$role)) r$role,
                                               if (!is.na(r$email) && nzchar(r$email)) r$email,
                                               paste("Last active", .pp_ago(r$last_active))), collapse = " · ")))),

        div(class = "pp-section",
            div(class = "pp-label", "Profile"),
            div(class = "pp-grid",
                div(class = "pp-field", tags$label("Job title"), textInput("pp_edit_title", NULL, value = if (is.na(r$role)) "" else r$role, width = "100%")),
                div(class = "pp-field", tags$label("Email"), textInput("pp_edit_email", NULL, value = if (is.na(r$email)) "" else r$email, width = "100%"))),
            actionButton("pp_save_profile", "Save profile", class = "pp-btn")),

        div(class = "pp-section",
            div(class = "pp-label", "Portfolio role"),
            div(class = "pp-seg-group", seg("member", "Member"), seg("admin", "Admin")),
            div(class = "pp-hint", if (last_admin) "The only admin can't be changed to a member. Make someone else an admin first."
                                   else if (r$is_admin) "Admins see and manage every trial." else "Members see only the trials below.")),

        div(class = "pp-section",
            div(class = "pp-label-row", div(class = "pp-label", "Trial access"),
                if (!r$is_admin && length(trs)) div(class = "pp-bulk",
                  tags$button(type = "button", class = "pp-link",
                              onclick = sprintf("Shiny.setInputValue('pp_bulk',{user:'%s',what:'readonly_all',n:Math.random()},{priority:'event'})", .pp_js(u)),
                              "Read-only to all the rest"),
                  tags$button(type = "button", class = "pp-link",
                              onclick = sprintf("Shiny.setInputValue('pp_bulk',{user:'%s',what:'none_all',n:Math.random()},{priority:'event'})", .pp_js(u)),
                              "Remove all"))),
            if (r$is_admin) div(class = "pp-hint", "Admins have full access to every trial.")
            else if (!length(trs)) div(class = "pp-hint", "There are no trials yet.")
            else div(class = "pp-trials", lapply(names(trs), function(tc) {
              cur <- role_of(tc)
              div(class = paste("pp-trial", if (cur != "none") "has"),
                  div(class = "pp-trial-name", tags$b(trs[[tc]]$short_name %||% toupper(tc)),
                      span(trs[[tc]]$name %||% "")),
                  .pp_role_select(u, tc, cur, "pp_set_mem"))
            }))),

        div(class = "pp-section",
            div(class = "pp-label", "Sign-in"),
            div(pw_pill), temp_box,
            div(class = "pp-row",
                actionButton("pp_reset_pw", HTML("&#8634; Issue a temporary password"), class = "pp-btn"),
                span(class = "pp-or", "or"),
                div(class = "pp-inline", passwordInput("pp_set_pw_value", NULL, placeholder = "Set one yourself (6+ characters)", width = "100%")),
                actionButton("pp_set_pw", "Set", class = "pp-btn")),
            div(class = "pp-hint", "Either way, they choose their own password at their next sign-in. Passwords are encrypted and can't be viewed.")),

        div(class = "pp-section",
            div(class = "pp-label", "Recent activity"), activity),

        div(class = "pp-section pp-danger",
            div(class = "pp-label", "Remove"),
            div(class = "pp-row",
                div(class = "pp-hint", style = "flex:1;",
                    if (me) "You can't remove yourself." else if (last_admin) "The only admin can't be removed."
                    else "They won't be able to sign in and lose access to every trial. Their past activity stays in the log."),
                actionButton("pp_delete", "Remove person", class = "pp-btn pp-btn-danger",
                             disabled = isTRUE(me || last_admin)))))
    )
  })

  # ── Actions ──────────────────────────────────────────────────────────────
  observeEvent(input$pp_create, {
    if (!is_admin()) return()
    name  <- trimws(input$pp_new_name %||% ""); email <- trimws(input$pp_new_email %||% "")
    title <- trimws(input$pp_new_title %||% ""); pw <- input$pp_new_pw %||% ""
    if (!nzchar(name)) return(notify("Enter their full name.", "warning"))
    if (tolower(name) %in% tolower(people()$fullname %||% character(0))) return(notify("Someone with that name already exists.", "warning"))
    if (!.is_valid_email(email)) return(notify("Enter a valid email address.", "warning"))
    if (!is.null(find_profile_by_email(email))) return(notify("That email already belongs to someone.", "warning"))
    if (nzchar(pw) && nchar(pw) < 6) return(notify("A password needs at least 6 characters.", "warning"))
    ok <- tryCatch({ db_save_profile(name, role = if (nzchar(title)) title else "Member", password = NULL, email = email); TRUE },
                   error = function(e) { notify(paste("Couldn't create them:", e$message), "error", 8); FALSE })
    if (!ok) return()
    res <- tryCatch(admin_reset_password(name, admin_fullname = rv$username, new_password = if (nzchar(pw)) pw else NULL),
                    error = function(e) list(success = FALSE))
    if (identical(input$pp_new_portrole, "admin")) set_portfolio_role(name, "admin")
    log_activity("user_created", sprintf("Added <strong>%s</strong>", htmltools::htmlEscape(name)), username = rv$username)
    mode("detail"); selected(name)
    temp_pw(if (isTRUE(res$success) && !nzchar(pw)) list(user = name, pw = res$temp_password) else NULL)
    changed(); notify(sprintf("%s added. Now choose which trials they can open.", name))
  })

  observeEvent(input$pp_save_profile, {
    u <- selected(); if (!is_admin() || is.null(u)) return()
    title <- trimws(input$pp_edit_title %||% ""); email <- trimws(input$pp_edit_email %||% "")
    if (nzchar(email)) {
      if (!.is_valid_email(email)) return(notify("Enter a valid email address.", "warning"))
      other <- find_profile_by_email(email)
      if (!is.null(other) && !identical(other$fullname, u)) return(notify("That email already belongs to someone else.", "warning"))
      set_email(u, email)
    }
    if (nzchar(title)) set_job_title(u, title)
    log_activity("user_updated", sprintf("Updated <strong>%s</strong>'s profile", htmltools::htmlEscape(u)), username = rv$username)
    changed(); notify("Profile saved.")
  })

  observeEvent(input$pp_set_portrole, {
    if (!is_admin()) return()
    u <- input$pp_set_portrole$user; r <- input$pp_set_portrole$role
    p <- people()
    if (identical(r, "member") && isTRUE(p$is_admin[p$fullname == u]) && sum(p$is_admin) <= 1)
      return(notify("There must always be at least one admin.", "warning"))
    set_portfolio_role(u, r)
    if (identical(u, rv$username)) rv$portfolio_role <- r
    log_activity("portfolio_role_changed",
                 sprintf("Made <strong>%s</strong> %s", htmltools::htmlEscape(u), if (r == "admin") "an admin" else "a member"),
                 username = rv$username)
    changed(); notify(sprintf("%s is now %s.", u, if (r == "admin") "an admin" else "a member"))
  })

  set_access <- function(u, tc, role) {
    if (identical(role, "none")) {
      revoke_membership(u, tc)
      log_activity("membership_changed", sprintf("Removed <strong>%s</strong>'s access", htmltools::htmlEscape(u)),
                   username = rv$username, trial_code = tc)
    } else if (role %in% names(.PP_ROLE_LABELS)) {
      grant_membership(u, tc, role)
      log_activity("membership_changed",
                   sprintf("Gave <strong>%s</strong> %s access", htmltools::htmlEscape(u), tolower(.PP_ROLE_LABELS[[role]])),
                   username = rv$username, trial_code = tc)
    }
  }

  observeEvent(input$pp_set_mem, {
    if (!is_admin()) return()
    x <- input$pp_set_mem
    if (isTRUE(people()$is_admin[people()$fullname == x$user])) return(notify("Admins always have full access.", "warning"))
    set_access(x$user, x$trial, x$role); changed()
  })

  observeEvent(input$pp_bulk, {
    if (!is_admin()) return()
    u <- input$pp_bulk$user; what <- input$pp_bulk$what
    m <- mems(); have <- m$trial_code[m$fullname == u]
    n <- 0
    for (tc in names(trials())) {
      if (identical(what, "readonly_all") && !tc %in% have) { set_access(u, tc, "readonly"); n <- n + 1 }
      if (identical(what, "none_all") && tc %in% have)      { set_access(u, tc, "none");     n <- n + 1 }
    }
    changed()
    notify(if (n == 0) "Nothing to change." else if (what == "readonly_all")
             sprintf("Read-only access added to %d trial%s.", n, if (n == 1) "" else "s")
           else sprintf("Access removed from %d trial%s.", n, if (n == 1) "" else "s"))
  })

  observeEvent(input$pp_reset_pw, {
    u <- selected(); if (!is_admin() || is.null(u)) return()
    res <- tryCatch(admin_reset_password(u, admin_fullname = rv$username), error = function(e) list(success = FALSE, message = e$message))
    if (isTRUE(res$success)) { temp_pw(list(user = u, pw = res$temp_password)); changed() }
    else notify(res$message %||% "Couldn't issue a temporary password.", "error", 6)
  })

  observeEvent(input$pp_set_pw, {
    u <- selected(); if (!is_admin() || is.null(u)) return()
    pw <- input$pp_set_pw_value %||% ""
    if (nchar(pw) < 6) return(notify("A password needs at least 6 characters.", "warning"))
    res <- tryCatch(admin_reset_password(u, admin_fullname = rv$username, new_password = pw),
                    error = function(e) list(success = FALSE, message = e$message))
    if (isTRUE(res$success)) { temp_pw(NULL); changed(); notify(sprintf("Password set for %s. They'll choose their own at their next sign-in.", u), d = 5) }
    else notify(res$message %||% "Couldn't set the password.", "error", 6)
  })

  observeEvent(input$pp_delete, {
    u <- selected(); if (!is_admin() || is.null(u)) return()
    p <- people()
    if (identical(u, rv$username)) return(notify("You can't remove yourself.", "warning"))
    if (isTRUE(p$is_admin[p$fullname == u]) && sum(p$is_admin) <= 1) return(notify("The only admin can't be removed.", "warning"))
    showModal(modalDialog(title = sprintf("Remove %s?", u), size = "s", easyClose = TRUE,
      footer = tagList(modalButton("Cancel"),
                       actionButton("pp_delete_confirm", "Remove", class = "pp-btn pp-btn-danger")),
      p("They won't be able to sign in and lose access to every trial. Their past activity stays in the log.")))
  })

  observeEvent(input$pp_delete_confirm, {
    u <- selected(); if (!is_admin() || is.null(u) || identical(u, rv$username)) return(removeModal())
    removeModal()
    delete_profile(u)
    log_activity("user_removed", sprintf("Removed <strong>%s</strong>", htmltools::htmlEscape(u)), username = rv$username)
    selected(NULL); temp_pw(NULL); changed()
    notify(sprintf("%s removed.", u))
  })

  # ── Access matrix ────────────────────────────────────────────────────────
  output$pp_matrix_ui <- renderUI({
    p <- people(); trs <- trials(); m <- mems()
    if (is.null(p) || !nrow(p) || !length(trs)) return(div(class = "pp-empty", "No people or trials yet."))
    p <- p[order(p$is_admin, tolower(p$fullname)), ]
    head <- tags$tr(tags$th(class = "pp-mx-person", "Person"),
                    lapply(names(trs), function(tc) tags$th(title = trs[[tc]]$name %||% tc, trs[[tc]]$short_name %||% toupper(tc))))
    rows <- lapply(seq_len(nrow(p)), function(i) {
      r <- p[i, ]
      tags$tr(
        tags$th(class = "pp-mx-person", span(class = "pp-avatar sm", .pp_initials(r$fullname)), r$fullname),
        lapply(names(trs), function(tc) {
          if (r$is_admin) return(tags$td(span(class = "pp-mx-admin", "Admin")))
          cur <- m$trial_role[m$fullname == r$fullname & m$trial_code == tc]
          cur <- if (length(cur)) cur[1] else "none"
          tags$td(class = paste0("pp-mx r-", cur), .pp_role_select(r$fullname, tc, cur, "pp_set_mem", "pp-mx-select"))
        }))
    })
    div(class = "pp-matrix", tags$table(tags$thead(head), tags$tbody(rows)))
  })
  # Both views start hidden (the matrix behind its toggle, the team list inside a
  # Settings section), so render them anyway rather than wait for Shiny to
  # notice they've been shown.
  outputOptions(output, "pp_matrix_ui", suspendWhenHidden = FALSE)

  # ════════════════════════════════════════════════════════════════════════
  # Settings → Trial profile → Team & access (this trial only, managers)
  # ════════════════════════════════════════════════════════════════════════
  output$st_team_ui <- renderUI({
    rv$home_membership_changed
    code <- rv$trial_code; if (is.null(code)) return(NULL)
    mem <- tryCatch(list_trial_members(code), error = function(e) data.frame())
    can_edit <- isTRUE(rv$trial_role == "manager")
    rows <- if (!nrow(mem)) div(class = "s-hint", "Nobody has been given access yet.") else
      lapply(seq_len(nrow(mem)), function(i) {
        r   <- mem[i, ]
        adm <- identical(r$portfolio_role, "admin")
        me  <- identical(r$fullname, rv$username)
        div(class = "team-row",
            span(class = "pp-avatar sm", .pp_initials(r$fullname)),
            div(class = "team-name", r$fullname, if (me) span(class = "pp-you", "you")),
            if (adm) span(class = "pp-badge", "Admin · full access")
            else if (!can_edit || me) span(class = "team-role", .PP_ROLE_LABELS[r$trial_role] %||% r$trial_role)
            else tagList(.pp_role_select(r$fullname, code, r$trial_role, "st_team_set"),
                         tags$button(type = "button", class = "team-remove",
                                     title = sprintf("Remove %s from this trial", r$fullname),
                                     `aria-label` = sprintf("Remove %s from this trial", r$fullname),
                                     onclick = sprintf("Shiny.setInputValue('st_team_set',{user:'%s',trial:'%s',role:'none',n:Math.random()},{priority:'event'})",
                                                       .pp_js(r$fullname), .pp_js(code)),
                                     HTML("&times;"))))
      })
    tagList(div(class = "team-list", rows),
            if (!can_edit) div(class = "s-hint", "Only this trial's managers can change who has access."))
  })
  outputOptions(output, "st_team_ui", suspendWhenHidden = FALSE)

  observe({
    rv$home_membership_changed
    code <- rv$trial_code; req(code)
    all <- tryCatch(list_all_users()$fullname, error = function(e) character(0))
    have <- tryCatch(list_trial_members(code)$fullname, error = function(e) character(0))
    updateSelectizeInput(session, "st_team_add_user", choices = setdiff(all, have), selected = character(0))
  })

  observeEvent(input$st_team_set, {
    if (!require_role(rv, "manager")) return()
    x <- input$st_team_set; code <- rv$trial_code
    if (!identical(x$trial, code)) return()
    if (identical(x$user, rv$username)) return(notify("You can't change your own access.", "warning"))
    if (user_is_admin(x$user)) return(notify("Admins always have full access.", "warning"))
    set_access(x$user, code, x$role); changed()
    notify(if (identical(x$role, "none")) sprintf("%s removed from this trial.", x$user)
           else sprintf("%s is now %s.", x$user, tolower(.PP_ROLE_LABELS[[x$role]])), d = 3)
  })

  observeEvent(input$st_team_add, {
    if (!require_role(rv, "manager")) return()
    u <- input$st_team_add_user %||% ""; role <- input$st_team_add_role %||% "readonly"
    if (!nzchar(u)) return(notify("Choose someone to add.", "warning"))
    set_access(u, rv$trial_code, role); changed()
    notify(sprintf("%s can now open this trial.", u), d = 3)
  })
}
