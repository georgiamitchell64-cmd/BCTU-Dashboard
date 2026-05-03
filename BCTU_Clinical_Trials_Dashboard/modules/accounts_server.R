accounts_server <- function(input, output, session, state) {
  rv <- state$rv
  auth <- state$auth

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
    rv$accounts %>%
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
