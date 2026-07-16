accounts_tab_ui <- function() {
                  tabPanel("accounts",

                           # ── Admin: user passwords (real profiles) ──────
                           tonic_card(
                             title = "User passwords",
                             tools = span(style = "font-size:11px;color:#64748B;font-style:italic;",
                                          "Trial Managers only · resets a user's password to a temporary one they must change on next login"),
                             div(style = "margin-bottom:10px;display:flex;gap:8px;align-items:center;flex-wrap:wrap;",
                                 div(style = "flex:1;min-width:240px;",
                                     selectInput("pwadm_target", label = NULL,
                                                 choices = NULL,
                                                 width = "100%")),
                                 actionButton("pwadm_reset", HTML("&#8634; Reset password"),
                                              class = "btn btn-warning tm-only",
                                              style = "font-weight:600;")),
                             uiOutput("pwadm_result_ui")
                           ),

                           div(style = "margin-bottom:14px;"),

                           div(class = "grid-2",
                               tonic_card(title = "Add / edit account",
                                          div(class = "form-grid g2",
                                              style = "grid-template-columns:1fr 1fr",
                                              div(class = "form-field",
                                                  textInput("acc_name", "Full name", placeholder = "Dr / Prof.")),
                                              div(class = "form-field",
                                                  textInput("acc_user", "Username", placeholder = "first.last"))
                                          ),
                                          div(class = "form-grid g2",
                                              style = "grid-template-columns:1fr 1fr",
                                              div(class = "form-field",
                                                  selectInput("acc_role", "Role",
                                                              choices = c("CI", "Team Leader", "Guest", "Trial Manager"))),
                                              div(class = "form-field",
                                                  passwordInput("acc_pw", "Password"))
                                          ),
                                          div(style = "display:flex;gap:10px;margin:14px 0",
                                              actionButton("create_account", HTML("+ Create"),
                                                           class = "btn btn-success", style = "flex:1"),
                                              actionButton("update_account", HTML("&#x270E; Update selected"),
                                                           class = "btn btn-primary", style = "flex:1")
                                          ),
                                          div(style = "font-size:10px;font-weight:600;color:var(--navy);text-transform:uppercase;letter-spacing:.5px;margin-bottom:8px",
                                              "Role permissions"),
                                          uiOutput("perms_table_ui")
                               ),
                               tonic_card(title = "Current accounts \u2014 select to edit or remove",
                                          div(style = "margin-bottom:10px",
                                              actionButton("remove_account", HTML("&#x1F5D1; Remove selected"),
                                                           class = "btn btn-danger btn-sm")),
                                          withSpinner(reactableOutput("accounts_table"),
                                                      type = 4, color = col_teal),
                                          div(style = "padding:8px 12px;font-size:11px;color:var(--muted);font-style:italic;border-top:1px solid #EEF3F8",
                                              "Accounts are saved to the local SQLite database automatically.")
                               )
                           )
                  )
}
