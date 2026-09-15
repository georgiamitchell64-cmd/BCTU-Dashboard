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
                               tonic_card(title = "Adding someone to the team",
                                          div(style = "font-size:12.5px;line-height:1.75;color:#334155;",
                                              HTML("Accounts are created once for the whole portfolio, not per
                                                    trial. An admin creates the profile, then gives it a role on
                                                    each trial the person works on."),
                                              tags$ol(style = "margin:12px 0 0 18px;padding:0;",
                                                tags$li(HTML("On the <strong>home screen</strong> (Back to trials),
                                                              open <strong>Manage users</strong>.")),
                                                tags$li(HTML("<strong>+ New user</strong> \u2014 full name and email.
                                                              Leave the password blank and a temporary one is
                                                              generated for you to pass on.")),
                                                tags$li(HTML("With the new user selected, set their role on this
                                                              trial. Until you do, they can sign in but will not
                                                              see it.")),
                                                tags$li(HTML("They set their own password the first time they
                                                              sign in.")))),
                                          div(style = "font-size:10px;font-weight:600;color:var(--navy);text-transform:uppercase;letter-spacing:.5px;margin:16px 0 8px",
                                              "What each trial role can do"),
                                          uiOutput("perms_table_ui")
                               ),
                               tonic_card(title = "Who has access to this trial",
                                          withSpinner(reactableOutput("accounts_table"),
                                                      type = 4, color = col_teal),
                                          div(style = "padding:8px 12px;font-size:11px;color:var(--muted);font-style:italic;border-top:1px solid #EEF3F8",
                                              "Roles are granted from Manage users on the home screen. Admins hold every trial.")
                               )
                           )
                  )
}
