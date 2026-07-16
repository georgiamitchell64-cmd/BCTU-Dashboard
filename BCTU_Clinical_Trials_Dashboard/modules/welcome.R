welcome_screen_ui <- function() {
  div(id = "welcome_screen", class = "login-stage",

      # ── Enter-to-submit: pressing Enter in a login field clicks the
      #    relevant button (sign in / register / set new password). ──────
      tags$script(HTML("
        $(document).on('keydown', '#login_password, #login_password_confirm', function(e){
          if (e.key === 'Enter') { e.preventDefault(); $('#login_go').click(); }
        });
        $(document).on('keydown', '#welcome_name, #welcome_email, #welcome_password, #welcome_password_confirm', function(e){
          if (e.key === 'Enter') { e.preventDefault(); $('#welcome_go').click(); }
        });
        $(document).on('keydown', '#change_pw_new, #change_pw_confirm', function(e){
          if (e.key === 'Enter') { e.preventDefault(); $('#change_pw_submit').click(); }
        });
      ")),

      # ── Scoped styles for login form controls ────────────────────
      tags$style(HTML("
        .login-stage .form-group { margin-bottom:0 !important; }
        .login-stage .form-control {
          height:42px !important; padding:0 14px !important;
          border:1px solid #E2E8EE !important; border-radius:8px !important;
          font-family:'Inter',system-ui,sans-serif !important;
          font-size:14px !important;
        }
        .login-stage .form-control:focus {
          border-color:#0FA88E !important;
          box-shadow:0 0 0 3px rgba(46,196,165,.18) !important;
        }
        .login-role-btn {
          padding:10px 12px !important; border-radius:8px !important;
          border:1px solid #E2E8EE !important; background:#fff !important;
          color:#27384A !important; font-size:12px !important; font-weight:500 !important;
          font-family:'Inter',sans-serif !important; transition:all .15s !important;
          width:100% !important;
        }
        .login-role-btn:hover { border-color:#1B4F6B !important; color:#1B4F6B !important; }
        .login-role-btn.active-role {
          border-color:#2EC4A5 !important; background:rgba(46,196,165,.06) !important;
          color:#1B4F6B !important; font-weight:600 !important;
          box-shadow:0 0 0 2px rgba(46,196,165,.15) !important;
        }

        /* Inline error / status messages under form fields */
        .login-inline-msg {
          margin-top: 10px;
          padding: 10px 12px;
          border-radius: 8px;
          font-size: 12.5px;
          line-height: 1.5;
          display: flex;
          align-items: flex-start;
          gap: 8px;
        }
        .login-inline-msg.error {
          background: #FEF2F2;
          border: 1px solid #FECACA;
          color: #991B1B;
        }
        .login-inline-msg.warn {
          background: #FFFBEB;
          border: 1px solid #FDE68A;
          color: #78350F;
        }
        .login-inline-msg.info {
          background: #EEF2FF;
          border: 1px solid #C7D2FE;
          color: #3730A3;
        }
        .login-inline-msg.ok {
          background: #ECFDF5;
          border: 1px solid #A7F3D0;
          color: #065F46;
        }
        .login-inline-msg-ic {
          flex-shrink: 0;
          font-weight: 700;
          font-size: 14px;
          line-height: 1.1;
        }

        /* Small help text under field labels */
        .login-help {
          font-size: 11px; color: #64748B; margin-top: 4px; line-height: 1.4;
        }

        /* Inline 'Forgot password?' link next to label */
        .login-link-inline {
          font-size: 11.5px; font-weight: 500; color: #2EC4A5 !important;
          text-decoration: none; padding: 0; background: transparent;
          border: none; cursor: pointer;
        }
        .login-link-inline:hover { color: #1B4F6B !important; text-decoration: underline; }

        /* The OTC code displayed in-app as the email-fallback */
        .login-otc-display {
          margin: 12px 0; padding: 14px 16px;
          background: #F0FDF9; border: 1.5px dashed #2EC4A5;
          border-radius: 10px; text-align: center;
        }
        .login-otc-display-eye {
          font-size: 10.5px; font-weight: 600; color: #1B4F6B;
          text-transform: uppercase; letter-spacing: .6px;
          margin-bottom: 6px;
        }
        .login-otc-display-code {
          font-family: 'JetBrains Mono', 'Menlo', monospace;
          font-size: 28px; font-weight: 700; color: #1B4F6B;
          letter-spacing: 6px;
        }
      ")),

      # ═══ LEFT: Form side ════════════════════════════════════════════
      div(class = "login-form-side",

        # Top right env badge (logo now lives in the centered hero block below)
        div(style = "display:flex;align-items:flex-start;justify-content:flex-end;",
            div(class = "login-env-badge",
                span(style = "width:6px;height:6px;background:#F59E0B;border-radius:50%;"),
                "Local")
        ),

        # Centered BCTU logo on the left form side
        div(class = "login-hero-logo",
            tags$img(src = "BlackText-landscape.png",
                     style = "width:100%;max-width:420px;object-fit:contain;display:block;")
        ),

        # Form area
        div(class = "login-form-wrap",
          div(class = "login-form-card",

            # ── Returning user profiles ──────────────────────────────
            uiOutput("welcome_profiles_ui"),

            # ── New user panel ───────────────────────────────────────
            div(id = "new_user_panel",

              div(class = "login-step-eye", "Create profile"),
              tags$h1(class = "login-title", "Get started"),
              p(class = "login-subtitle",
                "Create your profile to access the dashboard. Your data is stored locally."),

              # Name
              div(class = "login-field",
                  tags$label("Your full name"),
                  textInput("welcome_name", label = NULL,
                            placeholder = "e.g. Georgia Mitchell",
                            width = "100%")),

              # Email
              div(class = "login-field",
                  tags$label("Email address"),
                  textInput("welcome_email", label = NULL,
                            placeholder = "you@bham.ac.uk",
                            width = "100%"),
                  div(class = "login-help",
                      "Used for password reset codes. Stored locally only.")),

              # Password
              div(class = "login-field",
                  tags$label("Choose a password"),
                  passwordInput("welcome_password", label = NULL,
                                placeholder = "At least 6 characters",
                                width = "100%")),
              div(class = "login-field", style = "margin-top:-6px;",
                  passwordInput("welcome_password_confirm", label = NULL,
                                placeholder = "Confirm password",
                                width = "100%")),

              # Inline error region for the new-user form
              uiOutput("welcome_error_msg"),

              # Role picker
              div(class = "login-field",
                  tags$label("Your role"),
                  div(class = "login-role-grid",
                      actionButton("role_tm", "Trial Manager",
                                   class = "btn login-role-btn active-role"),
                      actionButton("role_ci", "CI / Investigator",
                                   class = "btn login-role-btn"),
                      actionButton("role_tl", "Team Leader",
                                   class = "btn login-role-btn"),
                      actionButton("role_guest", "Guest / Read-only",
                                   class = "btn login-role-btn")
                  )
              ),

              # Submit
              div(style = "margin-top:20px;",
                  actionButton("welcome_go", "Get started →",
                               class = "login-btn-primary"))
            ),

            # ── Password prompt panel (returning user) ───────────────
            shinyjs::hidden(div(id = "password_panel",
                div(class = "login-step-eye", "Sign in"),
                div(style = "margin-bottom:8px;",
                    tags$h1(class = "login-title",
                            textOutput("password_prompt_name", inline = TRUE))),
                div(class = "login-field",
                    tags$label("Password"),
                    passwordInput("login_password", label = NULL,
                                  placeholder = "Enter password",
                                  width = "100%"),
                    # No email-based reset on this install — contact an admin
                    # who can issue a temporary password from the Accounts tab.
                    div(class = "login-help",
                        style = "font-size:11px;color:#64748B;margin-top:6px;",
                        HTML("Forgotten your password? Ask a Trial Manager to issue you a temporary one from the <em>Accounts</em> tab."))),
                uiOutput("login_error_msg"),
                uiOutput("login_set_password_panel"),
                div(style = "display:grid;grid-template-columns:1fr 1fr;gap:8px;margin-top:16px;",
                    actionButton("login_back", HTML("&larr; Back"),
                                 class = "login-btn-secondary"),
                    actionButton("login_go", "Sign in",
                                 class = "login-btn-primary"))
            )),

            # ── Force-change panel — shown after a valid login when an admin
            #     has reset this user's password. Stops the temp password
            #     from quietly becoming a permanent one.
            shinyjs::hidden(div(id = "change_password_panel",
                div(class = "login-step-eye", "Choose a new password"),
                tags$h1(class = "login-title", "Set your password"),
                p(class = "login-subtitle",
                  "Your password was reset by an administrator. Pick a new one to continue — this one stays with you."),
                div(class = "login-field",
                    tags$label("New password"),
                    passwordInput("change_pw_new", label = NULL,
                                  placeholder = "At least 6 characters",
                                  width = "100%")),
                div(class = "login-field", style = "margin-top:-6px;",
                    passwordInput("change_pw_confirm", label = NULL,
                                  placeholder = "Confirm password",
                                  width = "100%")),
                uiOutput("change_pw_msg"),
                div(style = "margin-top:16px;",
                    actionButton("change_pw_submit", "Save password & continue",
                                 class = "login-btn-primary"))
            )),

            # Footer
            div(class = "login-form-foot",
                span("Profile stored locally on this computer"),
                span(style = "font-size:10px;color:var(--ov-muted2);",
                     paste0("v", packageVersion("shiny"))))
          )
        )
      ),

      # Footer band — small + unobtrusive, centred at the bottom of the page
      div(class = "login-page-foot",
          span(HTML("&copy; 2026 Birmingham Clinical Trials Unit")),
          span(class = "login-page-foot-sep", "·"),
          span("University of Birmingham")
      )
  )
}
