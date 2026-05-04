welcome_screen_ui <- function() {
  div(id = "welcome_screen",
      style = "position:fixed;top:0;left:0;right:0;bottom:0;z-index:9999;
               display:flex;align-items:center;justify-content:center;
               background:linear-gradient(135deg,#312E81 0%,#4338CA 25%,#6366F1 50%,#8B5CF6 75%,#A78BFA 100%);
               background-size:400% 400%;animation:welcomeGrad 22s ease infinite;",

      tags$style(HTML("
        @keyframes welcomeGrad { 0%,100%{background-position:0% 50%} 50%{background-position:100% 50%} }
      ")),

      div(style = "background:rgba(255,252,247,0.97);backdrop-filter:blur(24px);
                   border:1.5px solid rgba(255,255,255,0.6);border-radius:28px;
                   box-shadow:0 30px 80px rgba(14,53,73,0.30);
                   padding:42px;width:440px;max-width:92vw;text-align:center;",

          # Logo icon
          div(style = "width:78px;height:78px;margin:0 auto 16px;border-radius:22px;
                       background:linear-gradient(135deg,#4338CA,#8B5CF6);
                       display:flex;align-items:center;justify-content:center;",
              HTML('<svg width="40" height="40" viewBox="0 0 24 24" fill="none" stroke="white"
                    stroke-width="2.3" stroke-linecap="round" stroke-linejoin="round">
                    <path d="M12 2L2 7l10 5 10-5-10-5z"/><path d="M2 17l10 5 10-5"/>
                    <path d="M2 12l10 5 10-5"/></svg>')),

          div(style = "font-size:22px;font-weight:700;color:#1B4F6B;margin-bottom:4px;",
              "Clinical Trials Dashboard"),
          div(style = "font-size:14px;color:#6c7a86;margin-bottom:28px;",
              "Birmingham Clinical Trials Unit"),

          # ── Returning user panel ────────────────────────────────────
          uiOutput("welcome_profiles_ui"),

          # ── New user panel (hidden initially if profiles exist) ────
          div(id = "new_user_panel",
              div(style = "font-size:13px;font-weight:600;color:#1B4F6B;margin-bottom:12px;
                           text-align:left;", "Create your profile"),
              div(style = "text-align:left;margin-bottom:12px;",
                  div(style = "font-size:11px;font-weight:600;color:#1B4F6B;
                               text-transform:uppercase;letter-spacing:.5px;margin-bottom:4px;",
                      "Your full name"),
                  textInput("welcome_name", label = NULL,
                            placeholder = "e.g. Georgia Mitchell",
                            width = "100%")
              ),
              div(style = "text-align:left;margin-bottom:12px;",
                  div(style = "font-size:11px;font-weight:600;color:#1B4F6B;
                               text-transform:uppercase;letter-spacing:.5px;margin-bottom:4px;",
                      "Choose a password"),
                  passwordInput("welcome_password", label = NULL,
                                placeholder = "At least 6 characters",
                                width = "100%"),
                  passwordInput("welcome_password_confirm", label = NULL,
                                placeholder = "Confirm password",
                                width = "100%")
              ),
              div(style = "text-align:left;margin-bottom:16px;",
                  tags$label(style = "font-size:11px;font-weight:600;color:#1B4F6B;
                                      text-transform:uppercase;letter-spacing:.5px;",
                             "Your role"),
                  div(style = "display:grid;grid-template-columns:1fr 1fr;gap:8px;margin-top:6px;",
                      actionButton("role_tm",    "Trial Manager",
                                   class = "btn role-pick-btn active-role",
                                   style = "width:100%;"),
                      actionButton("role_ci",    "CI / Investigator",
                                   class = "btn role-pick-btn",
                                   style = "width:100%;"),
                      actionButton("role_tl",    "Team Leader",
                                   class = "btn role-pick-btn",
                                   style = "width:100%;"),
                      actionButton("role_guest", "Guest / Read-only",
                                   class = "btn role-pick-btn",
                                   style = "width:100%;")
                  )
              ),
              actionButton("welcome_go", "Get started",
                           class = "btn",
                           style = "width:100%;padding:14px;border:none;border-radius:14px;
                                    font-weight:600;font-size:15px;color:#fff;cursor:pointer;
                                    background:linear-gradient(135deg,#4338CA,#6366F1,#8B5CF6);
                                    background-size:200% 200%;animation:welcomeGrad 6s ease infinite;
                                    font-family:'Outfit',sans-serif;")
          ),

          # ── Password prompt panel (shown after picking a returning profile) ──
          shinyjs::hidden(div(id = "password_panel",
              div(style = "text-align:left;margin-bottom:8px;",
                  span(style = "font-size:13px;font-weight:600;color:#1B4F6B;",
                       textOutput("password_prompt_name", inline = TRUE))),
              div(style = "text-align:left;margin-bottom:12px;",
                  div(style = "font-size:11px;font-weight:600;color:#1B4F6B;
                               text-transform:uppercase;letter-spacing:.5px;margin-bottom:4px;",
                      "Password"),
                  passwordInput("login_password", label = NULL,
                                placeholder = "Enter password",
                                width = "100%")),
              uiOutput("login_set_password_panel"),
              div(style = "display:grid;grid-template-columns:1fr 1fr;gap:8px;",
                  actionButton("login_back", "← Back",
                               class = "btn",
                               style = "padding:12px;border:1.5px solid #d0dde6;
                                        background:#fff;color:#475569;border-radius:12px;
                                        font-weight:500;font-family:'Outfit',sans-serif;"),
                  actionButton("login_go", "Sign in",
                               class = "btn",
                               style = "padding:12px;border:none;border-radius:12px;
                                        font-weight:600;color:#fff;
                                        background:linear-gradient(135deg,#4338CA,#8B5CF6);
                                        font-family:'Outfit',sans-serif;"))
          )),

          div(style = "margin-top:22px;font-size:12px;color:#9aa3ad;",
              "Your profile is stored locally on this computer.")
      ),

      # ── Role button styling ──────────────────────────────────────
      tags$style(HTML("
        .role-pick-btn {
          padding:10px 12px !important; border-radius:10px !important;
          border:1.5px solid #d0dde6 !important; background:#fff !important;
          color:#475569 !important; font-size:12px !important; font-weight:500 !important;
          font-family:'Outfit',sans-serif !important; transition:all .15s !important;
        }
        .role-pick-btn:hover { border-color:#8B5CF6 !important; color:#4338CA !important; }
        .role-pick-btn.active-role {
          border-color:#8B5CF6 !important; background:#F5F3FF !important;
          color:#4338CA !important; font-weight:600 !important;
          box-shadow:0 0 0 2px rgba(139,92,246,0.2) !important;
        }
        #welcome_screen .form-group { margin-bottom:0 !important; }
        #welcome_screen .form-control {
          padding:12px 14px !important; border:1.5px solid #d0dde6 !important;
          border-radius:12px !important; font-family:'Outfit',sans-serif !important;
          font-size:14px !important;
        }
        #welcome_screen .form-control:focus {
          border-color:#8B5CF6 !important;
          box-shadow:0 0 0 3px rgba(139,92,246,0.15) !important;
        }
      "))
  )
}
