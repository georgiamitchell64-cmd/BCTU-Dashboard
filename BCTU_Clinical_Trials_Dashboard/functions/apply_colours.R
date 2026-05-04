# =============================================================================
# Trial Colour Application
# =============================================================================
# Injects CSS to apply a trial's colour scheme dynamically without restart.
# =============================================================================

apply_trial_colours <- function(colors, sidebar = "dark") {
  primary   <- colors$primary   %||% "#1B4F6B"
  secondary <- colors$secondary %||% "#2EC4A5"
  accent    <- colors$accent    %||% "#F59E0B"
  sidebar   <- match.arg(sidebar, c("dark", "light"))

  sidebar_bg     <- if (sidebar == "light") "#FFFFFF" else primary
  sidebar_text   <- if (sidebar == "light") "#0F172A" else "#FFFFFF"
  sidebar_muted  <- if (sidebar == "light") "#64748B" else "rgba(255,255,255,0.7)"
  nav_bg         <- if (sidebar == "light") "transparent" else "rgba(255,255,255,.06)"
  nav_hover_bg   <- if (sidebar == "light") "#F1F5F9"   else "rgba(255,255,255,.09)"
  nav_active_bg  <- if (sidebar == "light") "#F5F3FF"   else "rgba(255,255,255,.12)"
  nav_text       <- if (sidebar == "light") "#475569"   else "rgba(255,255,255,.65)"
  nav_text_hover <- if (sidebar == "light") "#0F172A"   else "#FFFFFF"
  logo_border    <- if (sidebar == "light") "#EEF2F7"   else "rgba(255,255,255,.1)"
  logo_invert    <- if (sidebar == "light") "none"      else "brightness(0) invert(1)"

  # Generate a darker shade of secondary for hover states
  darken <- function(hex, factor = 0.85) {
    rgb <- col2rgb(hex)
    sprintf("#%02X%02X%02X",
            as.integer(rgb[1] * factor),
            as.integer(rgb[2] * factor),
            as.integer(rgb[3] * factor))
  }
  secondary_dark <- tryCatch(darken(secondary), error = function(e) secondary)

  css <- sprintf("
    :root {
      --navy: %s !important;
      --teal: %s !important;
      --teal-dk: %s !important;
      --amber: %s !important;
    }
    .sidebar,
    .bslib-sidebar-layout > .sidebar,
    .bslib-sidebar-layout[style] > .sidebar {
      background: %s !important; color: %s !important;
      border-right: 1px solid rgba(0,0,0,0.04) !important;
    }
    .sidebar-logo { border-bottom-color: %s !important; }
    .sidebar-logo img { filter: %s !important; }
    .sidebar-nav-btn {
      background: %s !important; color: %s !important;
    }
    .sidebar-nav-btn:hover {
      background: %s !important; color: %s !important;
    }
    .sidebar-nav-btn.active-nav {
      background: %s !important; color: %s !important;
    }
    .sidebar-user-name { color: %s !important; }
    .sidebar-user-role, .nav-section-label { color: %s !important; }
    .topbar { background: %s !important; }
    .sidebar-nav-btn.active-nav { border-left-color: %s !important; }
    .sidebar-nav-btn:hover { border-left-color: %s !important; }
    .sidebar-nav-btn.active-nav .fa, .sidebar-nav-btn:hover .fa { color: %s !important; }
    .card { border-top-color: %s !important; }
    .tonic-vbox { border-top-color: %s !important; }
    .section-heading { border-bottom-color: %s !important; }
    .topbar-badge { background: rgba(255,255,255,.15) !important; color: %s !important;
                    border-color: rgba(255,255,255,.25) !important; }
    .prog-fill { background: linear-gradient(90deg, %s, %s) !important; }
  ",
  primary, secondary, secondary_dark, accent,
  sidebar_bg, sidebar_text,
  logo_border, logo_invert,
  nav_bg, nav_text,
  nav_hover_bg, nav_text_hover,
  nav_active_bg, nav_text_hover,
  sidebar_text, sidebar_muted,
  primary,
  secondary, secondary, secondary,
  secondary, secondary, secondary,
  secondary,
  secondary_dark, secondary)

  # Remove any previous override and inject new one
  shinyjs::runjs("$('#trial-colour-override').remove();")
  shinyjs::runjs(sprintf(
    "$('head').append('<style id=\"trial-colour-override\">%s</style>');",
    gsub("\n", " ", gsub("'", "\\\\'", css))
  ))
}
