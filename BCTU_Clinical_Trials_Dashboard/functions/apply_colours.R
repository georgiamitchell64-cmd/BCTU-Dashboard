# =============================================================================
# Trial Colour Application
# =============================================================================
# Injects CSS to apply a trial's colour scheme dynamically without restart.
# =============================================================================

apply_trial_colours <- function(colors) {
  primary   <- colors$primary   %||% "#1B4F6B"
  secondary <- colors$secondary %||% "#2EC4A5"
  accent    <- colors$accent    %||% "#F59E0B"

  # Generate a darker shade of secondary for hover states
  # (simple approximation — works for hex colours)
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
    .sidebar, .bslib-sidebar-layout > .sidebar { background: %s !important; }
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
  primary, primary,
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
