# =============================================================================
# Trial Colour Application
# =============================================================================
# The dashboard chrome is fixed to the University of Birmingham palette (black,
# white and gold — see www/tonic_core.css), so every trial looks clean and
# consistent. A trial's own colours are used only where they identify the
# trial: its charts and the bold section headings. They're exposed as
# --trial-primary / --trial-secondary / --trial-accent (plus soft tints) for CSS
# and the JS widgets; R-built charts use trial_palette().
# =============================================================================

.valid_hex <- function(x, default) {
  x <- trimws(as.character(x %||% ""))
  if (length(x) == 1 && grepl("^#[0-9A-Fa-f]{6}$", x)) x else default
}

# `sidebar` is accepted for compatibility with existing callers; the chrome no
# longer changes per trial, so it has no effect.
apply_trial_colours <- function(colors, sidebar = "dark") {
  pal  <- trial_palette(list(colors = colors))
  soft <- function(hex, a) {
    rgb <- grDevices::col2rgb(hex)
    sprintf("rgba(%d,%d,%d,%s)", rgb[1], rgb[2], rgb[3], a)
  }
  css <- sprintf(paste(
    ":root { --trial-primary: %s; --trial-secondary: %s; --trial-accent: %s;",
    "--trial-primary-soft: %s; --trial-secondary-soft: %s; --trial-accent-soft: %s; }"),
    pal[["primary"]], pal[["secondary"]], pal[["accent"]],
    soft(pal[["primary"]], 0.08), soft(pal[["secondary"]], 0.12), soft(pal[["accent"]], 0.14))

  # Remove any previous override and inject the new one
  shinyjs::runjs("$('#trial-colour-override').remove();")
  shinyjs::runjs(sprintf(
    "$('head').append('<style id=\"trial-colour-override\">%s</style>');", css))
}

# Trial colours for R-built charts (echarts, report figures).
trial_palette <- function(cfg = current_trial_config()) {
  c(primary   = .valid_hex(cfg$colors$primary,   "#1B1B1B"),
    secondary = .valid_hex(cfg$colors$secondary, "#00788E"),
    accent    = .valid_hex(cfg$colors$accent,    "#C59A00"))
}
