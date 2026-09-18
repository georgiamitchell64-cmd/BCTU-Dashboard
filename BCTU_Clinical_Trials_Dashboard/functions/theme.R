# ─────────────────────────────────────────────────────────────────────────────
# theme.R — bslib theme + brand palette
# Component styles live in www/tonic_core.css (loaded by layout.R).
# Design tokens (CSS custom properties) are declared in :root there.
# Keep the bs_theme() colours below in sync with the --navy / --teal / etc.
# values in www/tonic_core.css so Bootstrap components match the rest of
# the UI.
# ─────────────────────────────────────────────────────────────────────────────

tonic_theme <- bs_theme(
  version = 5,
  primary = "#1B1B1B",
  secondary = "#00ACA9",
  success = "#3AAA35",
  warning = "#F07F3C",
  danger = "#E30513",
  info = "#2581C4",
  bg = "#F4F4F4",
  fg = "#1B1B1B",
  # Named, not fetched: the @font-face rules at the top of www/tonic_core.css
  # point at the copies in www/fonts. font_google() would download the font
  # during the first render instead, which is what made the desktop build look
  # like it had failed to start on a slow network.
  base_font = font_collection("Outfit", "system-ui", "sans-serif"),
  heading_font = font_collection("Outfit", "system-ui", "sans-serif"),
  "card-border-radius" = "8px",
  "card-cap-bg" = "#F8FAFD",
  "card-border-color" = "#E3E3E3",
  "input-border-radius" = "5px",
  "input-border-color" = "#E3E3E3",
  "input-focus-border-color" = "#00ACA9",
  "input-focus-box-shadow" = "0 0 0 0.2rem rgba(0,172,169,.2)",
  "btn-border-radius" = "5px"
)
