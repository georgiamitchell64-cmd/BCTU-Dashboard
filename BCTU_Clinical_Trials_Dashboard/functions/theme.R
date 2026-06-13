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
  primary = "#1B4F6B",
  secondary = "#2EC4A5",
  success = "#10B981",
  warning = "#F59E0B",
  danger = "#EF4444",
  info = "#3B82F6",
  bg = "#EEF3F8",
  fg = "#1E293B",
  base_font = font_google("Outfit"),
  heading_font = font_google("Outfit", wght = "700"),
  "card-border-radius" = "8px",
  "card-cap-bg" = "#F8FAFD",
  "card-border-color" = "#DDE5EE",
  "input-border-radius" = "5px",
  "input-border-color" = "#DDE5EE",
  "input-focus-border-color" = "#2EC4A5",
  "input-focus-box-shadow" = "0 0 0 0.2rem rgba(46,196,165,.2)",
  "btn-border-radius" = "5px"
)
