# =============================================================================
# Inline help tooltips
# =============================================================================
# CSS-only tooltips (no extra deps). Use:
#   help_tip("Explanation text…")             → "?" icon with hover popup
#   help_label("Site name", "Used for…")      → label + inline ?
#   tooltip_styles()                          → include once in the UI tree
# =============================================================================

# Include this once at the top of the app UI (e.g. in build_app_ui or the
# home / in-trial layout root).
tooltip_styles <- function() {
  tags$style(HTML("
    .htip {
      display: inline-flex; align-items: center; justify-content: center;
      width: 14px; height: 14px; margin-left: 6px;
      border-radius: 50%; background: #E2E8F0; color: #475569;
      font-size: 9.5px; font-weight: 700; cursor: help;
      position: relative; vertical-align: middle;
      font-family: system-ui, sans-serif;
    }
    .htip:hover { background: #CBD5E1; color: #0F172A; }
    .htip .htip-body {
      display: none; position: absolute; bottom: calc(100% + 6px); left: 50%;
      transform: translateX(-50%);
      background: #0F172A; color: #FFFFFF;
      font-size: 11.5px; font-weight: 400; line-height: 1.5;
      padding: 8px 11px; border-radius: 6px;
      width: 240px; max-width: 80vw; white-space: normal; text-align: left;
      z-index: 2000;
      box-shadow: 0 6px 18px rgba(0,0,0,0.18);
      pointer-events: none;
    }
    .htip:hover .htip-body { display: block; }
    .htip .htip-body::after {
      content: ''; position: absolute; top: 100%; left: 50%;
      transform: translateX(-50%);
      border: 5px solid transparent; border-top-color: #0F172A;
    }
  "))
}

#' Build a small "?" icon with a hover tooltip.
#' @param text Plain-text or HTML content (will be htmlEscape'd if plain).
#' @param html If TRUE, treat `text` as raw HTML.
help_tip <- function(text, html = FALSE) {
  body <- if (isTRUE(html)) HTML(text) else htmltools::htmlEscape(text)
  tags$span(class = "htip",
            "?",
            tags$span(class = "htip-body", body))
}

#' Build a styled label that includes an inline help tooltip.
help_label <- function(label, tip) {
  tagList(label, help_tip(tip))
}
