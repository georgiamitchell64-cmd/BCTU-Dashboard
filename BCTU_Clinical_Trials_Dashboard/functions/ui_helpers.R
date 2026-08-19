tonic_card <- function(..., title = NULL, amber = FALSE, tools = NULL, full_screen = FALSE) {
  header_class <- if (amber) "card-header-amber" else ""
  card(
    full_screen = full_screen,
    if (!is.null(title) || !is.null(tools)) card_header(
      class = header_class,
      div(
        style = "display:flex;align-items:center;justify-content:space-between;flex-wrap:wrap;gap:8px",
        if (!is.null(title)) span(style = "font-size:13px;font-weight:600;color:#1B4F6B", title) else NULL,
        if (!is.null(tools)) tools else NULL
      )
    ),
    ...
  )
}

# Single source of truth for Font Awesome icon tags.
# Accepts a bare FA name ("hospital"), an explicit class ("fa-solid fa-hospital"),
# or a Shiny icon() result — returns a normalised <i> tag with the fixed-width
# modifier. Use this anywhere you'd previously call tags$i(class = "fa-...") or
# shiny::icon(): keeps look + DOM consistent across all modules.
fa_icon <- function(name, style = "solid", fw = TRUE) {
  if (inherits(name, "shiny.tag")) return(name)
  cls <- if (grepl("^fa-", name, fixed = FALSE)) name else sprintf("fa-%s fa-%s", style, name)
  if (fw && !grepl("fa-fw", cls)) cls <- paste(cls, "fa-fw")
  tags$i(class = cls)
}

vbox_html <- function(icon_class, label, value_id, sub, top_color = "#2EC4A5", delta_id = NULL, icon_bg = "background:#E8F0F5;color:#1B4F6B") {
  delta_span <- if (!is.null(delta_id)) span(uiOutput(delta_id, inline = TRUE)) else NULL
  div(
    class = "tonic-vbox",
    style = paste0("border-top-color:", top_color),
    div(class = "tonic-vbox-icon", style = icon_bg, fa_icon(icon_class)),
    div(class = "tonic-vbox-label", label),
    div(style = "display:flex;align-items:baseline;gap:6px", div(class = "tonic-vbox-value", textOutput(value_id, inline = TRUE)), delta_span),
    div(class = "tonic-vbox-sub", sub)
  )
}

nav_btn <- function(id, label, fa_class) {
  actionButton(id, icon = fa_icon(fa_class), label, class = "sidebar-nav-btn", width = "100%")
}

status_pill_html <- function(status) {
  cls <- switch(status,
    Recruiting = "sp-r", Open = "sp-o", `Set-up` = "sp-s",
    Closed = "sp-c", "sp-i"
  )
  sprintf("<span class='spill %s'>&#x25CF; %s</span>", cls, status)
}

prog_bar_html <- function(actual, target) {
  # Sites may have no target set (blank on a manually added site), so guard
  # every arithmetic path against NA rather than printing "NA" into the bar.
  actual  <- as.integer(ifelse(is.na(actual), 0L, actual))
  has_tgt <- !is.na(target) & target > 0
  pct     <- as.integer(ifelse(has_tgt, pmin(100, round(100 * actual / pmax(target, 1))), 0))
  tgt_lbl <- ifelse(has_tgt, as.character(target), "\u2014")
  sprintf('<div class="prog-wrap"><div class="prog-track"><div class="prog-fill" style="width:%d%%"></div></div><span class="prog-lbl">%d/%s</span></div>',
          pct, actual, tgt_lbl)
}

empty_reactable <- function(msg = "No data") {
  reactable::reactable(
    data.frame(Message = msg),
    columns = list(
      Message = reactable::colDef(
        minWidth = 300,
        style = list(color = "#94A3B8", fontStyle = "italic",
                     fontFamily = "Outfit, sans-serif", fontSize = "12px")
      )
    ),
    bordered = FALSE, highlight = FALSE, compact = TRUE
  )
}

empty_echart <- function(msg = "No data") {
  # A bare e_charts() (no series) makes echarts' renderer throw
  # "Cannot read properties of undefined (reading 'group')" in the browser,
  # so plot a single transparent bar to give it something to initialise with.
  data.frame(x = "—", y = 0) %>%
    e_charts(x) %>%
    e_bar(y, legend = FALSE, itemStyle = list(color = "transparent")) %>%
    e_title(subtext = msg, subtextStyle = list(color = col_muted, fontFamily = "Outfit", fontSize = 13)) %>%
    e_x_axis(show = FALSE) %>%
    e_y_axis(show = FALSE) %>%
    e_legend(show = FALSE) %>%
    e_tooltip(show = FALSE) %>%
    e_grid(top = "30%")
}
