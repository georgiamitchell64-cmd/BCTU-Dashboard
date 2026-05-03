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

vbox_html <- function(icon_class, label, value_id, sub, top_color = "#2EC4A5", delta_id = NULL, icon_bg = "background:#E8F0F5;color:#1B4F6B") {
  delta_span <- if (!is.null(delta_id)) span(uiOutput(delta_id, inline = TRUE)) else NULL
  div(
    class = "tonic-vbox",
    style = paste0("border-top-color:", top_color),
    div(class = "tonic-vbox-icon", style = icon_bg, tags$i(class = icon_class)),
    div(class = "tonic-vbox-label", label),
    div(style = "display:flex;align-items:baseline;gap:6px", div(class = "tonic-vbox-value", textOutput(value_id, inline = TRUE)), delta_span),
    div(class = "tonic-vbox-sub", sub)
  )
}

nav_btn <- function(id, label, fa_class) {
  actionButton(id, icon = tags$i(class = paste("fa-solid fa-fw", fa_class)), label, class = "sidebar-nav-btn", width = "100%")
}

status_pill_html <- function(status) {
  cls <- switch(status,
    Recruiting = "sp-r", Open = "sp-o", `Set-up` = "sp-s",
    Closed = "sp-c", "sp-i"
  )
  sprintf("<span class='spill %s'>&#x25CF; %s</span>", cls, status)
}

prog_bar_html <- function(actual, target) {
  pct <- pmin(100, ifelse(target > 0, round(100 * actual / target), 0))
  sprintf('<div class="prog-wrap"><div class="prog-track"><div class="prog-fill" style="width:%d%%"></div></div><span class="prog-lbl">%d/%d</span></div>',
          pct, actual, target)
}

empty_reactable <- function(msg = "No data") {
  reactable(data.frame(Message = msg), columns = list(Message = colDef(minWidth = 300)))
}

empty_echart <- function(msg = "No data") {
  e_charts() %>%
    e_title(subtext = msg, subtextStyle = list(color = col_muted, fontFamily = "Outfit", fontSize = 13)) %>%
    e_grid(top = "30%")
}
