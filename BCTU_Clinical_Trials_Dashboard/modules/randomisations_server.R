randomisations_server <- function(input, output, session, state) {
  rv <- state$rv

  output$rand_table <- renderReactable({
    df <- rv$sites
    if (nrow(df) == 0) return(empty_reactable("No sites loaded."))
    df <- df %>%
      mutate(
        btn_html = paste0(
          '<div class="rand-cell">',
          '<button class="rbtn rbtn-minus" onclick="Shiny.setInputValue(\'rand_minus\',\'', site_id, '\',{priority:\'event\'})">&#x2212;</button>',
          '<span class="rnum">', randomised, '</span>',
          '<button class="rbtn" onclick="Shiny.setInputValue(\'rand_plus\',\'', site_id, '\',{priority:\'event\'})">+</button>',
          '</div>'),
        status_html = vapply(status, status_pill_html, character(1)),
        prog_html   = prog_bar_html(randomised, target)
      )
    reactable(
      df %>% select(site_id, site_name, status_html, btn_html, prog_html),
      striped = TRUE, highlight = TRUE, compact = TRUE,
      defaultColDef = colDef(style = list(fontFamily = "Outfit", fontSize = "13px")),
      columns = list(
        site_id     = colDef(name = "Site ID", minWidth = 90,
                             cell = function(v) htmltools::span(class = "sid", v)),
        site_name   = colDef(name = "Site", minWidth = 180),
        status_html = colDef(name = "Status", html = TRUE, minWidth = 120),
        btn_html    = colDef(name = "Randomisations +/\u2212", html = TRUE, minWidth = 160, align = "center"),
        prog_html   = colDef(name = "Progress", html = TRUE, minWidth = 180)
      )
    )
  })

  observeEvent(input$rand_plus, {
    sid <- input$rand_plus
    idx <- which(rv$sites$site_id == sid)
    if (!length(idx)) return()
    rv$log <- bind_rows(rv$log, tibble(timestamp = Sys.time(), site_id = sid, action = "+1", note = "manual entry"))
    rv$sites$randomised[idx] <- rv$sites$randomised[idx] + 1L
    if (is.na(rv$sites$site_open_date[idx]))
      rv$sites$site_open_date[idx] <- Sys.Date()
  })

  observeEvent(input$rand_minus, {
    sid <- input$rand_minus
    idx <- which(rv$sites$site_id == sid)
    if (!length(idx) || rv$sites$randomised[idx] <= 0L) return()
    rv$log <- bind_rows(rv$log, tibble(timestamp = Sys.time(), site_id = sid, action = "-1", note = "manual entry"))
    rv$sites$randomised[idx] <- rv$sites$randomised[idx] - 1L
  })

  observeEvent(input$add_backdate, {
    req(input$bd_site)
    sid <- input$bd_site
    idx <- which(rv$sites$site_id == sid)
    rv$log <- bind_rows(rv$log, tibble(
      timestamp = as.POSIXct(input$bd_date),
      site_id = sid, action = "+1", note = input$bd_note))
    if (length(idx)) rv$sites$randomised[idx] <- rv$sites$randomised[idx] + 1L
    showNotification(paste0("Randomisation added for ", sid, " on ",
                            format(input$bd_date, "%d %b %Y")), type = "message")
    updateTextInput(session, "bd_note", value = "")
  })

  output$log_table <- renderReactable({
    if (nrow(rv$log) == 0) return(empty_reactable("No activity yet"))
    rv$log %>% arrange(desc(timestamp)) %>%
      mutate(timestamp = format(timestamp, "%d %b %Y  %H:%M")) %>%
      select(Timestamp = timestamp, `Site ID` = site_id, Action = action, Note = note) %>%
      reactable(striped = TRUE, highlight = TRUE, compact = TRUE,
                defaultColDef = colDef(style = list(fontFamily = "Outfit", fontSize = "12.5px")),
                columns = list(
                  `Site ID` = colDef(cell = function(v) htmltools::span(class = "sid", v)),
                  Action = colDef(minWidth = 70, cell = function(v)
                    htmltools::HTML(if (v == "+1")
                      "<span style='color:#059669;font-weight:700'>+1</span>"
                      else "<span style='color:#DC2626;font-weight:700'>\u22121</span>"))
                ))
  })
}
