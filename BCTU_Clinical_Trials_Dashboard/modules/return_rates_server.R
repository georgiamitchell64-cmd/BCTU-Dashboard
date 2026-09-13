# ── return_rates_server.R ─────────────────────────────────────────────────────
#
# The Returns tab: how many of the forms that have fallen due have come back —
# overall, by timepoint, over time, by site and by form — and who still owes one.
#
# Two sources (functions/return_rates_data.R), switched in the header when a
# trial has both:
#   "file"    the newest return-rate CSV from the export pipeline; the earlier
#             files in its folder are drawn as a trend
#   "redcap"  worked out from the loaded REDCap export by the trial-health
#             engine, which also splits site CRFs from questionnaires and lists
#             the participants with forms outstanding
#
# Rates are always entered / due, never entered / expected: "expected" counts
# forms whose window hasn't opened yet (Day 90 before anyone reaches Day 90),
# which would drag every rate down. Entered is capped at due on each row so
# forms entered early never push a rate past 100%.
#
# Colours (rr_band): >= 90% on target, >= 70% caution, below that chase;
# grey when nothing is due yet. Styles: www/return_rates.css.
# ─────────────────────────────────────────────────────────────────────────────

return_rates_server <- function(id, rr_data, health = reactive(NULL),
                                trial_code = reactive(NULL)) {
  # rr_data: reactive returning the newest return-rate file (load_return_rates)
  # health:  the shared th_build() reactive (state$health)

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Errors (and req() stops) upstream mean "not available", not a crash
    quiet <- function(expr) tryCatch(expr, error = function(e) NULL)

    # 99.6% shows as 99%, never as a rounded-up 100%
    fmt_pct <- function(p, digits = 0) {
      r <- round(p, digits)
      r <- ifelse(!is.na(p) & p < 100 & r >= 100, 100 - 10^-digits, r)
      ifelse(is.na(p), "—", paste0(formatC(r, format = "f", digits = digits), "%"))
    }
    fmt_n <- function(x) formatC(round(x), format = "d", big.mark = ",")
    js_str <- function(x) jsonlite::toJSON(as.character(x), auto_unbox = TRUE)
    set_input <- function(name, value_js)
      sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", ns(name), value_js)

    # ── Sources ────────────────────────────────────────────────────────────
    file_df <- reactive(quiet(rr_data()))
    H       <- reactive(quiet(health()))
    red_df  <- reactive(rr_from_health(H()))
    have    <- reactive(c(file = !is.null(file_df()), redcap = !is.null(red_df())))
    # Example files (made-up figures) never take the place of the real export
    is_example <- reactive(isTRUE(attr(file_df(), "example")))

    src <- reactive({
      h <- have(); pick <- input$source %||% ""
      if (pick %in% names(h)[h]) pick
      else if (h[["file"]] && !is_example()) "file"
      else if (h[["redcap"]]) "redcap" else if (h[["file"]]) "file" else "none"
    })
    base <- reactive(switch(src(), file = file_df(), redcap = red_df(), NULL))

    # ── Filters: timepoint chips and form type ─────────────────────────────
    tps_all <- reactive({ d <- base(); if (is.null(d)) character() else rr_tp_order(d$timepoint) })
    tp_off  <- reactiveVal(character())
    observeEvent(tps_all(), tp_off(intersect(tp_off(), tps_all())))
    observeEvent(input$tp_toggle, {
      tp <- input$tp_toggle$tp; off <- tp_off()
      if (tp %in% off) tp_off(setdiff(off, tp))
      else if (length(setdiff(tps_all(), c(off, tp)))) tp_off(c(off, tp))  # keep one on
    })

    has_kind <- reactive({
      d <- base()
      !is.null(d) && length(unique(d$kind[!is.na(d$kind)])) > 1
    })
    kind_sel <- reactive({
      k <- input$kind %||% "all"
      if (has_kind() && k %in% c("CRF", "PROM")) k else "all"
    })

    dat <- reactive({
      d <- base(); if (is.null(d)) return(NULL)
      d <- d[d$timepoint %in% setdiff(tps_all(), tp_off()), , drop = FALSE]
      if (kind_sel() != "all") d <- d[d$kind %in% kind_sel(), , drop = FALSE]
      if (nrow(d)) d else NULL
    })
    tps <- reactive({ d <- dat(); if (is.null(d)) character() else intersect(tps_all(), unique(d$timepoint)) })
    ov  <- reactive({ d <- dat(); if (is.null(d)) NULL else d[d$site == ".Overall", , drop = FALSE] })
    st  <- reactive({ d <- dat(); if (is.null(d)) NULL else d[d$site != ".Overall", , drop = FALSE] })
    site_list <- reactive({ s <- st(); if (is.null(s) || !nrow(s)) character() else sort(unique(s$site)) })

    # reactiveVals only invalidate when the value changes, so the page frame
    # and the site pickers aren't rebuilt by every 5-minute file refresh
    mode <- reactiveVal("none")
    observe(mode(paste(src(), have()[["redcap"]])))
    sites_rv <- reactiveVal(character())
    observe(sites_rv(site_list()))

    # ── Header: source and switch ──────────────────────────────────────────
    output$source_line <- renderUI({
      s <- src()
      if (s == "file") {
        d   <- file_df()
        at  <- attr(d, "exported_at") %||% attr(d, "file_mtime")
        age <- if (length(at) && !is.na(at)) as.numeric(difftime(Sys.time(), at, units = "days")) else NA
        if (is_example())
          return(div(class = "rt-src",
            span(class = "rt-src-chip example", "Example data"),
            span("made-up figures to show how this tab works · a real return-rate file in the folder replaces them")))
        stale <- !is.na(age) && age > 14
        div(class = "rt-src",
            span(class = paste("rt-src-chip", if (stale) "stale"), "Return-rate file"),
            span(attr(d, "source_file")),
            if (!is.na(age)) span(paste("exported", format(at, "%d %b %Y, %H:%M"))),
            if (stale) span(class = "rt-stale",
                            sprintf("%d days old — check the export is still running", floor(age))))
      } else if (s == "redcap") {
        g <- H()$settings$crf$grace_days %||% 14
        div(class = "rt-src",
            span(class = "rt-src-chip", "REDCap export"),
            span(sprintf("worked out from the loaded export · due once the visit has passed, overdue %s days after", g)))
      }
    })

    output$source_pick <- renderUI({
      if (!all(have())) return(NULL)
      s <- src()
      btn <- function(v, l) tags$button(type = "button", class = if (s == v) "on",
                                        onclick = set_input("source", js_str(v)), l)
      div(class = "th-pills", title = "Where the figures come from",
          btn("file", if (is_example()) "Example file" else "Return-rate file"),
          btn("redcap", "REDCap export"))
    })

    output$filters <- renderUI({
      all <- tps_all(); if (!length(all)) return(NULL)
      off <- tp_off()
      chips <- lapply(all, function(tp) {
        on <- !tp %in% off
        tags$button(type = "button", class = paste("rt-chip", if (on) "on"),
                    `aria-pressed` = tolower(on),
                    onclick = set_input("tp_toggle", sprintf("{tp: %s, n: Math.random()}", js_str(tp))), tp)
      })
      k <- kind_sel()
      kbtn <- function(v, l) tags$button(type = "button", class = if (k == v) "on",
                                         onclick = set_input("kind", js_str(v)), l)
      div(class = "rt-filters",
          div(class = "rt-fgroup", span(class = "rt-flabel", "Timepoints"), div(class = "rt-chips", chips)),
          if (has_kind())
            div(class = "rt-fgroup", span(class = "rt-flabel", "Forms"),
                div(class = "th-pills", kbtn("all", "All forms"), kbtn("CRF", "Site CRFs"),
                    kbtn("PROM", "Questionnaires"))))
    })

    # ── Page frame ─────────────────────────────────────────────────────────
    section <- function(title, sub, body, tools = NULL)
      div(class = "th-section",
          div(class = "th-section-head", div(tags$h3(title), div(class = "th-sub-t", sub)), tools),
          body)

    output$page <- renderUI({
      m <- strsplit(mode(), " ", fixed = TRUE)[[1]]
      if (m[1] == "none")
        return(div(class = "rt-empty",
          tags$h3("No return rates to show yet"),
          tags$p(paste("There's no return-rate file for this trial and no REDCap export loaded.",
                       "Upload an export on the Upload tab to work the rates out from REDCap,",
                       "or set the folder the return-rate files are saved to in Settings → Trial settings."))))
      tagList(
        uiOutput(ns("kpis")),
        section("By timepoint",
                "Forms returned out of those due at each timepoint. The grey figure is how much of the timepoint has fallen due so far.",
                uiOutput(ns("tp_cards"))),
        uiOutput(ns("trend_ui")),
        section("Sites",
                "Each site's return rate across the selected timepoints. Click a site to see its forms.",
                uiOutput(ns("site_table")), uiOutput(ns("site_tools"))),
        section("Forms",
                "Which forms are coming back and which aren't, at each timepoint.",
                uiOutput(ns("form_matrix")),
                div(class = "th-toolbar", uiOutput(ns("form_scope_ui")))),
        if (identical(m[2], "TRUE"))
          section("Participants with forms to return",
                  "From the REDCap export. Due now means the visit has passed and the form is inside its grace period; overdue means it's past it. Click a row for the participant's full CRF history.",
                  tagList(reactableOutput(ns("outstanding")),
                          div(class = "th-gfoot", "~ visit date estimated: the operation or discharge date isn't in the export yet.")),
                  div(class = "th-toolbar",
                      uiOutput(ns("out_site_ui"), inline = TRUE),
                      checkboxGroupInput(ns("out_status"), NULL, inline = TRUE,
                                         choices = c("Overdue" = "overdue", "Due now" = "due"),
                                         selected = c("overdue", "due")),
                      downloadButton(ns("dl_outstanding"), HTML("&darr; Download"), class = "btn-ghost-sm")))
      )
    })

    # ── Shared pieces ──────────────────────────────────────────────────────
    mark <- function() span(class = "rt-mark", style = sprintf("left:%d%%", RR_GOOD))
    fill <- function(p) if (!is.na(p)) tags$i(class = paste0("rt-f-", rr_band(p)), style = sprintf("width:%.1f%%", p))

    key_ui <- function() div(class = "rt-key",
      span(tags$i(class = "rt-f-good"), sprintf("%d%% or more", RR_GOOD)),
      span(tags$i(class = "rt-f-warn"), sprintf("%d–%d%%", RR_WARN, RR_GOOD - 1)),
      span(tags$i(class = "rt-f-bad"), sprintf("Under %d%%", RR_WARN)),
      span(tags$i(class = "rt-f-none"), "Nothing due yet"),
      span(tags$i(class = "mk"), sprintf("%d%% target", RR_GOOD)))

    rate_cell <- function(x) {
      if (is.null(x) || !nrow(x)) return(span(class = "rt-cell blank", "·"))
      if (x$due <= 0) return(span(class = "rt-cell none",
                                  title = sprintf("%s expected, none due yet", fmt_n(x$expected)), "Not due"))
      span(class = paste("rt-cell", rr_band(x$pct)),
           title = sprintf("%s of %s due returned · %s missing", fmt_n(x$entered), fmt_n(x$due), fmt_n(x$missing)),
           fmt_pct(x$pct), tags$small(sprintf("%s/%s", fmt_n(x$entered), fmt_n(x$due))))
    }
    rate_bar <- function(r)
      div(class = "rt-rate",
          div(class = "rt-rate-bar", fill(r$pct), mark()),
          span(class = paste0("rt-rate-v rt-t-", rr_band(r$pct)), fmt_pct(r$pct)))
    count_td <- function(v) tags$td(class = "r",
      if (isTRUE(v > 0)) span(class = "rt-miss", fmt_n(v)) else span(class = "rt-zero", "0"))

    # Forms x timepoints for one site, or the whole trial
    form_matrix <- function(d, tp) {
      if (is.null(d) || !nrow(d)) return(th_empty_note("No forms at these timepoints."))
      f   <- rr_summarise(d, c("form", "timepoint"))
      tot <- rr_summarise(d, "form")
      prom <- unique(d$form[d$kind %in% "PROM"])
      forms <- unique(d$form[order(match(d$timepoint, tp), seq_len(nrow(d)))])
      rows <- lapply(forms, function(fm) {
        t <- tot[tot$form == fm, , drop = FALSE]
        tags$tr(
          tags$td(class = "l", span(class = "rt-form", fm), if (fm %in% prom) span(class = "rt-kind", "Questionnaire")),
          lapply(tp, function(x) tags$td(rate_cell(f[f$form == fm & f$timepoint == x, , drop = FALSE]))),
          tags$td(class = "r", span(class = paste0("rt-rate-v rt-t-", rr_band(t$pct)), fmt_pct(t$pct))),
          count_td(t$missing))
      })
      div(class = "rt-table-wrap",
          tags$table(class = "rt-table",
                     tags$thead(tags$tr(tags$th(class = "l", "Form"), lapply(tp, tags$th),
                                        tags$th(class = "r", "Rate"), tags$th(class = "r", "Missing"))),
                     tags$tbody(rows)))
    }

    # ── KPIs ───────────────────────────────────────────────────────────────
    output$kpis <- renderUI({
      o <- ov(); if (is.null(o) || !nrow(o)) return(NULL)
      a  <- rr_summarise(o)
      tp <- rr_summarise(o, "timepoint");           tp <- tp[tp$due > 0, , drop = FALSE]
      fm <- rr_summarise(o, c("timepoint", "form")); fm <- fm[fm$due > 0, , drop = FALSE]
      ss <- rr_summarise(st(), "site")
      if (!is.null(ss)) ss <- ss[ss$due > 0, , drop = FALSE]
      low_tp <- if (nrow(tp)) tp[order(tp$pct, -tp$missing), , drop = FALSE][1, ] else NULL
      low_fm <- if (nrow(fm)) fm[order(fm$pct, -fm$missing), , drop = FALSE][1, ] else NULL
      n_low  <- if (!is.null(ss) && nrow(ss)) sum(ss$pct < RR_WARN) else NA
      grace  <- H()$settings$crf$grace_days %||% 14

      kpi <- function(l, v, s, cls = NULL, extra = NULL, txt = FALSE)
        div(class = paste("th-kpi", cls), div(class = "th-kpi-l", l),
            div(class = paste("th-kpi-v", if (txt) "rt-txt"), v), extra, div(class = "th-kpi-s", s))
      band_cls <- function(p) paste0("rt-", rr_band(p))

      div(class = "th-kpis",
        kpi("Return rate", fmt_pct(a$pct, 1),
            sprintf("%s of %s forms due have come back", fmt_n(a$entered), fmt_n(a$due)),
            band_cls(a$pct), div(class = "rt-kpi-bar", fill(a$pct), mark())),
        kpi("Still to return", fmt_n(a$missing),
            if (!is.na(a$overdue)) sprintf("%s past the %s-day grace period", fmt_n(a$overdue), grace)
            else "forms due but not yet entered"),
        kpi("Lowest timepoint", if (is.null(low_tp)) "—" else low_tp$timepoint,
            if (is.null(low_tp)) "nothing due yet"
            else sprintf("%s returned · %s missing", fmt_pct(low_tp$pct), fmt_n(low_tp$missing)),
            if (!is.null(low_tp)) band_cls(low_tp$pct), txt = TRUE),
        kpi("Lowest form", if (is.null(low_fm)) "—" else low_fm$form,
            if (is.null(low_fm)) "nothing due yet"
            else sprintf("%s at %s · %s missing", fmt_pct(low_fm$pct), low_fm$timepoint, fmt_n(low_fm$missing)),
            if (!is.null(low_fm)) band_cls(low_fm$pct), txt = TRUE),
        kpi(sprintf("Sites under %d%%", RR_WARN), if (is.na(n_low)) "—" else n_low,
            if (is.na(n_low)) "no site rows in this data"
            else sprintf("of %d site%s with forms due", nrow(ss), if (nrow(ss) == 1) "" else "s"),
            if (isTRUE(n_low > 0)) "rt-bad"),
        kpi("Fallen due so far", fmt_pct(if (a$expected > 0) 100 * a$due / a$expected else NA),
            sprintf("%s of the %s forms expected in total", fmt_n(a$due), fmt_n(a$expected))))
    })

    # ── By timepoint ───────────────────────────────────────────────────────
    output$tp_cards <- renderUI({
      o <- ov(); if (is.null(o) || !nrow(o)) return(NULL)
      t <- rr_summarise(o, "timepoint")
      t <- t[match(tps(), t$timepoint), , drop = FALSE]
      cards <- lapply(seq_len(nrow(t)), function(i) {
        r <- t[i, ]; b <- rr_band(r$pct)
        div(class = "rt-tp",
          div(class = "rt-tp-h", span(class = "rt-tp-n", r$timepoint),
              span(class = "rt-tp-open", title = "How much of this timepoint has fallen due so far",
                   sprintf("%s due so far", fmt_pct(if (r$expected > 0) 100 * r$due / r$expected else NA)))),
          if (is.na(r$pct)) div(class = "rt-tp-v none", "Not due yet")
          else div(class = paste0("rt-tp-v rt-t-", b), fmt_pct(r$pct, 1)),
          div(class = "rt-tp-bar", fill(r$pct), mark()),
          div(class = "rt-tp-s",
              span(HTML(sprintf("<b>%s</b> of %s due", fmt_n(r$entered), fmt_n(r$due)))),
              if (r$missing > 0) span(class = "rt-tp-miss", sprintf("%s missing", fmt_n(r$missing)))
              else span("none missing")))
      })
      tagList(div(class = "rt-tps", `data-chart` = "timepoints", cards), key_ui())
    })

    # ── Over time (return-rate files only) ─────────────────────────────────
    trend_svg <- function(h, tps) {
      at  <- sort(unique(h$at))
      all <- do.call(rbind, lapply(at, function(t) {
        x <- h[h$at == t, , drop = FALSE]
        data.frame(at = t, due = sum(x$due), entered = sum(x$entered))
      }))
      all$pct <- rr_pct(all$entered, all$due)
      ok <- !is.na(all$pct)
      if (sum(ok) < 2) return(NULL)

      # Drawn about as wide as it shows on a laptop, so its text matches the page
      W <- 1080; Ht <- 270; L <- 46; R <- 140; T <- 16; B <- 34
      x0 <- as.numeric(min(at)); x1 <- as.numeric(max(at))
      sx <- function(t) L + (as.numeric(t) - x0) / max(1, x1 - x0) * (W - L - R)
      lo  <- suppressWarnings(min(c(h$pct, all$pct), na.rm = TRUE))
      ylo <- if (is.finite(lo)) max(0, floor((min(lo, RR_WARN) - 5) / 10) * 10) else 0
      sy  <- function(p) T + (100 - p) / (100 - ylo) * (Ht - T - B)
      path <- function(x, y) {
        k <- !is.na(y)
        if (!any(k)) "" else paste0("M", paste(sprintf("%.1f,%.1f", x[k], y[k]), collapse = "L"))
      }

      gy <- seq(ylo, 100, by = if (100 - ylo > 50) 20 else 10)
      grid <- paste(sprintf('<line class="grid" x1="%d" x2="%d" y1="%.1f" y2="%.1f"/><text x="%d" y="%.1f" dy="3.5" text-anchor="end">%d%%</text>',
                            L, W - R, sy(gy), sy(gy), L - 8, sy(gy), gy), collapse = "")
      d0 <- as.Date(min(at)); d1 <- as.Date(max(at))
      ticks <- pretty(c(d0, d1), n = 6); ticks <- ticks[ticks >= d0 & ticks <= d1]
      if (!length(ticks)) ticks <- unique(c(d0, d1))
      tfmt <- if (as.numeric(d1 - d0) > 300) "%b %Y" else "%d %b"
      xt <- paste(sprintf('<text x="%.1f" y="%d" text-anchor="middle">%s</text>',
                          sx(as.POSIXct(format(ticks))), Ht - B + 18, sub("^0", "", format(ticks, tfmt))), collapse = "")
      tgt <- if (RR_GOOD >= ylo)
        sprintf('<line class="target" x1="%d" x2="%d" y1="%.1f" y2="%.1f"/><text class="target-l" x="%d" y="%.1f" dy="-5">Target %d%%</text>',
                L, W - R, sy(RR_GOOD), sy(RR_GOOD), L + 6, sy(RR_GOOD), RR_GOOD) else ""

      pal  <- c("#00788E", "#F07F3C", "#7C5CC4", "#3B82F6", "#059669", "#C59A00", "#B5405A", "#6B7280")
      cols <- setNames(rep(pal, length.out = length(tps)), tps)
      lines <- ""; ends <- list()
      for (tp in tps) {
        x <- h[h$timepoint == tp, , drop = FALSE]; x <- x[order(x$at), , drop = FALSE]
        if (!nrow(x) || all(is.na(x$pct))) next
        lines <- paste0(lines, sprintf('<path class="tp" d="%s" style="stroke:%s"/>', path(sx(x$at), sy(x$pct)), cols[[tp]]))
        last <- x[max(which(!is.na(x$pct))), ]
        ends[[length(ends) + 1]] <- list(y = sy(last$pct), cls = "end-l", col = cols[[tp]],
                                         label = paste(tp, fmt_pct(last$pct)))
      }
      xa <- sx(all$at[ok]); ya <- sy(all$pct[ok])
      area <- sprintf('<path class="area" d="%s L%.1f,%.1f L%.1f,%.1f Z"/>',
                      path(xa, ya), max(xa), sy(ylo), min(xa), sy(ylo))
      dots <- paste(sprintf('<circle class="dot" cx="%.1f" cy="%.1f" r="3.5"><title>%s: %s (%s of %s due)</title></circle>',
                            xa, ya, format(all$at[ok], "%d %b %Y"), fmt_pct(all$pct[ok], 1),
                            fmt_n(all$entered[ok]), fmt_n(all$due[ok])), collapse = "")
      lastA <- all[max(which(ok)), ]
      ends <- c(list(list(y = sy(lastA$pct), cls = "end-all", col = "#1B1B1B",
                          label = paste("All", fmt_pct(lastA$pct)))), ends)
      # End labels, nudged apart and kept inside the plot
      ys <- vapply(ends, function(e) e$y, numeric(1)); o <- order(ys); yy <- ys[o]
      for (i in seq_along(yy)[-1]) yy[i] <- max(yy[i], yy[i - 1] + 15)
      over <- yy[length(yy)] - (Ht - B); if (over > 0) yy <- yy - over
      yy <- pmax(yy, T); ys[o] <- yy
      labels <- paste(vapply(seq_along(ends), function(i)
        sprintf('<text class="%s" x="%d" y="%.1f" dy="3.5" style="fill:%s">%s</text>', ends[[i]]$cls,
                W - R + 10, ys[i], ends[[i]]$col, htmltools::htmlEscape(ends[[i]]$label)), ""), collapse = "")

      HTML(sprintf(paste0('<svg viewBox="0 0 %d %d" role="img" aria-label="Return rate in each return-rate file over time">',
                          '%s%s<line class="axis" x1="%d" x2="%d" y1="%.1f" y2="%.1f"/>%s%s%s<path class="all" d="%s"/>%s%s</svg>'),
                   W, Ht, grid, tgt, L, W - R, sy(ylo), sy(ylo), xt, area, lines, path(xa, ya), dots, labels))
    }

    output$trend_ui <- renderUI({
      if (src() != "file") return(NULL)
      h <- attr(file_df(), "history")
      if (is.null(h)) return(NULL)
      h <- h[h$timepoint %in% tps(), , drop = FALSE]
      svg <- if (nrow(h)) trend_svg(h, tps()) else NULL
      if (is.null(svg)) return(NULL)
      n <- length(unique(h$at))
      section("Over time",
              sprintf("The return rate in each of the last %d return-rate files in the folder, overall (black) and by timepoint.", n),
              div(class = "rt-trend", svg))
    })

    # ── Sites ──────────────────────────────────────────────────────────────
    open_sites <- reactiveVal(character())
    observeEvent(input$site_click, {
      s <- input$site_click$site; o <- open_sites()
      open_sites(if (s %in% o) setdiff(o, s) else c(o, s))
    })
    observeEvent(input$expand_all, {
      all <- site_list()
      open_sites(if (length(all) && all(all %in% open_sites())) character() else all)
    })
    sort_by <- reactive(if ((input$site_sort %||% "") %in% c("rate", "missing", "name")) input$site_sort else "rate")

    output$site_tools <- renderUI({
      all <- site_list(); if (!length(all)) return(NULL)
      s <- sort_by()
      btn <- function(v, l) tags$button(type = "button", class = paste("rt-sort", if (s == v) "on"),
                                        onclick = set_input("site_sort", js_str(v)), l)
      div(class = "th-toolbar",
          div(class = "th-pills", btn("rate", "Lowest rate"), btn("missing", "Most missing"), btn("name", "A–Z")),
          tags$button(type = "button", class = "th-link", onclick = set_input("expand_all", "Math.random()"),
                      if (all(all %in% open_sites())) "Collapse all" else "Expand all"))
    })

    output$site_table <- renderUI({
      s <- st()
      if (is.null(s) || !nrow(s))
        return(th_empty_note("There are only trial-wide figures here — no rows for individual sites."))
      tp    <- tps()
      tot   <- rr_summarise(s, "site")
      by_tp <- rr_summarise(s, c("site", "timepoint"))
      ord <- switch(sort_by(),
                    name    = order(tot$site),
                    missing = order(-tot$missing, tot$pct, tot$site),
                    order(is.na(tot$pct), tot$pct, -tot$missing, tot$site))
      tot  <- tot[ord, , drop = FALSE]
      red  <- src() == "redcap"
      open <- open_sites()
      ncol <- 4 + length(tp) + red
      cells_for <- function(bt, site) lapply(tp, function(x)
        tags$td(rate_cell(bt[bt$site == site & bt$timepoint == x, , drop = FALSE])))

      rows <- lapply(seq_len(nrow(tot)), function(i) {
        r <- tot[i, ]; is_open <- r$site %in% open
        main <- tags$tr(class = paste("rt-row", if (is_open) "open"), tabindex = "0",
          `aria-expanded` = tolower(is_open),
          onclick = set_input("site_click", sprintf("{site: %s, n: Math.random()}", js_str(r$site))),
          onkeydown = "if (event.key === 'Enter' || event.key === ' ') { event.preventDefault(); this.click(); }",
          tags$td(class = "l", span(class = "rt-chev", HTML("&#9656;")), span(class = "rt-site", r$site)),
          tags$td(class = "l", rate_bar(r)),
          cells_for(by_tp, r$site),
          tags$td(class = "r", fmt_n(r$due)),
          count_td(r$missing),
          if (red) count_td(r$overdue))
        if (!is_open) return(main)
        tagList(main, tags$tr(class = "rt-detail", tags$td(colspan = ncol,
          div(class = "rt-detail-h", sprintf("%s — every form at the selected timepoints", r$site)),
          form_matrix(s[s$site == r$site, , drop = FALSE], tp))))
      })

      o <- ov(); a <- rr_summarise(o)
      oa <- rr_summarise(o, "timepoint"); oa$site <- ".Overall"
      total <- tags$tr(class = "rt-total",
        tags$td(class = "l", span(class = "rt-chev"), "All sites"),
        tags$td(class = "l", rate_bar(a)),
        cells_for(oa, ".Overall"),
        tags$td(class = "r", fmt_n(a$due)),
        count_td(a$missing),
        if (red) count_td(a$overdue))

      div(class = "rt-table-wrap", `data-chart` = "sites",
          tags$table(class = "rt-table",
            tags$thead(tags$tr(tags$th(class = "l", "Site"), tags$th(class = "l", "Return rate"),
                               lapply(tp, tags$th), tags$th(class = "r", "Due"),
                               tags$th(class = "r", "Missing"), if (red) tags$th(class = "r", "Overdue"))),
            tags$tbody(rows, total)))
    })

    # ── Forms ──────────────────────────────────────────────────────────────
    output$form_scope_ui <- renderUI({
      s <- sites_rv(); if (!length(s)) return(NULL)
      cur <- isolate(input$form_scope) %||% ""
      selectInput(ns("form_scope"), NULL, choices = c("All sites" = "", s),
                  selected = if (cur %in% s) cur else "", width = "220px")
    })

    output$form_matrix <- renderUI({
      sc <- input$form_scope %||% ""
      d <- if (nzchar(sc) && sc %in% site_list()) st()[st()$site == sc, , drop = FALSE] else ov()
      if (is.null(d)) return(NULL)
      tagList(div(`data-chart` = "forms", form_matrix(d, tps())), key_ui())
    })

    # ── Participants with forms to return (REDCap only) ────────────────────
    outs <- reactive(rr_outstanding(H()))
    out_view <- reactive({
      o <- outs(); if (is.null(o) || !nrow(o)) return(o)
      if (nzchar(input$out_site %||% "")) o <- o[o$site == input$out_site, , drop = FALSE]
      o <- o[o$status %in% (input$out_status %||% character()), , drop = FALSE]
      # The filter chips name REDCap's timepoints only when REDCap is the source
      if (src() == "redcap") {
        o <- o[o$timepoint %in% tps(), , drop = FALSE]
        if (kind_sel() != "all") o <- o[o$kind_code == kind_sel(), , drop = FALSE]
      }
      o
    })

    output$out_site_ui <- renderUI({
      o <- outs(); s <- if (is.null(o)) character() else sort(unique(o$site))
      cur <- isolate(input$out_site) %||% ""
      selectInput(ns("out_site"), NULL, choices = c("All sites" = "", s),
                  selected = if (cur %in% s) cur else "", width = "190px")
    })

    output$outstanding <- renderReactable({
      o <- out_view()
      if (is.null(o) || !nrow(o))
        return(empty_reactable("No forms waiting to be returned with these filters."))
      est <- o$estimated
      reactable(
        o[, c("status", "id", "site", "timepoint", "form", "kind", "due_on", "days_overdue")],
        compact = TRUE, highlight = TRUE, searchable = TRUE,
        defaultPageSize = 15, showPageSizeOptions = TRUE, pageSizeOptions = c(15, 30, 60),
        onClick = htmlwidgets::JS("function(row) { Shiny.setInputValue('th_participant_open', {id: row.values.id, n: Math.random()}, {priority: 'event'}); }"),
        rowStyle = list(cursor = "pointer"),
        defaultColDef = colDef(style = list(fontSize = "12.5px")),
        columns = list(
          status = colDef(name = "Status", width = 104,
                          cell = function(v) span(class = paste("rt-st", v), if (v == "overdue") "Overdue" else "Due now")),
          id = colDef(name = "Participant", width = 110, style = list(fontWeight = 600, fontSize = "12.5px")),
          site = colDef(name = "Site", minWidth = 130),
          timepoint = colDef(name = "Timepoint", width = 110),
          form = colDef(name = "Form", minWidth = 170),
          kind = colDef(name = "Type", width = 120),
          due_on = colDef(name = "Visit date", width = 116,
                          cell = function(v, i) paste0(if (est[i]) "~" else "", format(v, "%d %b %Y"))),
          days_overdue = colDef(name = "Days overdue", width = 116, align = "right",
                                cell = function(v) if (v > 0) v else "—")))
    })

    # ── Downloads ──────────────────────────────────────────────────────────
    stamp <- function(what) sprintf("%s_%s_%s.csv", trial_code() %||% "trial", what, format(Sys.Date(), "%Y%m%d"))

    output$dl_rates <- downloadHandler(
      filename = function() stamp("return_rates"),
      content = function(file) {
        d <- dat()
        out <- if (is.null(d)) data.frame() else {
          x <- rr_summarise(d, c("site", "timepoint", "form", "kind"))
          x$site[x$site == ".Overall"] <- "All sites"
          x$rate_pct <- round(x$pct, 1); x$pct <- NULL
          x[order(x$site != "All sites", x$site, match(x$timepoint, tps())), , drop = FALSE]
        }
        utils::write.csv(out, file, row.names = FALSE, na = "")
      })

    output$dl_outstanding <- downloadHandler(
      filename = function() stamp("forms_to_return"),
      content = function(file) {
        o <- out_view()
        out <- if (is.null(o)) data.frame() else o[, setdiff(names(o), "kind_code"), drop = FALSE]
        utils::write.csv(out, file, row.names = FALSE, na = "")
      })
  })
}
