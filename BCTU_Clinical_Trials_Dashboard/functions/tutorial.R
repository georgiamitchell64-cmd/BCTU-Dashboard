# =============================================================================
# Guided tours and help pop-ups
# =============================================================================
# Everything people read in the tours and the "?" pop-ups is written here, so
# there's one place to update when the dashboard changes. www/tour.js runs
# them; styles are in www/tour.css.
#
#   TOURS       step-by-step walkthroughs: "home" (first sign-in) and "trial"
#               (the first time someone opens a trial). Each step spotlights
#               `target` (CSS selectors, the first one on screen wins; none =
#               a card in the middle), can open a tab first (`go` input and
#               `tab` button), and is left out when `requires` isn't on screen
#               (a tab this trial or this person doesn't have).
#   HELP_TIPS   the "?" after a section heading: `heading` is the heading's
#               exact text; `scope` limits it to one tab when a name repeats.
#   tutorial_assets()   the files, and both lists as JSON in the page
#   tutorial_server()   starts a tour the first time, and remembers who has
#                       seen which in profiles.tours_seen
# =============================================================================

.tour_step <- function(title, body, target = NULL, go = NULL, tab = NULL,
                       requires = NULL, eyebrow = NULL, next_label = NULL, skip = NULL) {
  s <- list(title = title, body = body)
  if (!is.null(target))     s$target   <- I(target)
  if (!is.null(go))         s$go       <- go
  if (!is.null(tab))        s$tab      <- tab
  if (!is.null(requires))   s$requires <- requires
  if (!is.null(eyebrow))    s$eyebrow  <- eyebrow
  if (!is.null(next_label)) s$`next`   <- next_label
  if (!is.null(skip))       s$skip     <- skip
  s
}

TOURS <- list(

  # ── Home screen: the first time someone signs in ─────────────────────────
  home = list(steps = list(
    .tour_step(
      eyebrow = "Welcome", next_label = "Show me around", skip = "Not now",
      "Welcome to the BCTU Clinical Trials Dashboard",
      paste0("<p>This two-minute tour shows you around the home screen. A short tour of the ",
             "trial dashboard starts the first time you open a trial.</p>",
             "<p>You can take either tour again from your profile menu.</p>")),
    .tour_step(target = ".home-tabs",
      "Your home screen",
      paste0("<ul><li><b>My Trials</b>: the trials you're a member of</li>",
             "<li><b>Portfolio</b>: every trial's progress at a glance</li>",
             "<li><b>All Trials</b>: the full list</li>",
             "<li><b>Sites</b>: every site across the trials</li>",
             "<li><b>Activity</b>: recent changes and uploads</li></ul>")),
    .tour_step(target = "#home_tab_my .qa-row",
      "Quick actions",
      paste0("Start a new trial dashboard, run a TMG or TSC report, or switch between light ",
             "and dark. Admins also manage people and access here.")),
    .tour_step(target = "#trial_cards_ui",
      "Open a trial",
      paste0("Each card shows a trial's recruitment against its target. ",
             "<b>Click a card</b> to open that trial's dashboard.")),
    .tour_step(target = ".home-topbar .tb-icon[title='Notifications']",
      "Notifications",
      paste0("Alerts worked out from the trial data, such as overdue forms and quiet sites. ",
             "A dot appears when there's something new.")),
    .tour_step(target = ".home-topbar .userchip",
      "Your profile",
      paste0("Change your password, take these tours again, or sign out. ",
             "Admins also manage people and backups here.")),
    .tour_step(
      eyebrow = "All set",
      "That's the home screen",
      "Open a trial to carry on. A short tour of the trial dashboard starts the first time you do.")
  )),

  # ── Trial dashboard: the first time someone opens a trial ────────────────
  trial = list(end_go = "go_overview", end_tab = "tn_overview", steps = list(
    .tour_step(
      eyebrow = "Welcome", next_label = "Show me around", skip = "Not now",
      "Welcome to the trial dashboard",
      paste0("<p>Everything for one trial lives here. This tour takes you through each ",
             "tab in about two minutes.</p>")),
    .tour_step(target = "#topnav_wrap",
      "The tabs",
      paste0("Each tab covers one part of the trial, and the one you're on is highlighted. ",
             "<b>All trials</b> takes you back to the home screen.")),
    .tour_step(go = "go_overview", tab = "tn_overview",
      target = c("#health_card_ui .th-hero", "#health_card_ui"),
      "Overview",
      paste0("The trial's health at a glance: a score out of 100 for recruitment, retention, ",
             "data completeness and site activity, with what needs attention first. Further ",
             "down are recruitment against target, projections and the site map.")),
    .tour_step(go = "go_participants", tab = "tn_participants",
      target = c("#data_health_kpis .th-kpis", ".data-hero-row"),
      "Data",
      paste0("How complete the data is: every CRF and questionnaire for every participant, ",
             "who needs chasing (most urgent first), safety events, demographics and ",
             "withdrawals. Click a participant to see their full history.")),
    .tour_step(go = "go_sites", tab = "tn_sites",
      target = c("#sites_summary_stats", "#sites_groups_ui"),
      "Sites",
      paste0("Every site with its status, open date and recruitment. Click a site to edit it. ",
             "Sites that aren't on REDCap yet can be added here and are kept in their own ",
             "group until they open.")),
    .tour_step(go = "go_randomisations", tab = "tn_randomisations",
      target = c("#rand_health_kpis", "#rand_trajectory_ui"),
      "Randomisations",
      paste0("Recruitment over time: how many each month, when people are randomised ",
             "(including out of hours and at weekends), and when the target is likely to be reached.")),
    .tour_step(requires = "#tn_postal", go = "go_postal", tab = "tn_postal",
      target = "#tn_postal",
      "Postal tracking",
      "Log the questionnaires posted to participants and mark them off when they come back."),
    .tour_step(requires = "#tn_returns", go = "go_returns", tab = "tn_returns",
      target = c(".rt-shell .th-kpis", ".rt-top", "#tn_returns"),
      "Return rates",
      paste0("How many of the forms that are due have come back, by timepoint, site and form, ",
             "and which participants still owe one.")),
    .tour_step(go = "go_reports", tab = "tn_reports",
      target = c("#rb_type_cards", "#tn_reports"),
      "Reports",
      paste0("Choose a report (TMG, iTMG, TSC and more), check it's ready, preview it and ",
             "download it as a PDF or Word document. Reports always cover the whole trial.")),
    .tour_step(go = "go_modifications", tab = "tn_modifications",
      target = c("#mod_summary_strip", "#tn_modifications"),
      "Modifications",
      paste0("The register of protocol amendments. Add them one at a time or import a ",
             "spreadsheet. The TSC report reads them from here.")),
    .tour_step(requires = "#tn_settings", go = "go_settings", tab = "tn_settings",
      target = c(".settings-list", ".settings-nav", "#tn_settings"),
      "Settings",
      paste0("The trial's set-up: names and targets, data sources, visit schedule, monitoring ",
             "thresholds and report content. Changes apply for everyone on this trial. ",
             "<b>Find a setting</b> jumps straight to one.")),
    .tour_step(target = "#topbar_account",
      "Your account",
      "Change your password, take this tour again, or sign out."),
    .tour_step(
      eyebrow = "Tips", next_label = "Finish",
      "You're ready to go",
      paste0("<ul><li>Click the <b>?</b> next to a heading for a short explanation.</li>",
             "<li>Every chart has an <b>Image</b> button to save it as a PNG or JPEG.</li>",
             "<li>Take this tour again from your account menu, top right.</li></ul>"))
  ))
)

.tip <- function(heading, text, scope = NULL) {
  t <- list(heading = heading, text = text)
  if (!is.null(scope)) t$scope <- scope
  t
}

HELP_TIPS <- list(
  .tip("Trial health",
       paste("A score out of 100 combining recruitment against target, retention, data completeness",
             "and site activity. The list beside it shows what needs attention first.",
             "Thresholds can be changed in Settings → Monitoring.")),
  .tip("Recruitment projection",
       "Where recruitment is heading if the current pace continues, compared with the protocol target."),
  .tip("UK site map", "Every site on a map. Hover over a site to see its recruitment."),
  .tip("Data health",
       paste("Every scheduled CRF and questionnaire for every participant. A form becomes overdue",
             "once its visit date plus the grace period (14 days unless changed in Settings) has passed.")),
  .tip("Participants needing attention",
       paste("Participants with overdue forms or missing operation or discharge dates, most urgent",
             "first. Click a row for the participant's full CRF history.")),
  .tip("Safety & regulatory",
       "SAEs, protocol deviations, withdrawals and pregnancies from the REDCap export. Click a tile to list the records."),
  .tip("Demographics",
       paste("Baseline characteristics of everyone randomised. Customise chooses which fields",
             "appear, what they're called and how they're grouped, such as age under and over 75.")),
  .tip("Withdrawals & change of status",
       paste("Participants whose status has changed (withdrawn, lost to follow-up or died) by type,",
             "month and site. The line on each site's bar is the trial-wide rate.")),
  .tip("Where recruitment is heading",
       paste("Three projections (the current pace, the recent trend and the effect of opening",
             "more sites) with the range of likely finish dates.")),
  .tip("Recruitment by site",
       "Randomisations at each site, with each site's monthly average against its target."),
  .tip("Out-of-hours recruitment by site",
       paste("Which sites randomise in the evenings, at weekends and on bank holidays.",
             "Working hours are set in Settings → Monitoring.")),
  .tip("Return rates",
       paste("A return rate is forms entered divided by forms due. Forms whose visit hasn't come",
             "round yet never count against a site. On target is 90% or more."),
       scope = ".rt-shell")
)

# The files, and both lists as JSON for www/tour.js (goes in the page <head>)
tutorial_assets <- function() {
  json <- function(x) HTML(jsonlite::toJSON(x, auto_unbox = TRUE))
  tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = .asset("tour.css")),
    tags$script(src = .asset("tour.js"), defer = NA),
    tags$script(type = "application/json", id = "bctu-tours", json(TOURS)),
    tags$script(type = "application/json", id = "bctu-help-tips", json(HELP_TIPS)))
}

# ── Who has seen which tour (profiles.tours_seen, comma-separated) ─────────
.tours_migrate <- function(con) {
  cols <- tryCatch(dbGetQuery(con, "PRAGMA table_info(profiles)")$name, error = function(e) character())
  if (length(cols) && !"tours_seen" %in% cols)
    dbExecute(con, "ALTER TABLE profiles ADD COLUMN tours_seen TEXT")
}

tours_seen <- function(fullname) {
  if (is.null(fullname) || !nzchar(fullname)) return(character())
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  .tours_migrate(con)
  v <- dbGetQuery(con, "SELECT tours_seen FROM profiles WHERE fullname = ?",
                  params = list(fullname))$tours_seen
  if (!length(v) || is.na(v[1]) || !nzchar(v[1])) character() else strsplit(v[1], ",", fixed = TRUE)[[1]]
}

mark_tour_seen <- function(fullname, tour) {
  if (is.null(fullname) || !nzchar(fullname)) return(invisible(FALSE))
  seen <- union(tours_seen(fullname), tour)
  con <- shared_db_connect(); on.exit(dbDisconnect(con))
  dbExecute(con, "UPDATE profiles SET tours_seen = ? WHERE fullname = ?",
            params = list(paste(seen, collapse = ","), fullname))
  invisible(TRUE)
}

# ── Start each tour once per person ────────────────────────────────────────
# Finishing and skipping both count as seen; either tour can be replayed
# from the account menu (bctuTour.replay() in www/tour.js).
tutorial_server <- function(input, output, session, state) {
  rv   <- state$rv
  seen <- reactiveVal(NULL)

  observeEvent(rv$username, {
    u <- rv$username
    if (!nzchar(u %||% "")) return()
    s <- tryCatch(tours_seen(u), error = function(e) {
      message("Tours: ", conditionMessage(e)); names(TOURS)   # can't tell: don't interrupt
    })
    seen(s)
    if (!"home" %in% s && is.null(rv$trial_code))
      session$sendCustomMessage("tour_start", list(tour = "home"))
  })

  observeEvent(rv$trial_code, {
    s <- seen()
    if (!is.null(s) && !"trial" %in% s)
      session$sendCustomMessage("tour_start", list(tour = "trial"))
  })

  observeEvent(input$tour_done, {
    t <- input$tour_done$tour %||% ""
    u <- rv$username
    if (!nzchar(u %||% "") || !t %in% names(TOURS)) return()
    tryCatch(mark_tour_seen(u, t), error = function(e) message("Tours: ", conditionMessage(e)))
    seen(union(seen() %||% character(), t))
  })
}
