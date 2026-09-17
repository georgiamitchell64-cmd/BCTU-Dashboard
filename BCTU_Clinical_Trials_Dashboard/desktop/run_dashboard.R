# =============================================================================
# Start the dashboard from a desktop shortcut
# =============================================================================
# Run by "Start BCTU Dashboard.bat" (Windows) and "BCTU Dashboard.app" (Mac,
# via start_mac.sh). The shortcut has already opened desktop/starting.html,
# which waits for the dashboard and then switches to it, so this only starts it.
#
#   - A second double-click while it's running leaves the running copy alone
#     (the starting page finds it and switches straight to it).
#   - It stops itself 10 minutes after the last browser tab closes
#     (BCTU_DESKTOP; see desktop_track_session() in functions/helpers.R).
#     Change the wait with BCTU_AUTO_STOP_MINUTES, the port with BCTU_PORT.
# =============================================================================

args    <- commandArgs(FALSE)
# Rscript passes spaces in the script's path as "~+~"
me      <- gsub("~+~", " ", sub("^--file=", "", grep("^--file=", args, value = TRUE)[1]), fixed = TRUE)
me      <- normalizePath(me)
app_dir <- normalizePath(file.path(dirname(me), ".."))
port    <- as.integer(Sys.getenv("BCTU_PORT", "3838"))
address <- sprintf("http://127.0.0.1:%d/", port)

# Is something already answering on the port, and is it the dashboard?
already <- local({
  old <- options(timeout = 3); on.exit(options(old))
  page <- tryCatch(suppressWarnings({
    con <- url(address, open = "r"); on.exit(close(con), add = TRUE)
    readLines(con, n = 400, warn = FALSE)
  }), error = function(e) NULL)
  if (is.null(page)) "free" else if (any(grepl("bctu-tours|Birmingham Clinical Trials Unit", page))) "dashboard" else "other"
})

if (already == "dashboard") {
  message("The dashboard is already running at ", address)
  quit(save = "no", status = 0)
}
if (already == "other") {
  message("Port ", port, " is being used by another program, so the dashboard can't start. ",
          "Close that program, or set BCTU_PORT to a different number.")
  quit(save = "no", status = 1)
}

Sys.setenv(BCTU_DESKTOP = "1")
setwd(app_dir)
message("Starting the BCTU Clinical Trials Dashboard at ", address)
message("It stops by itself once no browser tab has had it open for ",
        Sys.getenv("BCTU_AUTO_STOP_MINUTES", "10"), " minutes. Closing this window stops it now.")
shiny::runApp(app_dir, host = "127.0.0.1", port = port, launch.browser = FALSE)
