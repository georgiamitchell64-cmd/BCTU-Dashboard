# =============================================================================
# The app's font is shipped with it, not fetched at render time
# =============================================================================
# bslib's font_google() downloads the font while rendering the first page —
# inside the request. On a slow or filtered network that first render takes
# longer than any sane startup timeout, and the desktop build reports a
# dashboard that started fine as having failed to start.
#
# So the font lives in www/fonts and is declared in www/tonic_core.css. This
# fails if anyone puts font_google() back, or moves the files out from under
# the stylesheet.
# =============================================================================

app <- normalizePath(file.path(dirname(sub("^--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), ".."), winslash = "/")

pass <- 0L; fail <- character(0)
ok <- function(cond, label) {
  if (isTRUE(cond)) pass <<- pass + 1L else fail <<- c(fail, label)
}

# Parse rather than grep: the file explains in a comment why font_google() is
# not used, and a comment saying so must not read as the call itself.
theme_calls <- all.names(parse(file.path(app, "functions", "theme.R"),
                               keep.source = FALSE))
ok(!("font_google" %in% theme_calls),
   "functions/theme.R does not call font_google (it downloads during the render)")

theme_src <- readLines(file.path(app, "functions", "theme.R"), warn = FALSE)
ok(any(grepl("Outfit", theme_src)), "the theme still asks for Outfit")

css_path <- file.path(app, "www", "tonic_core.css")
css <- readLines(css_path, warn = FALSE)
ok(sum(grepl("@font-face", css)) >= 2, "tonic_core.css declares the font faces")

# Every font file the stylesheet points at must exist, at the path a browser
# would resolve it to: the CSS is served from www/, so url() is relative to it.
urls <- regmatches(css, gregexpr("url\\(['\"]?([^'\")]+)['\"]?\\)", css))
urls <- unique(sub("^url\\(['\"]?", "", sub("['\"]?\\)$", "", unlist(urls))))
urls <- urls[grepl("woff2?$|ttf$|otf$", urls)]
ok(length(urls) >= 2, "the stylesheet points at the font files")
for (u in urls) {
  f <- file.path(app, "www", u)
  ok(file.exists(f), paste0("the file the stylesheet points at exists: www/", u))
  if (file.exists(f)) {
    # wOF2 magic — a stray text file or an HTML error page saved as .woff2
    # would sail through a mere file.exists() check.
    magic <- readBin(f, "raw", 4)
    ok(identical(rawToChar(magic), "wOF2"),
       paste0("www/", u, " really is a woff2 file"))
    ok(file.size(f) > 5000, paste0("www/", u, " is not a truncated download"))
  }
}

# The variable font covers every weight; if that range went missing, bold text
# would be synthesised rather than drawn.
ok(any(grepl("font-weight:\\s*100\\s+900", css)),
   "the faces are declared across the full weight range")

# SIL Open Font License requires the licence to travel with the font.
ofl <- file.path(app, "www", "fonts", "OFL.txt")
ok(file.exists(ofl), "the font licence ships with the font")
if (file.exists(ofl))
  ok(any(grepl("SIL OPEN FONT LICENSE", readLines(ofl, warn = FALSE), ignore.case = TRUE)),
     "and it is the OFL")

# "fonts" must not collide with a Shiny resource prefix, or the browser gets a
# 404 for the font and silently falls back.
if (requireNamespace("shiny", quietly = TRUE)) {
  ok(!("fonts" %in% names(shiny::resourcePaths())),
     "www/fonts does not collide with a registered resource prefix")
}

if (length(fail)) {
  cat("\n", length(fail), " theme-font assertion(s) failed:\n", sep = "")
  for (f in fail) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("All theme-font assertions passed.\n")
