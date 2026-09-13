# =============================================================================
# Build www/dark_theme.css — the dashboard's dark mode
# =============================================================================
# Run from the app folder after changing colours in a stylesheet or the UI code:
#   Rscript tools/build_dark_theme.R
#
# Dark mode is html[data-theme="dark"] (Home → Switch theme). Rather than a
# hand-written dark twin of every rule, this reads the colours the dashboard
# actually uses and writes their dark equivalents:
#   1. every rule in www/*.css that sets a colour gets a dark copy scoped to
#      html[data-theme="dark"], which outranks the original wherever it loads
#   2. inline style="…" colours and SVG fill/stroke attributes in the R and JS
#      code get matching attribute rules (!important, to beat inline styles)
#   3. a few hand-made fixes (logos, maps, primary buttons) go last
# Colours are mapped in OKLab. Neutrals move onto one cool slate scale: the
# page darkest, cards a step lighter, lines lighter again, text lightest.
# Pale tints become deep tints of the same hue, dark accents are lifted so
# text stays readable, and mid-tone brand and status colours are kept. Charts
# drawn by echarts are re-coloured by www/dark_theme.js.
# The hand-written dark rules in www/modules_redesign.css load after this file
# and still win where they exist.
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) args[1] else "."
DARK <- 'html[data-theme="dark"]'

# ── Colour maths (OKLab / OKLCh) ────────────────────────────────────────────
.lin <- function(c) ifelse(c <= 0.04045, c / 12.92, ((c + 0.055) / 1.055)^2.4)
.gam <- function(c) ifelse(c <= 0.0031308, 12.92 * c, 1.055 * c^(1 / 2.4) - 0.055)

to_oklch <- function(rgb) {
  r <- .lin(rgb[1]); g <- .lin(rgb[2]); b <- .lin(rgb[3])
  l <- (0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b)^(1 / 3)
  m <- (0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b)^(1 / 3)
  s <- (0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b)^(1 / 3)
  L <- 0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s
  A <- 1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s
  B <- 0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s
  c(L = L, C = sqrt(A^2 + B^2), h = atan2(B, A))
}

from_oklch <- function(L, C, h) {
  for (k in 0:30) {                       # reduce chroma until it fits sRGB
    A <- C * cos(h); B <- C * sin(h)
    l <- (L + 0.3963377774 * A + 0.2158037573 * B)^3
    m <- (L - 0.1055613458 * A - 0.0638541728 * B)^3
    s <- (L - 0.0894841775 * A - 1.2914855480 * B)^3
    rgb <- c( 4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s,
             -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s,
             -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s)
    if (all(rgb >= -1e-4 & rgb <= 1 + 1e-4)) break
    C <- C * 0.9
  }
  .gam(pmin(1, pmax(0, rgb)))
}

# Neutral lightness: light theme (in) → dark theme (out). Cards (white) sit a
# step above the page (#F4F4F4), inner boxes (#F8F8F8) just below the card.
N_IN  <- c(0,    0.22,  0.37, 0.47,  0.64,  0.75,  0.87, 0.925, 0.945, 0.955, 0.965, 0.975, 0.985, 1)
N_OUT <- c(0.97, 0.935, 0.85, 0.745, 0.625, 0.525, 0.42, 0.33,  0.30,  0.29,  0.205, 0.225, 0.265, 0.245)
SLATE <- 250 * pi / 180

map_colour <- function(rgb, role) {
  o <- to_oklch(rgb); L <- o[["L"]]; C <- o[["C"]]; h <- o[["h"]]
  if (C < 0.04) {                          # greys, near-whites, inks
    L2 <- approx(N_IN, N_OUT, xout = min(max(L, 0), 1))$y
    return(from_oklch(L2, if (L2 < 0.5) 0.014 else 0.006, SLATE))
  }
  if (L >= 0.85) {                         # pale tints: deep tints of the same hue
    L2 <- switch(role, bg = 0.30, line = 0.40, fill = 0.36, fg = 0.45)
    return(from_oklch(L2, if (role == "fg") C else min(C, 0.05), h))
  }
  if (L < 0.62) {                          # deep accents: lift for contrast
    L2 <- switch(role, fg = 0.78, bg = 0.66, line = 0.60, fill = max(L, 0.62))
    return(from_oklch(L2, min(C, 0.14), h))
  }
  rgb                                      # mid-tone brand / status colours
}

COL_RE <- "#[0-9A-Fa-f]{8}\\b|#[0-9A-Fa-f]{6}\\b|#[0-9A-Fa-f]{3,4}\\b|rgba?\\([^)]*\\)|\\b(?:white|black)\\b"

parse_token <- function(t) {
  t <- tolower(t)
  if (t == "white") return(list(rgb = c(1, 1, 1), a = 1))
  if (t == "black") return(list(rgb = c(0, 0, 0), a = 1))
  if (startsWith(t, "#")) {
    h <- substring(t, 2)
    if (nchar(h) %in% c(3, 4)) h <- paste(rep(strsplit(h, "")[[1]], each = 2), collapse = "")
    v <- strtoi(substring(h, seq(1, nchar(h) - 1, 2), seq(2, nchar(h), 2)), 16L)
    return(list(rgb = v[1:3] / 255, a = if (length(v) == 4) v[4] / 255 else 1))
  }
  pct <- grepl("%", t)
  nums <- suppressWarnings(as.numeric(strsplit(trimws(gsub("rgba?\\(|\\)|%", "", t)), "[ ,/]+")[[1]]))
  if (length(nums) < 3 || anyNA(nums[1:3])) return(NULL)
  a <- if (length(nums) >= 4 && !is.na(nums[4])) nums[4] else 1
  if (pct && a > 1) a <- a / 100
  list(rgb = pmin(1, nums[1:3] / 255), a = a)
}

fmt_colour <- function(rgb, a) {
  v <- round(rgb * 255)
  if (a >= 0.999) sprintf("#%02X%02X%02X", v[1], v[2], v[3])
  else sprintf("rgba(%d,%d,%d,%s)", v[1], v[2], v[3], format(round(a, 3)))
}

map_token <- function(t, role) {
  p <- parse_token(t)
  if (is.null(p)) return(t)
  # Dimming overlays (drawer and modal backdrops) stay dark
  if (role == "bg" && p$a >= 0.3 && p$a < 1 && to_oklch(p$rgb)[["L"]] < 0.3) return(t)
  fmt_colour(map_colour(p$rgb, role), p$a)
}

map_value <- function(v, role) {
  mm <- gregexpr(COL_RE, v, perl = TRUE)
  if (mm[[1]][1] == -1) return(NULL)
  old <- regmatches(v, mm)[[1]]
  new <- vapply(old, map_token, "", role = role, USE.NAMES = FALSE)
  if (identical(tolower(new), tolower(old))) return(NULL)
  regmatches(v, mm) <- list(new)
  v
}

prop_role <- function(p) {
  p <- tolower(trimws(p))
  if (startsWith(p, "--")) {
    if (grepl("bg|panel|soft|box|rail|card|tint|surface|track|base|paper", p)) return("bg")
    if (grepl("line|border|rule|grid|divider", p)) return("line")
    return("fg")
  }
  if (p %in% c("background", "background-color", "background-image")) return("bg")
  if (grepl("^(border|outline|column-rule)", p)) return("line")
  if (p %in% c("fill", "stroke")) return("fill")
  if (p %in% c("color", "caret-color", "text-decoration-color", "-webkit-text-fill-color", "accent-color")) return("fg")
  NA_character_
}

# ── A small CSS reader: top-level blocks, and splits outside () and quotes ──
.sub <- function(ch, a, b) if (b >= a) paste(ch[a:b], collapse = "") else ""

css_blocks <- function(x) {
  ch <- strsplit(x, "", fixed = TRUE)[[1]]
  out <- list(); depth <- 0; seg <- 1; body <- 0; pre <- ""; q <- ""
  for (k in seq_along(ch)) {
    c1 <- ch[k]
    if (nzchar(q)) { if (c1 == q && (k == 1 || ch[k - 1] != "\\")) q <- ""; next }
    if (c1 == '"' || c1 == "'") { q <- c1; next }
    if (c1 == "{") {
      if (depth == 0) { pre <- .sub(ch, seg, k - 1); body <- k + 1 }
      depth <- depth + 1
    } else if (c1 == "}") {
      depth <- depth - 1
      if (depth == 0) {
        out[[length(out) + 1]] <- list(pre = trimws(pre), body = .sub(ch, body, k - 1))
        seg <- k + 1
      }
    } else if (c1 == ";" && depth == 0) seg <- k + 1       # @import, @charset
  }
  out
}

split_top <- function(x, sep) {
  ch <- strsplit(x, "", fixed = TRUE)[[1]]
  parts <- character(); depth <- 0; q <- ""; start <- 1
  for (k in seq_along(ch)) {
    c1 <- ch[k]
    if (nzchar(q)) { if (c1 == q) q <- ""; next }
    if (c1 == '"' || c1 == "'") q <- c1
    else if (c1 == "(") depth <- depth + 1
    else if (c1 == ")") depth <- depth - 1
    else if (c1 == sep && depth == 0) { parts <- c(parts, .sub(ch, start, k - 1)); start <- k + 1 }
  }
  parts <- c(parts, .sub(ch, start, length(ch)))
  parts[nzchar(trimws(parts))]
}

# Surfaces that are already dark in light mode keep their own colours (the
# Overview health band and its figures)
KEEP_DARK <- "\\.th-hero|\\.th-hs-"

dark_selector <- function(sel) {
  sel <- trimws(gsub("\\s+", " ", sel))
  if (!nzchar(sel) || grepl("data-theme", sel) || grepl(KEEP_DARK, sel)) return(NULL)
  if (grepl("^(:root|html)(?![-\\w])", sel, perl = TRUE))
    return(sub("^(:root|html)", DARK, sel))
  paste(DARK, sel)
}

dark_rules <- function(blocks) {
  out <- character()
  for (b in blocks) {
    pre <- b$pre
    if (grepl("^@(media|supports)", pre)) {
      if (grepl("\\bprint\\b", pre) && !grepl("\\bscreen\\b", pre)) next
      inner <- dark_rules(css_blocks(b$body))
      if (length(inner)) out <- c(out, paste0(pre, " {"), paste0("  ", inner), "}")
      next
    }
    if (startsWith(pre, "@")) next                      # keyframes, font-face
    decls <- character()
    for (d in split_top(b$body, ";")) {
      i <- regexpr(":", d, fixed = TRUE)
      if (i < 1) next
      prop <- trimws(substr(d, 1, i - 1)); val <- trimws(substring(d, i + 1))
      role <- prop_role(prop)
      if (is.na(role)) next
      imp <- grepl("!important", val, fixed = TRUE)
      val <- trimws(sub("!important", "", val, fixed = TRUE))
      nv <- map_value(val, role)
      if (!is.null(nv)) decls <- c(decls, paste0(prop, ": ", nv, if (imp) " !important"))
    }
    if (!length(decls)) next
    sels <- unlist(lapply(split_top(pre, ","), dark_selector))
    if (!length(sels)) next
    out <- c(out, paste0(paste(sels, collapse = ",\n"), " { ", paste(decls, collapse = "; "), " }"))
  }
  out
}

# ── 1. Stylesheets ─────────────────────────────────────────────────────────
css_files <- list.files(file.path(root, "www"), "\\.css$", full.names = TRUE)
css_files <- css_files[!basename(css_files) %in% c("dark_theme.css", "tour.css")]
sections <- list()
for (f in css_files) {
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  txt <- gsub("/\\*[\\s\\S]*?\\*/", "", txt, perl = TRUE)
  r <- dark_rules(css_blocks(txt))
  if (length(r)) sections[[basename(f)]] <- r
}

# Stylesheets written inside R files: tags$style(HTML("…"))
for (f in c(list.files(file.path(root, "modules"), "\\.R$", full.names = TRUE),
            list.files(file.path(root, "functions"), "\\.R$", full.names = TRUE))) {
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  css <- regmatches(txt, gregexpr('tags\\$style\\(HTML\\("((?:[^"\\\\]|\\\\.)*)"\\)', txt, perl = TRUE))[[1]]
  if (!length(css)) next
  css <- gsub('^tags\\$style\\(HTML\\("|"\\)$', "", css)
  css <- gsub('\\\\"', '"', paste(css, collapse = "\n"))
  css <- gsub("/\\*[\\s\\S]*?\\*/", "", css, perl = TRUE)
  r <- dark_rules(css_blocks(css))
  if (length(r)) sections[[paste0(basename(f), " (inline <style>)")]] <- r
}

# ── 2. Inline colours in the R and JS code ─────────────────────────────────
src <- c(list.files(file.path(root, "modules"), "\\.R$", full.names = TRUE),
         list.files(file.path(root, "functions"), "\\.R$", full.names = TRUE, recursive = TRUE),
         file.path(root, "app.R"),
         list.files(file.path(root, "www"), "\\.js$", full.names = TRUE))
code <- paste(unlist(lapply(src, readLines, warn = FALSE)), collapse = "\n")

INLINE_RE <- paste0("(?<![A-Za-z-])(background-color|background|color|border-(?:top|bottom|left|right)-color|",
                    "border-(?:top|bottom|left|right)|border-color|border|outline|fill|stroke)\\s*:\\s*",
                    "[^;\"'\\n{}<>]*?(?:#[0-9A-Fa-f]{3,8}\\b|rgba?\\([^)]*\\))")
hits <- unique(regmatches(code, gregexpr(INLINE_RE, code, perl = TRUE))[[1]])
hits <- hits[!grepl("gradient|url\\(|%s|\\{|\\\\", hits)]

longhand <- function(prop) {
  prop <- tolower(prop)
  if (prop %in% c("background", "background-color")) return("background-color")
  if (prop == "border") return("border-color")
  if (grepl("^border-(top|bottom|left|right)$", prop)) return(paste0(prop, "-color"))
  if (prop == "outline") return("outline-color")
  prop
}

inline <- character()
for (h in hits) {
  prop <- tolower(sub("\\s*:.*$", "", h))
  role <- prop_role(prop)
  tok  <- regmatches(h, regexpr(COL_RE, h, perl = TRUE))
  if (is.na(role) || !length(tok)) next
  new <- map_token(tok, role)
  if (identical(tolower(new), tolower(tok))) next
  q <- gsub('"', '\\\\"', h)
  # The colour must end there (#8A8A8C, not #8A8A8C18), and "color" mustn't be
  # the end of background-color
  ends <- function(pre) c(sprintf('[style*="%s%s;" i]', pre, q), sprintf('[style*="%s%s " i]', pre, q),
                          sprintf('[style$="%s%s" i]', pre, q))
  alts <- if (prop == "color")
    c(sprintf('[style^="%s;" i]', q), sprintf('[style^="%s " i]', q), sprintf('[style="%s" i]', q),
      ends(";"), ends(" "))
  else ends("")
  sels <- paste(paste(DARK, alts), collapse = ", ")
  inline <- c(inline, sprintf("%s { %s: %s !important }", sels, longhand(prop), new))
}

svg_re <- "(?:\\b(fill|stroke)\\s*=\\s*[\"']|setAttribute\\(\\s*['\"](fill|stroke)['\"]\\s*,\\s*['\"])(#[0-9A-Fa-f]{3,8})['\"]"
svg <- regmatches(code, gregexpr(svg_re, code, perl = TRUE))[[1]]
attrs <- character()
for (s in unique(svg)) {
  prop <- if (grepl("stroke", s)) "stroke" else "fill"
  tok  <- regmatches(s, regexpr("#[0-9A-Fa-f]{3,8}", s))
  new  <- map_token(tok, "fill")
  if (!identical(tolower(new), tolower(tok)))
    attrs <- c(attrs, sprintf('%s [%s="%s" i] { %s: %s }', DARK, prop, tok, prop, new))
}
attrs <- unique(attrs)

# ── 3. Hand-made fixes ─────────────────────────────────────────────────────
FIXES <- c(
  sprintf("%s { color-scheme: dark; }", DARK),
  "/* The BCTU wordmark is black on transparent: show it in white */",
  sprintf('%s img[src*="BlackText"] { filter: brightness(0) invert(.93) !important; }', DARK),
  "/* Map tiles come as images: invert them, keeping water blue */",
  sprintf("%s .leaflet-tile-pane { filter: invert(1) hue-rotate(180deg) brightness(.95) contrast(.85); }", DARK),
  "/* Bootstrap's primary button takes its colours from bslib, not these files */",
  sprintf("%s .btn-primary, %s .btn-primary:focus { background-color: #E8EDF2 !important; border-color: #E8EDF2 !important; color: #11171D !important; }", DARK, DARK),
  sprintf("%s .btn-primary:hover { background-color: #FFFFFF !important; }", DARK),
  sprintf("%s ::selection { background: rgba(46,196,193,.35); }", DARK),
  "/* reactable writes its own light theme at run time: let the card show through */",
  sprintf(paste("%s .ReactTable, %s .ReactTable .rt-table, %s .ReactTable .rt-thead, %s .ReactTable .rt-tbody,",
                "%s .ReactTable .rt-tr-group, %s .ReactTable .rt-tr, %s .ReactTable .rt-th, %s .ReactTable .rt-td,",
                "%s .ReactTable .rt-pagination, %s .ReactTable .rt-tfoot { background-color: transparent !important; }"),
          DARK, DARK, DARK, DARK, DARK, DARK, DARK, DARK, DARK, DARK),
  sprintf("%s .ReactTable .rt-tr-highlight:hover, %s .ReactTable .rt-tr-striped { background-color: rgba(255,255,255,.04) !important; }", DARK, DARK),
  sprintf("%s .ReactTable .rt-td, %s .ReactTable .rt-th, %s .ReactTable .rt-pagination { color: var(--ov-ink) !important; border-color: var(--ov-line2) !important; }", DARK, DARK, DARK),
  "/* The home 'New trial' tile is the primary action: keep it filled */",
  sprintf("%s .qa-tile.primary { background: var(--navy) !important; border-color: var(--navy) !important; }", DARK),
  sprintf("%s .qa-tile.primary:hover { background: var(--navy-lt) !important; }", DARK)
)

# ── Write ──────────────────────────────────────────────────────────────────
swatch <- function(hex, role) fmt_colour(map_colour(parse_token(hex)$rgb, role), 1)
out <- c(
  "/* =============================================================================",
  "   DARK MODE — generated by tools/build_dark_theme.R. Don't edit by hand:",
  "   change a colour in its own stylesheet or UI code, then rerun",
  "     Rscript tools/build_dark_theme.R",
  sprintf("   Built %s. Page %s · card %s · line %s · text %s · muted %s",
          format(Sys.Date()), swatch("#F4F4F4", "bg"), swatch("#FFFFFF", "bg"),
          swatch("#E6E6E6", "line"), swatch("#1B1B1B", "fg"), swatch("#58595B", "fg")),
  "   ========================================================================== */", "")
for (nm in names(sections))
  out <- c(out, sprintf("/* ── %s ── */", nm), sections[[nm]], "")
out <- c(out, "/* ── Inline style colours in the R and JS code ── */", inline, "",
         "/* ── SVG fill / stroke attributes ── */", attrs, "",
         "/* ── Hand-made fixes ── */", FIXES, "")
writeLines(out, file.path(root, "www", "dark_theme.css"))

# Canvas and script-built SVG colours can't be reached by CSS, so the widget
# scripts look them up here (trial_health.js / trial_replay.js: C(colour))
js_hex <- unique(toupper(unlist(regmatches(
  paste(unlist(lapply(list.files(file.path(root, "www"), "\\.js$", full.names = TRUE)[
    !grepl("dark_", list.files(file.path(root, "www"), "\\.js$"))], readLines, warn = FALSE)), collapse = "\n"),
  gregexpr("#[0-9A-Fa-f]{6}\\b|#[0-9A-Fa-f]{3}\\b", paste(unlist(lapply(
    list.files(file.path(root, "www"), "\\.js$", full.names = TRUE)[!grepl("dark_", list.files(file.path(root, "www"), "\\.js$"))],
    readLines, warn = FALSE)), collapse = "\n"))))))
pal <- vapply(js_hex, function(h) map_token(h, "fill"), "")
writeLines(c(
  "// Generated by tools/build_dark_theme.R — the dark twin of each colour the",
  "// widget scripts draw with (canvas and script-built SVG). Don't edit by hand.",
  sprintf("window.BCTU_DARK_PALETTE = %s;",
          jsonlite::toJSON(as.list(setNames(pal, tolower(js_hex))), auto_unbox = TRUE))),
  file.path(root, "www", "dark_palette.js"))
cat(sprintf("dark_palette.js: %d colours\n", length(pal)))
cat(sprintf("dark_theme.css: %d stylesheet rules from %d files, %d inline, %d SVG attribute rules\n",
            sum(lengths(sections)), length(sections), length(inline), length(attrs)))
