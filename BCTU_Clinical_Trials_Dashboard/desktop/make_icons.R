# =============================================================================
# Desktop shortcut icon
# =============================================================================
# Draws the icon (a dark tile, rising turquoise bars, BCTU) and writes it for
# each platform:
#   icon/bctu_dashboard.png   1024 px master
#   icon/bctu_dashboard.icns  Mac (needs macOS's iconutil)
#   icon/bctu_dashboard.ico   Windows (16–256 px, PNG-compressed)
# Run from the app folder:  Rscript desktop/make_icons.R
# =============================================================================

library(grid)
out <- file.path("desktop", "icon")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

INK  <- "#1B1B1B"
TEAL <- "#00ACA9"

draw_icon <- function(file, px) {
  ragg::agg_png(file, width = px, height = px, background = "transparent", res = 72)
  grid.newpage()
  # Mac icons sit inside ~82% of the canvas; Windows uses the full square
  pushViewport(viewport(width = 0.82, height = 0.82))
  grid.roundrect(r = unit(0.225, "snpc"), gp = gpar(fill = INK, col = NA))
  heights <- c(0.17, 0.27, 0.38, 0.52)
  xs <- c(0.26, 0.42, 0.58, 0.74)
  base <- 0.15
  for (i in 1:4)
    grid.roundrect(x = xs[i], y = base, width = 0.11, height = heights[i], just = c("centre", "bottom"),
                   r = unit(0.02, "snpc"), gp = gpar(fill = TEAL, col = NA))
  # The word only where it's legible
  if (px >= 64)
    grid.text("BCTU", x = 0.17, y = 0.83, just = c("left", "top"),
              gp = gpar(col = "white", fontface = "bold", fontfamily = "Arial",
                        fontsize = 0.82 * px * 0.17))
  invisible(dev.off())
}

draw_icon(file.path(out, "bctu_dashboard.png"), 1024)

# ── Mac .icns ─────────────────────────────────────────────────────────────
if (nzchar(Sys.which("iconutil"))) {
  set <- file.path(tempdir(), "bctu_dashboard.iconset")
  unlink(set, recursive = TRUE); dir.create(set)
  for (s in c(16, 32, 128, 256, 512)) {
    draw_icon(file.path(set, sprintf("icon_%dx%d.png", s, s)), s)
    draw_icon(file.path(set, sprintf("icon_%dx%d@2x.png", s, s)), 2 * s)
  }
  system2("iconutil", c("-c", "icns", shQuote(set), "-o", shQuote(file.path(out, "bctu_dashboard.icns"))))
}

# ── Windows .ico: an ICONDIR followed by PNG images ─────────────────────────
sizes <- c(16, 24, 32, 48, 64, 128, 256)
pngs  <- vapply(sizes, function(s) {
  f <- file.path(tempdir(), sprintf("ico_%d.png", s)); draw_icon(f, s); f
}, "")
blobs <- lapply(pngs, function(f) readBin(f, "raw", file.info(f)$size))
con <- file(file.path(out, "bctu_dashboard.ico"), "wb")
writeBin(as.integer(c(0, 1, length(blobs))), con, size = 2, endian = "little")
offset <- 6 + 16 * length(blobs)
for (i in seq_along(blobs)) {
  s <- if (sizes[i] >= 256) 0L else as.integer(sizes[i])
  writeBin(c(s, s, 0L, 0L), con, size = 1)
  writeBin(c(1L, 32L), con, size = 2, endian = "little")
  writeBin(as.integer(c(length(blobs[[i]]), offset)), con, size = 4, endian = "little")
  offset <- offset + length(blobs[[i]])
}
for (b in blobs) writeBin(b, con)
close(con)
cat("Icons written to", out, "\n")
