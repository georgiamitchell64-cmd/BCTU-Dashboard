# =============================================================================
# Desktop shortcut icon
# =============================================================================
# The brand mark is icon/2a-primary.png (the BCTU bar-chart tile). This resizes
# it for each platform:
#   icon/bctu_dashboard.png   1024 px master
#   icon/bctu_dashboard.icns  Mac (needs macOS's iconutil and sips)
#   icon/bctu_dashboard.ico   Windows (16-256 px, PNG-compressed)
# Run from the app folder:  Rscript desktop/make_icons.R
#
# The other tiles beside it (light, gold, mono black/white) are the same mark
# for other backgrounds; nothing builds from them yet.
# =============================================================================

out <- file.path("desktop", "icon")
src <- file.path(out, "2a-primary.png")
if (!file.exists(src)) stop("Missing the brand mark: ", src)

# One square PNG at the size asked for. sips ships with macOS; magick is the
# fallback so the script still runs elsewhere.
resize <- function(file, px) {
  if (nzchar(Sys.which("sips"))) {
    file.copy(src, file, overwrite = TRUE)
    system2("sips", c("-z", px, px, shQuote(file)), stdout = NULL, stderr = NULL)
  } else if (nzchar(Sys.which("magick"))) {
    system2("magick", c(shQuote(src), "-resize", sprintf("%dx%d", px, px), shQuote(file)))
  } else {
    stop("Need sips (macOS) or ImageMagick to resize the icon")
  }
  invisible(file)
}

resize(file.path(out, "bctu_dashboard.png"), 1024)

# ── Mac .icns ─────────────────────────────────────────────────────────────
if (nzchar(Sys.which("iconutil"))) {
  set <- file.path(tempdir(), "bctu_dashboard.iconset")
  unlink(set, recursive = TRUE); dir.create(set)
  for (s in c(16, 32, 128, 256, 512)) {
    resize(file.path(set, sprintf("icon_%dx%d.png", s, s)), s)
    resize(file.path(set, sprintf("icon_%dx%d@2x.png", s, s)), 2 * s)
  }
  system2("iconutil", c("-c", "icns", shQuote(set), "-o", shQuote(file.path(out, "bctu_dashboard.icns"))))
}

# ── Windows .ico: an ICONDIR followed by PNG images ─────────────────────────
sizes <- c(16, 24, 32, 48, 64, 128, 256)
pngs  <- vapply(sizes, function(s) {
  f <- file.path(tempdir(), sprintf("ico_%d.png", s)); resize(f, s); f
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
cat("Icons written to", out, "from", basename(src), "\n")
