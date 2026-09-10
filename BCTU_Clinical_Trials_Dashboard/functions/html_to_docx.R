# =============================================================================
# High-fidelity HTML → editable DOCX conversion
# =============================================================================
# The TMG / iTMG report (tmg_report.Rmd) is an HTML-styled document: its
# charts are drawn at runtime with Chart.js into <canvas> elements (plus an
# inline <svg> timeline), and its layout uses CSS grid/flex with coloured
# banners.
#
# A plain `pandoc html -t docx` loses ALL of that — pandoc never runs the
# JavaScript, so every chart comes out blank, and CSS layout is discarded.
# That is why the old DOCX looked nothing like the HTML.
#
# This helper closes most of the gap while keeping the output an *editable*
# Word document (real text + real Word tables, not a flat image):
#
#   1. Open the rendered HTML in headless Chrome (chromote) — the same engine
#      that makes the PDF pixel-perfect — and let the charts draw.
#   2. Rasterise every <canvas> and <svg> into an embedded PNG <img>, and drop
#      <script> tags. Now the charts survive a static conversion.
#   3. Convert that chart-baked HTML to DOCX with pandoc, applying a branded
#      reference document (navy headings, clean table style) so headings and
#      tables match the TONIC look.
#
# Layout-only flourishes (coloured banner bars, multi-column card grids) still
# won't match the HTML exactly — Word has no equivalent — but text, tables and
# charts all come through, which is the bulk of the report.
# =============================================================================

# Resolve the pandoc binary, assuming ensure_pandoc() has already run.
.pandoc_bin <- function() {
  pandoc_dir <- Sys.getenv("RSTUDIO_PANDOC")
  if (nzchar(pandoc_dir)) {
    bin <- file.path(pandoc_dir,
                     if (.Platform$OS.type == "windows") "pandoc.exe" else "pandoc")
    if (file.exists(bin)) return(bin)
  }
  "pandoc"
}

# ---------------------------------------------------------------------------
# Rasterise JS-drawn charts in a rendered HTML file.
#
# Returns the path to a new HTML file in which every <canvas> / <svg> has been
# replaced by an embedded PNG <img>, or NULL if chromote isn't available or the
# render fails (caller can then fall back to the raw HTML).
# ---------------------------------------------------------------------------
rasterize_html_charts <- function(html_path, out_html = tempfile(fileext = ".html")) {
  if (!requireNamespace("chromote", quietly = TRUE)) return(NULL)
  if (!file.exists(html_path)) return(NULL)

  # JS run inside the page: replace canvases + svgs with PNG <img>, strip
  # scripts, and hand back the rewritten document HTML. Returns a Promise so
  # the SVG rasterisation (which loads an Image) can complete first.
  js <- "
  (async () => {
    const dpr = 2;  // upscale so chart text stays crisp in Word

    // --- <canvas> (Chart.js) → PNG <img> -----------------------------------
    for (const cv of Array.from(document.querySelectorAll('canvas'))) {
      try {
        const rect = cv.getBoundingClientRect();
        const w = Math.max(1, Math.round(rect.width || cv.width));
        const url = cv.toDataURL('image/png');
        const img = document.createElement('img');
        img.src = url;
        img.setAttribute('width', w);
        img.style.maxWidth = '100%';
        if (cv.parentNode) cv.parentNode.replaceChild(img, cv);
      } catch (e) {}
    }

    // --- inline <svg> → PNG <img> ------------------------------------------
    for (const svg of Array.from(document.querySelectorAll('svg'))) {
      try {
        const rect = svg.getBoundingClientRect();
        const w = Math.max(1, Math.round(rect.width));
        const h = Math.max(1, Math.round(rect.height));
        const clone = svg.cloneNode(true);
        clone.setAttribute('width', w);
        clone.setAttribute('height', h);
        if (!clone.getAttribute('xmlns'))
          clone.setAttribute('xmlns', 'http://www.w3.org/2000/svg');
        const xml = new XMLSerializer().serializeToString(clone);
        const svg64 = 'data:image/svg+xml;base64,' +
                      btoa(unescape(encodeURIComponent(xml)));
        const dataUrl = await new Promise((resolve, reject) => {
          const im = new Image();
          im.onload = () => {
            const c = document.createElement('canvas');
            c.width = w * dpr; c.height = h * dpr;
            const ctx = c.getContext('2d');
            ctx.scale(dpr, dpr);
            ctx.drawImage(im, 0, 0, w, h);
            resolve(c.toDataURL('image/png'));
          };
          im.onerror = reject;
          im.src = svg64;
        });
        const img = document.createElement('img');
        img.src = dataUrl;
        img.setAttribute('width', w);
        img.style.maxWidth = '100%';
        if (svg.parentNode) svg.parentNode.replaceChild(img, svg);
      } catch (e) {}
    }

    document.querySelectorAll('script').forEach(s => s.remove());
    return '<!doctype html>' + document.documentElement.outerHTML;
  })()
  "

  baked <- tryCatch({
    b <- chromote::ChromoteSession$new()
    on.exit(try(b$close(), silent = TRUE), add = TRUE)
    b$Page$navigate(paste0("file://", normalizePath(html_path)))
    Sys.sleep(2)  # let webfonts + Chart.js animations settle
    res <- b$Runtime$evaluate(expression = js,
                              awaitPromise = TRUE,
                              returnByValue = TRUE)
    val <- res$result$value
    if (is.null(val) || !nzchar(val)) NULL else val
  }, error = function(e) {
    message("rasterize_html_charts: ", conditionMessage(e))
    NULL
  })

  if (is.null(baked)) return(NULL)
  writeLines(baked, out_html, useBytes = TRUE)
  out_html
}

# ---------------------------------------------------------------------------
# Build (once per session) a branded Word reference document so pandoc-styled
# headings and tables match the TONIC look. Best-effort: returns the path on
# success, or NULL so the caller falls back to pandoc's default styling.
# ---------------------------------------------------------------------------
ensure_brand_reference_docx <- function() {
  ref_path <- file.path(tempdir(), "tonic_reference.docx")
  if (file.exists(ref_path)) return(ref_path)

  tryCatch({
    if (!requireNamespace("xml2", quietly = TRUE) ||
        !requireNamespace("zip",  quietly = TRUE))
      return(NULL)

    pandoc_bin <- .pandoc_bin()

    # 1. Seed pandoc's default reference doc by converting trivial markdown.
    seed_md <- tempfile(fileext = ".md")
    writeLines(c("# H1", "", "## H2", "", "### H3", "", "Body text.",
                 "", "| A | B |", "|---|---|", "| 1 | 2 |"), seed_md)
    res <- suppressWarnings(system2(
      pandoc_bin,
      args = c(shQuote(seed_md), "-o", shQuote(ref_path)),
      stdout = TRUE, stderr = TRUE))
    if (!file.exists(ref_path) || file.info(ref_path)$size == 0)
      return(NULL)

    # 2. Unpack the docx (a zip) and recolour the heading styles to navy.
    work <- file.path(tempdir(), "tonic_ref_build")
    if (dir.exists(work)) unlink(work, recursive = TRUE)
    dir.create(work)
    utils::unzip(ref_path, exdir = work)

    styles_xml <- file.path(work, "word", "styles.xml")
    if (file.exists(styles_xml)) {
      doc <- xml2::read_xml(styles_xml)
      ns  <- xml2::xml_ns(doc)
      navy <- "1B4F6B"

      # Heading styles: set run colour + bold. Pandoc names them Heading1..N.
      for (sid in c("Heading1", "Heading2", "Heading3", "Heading4", "Title")) {
        style <- xml2::xml_find_first(
          doc, sprintf(".//w:style[@w:styleId='%s']", sid), ns)
        if (inherits(style, "xml_missing")) next
        rpr <- xml2::xml_find_first(style, "./w:rPr", ns)
        if (inherits(rpr, "xml_missing")) {
          xml2::xml_add_child(style, "w:rPr")
          rpr <- xml2::xml_find_first(style, "./w:rPr", ns)
        }
        # colour
        col <- xml2::xml_find_first(rpr, "./w:color", ns)
        if (inherits(col, "xml_missing"))
          col <- xml2::xml_add_child(rpr, "w:color")
        xml2::xml_set_attr(col, "w:val", navy)
        # bold
        if (inherits(xml2::xml_find_first(rpr, "./w:b", ns), "xml_missing"))
          xml2::xml_add_child(rpr, "w:b")
      }
      xml2::write_xml(doc, styles_xml)
    }

    # 3. Re-zip the folder back into a .docx (contents at archive root).
    files <- list.files(work, recursive = TRUE, all.files = TRUE,
                        full.names = FALSE, include.dirs = FALSE)
    unlink(ref_path)
    old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE)
    setwd(work)
    zip::zip(zipfile = ref_path, files = files, mode = "cherry-pick")
    setwd(old_wd)

    if (file.exists(ref_path) && file.info(ref_path)$size > 0) ref_path else NULL
  }, error = function(e) {
    message("ensure_brand_reference_docx: ", conditionMessage(e))
    NULL
  })
}

# ---------------------------------------------------------------------------
# Convert a rendered HTML report into an editable DOCX, baking in JS-drawn
# charts and applying the brand reference doc. Writes to `out_file`.
#
# Returns TRUE on success, FALSE on failure (caller can fall back / warn).
# ---------------------------------------------------------------------------
html_to_editable_docx <- function(html_path, out_file) {
  if (!file.exists(html_path)) return(FALSE)

  # Bake charts in; if chromote is unavailable, convert the raw HTML (charts
  # will be missing, but text + tables still come through).
  src_html <- tryCatch(rasterize_html_charts(html_path),
                       error = function(e) NULL)
  if (is.null(src_html)) src_html <- html_path

  ref_doc <- tryCatch(ensure_brand_reference_docx(), error = function(e) NULL)

  pandoc_bin <- .pandoc_bin()
  tmp_docx <- tempfile(fileext = ".docx")
  args <- c(shQuote(src_html), "-f", "html", "-t", "docx")
  if (!is.null(ref_doc) && file.exists(ref_doc))
    args <- c(args, paste0("--reference-doc=", ref_doc))
  args <- c(args, "-o", shQuote(tmp_docx))

  out <- suppressWarnings(system2(pandoc_bin, args = args,
                                  stdout = TRUE, stderr = TRUE))
  if (!file.exists(tmp_docx) || file.info(tmp_docx)$size == 0) {
    message("html_to_editable_docx: pandoc produced no output\n",
            paste(out, collapse = "\n"))
    return(FALSE)
  }
  file.copy(tmp_docx, out_file, overwrite = TRUE)
  TRUE
}
