`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# =============================================================================
# Pandoc discovery (cross-platform)
# =============================================================================
# rmarkdown::render() needs pandoc on PATH. On macOS RStudio sets
# RSTUDIO_PANDOC; on Windows / Linux without RStudio we have to look harder
# or surface a clear error. This helper tries every place we know about and
# sets RSTUDIO_PANDOC when it finds one.
ensure_pandoc <- function() {
  # Already configured?
  if (nzchar(Sys.getenv("RSTUDIO_PANDOC")) &&
      file.exists(file.path(Sys.getenv("RSTUDIO_PANDOC"),
                            if (.Platform$OS.type == "windows") "pandoc.exe" else "pandoc")))
    return(TRUE)
  
  # rmarkdown carries its own copy in newer versions. pandoc_exec() returns
  # character(0) when no pandoc is available, so guard length/NA before
  # file.exists() — otherwise the `if` errors instead of falling through.
  rm_path <- tryCatch(rmarkdown::pandoc_exec(), error = function(e) NULL)
  if (length(rm_path) == 1 && !is.na(rm_path) && nzchar(rm_path) &&
      file.exists(rm_path)) {
    Sys.setenv(RSTUDIO_PANDOC = dirname(rm_path)); return(TRUE)
  }
  
  # Common install locations across the three OSes
  candidates <- c(
    # macOS — RStudio + Quarto
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/x86_64",
    "/Applications/RStudio.app/Contents/Resources/app/bin/quarto/bin/tools",
    "/Applications/RStudio.app/Contents/MacOS/quarto/bin/tools",
    "/Applications/RStudio.app/Contents/MacOS/pandoc",
    "/usr/local/bin",
    "/opt/homebrew/bin",
    
    # Windows — RStudio + standalone + Quarto
    "C:/Program Files/RStudio/resources/app/quarto/bin/tools",
    "C:/Program Files/RStudio/bin/quarto/bin/tools",
    "C:/Program Files/RStudio/bin/pandoc",
    "C:/Program Files/Pandoc",
    "C:/Program Files (x86)/Pandoc",
    file.path(Sys.getenv("LOCALAPPDATA"), "Pandoc"),
    file.path(Sys.getenv("APPDATA"),      "local", "Pandoc"),
    "C:/Program Files/Quarto/bin/tools",
    "C:/Program Files/Quarto/bin",
    
    # Linux
    "/usr/bin", "/usr/local/bin"
  )
  bin <- if (.Platform$OS.type == "windows") "pandoc.exe" else "pandoc"
  for (pp in candidates) {
    if (nzchar(pp) && dir.exists(pp) && file.exists(file.path(pp, bin))) {
      Sys.setenv(RSTUDIO_PANDOC = pp); return(TRUE)
    }
  }
  
  # Last resort: maybe it's already on PATH but RSTUDIO_PANDOC isn't set
  found <- tryCatch(Sys.which(bin), error = function(e) "")
  if (nzchar(found) && file.exists(found)) {
    Sys.setenv(RSTUDIO_PANDOC = dirname(found)); return(TRUE)
  }
  
  FALSE
}

# =============================================================================
# Report templates (per-trial)
# =============================================================================
# Each trial keeps its own copy of the report Rmd files in trials/<code>/reports/.
# When a trial is missing a copy (legacy / new install), we fall back to the
# canonical templates at the project root. The Settings → Report templates UI
# writes back to the per-trial copy so trial managers can customise without
# affecting other trials.
# =============================================================================

REPORT_TEMPLATE_KINDS <- c("tonic", "tsc")  # tonic_report.Rmd = TMG/iTMG, tsc_report.Rmd = TSC

# Resolve the URL path (relative to Shiny's www/) for a trial's logo, if one
# was copied to www/trial_logos/<code>.<ext> at startup. Returns NULL when
# there's no logo so callers can fall back to a placeholder.
trial_logo_url <- function(cfg) {
  code <- cfg$code %||% ""
  if (!nzchar(code)) return(NULL)
  dir <- file.path(getwd(), "www", "trial_logos")
  if (!dir.exists(dir)) return(NULL)
  for (ext in c("png", "svg", "jpg", "jpeg", "webp", "gif")) {
    f <- file.path(dir, paste0(code, ".", ext))
    if (file.exists(f)) return(paste0("trial_logos/", code, ".", ext))
  }
  NULL
}

# Read the YAML header from an Rmd file and return the names of the params
# declared in the `params:` block. Used to filter out params the dashboard
# would otherwise pass to a stale per-trial template (rmarkdown errors with
# "render params not declared in YAML" if it sees an unknown one).
# Returns character(0) if the file is missing / unreadable / has no params.
rmd_declared_params <- function(rmd_path) {
  if (is.null(rmd_path) || !file.exists(rmd_path)) return(character(0))
  lines <- tryCatch(readLines(rmd_path, warn = FALSE, n = 400),
                    error = function(e) character(0))
  if (!length(lines) || !grepl("^---\\s*$", lines[1])) return(character(0))
  
  # Find the closing `---` of the YAML block
  close_idx <- which(grepl("^---\\s*$", lines))[2]
  if (is.na(close_idx)) return(character(0))
  yaml_lines <- lines[2:(close_idx - 1)]
  
  # Find the `params:` key and read its indented children
  pidx <- grep("^params:\\s*$", yaml_lines)
  if (!length(pidx)) return(character(0))
  remaining <- yaml_lines[(pidx[1] + 1):length(yaml_lines)]
  
  # Stop at the first non-indented line (next top-level key)
  stop_at <- which(grepl("^[^[:space:]#]", remaining))
  if (length(stop_at)) remaining <- remaining[seq_len(stop_at[1] - 1)]
  
  # Pick out lines like "  short_name: ..." or "  report_content: NULL"
  m <- regmatches(remaining,
                  regexec("^\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*:", remaining))
  out <- vapply(m, function(x) if (length(x) >= 2) x[[2]] else NA_character_,
                character(1))
  unique(out[!is.na(out)])
}

# Filter a named list of params to only those declared by the Rmd's YAML.
# Lets the dashboard pass new params without breaking older per-trial copies.
filter_params_for_rmd <- function(params, rmd_path) {
  decl <- rmd_declared_params(rmd_path)
  if (!length(decl)) return(params)
  dropped <- setdiff(names(params), decl)
  if (length(dropped))
    message("rmd_render: dropping params not in YAML of ",
            basename(rmd_path), ": ",
            paste(dropped, collapse = ", "))
  params[intersect(names(params), decl)]
}

# Filename for a given template kind, e.g. "tonic" → "tonic_report.Rmd"
report_template_filename <- function(kind) {
  if (!kind %in% REPORT_TEMPLATE_KINDS)
    stop("Unknown report template kind: ", kind)
  paste0(kind, "_report.Rmd")
}

# Path to the trial's own copy (may not exist yet).
trial_report_template_path <- function(cfg, kind) {
  trial_dir <- cfg$trial_dir %||% file.path(getwd(), "trials", cfg$code %||% "")
  file.path(trial_dir, "reports", report_template_filename(kind))
}

# Path to the project-level fallback template (the "factory default").
default_report_template_path <- function(kind) {
  file.path(getwd(), report_template_filename(kind))
}

# Resolve which file to use at render time. Order of precedence:
#   1. cfg$report_template_paths[[kind]] — explicit override path set in
#      Trial Settings → Report templates (lets a user point at an existing Rmd
#      they already maintain elsewhere, e.g. on a network drive).
#   2. trials/<code>/reports/<kind>_report.Rmd — the per-trial copy — unless
#      the canonical template is newer. Per-trial copies are seeded once and
#      then go stale when the canonical Rmd is updated (e.g. via git pull),
#      which used to silently keep rendering old layouts/figures; whichever
#      of the two files was modified most recently wins, so in-app edits to
#      the trial copy still take precedence until the canonical next changes.
#   3. <project root>/<kind>_report.Rmd — the canonical fallback.
# Returns NULL if none exist.
resolve_report_template <- function(cfg, kind) {
  override <- cfg$report_template_paths[[kind]]
  if (!is.null(override) && nzchar(override) && file.exists(override))
    return(override)
  
  trial_path   <- trial_report_template_path(cfg, kind)
  default_path <- default_report_template_path(kind)
  has_trial    <- file.exists(trial_path)
  has_default  <- file.exists(default_path)
  
  if (has_trial && has_default) {
    t_trial   <- suppressWarnings(file.info(trial_path)$mtime)
    t_default <- suppressWarnings(file.info(default_path)$mtime)
    if (!is.na(t_trial) && !is.na(t_default) && t_default > t_trial) {
      message("Report template: canonical ", basename(default_path),
              " is newer than the per-trial copy — rendering the canonical. ",
              "Save or reset the template in Trial Settings to refresh the copy.")
      return(default_path)
    }
  }
  if (has_trial)   return(trial_path)
  if (has_default) return(default_path)
  
  NULL
}

# Copy the canonical templates into a trial's reports/ folder. Idempotent —
# `overwrite = FALSE` by default so we don't trample edits the user has made.
seed_trial_report_templates <- function(cfg, overwrite = FALSE) {
  trial_dir   <- cfg$trial_dir %||% file.path(getwd(), "trials", cfg$code %||% "")
  reports_dir <- file.path(trial_dir, "reports")
  if (!dir.exists(reports_dir))
    dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)
  for (kind in REPORT_TEMPLATE_KINDS) {
    src <- default_report_template_path(kind)
    dst <- trial_report_template_path(cfg, kind)
    if (!file.exists(src)) next
    if (file.exists(dst) && !overwrite) next
    tryCatch(file.copy(src, dst, overwrite = TRUE),
             error = function(e) message("Template copy failed (", kind, "): ",
                                         e$message))
  }
  invisible(reports_dir)
}

# ── Hospital / city coordinate lookup (replaces city_latlon) ──────────────────
.uk_hospital_coords <- data.frame(
  stringsAsFactors = FALSE,
  name = c(
    "Musgrove Park Hospital",
    "Taunton",
    "Queen Elizabeth Hospital Birmingham",
    "QE Birmingham",
    "University Hospital Birmingham",
    "Leeds General Infirmary",
    "Leeds Teaching Hospitals",
    "Leeds",
    "St Mark's Hospital",
    "Harrow",
    "Manchester Royal Infirmary",
    "Manchester University NHS",
    "Manchester",
    "Sheffield Teaching Hospitals",
    "Northern General Hospital",
    "Sheffield",
    "Bristol Royal Infirmary",
    "Bristol",
    "Oxford University Hospitals",
    "John Radcliffe Hospital",
    "Oxford",
    "Royal Liverpool University Hospital",
    "Liverpool",
    "Freeman Hospital Newcastle",
    "Newcastle Freeman",
    "Newcastle",
    "Royal Victoria Infirmary",
    "Nottingham University Hospitals",
    "Queen's Medical Centre",
    "Nottingham",
    "Leicester Royal Infirmary",
    "Leicester",
    "Addenbrooke's Hospital",
    "Cambridge",
    "Southampton General Hospital",
    "Southampton",
    "King's College Hospital",
    "Guy's Hospital",
    "St Thomas' Hospital",
    "Royal Free Hospital",
    "Hammersmith Hospital",
    "Imperial College Healthcare",
    "London",
    "St George's Hospital",
    "Tooting",
    "University College London Hospital",
    "UCLH",
    "Aintree University Hospital",
    "Royal Preston Hospital",
    "Preston",
    "Blackpool Teaching Hospitals",
    "Blackpool",
    "Countess of Chester Hospital",
    "Chester",
    "Royal Stoke University Hospital",
    "Stoke-on-Trent",
    "Stoke",
    "University Hospital Coventry",
    "Coventry",
    "Worcestershire Royal Hospital",
    "Worcester",
    "Hereford County Hospital",
    "Hereford",
    "Royal Shrewsbury Hospital",
    "Shrewsbury",
    "Morriston Hospital",
    "Swansea",
    "University Hospital of Wales",
    "Cardiff",
    "Royal Gwent Hospital",
    "Newport",
    "Wrexham Maelor Hospital",
    "Wrexham",
    "Aberdeen Royal Infirmary",
    "Aberdeen",
    "Royal Infirmary Edinburgh",
    "Edinburgh",
    "Glasgow Royal Infirmary",
    "Queen Elizabeth University Hospital Glasgow",
    "Glasgow",
    "Ninewells Hospital",
    "Dundee",
    "Derriford Hospital",
    "Plymouth",
    "Royal Devon and Exeter",
    "Exeter",
    "Royal Cornwall Hospital",
    "Truro",
    "Yeovil District Hospital",
    "Yeovil",
    "Royal United Hospital Bath",
    "Bath",
    "Cheltenham General Hospital",
    "Cheltenham",
    "Gloucestershire Royal Hospital",
    "Gloucester"
  ),
  lat = c(
    51.0160,  # Musgrove Park Hospital
    51.0160,  # Taunton
    52.4525,  # Queen Elizabeth Hospital Birmingham
    52.4525,  # QE Birmingham
    52.4525,  # University Hospital Birmingham
    53.8068,  # Leeds General Infirmary
    53.8068,  # Leeds Teaching Hospitals
    53.8068,  # Leeds
    51.5776,  # St Mark's Hospital
    51.5776,  # Harrow
    53.4671,  # Manchester Royal Infirmary
    53.4671,  # Manchester University NHS
    53.4671,  # Manchester
    53.3804,  # Sheffield Teaching Hospitals
    53.3804,  # Northern General Hospital
    53.3804,  # Sheffield
    51.4582,  # Bristol Royal Infirmary
    51.4582,  # Bristol
    51.7623,  # Oxford University Hospitals
    51.7623,  # John Radcliffe Hospital
    51.7623,  # Oxford
    53.4122,  # Royal Liverpool University Hospital
    53.4122,  # Liverpool
    55.0019,  # Freeman Hospital Newcastle
    55.0019,  # Newcastle Freeman
    55.0019,  # Newcastle
    54.9783,  # Royal Victoria Infirmary
    52.9530,  # Nottingham University Hospitals
    52.9418,  # Queen's Medical Centre
    52.9530,  # Nottingham
    52.6234,  # Leicester Royal Infirmary
    52.6234,  # Leicester
    52.1751,  # Addenbrooke's Hospital
    52.1751,  # Cambridge
    50.9356,  # Southampton General Hospital
    50.9356,  # Southampton
    51.4686,  # King's College Hospital
    51.4995,  # Guy's Hospital
    51.4994,  # St Thomas' Hospital
    51.5532,  # Royal Free Hospital
    51.5167,  # Hammersmith Hospital
    51.5167,  # Imperial College Healthcare
    51.5074,  # London
    51.4277,  # St George's Hospital
    51.4277,  # Tooting
    51.5237,  # University College London Hospital
    51.5237,  # UCLH
    53.4584,  # Aintree University Hospital
    53.7638,  # Royal Preston Hospital
    53.7638,  # Preston
    53.8174,  # Blackpool Teaching Hospitals
    53.8174,  # Blackpool
    53.2006,  # Countess of Chester Hospital
    53.2006,  # Chester
    53.0027,  # Royal Stoke University Hospital
    53.0027,  # Stoke-on-Trent
    53.0027,  # Stoke
    52.4072,  # University Hospital Coventry
    52.4072,  # Coventry
    52.1952,  # Worcestershire Royal Hospital
    52.1952,  # Worcester
    52.0568,  # Hereford County Hospital
    52.0568,  # Hereford
    52.7081,  # Royal Shrewsbury Hospital
    52.7081,  # Shrewsbury
    51.6561,  # Morriston Hospital
    51.6561,  # Swansea
    51.5042,  # University Hospital of Wales
    51.5042,  # Cardiff
    51.5872,  # Royal Gwent Hospital
    51.5872,  # Newport
    53.0472,  # Wrexham Maelor Hospital
    53.0472,  # Wrexham
    57.1497,  # Aberdeen Royal Infirmary
    57.1497,  # Aberdeen
    55.9414,  # Royal Infirmary Edinburgh
    55.9414,  # Edinburgh
    55.8642,  # Glasgow Royal Infirmary
    55.8575,  # Queen Elizabeth University Hospital Glasgow
    55.8642,  # Glasgow
    56.4572,  # Ninewells Hospital
    56.4572,  # Dundee
    50.4102,  # Derriford Hospital
    50.4102,  # Plymouth
    50.7228,  # Royal Devon and Exeter
    50.7228,  # Exeter
    50.2681,  # Royal Cornwall Hospital
    50.2681,  # Truro
    50.9452,  # Yeovil District Hospital
    50.9452,  # Yeovil
    51.3966,  # Royal United Hospital Bath
    51.3966,  # Bath
    51.9001,  # Cheltenham General Hospital
    51.9001,  # Cheltenham
    51.8642,  # Gloucestershire Royal Hospital
    51.8642   # Gloucester
  ),
  lon = c(
    -3.0838,  # Musgrove Park Hospital
    -3.0838,  # Taunton
    -1.9399,  # Queen Elizabeth Hospital Birmingham
    -1.9399,  # QE Birmingham
    -1.9399,  # University Hospital Birmingham
    -1.5570,  # Leeds General Infirmary
    -1.5570,  # Leeds Teaching Hospitals
    -1.5570,  # Leeds
    -0.3168,  # St Mark's Hospital
    -0.3168,  # Harrow
    -2.2388,  # Manchester Royal Infirmary
    -2.2388,  # Manchester University NHS
    -2.2388,  # Manchester
    -1.4803,  # Sheffield Teaching Hospitals
    -1.4803,  # Northern General Hospital
    -1.4803,  # Sheffield
    -2.5987,  # Bristol Royal Infirmary
    -2.5987,  # Bristol
    -1.2233,  # Oxford University Hospitals
    -1.2233,  # John Radcliffe Hospital
    -1.2233,  # Oxford
    -2.9606,  # Royal Liverpool University Hospital
    -2.9606,  # Liverpool
    -1.6168,  # Freeman Hospital Newcastle
    -1.6168,  # Newcastle Freeman
    -1.6168,  # Newcastle
    -1.6229,  # Royal Victoria Infirmary
    -1.1862,  # Nottingham University Hospitals
    -1.2162,  # Queen's Medical Centre
    -1.1862,  # Nottingham
    -1.1270,  # Leicester Royal Infirmary
    -1.1270,  # Leicester
    0.1408,  # Addenbrooke's Hospital
    0.1408,  # Cambridge
    -1.3965,  # Southampton General Hospital
    -1.3965,  # Southampton
    -0.1057,  # King's College Hospital
    -0.1189,  # Guy's Hospital
    -0.1182,  # St Thomas' Hospital
    -0.1659,  # Royal Free Hospital
    -0.2256,  # Hammersmith Hospital
    -0.2256,  # Imperial College Healthcare
    -0.1278,  # London
    -0.1735,  # St George's Hospital
    -0.1735,  # Tooting
    -0.1348,  # University College London Hospital
    -0.1348,  # UCLH
    -2.9462,  # Aintree University Hospital
    -2.7052,  # Royal Preston Hospital
    -2.7052,  # Preston
    -3.0379,  # Blackpool Teaching Hospitals
    -3.0379,  # Blackpool
    -2.1744,  # Countess of Chester Hospital
    -2.1744,  # Chester
    -2.1294,  # Royal Stoke University Hospital
    -2.1294,  # Stoke-on-Trent
    -2.1294,  # Stoke
    -1.5072,  # University Hospital Coventry
    -1.5072,  # Coventry
    -2.2206,  # Worcestershire Royal Hospital
    -2.2206,  # Worcester
    -2.7135,  # Hereford County Hospital
    -2.7135,  # Hereford
    -2.7481,  # Royal Shrewsbury Hospital
    -2.7481,  # Shrewsbury
    -3.9336,  # Morriston Hospital
    -3.9336,  # Swansea
    -3.1933,  # University Hospital of Wales
    -3.1933,  # Cardiff
    -3.0028,  # Royal Gwent Hospital
    -3.0028,  # Newport
    -3.4809,  # Wrexham Maelor Hospital
    -3.4809,  # Wrexham
    -2.0996,  # Aberdeen Royal Infirmary
    -2.0996,  # Aberdeen
    -3.1791,  # Royal Infirmary Edinburgh
    -3.1791,  # Edinburgh
    -4.0726,  # Glasgow Royal Infirmary
    -4.3086,  # Queen Elizabeth University Hospital Glasgow
    -4.0726,  # Glasgow
    -2.9707,  # Ninewells Hospital
    -2.9707,  # Dundee
    -4.0726,  # Derriford Hospital
    -4.0726,  # Plymouth
    -3.5275,  # Royal Devon and Exeter
    -3.5275,  # Exeter
    -5.0517,  # Royal Cornwall Hospital
    -5.0517,  # Truro
    -2.6382,  # Yeovil District Hospital
    -2.6382,  # Yeovil
    -2.3531,  # Royal United Hospital Bath
    -2.3531,  # Bath
    -2.0829,  # Cheltenham General Hospital
    -2.0829,  # Cheltenham
    -2.0938,  # Gloucestershire Royal Hospital
    -2.0938   # Gloucester
  )
)

hospital_latlon <- function(name) {
  if (is.null(name) || !nzchar(trimws(name)))
    return(list(lat = NA_real_, lon = NA_real_))
  name_clean <- tolower(trimws(name))
  tbl_clean  <- tolower(.uk_hospital_coords$name)
  idx <- which(tbl_clean == name_clean)
  if (length(idx) == 0)
    idx <- which(grepl(name_clean, tbl_clean, fixed = TRUE))
  if (length(idx) == 0)
    idx <- which(vapply(tbl_clean, function(t) grepl(t, name_clean, fixed = TRUE), logical(1)))
  if (length(idx) == 0) {
    message("hospital_latlon: no match found for '", name, "'")
    return(list(lat = NA_real_, lon = NA_real_))
  }
  list(lat = .uk_hospital_coords$lat[idx[1]],
       lon = .uk_hospital_coords$lon[idx[1]])
}

# ── Hospital → city lookup ─────────────────────────────────────────────────────
.hospital_city_lookup <- c(
  "Musgrove Park Hospital"="Taunton","Taunton"="Taunton",
  "Queen Elizabeth Hospital Birmingham"="Birmingham","QE Birmingham"="Birmingham",
  "University Hospital Birmingham"="Birmingham",
  "Leeds General Infirmary"="Leeds","Leeds Teaching Hospitals"="Leeds","Leeds"="Leeds",
  "St Mark's Hospital"="London","Harrow"="London",
  "Manchester Royal Infirmary"="Manchester","Manchester University NHS"="Manchester","Manchester"="Manchester",
  "Sheffield Teaching Hospitals"="Sheffield","Northern General Hospital"="Sheffield","Sheffield"="Sheffield",
  "Bristol Royal Infirmary"="Bristol","Bristol"="Bristol",
  "Oxford University Hospitals"="Oxford","John Radcliffe Hospital"="Oxford","Oxford"="Oxford",
  "Royal Liverpool University Hospital"="Liverpool","Liverpool"="Liverpool",
  "Freeman Hospital Newcastle"="Newcastle","Newcastle Freeman"="Newcastle",
  "Newcastle"="Newcastle","Royal Victoria Infirmary"="Newcastle",
  "Nottingham University Hospitals"="Nottingham","Queen's Medical Centre"="Nottingham","Nottingham"="Nottingham",
  "Leicester Royal Infirmary"="Leicester","Leicester"="Leicester",
  "Addenbrooke's Hospital"="Cambridge","Cambridge"="Cambridge",
  "Southampton General Hospital"="Southampton","Southampton"="Southampton",
  "King's College Hospital"="London","Guy's Hospital"="London",
  "St Thomas' Hospital"="London","Royal Free Hospital"="London",
  "Hammersmith Hospital"="London","Imperial College Healthcare"="London",
  "London"="London","St George's Hospital"="London","Tooting"="London",
  "University College London Hospital"="London","UCLH"="London",
  "Aintree University Hospital"="Liverpool",
  "Royal Preston Hospital"="Preston","Preston"="Preston",
  "Blackpool Teaching Hospitals"="Blackpool","Blackpool"="Blackpool",
  "Countess of Chester Hospital"="Chester","Chester"="Chester",
  "Royal Stoke University Hospital"="Stoke-on-Trent","Stoke-on-Trent"="Stoke-on-Trent","Stoke"="Stoke-on-Trent",
  "University Hospital Coventry"="Coventry","Coventry"="Coventry",
  "Worcestershire Royal Hospital"="Worcester","Worcester"="Worcester",
  "Hereford County Hospital"="Hereford","Hereford"="Hereford",
  "Royal Shrewsbury Hospital"="Shrewsbury","Shrewsbury"="Shrewsbury",
  "Morriston Hospital"="Swansea","Swansea"="Swansea",
  "University Hospital of Wales"="Cardiff","Cardiff"="Cardiff",
  "Royal Gwent Hospital"="Newport","Newport"="Newport",
  "Wrexham Maelor Hospital"="Wrexham","Wrexham"="Wrexham",
  "Aberdeen Royal Infirmary"="Aberdeen","Aberdeen"="Aberdeen",
  "Royal Infirmary Edinburgh"="Edinburgh","Edinburgh"="Edinburgh",
  "Glasgow Royal Infirmary"="Glasgow","Queen Elizabeth University Hospital Glasgow"="Glasgow","Glasgow"="Glasgow",
  "Ninewells Hospital"="Dundee","Dundee"="Dundee",
  "Derriford Hospital"="Plymouth","Plymouth"="Plymouth",
  "Royal Devon and Exeter"="Exeter","Exeter"="Exeter",
  "Royal Cornwall Hospital"="Truro","Truro"="Truro",
  "Yeovil District Hospital"="Yeovil","Yeovil"="Yeovil",
  "Royal United Hospital Bath"="Bath","Bath"="Bath",
  "Cheltenham General Hospital"="Cheltenham","Cheltenham"="Cheltenham",
  "Gloucestershire Royal Hospital"="Gloucester","Gloucester"="Gloucester"
)

hospital_city <- function(name) {
  if (is.null(name) || !nzchar(trimws(name))) return(NA_character_)
  nm <- trimws(name)
  # exact match first
  hit <- .hospital_city_lookup[nm]
  if (!is.na(hit)) return(unname(hit))
  # case-insensitive fallback
  idx <- which(tolower(names(.hospital_city_lookup)) == tolower(nm))
  if (length(idx)) return(unname(.hospital_city_lookup[idx[1]]))
  NA_character_
}

get_hospital_names <- function() sort(unique(.uk_hospital_coords$name))

# ── Other helpers ─────────────────────────────────────────────────────────────
clean_df_names <- function(x) {
  names(x) <- iconv(names(x), from = "UTF-8", to = "ASCII", sub = "")
  names(x) <- tolower(gsub("[^a-z0-9]+", "_", trimws(names(x))))
  names(x) <- gsub("^_+|_+$", "", names(x))
  x
}

next_site_id <- function(sites_df) {
  prefix <- toupper(
    (current_trial_config()$short_name %||% current_trial_config()$code) %||% "SITE"
  )
  if (nrow(sites_df) == 0) return(sprintf("%s-001", prefix))
  existing <- suppressWarnings(as.integer(na.omit(str_extract(sites_df$site_id, "\\d+"))))
  sprintf("%s-%03d", prefix, if (length(existing) == 0) 1L else max(existing) + 1L)
}

# Resolve a trial logo path to an absolute path. Reports render in a temp
# intermediates_dir (so relative paths like "trials/<code>/www/logo.png" break
# during knit); making it absolute up-front — while we're still in the app's
# working directory — lets the report header embed the logo reliably.
resolve_logo_path <- function(cfg) {
  if (is.null(cfg)) return(NULL)
  lf <- cfg$logo_file
  if (is.null(lf) || !nzchar(lf)) return(NULL)
  normalizePath(lf, winslash = "/", mustWork = FALSE)
}

find_latest_csv <- function(data_dir = DATA_DIR) {
  if (!dir.exists(data_dir)) return(NULL)
  csvs <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  csvs <- csvs[!grepl("return[ _-]?rate", basename(csvs), ignore.case = TRUE)]
  if (length(csvs) == 0) return(NULL)
  csvs[order(file.info(csvs)$mtime, decreasing = TRUE)][1]
}

list_csvs <- function(data_dir = DATA_DIR) {
  if (!dir.exists(data_dir))
    return(tibble(file = character(), modified = as.POSIXct(character()), path = character()))
  csvs <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  csvs <- csvs[!grepl("return[ _-]?rate", basename(csvs), ignore.case = TRUE)]
  if (length(csvs) == 0)
    return(tibble(file = character(), modified = as.POSIXct(character()), path = character()))
  info <- file.info(csvs)
  tibble(file = basename(csvs), modified = info$mtime, path = csvs) %>% arrange(desc(modified))
}

read_redcap_file <- function(filepath) {
  raw <- readBin(filepath, "raw", file.info(filepath)$size)
  bom <- as.raw(c(0xef, 0xbb, 0xbf))
  if (length(raw) >= 3 && identical(raw[1:3], bom)) {
    tmp <- tempfile(fileext = ".csv"); writeBin(raw[4:length(raw)], tmp); filepath <- tmp
  }
  for (sep in c(",", "\t")) {
    df <- tryCatch(read.table(filepath, header = TRUE, sep = sep, stringsAsFactors = FALSE,
                              check.names = FALSE, encoding = "UTF-8", quote = '"',
                              fill = TRUE, comment.char = ""), error = function(e) NULL)
    if (!is.null(df) && ncol(df) > 2) return(df)
  }
  stop("Could not parse file. Export from REDCap as CSV.")
}

#' Read one REDCap export per work package and combine them.
#' For platform / multi-WP trials configured with `work_package_data_dirs`
#' (one folder per work package). Reads the newest CSV in each folder, tags
#' every row with its work-package index (and name), then row-binds them into a
#' single frame that process_redcap() handles like any other export.
#' @param wp_dirs   Character vector of folder paths, aligned to work_packages.
#' @param wp_labels Optional labels (cfg$work_packages) for work_package_name.
#' @return list(raw, files, first_path, n_wp). raw is NULL if nothing loaded.
read_wp_exports <- function(wp_dirs, wp_labels = NULL) {
  dfs <- list(); files <- character(0); first_path <- NULL
  for (i in seq_along(wp_dirs)) {
    d <- trimws(wp_dirs[i] %||% "")
    if (!nzchar(d) || !dir.exists(d)) next
    fp <- find_latest_csv(d)
    if (is.null(fp)) next
    df_i <- tryCatch(read_redcap_file(fp), error = function(e) NULL)
    if (is.null(df_i) || !nrow(df_i)) next
    # Tag rows with the work package this export belongs to. Overwrites any
    # existing column so a per-WP export is always attributed correctly.
    df_i$work_package <- i
    if (!is.null(wp_labels) && length(wp_labels) >= i)
      df_i$work_package_name <- as.character(wp_labels[i])
    dfs[[length(dfs) + 1]] <- df_i
    files <- c(files, basename(fp))
    if (is.null(first_path)) first_path <- fp
  }
  if (!length(dfs))
    return(list(raw = NULL, files = character(0), first_path = NULL, n_wp = 0L))
  list(raw = dplyr::bind_rows(dfs), files = files,
       first_path = first_path, n_wp = length(dfs))
}

process_redcap <- function(raw_df, current_sites) {
  df   <- clean_df_names(raw_df)
  orig <- names(raw_df)
  rec_col  <- names(df)[names(df) == "record_id"][1]
  if (is.na(rec_col)) rec_col <- names(df)[str_detect(names(df), "record")][1]
  if (is.na(rec_col)) rec_col <- names(df)[1]
  evt_col  <- names(df)[str_detect(names(df), "^redcap_event_name$|^event_name$")][1]
  if (is.na(evt_col)) evt_col <- names(df)[str_detect(names(df), "event")][1]
  dag_col  <- names(df)[names(df) == "site_name"][1]
  if (is.na(dag_col)) dag_col <- names(df)[str_detect(names(df), "^dag$")][1]
  if (is.na(dag_col)) dag_col <- names(df)[str_detect(names(df), "site|dag")][1]
  rand_col <- names(df)[str_detect(names(df), "rand_dttm|rand_date")][1]
  # Work-package column (platform / multi-WP trials). The export carries an
  # integer index that matches cfg$work_packages order; NA for single-WP trials.
  wp_col   <- names(df)[names(df) == "work_package"][1]
  if (is.na(wp_col)) wp_col <- names(df)[str_detect(names(df), "^work_package$|^workpackage$|^wp_code$|^wp$")][1]
  diag <- list(ncol = ncol(df), nrow = nrow(df), all_cols = paste(orig, collapse = ", "),
               rec_col = rec_col %||% "NOT FOUND", evt_col = evt_col %||% "NOT FOUND",
               dag_col = dag_col %||% "NOT FOUND", rand_col = rand_col %||% "NOT FOUND")
  if (is.na(rec_col) || is.na(evt_col))
    stop(sprintf("Could not find required columns. Found: %s",
                 paste(orig[1:min(8, length(orig))], collapse = ", ")))
  if (rec_col != "record_id") df <- df %>% rename(record_id = all_of(rec_col))
  
  # Build event_type from the active trial's redcap_events mapping.
  # For each logical role (baseline, discharge, day_30, day_90, ...) we accept
  # an exact match against the configured event name(s), then fall back to a
  # fuzzy regex match for backwards compatibility with TONIC-style names.
  cfg_evts <- (current_trial_config() %||% list())$redcap_events %||% list()
  pretty <- function(role) {
    role |>
      gsub("_", " ", x = _) |>
      tools::toTitleCase()
  }
  classify_event <- function(raw) {
    raw_t <- trimws(raw)
    if (is.na(raw_t) || nchar(raw_t) == 0) return(NA_character_)
    for (role in names(cfg_evts)) {
      vals <- cfg_evts[[role]]
      if (is.null(vals)) next
      if (raw_t %in% vals) return(pretty(role))
    }
    # Fallback fuzzy matching for unmapped/unknown trials.
    if (grepl("baseline",  raw_t, ignore.case = TRUE)) return("Baseline")
    if (grepl("discharge", raw_t, ignore.case = TRUE)) return("Discharge")
    if (grepl("day.?30",   raw_t, ignore.case = TRUE)) return("Day 30")
    if (grepl("day.?90",   raw_t, ignore.case = TRUE)) return("Day 90")
    raw_t
  }
  
  df <- df %>%
    mutate(
      record_id  = trimws(as.character(record_id)),
      event_type = vapply(.data[[evt_col]], classify_event, character(1)),
      site_dag   = if (!is.na(dag_col)) trimws(as.character(.data[[dag_col]])) else NA_character_,
      work_package = if (!is.na(wp_col)) suppressWarnings(as.integer(.data[[wp_col]])) else NA_integer_,
      .rand_dttm = if (!is.na(rand_col)) .data[[rand_col]] else NA_character_
    ) %>%
    filter(!is.na(record_id), nchar(record_id) > 0, record_id != "NA") %>%
    arrange(record_id) %>%
    group_by(record_id) %>%
    mutate(
      site_dag = dplyr::first(site_dag[!is.na(site_dag) & nchar(site_dag) > 0]),
      # A participant's WP is fixed; carry the first non-NA value across all
      # their event rows so baseline/discharge/sub-form rows agree.
      work_package = {
        wpv <- work_package[!is.na(work_package)]
        if (length(wpv)) wpv[1] else NA_integer_
      }
    ) %>%
    ungroup()
  participants <- df %>% select(record_id, event_type, site_dag, work_package) %>% distinct()
  dag_summary  <- df %>%
    filter(!is.na(site_dag), nchar(trimws(site_dag)) > 0) %>%
    group_by(site_dag) %>%
    summarise(
      rand_count = if (!is.na(rand_col))
        n_distinct(record_id[!is.na(.rand_dttm) & nchar(trimws(.rand_dttm)) > 0])
      else n_distinct(record_id[event_type == "Baseline"]),
      .groups = "drop")
  updated_sites <- current_sites
  # Statuses we'll never overwrite from data — these are manual decisions
  # ("Paused" / "Closed" / "Set-up" / "Open"). The auto-derive only updates
  # sites that are still in the default "Identified" state (or have no
  # status set at all). Once a user picks a real status on the Sites tab,
  # re-uploading a CSV preserves it.
  auto_override_allowed <- c("Identified", NA_character_, "")
  
  for (i in seq_len(nrow(dag_summary))) {
    dag_name   <- dag_summary$site_dag[i]
    rand_n     <- as.integer(dag_summary$rand_count[i])
    new_status <- if (rand_n > 0) "Recruiting" else "Identified"
    existing   <- which(updated_sites$site_name == dag_name)
    if (length(existing) > 0) {
      updated_sites$randomised[existing[1]] <- rand_n
      cur_status <- updated_sites$status[existing[1]]
      if (cur_status %in% auto_override_allowed) {
        updated_sites$status[existing[1]] <- new_status
      }
    } else {
      # Auto-lookup coordinates from hospital name
      ll <- hospital_latlon(dag_name)
      updated_sites <- bind_rows(updated_sites, tibble(
        site_id       = next_site_id(updated_sites),
        site_name     = dag_name,
        city          = NA_character_,
        region        = NA_character_,
        status        = new_status,
        site_open_date = NA_Date_,
        monthly_target = 2L,
        target        = 42L,
        randomised    = rand_n,
        lat           = ll$lat,
        lon           = ll$lon,
        source        = "auto"))
    }
  }
  list(participants = participants, sites = updated_sites, raw_data = df, diagnostics = diag)
}

parse_safety <- function(raw_df) {
  if (is.null(raw_df) || nrow(raw_df) == 0) return(NULL)
  df <- raw_df
  gc <- function(col) if (col %in% names(df)) df[[col]] else rep(NA, nrow(df))
  
  sae_col <- fld("sae_complete")
  cos_col <- fld("cos_type")
  pn_col  <- fld("pregnancy_notification_complete")
  po_col  <- fld("pregnancy_outcome_complete")
  dev_col <- fld("deviation_complete")
  
  sae_vals    <- suppressWarnings(as.integer(gc(sae_col)))
  cos_vals    <- as.character(gc(cos_col))
  preg_n_vals <- suppressWarnings(as.integer(gc(pn_col)))
  preg_o_vals <- suppressWarnings(as.integer(gc(po_col)))
  dev_vals    <- if (dev_col %in% names(df))
    suppressWarnings(as.integer(df[[dev_col]])) else rep(NA_integer_, nrow(df))
  tibble(
    record_id  = df$record_id, site_dag = df$site_dag, event_type = df$event_type,
    sae        = !is.na(sae_vals)    & sae_vals == 2L,
    withdrawn  = !is.na(cos_vals)    & nchar(trimws(cos_vals)) > 0 & cos_vals != "NA",
    cos_type   = cos_vals,
    preg_notif = !is.na(preg_n_vals) & preg_n_vals == 2L,
    preg_out   = !is.na(preg_o_vals) & preg_o_vals == 2L,
    deviation  = !is.na(dev_vals)    & dev_vals == 2L
  )
}

make_monthly_df <- function(log_df, sites_df, site_filter = NULL) {
  
  # --- Standardise input formats (handles BOTH log + REDCap import) ---
  rand_field <- fld("randomisation_datetime", "rand_dttm_s")
  if (rand_field %in% names(log_df)) {
    rands <- log_df %>%
      dplyr::mutate(
        site_id   = dplyr::coalesce(site_id, site_name),
        timestamp = as.POSIXct(trimws(.data[[rand_field]]),
                               format = "%d/%m/%Y %H:%M",
                               tz = "UTC"),
        action    = "+1",
        month     = lubridate::floor_date(timestamp, "month")
      )
  } else {
    rands <- log_df %>%
      dplyr::filter(action %in% c("+1", 1, "1", "add")) %>%
      dplyr::mutate(
        timestamp = as.POSIXct(timestamp, tz = "UTC"),
        month     = lubridate::floor_date(timestamp, "month")
      )
  }
  
  rands <- rands %>%
    dplyr::filter(!is.na(month))
  
  # --- Clean sites ---
  sites_open <- sites_df %>%
    dplyr::mutate(
      site_open_date = as.Date(site_open_date),
      site_open_date = dplyr::if_else(
        is.na(site_open_date),
        as.Date("2024-01-01"),
        site_open_date
      ),
      monthly_target = as.numeric(monthly_target)
    )
  
  # --- Optional filter ---
  if (!is.null(site_filter) && length(site_filter) > 0) {
    sites_open <- sites_open %>% dplyr::filter(site_id %in% site_filter)
    rands      <- rands %>% dplyr::filter(site_id %in% site_filter)
  }
  
  # --- Month grid ---
  all_months <- seq(
    lubridate::floor_date(min(sites_open$site_open_date, na.rm = TRUE), "month"),
    lubridate::floor_date(Sys.Date(), "month"),
    by = "month"
  )
  
  grid <- tidyr::expand_grid(
    sites_open %>% dplyr::select(site_id, site_name, site_open_date, monthly_target),
    month = all_months
  ) %>%
    dplyr::filter(month >= site_open_date) %>%
    dplyr::select(site_id, site_name, month, monthly_target)
  
  # --- Monthly counts ---
  monthly_act <- rands %>%
    dplyr::count(site_id, month, name = "actual")
  
  # --- Final output ---
  df <- grid %>%
    dplyr::left_join(monthly_act, by = c("site_id", "month")) %>%
    tidyr::replace_na(list(actual = 0)) %>%
    dplyr::arrange(site_id, month) %>%
    dplyr::group_by(site_id) %>%
    dplyr::mutate(
      cum_actual = cumsum(actual),
      cum_target = cumsum(monthly_target)
    ) %>%
    dplyr::ungroup()
  
  if (nrow(df) == 0) return(NULL)
  df
}
make_overall_df <- function(md) {
  if (is.null(md) || nrow(md) == 0) return(NULL)
  md %>% group_by(month) %>%
    summarise(actual = sum(actual), monthly_target = sum(monthly_target), .groups = "drop") %>%
    arrange(month) %>%
    mutate(month       = as.Date(month),
           month_label = format(month, "%b %Y"),
           cum_actual  = cumsum(actual),
           cum_target  = cumsum(monthly_target))
}

make_map_icon <- function(count, status) {
  col <- switch(status, Recruiting = "#2EC4A5", Open = "#3B82F6",
                "Set-up" = "#F59E0B", Closed = "#EF4444", "#94A3B8")
  sz  <- if (count > 0) max(32L, min(52L, as.integer(24 + count * 1.1))) else 20L
  lbl <- if (count > 0)
    sprintf('<text x="%d" y="%d" text-anchor="middle" dominant-baseline="central" fill="white" font-size="%d" font-weight="700" font-family="Outfit,sans-serif">%d</text>',
            sz %/% 2L, sz %/% 2L, max(9L, sz %/% 3L), count) else ""
  svg <- sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d"><circle cx="%d" cy="%d" r="%d" fill="%s" stroke="white" stroke-width="2.5"/>%s</svg>',
                 sz, sz, sz %/% 2L, sz %/% 2L, sz %/% 2L - 2L, col, lbl)
  makeIcon(iconUrl     = paste0("data:image/svg+xml;charset=utf-8,", URLencode(svg, reserved = TRUE)),
           iconWidth   = sz, iconHeight   = sz,
           iconAnchorX = sz %/% 2L, iconAnchorY = sz %/% 2L,
           popupAnchorX = 0L, popupAnchorY = as.integer(-(sz %/% 2L) - 4L))
}

make_popup <- function(site_name, site_id, region, status, rand, target) {
  pct <- if (target > 0) min(100, round(100 * rand / target)) else 0
  sprintf(
    '<div style="font-family:Outfit,sans-serif;min-width:190px;">
    <div style="font-size:15px;font-weight:700;color:#1B4F6B;margin-bottom:2px;">%s</div>
    <div style="font-size:11px;color:#64748B;margin-bottom:10px;">%s &bull; %s</div>
    <table style="font-size:12px;width:100%%;border-collapse:collapse;">
      <tr><td style="color:#64748B;padding:3px 10px 3px 0">Status</td><td><strong>%s</strong></td></tr>
      <tr><td style="color:#64748B;padding:3px 10px 3px 0">Randomised</td><td><strong>%d / %d</strong></td></tr>
    </table>
    <div style="background:#E2EAF0;border-radius:4px;height:6px;margin:8px 0 3px;">
      <div style="width:%d%%;height:6px;border-radius:4px;background:linear-gradient(90deg,#0FA88E,#2EC4A5);"></div></div>
    <span style="font-size:10px;color:#64748B">%d%% of site target</span></div>',
    site_name, site_id, coalesce(region, ""), status, rand, target, pct, pct)
}

hm_cell_html <- function(actual, target) {
  if (is.na(actual)) return('<span class="hm-zero">&mdash;</span>')
  pct <- if (!is.na(target) && target > 0) actual / target else 0
  cls <- if (pct >= 1.0) "hm-green" else if (pct >= 0.8) "hm-amber" else if (actual > 0) "hm-red" else "hm-zero"
  if (actual == 0) return('<span class="hm-zero">&mdash;</span>')
  sprintf('<span class="%s">%d</span>', cls, as.integer(actual))
}

comp_label <- function(v) {
  dplyr::case_when(
    is.na(v) ~ "<span class='c-none'>&mdash;</span>",
    v == 2   ~ "<span class='c-complete'>&check;</span>",
    v == 1   ~ "<span class='c-unverified'>?</span>",
    v == 0   ~ "<span class='c-partial'>&#9684;</span>",
    TRUE     ~ "<span class='c-none'>&mdash;</span>"
  )
}

delta_badge_ui <- function(current, previous, suffix = "") {
  if (is.null(previous)) return(NULL)
  diff <- current - previous
  if (diff == 0) {
    return(span(class = "delta-badge pov-delta-badge flat",
                paste0("→ 0", suffix)))
  }
  if (diff > 0) {
    span(class = "delta-badge pov-delta-badge up",
         paste0("↑ ", abs(diff), suffix))
  } else {
    span(class = "delta-badge delta-neg pov-delta-badge down",
         paste0("↓ ", abs(diff), suffix))
  }
}

e_tonic <- function(p) {
  p %>%
    e_tooltip(trigger = "axis",
              backgroundColor = "rgba(27,79,107,0.92)",
              borderColor = "#1B4F6B",
              textStyle = list(color = "#fff", fontFamily = "Outfit")) %>%
    e_legend(bottom = 0, textStyle = list(fontFamily = "Outfit", fontSize = 11, color = col_muted)) %>%
    e_grid(left = "8%", right = "4%", bottom = "18%", top = "8%") %>%
    e_x_axis(axisLabel = list(fontFamily = "Outfit", fontSize = 11, color = col_muted),
             axisLine  = list(lineStyle = list(color = col_muted))) %>%
    e_y_axis(axisLabel  = list(fontFamily = "Outfit", fontSize = 11, color = col_muted),
             splitLine  = list(lineStyle = list(color = "#E2EAF0")),
             min = 0) %>%
    e_color(c(col_teal, col_navy, col_amber, "#A78BFA", "#F97316", "#EC4899"))
}

# build_participant_table renders a wide HTML table whose timepoint columns are
# defined by the active trial's participant_table_layout. Each instrument cell
# reads from df[[instrument$col]] (or "<event>_<field-without-_complete>" by
# default). If no layout is configured, only Record ID and Site are shown.
build_participant_table <- function(df, layout = NULL) {
  cfg    <- current_trial_config()
  layout <- layout %||% cfg$participant_table_layout
  tps    <- layout$timepoints
  
  icon <- function(v) {
    dplyr::case_when(
      is.na(v)  ~ '<span class="c-none">&mdash;</span>',
      v == 2L   ~ '<span class="c-complete">&#10003;</span>',
      v == 1L   ~ '<span class="c-partial">&#9679;</span>',
      v == 0L   ~ '<span class="c-none">&mdash;</span>',
      TRUE      ~ '<span class="c-none">&mdash;</span>'
    )
  }
  
  # No layout configured: minimal Record ID + Site table.
  if (is.null(tps) || length(tps) == 0) {
    rows <- vapply(seq_len(nrow(df)), function(i) sprintf(
      '<tr><td>%s</td><td>%s</td></tr>',
      htmltools::htmlEscape(df$record_id[i]),
      htmltools::htmlEscape((df$site_dag %||% rep("", nrow(df)))[i])
    ), character(1))
    return(HTML(paste0(
      '<table><thead><tr><th style="text-align:left">Record ID</th><th style="text-align:left">Site</th></tr></thead><tbody>',
      paste(rows, collapse = "\n"), '</tbody></table>')))
  }
  
  cell_col <- function(tp, ins) {
    ins$col %||% sprintf("%s_%s", tp$event %||% tolower(tp$name),
                         sub("_complete$", "", ins$field %||% ""))
  }
  
  hdr1 <- paste(vapply(tps, function(tp) sprintf(
    '<th colspan="%d" style="text-align:center">%s</th>',
    length(tp$instruments), htmltools::htmlEscape(tp$name %||% "")
  ), character(1)), collapse = "")
  
  hdr2 <- paste(vapply(tps, function(tp) paste(vapply(tp$instruments, function(ins)
    sprintf('<th>%s</th>', htmltools::htmlEscape(ins$label %||% "")),
    character(1)), collapse = ""), character(1)), collapse = "")
  
  header <- sprintf('
  <table>
  <thead>
    <tr style="background:#F8FAFD">
      <th rowspan="2" style="text-align:left;border-right:2px solid #EEF3F8;min-width:90px;background:#F8FAFD">Record ID</th>
      <th rowspan="2" style="text-align:left;border-right:2px solid #EEF3F8;min-width:110px;background:#F8FAFD">Site</th>
      %s
    </tr>
    <tr>%s</tr>
  </thead>
  <tbody>', hdr1, hdr2)
  
  cell_lookup <- unlist(lapply(tps, function(tp)
    vapply(tp$instruments, function(ins) cell_col(tp, ins), character(1))))
  
  rows <- vapply(seq_len(nrow(df)), function(i) {
    cells <- paste(vapply(cell_lookup, function(c) sprintf(
      '<td>%s</td>',
      icon(if (c %in% names(df)) df[[c]][i] else NA)
    ), character(1)), collapse = "")
    sprintf('<tr><td class="sid" style="text-align:left;border-right:2px solid #EEF3F8">%s</td><td style="text-align:left;border-right:2px solid #EEF3F8">%s</td>%s</tr>',
            htmltools::htmlEscape(df$record_id[i]),
            htmltools::htmlEscape((df$site_dag %||% rep("", nrow(df)))[i]),
            cells)
  }, character(1))
  
  HTML(paste0(header, paste(rows, collapse = "\n"), "</tbody></table>"))
}

xlsx_download <- function(data_fn, fname) {
  downloadHandler(
    filename = function() paste0(fname, "_", Sys.Date(), ".xlsx"),
    content  = function(path) {
      data <- data_fn()
      write_xlsx(if (!is.null(data) && nrow(data) > 0) data else data.frame(Message = "No data"), path)
    }
  )
}

# ── Fallback empty chart/table helpers ────────────────────────────────────────
# These are used when no data is available. If empty_echart / empty_reactable
# are already defined in ui_helpers.R or layout.R, those definitions take
# precedence (source order matters — source helpers.R before those files).

if (!exists("empty_echart")) {
  empty_echart <- function(msg = "No data available") {
    data.frame(x = 0, y = 0) |>
      echarts4r::e_charts(x) |>
      echarts4r::e_scatter(y, symbol_size = 0) |>
      echarts4r::e_title(subtext = msg,
                         subtextStyle = list(
                           color     = "#94A3B8",
                           fontSize  = 13,
                           fontFamily = "Outfit, sans-serif"
                         )) |>
      echarts4r::e_legend(show = FALSE) |>
      echarts4r::e_x_axis(show = FALSE) |>
      echarts4r::e_y_axis(show = FALSE) |>
      echarts4r::e_grid(top = "40%")
  }
}

if (!exists("empty_reactable")) {
  empty_reactable <- function(msg = "No data available") {
    reactable::reactable(
      data.frame(Message = msg),
      columns = list(
        Message = reactable::colDef(
          style = list(color = "#94A3B8", fontStyle = "italic",
                       fontFamily = "Outfit, sans-serif", fontSize = "12px")
        )
      ),
      bordered = FALSE, highlight = FALSE, compact = TRUE
    )
  }
}