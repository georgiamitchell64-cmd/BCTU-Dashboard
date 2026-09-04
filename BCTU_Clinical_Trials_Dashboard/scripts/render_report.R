# =============================================================================
# Render a trial's TMG/iTMG report without the dashboard
# =============================================================================
# A fallback for when the app is unavailable, or to check a report against a
# CSV by hand. Uses exactly the same config, recruitment rules and template the
# dashboard uses, so the numbers match.
#
# Run from the app directory (BCTU_Clinical_Trials_Dashboard):
#
#   Rscript scripts/render_report.R --trial=panorama --csv=path/to/export.csv
#
# Options
#   --trial=<code>   trial folder under trials/            (default: panorama)
#   --csv=<path>     REDCap CSV export                     (default: newest CSV
#                    in the trial's data folder, or in its wp*/ folders for a
#                    multi-work-package trial)
#   --wp=<n>         work package to scope the report to   (default: none)
#   --out=<path>     output HTML                           (default:
#                    <trial>_tmg_report_<date>.html in the working directory)
#   --kind=<tonic|tsc>  which template                     (default: tonic, the
#                    TMG/iTMG report)
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
opt  <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^--", name, "="), "", hit[1])
}

trial_code <- opt("trial", "panorama")
csv_path   <- opt("csv")
wp_arg     <- opt("wp")
out_path   <- opt("out")
kind       <- opt("kind", "tonic")

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(lubridate)
  library(tibble); library(rlang); library(jsonlite)
})

# The app's own logic, minus the Shiny modules.
for (f in c("globals/constants.R", "globals/datasets.R", "globals/trial_config.R",
            "functions/helpers.R", "functions/trial_overrides.R",
            "functions/recruitment.R", "functions/prepare_report_data.R",
            "functions/consort_flow.R", "functions/flat_completeness.R",
            "functions/baseline_table.R", "functions/codebook.R"))
  source(f)

trials <- discover_trials()
cfg    <- trials[[trial_code]]
if (is.null(cfg))
  stop("No trial '", trial_code, "'. Available: ",
       paste(names(trials), collapse = ", "))
apply_trial_globals(cfg)

wp_index <- if (is.null(wp_arg)) NULL else suppressWarnings(as.integer(wp_arg))

# ── Data ────────────────────────────────────────────────────────────────────
# Same precedence the dashboard uses: one export per work package for a
# multi-WP trial, otherwise the newest CSV in the trial's data folder.
if (is.null(csv_path)) {
  if (trial_is_multi_wp(cfg)) {
    res <- read_wp_exports(wp_data_dirs(cfg), cfg$work_packages)
    raw <- res$raw
    if (is.null(raw))
      stop("No CSVs in the work-package folders:\n  ",
           paste(wp_data_dirs(cfg), collapse = "\n  "))
    message("Read ", res$n_wp, " work-package export(s): ",
            paste(res$files, collapse = ", "))
  } else {
    data_dir <- cfg$data_dir %||% file.path(getwd(), "trials", cfg$code, "data")
    csv_path <- find_latest_csv(data_dir)
    if (is.null(csv_path)) stop("No CSV found in ", data_dir)
    message("Read ", csv_path)
    raw <- read_redcap_file(csv_path)
  }
} else {
  message("Read ", csv_path)
  raw <- read_redcap_file(csv_path)
}

# Scope to one work package, as the dashboard's WP picker does.
if (!is.null(wp_index) && !is.na(wp_index) && "work_package" %in% names(raw))
  raw <- raw[!is.na(raw$work_package) & raw$work_package == wp_index, , drop = FALSE]

# ── What the recruitment rules make of it ───────────────────────────────────
rc <- recruitment_counts(raw, cfg)
message(sprintf(
  "Recruitment (%s): screened %d · eligible %d · approached %d · recruited %d · screened only %d",
  rc$spec$basis, rc$n_screened, rc$n_eligible, rc$n_approached,
  rc$n_recruited, rc$n_screened_only))

report_data <- prepare_report_data(raw)

# ── Render ──────────────────────────────────────────────────────────────────
tmpl <- resolve_report_template(cfg, kind)
if (is.null(tmpl)) stop("No '", kind, "' template for ", trial_code)
if (is.null(out_path))
  out_path <- sprintf("%s_%s_report_%s.html", trial_code, kind,
                      format(Sys.Date(), "%Y%m%d"))
out_path <- normalizePath(out_path, mustWork = FALSE)

# Render from a copy so the template's siblings (logos, sourced helpers) sit
# beside it exactly as the dashboard stages them.
tmp_dir  <- file.path(tempdir(), paste0("report_", trial_code))
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
rmd_dest <- file.path(tmp_dir, basename(tmpl))
file.copy(tmpl, rmd_dest, overwrite = TRUE)
for (f in c("functions/consort_flow.R", "functions/flat_completeness.R",
            "functions/baseline_table.R", "www/BlackText-landscape.png",
            "www/NIHR_Acknowledgement_Funded by_Logo_RGB.png"))
  if (file.exists(f)) file.copy(f, file.path(tmp_dir, basename(f)), overwrite = TRUE)

rmarkdown::render(
  input         = rmd_dest,
  output_file   = out_path,
  output_format = "html_document",
  # Drop anything this template does not declare, as the dashboard does.
  params = filter_params_for_rmd(list(
    report_data    = report_data,
    selected_sites = "All sites",
    report_date    = format(Sys.Date(), "%d %B %Y"),
    report_type    = "TMG",
    report_content = cfg$report_content,
    column_labels  = cfg$column_labels,
    work_package   = wp_report_context(cfg, wp_index),
    logo_path      = cfg$logo_file), rmd_dest),
  envir             = new.env(parent = globalenv()),
  intermediates_dir = tmp_dir,
  clean             = TRUE,
  quiet             = FALSE)

message("Wrote ", out_path)
