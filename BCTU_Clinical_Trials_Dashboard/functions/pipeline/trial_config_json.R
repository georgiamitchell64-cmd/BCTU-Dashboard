# =============================================================================
# Declarative trial configuration (trial.json)
# =============================================================================
# One JSON document per trial replaces executable config.R
# (docs/ARCHITECTURE.md §9). Layout:
#
#   {
#     "config_version": 1,
#     "meta":    { code, name, short_name, trial_target, category, colors },
#     "source":  { adapter, data_dir },
#     "mapping": { source_fingerprint,
#                  concepts: { "<concept.id>": {field, confirmed_by, confirmed_at} },
#                  events:   { "<role>": {source_events: [...]} },
#                  decodes:  { "<concept.id>": {"<code>": "<canonical>"} } },
#     "modules": { "<module_id>": { enabled: true/false, ... } }
#   }
#
# Migration model: if trials/<code>/trial.json exists it overlays config.R
# (deep merge, JSON wins per key), so trials can move over incrementally.
# A trial with ONLY a trial.json needs no R code at all. `overrides.json`
# continues to apply last, preserving existing UI-driven edits.
# =============================================================================

TRIAL_CONFIG_VERSION <- 1L

trial_json_path <- function(trial_dir) file.path(trial_dir, "trial.json")

#' Load trials/<code>/trial.json (NULL when absent or unreadable).
load_trial_json <- function(trial_dir) {
  path <- trial_json_path(trial_dir)
  if (!file.exists(path)) return(NULL)
  tryCatch({
    tj <- jsonlite::fromJSON(path, simplifyVector = TRUE,
                             simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
    v <- tj$config_version %||% 1L
    if (v > TRIAL_CONFIG_VERSION)
      message("trial.json at ", path, " has config_version ", v,
              " (this app understands ", TRIAL_CONFIG_VERSION, ")")
    tj
  }, error = function(e) {
    message("trial.json read error for ", path, ": ", e$message)
    NULL
  })
}

save_trial_json <- function(trial_dir, tj) {
  tj$config_version <- tj$config_version %||% TRIAL_CONFIG_VERSION
  jsonlite::write_json(tj, trial_json_path(trial_dir),
                       auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(trial_json_path(trial_dir))
}

#' Validate the minimal structural requirements of a trial.json.
#' @return character vector of problems (empty when valid)
validate_trial_json <- function(tj) {
  problems <- character(0)
  if (is.null(tj$meta) || !is.list(tj$meta)) {
    problems <- c(problems, "missing 'meta' block")
  } else {
    for (k in c("code", "name", "short_name"))
      if (!nzchar(as.character(tj$meta[[k]] %||% "")))
        problems <- c(problems, paste0("missing meta.", k))
  }
  m <- tj$mapping
  if (!is.null(m) && !is.null(m$concepts)) {
    reg <- concept_registry()
    unknown <- setdiff(names(m$concepts), names(reg))
    if (length(unknown))
      problems <- c(problems,
                    paste0("unknown concept id(s): ",
                           paste(unknown, collapse = ", ")))
  }
  problems
}

#' Flatten a trial.json into the legacy cfg list shape the app runs on today,
#' overlaying an existing config.R-derived cfg. Keeps every module working
#' during migration: `redcap_fields`/`redcap_events` are synthesised from the
#' concept mapping via each concept's legacy_role, so fld()/evt() call sites
#' resolve identically whichever config format defined the mapping.
apply_trial_json <- function(cfg, tj, registry = concept_registry()) {
  if (is.null(tj)) return(cfg)

  # meta → top-level identity keys
  for (k in c("code", "name", "short_name", "category")) {
    v <- tj$meta[[k]]
    if (!is.null(v) && nzchar(as.character(v))) cfg[[k]] <- v
  }
  if (!is.null(tj$meta$trial_target))
    cfg$trial_target <- as.integer(tj$meta$trial_target)
  if (!is.null(tj$meta$colors)) cfg$colors <- tj$meta$colors

  if (!is.null(tj$source$data_dir) && nzchar(tj$source$data_dir))
    cfg$data_dir <- tj$source$data_dir
  if (!is.null(tj$source$adapter)) cfg$source_adapter <- tj$source$adapter

  # mapping → carried whole (new pipeline reads cfg$mapping directly) and
  # ALSO synthesised into legacy keys for existing fld()/evt() call sites.
  if (!is.null(tj$mapping)) {
    cfg$mapping <- tj$mapping

    rf <- cfg$redcap_fields %||% list()
    for (cid in names(tj$mapping$concepts %||% list())) {
      cc <- registry[[cid]]
      if (is.null(cc)) next
      lr <- cc$legacy_role
      if (is.na(lr %||% NA_character_)) next
      f <- tj$mapping$concepts[[cid]]$field %||% tj$mapping$concepts[[cid]]
      if (is.character(f) && length(f) == 1 && nzchar(f)) rf[[lr]] <- f
    }
    cfg$redcap_fields <- rf

    re <- cfg$redcap_events %||% list()
    for (role in names(tj$mapping$events %||% list())) {
      ev <- tj$mapping$events[[role]]$source_events %||% tj$mapping$events[[role]]
      if (is.character(ev) && length(ev)) re[[role]] <- ev
    }
    cfg$redcap_events <- re
  }

  # modules → merge over feature flags (module block wins where present)
  if (!is.null(tj$modules)) {
    cfg$modules <- tj$modules
    ft <- cfg$features %||% list()
    for (mid in names(tj$modules)) {
      en <- tj$modules[[mid]]$enabled
      if (!is.null(en)) ft[[mid]] <- isTRUE(en)
    }
    cfg$features <- ft
  }

  cfg$config_source <- c(cfg$config_source %||% character(0), "trial.json")
  cfg
}

#' One-off converter: write a trial.json capturing an existing config.R
#' trial's mapping (the migration shim, docs/ARCHITECTURE.md §9).
convert_config_to_trial_json <- function(cfg, registry = concept_registry()) {
  concepts <- list()
  for (cid in names(registry)) {
    lr <- registry[[cid]]$legacy_role
    if (is.na(lr %||% NA_character_)) next
    f <- cfg$redcap_fields[[lr]]
    if (is.character(f) && length(f) == 1 && nzchar(f))
      concepts[[cid]] <- list(field = f)
  }
  events <- list()
  for (role in names(cfg$redcap_events %||% list())) {
    ev <- cfg$redcap_events[[role]]
    if (is.character(ev) && length(ev))
      events[[role]] <- list(source_events = as.list(ev))
  }
  modules <- list()
  for (mid in names(cfg$features %||% list()))
    modules[[mid]] <- list(enabled = isTRUE(cfg$features[[mid]]))

  list(
    config_version = TRIAL_CONFIG_VERSION,
    meta = list(
      code         = cfg$code,
      name         = cfg$name,
      short_name   = cfg$short_name,
      trial_target = cfg$trial_target,
      category     = cfg$category %||% NULL,
      colors       = cfg$colors %||% NULL
    ),
    source = list(
      adapter  = cfg$source_adapter %||% "redcap_csv",
      data_dir = cfg$data_dir %||% NULL
    ),
    mapping = list(
      source_fingerprint = cfg$mapping$source_fingerprint %||% NULL,
      concepts = concepts,
      events   = events,
      decodes  = cfg$mapping$decodes %||% NULL
    ),
    modules = modules
  )
}
