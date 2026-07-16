# =============================================================================
# Trial config overrides
# =============================================================================
# Each trial can have an `overrides.json` next to its `config.R`. Whatever's
# in the JSON wins over the original config when the trial is loaded.
# This keeps user-edits separate from the auto-generated config file —
# so resetting a trial means deleting one JSON, not patching code.
#
# Supported override keys (top-level fields of trial_config):
#   short_name, name, trial_target, category, colors, features, report_defaults
# Other fields can be added without changing this file.
# =============================================================================

overrides_path <- function(cfg) {
  trial_dir <- cfg$trial_dir %||% file.path(getwd(), "trials", cfg$code)
  file.path(trial_dir, "overrides.json")
}

load_overrides <- function(cfg) {
  path <- overrides_path(cfg)
  if (!file.exists(path)) return(list())
  tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
           error = function(e) {
             message("Override read error for ", cfg$code, ": ", e$message)
             list()
           })
}

save_overrides <- function(cfg, overrides) {
  path <- overrides_path(cfg)
  if (!dir.exists(dirname(path)))
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(overrides, path, auto_unbox = TRUE, pretty = TRUE)
}

clear_overrides <- function(cfg) {
  path <- overrides_path(cfg)
  if (file.exists(path)) file.remove(path)
}

# Merge a list of overrides over a cfg. Lists merge key-by-key (one level deep),
# scalars replace.
#
# Exception — `.replace_keys` are written as a complete picture the user
# controls (e.g. the follow-up schedule), so they fully replace rather than
# deep-merge. Otherwise a timepoint removed in the editor would survive because
# the original config.R key would merge back in.
.override_replace_keys <- c("redcap_events")

apply_overrides <- function(cfg, overrides = NULL) {
  if (is.null(overrides)) overrides <- load_overrides(cfg)
  if (length(overrides) == 0) return(cfg)
  for (k in names(overrides)) {
    v <- overrides[[k]]
    if (!(k %in% .override_replace_keys) &&
        is.list(v) && !is.null(cfg[[k]]) && is.list(cfg[[k]])) {
      for (kk in names(v)) cfg[[k]][[kk]] <- v[[kk]]
    } else {
      cfg[[k]] <- v
    }
  }
  cfg
}

# Update one or more override fields without rewriting the whole file.
update_overrides <- function(cfg, ...) {
  current <- load_overrides(cfg)
  patch   <- list(...)
  for (k in names(patch)) current[[k]] <- patch[[k]]
  save_overrides(cfg, current)
  current
}
