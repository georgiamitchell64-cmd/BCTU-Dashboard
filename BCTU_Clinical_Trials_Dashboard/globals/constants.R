# =============================================================================
# Constants — shared across all trials
# =============================================================================
# TRIAL_TARGET and DATA_DIR are set dynamically when a trial is selected.
# They start with safe defaults and are overwritten by trial_selector_server.
# =============================================================================

TRIAL_TARGET <- 0L          # Overwritten when trial is selected
DATA_DIR     <- app_data_dir()              # Overwritten when trial is selected

# Air-gap toggle: set to TRUE only if you want the dashboard to call out to
# OpenStreetMap Nominatim for geocoding new sites. Default FALSE keeps the
# app fully offline; UK hospitals are still resolved from a local lookup.
ALLOW_REMOTE_GEOCODING <- FALSE

# University of Birmingham palette (see www/tonic_core.css for the full set)
col_navy  <- "#1B1B1B";  col_teal  <- "#00ACA9";  col_teal2 <- "#00788E"
col_amber <- "#F07F3C";  col_red   <- "#E30513";  col_blue  <- "#0057BF"
col_muted <- "#58595B";  col_bg    <- "#F4F4F4";  col_gold  <- "#C59A00"

status_cols <- c(
  "Identified" = "#8A8A8C", "Set-up" = "#F07F3C",
  "Open" = "#0057BF", "Recruiting" = "#3AAA35", "Closed" = "#E30513"
)

# Portfolio categories (Stage 4). Each trial's config carries
# `category = "Surgery"`. Trials with no value fall under "Uncategorised".
TRIAL_CATEGORIES <- c(
  "Surgery", "Women's Health", "Oncology", "Cardiovascular",
  "Mental Health", "Respiratory", "Other"
)
TRIAL_CATEGORY_FALLBACK <- "Uncategorised"
TRIAL_CATEGORY_ICONS <- c(
  "Surgery"        = "&#x1FA7A;",
  "Women's Health" = "&#x2640;",
  "Oncology"       = "&#x1F388;",
  "Cardiovascular" = "&#x2764;",
  "Mental Health"  = "&#x1F9E0;",
  "Respiratory"    = "&#x1FAC1;",
  "Other"          = "&#x1F52C;",
  "Uncategorised"  = "&#x1F4CB;"
)
trial_category <- function(cfg) {
  cat <- cfg$category %||% TRIAL_CATEGORY_FALLBACK
  if (!nzchar(cat)) TRIAL_CATEGORY_FALLBACK else cat
}

# ── Themes (Stage 6) ────────────────────────────────────────────────────────
# Each theme is a named palette + sidebar variant ('dark' or 'light').
# Cards on the trial settings tab let TMs pick one; values persist as
# `theme = "indigo"` in overrides.json. "Custom" disables the picker and
# lets the user set primary/secondary/accent freely.
TRIAL_THEMES <- list(
  uob = list(
    label   = "University of Birmingham",
    sublabel = "Black, white & gold",
    primary = "#1B1B1B", secondary = "#00788E", accent = "#C59A00",
    sidebar = "light"
  ),
  navy_teal = list(
    label   = "Navy & Teal",
    sublabel = "Classic BCTU",
    primary = "#1B4F6B", secondary = "#2EC4A5", accent = "#F59E0B",
    sidebar = "dark"
  ),
  indigo = list(
    label   = "Indigo",
    sublabel = "Soft & contemporary",
    primary = "#312E81", secondary = "#8B5CF6", accent = "#F59E0B",
    sidebar = "dark"
  ),
  emerald = list(
    label   = "Emerald",
    sublabel = "Calm & clinical",
    primary = "#064E3B", secondary = "#10B981", accent = "#FBBF24",
    sidebar = "dark"
  ),
  ocean = list(
    label   = "Ocean",
    sublabel = "Cool & focused",
    primary = "#0C4A6E", secondary = "#06B6D4", accent = "#F97316",
    sidebar = "dark"
  ),
  light = list(
    label   = "Light",
    sublabel = "Minimal & airy",
    primary = "#6366F1", secondary = "#8B5CF6", accent = "#10B981",
    sidebar = "light"
  )
)

trial_theme_name <- function(cfg) {
  cfg$theme %||% "custom"
}

# COS labels are trial-specific and loaded from config, but we keep a
# default set here so modules work before a trial is selected.
cos_type_labels <- c(
  "1" = "Death", "2" = "No Operation", "3" = "Part withdrawal",
  "4" = "Complete withdrawal", "5" = "Lost to follow-up"
)
cos_type_descriptions <- cos_type_labels  # Overwritten per trial

city_coords <- tribble(
  ~city,           ~lat,    ~lon,
  "London",        51.507, -0.128,  "Manchester",   53.483, -2.244,
  "Birmingham",    52.486, -1.890,  "Leeds",        53.801, -1.549,
  "Glasgow",       55.864, -4.252,  "Edinburgh",    55.953, -3.189,
  "Bristol",       51.455, -2.595,  "Liverpool",    53.408, -2.991,
  "Sheffield",     53.381, -1.470,  "Newcastle",    54.978, -1.618,
  "Nottingham",    52.954, -1.150,  "Cardiff",      51.481, -3.180,
  "Leicester",     52.636, -1.133,  "Southampton",  50.909, -1.404,
  "Oxford",        51.752, -1.258,  "Cambridge",    52.205,  0.120,
  "Exeter",        50.718, -3.534,  "Norwich",      52.628,  1.293,
  "Brighton",      50.827, -0.137,  "Coventry",     52.408, -1.510,
  "Aberdeen",      57.150, -2.094,  "Dundee",       56.462, -2.971
)
