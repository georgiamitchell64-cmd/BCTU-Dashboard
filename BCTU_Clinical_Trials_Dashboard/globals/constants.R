# =============================================================================
# Constants — shared across all trials
# =============================================================================
# TRIAL_TARGET and DATA_DIR are set dynamically when a trial is selected.
# They start with safe defaults and are overwritten by trial_selector_server.
# =============================================================================

TRIAL_TARGET <- 0L          # Overwritten when trial is selected
DATA_DIR     <- file.path(getwd(), "data")  # Overwritten when trial is selected

col_navy  <- "#1B4F6B";  col_teal  <- "#2EC4A5";  col_teal2 <- "#0FA88E"
col_amber <- "#F59E0B";  col_red   <- "#EF4444";  col_blue  <- "#3B82F6"
col_muted <- "#64748B";  col_bg    <- "#EEF3F8"

status_cols <- c(
  "Identified" = "#94A3B8", "Set-up" = "#F59E0B",
  "Open" = "#3B82F6", "Recruiting" = "#2EC4A5", "Closed" = "#EF4444"
)

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
