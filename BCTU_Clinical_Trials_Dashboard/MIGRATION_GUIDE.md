# Multi-Trial Dashboard — Migration Guide

## What changed

This version adds a **trial selector screen** after login. Users authenticate
once, then pick which trial to work in. Each trial has its own configuration,
data folder, database, and branding.

### New files

| File | Purpose |
|------|---------|
| `globals/trial_config.R` | Config system: `discover_trials()`, `validate_trial_config()`, `create_trial_template()` |
| `trials/tonic/config.R` | TONIC's full config (target, field mappings, events, ethnicity labels, schedule, report defaults, feature flags) |
| `modules/trial_selector.R` | Trial selector UI (card grid) |
| `modules/trial_selector_server.R` | Handles trial selection, loads config, switches view |

### Modified files

| File | What changed |
|------|-------------|
| `app.R` | Sources new files, uses `build_auth_head()`, discovers trials at startup, copies logos |
| `globals/constants.R` | `TRIAL_TARGET` and `DATA_DIR` start at safe defaults, overwritten when trial is selected |
| `functions/layout.R` | Wraps dashboard in `#dashboard_panel` (hidden initially), adds `#trial_selector_panel`, adds "Switch trial" button, sidebar logo is now dynamic via `uiOutput` |
| `functions/theme.R` | Added `.trial-select-card` CSS; login screen text changed from "TONIC" to generic |
| `functions/auth_head.R` | Login screen text changed from "Welcome to the TONIC dashboard" to "Clinical Trials Dashboard" |
| `modules/core.R` | `rv` gains `trial_config`, `trial_code`, `available_trials`, `trigger_data_load`; adds back-to-selector logic; auto-selects trial if only one configured; sidebar logo is now dynamic |

### Unchanged files (copied as-is)

All other files in `globals/`, `functions/`, and `modules/` are unchanged. They
continue to reference `TRIAL_TARGET`, `DATA_DIR`, and `DB_PATH` as globals,
which are now set dynamically when a trial is selected.


## Folder structure

```
TONIC_app/                     (or whatever you rename it)
├── app.R                      ← updated entry point
├── globals/
│   ├── packages.R
│   ├── credentials.R
│   ├── constants.R            ← updated
│   ├── datasets.R
│   └── trial_config.R         ← NEW
├── functions/
│   ├── auth_head.R            ← updated (generic branding)
│   ├── layout.R               ← updated (trial selector + dashboard panels)
│   ├── theme.R                ← updated (trial card CSS + generic login text)
│   ├── helpers.R
│   ├── database.R
│   ├── ... (all other function files unchanged)
├── modules/
│   ├── core.R                 ← updated
│   ├── trial_selector.R       ← NEW
│   ├── trial_selector_server.R ← NEW
│   ├── overview.R
│   ├── overview_server.R
│   ├── ... (all other module files unchanged)
├── trials/
│   └── tonic/
│       ├── config.R           ← TONIC's configuration
│       ├── www/               ← put TONIC_Logo.jpg here
│       ├── data/              ← REDCap CSV exports go here
│       └── reports/           ← Rmd templates
│           ├── tonic_report.Rmd
│           └── tsc_report.Rmd
├── www/
│   └── trial_logos/           ← auto-generated on startup
└── data/
    └── tonic.sqlite           ← global accounts DB (or per-trial DBs)
```


## How to set up

### 1. Replace your existing app folder

Back up your current `TONIC_app/` folder, then replace its contents with the
files from this package.

### 2. Move your data

Your REDCap CSV exports currently go in `TONIC_app/data/`. You have two options:

**Option A — Use the network K: drive path** (recommended for BCTU):
Edit `trials/tonic/config.R` and uncomment the `data_dir` line:
```r
data_dir = "K:/BCTU/BCTU/Teams/Coloproctology/CURRENT TRIALS/TONIC/Data",
```
Your existing `data/tonic.sqlite` (sites, accounts, log) will continue to work.

**Option B — Use the local trials folder**:
Copy your CSV exports to `trials/tonic/data/`. The SQLite database will be
created automatically in the same folder.

### 3. Copy your logo

Put `TONIC_Logo.jpg` into `trials/tonic/www/`. On startup the app copies it to
`www/trial_logos/tonic.jpg` so Shiny can serve it.

### 4. Migrate your existing SQLite database

If you have an existing `data/tonic.sqlite` with sites, accounts, and logs,
copy it to `trials/tonic/data/tonic.sqlite` (if using local data) or update
`db_path` in `trials/tonic/config.R` to point to its current location.

### 5. Test

Run the app. You should see:
1. The login screen (now says "Clinical Trials Dashboard" instead of TONIC)
2. After login, a trial selector with a TONIC card
3. Click TONIC → the familiar dashboard loads


## How to add a new trial

### Quick start

```r
# From the R console, with your working directory set to the app folder:
source("globals/trial_config.R")
create_trial_template("mytrial")
```

This creates `trials/mytrial/config.R` with a blank template. Edit it with your
trial's details: name, target, REDCap field names, events, etc.

### Step by step

1. **Copy `trials/tonic/` to `trials/newtrial/`** (or use `create_trial_template()`)

2. **Edit `trials/newtrial/config.R`**:
   - Set `code`, `name`, `short_name`, `trial_target`
   - Map your REDCap event names in `redcap_events`
   - Map your REDCap variable names in `redcap_fields`
   - Set `data_dir` to wherever your REDCap exports live
   - Adjust `features` flags for which tabs to show

3. **Place your logo** in `trials/newtrial/www/logo.jpg`

4. **Restart the app** — the new trial appears on the selector screen

### What the config controls

The config drives:
- **TRIAL_TARGET** — used in value boxes, percentage calculations, projection charts
- **DATA_DIR** — where the app looks for REDCap CSV exports
- **DB_PATH** — where the SQLite database lives (sites, log, accounts)
- **Feature flags** — which tabs are visible (postal tracking, return rates, etc.)
- **Report defaults** — pre-fills the report generator with protocol details

### What you may need to customise further

For trials with significantly different data structures (e.g. no follow-up
questionnaires, different timepoints, different demographics), you may need to:

1. **Update `prepare_report_data.R`** to read field names from
   `rv$trial_config$redcap_fields` instead of hardcoded strings. Currently this
   function has TONIC field names baked in — for a second trial with different
   REDCap fields, you'd wrap the field lookups in a helper like:
   ```r
   fld <- function(name) rv$trial_config$redcap_fields[[name]]
   # Then use: fld("operation_date") instead of "iop_op_end_dt"
   ```

2. **Update `helpers.R`** — the `process_redcap()` and `build_participant_table()`
   functions reference TONIC-specific column names. Same approach: read from config.

3. **Update module servers** — any server that references TONIC-specific REDCap
   fields (e.g. `participants_server.R`, `reports_server.R`) should read field
   names from `state$rv$trial_config$redcap_fields`.

These are targeted find-and-replace changes rather than a full rewrite. The
architecture now supports them — it's just a matter of gradually replacing
hardcoded field names with config lookups as you onboard each new trial.


## How the trial selector works

1. **On startup**, `discover_trials()` scans `trials/*/config.R` and loads each config
2. **After login**, if there's only one trial, it's auto-selected (no selector shown)
3. **If multiple trials**, the selector shows cards — click one to enter
4. **On selection**, `trial_selector_server` sets `TRIAL_TARGET`, `DATA_DIR`, `DB_PATH` globally, initialises the trial's database, and triggers data loading
5. **"Switch trial"** button in the sidebar saves current state and returns to the selector


## Notes

- **Accounts are shared** across all trials (single login)
- **Sites, logs, and randomisation data** are per-trial (separate SQLite databases)
- **The sidebar, theme, and all UI modules** are shared — only data and config change
- If you only ever have one trial configured, the selector auto-skips and the experience is identical to before
