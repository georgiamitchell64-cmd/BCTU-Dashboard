# BCTU Dashboard — Desktop (Electron) build

Packages the Shiny dashboard as a standalone Windows application with its own
private copy of R. Users install one `.exe`; they do not need R, RStudio, or
any packages installed.

## Getting the installer

### Easiest: let GitHub build it

You do not need R, Node or a build environment.

1. Go to the repository's **Actions** tab
2. Choose **Desktop build (Windows)** -> **Run workflow**
3. Wait ~20-30 minutes
4. Download **BCTU-Dashboard-Windows-Installer** from the run's Artifacts

That artefact contains the `.exe`. Run it to install - no admin rights needed.

### Alternative: build locally on Windows

```powershell
cd electron
npm run fetch-r     # downloads R + 28 packages + pandoc (10-20 min, one-off)
npm install
npm run dist        # -> dist\BCTU-Dashboard-Setup-1.0.0.exe
```

To run without building an installer, use `npm start`.

## Does the user need R installed?

**No.** R is bundled inside the installer, along with pandoc for report export.
The app launches `Rscript.exe` from its own `resources/R` folder and never
consults the system `PATH` or registry, so a user's own R installation (if any)
is neither required nor touched.

The trade is size: expect a 300-500MB installer.

## Where user data lives

This is the part that makes edits stick.

The app writes **nothing** inside its own program folder. On Windows all user
data goes to:

```
C:\Users\<you>\AppData\Roaming\BCTU Clinical Trials Dashboard\
├── data/                     app-level databases
└── trials/
    └── tonic/
        ├── config.R          seeded from the bundle on first launch
        ├── overrides.json    your trial setting overrides
        └── data/tonic.sqlite sites, open dates, targets, activity log
```

Electron passes that folder to R as the `BCTU_DATA_ROOT` environment variable;
`globals/app_paths.R` routes every writable path through it.

This matters because the program folder is **replaced wholesale on every app
update**. Anything written there — which is what the code used to do, via
`file.path(getwd(), "data")` — is destroyed on update. Keeping data in
`AppData` means installing a new version leaves your sites, open dates and
targets untouched.

### Backing up

The `AppData` folder above is the entire application state. Copy it to back up;
copy it back to restore. Closing the app first is recommended so SQLite isn't
mid-write.

### Starting over

Delete the folder. The app rebuilds it, re-seeding trial configs from the
bundle, and you re-import from your REDCap CSV export.

## Development

Running from source outside Electron still works exactly as before:

```r
setwd("BCTU_Clinical_Trials_Dashboard")
shiny::runApp()
```

With `BCTU_DATA_ROOT` unset, `app_data_root()` falls back to the working
directory, so a local checkout behaves the way it always has.

## Differences from the server build

| Feature | Server | Desktop |
|---|---|---|
| Login / self-registration | Yes | **Removed** — single local user, opens straight to the trial selector |
| Accounts tab | Yes | **Removed** |
| Postal tracking tab | Feature-flagged | **Removed** |
| Multi-trial selector | Yes | Yes |
| Data location | App directory | Per-user `AppData` |
| Site edits saved | On exit | **On every edit** |

## Troubleshooting

**"Bundled R not found"** — `npm run fetch-r` hasn't been run, or it failed.
Check that `electron/runtime/R/bin/x64/Rscript.exe` exists.

**"Timed out waiting for the R server to start"** — R started but Shiny didn't
come up within 120s. Run `npm start` from a terminal; R's stdout/stderr is
echoed there prefixed with `[R]` and will show the actual error.

**Edits still not persisting** — confirm the data root. The app logs
`Data root: ...` on startup; visible via `npm start`.
