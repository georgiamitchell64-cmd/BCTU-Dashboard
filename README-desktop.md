# TONIC Dashboard — desktop app

A standalone Electron app for at-a-glance TONIC trial reporting: recruitment
by site and month, accrual against the protocol schedule, and follow-up
questionnaire return. It runs on its own — no R, no Shiny server, no network.

## Running it

```bash
npm install     # first time only
npm start
```

**Node.js 20 or newer — the current LTS (24.x) is the safe choice.** Check with
`node -v`; get the Windows `.msi` from <https://nodejs.org>.

`.npmrc` sets `engine-strict=true`, so an unsupported Node now fails
immediately with a message naming the version, rather than crashing later with
something misleading. For reference, `Cannot find module 'readline/promises'`
means Node is below 17, and `ERR_REQUIRE_ESM` means below 22.12.

After upgrading Node, reinstall from scratch — the existing tree was built
against the old version, and `node -v` reporting the new version is not enough
on its own:

```bash
rm -rf node_modules package-lock.json   # PowerShell: Remove-Item -Recurse -Force
npm install
```

If `node -v` still reports the old version after installing, close **every**
terminal window and open a new one; PATH does not refresh in shells that were
already open.

## Building a Windows executable

Two targets, and **try the second if the installer build fails**:

```bash
npm run build            # NSIS installer  -> dist\TONIC Dashboard Setup 1.0.0.exe
npm run build:portable   # unpacked folder -> dist\win-unpacked\TONIC Dashboard.exe
```

Must be run **on Windows**. Running the installer `.exe` installs per-user under
`%LOCALAPPDATA%`, with no administrator rights. `build:portable` produces a
folder you can run or copy to a share directly — no installer and no Start-menu
entry, but no code-signing step either, which is where the installer build tends
to fall over.

### If the build fails with "Cannot create symbolic link"

```
ERROR: Cannot create symbolic link : A required privilege is not held by the
client. : ...winCodeSign\...\darwin\10.12\lib\libcrypto.dylib
```

electron-builder downloads a code-signing toolkit whose archive contains **macOS**
symlinks. Creating a symlink on Windows needs a privilege ordinary accounts do
not hold, so extraction fails — on files a Windows-only build never uses.

In order of preference:

1. **Enable Developer Mode.** Settings → Privacy & security → For developers →
   Developer Mode. That grants your account the symlink privilege, which is what
   the error is asking for. Then delete
   `%LOCALAPPDATA%\electron-builder\Cache\winCodeSign` and build again.
2. **Use `npm run build:portable`**, which skips signing entirely so the toolkit
   is never fetched.
3. **Run PowerShell as Administrator**, if you have local admin — administrators
   hold the privilege by default.

If Developer Mode is blocked by group policy, option 2 needs nothing from IT.

## What's in here

| Path | Purpose |
|---|---|
| `electron/main.js` | Window creation and lifecycle |
| `electron/preload.js` | Context-isolated preload bridge |
| `app/index.html` | Markup for all five sections |
| `app/styles.css` | Design tokens and components |
| `app/data.js` | **Every figure the app renders** |
| `app/dashboard.js` | Derivation, charts, tooltips, sorting, navigation |
| `app/chart.umd.js` | Chart.js, vendored |
| `app/fonts/` | IBM Plex Sans/Mono and Newsreader, as woff2 |
| `build/icon.ico` | Application and installer icon |

Fully offline: fonts are bundled locally rather than fetched from Google Fonts,
and Chart.js is vendored, so nothing is requested over the network at runtime.
The renderer runs with `contextIsolation` on and `nodeIntegration` off under a
restrictive Content-Security-Policy.

---

# Security

## What the password does

On first run the app asks you to set a password. It is not a user account — it
is an encryption key. Any trial data you import is encrypted with a key derived
from it (PBKDF2-SHA256, 250,000 rounds → AES-GCM-256) before being written to
local storage, using the browser's own Web Crypto implementation.

Without the password the stored data is unreadable. The key exists only in
memory for the session, and nothing renders until the password is accepted.

**There is no password recovery.** That is a consequence of encrypting properly
rather than an oversight — if the password could be recovered from the machine,
so could the data. If it is lost, use *Forgotten the password* on the lock
screen to clear the stored data and import again.

## What it does not do

Be clear about the boundary, because a login box invites more confidence than
it deserves:

- It does **not** protect a machine that is already unlocked with the app open.
  Anyone sitting at it can read what is on screen.
- It is **not** a multi-user system. One password, shared by whoever uses the
  machine, with no audit trail of who looked at what.
- It does **not** protect the source CSVs sitting in a folder, or anything you
  export out of the app.

It protects data at rest in the app's own storage. It complements BitLocker and
network-share permissions; it does not replace them, and it is not on its own a
defensible answer to an information-governance review.

## Importing an encrypted spreadsheet

**A password-protected `.xlsx` cannot be read by the app.** Excel's "Encrypt
with Password" applies real AES encryption to the whole file, so nothing can
open it without the password — including this app, which has no code to decrypt
that format.

There are two very different Excel features worth separating:

| Excel feature | What it does |
|---|---|
| File → Info → **Encrypt with Password** | Genuine AES encryption. The app cannot read it. |
| Review → **Protect Sheet / Workbook** | Only stops casual editing in Excel. Trivially bypassed, and no protection at all. |

The practical route is to keep the protected workbook as the master, and export
a CSV when you import. The exported CSV is unencrypted while it exists, so put
it somewhere covered by disk encryption and delete it afterwards — the app
encrypts the data once imported, so the CSV only needs to survive the import.

If decrypting a protected workbook directly would genuinely help, it can be
added — it is a bounded piece of work — but it moves the password prompt rather
than removing it.

---

# Putting your own data in

## The import button

**Import data**, bottom-left, takes the two exports the Shiny dashboard already
uses:

| File | Shape | Drives |
|---|---|---|
| **REDCap export** | Long format — one row per record per event, with `record_id` and `redcap_event_name` | Recruitment by month, sites, the randomisation log, age and NELA profiles |
| **Return rates** | `Site, Event, Form, Expected, Due, Entered` | Follow-up return by window and by site |

Either file works on its own; you do not need both. Both are read **in the
window** with the browser's own file API — nothing is uploaded, and no file
leaves the machine. The result is kept in local storage, so it survives closing
the app, and **Revert to demonstration data** puts the sample figures back.

Before applying, the dialog reports what it found and what it could not make
sense of: records with no randomisation date, sites in the export that are not
in the register, missing columns. Read that summary — it is where a mismatched
site name or a wrong file shows up.

Column names are matched leniently (`site_name`, `site`, `centre` all work), so
a slightly different export usually still imports. If a required column really
is absent the dialog says which, and lists the headers it did find.

### What the import does and does not touch

It sets participant counts, dates, recruitment by month, follow-up figures and
the data cut. It **does not** overwrite the manual parts of the site register —
region, status and opening date stay as you set them, and sites still in set-up
keep their place in the denominator rather than vanishing because the export
has never heard of them.

A site in the export that is not in the register is added, with no region, and
flagged in the summary. If that is a spelling difference rather than a genuinely
new site, fix the name in `data.js` — otherwise its participants are counted
separately from the site they belong to.

### After importing

The projection re-derives itself: per-site recruitment rate and site-opening
rate both come from the imported data, so the Recruitment page reflects the new
figures without any further work. The sidebar shows the import date and the
filenames, so it is always clear whether you are looking at real data or the
shipped sample.

## Editing the file directly

The importer writes the same values you can set by hand, so this remains
available for anything the exports do not carry — the site register especially.

**Everything the app displays comes from `app/data.js`.** Nothing is hardcoded
anywhere else — every percentage, rate, projection, ranking and status in the
interface is calculated from that one file when the app starts. Edit it, save,
and either restart the app or press `Ctrl+R`.

Note that an import takes precedence over this file. If your edits appear to do
nothing, an import is in effect — use **Revert to demonstration data** to clear
it first.

Open it in any text editor. It has seven blocks.

### 1. `TRIAL` — identity and targets

```js
target: 898,          // recruitment target
targetSites: 24,      // planned number of sites
dataCut: "31 Jul 2026",   // shown bottom-left; the "as at" date
targetClose: "Jun 2028",  // protocol end of recruitment
```

`dataCut` matters more than it looks: dormant sites are worked out relative to
it, so set it to the date your export was pulled.

### 2. `MONTHS` — recruitment per month

One line per month. `randomised` is what actually happened, `target` is what
the protocol scheduled for that month.

```js
{ label: "Jul 2026", short: "Jul", randomised: 13, target: 12 },
```

Add a new line each month. This drives the recruitment charts, the cumulative
curve, the current rate and the forecast.

### 3. `TARGET_SCHEDULE` — the protocol curve ahead of you

Cumulative targets for months *after* the data cut, used to draw the target
line on the forecast. Already filled in from the trial config through May 2028.

### 3a. `PROJECTION` — how the forecast is modelled

The projection on the Recruitment page works the way the Shiny dashboard does
(`functions/projection_math.R`): each month a few more sites open, up to
`targetSites`, and **every open site recruits at a per-site rate**. This matters
— a straight line through the current monthly rate assumes the network never
grows, which understates a trial that is still opening sites.

The `ratePerSite` and `sitesPerMonth` values here are only fallbacks. Once
there are three months of recruitment the app derives both from what actually
happened:

- **per-site rate** = participants ÷ (months elapsed × open sites)
- **site opening rate** = sites opened ÷ months since the first opened

The two sliders let you test other assumptions without touching the data;
**Reset** returns them to the derived figures. Optimistic and pessimistic are
the central case bent by `rateSpread` (±20% on the rate) and `siteSpread`
(±1 site per month).

### 4. `SITES` — the site register

```js
{ name: "…", region: "…", status: "Recruiting", opened: "Mar 2026", n: 8, lastRand: "28 Jul 2026" },
```

`status` must be one of `Recruiting`, `Open`, `Set-up`, `Identified`.
`n` is participants randomised, `lastRand` the date of the most recent one
(`null` if none). A site with no randomisation within 60 days of `dataCut` is
flagged dormant automatically.

**Keep sites that have not recruited.** A REDCap export only evidences sites
that have randomised someone, so if you rebuild this list from participant
records the ones still in set-up vanish — and "12 of 24 recruiting" quietly
becomes "12 of 12", which reads as a full network when six sites are still
spinning up. Add a site the moment it is identified:

```js
{ name: "…", region: "…", status: "Identified", opened: null, n: 0, lastRand: null },
```

When you wire up a live feed, **only `n` and `lastRand` should come from it**,
matched onto these rows by `name`. Everything else stays hand-maintained.

### 5. `RANDOMISATIONS` — individual participants

Newest first. Feeds the randomisation log and the age / NELA charts.

```js
{ id: "TON-0047", site: "…", date: "30 Jul 2026", age: 71, sex: "F", nela: 8.4 },
```

There is deliberately no allocation field — see *Blinding* below.

### 6. `FOLLOWUP` — questionnaire return by visit window

`complete` is forms received, `expected` is forms due at the data cut — so
`expected` counts only participants who have actually reached that window, not
everyone randomised. Each window lists its instruments.

### 7. `SITE_COMPLIANCE` — follow-up return per site

`complete` and `due` across all windows combined, per site.

## Two things to watch

**Dates** are plain strings in `"DD Mon YYYY"` form (`"08 Jul 2026"`) — keep
that format or the dormant-site calculation will misread them.

**Site names must match** between `SITES`, `RANDOMISATIONS` and
`SITE_COMPLIANCE`. They are matched as exact text, so
`"St James's Hospital Leeds"` and `"St James Hospital Leeds"` are two different
sites as far as the app is concerned.

## If something looks wrong

Press `Ctrl+Shift+I` to open developer tools; a red message in the Console
usually names the line in `data.js` at fault. A trailing comma after the last
item in a list, or a missing `}`, is the usual cause — the app will show
nothing at all rather than a partial page.

## Blinding

The randomisation log carries no allocation column, and the data model has no
allocation field at all — so there is nothing to leak through the interface.
Keep it that way: adding an `allocation` field to `RANDOMISATIONS` would put
treatment assignment one small UI change away from being on screen.
