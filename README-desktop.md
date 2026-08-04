# TONIC Dashboard — desktop app

A standalone Electron app for at-a-glance TONIC trial reporting: recruitment
by site and month, accrual against the protocol schedule, and follow-up
questionnaire return. It runs on its own — no R, no Shiny server, no network.

## Running it

```bash
npm install     # first time only
npm start
```

**Node.js 20 or newer.** Check with `node -v`; get it from <https://nodejs.org>.

## Building the Windows installer

```bash
npm run build
```

Must be run **on Windows** — the NSIS installer cannot be produced from macOS
or Linux without extra setup. Output lands in `dist/`, as
`TONIC Dashboard Setup 1.0.0.exe`.

Note the two steps: `npm run build` *builds* the installer; running the
resulting `.exe` *installs* the app. It installs per-user under
`%LOCALAPPDATA%` and needs no administrator rights.

### If the build fails with `ERR_REQUIRE_ESM`

Fixed in this version, recorded here in case it resurfaces. `electron-builder`
pulls `@noble/hashes`, which went ESM-only at v2. Older Node cannot `require()`
an ESM module, so the build dies before it starts. The `overrides` block in
`package.json` pins that dependency to the last CommonJS release:

```json
"overrides": { "@noble/hashes": "^1.4.0" }
```

If you hit it again after upgrading anything, delete `node_modules` and
`package-lock.json` and reinstall.

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

# Putting your own data in

**Everything the app displays comes from `app/data.js`.** Nothing is hardcoded
anywhere else — every percentage, rate, projection, ranking and status in the
interface is calculated from that one file when the app starts. Edit it, save,
and either restart the app or press `Ctrl+R`.

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
