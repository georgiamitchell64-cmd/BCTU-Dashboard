# Planner

A desktop planner for day-to-day trial work. Built with Electron and plain
JavaScript — no build step, no framework, no network calls. Everything lives in
one JSON file on your own machine.

It sits alongside the BCTU clinical trials dashboard and borrows its palette, so
the two feel like part of the same kit.

---

## Installing it

Grab the installer from `dist/` (or the zip that was handed to you) and run
**BCTU Planner Setup 1.0.0.exe**. It installs per-user, so it needs no
administrator rights and asks for no elevation — it lands in your own
`%LOCALAPPDATA%\Programs` and adds a Start-menu entry.

There is also **BCTU Planner 1.0.0.exe**, a portable build: no install at all,
just double-click it, including from a USB stick or a network drive. Both
builds share the same data folder, so you can move between them.

The app is not code-signed, so SmartScreen may show *"Windows protected your
PC"* the first time. Click **More info → Run anyway**.

## Running it from source

```bash
cd planner
npm install
npm start
```

`npm run dev` opens the same thing with dev tools attached.

There is also a browser preview for quick tinkering, which stores its data in
the browser rather than on disk:

```bash
npm run preview   # then open http://localhost:4173
```

Tests (dates, repeat rules, the quick-add parser, the data model and the
selectors) run with:

```bash
npm test
```

## Building the installers yourself

```bash
npm run dist:win     # NSIS installer + portable .exe
npm run dist:mac     # dmg
npm run dist:linux   # AppImage
```

Output lands in `dist/`. Build on the platform you are targeting — cross-building
a signed Windows or macOS app from Linux needs extra tooling.

**On a managed Windows machine**, the build config sets
`win.signAndEditExecutable: false`. Without it, electron-builder downloads its
`winCodeSign` toolkit, which contains macOS symlinks, and extracting them fails
with *"Cannot create symbolic link: A required privilege is not held by the
client"* — creating symlinks on Windows needs a privilege a standard account
does not hold. Turning the option off skips that download entirely; the only
cost is that the `.exe` keeps Electron's default file metadata. Delete the line
if you have Developer Mode, administrator rights, or a code-signing certificate.

This is the same fix as the one in `site-contact-mailer`.

## The icon

The icon is drawn as SVG in `tools/make-icon.js` and rendered by:

```bash
npm run icon
```

That writes three files, all of them build output rather than hand-made
binaries, so changing the icon means editing the shape in that script and
re-running it:

| File | Used for |
| --- | --- |
| `build/icon.ico` | Windows — the `.exe`, its shortcuts, and the installer |
| `build/icon.png` | Linux AppImage, and the source macOS converts to `.icns` |
| `src/assets/icon.png` | Shipped inside the app; `BrowserWindow` uses it for the window and taskbar icon at runtime |

### How the icon reaches the .exe

Stamping an icon into a Windows executable is `rcedit`'s job. electron-builder
normally runs it out of its `winCodeSign` toolkit — the same download we
disabled above, because it cannot be unpacked without the symlink privilege.

So `build/after-pack.js` runs `rcedit` directly instead. The `rcedit` npm
package ships `rcedit.exe` on its own, with no archive and no symlinks, so it
needs no special privilege. Build on Windows and everything gets the icon:

| Where | Icon |
| --- | --- |
| The `.exe` in File Explorer | ✅ |
| Start-menu and desktop shortcuts | ✅ (a shortcut inherits its target's icon) |
| Setup window and Add/Remove Programs | ✅ |
| App window and taskbar, while running | ✅ (set by `BrowserWindow`) |

The hook is best-effort by design: if `rcedit` is missing or fails, it prints a
warning and the build carries on. A wrong icon is a blemish, a failed build is
not. The one case where it does fail is cross-building for Windows from Linux
or macOS, where `rcedit.exe` needs wine — build on Windows and the question
does not arise.

---

## What it does

**Three planning horizons.** Every task belongs to a day, a week or a month.
Daily tasks land on a date; weekly ones sit in a lane above the week board for
"sometime this week"; monthly ones are the objectives panel on the calendar.
Drag a task between them and it changes horizon.

**Views**

| View | What it is for |
| --- | --- |
| Today | The day itself — a timeline for anything with a start time, an "anytime" list for the rest, overdue items, a day note and a three-day look-ahead |
| Week | Seven drag-and-drop columns plus the weekly lane |
| Month | A calendar you can drag tasks around, with monthly objectives and a per-trial breakdown |
| Priority matrix | Urgent/important quadrants; dropping a task into a quadrant really does change its urgency |
| Trials | Add, colour and archive trials, and see the workload split between them |
| Insights | Completion trend, a 13-week consistency map, your most productive weekday, and what has been gathering dust |

**Urgency** is four levels — Critical, High, Medium, Low — shown as a colour
stripe down the left of every task, plus a separate "important" flag. The matrix
treats anything critical, high, or due within a day as urgent.

**Trials.** Every task can be tagged to a trial. TONIC is set up to begin with;
add more as you pick them up, each with its own colour. Filter any view down to
one trial with a single click, or set a default so new tasks are tagged for you.
Deleting a trial never deletes its tasks — they simply become untagged.

**Repeats.** Daily, weekday-only, weekly on chosen days, monthly (including
"last day of the month"), or yearly, every *n* periods, with an optional end
date. Ticking off a repeating task rolls it forward to the next occurrence and
records the completion, which is what feeds the streak and the consistency map.
Future occurrences appear as dashed ghosts in the week and month views.

---

## Quick add

The box at the top of Today, Week and Month reads plain English and shows you
what it understood before you press Enter.

```
Chase site 12 for SAE forms tomorrow at 9:30 !1 @tonic #monitoring ~45m
```

| You type | It means |
| --- | --- |
| `today`, `tomorrow`, `fri`, `next friday`, `in 3 days`, `12/09`, `2026-09-12`, `eow`, `eom` | when it is due |
| `at 9:30`, `3pm` | start time |
| `!1` … `!4`, or `!urgent` / `!high` / `!low` | urgency |
| `@tonic` | which trial (only codes that exist) |
| `#monitoring` | a tag |
| `~45m`, `~2h` | time estimate |
| `*` | mark as important |
| `every monday`, `every 2 weeks`, `every weekday` | repeat |
| `this week`, `this month` | plan it at that horizon instead of a day |

Dates are read UK-first: `09/12` is 9 December. A bare day/month that has
already passed rolls to next year.

---

## Keyboard

| Key | Does |
| --- | --- |
| `N` | New task |
| `Ctrl`/`⌘` `K` | Command palette — runs commands and searches tasks |
| `/` | Jump to the search box |
| `1` – `6` | Switch view |
| `T` | Back to today |
| `←` `→` | Previous / next day, week or month |
| `F` | Start or stop the focus timer |
| `D` | Dark mode |
| `Ctrl`/`⌘` `Z` | Undo (Shift to redo) |
| `?` | The whole list |
| `Esc` | Close whatever is open |

Inside the task editor, `Ctrl`/`⌘` `Enter` saves.

---

## Customising it

Settings covers your name and role (they show in the sidebar and the greeting),
light/dark/system theme, accent colour, density, which day the week starts on,
12- or 24-hour clock, whether weekends show, defaults for new tasks, focus and
break lengths, reminder lead time, and which views appear in the sidebar at all.

---

## Your data

Everything is a single JSON file:

- **Windows** `%APPDATA%\BCTU Planner\planner-data.json`
- **macOS** `~/Library/Application Support/BCTU Planner/planner-data.json`
- **Linux** `~/.config/BCTU Planner/planner-data.json`

A dated copy is written to `backups/` in the same folder each time the app
starts, and the last fourteen are kept. **File → Open Data Folder** takes you
straight there.

Export a JSON backup or a CSV of every task from Settings or the File menu.
Import replaces everything, after backing up what is already there.

Nothing is sent anywhere. There is no account, no sync and no telemetry; the
renderer runs under a content security policy that blocks outbound requests
entirely.

---

## How it is put together

```
main.js              Electron main process — window, menu, the data file
preload.js           The only bridge to Node: load, save, dialogs, menu events
src/
  index.html
  styles/            tokens.css (design tokens) · app.css (shell) · views.css
  js/
    core/            Pure logic, no DOM: dates, repeat rules, the parser, selectors
    state/           model.js (shape + migration) · store.js (state, undo, saving)
    ui/              Shared components: task rows, the editor, palette, timer, drag and drop
    views/           One file per view
    app.js           Bootstrap and routing
test/                Unit tests for everything in core/ and state/
tools/               Browser preview server
```

Two rules keep it honest: anything in `core/` is a pure function and is tested,
and the renderer never touches the filesystem — it asks the preload bridge, and
falls back to browser storage when there isn't one.
