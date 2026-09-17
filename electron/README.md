# The desktop build

Wraps the dashboard in a window, so it installs and starts like any other
Windows program: no R, no RStudio, no Chrome, no browser tab, nothing to set up.
The dashboard inside it is the same Shiny app — this only starts it and shows it.

Build it from the **Actions** tab on GitHub: *Desktop build (Windows)* → **Run
workflow**, then download the `.exe` from the finished run. It takes 30-40
minutes and needs a Windows machine, which is why it runs there and not here.

## How it starts

`main.js` asks `lib/locate.js` where everything is, `lib/server.js` starts R on
a free port and waits for it to answer, then the window loads it. Until then it
shows `loading.html`. Closing the window stops R.

Two layouts, because it runs both from a checkout and from an installer:

| | app | R, pandoc, Chrome |
|---|---|---|
| developing | `../BCTU_Clinical_Trials_Dashboard` | `electron/runtime` |
| installed | `resources/app` | `resources/runtime` |

R gets two environment variables, which are the whole contract with the R side:

- `BCTU_DATA_DIR` — where the app writes. The install folder is read-only, so
  everything goes to the per-user folder instead (`globals/paths.R`).
- `BCTU_RUNTIME_DIR` — where the bundled pandoc and Chrome are, which is what
  makes report export work on a machine with neither (`globals/runtime.R`).

Deliberately *not* `BCTU_DESKTOP`: that arms the ten-minute idle shutdown meant
for the browser shortcut, and here the window already owns the lifetime.

## What gets packaged

`npm run dist` runs `npm run stage` first, which is
`scripts/prepare_distribution.R` — the installer is built from a stripped copy,
not from the working folder. That matters: the working folder accumulates real
accounts, activity logs and participant records, and none of it should go out
in an installer. The staged copy ships with no accounts and needs none — **the
first person to register on it becomes its admin.**

The build workflow checks the staged copy is clean before uploading, so this
can't quietly stop working.

## Working on it

```
npm test     # the launcher logic, and a real R round trip — no install needed
npm install  # fetches Electron itself (~100MB)
npm start    # runs it against the checkout
```

`npm test` is plain node with no dependencies, so it runs before `npm install`
and in CI. It covers both layouts, finding R on each platform, and — where the
machine has R and shiny — actually starting a Shiny app, waiting for it, and
stopping it again.

To run `npm start` with report export working, fetch the runtime first:

```
cd ../BCTU_Clinical_Trials_Dashboard && Rscript scripts/fetch_runtime.R
mv runtime ../electron/runtime
```
