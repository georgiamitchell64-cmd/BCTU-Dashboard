# Starting the dashboard from the desktop

Double-click to start the dashboard; you don't need RStudio. The computer needs R installed (from cran.r-project.org). The first start installs the R packages the dashboard uses, which takes a few minutes; after that it's seconds.

The dashboard itself runs in your browser, the same as it does through RStudio — the shortcut just starts it and opens the tab for you.

**Report export (PDF / Word) needs Pandoc.** RStudio bundles its own copy, but a plain double-click launch does not, so install it once from [github.com/jgm/pandoc/releases/latest](https://github.com/jgm/pandoc/releases/latest) (the `.msi` on Windows, the `.pkg` on Mac) if report export says "Pandoc not found".

## Windows

1. Get the dashboard onto the PC: in GitHub Desktop, clone `georgiamitchell64-cmd/BCTU-Dashboard` (or pull, if it's already there).
2. Open `BCTU_Clinical_Trials_Dashboard\desktop` and double-click **Create desktop shortcut**. Once per computer.
3. From then on, double-click **BCTU Dashboard** on your desktop.

Pull in GitHub Desktop whenever you want the latest version — the shortcut always runs whatever is in that folder.

If Windows shows *"The publisher could not be verified. Are you sure you want to run this software?"*, that is Windows' mark-of-the-web on files that came out of a downloaded zip, not a problem with the dashboard. Click **Run**. **Create desktop shortcut** clears the mark for this folder, so it should only appear once. To clear it for the whole folder yourself, right-click the downloaded `.zip` before extracting it, choose **Properties**, tick **Unblock**, then extract again.

A *Starting the dashboard…* page opens in your browser. After 10 to 20 seconds it switches to the dashboard.

The dashboard runs in a minimised **BCTU Dashboard** window in the taskbar. You can stop it in two ways:

- Close that window to stop it straight away.
- Close the browser tab. The dashboard stops by itself ten minutes later.

If you double-click the shortcut while the dashboard is already running, it just opens the dashboard again.

## Mac

1. In Terminal, run `bash desktop/build_mac_app.sh` from the app folder. Do this once per Mac, and again if the folder moves.
   It builds **BCTU Dashboard.app** and puts it on the Desktop.
2. Double-click **BCTU Dashboard** to start the dashboard.

The dashboard stops ten minutes after the last browser tab closes. Details are logged in `~/Library/Logs/BCTU Dashboard.log`.

## Settings

Set these as environment variables before starting:

- `BCTU_PORT`: the port to use (default 3838).
- `BCTU_AUTO_STOP_MINUTES`: how long the dashboard waits after the last tab closes before stopping (default 10).
- `BCTU_DATA_DIR`: where the dashboard keeps what it writes — the databases, each trial's settings and report templates, and the logo cache. Left unset it writes inside the dashboard folder, which is what every current install does and needs no change. Set it to put those files somewhere else: a shared folder on K: so a team works off one copy, or a per-user folder when the dashboard folder itself is read-only. The trials that shipped with the dashboard are copied across the first time, and left alone after that.

The icon is drawn by `make_icons.R`.
