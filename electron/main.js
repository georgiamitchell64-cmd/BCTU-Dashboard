const { app, BrowserWindow } = require("electron");
const path = require("path");

let win = null;

const ICON = path.join(__dirname, "..", "build", "icon.ico");

app.on("ready", () => {
  win = new BrowserWindow({
    width: 1280,
    height: 860,
    minWidth: 900,
    minHeight: 600,
    icon: ICON,
    title: "TONIC Dashboard",
    backgroundColor: "#EEF3F8",
    webPreferences: {
      preload: path.join(__dirname, "preload.js"),
      contextIsolation: true,
      nodeIntegration: false,
    },
  });

  win.loadFile(path.join(__dirname, "..", "app", "index.html"));
  win.on("closed", () => { win = null; });
});

app.on("window-all-closed", () => app.quit());
