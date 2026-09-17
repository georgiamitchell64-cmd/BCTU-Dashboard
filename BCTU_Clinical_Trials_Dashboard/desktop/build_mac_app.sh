#!/bin/bash
# Builds desktop/BCTU Dashboard.app (with its icon) and puts it on the Desktop.
# Run once on each Mac, and again if the project folder moves:
#   bash desktop/build_mac_app.sh
set -e
DIR="$(cd "$(dirname "$0")" && pwd)"
APP="$DIR/BCTU Dashboard.app"
SRC="$(mktemp -t bctu_launcher).applescript"

sed "s|__DESKTOP_DIR__|$DIR|" "$DIR/mac_launcher.applescript" > "$SRC"
rm -rf "$APP"
osacompile -o "$APP" "$SRC"
rm -f "$SRC"
# Use our icon: drop the compiler's asset catalogue (which would win over it),
# then re-sign so macOS accepts the changed bundle
cp "$DIR/icon/bctu_dashboard.icns" "$APP/Contents/Resources/applet.icns"
rm -f "$APP/Contents/Resources/Assets.car"
/usr/libexec/PlistBuddy -c "Delete :CFBundleIconName" "$APP/Contents/Info.plist" 2>/dev/null || true
/usr/libexec/PlistBuddy -c "Set :CFBundleIconFile applet" "$APP/Contents/Info.plist" 2>/dev/null ||
  /usr/libexec/PlistBuddy -c "Add :CFBundleIconFile string applet" "$APP/Contents/Info.plist"
codesign --force --deep --sign - "$APP" 2>/dev/null || true
chmod +x "$DIR/start_mac.sh"
touch "$APP"

# A Finder alias follows the app if the folder is renamed; a symbolic link is
# the fallback when Finder can't be asked
rm -rf "$HOME/Desktop/BCTU Dashboard" "$HOME/Desktop/BCTU Dashboard.app"
if osascript -e "tell application \"Finder\"
    set a to make alias file to (POSIX file \"$APP\" as alias) at (path to desktop folder)
    set name of a to \"BCTU Dashboard\"
  end tell" > /dev/null 2>&1; then
  echo "Built $APP and put a BCTU Dashboard alias on the Desktop."
else
  ln -s "$APP" "$HOME/Desktop/BCTU Dashboard.app"
  echo "Built $APP and linked it on the Desktop (rerun this if the project folder moves)."
fi
