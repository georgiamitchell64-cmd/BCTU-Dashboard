#!/bin/bash
# Starts the dashboard for "BCTU Dashboard.app" (built by build_mac_app.sh):
# opens the starting page, then runs desktop/run_dashboard.R in the background.
# Details go to ~/Library/Logs/BCTU Dashboard.log.

DIR="$(cd "$(dirname "$0")" && pwd)"
LOG="$HOME/Library/Logs/BCTU Dashboard.log"

RSCRIPT=""
for R in "$(command -v Rscript 2>/dev/null)" /usr/local/bin/Rscript /opt/homebrew/bin/Rscript \
         /Library/Frameworks/R.framework/Resources/bin/Rscript; do
  if [ -n "$R" ] && [ -x "$R" ]; then RSCRIPT="$R"; break; fi
done
if [ -z "$RSCRIPT" ]; then
  osascript -e 'display alert "R isn’t installed on this Mac" message "Install R from cran.r-project.org, then double-click BCTU Dashboard again."'
  exit 1
fi

open "$DIR/starting.html"
mkdir -p "$(dirname "$LOG")"
BCTU_DESKTOP=1 nohup "$RSCRIPT" "$DIR/run_dashboard.R" > "$LOG" 2>&1 &
