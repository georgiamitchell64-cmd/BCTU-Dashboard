@echo off
rem =========================================================================
rem  Puts a "BCTU Dashboard" shortcut, with its icon, on this computer's
rem  desktop. Double-click once; afterwards use the shortcut to start the
rem  dashboard.
rem =========================================================================
set "TARGET=%~dp0Start BCTU Dashboard.bat"
set "ICON=%~dp0icon\bctu_dashboard.ico"

powershell -NoProfile -ExecutionPolicy Bypass -Command ^
  "$d = [Environment]::GetFolderPath('Desktop');" ^
  "$s = (New-Object -ComObject WScript.Shell).CreateShortcut((Join-Path $d 'BCTU Dashboard.lnk'));" ^
  "$s.TargetPath = $env:TARGET; $s.WorkingDirectory = (Split-Path $env:TARGET);" ^
  "$s.IconLocation = $env:ICON; $s.WindowStyle = 7;" ^
  "$s.Description = 'Start the BCTU Clinical Trials Dashboard'; $s.Save()"

echo.
if errorlevel 1 (
  echo  The shortcut couldn't be made automatically. Instead, right-click
  echo  "Start BCTU Dashboard" in this folder and choose Send to, then Desktop.
) else (
  echo  Done: double-click "BCTU Dashboard" on your desktop to start the dashboard.
)
echo.
pause
