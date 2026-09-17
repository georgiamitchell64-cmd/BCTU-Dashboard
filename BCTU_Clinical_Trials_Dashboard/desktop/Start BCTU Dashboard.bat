@echo off
rem =========================================================================
rem  BCTU Clinical Trials Dashboard: double-click to start (Windows)
rem  Opens a "Starting" page in the browser, which switches to the dashboard
rem  when it's ready. The dashboard runs in the minimised "BCTU Dashboard"
rem  window in the taskbar: close it to stop, or just close the browser tab
rem  and it stops by itself ten minutes later.
rem  "Create desktop shortcut.bat" puts a shortcut to this on the desktop.
rem =========================================================================
setlocal EnableDelayedExpansion

set "RSCRIPT="
set "RHOME="
for /f "tokens=2,*" %%A in ('reg query "HKLM\SOFTWARE\R-core\R" /v InstallPath 2^>nul ^| find "InstallPath"') do set "RHOME=%%B"
if not defined RHOME for /f "tokens=2,*" %%A in ('reg query "HKCU\SOFTWARE\R-core\R" /v InstallPath 2^>nul ^| find "InstallPath"') do set "RHOME=%%B"
if defined RHOME if exist "!RHOME!\bin\Rscript.exe" set "RSCRIPT=!RHOME!\bin\Rscript.exe"
if not defined RSCRIPT for /d %%D in ("%ProgramFiles%\R\R-*") do if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"
if not defined RSCRIPT for /d %%D in ("%LOCALAPPDATA%\Programs\R\R-*") do if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"

if not defined RSCRIPT (
  echo.
  echo  R isn't installed on this computer, or it couldn't be found.
  echo  Install R from https://cran.r-project.org and then try again.
  echo.
  pause
  exit /b 1
)

start "" "%~dp0starting.html"
set "BCTU_DESKTOP=1"
start "BCTU Dashboard" /min "%RSCRIPT%" "%~dp0run_dashboard.R"
