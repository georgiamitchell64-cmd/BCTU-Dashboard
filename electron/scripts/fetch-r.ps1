# =============================================================================
# fetch-r.ps1 — download a portable R runtime and install the dashboard's
#               package dependencies into it.
# =============================================================================
# Run this ONCE on a Windows machine before `npm run dist`. It produces
# electron/runtime/R, which electron-builder copies into the installer as the
# app's private R. The user's own R installation (if any) is never touched and
# is not required.
#
#   powershell -ExecutionPolicy Bypass -File scripts/fetch-r.ps1
#
# Re-run it to upgrade R or refresh packages. Safe to re-run: it skips the
# download if the runtime already exists (use -Force to rebuild).
# =============================================================================

param(
  [string]$RVersion = "4.4.2",
  [switch]$Force
)

$ErrorActionPreference = "Stop"

$scriptDir  = Split-Path -Parent $MyInvocation.MyCommand.Path
$electronDir = Split-Path -Parent $scriptDir
$runtimeDir = Join-Path $electronDir "runtime"
$rDir       = Join-Path $runtimeDir "R"
$libDir     = Join-Path $rDir "library"

if ((Test-Path $rDir) -and -not $Force) {
  Write-Host "R runtime already present at $rDir (use -Force to rebuild)." -ForegroundColor Yellow
} else {
  if (Test-Path $rDir) { Remove-Item $rDir -Recurse -Force }
  New-Item -ItemType Directory -Force -Path $runtimeDir | Out-Null

  $url       = "https://cran.r-project.org/bin/windows/base/R-$RVersion-win.exe"
  $installer = Join-Path $env:TEMP "R-$RVersion-win.exe"

  Write-Host "Downloading R $RVersion ..." -ForegroundColor Cyan
  Invoke-WebRequest -Uri $url -OutFile $installer

  # The R Windows installer is an Inno Setup package; /DIR extracts a
  # self-contained copy without registering anything system-wide.
  Write-Host "Extracting to $rDir ..." -ForegroundColor Cyan
  Start-Process -FilePath $installer `
    -ArgumentList "/VERYSILENT", "/SUPPRESSMSGBOXES", "/NORESTART", "/DIR=$rDir" `
    -Wait -NoNewWindow

  Remove-Item $installer -Force
}

$rscript = Join-Path $rDir "bin\x64\Rscript.exe"
if (-not (Test-Path $rscript)) { $rscript = Join-Path $rDir "bin\Rscript.exe" }
if (-not (Test-Path $rscript)) { throw "Rscript.exe not found under $rDir" }

New-Item -ItemType Directory -Force -Path $libDir | Out-Null

# Packages the dashboard loads via library() plus those referenced as pkg::fn.
$packages = @(
  "shiny", "bslib", "shinyWidgets", "shinyjs", "reactable", "echarts4r",
  "shinycssloaders", "dplyr", "tidyr", "stringr", "lubridate", "writexl",
  "readxl", "leaflet", "tibble", "rlang", "DBI", "RSQLite", "jsonlite",
  "digest", "rmarkdown", "knitr", "purrr", "htmltools", "base64enc",
  "officer", "flextable", "scales"
) -join '","'

Write-Host "Installing R packages (this takes a while) ..." -ForegroundColor Cyan
$rCode = @"
.libPaths("$($libDir -replace '\\','/')")
pkgs <- c("$packages")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing,
                   lib   = "$($libDir -replace '\\','/')",
                   repos = "https://cloud.r-project.org",
                   type  = "binary")
}
still <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(still)) {
  stop("Failed to install: ", paste(still, collapse = ", "))
}
cat("All packages present.\n")
"@

& $rscript -e $rCode
if ($LASTEXITCODE -ne 0) { throw "R package installation failed." }

# ── pandoc ───────────────────────────────────────────────────────────────────
# rmarkdown shells out to pandoc to render the TMG/TSC reports. It normally
# ships with RStudio, which a packaged app does not have, so bundle it or
# report export fails on a clean machine.
$pandocDir = Join-Path $runtimeDir "pandoc"
$pandocExe = Join-Path $pandocDir "pandoc.exe"

if ((Test-Path $pandocExe) -and -not $Force) {
  Write-Host "pandoc already present at $pandocDir" -ForegroundColor Yellow
} else {
  New-Item -ItemType Directory -Force -Path $pandocDir | Out-Null
  $pandocVer = "3.1.11"
  $zipUrl = "https://github.com/jgm/pandoc/releases/download/$pandocVer/pandoc-$pandocVer-windows-x86_64.zip"
  $zip    = Join-Path $env:TEMP "pandoc-$pandocVer.zip"
  $tmp    = Join-Path $env:TEMP "pandoc-extract"

  Write-Host "Downloading pandoc $pandocVer ..." -ForegroundColor Cyan
  Invoke-WebRequest -Uri $zipUrl -OutFile $zip
  if (Test-Path $tmp) { Remove-Item $tmp -Recurse -Force }
  Expand-Archive -Path $zip -DestinationPath $tmp -Force

  $found = Get-ChildItem $tmp -Recurse -Filter "pandoc.exe" | Select-Object -First 1
  if (-not $found) { throw "pandoc.exe not found in the downloaded archive" }
  Copy-Item $found.FullName -Destination $pandocExe -Force

  Remove-Item $zip -Force
  Remove-Item $tmp -Recurse -Force
  Write-Host "pandoc ready at $pandocDir" -ForegroundColor Green
}

Write-Host ""
Write-Host "R runtime ready at $rDir" -ForegroundColor Green
Write-Host "Next: npm install; npm run dist" -ForegroundColor Green
