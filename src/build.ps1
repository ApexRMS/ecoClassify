<#
.SYNOPSIS
  Builds the ecoClassify SyncroSim package: compiles the Studio UI DLL, then creates
  (and optionally installs) ecoClassify.ssimpkg.

.DESCRIPTION
  The package folder is src/, which also holds the UI source (src/ecoclassifyUI/). The package
  is created from a staged copy of src/ without the UI source, build output or debug symbols,
  so only ecoclassifyUI.dll ships in the .ssimpkg.

.EXAMPLE
  .\src\build.ps1                 # build UI + create package
  .\src\build.ps1 -Install        # ...and install it into SyncroSim
  .\src\build.ps1 -SkipUI         # package only, using the committed src\ecoclassifyUI.dll
#>
param(
    [string]$SyncroSimDir = "C:\Program Files\SyncroSim",
    [string]$OutFile = (Join-Path $PSScriptRoot "..\ecoClassify.ssimpkg"),
    [switch]$SkipUI,
    [switch]$Install
)

$ErrorActionPreference = "Stop"
$pkgDir = $PSScriptRoot
$uiProj = Join-Path $PSScriptRoot "ecoclassifyUI\ecoclassifyUI.csproj"
$pm = Join-Path $SyncroSimDir "SyncroSim.PackageManager.exe"

if (-not $SkipUI) {
    Write-Host "Building Studio UI..."
    # Pass the SyncroSim folder as an environment variable (MSBuild reads these as properties):
    # Windows PowerShell mangles "-p:Name=C:\Program Files\..." arguments with spaces.
    $env:SyncroSimDir = $SyncroSimDir
    try {
        dotnet build $uiProj -c Release
    }
    finally {
        Remove-Item Env:\SyncroSimDir -ErrorAction SilentlyContinue
    }
    if ($LASTEXITCODE -ne 0) { throw "UI build failed" }
}

if (-not (Test-Path (Join-Path $pkgDir "ecoclassifyUI.dll"))) {
    throw "ecoclassifyUI.dll is missing from $pkgDir; run without -SkipUI."
}

# Stage the package without UI source, build output, debug symbols or caches.
$stage = Join-Path ([System.IO.Path]::GetTempPath()) ("ecoClassify-pkg-" + [guid]::NewGuid().ToString("N"))
New-Item -ItemType Directory -Path $stage | Out-Null

try {
    Get-ChildItem $pkgDir -Force | Where-Object {
        $_.Name -notin @("ecoclassifyUI", "build.ps1") -and $_.Extension -ne ".pdb"
    } | Copy-Item -Destination $stage -Recurse -Force

    Get-ChildItem $stage -Recurse -Directory -Filter "__pycache__" | Remove-Item -Recurse -Force

    $OutFile = [System.IO.Path]::GetFullPath($OutFile)
    Write-Host "Creating $OutFile..."
    & $pm "--create=$stage" "--file=$OutFile" --force
    if ($LASTEXITCODE -ne 0) { throw "Package creation failed" }
}
finally {
    Remove-Item $stage -Recurse -Force -ErrorAction SilentlyContinue
}

if ($Install) {
    Write-Host "Installing..."
    & $pm "--finstall=$OutFile"
    if ($LASTEXITCODE -ne 0) { throw "Package install failed" }
}

Write-Host "Done."
