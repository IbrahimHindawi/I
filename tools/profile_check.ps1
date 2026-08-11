# Runs `I.exe check` in a loop so a sampling profiler has a long enough workload.
#
# A single check of a large project takes well under a second, which gives a
# sampler only a few hundred samples. Looping turns that into tens of seconds of
# continuous, identical work.
#
#   .\scripts\profile_check.ps1                       # default target and count
#   .\scripts\profile_check.ps1 -Entry path\to.i -Count 50
#   .\scripts\profile_check.ps1 -Wpr                  # capture an ETL trace too
#
# Use build-profile\I.exe: it is /O2 with /Zi, so the profile reflects optimized
# code and still resolves to function names via build-profile\I.pdb.

param(
    [string]$Exe    = "$PSScriptRoot\..\build-profile\I.exe",
    [string]$Entry  = "C:\devel\njinn\src\gin_win32.i",
    [int]$Count     = 30,
    [switch]$Wpr
)

$ErrorActionPreference = "Stop"

$Exe = (Resolve-Path $Exe).Path
if (-not (Test-Path $Entry)) { throw "entry not found: $Entry" }

$pdb = [IO.Path]::ChangeExtension($Exe, ".pdb")
if (-not (Test-Path $pdb)) {
    Write-Warning "no PDB beside $Exe - the profile will show addresses, not function names."
    Write-Warning "build with: cmake -S . -B build-profile -G Ninja -DCMAKE_C_COMPILER=clang-cl -DCMAKE_BUILD_TYPE=RelWithDebInfo"
}

Write-Host "exe:   $Exe"
Write-Host "entry: $Entry"
Write-Host "runs:  $Count"

$trace = Join-Path (Split-Path $Exe) "check_profile.etl"
if ($Wpr) {
    Write-Host "starting WPR CPU capture..."
    & wpr -start CPU -filemode
    if ($LASTEXITCODE -ne 0) { throw "wpr -start failed (needs an elevated shell)" }
}

$sw = [Diagnostics.Stopwatch]::StartNew()
for ($i = 0; $i -lt $Count; $i++) {
    & $Exe check $Entry | Out-Null
    if ($LASTEXITCODE -ne 0) { Write-Warning "run $i exited $LASTEXITCODE" }
}
$sw.Stop()

if ($Wpr) {
    Write-Host "stopping capture..."
    & wpr -stop $trace
    Write-Host "trace: $trace"
    Write-Host "open with: wpa `"$trace`""
}

$per = $sw.Elapsed.TotalMilliseconds / $Count
Write-Host ("{0} runs in {1:N2}s, {2:N1} ms per run" -f $Count, $sw.Elapsed.TotalSeconds, $per)
