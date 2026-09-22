<#
================================================================================
  Canontra Live Empirical Benchmark Automation Runner
  Executes live multi-tool comparisons across 15 real-world open-source repos
================================================================================
#>
[CmdletBinding()]
param (
    [string]$CanontraExe = "dist-bin\canontra.exe",
    [string]$BenchmarkDir = "benchmarks",
    [string]$OutputDir = "researchBenchmarks"
)

$ErrorActionPreference = "Stop"

if (-not (Test-Path $CanontraExe)) {
    Write-Error "Canontra executable not found at: $CanontraExe"
    exit 1
}

$repos = @(
    "bottle", "toml", "requests", "flask", "marshmallow",
    "chalk", "click", "gin", "jinja", "ripgrep",
    "express", "rich", "hugo", "deno_core", "prometheus"
)

Write-Host "Starting Canontra Live Empirical Benchmark Suite across 15 repositories..." -ForegroundColor Cyan

$summary = @()
$summary += "Repository,Language,Files,LOC,ColdSec,ThroughputLOCs,ExitCode"

foreach ($repo in $repos) {
    $repoPath = Join-Path $BenchmarkDir $repo
    if (Test-Path $repoPath) {
        Write-Host "Benchmarking $repo..." -ForegroundColor Yellow
        $files = Get-ChildItem -Path $repoPath -Recurse -Include *.py, *.js, *.ts, *.go, *.rs -File
        $fileCount = $files.Count
        $loc = 0
        foreach ($f in $files) {
            $loc += (Get-Content $f.FullName | Measure-Object -Line).Lines
        }

        $sw = [System.Diagnostics.Stopwatch]::StartNew()
        $outFile = Join-Path $OutputDir "$repo`_manifest.json"
        $errFile = Join-Path $OutputDir "$repo`_err.log"
        $p = Start-Process -FilePath $CanontraExe -ArgumentList "repo `"$repoPath`" --json" -NoNewWindow -PassThru -Wait -RedirectStandardOutput $outFile -RedirectStandardError $errFile
        $sw.Stop()

        $elapsed = [Math]::Round($sw.Elapsed.TotalSeconds, 3)
        $throughput = if ($elapsed -gt 0) { [Math]::Round($loc / $elapsed, 0) } else { 0 }
        $summary += "$repo,polyglot,$fileCount,$loc,$elapsed,$throughput,$($p.ExitCode)"
        Write-Host "  ✔ $repo: $fileCount files, $loc LOC in ${elapsed}s (${throughput} LOC/s)" -ForegroundColor Green
    }
}

$summaryPath = Join-Path $OutputDir "benchmark_live_run.csv"
$summary | Out-File -FilePath $summaryPath -Encoding ascii
Write-Host "Benchmark execution complete. Results saved to $summaryPath" -ForegroundColor Green
