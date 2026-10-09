# ================================================================================
#   Canontra v0.2.0 Live Empirical Benchmark Automation Runner
#   Measures Cold and Warm (CNTR\x06 Slab Cache) Ingestion across 15 Repos
# ================================================================================
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

$repoLanguages = @{
    "bottle"      = "Python"
    "toml"        = "Rust"
    "requests"    = "Python"
    "flask"       = "Python"
    "marshmallow" = "Python"
    "chalk"       = "JavaScript"
    "click"       = "Python"
    "gin"         = "Go"
    "jinja"       = "Python"
    "ripgrep"     = "Rust"
    "express"     = "JavaScript"
    "rich"        = "Python"
    "hugo"        = "Go"
    "deno_core"   = "TypeScript/Rust"
    "prometheus"  = "Go"
}

Write-Host "Starting Canontra v0.2.0 Empirical Benchmarks across 15 repositories..." -ForegroundColor Cyan

$summary = @()
$summary += '"Repository","Language","TotalSourceFiles","IndexedFiles","TotalLOC","ColdLatencyMs","WarmLatencyMs","CacheSpeedup","ThroughputLOCsec","RepositoryHash","WholeRepoCallGraph","WholeRepoDataFlow","ColdExitCode","WarmExitCode"'

foreach ($repo in $repos) {
    $repoPath = Join-Path $BenchmarkDir $repo
    if (Test-Path $repoPath) {
        $lang = $repoLanguages[$repo]
        Write-Host "Benchmarking ${repo} (${lang})..." -ForegroundColor Yellow

        # Count total source files and LOC
        $files = Get-ChildItem -Path $repoPath -Recurse -Include *.py, *.js, *.ts, *.go, *.rs -File
        $totalFiles = $files.Count
        $totalLoc = 0
        foreach ($f in $files) {
            $totalLoc += (Get-Content $f.FullName | Measure-Object -Line).Lines
        }

        # Clean existing cache for fair cold measurement
        $cacheDir = Join-Path $repoPath ".canontra"
        if (Test-Path $cacheDir) {
            Remove-Item -Path $cacheDir -Recurse -Force
        }

        # Cold Run (writes CNTR\x06 slab cache)
        $coldOut = Join-Path $OutputDir "${repo}_manifest.json"
        $coldErr = Join-Path $OutputDir "${repo}_err.log"
        $swCold = [System.Diagnostics.Stopwatch]::StartNew()
        $pCold = Start-Process -FilePath $CanontraExe -ArgumentList "repo `"$repoPath`" --json --cache" -NoNewWindow -PassThru -Wait -RedirectStandardOutput $coldOut -RedirectStandardError $coldErr
        $swCold.Stop()
        $coldMs = [Math]::Round($swCold.Elapsed.TotalMilliseconds, 2)

        # Warm Run (hits CNTR\x06 slab cache and CSR graphs)
        $warmOut = Join-Path $OutputDir "${repo}_cached_manifest.json"
        $warmErr = Join-Path $OutputDir "${repo}_cached_err.log"
        $swWarm = [System.Diagnostics.Stopwatch]::StartNew()
        $pWarm = Start-Process -FilePath $CanontraExe -ArgumentList "repo `"$repoPath`" --json --cache" -NoNewWindow -PassThru -Wait -RedirectStandardOutput $warmOut -RedirectStandardError $warmErr
        $swWarm.Stop()
        $warmMs = [Math]::Round($swWarm.Elapsed.TotalMilliseconds, 2)

        # Parse Manifest
        $repoHash = ""
        $wcgHash = ""
        $wdfHash = ""
        $indexedFiles = 0
        if (Test-Path $coldOut) {
            try {
                $rawJson = Get-Content $coldOut -Raw
                $json = ConvertFrom-Json -InputObject $rawJson
                $repoHash = $json.repository_fingerprint
                $wcgHash = $json.whole_repo_call_graph
                $wdfHash = $json.whole_repo_data_flow
                if ($null -ne $json.files) {
                    $indexedFiles = $json.files.Count
                }
            } catch {
                Write-Warning "Could not parse JSON output for ${repo}"
            }
        }

        $speedup = if ($warmMs -gt 0) { [Math]::Round(($coldMs / $warmMs), 2) } else { 1.0 }
        $speedupStr = "${speedup}x"
        $coldSec = $coldMs / 1000.0
        $throughput = if ($coldSec -gt 0) { [Math]::Round($totalLoc / $coldSec, 0) } else { 0 }

        $summaryLine = "`"$repo`",`"$lang`",`"$totalFiles`",`"$indexedFiles`",`"$totalLoc`",`"$coldMs`",`"$warmMs`",`"$speedupStr`",`"$throughput`",`"$repoHash`",`"$wcgHash`",`"$wdfHash`",`"$($pCold.ExitCode)`",`"$($pWarm.ExitCode)`""
        $summary += $summaryLine

        Write-Host "  * ${repo}: ${indexedFiles}/${totalFiles} files, ${totalLoc} LOC | Cold: ${coldMs}ms, Warm: ${warmMs}ms (${speedupStr}) | Throughput: ${throughput} LOC/s" -ForegroundColor Green
    }
}

$summaryPath = Join-Path $OutputDir "v0.2.0_benchmark_summary.csv"
$summary | Out-File -FilePath $summaryPath -Encoding ascii
Write-Host "Canontra v0.2.0 Benchmark execution complete. Results saved to $summaryPath" -ForegroundColor Green
