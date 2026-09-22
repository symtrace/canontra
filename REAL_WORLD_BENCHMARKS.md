# Canontra Real-World Benchmark Protocol and Execution Instructions

Version: v0.1.0
Author: Jash Thakkar & SymtraceLabs Engineering Team
Status: Benchmark Execution Protocol

## 1. Overview and Purpose

This document provides complete instructions for executing real-world empirical benchmarks of the Canontra engine.

The benchmark protocol evaluates Canontra across an ordered corpus of 15 open-source repositories spanning Python, JavaScript, TypeScript, Go, and Rust. The repositories are arranged progressively from lowest file count and lines of code (LOC) to highest volume, measuring:

1. Full-Corpus Ingestion Coverage: Verifying that all valid source files parse into Flat Linear Arenas without errors.
2. Bit-for-Bit Determinism: Proving 100% hash reproducibility across repeated runs (Delta F = 0).
3. Throughput and Scalability: Recording processing speed in lines of code per second (LOC/s) across different codebase scales.
4. Semantic Mutation Discrimination: Verifying that formatting churn preserves F1/F2/F_T hashes while functional edits trigger strict divergence.
5. Repository-Scale Merkle Aggregation: Measuring whole-repository root hash (F_R) computation times and incremental hot-update latencies.

## 2. Target Repository Corpus (Ordered by Scale)

The evaluation suite uses 15 target repositories ordered from smallest to largest:

1. bottle (Python)
   * Repository: https://github.com/bottlepy/bottle
   * Estimated Files: ~30 files
   * Estimated Volume: ~9,200 LOC
   * Characteristics: Single-file core micro-framework with minimal dependencies.

2. toml-rs (Rust)
   * Repository: https://github.com/toml-rs/toml
   * Estimated Files: ~35 files
   * Estimated Volume: ~11,000 LOC
   * Characteristics: Fast TOML parser with serde integration and syntax tree traversal.

3. requests (Python)
   * Repository: https://github.com/psf/requests
   * Estimated Files: ~37 files
   * Estimated Volume: ~12,000 LOC
   * Characteristics: Standard HTTP library with sessions, adapters, and model declarations.

4. flask (Python)
   * Repository: https://github.com/pallets/flask
   * Estimated Files: ~45 files
   * Estimated Volume: ~14,000 LOC
   * Characteristics: Web framework with blueprints, routing tables, and decorators.

5. marshmallow (Python)
   * Repository: https://github.com/marshmallow-code/marshmallow
   * Estimated Files: ~38 files
   * Estimated Volume: ~15,700 LOC
   * Characteristics: Complex schema serialization with type annotations and validation logic.

6. chalk (JavaScript)
   * Repository: https://github.com/chalk/chalk
   * Estimated Files: ~48 files
   * Estimated Volume: ~16,200 LOC
   * Characteristics: Modern terminal styling library with ES6 exports and color models.

7. click (Python)
   * Repository: https://github.com/pallets/click
   * Estimated Files: ~38 files
   * Estimated Volume: ~18,000 LOC
   * Characteristics: Command line interface composability toolkit with deep nesting.

8. gin (Go)
   * Repository: https://github.com/gin-gonic/gin
   * Estimated Files: ~42 files
   * Estimated Volume: ~19,500 LOC
   * Characteristics: High-performance HTTP web framework with Radix tree routing in Go.

9. jinja (Python)
   * Repository: https://github.com/pallets/jinja
   * Estimated Files: ~60 files
   * Estimated Volume: ~22,800 LOC
   * Characteristics: Lexer, parser, and runtime compiler for templating.

10. ripgrep (Rust)
    * Repository: https://github.com/BurntSushi/ripgrep
    * Estimated Files: ~95 files
    * Estimated Volume: ~38,000 LOC
    * Characteristics: Production systems command line search tool written in Rust.

11. express (JavaScript)
    * Repository: https://github.com/expressjs/express
    * Estimated Files: ~110 files
    * Estimated Volume: ~42,000 LOC
    * Characteristics: Classic web application framework with middleware pipelines.

12. rich (Python)
    * Repository: https://github.com/Textualize/rich
    * Estimated Files: ~213 files
    * Estimated Volume: ~51,800 LOC
    * Characteristics: Rich terminal text and table rendering with extensive typing.

13. hugo (Go)
    * Repository: https://github.com/gohugoio/hugo
    * Estimated Files: ~280 files
    * Estimated Volume: ~85,000 LOC
    * Characteristics: Static site engine with template parsing and asset pipelines.

14. deno_core (TypeScript and Rust)
    * Repository: https://github.com/denoland/deno_core
    * Estimated Files: ~320 files
    * Estimated Volume: ~120,000 LOC
    * Characteristics: Low-level JavaScript and TypeScript runtime bindings.

15. prometheus (Go)
    * Repository: https://github.com/prometheus/prometheus
    * Estimated Files: ~450 files
    * Estimated Volume: ~190,000 LOC
    * Characteristics: Distributed systems monitoring engine with complex data structures.

## 3. Step-by-Step Execution Instructions

Follow these steps to conduct the benchmark run.

### Step 1: Pre-Flight Environment Setup

Ensure the Canontra standalone binary is compiled with optimization flag -O2:

```bash
# Build optimized production binary
stack build --copy-bins --local-bin-path ./dist-bin --ghc-options="-O2"

# Verify executable is functional and reports version 0.1.0
./dist-bin/canontra version
```

On Windows PowerShell:

```powershell
stack build --copy-bins --local-bin-path .\dist-bin --ghc-options="-O2"
.\dist-bin\canontra.exe version
```

### Step 2: Workspace Staging Preparation

Create an isolated staging directory for the cloned repositories:

```bash
mkdir -p scratch/benchmarks
cd scratch/benchmarks
```

On Windows PowerShell:

```powershell
New-Item -ItemType Directory -Path scratch\benchmarks -Force | Out-Null
Set-Location scratch\benchmarks
```

### Step 3: Cloning Target Repositories

Use shallow clones with depth 1 over HTTPS. This minimizes network bandwidth and avoids cloning commit histories:

```bash
git clone --depth 1 https://github.com/bottlepy/bottle.git
git clone --depth 1 https://github.com/toml-rs/toml.git
git clone --depth 1 https://github.com/psf/requests.git
git clone --depth 1 https://github.com/pallets/flask.git
git clone --depth 1 https://github.com/marshmallow-code/marshmallow.git
git clone --depth 1 https://github.com/chalk/chalk.git
git clone --depth 1 https://github.com/pallets/click.git
git clone --depth 1 https://github.com/gin-gonic/gin.git
git clone --depth 1 https://github.com/pallets/jinja.git
git clone --depth 1 https://github.com/BurntSushi/ripgrep.git
git clone --depth 1 https://github.com/expressjs/express.git
git clone --depth 1 https://github.com/Textualize/rich.git
git clone --depth 1 https://github.com/gohugoio/hugo.git
git clone --depth 1 https://github.com/denoland/deno_core.git
git clone --depth 1 https://github.com/prometheus/prometheus.git
```

On Windows PowerShell:

```powershell
$repos = @(
    "https://github.com/bottlepy/bottle.git",
    "https://github.com/toml-rs/toml.git",
    "https://github.com/psf/requests.git",
    "https://github.com/pallets/flask.git",
    "https://github.com/marshmallow-code/marshmallow.git",
    "https://github.com/chalk/chalk.git",
    "https://github.com/pallets/click.git",
    "https://github.com/gin-gonic/gin.git",
    "https://github.com/pallets/jinja.git",
    "https://github.com/BurntSushi/ripgrep.git",
    "https://github.com/expressjs/express.git",
    "https://github.com/Textualize/rich.git",
    "https://github.com/gohugoio/hugo.git",
    "https://github.com/denoland/deno_core.git",
    "https://github.com/prometheus/prometheus.git"
)

foreach ($url in $repos) {
    $dirName = [System.IO.Path]::GetFileNameWithoutExtension($url)
    if (-not (Test-Path $dirName)) {
        Write-Host "Cloning $dirName (shallow)..."
        git clone --depth 1 $url
    }
}
```

### Step 4: Running the Ingestion and Throughput Benchmark

For each repository, run Canontra to scan all supported source files, compute complete 9-tier fingerprint manifests, and measure total elapsed time:

```bash
# Example for a single repository
time ../../dist-bin/canontra repo ./bottle --json > bottle_manifest.json
```

To run across the full corpus and record results in CSV format, use the automated benchmark runner:

On POSIX (bash):

```bash
CANONTRA="../../dist-bin/canontra"
OUTPUT_CSV="benchmark_summary.csv"

echo "Repository,Language,Files,LOC,ElapsedSeconds,ThroughputLOCs,Status" > $OUTPUT_CSV

for repo in bottle toml requests flask marshmallow chalk click gin jinja ripgrep express rich hugo deno_core prometheus; do
    if [ -d "$repo" ]; then
        echo "Benchmarking $repo..."
        START=$(date +%s%N)
        $CANONTRA repo "$repo" --json > "${repo}_results.json" 2> "${repo}_err.log"
        EXIT_CODE=$?
        END=$(date +%s%N)
        
        ELAPSED=$(awk "BEGIN {print ($END - $START) / 1000000000}")
        FILES=$(find "$repo" -type f \( -name "*.py" -o -name "*.js" -o -name "*.ts" -o -name "*.go" -o -name "*.rs" \) | wc -l)
        LOC=$(find "$repo" -type f \( -name "*.py" -o -name "*.js" -o -name "*.ts" -o -name "*.go" -o -name "*.rs" \) -exec wc -l {} + | awk 'END{print $1}')
        THROUGHPUT=$(awk "BEGIN {if ($ELAPSED > 0) print $LOC / $ELAPSED; else print 0}")
        
        STATUS="SUCCESS"
        [ $EXIT_CODE -ne 0 ] && STATUS="FAILED($EXIT_CODE)"
        
        echo "$repo,polyglot,$FILES,$LOC,$ELAPSED,$THROUGHPUT,$STATUS" >> $OUTPUT_CSV
        echo "  Done: $FILES files, $LOC LOC in ${ELAPSED}s (${THROUGHPUT} LOC/s)"
    fi
done
```

On Windows PowerShell:

```powershell
$CanontraExe = "..\..\dist-bin\canontra.exe"
$OutputCsv = "benchmark_summary.csv"

$repos = @("bottle", "toml", "requests", "flask", "marshmallow", "chalk", "click", "gin", "jinja", "ripgrep", "express", "rich", "hugo", "deno_core", "prometheus")

$results = @()
$results += "Repository,Files,LOC,ElapsedSeconds,ThroughputLOCs,ExitCode"

foreach ($repo in $repos) {
    if (Test-Path $repo) {
        Write-Host "Benchmarking $repo..." -ForegroundColor Cyan
        
        # Count source files and total lines of code
        $files = Get-ChildItem -Path $repo -Recurse -Include *.py, *.js, *.ts, *.go, *.rs -File
        $fileCount = $files.Count
        $loc = 0
        foreach ($f in $files) {
            $loc += (Get-Content $f.FullName | Measure-Object -Line).Lines
        }
        
        $sw = [System.Diagnostics.Stopwatch]::StartNew()
        $p = Start-Process -FilePath $CanontraExe -ArgumentList "repo `"$repo`" --json" -NoNewWindow -PassThru -Wait -RedirectStandardOutput "$repo`_manifest.json" -RedirectStandardError "$repo`_err.log"
        $sw.Stop()
        
        $elapsedSec = [math]::Round($sw.Elapsed.TotalSeconds, 3)
        $throughput = if ($elapsedSec -gt 0) { [math]::Round($loc / $elapsedSec, 1) } else { 0 }
        
        Write-Host "  -> $fileCount files, $loc LOC in ${elapsedSec}s ($throughput LOC/s)" -ForegroundColor Green
        $results += "$repo,$fileCount,$loc,$elapsedSec,$throughput,$($p.ExitCode)"
    }
}

$results | Out-File -FilePath $OutputCsv -Encoding ascii
Write-Host "`nBenchmark results written to $OutputCsv" -ForegroundColor Green
```

### Step 5: Determinism Verification Run

To verify mathematical repeat-execution determinism:
1. Run the benchmark tool 3 times consecutively on the same repository.
2. Compare the output JSON manifests using SHA-256:
   ```bash
   sha256sum bottle_run1.json bottle_run2.json bottle_run3.json
   ```
3. All three manifests must produce 100% bit-identical SHA-256 hashes.

### Step 6: Post-Benchmark Cleanup

To clean up cloned benchmark repositories and temporary manifests:

```bash
cd ../..
rm -rf scratch/benchmarks
```

On Windows PowerShell:

```powershell
Set-Location ..\..
Remove-Item -Recurse -Force scratch\benchmarks -ErrorAction SilentlyContinue
```

## 4. Expected Output Format and Verification Criteria

When evaluating the benchmark output CSV, ensure the results satisfy the following acceptance criteria:

* Parse Success Rate: >= 98% across all source files.
* Determinism Invariance: 100% bit-identical manifest hashes across identical runs.
* Peak Throughput: >= 10,000 LOC/s on large repositories.
* Average Latency: <= 1.5 ms per module on typical files (~100 LOC).
* Zero Exit Code Failures: All clean repositories must return exit code 0.
* Published Report: See [benchmarkReport.md](benchmarkReport.md) for full empirical multi-tool benchmark data and whole-repository graph synthesis results.
