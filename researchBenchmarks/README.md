# Canontra Research Benchmarks Artifact Repository

This directory contains the primary empirical research deliverables and benchmark datasets produced under Canontra benchmark protocol.

All metrics reflect live process executions on host hardware using high-resolution monotonic hardware timers (`System.Diagnostics.Stopwatch`).

## Directory Contents

| Filename / Pattern | Format | Description |
| :--- | :--- | :--- |
| **`benchmark_summary.csv`** | CSV | Comprehensive benchmark summary of Canontra cold/warm indexing across all 15 repositories, including $F_R$, $F_{WCG}$, $F_{WDF}$, throughput, and exit codes. |
| **`benchmark_comparative_summary.csv`** | CSV | Comparative multi-tool wall-clock timings across Canontra, Git, Turborepo, Sccache, and GitHub CodeQL. |
| **`mutation_eval_results.csv`** | CSV | Metamorphic mutation sensitivity evaluation across 14 trials, validating $\text{FDR} = 0.0\%$ and $\text{TDR} = 100.0\%$. |
| **`<repo>_manifest.json`** | JSON | Cold-pass cryptographic manifest for each repository containing full 9-tier fingerprints for every indexed file, plus repository root $F_R$, whole-repo call graph $F_{WCG}$, and whole-repo data flow $F_{WDF}$. |
| **`<repo>_cached_manifest.json`** | JSON | Warm-pass cryptographic manifest generated from persistent disk cache (`.canontra/`). |
| **`<repo>_err.log`** | Plaintext | Non-fatal parse warning logs capturing isolated syntax anomalies without aborting manifest generation. |
| **`index.json`** | JSON | Machine-readable aggregated index of all 15 repository benchmarks and graph digests. |

## Benchmark Corpus & Ingestion Results

| Repository | Primary Language | Total Files | Indexed Files | Total LOC | Cold Latency | Warm Latency | Throughput (LOC/s) | Repository Digest ($F_R$) | Whole-Repo Call Graph ($F_{WCG}$) | Whole-Repo Data Flow ($F_{WDF}$) |
| :--- | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :--- | :--- | :--- |
| **`bottle`** | Python | 30 | 16 | 7,759 | 1,020.87 ms | 1,011.52 ms | 7,599 | `936a0ddbc223603f...` | `f34a16e2e81a1947...` | `cbb258bf8a3cbdb1...` |
| **`toml`** | Rust | 166 | 166 | 44,360 | 3,013.35 ms | 4,020.49 ms | 14,723 | `96e8952b079bf6bd...` | `11b67768c0991a6f...` | `fd02a44d9175f518...` |
| **`requests`** | Python | 37 | 21 | 9,841 | 1,059.82 ms | 1,030.84 ms | 9,284 | `7e604c0a49accfd3...` | `10cc5961235e22ff...` | `7a3f99351a2b36e7...` |
| **`flask`** | Python | 83 | 42 | 14,085 | 1,023.72 ms | 1,013.70 ms | 13,755 | `8f8da4ce410e4c0b...` | `c6375cc40c5bca79...` | `0e97fbacfe8edadf...` |
| **`marshmallow`** | Python | 38 | 15 | 12,734 | 1,010.35 ms | 1,010.04 ms | 12,608 | `c9a4ece1dfe71f88...` | `60bf7bd4ba4ce718...` | `37e553e994a54bf5...` |
| **`chalk`** | JavaScript | 14 | 14 | 1,095 | 1,013.46 ms | 1,010.11 ms | 1,081 | `29ac0e1f57adfe5d...` | `dfcb69fb44dbd437...` | `3b650f241c6eeb7f...` |
| **`click`** | Python | 90 | 53 | 23,803 | 1,010.34 ms | 3,023.84 ms | 23,567 | `3728be435ffada78...` | `887c04593a382616...` | `023a49c097eb5ab1...` |
| **`gin`** | Go | 99 | 99 | 20,528 | 1,010.25 ms | 2,019.13 ms | 20,325 | `8f38b9486e63bfc9...` | `c4fba14af08efa07...` | `59e543218c88c9a0...` |
| **`jinja`** | Python | 60 | 25 | 18,825 | 1,010.44 ms | 1,011.69 ms | 18,639 | `b07f2640dd1014be...` | `7d1087a087bab2cb...` | `d544cbe367d050f8...` |
| **`ripgrep`** | Rust | 110 | 110 | 50,953 | 2,027.52 ms | 3,012.18 ms | 25,125 | `7006ed5f8a8832a3...` | `70c307b5eea3a681...` | `0c9a56d39379645c...` |
| **`express`** | JavaScript | 141 | 141 | 17,552 | 3,232.07 ms | 6,507.72 ms | 5,431 | `b080e84d8ce2a646...` | `c5d60e994e302ff6...` | `3586e558224128dc...` |
| **`rich`** | Python | 213 | 138 | 45,787 | 7,029.83 ms | 12,023.24 ms | 6,513 | `ea72a7af04051c61...` | `532d3397c8a0f884...` | `aa30d1c9785d52e8...` |
| **`hugo`** | Go | 937 | 937 | 202,891 | 19,037.05 ms | 30,065.97 ms | 10,658 | `1ed6be7e16bd264a...` | `6bfe180e256a625c...` | `f570074f8d53ccf4...` |
| **`deno_core`** | TS/Rust | 318 | 318 | 62,799 | 3,019.62 ms | 3,010.62 ms | 20,794 | `79ba965e53056d86...` | `47673203455b5ff0...` | `d43adb369099824b...` |
| **`prometheus`** | Go | 994 | 994 | 388,080 | 46,059.46 ms | 66,157.62 ms | 8,426 | `e82e575132086594...` | `fe189db386669c0f...` | `fd7269054fa17154...` |

*Note: All 15 manifests emit non-null, 64-character SHA-256 hashes for both Whole-Repository Call Graph ($F_{WCG}$) and Whole-Repository Data-Flow Graph ($F_{WDF}$). Zero values are null.*

## Live Multi-Tool Empirical Comparison

| Repository | Language | LOC | Canontra Cold (s) | Canontra LOC/s | Git Hashing (s) | Turborepo (s) | Sccache (s) | CodeQL DB Create (s) | Canontra Speedup vs CodeQL |
| :--- | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| **`bottle`** | Python | 7,759 | **1.021s** | 7,599 | 1.456s | 0.031s | N/A | 18.052s | **17.7×** |
| **`toml`** | Rust | 44,360 | **3.013s** | 14,723 | 7.909s | 0.177s | 3.308s | 279.388s | **92.7×** |
| **`requests`** | Python | 9,841 | **1.060s** | 9,284 | 2.514s | 0.039s | N/A | 18.054s | **17.0×** |
| **`flask`** | Python | 14,085 | **1.024s** | 13,755 | 4.212s | 0.056s | N/A | 19.060s | **18.6×** |
| **`marshmallow`** | Python | 12,734 | **1.010s** | 12,608 | 1.795s | 0.051s | N/A | 17.093s | **16.9×** |
| **`chalk`** | JavaScript | 1,095 | **1.013s** | 1,081 | 0.695s | 0.140s | N/A | 22.040s | **21.8×** |
| **`click`** | Python | 23,803 | **1.010s** | 23,567 | 4.074s | 0.095s | N/A | 19.057s | **18.9×** |
| **`gin`** | Go | 20,528 | **1.010s** | 20,325 | 4.525s | 0.082s | 2.626s | 25.045s | **24.8×** |
| **`jinja`** | Python | 18,825 | **1.010s** | 18,639 | 2.699s | 0.075s | N/A | 19.059s | **18.9×** |
| **`ripgrep`** | Rust | 50,953 | **2.028s** | 25,125 | 4.931s | 0.204s | 3.493s | 249.391s | **123.0×** |
| **`express`** | JavaScript | 17,552 | **3.232s** | 5,431 | 6.732s | 1.365s | N/A | 26.056s | **8.1×** |
| **`rich`** | Python | 45,787 | **7.030s** | 6,513 | 9.882s | 0.183s | N/A | 29.100s | **4.1×** |
| **`hugo`** | Go | 202,891 | **19.037s** | 10,658 | 42.110s | 0.812s | 9.262s | 266.449s | **14.0×** |
| **`deno_core`** | TS/Rust | 62,799 | **3.020s** | 20,794 | 14.516s | 1.151s | 3.829s | 27.050s | **9.0×** |
| **`prometheus`** | Go | 388,080 | **46.059s** | 8,426 | 47.279s | 1.552s | 14.640s | 1,043.217s | **22.6×** |
