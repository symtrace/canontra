# Changelog

All notable changes to `canontra` are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the [Haskell Package Versioning Policy (PVP)](https://pvp.haskell.org/)
and [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0.0] - 2026-10-09

### Hardened Performance, Precision & Soundness Milestone

The v0.2.0 release delivers major performance optimizations, zero-copy caching, multi-core work-stealing parallelism, modern grammar conformance across polyglot ecosystems, zero-trust platform hardening, and an exhaustive empirical benchmark evaluation.

#### Added

- **Unboxed Compressed Sparse Row (CSR) Graph Engine (`Canontra.Analysis.CSRGraph`)**:
  - Contiguous unboxed `Vector Word32` / `Vector Word16` representations (`csrRowOffsets`, `csrColIndices`, `csrEdgeFlags`) reducing whole-repository graph memory footprints by over 70%.
  - Linear-time Tarjan Strongly Connected Component (SCC) cycle collapse and canonical topological condensation DAG synthesis computed directly over unboxed vectors.
  - $O(\log(\text{deg}(u)))$ binary-search edge queries (`csrHasEdge`), linear-time graph transposition (`transposeCSR`), and forward/backward reachability cone masks.
  - Integration with `WholeRepoGraph`: replaced boxed `Map Symbol (Set Symbol)` representations with high-performance unboxed CSR call graphs (`toCSRCallGraph`) and data-flow graphs (`toCSRDataFlowGraph`).

- **CNTR\x06 Zero-Copy Memory-Mapped Slab Cache (`Canontra.Cache.SlabV6`)**:
  - 64-byte fixed-width cache records (`CacheRecordV6`) aligned precisely to CPU cache lines with pure `Storable` serialization.
  - 256-way L1 Radix Jump Table (`0x0020 - 0x081F`) enabling 1-cycle CPU fast-path indexing for warm file lookups.
  - Zero-copy memory-mapped verification via `openSlabCache`, `lookupSlabCacheWarm`, and pure `lookupSlabBinaryBS`, achieving sub-microsecond warm lookups (< 500 ns per file).
  - Whole-repository binary CSR graph persistence (`saveRepoGraphsSlab` / `loadRepoGraphsSlab`) replacing textual cache serialization.
  - Isolated 4KB page IEEE 802.3 CRC-32C bit-rot recovery (`salvageSlabCacheFile`, `readSlabCacheFileResilient`), dropping only damaged pages while salvaging intact cache entries.

- **Hardware SIMD FastScan & Lock-Free Work-Stealing Parallelism (`Canontra.Canonical.SIMDScan` & `Canontra.Repository.Parallel`)**:
  - 256-bit SIMD FastScan kernel (`scanSourceSIMD`, `fastCanonicalizeSIMD`, `isPureAsciiUnixSIMD`) evaluating 32 bytes per cycle via 4x 64-bit SWAR vector lanes for non-ASCII bytes, Windows CRLF line endings, string quotes, and comment delimiters with zero C-FFI.
  - Chase-Lev lock-free work-stealing parallel scheduler (`ChaseLevDeque`, `parProcessWorkStealing`) with dynamic circular buffer growth, LIFO worker pops, FIFO remote steals, and deterministic stream reassembly.
  - Sustained multi-core ingestion throughput reaching $\ge 100,000$ LOC/s on multi-core benchmark runners.
  - Localized reachability-cone incremental graph hot-updates (`spliceCSREdges`, `reachabilityConeUnion`, `incrementalUpdateWholeRepoGraphs`) executing in under 10 ms without rebuilding whole-repository graphs.

- **Polyglot Grammar Conformance & Modern Language Support**:
  - **Python 3.12**: PEP 701 nested f-strings with arbitrary quote reuse and inline comments, PEP 695 generic type parameter syntax (`type Alias[T] = ...`, `def func[T, **P]()`, `class Store[K, V]`), single-expression generator arguments, and comprehensive PEP 572 walrus operator `:=` scope hoisting across list, dict, set, and generator comprehensions into enclosing function scopes and `DFG` reaching definitions.
  - **TypeScript 5.2 / JavaScript**: Explicit resource management (`using` and `await using`) with CFG synthesis of synthetic disposal exit blocks (`Symbol.dispose`) and exceptional cleanup edges (`CondException "*"`), alongside context-aware two-token lookahead regex vs division operator disambiguation following curly braces `}`.
  - **Go 1.21+**: Builtins (`min`, `max`, `clear`), tilde constraint sets (`~T`) with commutative union normalization (`~int | ~float64 == ~float64 | ~int`), and structural type cyclic struct recursion breaker emitting `TypeRecVar 0`.
  - **Rust 2021**: Generic Associated Types (GATs) lifetime normalization (`'a` $\to$ `'0`), trait associated types, and raw identifier syntax interning (`r#type`, `r#match` interned to bit-identical `SymbolId` in `SwissTable`).

- **Zero-Trust Security, Resilient I/O & Platform Hardening**:
  - Windows Antivirus/Indexer Atomic Swap Resiliency: Exponential backoff with monotonic jitter (`atomicSwapWithRetry`, `atomicSwapWithRetry_`) across all cache writers (`SlabV6`, `PagedCache`, `MerkleCache`) eliminating transient Windows Defender / SearchIndexer file sharing violations.
  - Cross-Volume Symlink Loop Breaker: Composite `FileNodeIdentity` (`fniVolumeID`, `fniFileID`) tracking in `Canontra.Security.Path` preventing infinite circular traversal across NTFS junctions, mounted volumes, and POSIX symlinks.
  - Case-Folding Path Collation: Cross-platform deterministic Unicode-aware path collation ensuring bit-identical Merkle roots ($F_R$) across case-sensitive Linux ext4 and case-insensitive Windows NTFS file systems.
  - Hard resource ceiling enforcement: 50 MB file size limit, 64-level directory recursion limit, and AST depth protection.

- **Exhaustive Metamorphic Mutation & Soundness Verification Suite**:
  - Over 160 new automated test cases across unit, metamorphic property, and mutation suites, bringing the project total to 618 passing tests with 0 failures under GHC 9.6.6 with `-Wall -Werror --pedantic`.
  - Multi-language metamorphic property suites verifying algebraic invariance of $F_1$, $F_2$, $F_3$, $F_4$, $F_T$, and $F_R$ under semantics-preserving trivia transformations and strict divergence under semantic perturbations.

- **v0.2.0 Empirical Benchmarking Harness & 15-Repository Evaluation**:
  - Expanded tasty-bench microbenchmark harness (`bench/Bench.hs`) with RQ13 (unboxed CSR graph algorithms), RQ14 (`CNTR\x06` zero-copy slab cache), and RQ15 (hardware SIMD FastScan & work-stealing scheduler) across 135 total benchmarks.
  - End-to-end multi-language empirical evaluation across 15 real-world repositories (Flask, Gin, Ripgrep, Deno Core, Prometheus, Hugo, Rich, Click, Requests, Marshmallow, Chalk, Express, Jinja2, Bottle, Toml) documenting cold-cache and warm-cache latencies, memory footprint, and whole-repository graph synthesis.
  - Comprehensive documentation updates across [BENCHMARKS.md](BENCHMARKS.md), [REAL_WORLD_BENCHMARKS.md](REAL_WORLD_BENCHMARKS.md), and [benchmarkReport.md](benchmarkReport.md).


## [0.1.0.0] - 2026-09-22

### Production Release - Multi-Tier Polyglot Program Identity & Semantic Graph Engine

The milestone production release transitions Canontra from exploratory research tracks into a hardened, high-throughput static analysis runtime, build cache adapter, and production CLI tool for CI/CD fabrics, monorepo systems, and local developer workflows.

#### Added

- **Unified 9-Tier Identity Manifest**:
  - Full single-file orthogonal tiers: $F_0$ (Source Bytes), $F_1$ (Normalized AST), $F_2$ (Public Declarations), $F_3$ (Module Dependencies), $F_{CG}$ (Call Graph), $F_{CF}$ (Control Flow), $F_{DF}$ (Data Flow), $F_T$ (Structural Type Contracts), and $F_4$ (Composite Cryptographic Identity).
  - Whole-repository projections: $F_R$ (Incremental Merkle Root), $F_{WCG}$ (Inter-Module Call Graph), $F_{WDF}$ (Whole-Repo Data Flow), and $F_{W4}$ (Composite Workspace Identity).
- **Production CLI Subcommand Suite**:
  - `fp`: Multi-tier fingerprint generation for files and directories.
  - `compare`: Semantic equivalence verification proving invariance under formatting, comments, or refactorings.
  - `diff`: Fine-grained structural and AST diff diagnostics between revisions.
  - `graph`: Visualization and export of call graphs, CFGs, DFGs, and whole-repository dependency networks.
  - `impact`: Slicing and change impact analysis for files and specific symbols.
  - `slice`: Forward and backward program slicing based on dominance-frontier def-use chains.
  - `repo`: Repository-wide Merkle DAG generation and workspace analysis.
  - `watch`: Real-time file system watcher and in-memory incremental Merkle DAG daemon.
  - `cache`: Paged radix cache management, statistics, and integrity verification.
  - `export`: Export graphs to OASIS SARIF v2.1.0, Graphviz DOT, and Mermaid diagram formats.
  - `verify`: Determinism and metamorphic mutation verification testing.
- **UNIX Stream Interoperability & Formats**:
  - Stdin/stdout stream piping support via `-` indicator.
  - Machine-readable outputs: pretty-printed JSON, line-delimited JSONL, SARIF v2.1.0, and formatted terminal tables.
  - Strict POSIX exit codes: `0` (Identical/Success), `1` (Different), `2` (CLI/Syntax Error), `3` (IO/Filesystem Error), `4` (Integrity Failure).
- **Shell Autocompletion & Native Installers**:
  - Built-in completion generators for Bash, Zsh, Fish, and PowerShell (`canontra completions <shell>`).
  - Cross-platform zero-dependency standalone installation scripts (`install.sh` for POSIX, `install.ps1` for Windows).
- **Testing & Verification**:
  - Over 450 comprehensive test cases spanning property-based QuickCheck tests, metamorphic mutation verification, and cross-platform invariance with 0 failures under `-Wall -Werror`.

## [0.0.9-alpha] - 2026-09-18

### Whole-Repository Synthesis, Change Impact Analysis & Paged Radix Cache

#### Added

- **Whole-Repository Semantic Synthesis**:
  - Cross-module call graph synthesis ($F_{WCG}$) and inter-procedural SSA data flow analysis ($F_{WDF}$).
  - Module import resolution across relative paths and package hierarchies.
- **Semantic Change Impact Analysis (CIA)**:
  - Symbol-level dependency tracking to calculate the minimal downstream test and build invalidation set.
  - Forward and backward slicing over semantic graph representations.
- **Memory-Mapped Paged Radix Cache (`CNTR\x05`)**:
  - 4KB page-aligned memory layout with atomic flush guarantees.
  - Sub-microsecond warm-cache lookup latency ($2.70\,\mu\text{s}$ over 1,000 files).
  - IEEE 802.3 32-bit CRC checksum integrity validation per page.
- **In-Memory Watch Daemon**:
  - `canontra watch` background engine with OS-native file change detection and incremental DAG updates in $< 25\,\text{ms}$.
- **Structural Type Contract Tier ($F_T$)**:
  - Structural subtyping and interface contract hashing across Python protocols, TypeScript interfaces, Go interfaces, and Rust traits.

## [0.0.8-alpha] - 2026-09-14

### Hardened Semantic Precision & Polyglot Grammar Conformance

#### Added

- **`HybridIndentStack` Engine**:
  - Replaced fixed-width 7-level indentation bitmasks with an unboxed hybrid register-heap stack supporting arbitrary indentation nesting depth.
- **Polyglot Grammar Edge-Case Conformance**:
  - Python: PEP 634 pattern matching (`match/case`), walrus operator (`:=`) scope hoisting, and nested multi-expression f-strings.
  - TypeScript / JavaScript: Automatic Semicolon Insertion (ASI) rules, regular expression vs. division operator disambiguation state machine, and JSX fragment handling.
  - Go: Parameterized generic type parameters (`[T any]`), type constraints, and factored import/type blocks.
  - Rust: Macro token tree matching, trait object bounds, and lifetime annotations.
- **Graph Soundness Hardening**:
  - Sound short-circuit evaluation paths in basic block CFG construction.
  - Dominance-frontier calculation for SSA Data-Flow Graphs.

## [0.0.7-alpha] - 2026-09-10

### Nanosecond Systems Engineering & Memory Layouts

#### Added

- **Eytzinger Radix Cache Layout (`CNTR\x03`)**:
  - Breadth-First Search (BFS) array ordering for binary search acceleration, fitting search paths directly within CPU L1/L2 cache lines.
  - 64-bit FastPath hash filter eliminating string comparison on non-matching entries.
- **Flat Linear Arena AST (`LinearAST`)**:
  - Flattened pointerless vector storage for AST nodes, eliminating heap pointer chasing and reducing memory fragmentation.
- **SwissTable Symbol Interning**:
  - SIMD-accelerated 16-way control-byte probing for sub-35ns string deduplication and symbol interning.
- **Hierarchical Incremental Merkle DAG**:
  - Fast single-leaf delta propagation for repository root hash ($F_R$) calculation in $< 1\,\mu\text{s}$.

## [0.0.6-alpha] - 2026-09-06

### Hardware-Speed Latency Annihilation & Fused Streaming

#### Added

- **SWAR 64-Bit Fast Scanner**:
  - SIMD-within-a-register algorithm processing 8 bytes per iteration for instant ASCII validation, UTF-8 checking, and CRLF (`\r\n` $\to$ `\n`) newline normalization.
- **Fused Direct-to-Hash Streaming**:
  - Fused canonical binary serialization directly into SHA-256 state contexts, bypassing intermediate `ByteString` buffer allocation.
- **Fixed-Width Binary Cache (`CNTR\x02`)**:
  - Replaced legacy JSON cache serialization with a compact binary disk format achieving $80.7\,\text{ns}$ record decoding.

## [0.0.5-alpha] - 2026-09-02

### Direct-to-IR Parsing Architecture

#### Changed

- Migrated parsing frontend from third-party concrete syntax tree (CST) wrappers to direct-to-IR recursive descent parsers using `flatparse`.
- Slashed parsing latency by $85\%$, achieving sub-millisecond parsing across standard production modules with zero C-FFI runtime overhead.

## [0.0.4-alpha] - 2026-08-28

### High-Throughput Optimization & Pipeline Fusion

#### Added

- **Fused Single-Pass Traversal**:
  - Combined AST desugaring, scope resolution, and graph construction into a unified pass.
- **Unboxed Vector Allocations**:
  - Employed unboxed contiguous vectors for graph edges and symbol identifiers, driving garbage collection pauses to $< 0.2\%$.
- **Parallel Work Scheduler**:
  - Multi-core chunked processing using Haskell's lightweight green threads (`async`), achieving $> 1,200,000$ LOC/s aggregate throughput.

## [0.0.3-alpha] - 2026-08-22

### Polyglot Expansion & Deep Semantic Graphs

#### Added

- **Three Polyglot Frontends**:
  - Full ingestion for JavaScript / TypeScript (`.js`, `.jsx`, `.ts`, `.tsx`), Go (`.go`), and Rust (`.rs`).
- **Deep Semantic Graph Extractors**:
  - Control Flow Graph ($F_{CF}$) tracking basic block branching and loop headers.
  - Data Flow Graph ($F_{DF}$) tracking reaching definitions and def-use chains.
- **Mathematical Invariance Hardening**:
  - IEEE-754 floating-point canonicalization (canonical NaN representation and sign bit normalization).
  - Unicode NFC normalization for all identifier strings.
  - Tarjan's Strongly Connected Components (SCC) algorithm for call graph recursion cycle handling.

## [0.0.2-alpha] - 2026-08-15

### Modern Python 3.8+ & Zero-Span Ingestion

#### Added

- **Zero-Span AST Representation**:
  - Stripped all source location spans (line/column metadata) from internal nodes to guarantee that purely positional movements do not leak into structural hashes.
- **Modern Python Grammar Coverage**:
  - Full support for assignment expressions (`:=`), positional-only parameters (`/`), type hints, async/await coroutines, generators, and structural pattern matching.
- **Lexical Scope & Call Graphs**:
  - Scope tree resolution and intra-module static call graph generation ($F_{CG}$).
  - Fine-grained structural diffing separating cosmetic whitespace from algorithmic changes.

## [0.0.1-alpha] - 2026-08-08

### Initial Proof of Concept & Mathematical Foundations

#### Added

- **Pure-Haskell Deterministic Pipeline**:
  - Initial proof of concept demonstrating deterministic program identity implemented in 100% pure Haskell with zero runtime dependencies.
- **Orthogonal Multi-Tier Fingerprinting**:
  - Defined the initial 5-tier cryptographic hierarchy:
    - $F_0$: Raw source text byte digest (SHA-256).
    - $F_1$: Normalized Abstract Syntax Tree digest invariant under comments, whitespace, and docstrings.
    - $F_2$: Public declaration signature digest for exported API surfaces.
    - $F_3$: Dependency and import topology digest.
    - $F_4$: Composite single-file program digest.
  - $F_R$: Deterministic repository Merkle tree digest.
- **Python 3 Ingestion**:
  - Parsing, AST normalization, dead statement removal, and canonical serialization for Python modules.
- **Command-Line Interface**:
  - Basic CLI for single-file and directory fingerprint computation and semantic comparison.
