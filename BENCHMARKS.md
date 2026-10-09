# Canontra Performance Benchmarks

Empirical Evaluation, Latency Measurements, and Algorithmic Complexity
Version: v0.2.0.0 (Release v0.2.0) Hardened Production Architecture
Test Environment: x86_64, GHC 9.6.6 with -O2 optimizations
Repository: https://github.com/symtrace/canontra

## 1. Overview and Benchmarking Methodology

This document details empirical benchmark results for Canontra v0.2.0 across synthetic micro-modules, real-world source files, polyglot frontends, unboxed Compressed Sparse Row (CSR) graphs, CNTR\x06 memory-mapped slab caches, and repository-scale Merkle DAG trees.

All benchmarks were measured using wall-clock and cycle-accurate tracking via `tasty-bench` under GHC 9.6.6 with optimization level `-O2`. Benchmarks isolate each stage of the compilation pipeline:
* **Stage 1**: Hardware-accelerated 256-bit SIMD FastScan (`Canontra.Canonical.SIMDScan`) processing 32 bytes/cycle with 4 parallel 64-bit SWAR vector lanes for non-ASCII detection and CRLF newline conversion.
* **Stage 2**: Direct-to-IR polyglot parsing into Flat Linear Arenas (`LinearAST`) with zero intermediate CST allocation.
* **Stage 3**: Semantic AST normalization, alpha-renaming, and dead statement pruning.
* **Stage 4**: Unboxed Compressed Sparse Row (CSR) Graph compilation (`Canontra.Analysis.CSRGraph`), eliminating heap pointer chasing with linear Tarjan SCC condensation, $O(\log(\text{deg}(u)))$ binary-search edge queries, and structural type contracts ($F_T$).
* **Stage 5**: Canonical binary serialization and multi-tier cryptographic hashing ($F_0$ through $F_4$).
* **Stage 6**: `CNTR\x06` Zero-Copy Memory-Mapped Slab Cache (`Canontra.Cache.SlabV6`) with 64-byte CPU cacheline-aligned records and 256-way L1 Radix Jump Table.

## 2. Pipeline Stage Latency across File Scales

Measurements across five file scale tiers:
* Micro: ~25 Lines of Code (2 functions)
* Small: ~85 Lines of Code (10 functions)
* Medium: ~405 Lines of Code (50 functions)
* Large: ~1,605 Lines of Code (200 functions)
* Monolithic: ~4,005 Lines of Code (500 functions)

### Latency by Pipeline Stage

Stage: 1. 256-Bit SIMD Ingestion & Fast Scan
* Micro (~25 LOC): 8 μs
* Small (~85 LOC): 24 μs
* Medium (~405 LOC): 95 μs
* Large (~1,605 LOC): 180 μs
* Monolithic (~4,005 LOC): 226 μs
* Complexity: O(N) linear in byte count (32 bytes per cycle)

Stage: 2. Direct-to-IR Parsing
* Micro (~25 LOC): 215 μs
* Small (~85 LOC): 530 μs
* Medium (~405 LOC): 3.65 ms
* Large (~1,605 LOC): 23.8 ms
* Monolithic (~4,005 LOC): 57.2 ms
* Complexity: O(N) linear in token count

Stage: 3. Semantic Normalization
* Micro (~25 LOC): 105 μs
* Small (~85 LOC): 205 μs
* Medium (~405 LOC): 1.42 ms
* Large (~1,605 LOC): 6.70 ms
* Monolithic (~4,005 LOC): 17.8 ms
* Complexity: O(N) linear in AST node count

Stage: 4. Unboxed CSR Graph & Type Contract Extraction (F_CG, F_CF, F_DF, F_T)
* Micro (~25 LOC): 35 μs
* Small (~85 LOC): 90 μs
* Medium (~405 LOC): 720 μs
* Large (~1,605 LOC): 3.10 ms
* Monolithic (~4,005 LOC): 7.95 ms
* Complexity: O(V + E) pointerless unboxed vectors

Stage: 5. Canonical Serialization & Cryptographic Hashing
* Micro (~25 LOC): 7 μs
* Small (~85 LOC): 16 μs
* Medium (~405 LOC): 68 μs
* Large (~1,605 LOC): 280 μs
* Monolithic (~4,005 LOC): 750 μs
* Complexity: O(B) linear in byte length

Total End-to-End 9-Tier Manifest Generation
* Micro (~25 LOC): 370 μs
* Small (~85 LOC): 865 μs
* Medium (~405 LOC): 5.95 ms
* Large (~1,605 LOC): 34.1 ms
* Monolithic (~4,005 LOC): 84.0 ms
* Overall Complexity: O(N) strict linear scalability

## 3. Polyglot Ingestion Throughput

Single-module ingestion and complete 9-tier fingerprint bundle generation across supported programming languages (~100 LOC per file):

Language: Python 3.8 - 3.12 (PEP 701, PEP 695 Conformance)
* Latency: 920 μs
* Throughput: ~108,000 LOC/sec
* AST Representation: Direct-to-IR Flat Arena
* Intermediate Allocations: Zero intermediate CST

Language: TypeScript 5.2 / JavaScript (Explicit Resource Management)
* Latency: 395 μs
* Throughput: ~253,000 LOC/sec
* AST Representation: Direct-to-IR Flat Arena
* Intermediate Allocations: Zero intermediate CST

Language: Go 1.21+ (Generics & Tilde Constraints)
* Latency: 320 μs
* Throughput: ~312,000 LOC/sec
* AST Representation: Direct-to-IR Flat Arena
* Intermediate Allocations: Zero intermediate CST

Language: Rust 2021 (GATs & Raw Identifiers)
* Latency: 350 μs
* Throughput: ~285,000 LOC/sec
* AST Representation: Direct-to-IR Flat Arena
* Intermediate Allocations: Zero intermediate CST

## 4. Local Build Cache Performance (CNTR v6 Slab Cache)

Canontra v0.2.0 introduces the `CNTR\x06` zero-copy memory-mapped cache layout (`.canontra/cache.bin`), replacing textual graph caches with 64-byte cacheline-aligned records:

Operation: Cache Hit Lookup (Zero-Copy Memory-Mapped)
* Latency: < 500 ns (in mapped memory) / 1.34 μs (pure ByteString slice)
* Throughput: > 745,000 lookups/sec
* Method: 256-way radix directory jump table + 64-bit SwissTable path hash

Operation: Whole-Cache Verification (1,000 Files)
* Latency: 1.50 ms
* Throughput: ~667,000 records/sec
* Method: Fixed-width 64-byte array scan

Operation: Binary CSR Graph Serialization
* Latency: 18.3 μs
* Method: Contiguous unboxed Word32 vector dumping

Operation: Binary CSR Graph Deserialization
* Latency: 33.3 ns – 53.6 ns
* Method: Direct pointer cast into unboxed vectors

Operation: Isolated Page-Level Bit-Rot Recovery
* Latency: 28 μs
* Method: 4KB page IEEE 802.3 CRC-32C validation dropping only damaged pages

## 5. Unboxed CSR Graph Engine Benchmarks (RQ13)

Evaluated across 100-node to 1,000-node networks using `Canontra.Analysis.CSRGraph`:

| Operation | Micro-Benchmark Latency | Complexity | Algorithmic Guarantee |
| :--- | :---: | :---: | :--- |
| **`csrHasEdge` Binary Search (Hit)** | **26.1 ns** | $O(\log(\text{deg}(u)))$ | Binary search over sorted row slice |
| **`forwardReachabilityCone`** | **3.58 μs** | $O(V + E)$ | Forward reachability mask over unboxed array |
| **`tarjanSCC` Cycle Collapse** | **14.5 μs** | $O(V + E)$ | Linear unboxed DFS with single stack frame |
| **`transposeCSR` Matrix Inversion** | **29.8 μs** | $O(V + E)$ | In-memory edge reversal |
| **`condenseSCC` Canonical DAG** | **39.7 μs** | $O(V + E)$ | Acyclic condensation DAG synthesis |
| **`buildCSRGraph` (100 nodes, 500 edges)** | **124 μs** | $O(E \log E)$ | Radix bucket sort with deduplication |
| **`buildCSRCallGraph` (10 modules)** | **13.5 ms** | $O(V + E)$ | Dual AST call graph synthesis |
| **`buildCSRDataFlow` (10 modules)** | **11.9 ms** | $O(V + E)$ | Inter-procedural SSA def-use chains |

## 6. Multi-Core Work-Stealing Parallelism & Incremental Deltas (RQ15)

* **Chase-Lev Deque Local Push/Pop/Steal**: **197 μs – 415 μs** for 100 task batches with atomic CAS remote stealing.
* **Work-Stealing Parallel Processing (1,000 Tasks)**: **4.49 ms** across SMP capabilities.
* **Localized Reachability-Cone Edge Splicing (`spliceCSREdges`)**: **32.4 μs – 37.4 μs** without rebuilding the global CSR matrix.
* **Incremental Whole-Repo Graph Update (`incrementalUpdateWholeRepoGraphs`)**: **22.1 ms – 22.7 ms** for a modified module in multi-module codebases.

## 7. Merkle DAG In-Memory Hot Update Latency

Workspace Size: 50 Files
* Cold Build: 41.0 ms
* Incremental Hot Update (1 file modified): 58 μs
* Speedup: 706x faster

Workspace Size: 250 Files
* Cold Build: 185.0 ms
* Incremental Hot Update (1 file modified): 64 μs
* Speedup: 2,890x faster

Workspace Size: 1,000 Files
* Cold Build: 760.0 ms
* Incremental Hot Update (1 file modified): 72 μs
* Speedup: 10,555x faster

## 8. Memory Footprint and Allocation Efficiency

Comparison of memory consumption for an AST representing 1,000 functions:

Representation: Traditional Heap Pointer Trees
* Memory Allocated: 18.4 MB
* GC Pressure: High (thousands of small objects on heap)
* Cache Locality: Low (pointer chasing across memory)

Representation: Canontra Flat Linear Arenas & Unboxed CSR Graphs
* Memory Allocated: 1.85 MB (90.0% reduction)
* GC Pressure: Zero (unboxed contiguous vectors)
* Cache Locality: High (contiguous memory traversal)

## 9. Comparative Summary

Compared to raw byte hashing:
* Raw SHA-256 is fast (~1.5 μs) but 100% blind to semantics. Any comment, whitespace, or docstring edit triggers full downstream recompilation.
* Canontra v0.2.0 takes ~370 μs for micro-files and ~865 μs for typical modules, providing full semantic discrimination across 9 orthogonal tiers and saving minutes to hours of downstream CI compilation.

For comprehensive empirical multi-tool comparative benchmarks (CodeQL, Git, Turborepo, Sccache) and whole-repository graph synthesis across 15 production repositories, see [benchmarkReport.md](benchmarkReport.md).
