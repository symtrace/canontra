# Canontra Performance Benchmarks

Empirical Evaluation, Latency Measurements, and Algorithmic Complexity
Version: v0.1.0 Production Architecture
Test Environment: x86_64, GHC 9.6.6 with -O2 optimizations
Repository: https://github.com/symtrace/canontra

## 1. Overview and Benchmarking Methodology

This document details empirical benchmark results for Canontra v0.1.0 across synthetic micro-modules, real-world source files, polyglot frontends, and repository-scale Merkle DAG trees.

All benchmarks were measured using wall-clock time tracking under GHC 9.6.6 with optimization level -O2. Benchmarks isolate each stage of the compilation pipeline:
* Stage 1: Fast scanning and SWAR CRLF conversion.
* Stage 2: Direct-to-IR polyglot parsing into Flat Linear Arenas.
* Stage 3: Semantic AST normalization and dead statement pruning.
* Stage 4: Semantic graph compilation (Call Graph, CFG, DFG, and F_T Type Contract).
* Stage 5: Canonical binary serialization and multi-tier cryptographic hashing (F0 through F4).
* Stage 6: Radix-directed binary caching (CNTR v5).

## 2. Pipeline Stage Latency across File Scales

Measurements across five file scale tiers:
* Micro: ~25 Lines of Code (2 functions)
* Small: ~85 Lines of Code (10 functions)
* Medium: ~405 Lines of Code (50 functions)
* Large: ~1,605 Lines of Code (200 functions)
* Monolithic: ~4,005 Lines of Code (500 functions)

### Latency by Pipeline Stage

Stage: 1. Ingestion & Fast Scan
* Micro (~25 LOC): 12 us
* Small (~85 LOC): 38 us
* Medium (~405 LOC): 180 us
* Large (~1,605 LOC): 720 us
* Monolithic (~4,005 LOC): 1.85 ms
* Complexity: O(N) linear in byte count

Stage: 2. Direct-to-IR Parsing
* Micro (~25 LOC): 215 us
* Small (~85 LOC): 540 us
* Medium (~405 LOC): 3.80 ms
* Large (~1,605 LOC): 24.2 ms
* Monolithic (~4,005 LOC): 58.1 ms
* Complexity: O(N) linear in token count

Stage: 3. Semantic Normalization
* Micro (~25 LOC): 110 us
* Small (~85 LOC): 210 us
* Medium (~405 LOC): 1.45 ms
* Large (~1,605 LOC): 6.80 ms
* Monolithic (~4,005 LOC): 18.2 ms
* Complexity: O(N) linear in AST node count

Stage: 4. Graph & Type Contract Extraction (F_CG, F_CF, F_DF, F_T)
* Micro (~25 LOC): 45 us
* Small (~85 LOC): 120 us
* Medium (~405 LOC): 950 us
* Large (~1,605 LOC): 4.10 ms
* Monolithic (~4,005 LOC): 11.5 ms
* Complexity: O(V + E) graph complexity

Stage: 5. Canonical Serialization & Cryptographic Hashing
* Micro (~25 LOC): 8 us
* Small (~85 LOC): 18 us
* Medium (~405 LOC): 75 us
* Large (~1,605 LOC): 310 us
* Monolithic (~4,005 LOC): 820 us
* Complexity: O(B) linear in byte length

Total End-to-End 9-Tier Manifest Generation
* Micro (~25 LOC): 390 us
* Small (~85 LOC): 926 us
* Medium (~405 LOC): 6.45 ms
* Large (~1,605 LOC): 36.1 ms
* Monolithic (~4,005 LOC): 90.5 ms
* Overall Complexity: O(N) strict linear scalability

## 3. Polyglot Ingestion Throughput

Single-module ingestion and complete 9-tier fingerprint bundle generation across supported programming languages (~100 LOC per file):

Language: Python 3.8+
* Latency: 980 us
* Throughput: ~102,000 LOC/sec
* AST Representation: Direct-to-IR Flat Arena
* Intermediate Allocations: Zero intermediate CST

Language: TypeScript / JavaScript
* Latency: 420 us
* Throughput: ~238,000 LOC/sec
* AST Representation: Direct-to-IR Flat Arena
* Intermediate Allocations: Zero intermediate CST

Language: Go 1.20+
* Latency: 340 us
* Throughput: ~294,000 LOC/sec
* AST Representation: Direct-to-IR Flat Arena
* Intermediate Allocations: Zero intermediate CST

Language: Rust 2021+
* Latency: 375 us
* Throughput: ~266,000 LOC/sec
* AST Representation: Direct-to-IR Flat Arena
* Intermediate Allocations: Zero intermediate CST

## 4. Local Build Cache Performance (CNTR v5)

Canontra's 4KB paged binary cache (`.canontra/cache.bin`) provides microsecond record lookups and updates:

Operation: Cache Hit Lookup (Hot in Memory)
* Latency: 14 us
* Throughput: ~71,000 lookups/sec
* Method: 256-way radix directory jump + SwissTable hash check

Operation: Cache Verification (CRC32 Check across all Pages)
* Latency: 45 us (per 100 indexed files)
* Throughput: ~2,200,000 records/sec
* Method: IEEE 802.3 CRC32 page verification

Operation: Page Invalidation and Isolated Recovery
* Latency: 38 us
* Throughput: Immediate single-page discard without global invalidation

Operation: Cache Pruning (Deleting Stale Files)
* Latency: 85 us (for 500 repository files)
* Method: Inode and path existence check with linear scan

## 5. Merkle DAG In-Memory Hot Update Latency

When running in file-watcher mode or processing continuous commits in monorepos:

Workspace Size: 50 Files
* Cold Build: 42.1 ms
* Incremental Hot Update (1 file modified): 68 us
* Speedup: 619x faster

Workspace Size: 250 Files
* Cold Build: 198.5 ms
* Incremental Hot Update (1 file modified): 74 us
* Speedup: 2,682x faster

Workspace Size: 1,000 Files
* Cold Build: 812.0 ms
* Incremental Hot Update (1 file modified): 82 us
* Speedup: 9,902x faster

Because Canontra's Merkle DAG updates only the direct ancestors of a modified leaf node, recomputing the entire workspace root hash takes less than 100 microseconds regardless of repository size.

## 6. Memory Footprint and Arena Allocation Efficiency

Comparison of memory consumption for an AST representing 1,000 functions:

Representation: Traditional Heap Pointer Trees
* Memory Allocated: 18.4 MB
* GC Pressure: High (thousands of small objects on heap)
* Cache Locality: Low (pointer chasing across memory)

Representation: Canontra Flat Linear Arenas (Unboxed Vectors)
* Memory Allocated: 2.1 MB (88.6% reduction)
* GC Pressure: Zero (unboxed contiguous buffers)
* Cache Locality: High (contiguous memory traversal)

## 7. Comparative Summary

Compared to raw byte hashing:
* Raw SHA-256 is fast (~1.5 us) but 100% blind to semantics. Any comment or whitespace edit triggers full rebuilds.
* Canontra takes ~390 us for micro-files and ~926 us for typical modules, providing full semantic discrimination across 9 orthogonal tiers and saving minutes to hours of downstream CI compilation.

For comprehensive empirical multi-tool comparative benchmarks (CodeQL, Git, Turborepo, Sccache) and whole-repository graph synthesis across 15 production repositories, see [benchmarkReport.md](benchmarkReport.md).
