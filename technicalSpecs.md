# Canontra Technical Specifications

System Architecture, Compiler Pipeline, and Identity Engine
Version: v0.3.0.0 (Release v0.3.0)
Author: Jash Thakkar & SymtraceLabs Engineering Team
Status: Production Specification

## 1. Architectural Philosophy and Design Principles

Canontra is a deterministic program identity and semantic graph compiler written in 100% pure Haskell. It transforms polyglot source code into an orthogonal vector of cryptographic hashes that distinguish superficial text edits from functional, structural, and interface mutations.

The engine is engineered around four core systems principles:

1. Platform Invariance: The engine produces bit-identical digests for identical logical programs regardless of the host operating system (Linux, macOS, Windows), CPU architecture (x86_64, AArch64, RISC-V), or filesystem semantics (NTFS, APFS, ext4).
2. Zero Runtime Dependencies: The core library contains zero C-FFI bindings, zero dynamic library dependencies (such as libtree-sitter), and zero external binaries.
3. Air-Gapped Zero-Trust Operation: The runtime opens zero network sockets, initiates zero HTTP/RPC connections, and transmits zero telemetry.
4. Bounded Latency and Memory Safety: Single-pass parsing, Flat Linear Arena vector representations, unboxed CSR graph arrays, and 4KB paged caching guarantee bounded sub-millisecond execution for single-file operations.

## 2. The 9-Tier Identity Hierarchy

Canontra decomposes source code into an orthogonal hierarchy of cryptographic digests:

Tier F0 (Source Text Digest)
Calculates SHA-256 over raw source bytes. Captures any edit, including whitespace changes, comment modifications, and line endings.

Tier F1 (Structural AST Digest)
Calculates SHA-256 over the normalized Abstract Syntax Tree. Normalization eliminates comments, unflagged docstrings, formatting differences, and alpha-renames internal local variables to de Bruijn index form. Commutative pure expressions, dual relational operators, and independent pure functions are sorted canonically. F1 is invariant under semantics-preserving code cleanup, refactoring, and local renaming.

Tier F2 (Declaration Signature Digest)
Calculates SHA-256 over the public API surface of the module. Includes exported functions, classes, interfaces, parameter names, type annotations, and default value hashes. Internal function bodies and private helpers are excluded.

Tier F3 (Dependency Digest)
Calculates SHA-256 over the external import topology. Captures imported modules, packages, and alias mappings in canonically sorted order.

Tier F_CG (Call Graph Digest)
Calculates SHA-256 over the intra-module function dispatch graph. Encodes caller-to-callee edges, recursive call structures, and invocation frequencies.

Tier F_CF (Control Flow Graph Digest)
Calculates SHA-256 over canonical basic block transition topologies. Employs dominator-tree canonicalization, branch inversion normal form, guard-clause lifting, and Reverse Post-Order (RPO) relabeling.

Tier F_DF (Data Flow Graph Digest)
Calculates SHA-256 over Static Single Assignment (SSA) Def-Use chains. Identifies reaching definitions, variable assignments, and expression consumer relationships.

Tier F_T (Structural Type Contract Digest)
Calculates SHA-256 over public structural types, trait definitions, and interface shapes. Evaluates structural subtyping, recursive ANF type flattening, generic type parameter normalization, and canonical record field ordering.

Tier F4 (Composite Program Digest)
Calculates SHA-256 over the composite tuple:
SHA-256(F1 || F2 || F3 || F_CG || F_CF || F_DF || F_T)
Represents the comprehensive semantic identity of the module.

Whole-Repository Tiers
For multi-module workspaces, Canontra computes whole-repository projections:

* F_WCG: Whole-repository inter-module call graph digest.
* F_WDF: Cross-module inter-procedural data flow digest.
* F_R: Repository topology and module dependency DAG digest.
* F_W4: Root Merkle digest over the entire repository.

## 3. The Five Pillars of Semantic Canonicalization (v0.3.0 Engine)

Version 0.3.0 introduces five foundational canonicalization and analysis pillars that close semantic divergence gaps across all supported languages:

### Pillar 1: Complete Alpha-Equivalence Normalization (Canontra.Normalize.Alpha)

Local variable naming in private function scopes is an implementation detail. Canontra canonicalizes all bound local variable references into a deterministic, scope-aware de Bruijn coordinate: `@v<depth>.<ordinal>`.

* Scope Hierarchy: Each nested lexical block (loops, conditionals, closures, comprehensions) increments scope depth.
* Ordinal Allocation: Local variable declarations within a scope are assigned ascending ordinal indices based on their textual declaration order.
* Parameter and Shadow Isolation: Parameter bindings and shadowed variables are resolved deterministically against their nearest enclosing lexical scope.
* Invariance Guarantee: Renaming any local variable across its definition and use sites produces a bit-identical AST and identical F1 digest.

### Pillar 2: Algebraic Normal Form and Operator Simplification (Canontra.Normalize.Algebraic)

Syntactic variations in expression phrasing frequently mask identical underlying computation. Canontra applies purity-guarded algebraic rewrites:

* Commutative and Associative Sorting: Pure operands of commutative operators (`+`, `*`, `&&`, `||`, `==`, `!=`, `&`, `|`, `^`) are sorted lexicographically by normalized operand representation.
* Relational Dual Inversion: Greater-than relational expressions are canonicalized to less-than relational forms by transposing operands: `b > a` normalizes to `a < b`, and `b >= a` normalizes to `a <= b`.
* Double-Negation Elimination: Redundant boolean inversions are stripped: `not (not x)` simplifies to `x`.
* Set and Dict Literal Ordering: Unordered collections containing pure elements or keys are sorted into canonical ascending sequence.

### Pillar 3: Control-Flow Graph Canonicalization (Canontra.Analysis.CFGCanonical)

Syntactically inverted conditional branches and guard clauses often generate disparate basic block layouts. Canontra canonicalizes the CFG topology:

* Dominator-Tree Canonicalization: Basic blocks are relabeled according to their depth and traversal index within the dominator tree.
* Reverse Post-Order (RPO) Indexing: Block IDs are reassigned based on a canonical RPO traversal from the function entry node.
* Branch Inversion Normal Form: Branches testing inverted conditions with empty alternate suites (`if (!cond) A` versus `if (cond) ; else A`) are mapped to a uniform conditional structure.
* Guard-Clause Lifting: Guard clauses featuring early returns (`if (!valid) return; doWork()`) are aligned with equivalent nested conditional topologies.

### Pillar 4: Deepened Structural Type Contract Lattice (Canontra.Analysis.TypeContract)

Structural contracts determine whether interface changes break downstream consumers. Canontra formalizes type analysis into a structural subtyping lattice:

* Recursive ANF Type Flattening: Nested composite types, intersection types, and union types are flattened into canonical A-Normal Form.
* Structural Subtyping Evaluation: Determines width subtyping (excess fields permitted), depth subtyping (covariant returns, contravariant parameters), and union inclusion ($A \subseteq A \cup B$).
* Generic Type Parameter Normalization: Generic parameters are normalized to ordinal alpha variables (`[T, U]` maps to `[α0, α1]`) across Python PEP 695, TypeScript generics, Go interfaces, and Rust traits.
* Tuple Bound and Tilde Constraint Normalization: Structural constraints like Go `~int | ~float64` or Python `(int, float)` bounds are sorted and deduplicated.

### Pillar 5: CSR-Native PR Batch Impact Analysis (Canontra.Analysis.BatchImpact, Canontra.Analysis.Impact)

For continuous integration workflows, Canontra provides fast, multi-file change impact analysis directly over unboxed Compressed Sparse Row (CSR) call graphs:

* Batch Reachability Cones: Given a set of modified files in a Pull Request, Canontra maps changed symbols to CSR node indices and executes a multi-source bitset BFS reachability traversal.
* PR Base Diffing: The `canontra impact --pr-base <branch>` command inspects `git diff --name-only` against the PR base branch, runs incremental analysis, and outputs the affected downstream symbol blast radius.
* Machine-Readable Manifests: Emits structured JSON impact manifests detailing direct changes, transitively impacted symbols, and impacted test suites.

## 4. End-to-End Compiler Pipeline Flow

The compilation and fingerprinting pipeline executes in eight sequential phases:

```
[ Raw Source Bytes ]
        |
        v
Phase 1: Ingestion & Fast Scanning
  - Fast UTF-8 validation
  - SIMD / SWAR CRLF conversion (\r\n -> \n)
  - Unicode NFC normalization
        |
        v
Phase 2: Polyglot Parsing (Direct-to-IR)
  - Recursive descent parsing (Python, JS/TS, Go, Rust)
  - Flat Linear Arena vector allocation
  - SwissTable symbol interning
        |
        v
Phase 3: Semantic AST & Algebraic Normalization
  - Comment and docstring stripping
  - Dead statement elimination (StmtPass)
  - Bound local variable de Bruijn renaming (@v<d>.<o>)
  - Algebraic Normal Form & relational dual inversion
  - Pure function canonical permutation sorting
        |
        v
Phase 4: Semantic Graph Extraction & Canonicalization
  - Intra-module Call Graph compilation
  - Dominator-tree canonicalized CFG construction (RPO)
  - SSA Reaching-Definition Data-Flow Graph (DFG)
  - Structural Type Contract (F_T) lattice derivation
        |
        v
Phase 5: Canonical Binary Serialization
  - Length-prefixed big-endian encoding
  - 1-byte constructor discriminant tags
  - IEEE 754 canonical floating-point bitmasking
        |
        v
Phase 6: Multi-Tier Cryptographic Hashing
  - Compute F0, F1, F2, F3, F_CG, F_CF, F_DF, F_T, F4
  - Construct 9-tier FingerprintBundle
        |
        v
Phase 7: Radix-Directed Binary Caching (CNTR v6)
  - 4KB paged slab storage with IEEE 802.3 CRC-32C checksums
  - 256-way L1 radix jump table for < 500 ns warm lookups
  - Unboxed CSR graph persistence
  - Resilient atomic file swapping with retry
        |
        v
Phase 8: Machine Interchange, Impact Analysis & Diagnostics
  - PR Batch Change Impact Analysis (CSR reachability)
  - OASIS SARIF v2.1.0 diagnostics generation
  - Graphviz DOT call graph export
  - Shell autocompletions and POSIX exit code reporting
```

### Phase 1: Ingestion and Fast Scanning

Incoming source bytes from a file or standard input stream are processed via SIMD and SWAR (SIMD Within A Register) operations. Carriage returns (\r\n) are normalized to UNIX newlines (\n) in 64-bit word chunks. Text is verified as valid UTF-8 and normalized into Unicode Canonical Composition (NFC), ensuring that composed and decomposed Unicode representations produce bit-identical byte streams.

### Phase 2: Polyglot Parsing and Arena Allocation

Canontra avoids the overhead of deep pointer-chasing tree structures by compiling ASTs directly into Flat Linear Arenas. An AST arena stores nodes in four parallel unboxed vectors:

* astTags: Vector of 8-bit constructor discriminants (Function, Loop, If, Assign, Return).
* astFirstChild: Vector of 32-bit indices pointing to the first child node.
* astNextSibling: Vector of 32-bit indices pointing to the next sibling node.
* astPayloads: Vector of 64-bit payloads (symbol indices, literal offsets, span identifiers).

Identified variable and function names are interned into open-addressing SwissTables with 8-bit control bytes (h2 metadata). This provides O(1) symbol resolution with cache-line locality and zero pointer fragmentation.

### Phase 3: Semantic Normalization and Algebraic Simplification

The raw AST undergoes semantics-preserving algebraic rewrite passes:

1. Trivia Stripping: Comments, trailing whitespace, blank lines, and unflagged docstrings are purged.
2. Canonical Pure Function Sorting: Independent top-level functions whose bodies have no cyclic caller-callee dependencies are sorted lexicographically by normalized structural signature. Permuting the source order of two independent helper functions results in a bit-identical AST.
3. Dead Statement Elimination: Meaningless pass-through statements (such as Python `pass`) are stripped from statement blocks containing other executable operations.
4. Alpha-Renaming: Internal local variable identifiers within private function bodies are normalized into de Bruijn-style synthetic symbols (`@v<depth>.<ordinal>`).
5. Commutative and Relational Normalization: Commutative operators are ordered canonically, relational operators are converted to less-than duals, and double-negations are eliminated.

### Phase 4: Graph, Unboxed CSR Representation, and Type Contract Extraction

From the normalized AST, the compiler extracts three orthogonal graphs:

* Call Graph: Identifies function invocations, recursive loops, and dispatch hierarchies.
* CFG (Control Flow Graph): Partitions the function into maximal basic blocks connected by conditional, unconditional, and exceptional edges, canonicalized via dominator-tree and RPO relabeling.
* DFG (Data Flow Graph): Computes definition-use pairs for every variable across basic blocks using forward dataflow analysis.

In v0.3.0, repository-wide call graphs and data-flow graphs are compiled directly into an Unboxed Compressed Sparse Row (CSR) Engine (`Canontra.Analysis.CSRGraph`):

* Contiguous unboxed `Vector Word32` row offsets and column indices (`csrRowOffsets`, `csrColIndices`) with bit-packed `Vector Word16` edge attributes (`csrEdgeFlags`).
* $O(\log(\text{deg}(u)))$ binary search edge verification (`csrHasEdge`) executing in 26 ns.
* Linear-time Tarjan Strongly Connected Component (SCC) cycle collapse executing in 14.5 microseconds and topological condensation DAG synthesis in 39.7 microseconds.
* Localized reachability cone edge splicing (`spliceCSREdges`) allowing sub-10ms incremental updates without rebuilding whole-repository graph matrices.
* Multi-source batch reachability queries (`computeBatchImpactSliceCSR`) executing in < 15 ms across 100,000+ LOC repositories.

Simultaneously, the compiler derives structural type contracts (F_T):

* Interface methods and struct fields are canonicalized into sorted order.
* Primitive types, arrays, optionals, and composite records are mapped to normalized type algebra.
* Universal generic parameter normalization converts type variable names to positional indices.
* Structural subtyping evaluation determines whether a changed module satisfies backwards compatibility requirements.

### Phase 5: Canonical Binary Serialization

All IR structures are serialized into canonical byte streams:

* All integers and lengths use strict Big-Endian byte order.
* Floating-point numbers are encoded according to IEEE 754 with canonical sign-bit handling (-0.0 normalized to +0.0) and quiet NaN canonicalization.
* Every AST constructor begins with a unique 1-byte tag, preventing parsing ambiguities.

### Phase 6: Multi-Tier Cryptographic Hashing

The serialized canonical byte streams are hashed using NIST-standard SHA-256 via optimized primitives in `cryptohash-sha256`. The resulting 256-bit hashes are packaged into the `FingerprintBundle` structure.

### Phase 7: Radix-Directed Binary Slab Caching (CNTR v6)

Fingerprint manifests and Merkle DAG states are saved to a binary cache file (`.canontra/cache.bin`). The CNTR v6 (`CNTR\x06`) layout uses cache-line-aligned slab pages:

* Magic Header (16 bytes): `CNTR\x06` magic identifier and cache version.
* 256-Way L1 Radix Jump Table (`0x0020 - 0x081F`, 2,048 bytes): Provides 1-cycle CPU fast-path indexing for warm file lookups.
* 64-Byte Cache Records: Fixed-width `CacheRecordV6` aligned to CPU cache lines, containing 64-bit device/file IDs, nanosecond modification timestamps, and all orthogonal cryptographic digests.
* Zero-Copy Memory-Mapped Retrieval: `lookupSlabBinaryBS` and `lookupSlabCacheWarm` execute in sub-microsecond latency (< 500 ns per file).
* Whole-Repository Binary CSR Graph Persistence: Saves compacted CSR call graphs and data-flow matrices (`saveRepoGraphsSlab`) with zero textual formatting overhead.
* Isolated 4KB Page Recovery: Every 4KB page maintains an independent IEEE 802.3 CRC-32C checksum (`salvageSlabCacheFile`). Damaged pages are isolated while valid entries remain instantly accessible.
* Atomic Write Swapping with Windows Resiliency: Staged cache writes are finalized via atomic rename with exponential backoff and jitter (`atomicSwapWithRetry`), eliminating Windows Defender and SearchIndexer file-locking conflicts.

### Phase 8: Machine Interchange, Impact Analysis and Reporting

The final phase exposes findings through standard formats:

* Pull Request Change Impact Analysis: Calculates blast radius, affected symbols, and downstream tests via `canontra impact --pr-base <branch>`.
* OASIS SARIF v2.1.0 JSON format for GitHub Code Scanning, SonarQube, and CI dashboards.
* Graphviz DOT format for visualizing call graphs, CFGs, and DFGs.
* Strict POSIX exit codes (0 for identical, 1 for changed, 2 for CLI error, 3 for parse error, 4 for I/O or security violation).

## 5. Formal Verification and Determinism Theorems

Canontra's correctness is validated by a metamorphic testing corpus implementing formal algebraic theorems:

### Soundness Invariance Theorem

For any program P and any transformation T in the set of semantics-preserving transforms T_sound (such as whitespace reformatting, comment injection, dead statement removal, and independent function reordering):
F1(P) == F1(T(P))
F2(P) == F2(T(P))
F_T(P) == F_T(T(P))
F4(P) == F4(T(P))

### Sensitivity Divergence Theorem

For any program P and any semantic mutation M in M_divergent (such as operator inversion, literal modification, branch inversion, or parameter addition):
F1(P) != F1(M(P))
F4(P) != F4(M(P))

### Alpha-Equivalence Invariance Theorem

For any expressions e1 and e2:
e1 =_alpha e2 implies F1(canonicalizeLocals(e1)) == F1(canonicalizeLocals(e2))

### Algebraic Normal Form Invariance Theorem

For any expressions e1 and e2 differing solely in pure commutative operand ordering or relational dual representations (`a + b` vs `b + a`, `x > y` vs `y < x`, `not (not z)` vs `z`):
F1(algebraicNormalize(e1)) == F1(algebraicNormalize(e2))

### CFG Isomorphism Invariance Theorem

For any function implementations f1 and f2 whose control flow graphs differ solely in empty alternate branches or guard-clause early returns while preserving identical execution pathways:
F_CF(canonicalizeCFG(f1)) == F_CF(canonicalizeCFG(f2))

### Type Contract Lattice Monotonicity Theorem

For any module M and its modified version M', if M' extends M with purely backwards-compatible additions (width subtyping, covariant return specialization) without altering existing signatures:
F2(M) != F2(M') but M' is recognized as a valid structural subtype of M under the F_T subtyping lattice.

### CSR Batch Reachability Soundness Theorem

For any set of modified symbols S in a whole-repository call graph G = (V, E), the unboxed CSR batch impact slice computed by `computeBatchImpactSliceCSR(G, S)` contains all vertices v in V reachable from S via directed dispatch paths, guaranteeing zero false negatives in downstream test selection.

### Rice's Theorem Defensibility

Rice's Theorem establishes that non-trivial semantic properties of general computing programs are undecidable. Canontra does not claim to solve general semantic equivalence. Instead, Canontra guarantees exact equivalence under an explicit, finite set of canonical normalization rules. If two programs produce identical F4 hashes, they are proven to possess identical canonical representations under those explicit rules.

## 6. Supported Language Grammars

Canontra currently parses and analyzes five languages:

Language: Python (3.12 Conformance)
File Extensions: .py, .pyi
Coverage: Functions, async functions, classes, decorators, docstrings, type annotations, imports, list/dict/set/generator comprehensions, control flow, PEP 701 nested f-strings with quote reuse and comments, PEP 695 generic type parameters (`type Alias[T]`, `def f[T]()`, `class C[T]`), and PEP 572 walrus operator scope hoisting into enclosing functions and reaching definitions.

Language: JavaScript & TypeScript (TS 5.2 Conformance)
File Extensions: .js, .mjs, .cjs, .jsx, .ts, .tsx, .d.ts
Coverage: Functions, arrow functions, ES6 classes, commonjs/ESM imports, destructuring, control flow, interfaces, type aliases, union types, generic constraints, enum declarations, TypeScript 5.2 explicit resource management (`using` and `await using`) with CFG synthetic disposal blocks, and two-token lookahead regex vs division operator disambiguation.

Language: Go (Go 1.21+ Conformance)
File Extensions: .go
Coverage: Package statements, functions, methods with receivers, structs, interfaces, goroutines, select/switch blocks, imports, Go 1.21+ builtins (`min`, `max`, `clear`), generic tilde constraint sets (`~T`) with commutative union normalization, and structural cyclic struct recursion breaker emitting `TypeRecVar 0`.

Language: Rust (Rust 2021 Edition Conformance)
File Extensions: .rs
Coverage: Functions, structs, enums, traits, impl blocks, match expressions, let bindings, use declarations, Generic Associated Types (GATs) lifetime normalization (`'a` -> `'0`), trait associated types, and raw identifier syntax interning (`r#type`, `r#match` interned to bit-identical `SymbolId`).
