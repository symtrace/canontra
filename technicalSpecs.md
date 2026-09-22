# Canontra Technical Specifications

System Architecture, Compiler Pipeline, and Identity Engine
Version: v0.1.0
Author: Jash Thakkar & SymtraceLabs Engineering Team
Status: Production Specification

## 1. Architectural Philosophy and Design Principles

Canontra is a deterministic program identity and semantic graph compiler written in 100% pure Haskell. It transforms polyglot source code into an orthogonal vector of cryptographic hashes that distinguish superficial text edits from functional, structural, and interface mutations.

The engine is engineered around four core systems principles:

1. Platform Invariance: The engine produces bit-identical digests for identical logical programs regardless of the host operating system (Linux, macOS, Windows), CPU architecture (x86_64, AArch64, RISC-V), or filesystem semantics (NTFS, APFS, ext4).
2. Zero Runtime Dependencies: The core library contains zero C-FFI bindings, zero dynamic library dependencies (such as libtree-sitter), and zero external binaries.
3. Air-Gapped Zero-Trust Operation: The runtime opens zero network sockets, initiates zero HTTP/RPC connections, and transmits zero telemetry.
4. Bounded Latency and Memory Safety: Single-pass parsing, Flat Linear Arena vector representations, unboxed arrays, and 4KB paged caching guarantee bounded sub-millisecond execution for single-file operations.

## 2. The 9-Tier Identity Hierarchy

Canontra rejects the notion of a single monolithic program hash. It decomposes source code into an orthogonal hierarchy of cryptographic digests:

Tier F0 (Source Text Digest)
Calculates SHA-256 over raw source bytes. Captures any edit, including whitespace changes, comment modifications, and line endings.

Tier F1 (Structural AST Digest)
Calculates SHA-256 over the normalized Abstract Syntax Tree. Normalization eliminates comments, unflagged docstrings, formatting differences, and alpha-renames internal local variables. Independent pure functions are sorted canonically. F1 is invariant under semantics-preserving code cleanup and refactoring.

Tier F2 (Declaration Signature Digest)
Calculates SHA-256 over the public API surface of the module. Includes exported functions, classes, interfaces, parameter names, type annotations, and default value hashes. Internal function bodies and private helpers are excluded.

Tier F3 (Dependency Digest)
Calculates SHA-256 over the external import topology. Captures imported modules, packages, and alias mappings in canonically sorted order.

Tier F_CG (Call Graph Digest)
Calculates SHA-256 over the intra-module function dispatch graph. Encodes caller-to-callee edges, recursive call structures, and invocation frequencies.

Tier F_CF (Control Flow Graph Digest)
Calculates SHA-256 over basic block transition topologies. Encodes branching conditions, loop headers, break/continue targets, and exception pathways.

Tier F_DF (Data Flow Graph Digest)
Calculates SHA-256 over Static Single Assignment (SSA) Def-Use chains. Identifies reaching definitions, variable assignments, and expression consumer relationships.

Tier F_T (Structural Type Contract Digest)
Calculates SHA-256 over public structural types, trait definitions, and interface shapes. Method declarations and struct fields are canonically sorted, guaranteeing that permuting method orders produces an identical F_T digest.

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

## 3. End-to-End Compiler Pipeline Flow

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
Phase 3: Semantic AST Normalization
  - Comment and docstring stripping
  - Dead statement elimination (StmtPass)
  - Pure function canonical permutation sorting
  - Local variable alpha-renaming
        |
        v
Phase 4: Semantic Graph Extraction
  - Intra-module Call Graph compilation
  - Basic Block Control-Flow Graph (CFG) construction
  - SSA Reaching-Definition Data-Flow Graph (DFG) compilation
  - Structural Type Contract (F_T) derivation
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
Phase 7: Radix-Directed Binary Caching (CNTR v5)
  - 4KB paged slab storage
  - Page-level IEEE 802.3 CRC32 integrity verification
  - Atomic swap via temporary file rename
        |
        v
Phase 8: Machine Interchange & Diagnostics
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

### Phase 3: Semantic Normalization

The raw AST undergoes semantics-preserving algebraic rewrite passes:

1. Trivia Stripping: Comments, trailing whitespace, blank lines, and unflagged docstrings are purged.
2. Canonical Pure Function Sorting: Independent top-level functions whose bodies have no cyclic caller-callee dependencies are sorted lexicographically by normalized structural signature. Permuting the source order of two independent helper functions results in a bit-identical AST.
3. Dead Statement Elimination: Meaningless pass-through statements (such as Python `pass`) are stripped from statement blocks containing other executable operations.
4. Alpha-Renaming: Internal local variable identifiers within private function bodies are normalized into de Bruijn-style synthetic symbols, ensuring that local variable renames do not mutate structural hashes.

### Phase 4: Graph and Type Contract Extraction

From the normalized AST, the compiler extracts three orthogonal graphs:

* Call Graph: Identifies function invocations, recursive loops, and dispatch hierarchies.
* CFG (Control Flow Graph): Partitions the function into maximal basic blocks connected by conditional, unconditional, and exceptional edges. Loops are identified via Tarjan strongly connected component analysis.
* DFG (Data Flow Graph): Computes definition-use pairs for every variable across basic blocks using forward dataflow analysis.

Simultaneously, the compiler derives structural type contracts (F_T):

* Interface methods and struct fields are canonicalized into sorted order.
* Primitive types, arrays, optionals, and composite records are mapped to normalized type algebra.
* Structural subtyping evaluation determines whether a changed module satisfies backwards compatibility requirements.

### Phase 5: Canonical Binary Serialization

All IR structures are serialized into canonical byte streams:

* All integers and lengths use strict Big-Endian byte order.
* Floating-point numbers are encoded according to IEEE 754 with canonical sign-bit handling (-0.0 normalized to +0.0) and quiet NaN canonicalization.
* Every AST constructor begins with a unique 1-byte tag, preventing parsing ambiguities.

### Phase 6: Multi-Tier Cryptographic Hashing

The serialized canonical byte streams are hashed using NIST-standard SHA-256 via optimized primitives in `cryptohash-sha256`. The resulting 256-bit hashes are packaged into the `FingerprintBundle` structure.

### Phase 7: Radix-Directed Binary Caching (CNTR v5)

Fingerprint manifests and Merkle DAG states are saved to a binary cache file (`.canontra/cache.bin`). The CNTR v5 file layout uses 4KB paged slabs:

* Magic Header (16 bytes): `CNTR\x05` magic identifier and cache version.
* 256-Way Radix Directory (1,024 bytes): High-byte directory mapping path hashes to slab page offsets.
* 4KB Paged Slabs: Each 4,096-byte page contains an IEEE 802.3 CRC32 checksum, record count, and serialized records.
* Isolated Page Recovery: If a single 4KB page suffers byte corruption, only that page is invalidated and re-evaluated. The rest of the cache remains valid.
* Atomic Write Swapping: Cache updates are written to a temporary sibling file (`.canontra/cache.bin.tmp.<pid>`) and swapped atomically using OS kernel rename operations, preventing torn writes upon sudden process termination.

### Phase 8: Machine Interchange and Reporting

The final phase exposes findings through standard formats:

* OASIS SARIF v2.1.0 JSON format for GitHub Code Scanning, SonarQube, and CI dashboards.
* Graphviz DOT format for visualizing call graphs, CFGs, and DFGs.
* Strict POSIX exit codes (0 for identical, 1 for changed, 2 for CLI error, 3 for parse error, 4 for I/O or security violation).

## 4. Air-Gapped Zero-Trust Security Model

Canontra is designed to execute safely inside untrusted source repositories and high-security air-gapped enclaves.

### Path Sandboxing and Root Containment

All file discovery and path queries pass through `canonicalizeSafePath`:

* Null Byte Defense: Paths containing embedded null bytes (`\0`) are immediately rejected.
* Canonical Containment: The candidate file path is canonicalized to resolve symlinks and `..` traversal components. The resulting path is verified to reside strictly within the project root directory prefix.
* Symlink Cycle Breaking: Traversal tracks `(DeviceID, FileID)` 64-bit tuples in a visited set. Recursive symlink loops and junctions are detected and severed before stack exhaustion can occur.

### Hard Resource Ceilings

To defend against zip-bombs, cyclic filesystem attacks, and memory exhaustion:

* File Size Ceiling: Files larger than 50 MB (52,428,800 bytes) are safely skipped and reported.
* Directory Recursion Ceiling: Directory nesting depths greater than 64 levels are rejected.
* Unchecked Recursion Limits: All graph traversal algorithms (dominator tree computation, cycle detection) enforce finite recursion bounds.

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

### Rice's Theorem Defensibility

Rice's Theorem establishes that non-trivial semantic properties of general computing programs are undecidable. Canontra does not claim to solve general semantic equivalence. Instead, Canontra guarantees exact equivalence under an explicit, finite set of canonical normalization rules. If two programs produce identical F4 hashes, they are proven to possess identical canonical representations under those explicit rules.

## 6. Supported Language Grammars

Canontra currently parses and analyzes five languages:

Language: Python
File Extensions: .py, .pyi
Coverage: Functions, async functions, classes, decorators, docstrings, type annotations, imports, list/dict comprehensions, control flow.

Language: JavaScript
File Extensions: .js, .mjs, .cjs, .jsx
Coverage: Functions, arrow functions, ES6 classes, commonjs/ESM imports, destructuring, control flow.

Language: TypeScript
File Extensions: .ts, .tsx, .d.ts
Coverage: All JavaScript features plus interfaces, type aliases, union types, generic constraints, enum declarations.

Language: Go
File Extensions: .go
Coverage: Package statements, functions, methods with receivers, structs, interfaces, goroutines, select/switch blocks, imports.

Language: Rust
File Extensions: .rs
Coverage: Functions, structs, enums, traits, impl blocks, match expressions, let bindings, use declarations.
