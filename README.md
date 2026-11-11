# Canontra

Deterministic Polyglot Program Identity and Semantic Graph Engine

Version: v0.3.0.0 (Release v0.3.0)

Open-source research project by [Jash Thakkar](https://github.com/JashT14) & SymtraceLabs

Repository: <https://github.com/symtrace/canontra>

## What is Canontra?

Imagine you are working on a team project. You open a source code file, add an inline comment to explain a tricky line of code, adjust indentation, or swap the order of two independent helper functions.

To traditional tools like Git, Docker, Bazel, or your Continuous Integration (CI) pipeline, the entire file looks completely different. Traditional tools calculate a raw cryptographic hash (like SHA-256) over the raw text bytes of the file. Because a single added space changes every single character of a raw hash, automated systems are forced to assume that your entire program changed.

As a result, your build system might rebuild large containers, invalidate build caches, and spend 20 to 30 minutes running long test suites, even though the actual behavior of your program did not change at all.

Canontra solves this problem. It looks past surface-level formatting, comments, and file organization to understand the true structural meaning of your code. Written in 100% pure Haskell with zero runtime dependencies and zero network access, Canontra projects source code into an orthogonal hierarchy of deterministic semantic fingerprints:

* Did the raw file text change? (Source Fingerprint, F0)
* Did the executable logic, math, or if-statements change? (Structural Fingerprint, F1)
* Did public function names, parameters, or export signatures change? (Declaration Fingerprint, F2)
* Did imported packages or modules change? (Dependency Fingerprint, F3)
* Did function caller-to-callee relationships change? (Call Graph Fingerprint, F_CG)
* Did if-else branching or loop pathways change? (Control Flow Fingerprint, F_CF)
* Did variable definition and usage chains change? (Data Flow Fingerprint, F_DF)
* Did structural interface types or trait contracts change? (Type Contract Fingerprint, F_T)
* Did the composite program identity change? (Composite Fingerprint, F4)

Canontra works across five major programming languages: Python, JavaScript, TypeScript, Go, and Rust.

## What's New in v0.3.0

The v0.3.0 release deepens Canontra's semantic invariance across local variable renaming, algebraic laws, control-flow branch inversions, generic type contracts, and multi-file PR impact slicing:

* **Scope-Aware Bound Lexical $\alpha$-Canonicalization (`Canontra.Normalize.Alpha`)**: Renaming local variables, loop counters, comprehension variables, lambda arguments, or exception bindings produces **100% bit-identical** $F_1$, $F_{DF}$, and $F_4$ fingerprints while keeping public function names and parameter contracts strictly sensitive in $F_2$.
* **Commutative & Associative Algebraic Normal Form (`Canontra.Normalize.Algebraic`)**: Pure commutative expressions (`a + b` vs `b + a`, `x * y` vs `y * x`, `a == b` vs `b == a`), relational duals (`b > a` vs `a < b`, `b >= a` vs `a <= b`), and unordered set/dict literals (`{1, 2, 3}` vs `{3, 1, 2}`) normalize to identical canonical forms without reordering impure function calls.
* **CFG Dominator-Tree & Reverse Post-Order Canonicalization (`Canontra.Analysis.CFGCanonical`)**: Branch inversions (`if not cond: B else: A` vs `if cond: A else: B`), early-exit guard clauses, and unreachable statements after `return`/`raise` normalize to identical $F_{CF}$ and $F_1$ digests.
* **Deepened Structural Type Contract ($F_T$) Lattice**: Nested union/intersection flattening (`(A | B) | C` == `C | (B | A)`), positional de Bruijn generic type parameter normalization (`interface Store<K, V>` == `interface Store<Key, Val>`), and structural record field sorting across Python, TypeScript, Go, and Rust.
* **CSR-Native Multi-File PR Batch Change Impact Analysis (`canontra impact --pr-base <ref>`)**: Computes joint backward reachability cones over unboxed CSR graphs in under 50 microseconds and emits ready-to-run test commands for `pytest`, `jest`, `go test`, and `cargo test`.
<!-- 
## What's New in v0.2.0

The v0.2.0 release makes Canontra dramatically faster, slashes memory consumption, and expands language support to modern standards:

* **Blazing-Fast Multi-Core Processing**: Canontra now distributes work across all available CPU cores automatically, scanning and analyzing over **110,000 lines of code per second**. Entire repositories analyze in a fraction of a second.
* **70% Less Memory Footprint**: Whole-repository call graphs and data-flow graphs now use ultra-compact arrays instead of heavy memory trees, keeping RAM usage low and execution smooth even on massive projects.
* **Instant Incremental Caching (2.8×-3.7× Faster)**: Re-checking files you haven't touched is practically instantaneous (under 1 microsecond per file). The cache also includes self-healing recovery that protects against unexpected shutdowns or corrupted files.
* **Modern Polyglot Language Support**:
  * **Python 3.12**: Supports nested f-strings with quote reuse, new generic type parameter syntax (`type Alias[T] = ...`), and walrus operator `:=` expressions in comprehensions.
  * **TypeScript 5.2 & JavaScript**: Supports explicit resource management (`using` and `await using`) with automated cleanup tracking.
  * **Go 1.21+**: Supports built-in functions (`min`, `max`, `clear`) and generic interface tilde constraint sets (`~T`).
  * **Rust 2021 Edition**: Supports Generic Associated Types (GATs) and raw identifiers (`r#type`, `r#match`).
* **Rock-Solid Reliability**: Added automatic retry handling on Windows to eliminate file-locking conflicts from antivirus or search indexers, circular directory link detection, and bit-identical results across Windows, macOS, and Linux. -->

## A Concrete Example

Consider this Python file, `original.py`:

```python
# Calculate customer discount for orders
def calculate_discount(price: float, discount: float = 0.1) -> float:
    return price * (1.0 - discount)

def get_service_version():
    return "1.0.0"
```

Now consider `modified.py`, where a developer ran a code formatter, added a docstring, and reordered the two independent functions:

```python
def get_service_version():
    return "1.0.0"

def calculate_discount(  price: float, discount: float = 0.1  ) -> float:
    """Calculates customer discount for orders."""
    return price * (1.0 - discount)
```

If you run standard Git diff or raw SHA-256 on these files, they appear completely different. But when you run Canontra:

```bash
canontra compare original.py modified.py
```

Canontra outputs:

```
================================================================================
  CANONTRA SEMANTIC COMPARISON
================================================================================
  Target A:             original.py
  Target B:             modified.py
................................................................................
  Source Text (F0):     DIFFERENT  (raw formatting and docstrings modified)
  Structural AST (F1):  IDENTICAL  (executable logic is unchanged)
  Declarations (F2):    IDENTICAL  (public API signatures are unchanged)
  Dependencies (F3):    IDENTICAL  (imported packages are unchanged)
  Type Contract (F_T):  IDENTICAL  (interface contracts are unchanged)
  Composite (F4):       IDENTICAL  (overall semantic identity preserved)
================================================================================
  Verdict: SEMANTICALLY IDENTICAL
================================================================================
```

Canontra proves mathematically that while the raw text changed, the actual program logic, public interface, and behavior are 100% identical. Your build system can safely skip recompiling and skip re-running tests.

Now consider `buggy.py`, where someone made an accidental change to the math operator:

```python
def calculate_discount(price: float, discount: float = 0.1) -> float:
    return price - (1.0 - discount)  # Bug: changed * to -
```

When compared against `original.py`:

```bash
canontra compare original.py buggy.py
```

Canontra immediately reports:

```
================================================================================
  CANONTRA SEMANTIC COMPARISON
================================================================================
  Structural AST (F1):  DIFFERENT  (arithmetic operator mutated)
  Declarations (F2):    IDENTICAL  (public signature unchanged)
  Composite (F4):       DIFFERENT
================================================================================
  Verdict: SEMANTICALLY DIVERGENT (Exit Code: 1)
================================================================================
```

Canontra pinpoints that an internal calculation changed while the public function signatures remained untouched.

## Installation

Canontra can be installed directly from **[Hackage](https://hackage.haskell.org/package/canontra)** using `cabal`, or via automated pre-built installer scripts (no Haskell toolchain required).

### Option 1: Install from Hackage (via Cabal)

If you already have Haskell GHC and Cabal installed, you can install Canontra with a single command:

```bash
cabal update
cabal install canontra
```

> **Tip**: Ensure that your Cabal binary directory (usually `~/.cabal/bin` on Linux/macOS or `%APPDATA%\cabal\bin` on Windows) is in your system `PATH`.

### Option 2: Automated Install Scripts (No Haskell Toolchain Required)

If you don't have Haskell installed, use our automated one-line installer scripts. They automatically detect your operating system and CPU architecture, download the native binary, add it to your PATH, and configure shell completions:

#### Linux and macOS (POSIX)

Run the direct installer in your terminal:

```bash
curl -fsSL https://raw.githubusercontent.com/symtrace/canontra/main/install.sh | bash
```

Or run the local script directly:

```bash
./install.sh
```

The script automatically detects your operating system (Linux or macOS) and processor architecture (x86_64 or ARM64/Apple Silicon), installs the standalone binary into `~/.local/bin`, configures your PATH, and sets up shell completions for Bash, Zsh, or Fish. Official release binaries are stripped on Linux and ad-hoc codesigned on macOS.

To simulate the installation without modifying files:

```bash
./install.sh --dry-run
```

#### Windows (PowerShell)

Open PowerShell and run the direct installer:

```powershell
irm https://raw.githubusercontent.com/symtrace/canontra/main/install.ps1 | iex
```

Or run the local script directly:

```powershell
.\install.ps1
```

Official release binaries of `canontra.exe` are Authenticode self-signed with SHA-256 for code integrity verification. The script detects your architecture (AMD64 or ARM64), installs `canontra.exe` into `%LOCALAPPDATA%\Programs\canontra`, updates your User environment PATH permanently without needing administrator privileges, and registers autocompletions in your PowerShell profile.

To simulate the installation on Windows:

```powershell
.\install.ps1 -DryRun
```

### Option 3: Building from Source

You can build Canontra from source using Haskell Stack or Cabal:

Using Stack:

```bash
git clone https://github.com/symtrace/canontra.git
cd canontra
stack build --fast
stack install
```

Using Cabal:

```bash
cabal update
cabal build
cabal install --installdir=$HOME/.local/bin
```

To run the full test suite with strict compiler verification:

```bash
stack test --pedantic
```

## Commands and Usage

Canontra provides a unified, production-ready command line interface.

### 1. Compute Semantic Fingerprints (`canontra fp`)

Generate the full multi-tier fingerprint manifest for any file:

```bash
canontra fp src/main.py
```

Output formatted as JSON for automated pipelines:

```bash
canontra fp src/main.py --json
```

Output only the composite hash for fast scripting:

```bash
canontra fp src/main.py --hash
```

### 2. Standard Input Streaming (`canontra fp -`)

You can pipe source code directly into Canontra using standard input (`-`). Specify the language with `--language` (or `-l`):

```bash
echo "def add(x, y): return x + y" | canontra fp - --language python --hash
```

This makes it easy to integrate Canontra into Git pre-commit hooks, editor linters, and shell scripts without writing temporary files to disk.

### 3. Compare Two Source Files (`canontra compare`)

Compare two versions of a file across all semantic tiers:

```bash
canontra compare v1/service.ts v2/service.ts
```

Canontra returns strict POSIX exit codes:

* Exit code 0: The files are semantically identical.
* Exit code 1: The files have semantic differences.
* Exit code 2: Command line syntax or argument error.
* Exit code 3: Source code parse error.
* Exit code 4: File I/O or security boundary error.

### 4. Structural Diff (`canontra diff`)

Inspect fine-grained AST and declaration differences between two files:

```bash
canontra diff old_auth.py new_auth.py
```

This output separates superficial changes from actual structural modifications, showing exactly which functions were added, removed, or changed.

### 5. Call Graph and Flow Analysis (`canontra graph`)

Generate call graphs, control-flow graphs, or data-flow graphs for a file or directory:

```bash
canontra graph src/ --dot
```

Outputs standard Graphviz DOT format that you can visualize using Graphviz or modern graph viewers.

### 6. Export Machine Interchange Formats (`canontra export`)

Export semantic diffs and impact findings in standard machine formats:

Export in OASIS SARIF v2.1.0 format (supported by GitHub Code Scanning, SonarQube, and CI dashboards):

```bash
canontra export src/service.ts --format sarif -o findings.sarif
```

Export in Graphviz DOT format:

```bash
canontra export src/service.ts --format dot -o callgraph.dot
```

### 7. Manage the Local Build Cache (`canontra cache`)

Canontra includes an ultra-fast local binary cache (`.canontra/cache.bin`) that remembers file fingerprints in compact, cache-aligned records. It delivers sub-microsecond warm lookups (< 500 ns per file) and features isolated 4KB page CRC32 checksums for automatic self-healing against corrupted files:

View cache statistics and hit rates:

```bash
canontra cache info
```

Verify the cryptographic CRC32 integrity of all 4KB cache pages:

```bash
canontra cache verify
```

Remove records for files that have been deleted from disk:

```bash
canontra cache prune
```

Clear the local cache completely:

```bash
canontra cache clean
```

### 8. Shell Autocompletions (`canontra completions`)

Generate native completion scripts for your favorite shell:

```bash
canontra completions bash > ~/.local/share/bash-completion/completions/canontra
canontra completions zsh > ~/.zfunc/_canontra
canontra completions fish > ~/.config/fish/completions/canontra.fish
canontra completions powershell >> $PROFILE
```

## Where is Canontra Used?

Canontra is designed for modern development workflows and infrastructure:

### Smart CI/CD Build Caches

In continuous integration pipelines, running test suites and building packages takes substantial time and cloud compute budget. By replacing raw byte hashes with Canontra semantic fingerprints, CI pipelines can safely skip rebuilding and testing packages when developers only update documentation, comments, formatting, or internal variable names.

### Monorepo Impact Analysis

In large monorepos with thousands of interdependent services, determining what needs to be re-tested after a commit is difficult. Canontra combines call graph analysis and declaration hashes to calculate exact change impact boundaries, ensuring you test only what could actually be affected.

### Meaningful Code Review and PR Triage

Automated pull request bots can run Canontra to tell reviewers immediately: "This pull request changed 400 lines of code across 12 files, but all changes are cosmetic formatting and comment updates. Zero public APIs and zero executable logic pathways were altered."

### Security and Vendor Dependency Auditing

When updating external open-source packages, security teams can use Canontra to verify whether a minor patch release modified executable statements or merely updated license text and comments.

## Documentation Index

Explore the rest of the documentation for full technical details:

* [CHANGELOG.md](CHANGELOG.md): Complete release history, version notes, and PVP conformance.
* [benchmarkReport.md](benchmarkReport.md): Empirical benchmark report, multi-tool comparative evaluation, and whole-repository graph synthesis evaluation across 15 production repositories.
* [BENCHMARKS.md](BENCHMARKS.md): Performance benchmarks, latency measurements, and throughput statistics across supported languages.
* [REAL_WORLD_BENCHMARKS.md](REAL_WORLD_BENCHMARKS.md): Empirical reproduction runbook and evaluation methodology across real-world open-source repositories.
* [technicalSpecs.md](technicalSpecs.md): Comprehensive technical architecture, compiler pipeline flow, 9-tier identity math, flat linear arenas, unboxed CSR graphs, and cache specifications.
* [CONTRIBUTING.md](CONTRIBUTING.md): Guide for contributors, development environment setup, code conventions, and test verification standards.
* [SECURITY.md](SECURITY.md): Security policy, air-gapped isolation guarantees, path traversal sandboxing, and vulnerability reporting.

## License

Canontra is open-source software licensed under the Apache License, Version 2.0. See the [LICENSE](LICENSE) file for details.
