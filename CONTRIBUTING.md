# Contributing to Canontra

Thank you for your interest in contributing to Canontra.

Canontra is an open-source systems research project building deterministic, polyglot program identity and semantic graph compilation in 100% pure Haskell. We welcome contributions from developers of all backgrounds, whether you are fixing a typo, adding support for a new language construct, improving microbenchmark performance, or writing metamorphic test cases.

## Development Setup

### Prerequisites

To build and test Canontra locally, you need:

1. Haskell GHC 9.6.6
2. Haskell Stack (recommended, using LTS 22.28) or Cabal (>= 3.0)
3. Git

### Building the Project

Clone the repository and build the library and executables:

```bash
git clone https://github.com/symtrace/canontra.git
cd canontra
stack build --fast
```

To run the Canontra command line tool directly from your development build:

```bash
stack exec canontra -- --help
```

### Running the Test Suite

We maintain a strict zero-warning and zero-failure policy across our entire test suite. Always run tests with the pedantic flag before submitting code:

```bash
stack test --pedantic
```

All 470+ automated tests should pass cleanly without any compiler warnings or test failures.

## Core Architectural Constraints

To preserve Canontra's production guarantees, all contributions must adhere to these foundational constraints:

1. 100% Pure Haskell:
   Never introduce external C-FFI bindings, C++ libraries, or runtime dynamic library dependencies (such as libtree-sitter). All parsers, serializers, and graph engines must be written in pure, type-safe Haskell.

2. Air-Gapped Zero-Trust Security:
   Never import networking libraries, HTTP clients, socket abstractions, or telemetry modules. Canontra must remain 100% functional in strictly isolated, air-gapped server environments.

3. Cross-Platform Determinism:
   Ensure all binary serialization is strictly Big-Endian. Never rely on host CPU endianness or host filesystem path separators. Always normalize paths to forward slashes.

4. High-Performance Memory Hygiene:
   Where possible, avoid allocating deeply nested pointer-heavy tree structures on the garbage-collected heap. Use Flat Linear Arenas and unboxed Vectors for AST representations, and use SwissTables for symbol interning.

5. Strict Compiler Flags:
   The codebase compiles under `-Wall -Werror -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wmissing-export-lists -Wpartial-fields -Wredundant-constraints`. Unused imports, missing export lists, or non-exhaustive pattern matches will fail the build.

## How to Add a New Transformation or Parser Feature

When extending Canontra's polyglot parsers or normalization rules:

1. Identify the Language Module:
   Parsers reside under `src/Canontra/Parser/` (e.g., `Python.hs`, `JS.hs`, `Go.hs`, `Rust.hs`). Normalization rules reside under `src/Canontra/Normalize/`.

2. Preserve Structural Invariance:
   If your transformation is semantics-preserving (such as stripping a new kind of formatting or comment), ensure that it maps to an invariant F1 AST representation.

3. Add Metamorphic Verification Tests:
   Add both a soundness test (verifying that the transformation preserves F1, F2, F_T, and F4) and a sensitivity test (verifying that mutating the logic changes F1 and F4) in `test/Canontra/MetamorphicSpec.hs`.

4. Update Documentation:
   Update `technicalSpecs.md` and `README.md` if your change introduces new flags or language features.

## Submitting Pull Requests

Follow this workflow to submit your contribution:

1. Fork the repository on GitHub and create a feature branch:
   `git checkout -b feature/my-new-improvement`

2. Make your changes and commit with clear, descriptive commit messages:
   `git commit -m "Add TypeScript union type sorting in F_T type contract"`

3. Verify syntax and tests:
   `stack test --pedantic`
   `bash -n install.sh`
   `powershell -NoProfile -Command "Get-Command .\install.ps1 -Syntax"`

4. Push your branch to GitHub and open a Pull Request against `main`.

5. In your Pull Request description, explain what changed, why the change is necessary, and summarize the test results.

## Code of Conduct

We are committed to providing a friendly, safe, and welcoming environment for everyone. Please be respectful, constructive, and collaborative in all discussions, issues, and pull requests.
