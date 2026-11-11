# Security Policy and Architecture

Version: v0.3.0.0 (Release v0.3.0)
Target: Canontra Production Release
Organization: SymtraceLabs Security Team

## Security Model and Guarantees

Canontra is engineered to execute safely on untrusted source repositories, automated continuous integration runners, and high-security air-gapped enclaves. The engine provides mathematical guarantees of confidentiality, deterministic integrity, and memory safety.

### 1. 100% Offline and Air-Gapped Operation

* Zero Network Sockets: Canontra contains no networking libraries, HTTP/RPC clients, socket listeners, or remote connections of any kind.
* Zero Telemetry or Analytics: No code snippets, file paths, developer identifiers, or usage telemetry are ever recorded, collected, or transmitted outside the local machine.
* Self-Contained Execution: Canontra runs with 100% functionality in completely isolated, offline environments where internet access is prohibited.

### 2. Path Sandboxing, Directory Containment & Loop Detection

When scanning repositories or comparing files, Canontra actively defends against directory traversal attacks and malicious filesystem structures:

* Root Containment: All target paths are canonicalized and verified to reside strictly within the project root directory prefix. Relative traversal sequences such as `../../etc/passwd` or windows drive jumps are safely detected and rejected with exit code 4.
* Symlink & Junction Cycle Breaking: Canontra tracks composite `FileNodeIdentity` (`fniVolumeID`, `fniFileID`) tuples during filesystem traversal (`Canontra.Security.Path`). Recursive symlink loops, cross-volume mounted junctions, and circular directory graphs are severed before recursive stack exhaustion can occur.
* Case-Folding Determinism: Paths are collated deterministically across case-sensitive and case-insensitive filesystems, guaranteeing identical Merkle root hashes on Linux ext4 and Windows NTFS.
* Null Byte Invariant: Paths containing embedded null bytes (`\0`) are immediately rejected before passing to OS filesystem APIs.

### 3. Hard Resource Ceilings

To prevent denial-of-service, zip-bombs, and memory exhaustion attacks:

* Maximum File Size: Canontra enforces a strict 50 MB (52,428,800 bytes) file size ceiling. Any individual file exceeding this limit is skipped and reported.
* Maximum Recursion Depth: Directory trees nested deeper than 64 levels are rejected to protect process call stacks.
* Bounded Graph Traversal: Dominator tree computations and data-flow reachability passes enforce finite iteration bounds, guaranteeing termination on arbitrary control flow graphs.
* Bounded CSR Batch Reachability: Multi-source reachability BFS queues allocate memory strictly bounded by the total number of repository symbols, preventing runaway allocations on cyclic or adversarial graph structures.

### 4. Memory Safety and Binary Slab Cache Security (CNTR v6)

* Pure Haskell Runtime: Built on GHC 9.6.6 with pure functional semantics. The core library strictly avoids unmanaged pointer manipulation.
* Flat Linear Arena & Unboxed CSR Protection: Unboxed vector representations prevent heap-allocated pointer corruption and enforce strict array boundary checking.
* 4KB Paged Radix Slab Cache Security (`CNTR\x06`):
  * Every 4,096-byte slab page in `.canontra/cache.bin` is protected by an IEEE 802.3 CRC-32C checksum.
  * Corrupted cache pages are discarded and recomputed in isolation (`salvageSlabCacheFile`) without crashing the engine.
  * Cache writes are staged to a private temporary file and finalized using `atomicSwapWithRetry` with exponential backoff and jitter, preventing Windows Defender / SearchIndexer lock failures and torn writes during sudden termination.

### 5. Safe Git and CI Change Impact Analysis Integration

When querying revision history, branch snapshots, or evaluating PR batch change impact:

* Controlled Command Invocation: Git commands (`git diff`, `git ls-tree`, `git show`) are invoked via `System.Process.readProcessWithExitCode` with explicit argument arrays.
* No Shell Execution: Arguments are passed directly to OS process spawning routines without invoking intermediate shells (`sh`, `bash`, `cmd.exe`, or `powershell.exe`), eliminating shell injection vectors.
* Path Sanitization: All file paths reported by `git diff --name-only` are validated via `canonicalizeSafePath` before being processed by the CSR reachability engine.
* Read-Only Access: Canontra only executes read-only inspection commands. It never modifies, commits, or alters repository git state.

## Reporting a Security Vulnerability

If you discover a potential security vulnerability, path traversal bypass, or cryptographic discrepancy in Canontra, please report it responsibly:

1. Reporting Channel:
   * GitHub Private Vulnerability Reporting: Submit a private advisory at <https://github.com/symtrace/canontra/security/advisories/new>
   * GitHub Issues: Alternatively, open an issue on GitHub at <https://github.com/symtrace/canontra/issues>
2. Title Prefix: Please use the title or subject line: "[SECURITY] Canontra Vulnerability Report"
3. Details to Include:
   * A description of the issue and potential impact.
   * Minimal reproduction steps, sample source code, or command line arguments.
   * The version of Canontra and host operating system.
4. Response Timeline: We will acknowledge receipt of your report within 48 hours and provide a timeline for triage and resolution.
5. Coordinated Disclosure: For sensitive vulnerabilities, we ask that you use GitHub security advisories prior to public disclosure until an official patch and advisory have been released.
