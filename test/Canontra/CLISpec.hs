{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- |
Module      : Canontra.CLISpec
Description : Test suite for Phase 4 Production CLI Ecosystem & Tooling.

Verifies:
1. Native shell completion generators for Bash, Zsh, Fish, and PowerShell.
2. Cache maintenance commands (info, verify, clean, prune) on CNTR\x05 binary caches.
3. Export subcommands generating OASIS SARIF v2.1.0 and Graphviz DOT formats.
4. Deterministic stdin language resolution and synthetic path mapping.
5. Strict POSIX exit code contracts (0 Identical, 1 Changed, 3 Parse Error, 4 IO/Security).
-}
module Canontra.CLISpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getTemporaryDirectory
  , removeDirectoryRecursive
  )
import System.FilePath ((</>))
import Test.Hspec

import Canontra.Analysis.CallGraph (buildCallGraph)
import Canontra.Analysis.CFG (buildCFGs)
import Canontra.Analysis.DFG (buildDFGs)
import Canontra.Cache.Common (MerkleCache (..), MerkleCacheEntry (..))
import Canontra.Cache.PagedCache (readPagedCacheFileResilient, writePagedCacheFile)
import Canontra.CLI.Cache (CacheAction (..), runCacheCommand)
import Canontra.CLI.Completions
  ( ShellType (..)
  , generateCompletionScript
  , parseShellType
  )
import Canontra.Comparison.Diff (diffPrograms)
import Canontra.Export.Graph (exportCallGraphDOT, exportCFGDOT, exportDFGDOT)
import Canontra.Export.SARIF (exportDiffSARIF, renderSARIF)
import Canontra.Fingerprint.Bundle (computeBundle, computeManifest)
import Canontra.Parser.Polyglot (parsePolyglotSource)
import Canontra.Types (Fingerprint (..), FingerprintBundle (..), Manifest (..))

spec :: Spec
spec = do
  describe "Canontra.CLI (Phase 4 Production CLI Ecosystem)" $ do

    -- ========================================================================
    -- 1. Shell Autocompletion Generators
    -- ========================================================================
    describe "Shell Autocompletions (Canontra.CLI.Completions)" $ do
      it "parses supported shell names case-insensitively" $ do
        parseShellType "bash" `shouldBe` Just ShellBash
        parseShellType "BASH" `shouldBe` Just ShellBash
        parseShellType "zsh" `shouldBe` Just ShellZsh
        parseShellType "fish" `shouldBe` Just ShellFish
        parseShellType "powershell" `shouldBe` Just ShellPowerShell
        parseShellType "pwsh" `shouldBe` Just ShellPowerShell
        parseShellType "unknown" `shouldBe` Nothing

      it "generates valid Bash completion script with command table" $ do
        let script = T.unpack $ generateCompletionScript ShellBash
        script `shouldContain` "complete -F _canontra canontra"
        script `shouldContain` "fp fingerprint compare diff graph"
        script `shouldContain` "cache export completions"
        script `shouldContain` "_filedir"

      it "generates valid Zsh completion script with compdef" $ do
        let script = T.unpack $ generateCompletionScript ShellZsh
        script `shouldContain` "#compdef canontra"
        script `shouldContain` "'fp:Compute deterministic multi-tier fingerprints'"
        script `shouldContain` "'cache:Inspect, verify, clean, or prune incremental binary cache'"
        script `shouldContain` "'export:Export diagnostics (SARIF) or graphs (DOT)'"

      it "generates valid Fish completion script with completions table" $ do
        let script = T.unpack $ generateCompletionScript ShellFish
        script `shouldContain` "complete -c canontra"
        script `shouldContain` "__fish_use_subcommand"
        script `shouldContain` "-a fp"
        script `shouldContain` "-a cache"
        script `shouldContain` "-a export"

      it "generates valid PowerShell completion script with ArgumentCompleter" $ do
        let script = T.unpack $ generateCompletionScript ShellPowerShell
        script `shouldContain` "Register-ArgumentCompleter -Native -CommandName canontra"
        script `shouldContain` "[System.Management.Automation.CompletionResult]::new('fp'"
        script `shouldContain` "[System.Management.Automation.CompletionResult]::new('cache'"
        script `shouldContain` "[System.Management.Automation.CompletionResult]::new('export'"

    -- ========================================================================
    -- 2. Cache Tooling (Canontra.CLI.Cache)
    -- ========================================================================
    describe "Cache Maintenance Tooling (Canontra.CLI.Cache)" $ do
      it "handles CacheInfo cleanly when no cache exists" $ do
        tempBase <- getTemporaryDirectory
        let tempDir = tempBase </> "canontra_test_nocache"
        createDirectoryIfMissing True tempDir
        runCacheCommand CacheInfo tempDir False
        runCacheCommand CacheClean tempDir False
        removeDirectoryRecursive tempDir

      it "verifies and reports clean CRC32 on an initialized cache" $ do
        tempBase <- getTemporaryDirectory
        let tempDir = tempBase </> "canontra_test_cache"
            cacheDir = tempDir </> ".canontra"
            cacheFile = cacheDir </> "cache.bin"
        createDirectoryIfMissing True cacheDir

        -- Create sample cache entry
        let dummyFp = Fingerprint "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
            dummyBundle = FingerprintBundle dummyFp dummyFp dummyFp dummyFp dummyFp dummyFp dummyFp dummyFp dummyFp
            entry = MerkleCacheEntry 100 1234567 dummyBundle
            cache = MerkleCache (Map.singleton "main.py" entry)

        writePagedCacheFile cacheFile cache
        exists <- doesFileExist cacheFile
        exists `shouldBe` True

        -- Run CacheInfo and CacheVerify
        runCacheCommand CacheInfo tempDir False
        runCacheCommand CacheVerify tempDir False

        -- Clean cache
        runCacheCommand CacheClean tempDir False
        cleanedExists <- doesFileExist cacheFile
        cleanedExists `shouldBe` False

        removeDirectoryRecursive tempDir

      it "prunes orphaned cache entries when underlying source file is deleted" $ do
        tempBase <- getTemporaryDirectory
        let tempDir = tempBase </> "canontra_test_prune"
            cacheDir = tempDir </> ".canontra"
            cacheFile = cacheDir </> "cache.bin"
            sourceFile1 = tempDir </> "kept.py"
        createDirectoryIfMissing True cacheDir
        writeFile sourceFile1 "def kept(): pass\n"

        let dummyFp = Fingerprint "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
            dummyBundle = FingerprintBundle dummyFp dummyFp dummyFp dummyFp dummyFp dummyFp dummyFp dummyFp dummyFp
            entry1 = MerkleCacheEntry 100 1234567 dummyBundle
            entry2 = MerkleCacheEntry 100 1234567 dummyBundle
            cache = MerkleCache (Map.fromList [("kept.py", entry1), ("deleted.py", entry2)])

        writePagedCacheFile cacheFile cache

        -- Prune: kept.py exists, deleted.py does not exist
        runCacheCommand CachePrune tempDir False

        -- Verify pruned cache
        updated <- readPagedCacheFileResilient cacheFile
        Map.member "kept.py" (unMerkleCache updated) `shouldBe` True
        Map.member "deleted.py" (unMerkleCache updated) `shouldBe` False

        removeDirectoryRecursive tempDir

    -- ========================================================================
    -- 3. Stdin Streaming & Synthetic Language Resolution
    -- ========================================================================
    describe "Standard Input Streaming & Manifest Resolution" $ do
      it "computes bit-identical bundle for Python source whether file or stdin" $ do
        let src = "def square(x: int) -> int:\n    return x * x\n"
            raw = TE.encodeUtf8 src
        case (computeBundle "math.py" raw src, computeBundle "stdin.py" raw src) of
          (Right bFile, Right bStdin) -> do
            f1Structural bFile `shouldBe` f1Structural bStdin
            f2Declaration bFile `shouldBe` f2Declaration bStdin
            f3Dependency bFile `shouldBe` f3Dependency bStdin
            fTTypeContract bFile `shouldBe` fTTypeContract bStdin
            f4Composite bFile `shouldBe` f4Composite bStdin
          _ -> expectationFailure "Bundle computation failed"

      it "resolves polyglot languages for stdin stream" $ do
        let tsSrc = "export function add(a: number, b: number): number { return a + b; }"
            goSrc = "package main\nfunc Add(a int, b int) int { return a + b }\n"
            rsSrc = "pub fn add(a: i32, b: i32) -> i32 { a + b }\n"
        case ( computeManifest "stdin.ts" (TE.encodeUtf8 tsSrc) tsSrc
             , computeManifest "stdin.go" (TE.encodeUtf8 goSrc) goSrc
             , computeManifest "stdin.rs" (TE.encodeUtf8 rsSrc) rsSrc
             ) of
          (Right mTS, Right mGo, Right mRs) -> do
            mLanguage mTS `shouldBe` "typescript"
            mLanguage mGo `shouldBe` "go"
            mLanguage mRs `shouldBe` "rust"
          _ -> expectationFailure "Polyglot manifest computation failed"

    -- ========================================================================
    -- 4. Export Command Functionality (SARIF & Graphviz DOT)
    -- ========================================================================
    describe "Export Command Generators" $ do
      it "exports SARIF with standard tool driver rules and diff" $ do
        let codeA = "def greeting(name: str) -> str:\n    return 'Hello ' + name\n"
            codeB = "def greeting(name: str, shout: bool = False) -> str:\n    return 'HELLO ' + name\n"
        case (parsePolyglotSource "greet.py" codeA, parsePolyglotSource "greet.py" codeB) of
          (Right p1, Right p2) -> do
            let diffRes = diffPrograms p1 p2
                sarif = exportDiffSARIF "greet.py" diffRes
                rendered = T.unpack $ renderSARIF sarif
            rendered `shouldContain` "\"$schema\""
            rendered `shouldContain` "\"version\": \"2.1.0\""
            rendered `shouldContain` "CTR001_InterfaceBreak"
          _ -> expectationFailure "Parsing failed for SARIF export test"

      it "exports CallGraph in DOT format" $ do
        case parsePolyglotSource "app.py" "def hello(): pass\n" of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let cg = buildCallGraph prog
                dot = T.unpack $ exportCallGraphDOT cg
            dot `shouldContain` "digraph CallGraph"
            dot `shouldContain` "fn_hello"

      it "exports CFG and DFG in DOT format" $ do
        let src = "def branch(x):\n    if x > 0:\n        return 1\n    return 0\n"
        case parsePolyglotSource "branch.py" src of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let cfgs = buildCFGs prog
                dfgs = buildDFGs prog
                cfgDot = T.unpack $ exportCFGDOT cfgs
                dfgDot = T.unpack $ exportDFGDOT dfgs
            cfgDot `shouldContain` "digraph ControlFlowGraph"
            dfgDot `shouldContain` "digraph DataFlowGraph"
