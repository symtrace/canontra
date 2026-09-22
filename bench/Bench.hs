{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : Research benchmark suite measuring latency, polyglot throughput, Direct-to-IR parsers, SymbolTable interning, and Outline mode (v0.0.5-alpha).

This suite empirically evaluates canontra v0.0.5-alpha across 9 core Research Questions (RQs):
- RQ1: Execution Latency & Algorithmic Scalability across Pipeline Stages & Program Scales
- RQ2: Polyglot Ingestion & Pipeline Throughput (Python, TS/JS, Go, Rust)
- RQ3: Mutation Invariance Processing (Whitespace, Comments, Body Edits)
- RQ4: Multi-File Polyglot Repository Aggregation Scaling (FR Tier up to 1,000 files)
- RQ5: Comparative Baseline Overhead & Throughput Limits
- RQ6: High-Performance Optimizations (StreamingHash, CompactGraph, Fused Normalizer, MerkleCache)
- RQ7: v0.0.5-alpha FlatFusion Direct-to-IR Parser Throughput & Sub-ms Verification
- RQ8: SymbolTable Interning & Zero-Allocation Symbol Resolution
- RQ9: Selective Outline Mode Speedup for F2 (Declarations) & F3 (Dependencies)
-}
module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.IO.Encoding (setFileSystemEncoding, setForeignEncoding, setLocaleEncoding, utf8)
import System.IO (hSetEncoding, stderr, stdout)
import Test.Tasty.Bench

import Canontra.Analysis.CallGraph (buildCallGraph)
import Canontra.Analysis.CFG (buildCFGs)
import Canontra.Analysis.CompactGraph (fromControlFlowGraph, fromDataFlowGraph)
import Canontra.Analysis.DFG (buildDFGs)
import Canontra.Analysis.Scope (analyzeProgramScope)
import Canontra.Cache.Inode (FileMetadata (..))
import Canontra.Cache.MerkleCache (MerkleCache (..), decodeBinaryCache, emptyCache, encodeBinaryCache, insertCache, lookupBinaryCache, lookupCache)
import Canontra.Canonical.FastScan (fastCanonicalizeBS, scanAsciiAndLineEndings)
import Canontra.Canonical.FusedStream (fusedHashDeclarations, fusedHashProgram)
import Canontra.Canonical.Serialize (canonicalizeProgram)
import Canontra.Canonical.StreamingHash (hashBuilderDirect)
import Canontra.Canonical.Unicode (canonicalizeText)
import Canontra.Fingerprint.Bundle (computeBundle, computeBundleFromSource)
import Canontra.Fingerprint.CallGraph (computeFCG)
import Canontra.Fingerprint.Composite (computeF4)
import Canontra.Fingerprint.ControlFlow (computeFCF)
import Canontra.Fingerprint.DataFlow (computeFDF)
import Canontra.Fingerprint.Declaration (computeF2, extractDeclarations)
import Canontra.Fingerprint.Dependency (computeF3)
import Canontra.Fingerprint.Source (computeF0, hashBytes)
import Canontra.Fingerprint.Structural (computeF1)
import Canontra.IR.Arena (fusedHashLinearAST, programToLinearAST)
import Canontra.Normalize.Fused (fusedNormalizeProgram)
import Canontra.Normalize.Normalize (normalizeProgram)
import Canontra.Parser.FastPython (parseFastPythonByteString, parseFastPythonToArena)
import Canontra.Parser.Go (parseGoSource)
import Canontra.Parser.Ingest (ingestOutlineSource, ingestSource)
import Canontra.Parser.JS (parseJSSource)
import Canontra.Parser.Outline (computeF2Outline, computeF3Outline, parseOutlineGo, parseOutlineJS, parseOutlinePython, parseOutlineRust)
import Canontra.Parser.Python (parsePythonSource)
import Canontra.Parser.Rust (parseRustSource)
import Canontra.Parser.SwissTable (emptySwissTable, swissInternBS, swissLookupBS, swissResolveId)
import Canontra.Parser.SymbolTable (SymbolId (..), emptySymbolTable, internManyBS, internSymbolBS, preloadPolyglotKeywords, resolveSymbolBS)
import Canontra.Repository.MerkleDAG (buildMerkleDAG, diffMerkleDAG, merkleDAGRootHash)
import Canontra.Repository.Parallel (parMapChunks)
import Canontra.Repository.Repository (computeRepositoryFingerprint)
import Canontra.Analysis.Impact (classifySeverity, computeImpactSlice)
import Canontra.Analysis.TypeContract (extractTypeContracts)
import Canontra.Analysis.WholeRepoGraph (buildWholeRepoCallGraph, buildWholeRepoDataFlow)
import Canontra.Cache.PagedCache (decodeBinaryCacheV5, encodeBinaryCacheV5, lookupBinaryCacheV5)
import Canontra.Fingerprint.TypeContract (computeFT)
import Canontra.Fingerprint.WholeRepoCallGraph (computeFWCG)
import Canontra.Fingerprint.WholeRepoDataFlow (computeFWDF)
import Canontra.IR.Expression (Op (..))
import Canontra.Types (FileEntry (..), Fingerprint (..), FingerprintBundle (..))
import Canontra.Verification.Metamorphic (MetamorphicMutation (..), MetamorphicTransform (..), runMetamorphicSuite, verifyMetamorphicProgramTransform, verifyProgramMutation)

-- | Generates synthetic Python module with n function definitions
generateSyntheticPython :: Int -> T.Text
generateSyntheticPython n =
  T.unlines $
    [ "import math"
    , "import os"
    , "from collections import defaultdict"
    , ""
    ] ++ concatMap (\i ->
      [ "def compute_metric_" <> T.pack (show i) <> "(alpha: float, beta: float = 1.0, verbose: bool = False) -> float:"
      , "    \"\"\"Docstring for function " <> T.pack (show i) <> " to test stripping.\"\"\""
      , "    scale = alpha * " <> T.pack (show (i + 1))
      , "    accumulator = 0.0"
      , "    if scale > 50.0:"
      , "        accumulator += (scale * 2.5) + beta"
      , "    else:"
      , "        accumulator -= (scale * 0.5) - beta"
      , "    return accumulator"
      , ""
      ]) [1 .. n]

-- | Generates synthetic TypeScript module with n function definitions
generateSyntheticTS :: Int -> T.Text
generateSyntheticTS n =
  T.unlines $
    [ "import { calculate } from './calc';"
    , ""
    ] ++ concatMap (\i ->
      [ "export function computeMetric" <> T.pack (show i) <> "(alpha: number, beta: number = 1.0): number {"
      , "    const scale = alpha * " <> T.pack (show (i + 1)) <> ";"
      , "    return scale + beta;"
      , "}"
      , ""
      ]) [1 .. n]

-- | Generates synthetic Go module with n function definitions
generateSyntheticGo :: Int -> T.Text
generateSyntheticGo n =
  T.unlines $
    [ "package mathops"
    , "import \"fmt\""
    , ""
    ] ++ concatMap (\i ->
      [ "func ComputeMetric" <> T.pack (show i) <> "(alpha float64, beta float64) float64 {"
      , "    scale := alpha * " <> T.pack (show (i + 1))
      , "    return scale + beta"
      , "}"
      , ""
      ]) [1 .. n]

-- | Generates synthetic Rust module with n function definitions
generateSyntheticRust :: Int -> T.Text
generateSyntheticRust n =
  T.unlines $
    [ "use std::collections::HashMap;"
    , ""
    ] ++ concatMap (\i ->
      [ "pub fn compute_metric_" <> T.pack (show i) <> "(alpha: f64, beta: f64) -> f64 {"
      , "    let scale = alpha * " <> T.pack (show (i + 1)) <> ".0;"
      , "    scale + beta"
      , "}"
      , ""
      ]) [1 .. n]

main :: IO ()
main = do
  setLocaleEncoding utf8
  setForeignEncoding utf8
  setFileSystemEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  -- Pre-generate benchmark inputs of multiple scales
  let microSrc = generateSyntheticPython 2     -- ~25 LOC
      smallSrc = generateSyntheticPython 10    -- ~85 LOC
      medSrc   = generateSyntheticPython 50    -- ~405 LOC
      largeSrc = generateSyntheticPython 200   -- ~1,605 LOC
      xlSrc    = generateSyntheticPython 500   -- ~4,005 LOC

      microBytes = TE.encodeUtf8 microSrc
      smallBytes = TE.encodeUtf8 smallSrc
      medBytes   = TE.encodeUtf8 medSrc
      largeBytes = TE.encodeUtf8 largeSrc
      xlBytes    = TE.encodeUtf8 xlSrc

      tsSmall    = generateSyntheticTS 10      -- ~85 LOC
      tsMed      = generateSyntheticTS 50      -- ~405 LOC
      tsLarge    = generateSyntheticTS 200     -- ~1,605 LOC
      tsSmallBytes = TE.encodeUtf8 tsSmall

      goSmall    = generateSyntheticGo 10      -- ~85 LOC
      goMed      = generateSyntheticGo 50      -- ~405 LOC
      goLarge    = generateSyntheticGo 200     -- ~1,605 LOC
      goSmallBytes = TE.encodeUtf8 goSmall

      rsSmall    = generateSyntheticRust 10    -- ~85 LOC
      rsMed      = generateSyntheticRust 50    -- ~405 LOC
      rsLarge    = generateSyntheticRust 200   -- ~1,605 LOC
      rsSmallBytes = TE.encodeUtf8 rsSmall

      -- Mutation Fixtures
      baseCode = T.unlines
        [ "def calculate(x: int, y: int = 10) -> int:"
        , "    \"\"\"Docstring to strip.\"\"\""
        , "    result = x + y"
        , "    return result"
        ]
      mutWhitespace = T.unlines
        [ "def calculate(  x : int , y : int = 10 )  ->  int :"
        , ""
        , "    \"\"\"Docstring to strip.\"\"\""
        , "    result  =   x  +  y"
        , "    return   result"
        ]
      mutComments = T.unlines
        [ "# Leading comment"
        , "def calculate(x: int, y: int = 10) -> int:"
        , "    # Inner comment"
        , "    \"\"\"Docstring to strip.\"\"\""
        , "    result = x + y  # trailing comment"
        , "    return result"
        ]
      mutBody = T.unlines
        [ "def calculate(x: int, y: int = 10) -> int:"
        , "    \"\"\"Docstring to strip.\"\"\""
        , "    result = (x * 2) + y"
        , "    return result"
        ]

      -- 1000 sample identifier ByteStrings for SymbolTable benchmark
      sampleIdentifiers = [ "var_identifier_" <> BS.pack (map (fromIntegral . fromEnum) (show (i :: Int))) | i <- [1..1000] ]
      (!_, !benchSymTable) = internManyBS sampleIdentifiers emptySymbolTable

  case ( parsePythonSource "micro.py" microSrc
       , parsePythonSource "small.py" smallSrc
       , parsePythonSource "med.py" medSrc
       , parsePythonSource "large.py" largeSrc
       , parsePythonSource "xl.py" xlSrc
       ) of
    (Right microProg, Right smallProg, Right medProg, Right largeProg, Right xlProg) -> do
      -- Mock repository entries for RQ4
      let makeRepoEntries :: Int -> [FileEntry]
          makeRepoEntries n =
            [ FileEntry ("src/module_" ++ show i ++ ".py")
                (FingerprintBundle
                  (Fingerprint $ T.pack $ "s" ++ show i)
                  (Fingerprint $ T.pack $ "str" ++ show i)
                  (Fingerprint $ T.pack $ "d" ++ show i)
                  (Fingerprint $ T.pack $ "dp" ++ show i)
                  (Fingerprint $ T.pack $ "cg" ++ show i)
                  (Fingerprint $ T.pack $ "cf" ++ show i)
                  (Fingerprint $ T.pack $ "df" ++ show i)
                  (Fingerprint $ T.pack $ "c" ++ show i))
            | i <- [1 .. n]
            ]
          repo10   = makeRepoEntries 10
          repo50   = makeRepoEntries 50
          repo200  = makeRepoEntries 200
          repo500  = makeRepoEntries 500
          repo1000 = makeRepoEntries 1000

          sampleMeta = FileMetadata "src/module_1.py" 1024 1700000000
          sampleBundle = head repo10
          populatedCache = insertCache "src/module_1.py" sampleMeta (feFingerprints sampleBundle) emptyCache

          largeCache = foldl' (\c (FileEntry p b) -> insertCache p (FileMetadata p 1024 1700000000) b c) emptyCache repo1000
          largeCacheBin = encodeBinaryCache largeCache
          largeCacheJSON = LBS.toStrict (Aeson.encode largeCache)

          repoEntries1000 = [(p, b) | FileEntry p b <- repo1000]
          dag1000 = buildMerkleDAG repoEntries1000
          dag1000Mod = buildMerkleDAG (("src/module_500.py", FingerprintBundle (Fingerprint "m") (Fingerprint "m") (Fingerprint "m") (Fingerprint "m") (Fingerprint "m") (Fingerprint "m") (Fingerprint "m") (Fingerprint "m")) : tail repoEntries1000)
          benchSwissTable = snd $ foldl' (\(_, tbl) bs -> swissInternBS tbl bs) (SymbolId 0, emptySwissTable 1024) sampleIdentifiers
          xlArena = programToLinearAST xlProg

          largeCacheV5Bin = encodeBinaryCacheV5 largeCache
          modules10 = [("src/module_" ++ show i ++ ".py", if i == 1 then microProg else smallProg) | i <- [1..10 :: Int]]
          wcg10 = buildWholeRepoCallGraph modules10
          polyglotFixtures = [("small.py", smallSrc), ("small.ts", tsSmall), ("small.go", goSmall)]

      defaultMain
        [ bgroup "RQ1: Pipeline Latency across Scales"
            [ bgroup "1. AST Parsing"
                [ bench "Micro (~25 LOC)"    $ whnf (parsePythonSource "micro.py") microSrc
                , bench "Small (~85 LOC)"    $ whnf (parsePythonSource "small.py") smallSrc
                , bench "Medium (~405 LOC)"  $ whnf (parsePythonSource "med.py") medSrc
                , bench "Large (~1605 LOC)"  $ whnf (parsePythonSource "large.py") largeSrc
                , bench "XL (~4005 LOC)"     $ whnf (parsePythonSource "xl.py") xlSrc
                ]
            , bgroup "2. AST Normalization & Serialization"
                [ bench "Micro (~25 LOC)"    $ whnf (canonicalizeProgram . normalizeProgram) microProg
                , bench "Small (~85 LOC)"    $ whnf (canonicalizeProgram . normalizeProgram) smallProg
                , bench "Medium (~405 LOC)"  $ whnf (canonicalizeProgram . normalizeProgram) medProg
                , bench "Large (~1605 LOC)"  $ whnf (canonicalizeProgram . normalizeProgram) largeProg
                , bench "XL (~4005 LOC)"     $ whnf (canonicalizeProgram . normalizeProgram) xlProg
                ]
            , bgroup "3. Semantic Graph Analysis"
                [ bench "Scope Analysis (XL)"        $ whnf analyzeProgramScope xlProg
                , bench "Call Graph Extraction (XL)"   $ whnf buildCallGraph xlProg
                , bench "Control-Flow Graph (CFG) (XL)"$ whnf buildCFGs xlProg
                , bench "Data-Flow Graph (DFG) (XL)"   $ whnf buildDFGs xlProg
                ]
            , bgroup "4. Cryptographic Hashing"
                [ bench "F0 Raw Bytes Hash (XL)"     $ whnf computeF0 xlBytes
                , bench "F1 Structural Hash (XL)"    $ whnf computeF1 xlProg
                , bench "F2 Declaration Hash (XL)"   $ whnf computeF2 xlProg
                , bench "F3 Dependency Hash (XL)"    $ whnf computeF3 xlProg
                , bench "F_CG Call Graph Hash (XL)"  $ whnf computeFCG xlProg
                , bench "F_CF Control-Flow Hash (XL)"$ whnf computeFCF xlProg
                , bench "F_DF Data-Flow Hash (XL)"   $ whnf computeFDF xlProg
                , bench "F4 Composite Hash"          $ whnf (\fp -> computeF4 fp fp fp fp fp fp fp) (Fingerprint "test")
                ]
            , bgroup "5. Full 8-Tier Pipeline"
                [ bench "Micro (~25 LOC)"    $ whnf (\s -> computeBundle "micro.py" microBytes s) microSrc
                , bench "Small (~85 LOC)"    $ whnf (\s -> computeBundle "small.py" smallBytes s) smallSrc
                , bench "Medium (~405 LOC)"  $ whnf (\s -> computeBundle "med.py" medBytes s) medSrc
                , bench "Large (~1605 LOC)"  $ whnf (\s -> computeBundle "large.py" largeBytes s) largeSrc
                , bench "XL (~4005 LOC)"     $ whnf (\s -> computeBundle "xl.py" xlBytes s) xlSrc
                ]
            ]
        , bgroup "RQ2: Polyglot Ingestion Pipelines"
            [ bench "Python Pipeline (~100 LOC)"     $ whnf (\s -> computeBundle "sample.py" smallBytes s) smallSrc
            , bench "TypeScript Pipeline (~100 LOC)" $ whnf (\s -> computeBundle "sample.ts" tsSmallBytes s) tsSmall
            , bench "Go Pipeline (~100 LOC)"         $ whnf (\s -> computeBundle "sample.go" goSmallBytes s) goSmall
            , bench "Rust Pipeline (~100 LOC)"       $ whnf (\s -> computeBundle "sample.rs" rsSmallBytes s) rsSmall
            ]
        , bgroup "RQ3: Mutation Invariance Processing"
            [ bench "Base Code Evaluation"         $ whnf (computeBundleFromSource "base.py") baseCode
            , bench "M1: Whitespace Jitter"        $ whnf (computeBundleFromSource "m1.py") mutWhitespace
            , bench "M2: Comment Churn"            $ whnf (computeBundleFromSource "m2.py") mutComments
            , bench "M3: Internal Body Edit"       $ whnf (computeBundleFromSource "m3.py") mutBody
            ]
        , bgroup "RQ4: Repository Aggregation Scaling (FR)"
            [ bench "10 Files"     $ whnf computeRepositoryFingerprint repo10
            , bench "50 Files"     $ whnf computeRepositoryFingerprint repo50
            , bench "200 Files"    $ whnf computeRepositoryFingerprint repo200
            , bench "500 Files"    $ whnf computeRepositoryFingerprint repo500
            , bench "1,000 Files"  $ whnf computeRepositoryFingerprint repo1000
            ]
        , bgroup "RQ5: Comparative Baseline Overhead"
            [ bench "Raw SHA-256 Hashing (XL)"   $ whnf computeF0 xlBytes
            , bench "AST Parsing Only (XL)"       $ whnf (parsePythonSource "xl.py") xlSrc
            , bench "Canontra Full 8-Tier (XL)"   $ whnf (\s -> computeBundle "xl.py" xlBytes s) xlSrc
            ]
        , bgroup "RQ6: High-Performance Optimizations (v0.0.4-alpha)"
            [ bench "StreamingHash (XL Builder)"  $ whnf hashBuilderDirect (BB.byteString xlBytes)
            , bench "Standard hashBytes (XL)"     $ whnf hashBytes xlBytes
            , bench "Fused Normalization (XL)"    $ whnf fusedNormalizeProgram xlProg
            , bench "Standard Normalization (XL)" $ whnf normalizeProgram xlProg
            , bench "CompactCFG Conversion (XL)"  $ whnf (map fromControlFlowGraph . buildCFGs) xlProg
            , bench "CompactDFG Conversion (XL)"  $ whnf (map fromDataFlowGraph . buildDFGs) xlProg
            , bench "MerkleCache Lookup (Hit)"    $ whnf (lookupCache "src/module_1.py" sampleMeta) populatedCache
            , bench "Zero-Copy Ingestion (XL)"    $ whnf (\b -> ingestSource "xl.py" b xlSrc) xlBytes
            ]
        , bgroup "RQ7: v0.0.5-alpha FlatFusion Direct-to-IR Parser Throughput"
            [ bgroup "Python Parser"
                [ bench "Small (~85 LOC)"   $ whnf (parsePythonSource "small.py") smallSrc
                , bench "Med (~405 LOC)"    $ whnf (parsePythonSource "med.py") medSrc
                , bench "Large (~1605 LOC)" $ whnf (parsePythonSource "large.py") largeSrc
                ]
            , bgroup "TypeScript Parser"
                [ bench "Small (~85 LOC)"   $ whnf (parseJSSource "small.ts") tsSmall
                , bench "Med (~405 LOC)"    $ whnf (parseJSSource "med.ts") tsMed
                , bench "Large (~1605 LOC)" $ whnf (parseJSSource "large.ts") tsLarge
                ]
            , bgroup "Go Parser"
                [ bench "Small (~85 LOC)"   $ whnf (parseGoSource "small.go") goSmall
                , bench "Med (~405 LOC)"    $ whnf (parseGoSource "med.go") goMed
                , bench "Large (~1605 LOC)" $ whnf (parseGoSource "large.go") goLarge
                ]
            , bgroup "Rust Parser"
                [ bench "Small (~85 LOC)"   $ whnf (parseRustSource "small.rs") rsSmall
                , bench "Med (~405 LOC)"    $ whnf (parseRustSource "med.rs") rsMed
                , bench "Large (~1605 LOC)" $ whnf (parseRustSource "large.rs") rsLarge
                ]
            ]
        , bgroup "RQ8: SymbolTable Interning & Resolution"
            [ bench "Batch Intern 1,000 Identifiers" $ whnf (internManyBS sampleIdentifiers) emptySymbolTable
            , bench "Single Symbol Intern"           $ whnf (internSymbolBS "identifier_example") benchSymTable
            , bench "SymbolId Lookup (Hit)"          $ whnf (`resolveSymbolBS` benchSymTable) (SymbolId 42)
            , bench "Preloaded Polyglot Lookup"      $ whnf (`resolveSymbolBS` preloadPolyglotKeywords) (SymbolId 5)
            ]
        , bgroup "RQ9: Selective Outline Mode Speedup for F2 / F3"
            [ bgroup "Python Outline"
                [ bench "Full AST Parse (Large)"     $ whnf (parsePythonSource "large.py") largeSrc
                , bench "Outline Parse (Large)"      $ whnf (parseOutlinePython "large.py") largeSrc
                , bench "F2 Full AST Compute"        $ whnf computeF2 largeProg
                , bench "F2 Outline Mode Compute"    $ whnf (\s -> case parseOutlinePython "large.py" s of Right o -> computeF2Outline o; Left _ -> Fingerprint "") largeSrc
                , bench "F3 Full AST Compute"        $ whnf computeF3 largeProg
                , bench "F3 Outline Mode Compute"    $ whnf (\s -> case parseOutlinePython "large.py" s of Right o -> computeF3Outline o; Left _ -> Fingerprint "") largeSrc
                ]
            , bgroup "Polyglot Outline Extraction"
                [ bench "TS Outline (Large)"         $ whnf (parseOutlineJS "large.ts") tsLarge
                , bench "Go Outline (Large)"         $ whnf (parseOutlineGo "large.go") goLarge
                , bench "Rust Outline (Large)"       $ whnf (parseOutlineRust "large.rs") rsLarge
                , bench "IngestOutlineSource (Large)"$ whnf (\b -> ingestOutlineSource "large.py" b largeSrc) largeBytes
                ]
            ]
        , bgroup "RQ10: v0.0.6-alpha Hardware-Speed Engines"
            [ bgroup "Engine 1: SWAR FastScan"
                [ bench "SWAR scanAsciiAndLineEndings (XL)" $ whnf scanAsciiAndLineEndings xlBytes
                , bench "SWAR fastCanonicalizeBS (XL)"       $ whnf fastCanonicalizeBS xlBytes
                , bench "Standard Unicode canonicalizeText (XL)" $ whnf canonicalizeText xlSrc
                ]
            , bgroup "Engine 2: Fused Direct-to-Hash Streaming"
                [ bench "fusedHashProgram F1 Direct (XL)"   $ whnf fusedHashProgram xlProg
                , bench "Standard 3-Pass F1 Compute (XL)"   $ whnf (hashBytes . canonicalizeProgram . normalizeProgram) xlProg
                , bench "fusedHashDeclarations F2 Direct (XL)" $ whnf (fusedHashDeclarations . extractDeclarations) xlProg
                ]
            , bgroup "Engine 3: Compact Binary Merkle Cache Index (CNTR v2)"
                [ bench "encodeBinaryCache (1,000 files)"   $ whnf encodeBinaryCache largeCache
                , bench "decodeBinaryCache (1,000 files)"   $ whnf decodeBinaryCache largeCacheBin
                , bench "lookupBinaryCache O(log N) Hit"    $ whnf (lookupBinaryCache "src/module_500.py" (FileMetadata "src/module_500.py" 1024 1700000000)) largeCacheBin
                , bench "Legacy JSON Aeson Decode (1,000 files)" $ whnf (Aeson.decodeStrict :: BS.ByteString -> Maybe MerkleCache) largeCacheJSON
                ]
            , bgroup "Engine 4: Dynamic Work-Stealing Parallel Scheduler"
                [ bench "parMapChunks 1,000 Tasks"          $ nfIO (parMapChunks pure [1..1000 :: Int])
                ]
            ]
        , bgroup "RQ11: v0.0.7-alpha Ultra-Low Latency Architecture Engines"
            [ bgroup "Engine 1: Radix-Directed Binary Cache (CNTR v3)"
                [ bench "lookupBinaryCache Radix Hit (1,000 files)" $ whnf (lookupBinaryCache "src/module_500.py" (FileMetadata "src/module_500.py" 1024 1700000000)) largeCacheBin
                , bench "encodeBinaryCache with Radix Directory"    $ whnf encodeBinaryCache largeCache
                , bench "decodeBinaryCache v3 (1,000 files)"        $ whnf decodeBinaryCache largeCacheBin
                ]
            , bgroup "Engine 2: Flat Linear Arena AST"
                [ bench "programToLinearAST Linearization (XL)"     $ whnf programToLinearAST xlProg
                , bench "fusedHashLinearAST Direct Arena Hash (XL)"  $ whnf fusedHashLinearAST xlArena
                ]
            , bgroup "Engine 3: SWAR Direct-to-IR FastPython Parser"
                [ bench "parseFastPythonByteString Direct (XL)"      $ whnf (parseFastPythonByteString "xl.py") xlBytes
                , bench "parseFastPythonToArena Direct (XL)"        $ whnf (parseFastPythonToArena "xl.py") xlSrc
                ]
            , bgroup "Engine 4: Isomorphic Incremental Merkle DAG"
                [ bench "buildMerkleDAG (1,000 files)"              $ whnf buildMerkleDAG repoEntries1000
                , bench "merkleDAGRootHash Root Evaluation"         $ whnf merkleDAGRootHash dag1000
                , bench "diffMerkleDAG (1 delta in 1,000 files)"    $ whnf (diffMerkleDAG dag1000) dag1000Mod
                ]
            , bgroup "Engine 5: SwissTable Open-Addressing Interning"
                [ bench "swissInternBS 1,000 Symbols"               $ whnf (\ids -> foldl' (\(_, tbl) bs -> swissInternBS tbl bs) (SymbolId 0, emptySwissTable 1024) ids) sampleIdentifiers
                , bench "swissLookupBS Hit"                         $ whnf (swissLookupBS benchSwissTable) "identifier_500"
                , bench "swissResolveId Inverse Lookup"             $ whnf (swissResolveId benchSwissTable) (SymbolId 500)
                ]
            ]
        , bgroup "RQ12: v0.0.9-alpha Whole-Repo Intelligence & Verification"
            [ bgroup "Engine 1: Whole-Repo Call Graph (F_WCG)"
                [ bench "buildWholeRepoCallGraph (10 modules)"  $ whnf buildWholeRepoCallGraph modules10
                , bench "computeFWCG (10 modules)"             $ whnf computeFWCG modules10
                ]
            , bgroup "Engine 2: Whole-Repo Inter-Procedural Data-Flow (F_WDF)"
                [ bench "buildWholeRepoDataFlow (10 modules)"   $ whnf buildWholeRepoDataFlow modules10
                , bench "computeFWDF (10 modules)"             $ whnf computeFWDF modules10
                ]
            , bgroup "Engine 3: Semantic Change Impact Slicing (CIA)"
                [ bench "classifySeverity (Interface Mutation)" $ whnf (classifySeverity (feFingerprints sampleBundle)) (feFingerprints (head repo1000))
                , bench "computeImpactSlice (10 modules)"       $ whnf (\b -> computeImpactSlice "src/module_1.py" (feFingerprints sampleBundle) b wcg10 (map fst modules10)) (feFingerprints (head repo1000))
                ]
            , bgroup "Engine 4: Structural Type Contract Invariance (F_T)"
                [ bench "extractTypeContracts (XL)"             $ whnf extractTypeContracts xlProg
                , bench "computeFT (XL)"                        $ whnf computeFT xlProg
                ]
            , bgroup "Engine 5: Memory-Mapped Paged Radix Cache (CNTR v5)"
                [ bench "encodeBinaryCacheV5 (1,000 files)"     $ whnf encodeBinaryCacheV5 largeCache
                , bench "decodeBinaryCacheV5 (1,000 files)"     $ whnf decodeBinaryCacheV5 largeCacheV5Bin
                , bench "lookupBinaryCacheV5 Hit"               $ whnf (lookupBinaryCacheV5 "src/module_500.py" (FileMetadata "src/module_500.py" 1024 1700000000)) largeCacheV5Bin
                ]
            , bgroup "Engine 6: Metamorphic Mutation & Verification"
                [ bench "verifyMetamorphicProgramTransform (XL)"$ whnf (`verifyMetamorphicProgramTransform` (ReformatWhitespaceTrivia 4)) xlProg
                , bench "verifyProgramMutation (XL)"            $ whnf (`verifyProgramMutation` (MutFlipArithmeticOp OpAdd OpSub)) xlProg
                , bench "runMetamorphicSuite (Polyglot Corpus)" $ whnf runMetamorphicSuite polyglotFixtures
                ]
            ]
        ]
    _ -> putStrLn "Error initializing benchmark source fixtures."
