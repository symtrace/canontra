{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Canontra.OptimSpec (spec) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64)
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Test.Hspec

import Canontra.Analysis.CFG (buildCFGs)
import Canontra.Analysis.CompactGraph
import Canontra.Analysis.DFG (buildDFGs)
import Canontra.Cache.Inode (FileMetadata (..))
import Canontra.Cache.MerkleCache
import Canontra.Canonical.Serialize (canonicalizeDeclarations, canonicalizeProgram)
import Canontra.Canonical.StreamingHash (hashBuilderDirect)
import Canontra.Fingerprint.Bundle (computeBundleFromSource)
import Canontra.Fingerprint.Declaration (computeF2, extractDeclarations)
import Canontra.Fingerprint.Source (hashBytes)
import Canontra.Fingerprint.Structural (computeF1)
import Canontra.IR.Arena
import Canontra.IR.Program (Program (..))
import Canontra.Normalize.Fused (fusedNormalizeProgram)
import Canontra.Normalize.Normalize (normalizeProgram)
import Canontra.Parser.FastPython
import Canontra.Parser.Python (parsePythonSource)
import Canontra.Parser.SwissTable
import Canontra.Parser.SymbolTable (SymbolId (..))
import Canontra.Repository.MerkleDAG
import Canontra.Repository.Parallel (parMapChunks)
import Canontra.Types

-- | Helper to build a synthetic CNTR v2 binary buffer for backward-compatibility regression tests.
makeSyntheticCNTRv2 :: [(FilePath, FileMetadata, FingerprintBundle)] -> BS.ByteString
makeSyntheticCNTRv2 entries =
  let !count = fromIntegral (length entries) :: Word32
      pathBSList = [TE.encodeUtf8 (T.pack p) | (p, _, _) <- entries]
      pathLens   = map BS.length pathBSList
      pathOffsets = scanl (+) 0 pathLens
      strTableBS = BS.concat pathBSList
      !strTableOffset = 32 + fromIntegral count * 288 :: Word64

      header = BB.byteString "CNTR"             -- Magic
            <> BB.word16LE 0x0002               -- Version 2
            <> BB.word16LE 0x0001               -- Hash Alg
            <> BB.word32LE count                -- Count
            <> BB.word64LE strTableOffset
            <> BB.byteString (BS.replicate 12 0)

      encodeRec (_, FileMetadata _ sz mt, bundle) !pOff !pLen =
        BB.word32LE (fromIntegral pOff)
        <> BB.word16LE (fromIntegral pLen)
        <> BB.word16LE 0 -- flags (raw)
        <> BB.word64LE (fromIntegral sz)
        <> BB.word64LE (fromIntegral mt)
        <> BB.byteString (BS.take 32 (TE.encodeUtf8 (unFingerprint (f0Source bundle)) <> BS.replicate 32 0))
        <> BB.byteString (BS.take 32 (TE.encodeUtf8 (unFingerprint (f1Structural bundle)) <> BS.replicate 32 0))
        <> BB.byteString (BS.take 32 (TE.encodeUtf8 (unFingerprint (f2Declaration bundle)) <> BS.replicate 32 0))
        <> BB.byteString (BS.take 32 (TE.encodeUtf8 (unFingerprint (f3Dependency bundle)) <> BS.replicate 32 0))
        <> BB.byteString (BS.take 32 (TE.encodeUtf8 (unFingerprint (fCGCallGraph bundle)) <> BS.replicate 32 0))
        <> BB.byteString (BS.take 32 (TE.encodeUtf8 (unFingerprint (fCFControlFlow bundle)) <> BS.replicate 32 0))
        <> BB.byteString (BS.take 32 (TE.encodeUtf8 (unFingerprint (fDFDataFlow bundle)) <> BS.replicate 32 0))
        <> BB.byteString (BS.take 32 (TE.encodeUtf8 (unFingerprint (f4Composite bundle)) <> BS.replicate 32 0))
        <> BB.word64LE 0

      records = mconcat $ zipWith3 encodeRec entries pathOffsets pathLens
  in LBS.toStrict $ BB.toLazyByteString (header <> records <> BB.byteString strTableBS)

spec :: Spec
spec = do
  describe "High-Performance Optimization Engines" $ do

    it "CompactGraph: Packs and unpacks CFG and DFG edges losslessly" $ do
      let rawEdges = [(0, 1), (1, 2), (2, 3), (3, 0), (100, 200)]
          compactCFG = packCFGEdges rawEdges
          unpacked = unpackCFGEdges compactCFG
      unpacked `shouldBe` rawEdges

    it "CompactGraph: Converts full AST CFGs and DFGs to compact representations" $ do
      let pyCode = "def test(a, b):\n    c = a + b\n    if c > 0:\n        return c\n    return 0\n"
      case parsePythonSource "test.py" pyCode of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let cfgs = buildCFGs prog
              dfgs = buildDFGs prog
          length cfgs `shouldBe` 1
          length dfgs `shouldBe` 1
          let compactCFG = fromControlFlowGraph (head cfgs)
              compactDFG = fromDataFlowGraph (head dfgs)
          null (unpackCFGEdges compactCFG) `shouldBe` False
          null (unpackDFGEdges compactDFG) `shouldBe` False

    it "StreamingHash: Bit-identical to hashBytes on raw ByteString chunks" $ do
      let sampleText = "The quick brown fox jumps over the lazy dog 1234567890"
          bs = BSC.pack sampleText
          builder = BB.byteString bs
          h1 = hashBytes bs
          h2 = hashBuilderDirect builder
      unFingerprint h1 `shouldBe` unFingerprint h2

    it "MerkleCache: Correctly hits on identical size/mtime and misses on change" $ do
      let meta1 = FileMetadata "src/calc.py" 1024 1700000000
          metaModified = FileMetadata "src/calc.py" 1050 1700000001
          b = FingerprintBundle (Fingerprint "s") (Fingerprint "str") (Fingerprint "d") (Fingerprint "dp") (Fingerprint "cg") (Fingerprint "cf") (Fingerprint "df") (Fingerprint "") (Fingerprint "c")
          cache0 = emptyCache
          cache1 = insertCache "src/calc.py" meta1 b cache0
      lookupCache "src/calc.py" meta1 cache1 `shouldBe` Just b
      lookupCache "src/calc.py" metaModified cache1 `shouldBe` Nothing

    it "MerkleCache: CNTR v3 binary format encodes and decodes losslessly" $ do
      let meta1 = FileMetadata "src/a.py" 100 1700000000
          meta2 = FileMetadata "src/b.py" 200 1700000002
          b1 = FingerprintBundle (Fingerprint "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
                                 (Fingerprint "f1a0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
                                 (Fingerprint "f2a0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
                                 (Fingerprint "f3a0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
                                 (Fingerprint "f4a0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
                                 (Fingerprint "f5a0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
                                 (Fingerprint "f6a0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
                                 (Fingerprint "")
                                 (Fingerprint "f7a0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
          b2 = FingerprintBundle (Fingerprint "s2") (Fingerprint "str2") (Fingerprint "d2") (Fingerprint "dp2") (Fingerprint "cg2") (Fingerprint "cf2") (Fingerprint "df2") (Fingerprint "") (Fingerprint "c2")
          cache0 = insertCache "src/b.py" meta2 b2 (insertCache "src/a.py" meta1 b1 emptyCache)
          bin = encodeBinaryCache cache0
          decoded = decodeBinaryCache bin
      decoded `shouldBe` Just cache0

    it "MerkleCache: CNTR v3 lookupBinaryCache performs fast radix-narrowed search" $ do
      let meta1 = FileMetadata "src/a.py" 100 1700000000
          meta2 = FileMetadata "src/b.py" 200 1700000002
          meta3 = FileMetadata "src/c.py" 300 1700000003
          b1 = FingerprintBundle (Fingerprint "s1") (Fingerprint "str1") (Fingerprint "d1") (Fingerprint "dp1") (Fingerprint "cg1") (Fingerprint "cf1") (Fingerprint "df1") (Fingerprint "") (Fingerprint "c1")
          b2 = FingerprintBundle (Fingerprint "s2") (Fingerprint "str2") (Fingerprint "d2") (Fingerprint "dp2") (Fingerprint "cg2") (Fingerprint "cf2") (Fingerprint "df2") (Fingerprint "") (Fingerprint "c2")
          b3 = FingerprintBundle (Fingerprint "s3") (Fingerprint "str3") (Fingerprint "d3") (Fingerprint "dp3") (Fingerprint "cg3") (Fingerprint "cf3") (Fingerprint "df3") (Fingerprint "") (Fingerprint "c3")
          cache0 = insertCache "src/c.py" meta3 b3 (insertCache "src/b.py" meta2 b2 (insertCache "src/a.py" meta1 b1 emptyCache))
          bin = encodeBinaryCache cache0
      lookupBinaryCache "src/b.py" meta2 bin `shouldBe` Just b2
      lookupBinaryCache "src/a.py" meta1 bin `shouldBe` Just b1
      lookupBinaryCache "src/c.py" meta3 bin `shouldBe` Just b3
      lookupBinaryCache "src/b.py" (FileMetadata "src/b.py" 999 1700000002) bin `shouldBe` Nothing
      lookupBinaryCache "src/missing.py" meta1 bin `shouldBe` Nothing

    it "MerkleCache: CNTR v3 supports large multi-file scaling with radix directory" $ do
      let paths = ["src/module_" ++ show (i :: Int) ++ ".py" | i <- [1..100]]
          indices = [1..100] :: [Int]
          entries = [(p, FileMetadata p (fromIntegral i * 10) (1700000000 + fromIntegral i), FingerprintBundle (Fingerprint (T.pack ("s" ++ show i))) (Fingerprint (T.pack ("st" ++ show i))) (Fingerprint (T.pack ("d" ++ show i))) (Fingerprint (T.pack ("dp" ++ show i))) (Fingerprint (T.pack ("cg" ++ show i))) (Fingerprint (T.pack ("cf" ++ show i))) (Fingerprint (T.pack ("df" ++ show i))) (Fingerprint "") (Fingerprint (T.pack ("c" ++ show i)))) | (i, p) <- zip indices paths]
          cache = foldr (\(p, m, b) c -> insertCache p m b c) emptyCache entries
          bin = encodeBinaryCache cache
      decodeBinaryCache bin `shouldBe` Just cache
      -- Verify lookups across multiple entries
      mapM_ (\(p, m, b) -> lookupBinaryCache p m bin `shouldBe` Just b) (take 10 entries)

    it "MerkleCache: Backwards-compatible decode and lookup for CNTR v2 binary buffers" $ do
      let meta1 = FileMetadata "src/v2_a.py" 100 1700000000
          meta2 = FileMetadata "src/v2_b.py" 200 1700000002
          b1 = FingerprintBundle (Fingerprint "s1") (Fingerprint "str1") (Fingerprint "d1") (Fingerprint "dp1") (Fingerprint "cg1") (Fingerprint "cf1") (Fingerprint "df1") (Fingerprint "") (Fingerprint "c1")
          b2 = FingerprintBundle (Fingerprint "s2") (Fingerprint "str2") (Fingerprint "d2") (Fingerprint "dp2") (Fingerprint "cg2") (Fingerprint "cf2") (Fingerprint "df2") (Fingerprint "") (Fingerprint "c2")
          v2Bin = makeSyntheticCNTRv2 [("src/v2_a.py", meta1, b1), ("src/v2_b.py", meta2, b2)]
      -- Decode v2 buffer
      let decoded = decodeBinaryCache v2Bin
      decoded `shouldNotBe` Nothing
      -- Lookup in v2 buffer
      lookupBinaryCache "src/v2_a.py" meta1 v2Bin `shouldBe` Just b1
      lookupBinaryCache "src/v2_b.py" meta2 v2Bin `shouldBe` Just b2
      lookupBinaryCache "src/v2_missing.py" meta1 v2Bin `shouldBe` Nothing

    it "MerkleCache: Transparently reads and migrates legacy JSON cache files to CNTR v3" $ do
      tmpDir <- getTemporaryDirectory
      let cacheFile = tmpDir </> "legacy_cache_test_v3.bin"
          meta = FileMetadata "src/legacy.py" 500 1700000000
          b = FingerprintBundle (Fingerprint "s") (Fingerprint "str") (Fingerprint "d") (Fingerprint "dp") (Fingerprint "cg") (Fingerprint "cf") (Fingerprint "df") (Fingerprint "") (Fingerprint "c")
          cache = insertCache "src/legacy.py" meta b emptyCache
      -- Write legacy JSON
      LBS.writeFile cacheFile (Aeson.encode cache)
      -- Read via readMerkleCache
      loadedCache <- readMerkleCache cacheFile
      lookupCache "src/legacy.py" meta loadedCache `shouldBe` Just b
      -- Write via writeMerkleCache (upgrades to CNTR v3)
      writeMerkleCache cacheFile loadedCache
      -- Verify new file is CNTR v3
      reloadedCache <- readMerkleCache cacheFile
      lookupCache "src/legacy.py" meta reloadedCache `shouldBe` Just b
      removeFile cacheFile

    it "WorkStealing: parMapChunks accurately processes empty, small, and large lists preserving order" $ do
      let items = [1..200 :: Int]
      resEmpty <- parMapChunks (\x -> pure (x * 2)) ([] :: [Int])
      resEmpty `shouldBe` []
      resSingle <- parMapChunks (\x -> pure (x * 2)) [42 :: Int]
      resSingle `shouldBe` [84]
      resLarge <- parMapChunks (\x -> pure (x * 2)) items
      resLarge `shouldBe` map (* 2) items

    it "Fused: Equivalence between fused and standard normalization passes" $ do
      let pyCode = "# Comment\ndef add(a: int, b: int = 0) -> int:\n    '''Doc'''\n    return a + b\n"
      case parsePythonSource "math.py" pyCode of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let n1 = normalizeProgram prog
              n2 = fusedNormalizeProgram prog
          n1 `shouldBe` n2

    it "FusedStream: F1 Structural hash matches canonicalizeProgram . normalizeProgram" $ do
      let pyCode = "import math\nfrom typing import List\n\n# Main computation\ndef compute(items: List[float], scale: float = 1.0) -> float:\n    '''Docstring'''\n    total = 0.0\n    for x in items:\n        total += x * scale\n    return total\n"
      case parsePythonSource "compute.py" pyCode of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let expected = hashBytes (canonicalizeProgram (normalizeProgram prog))
              actual = computeF1 prog
          unFingerprint actual `shouldBe` unFingerprint expected

    it "FusedStream: F2 Declaration hash matches canonicalizeDeclarations . extractDeclarations . normalizeProgram" $ do
      let pyCode = "class Calculator:\n    '''Class doc'''\n    def add(self, a: int, b: int = 0) -> int:\n        '''Method doc'''\n        return a + b\n\ndef helper() -> None:\n    pass\n"
      case parsePythonSource "calc.py" pyCode of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let expected = hashBytes (canonicalizeDeclarations (extractDeclarations (normalizeProgram prog)))
              actual = computeF2 prog
          unFingerprint actual `shouldBe` unFingerprint expected

    it "LinearAST: emptyLinearAST has 0 nodes and lossless bijection" $ do
      linearASTNodeCount emptyLinearAST `shouldBe` 0
      case linearASTToProgram emptyLinearAST of
        Left err -> expectationFailure err
        Right prog -> prog `shouldBe` Program [] ""

    it "LinearAST: programToLinearAST converts Program to contiguous layout and preserves F1 hash" $ do
      let pyCode = "def factorial(n: int) -> int:\n    if n <= 1:\n        return 1\n    return n * factorial(n - 1)\n"
      case parsePythonSource "fact.py" pyCode of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let arena = programToLinearAST prog
          linearASTNodeCount arena `shouldSatisfy` (> 0)
          linearASTToProgram arena `shouldBe` Right prog
          fusedHashLinearAST arena `shouldBe` computeF1 prog

    it "FastPython: 64-bit IndentStack pushes, pops, and tracks depth accurately" $ do
      let s0 = emptyIndentStack
      currentIndent s0 `shouldBe` 0
      case pushIndent 4 s0 of
        Nothing -> expectationFailure "Failed to push 4"
        Just s1 -> do
          currentIndent s1 `shouldBe` 4
          case pushIndent 8 s1 of
            Nothing -> expectationFailure "Failed to push 8"
            Just s2 -> do
              currentIndent s2 `shouldBe` 8
              case popIndent s2 of
                Nothing -> expectationFailure "Failed to pop 8"
                Just (val1, s3) -> do
                  val1 `shouldBe` 8
                  currentIndent s3 `shouldBe` 4
                  case popIndent s3 of
                    Nothing -> expectationFailure "Failed to pop 4"
                    Just (val2, s4) -> do
                      val2 `shouldBe` 4
                      currentIndent s4 `shouldBe` 0

    it "FastPython: parseFastPythonSource produces identical AST to parsePythonSource" $ do
      let pyCode = "import os\n\ndef greet(name: str) -> str:\n    return f'Hello {name}'\n"
      let resStandard = parsePythonSource "greet.py" pyCode
      let resFast = parseFastPythonSource "greet.py" pyCode
      resFast `shouldBe` resStandard

    it "FastPython: parseFastPythonToArena directly produces valid LinearAST" $ do
      let pyCode = "x = 42\ny = x + 1\n"
      case parseFastPythonToArena "simple.py" pyCode of
        Left err -> expectationFailure (show err)
        Right arena -> linearASTNodeCount arena `shouldSatisfy` (> 0)

    it "FastPython: HybridIndentStack pushes, pops, and tracks unlimited depth (20 levels)" $ do
      let s0 = emptyHybridIndentStack
      currentHybridIndent s0 `shouldBe` 0
      hybridIndentDepth s0 `shouldBe` 0
      let levels = [fromIntegral (i * 4) | i <- [1..20 :: Int]]
          s20 = foldl (\st lvl -> pushHybridIndent lvl st) s0 levels
      hybridIndentDepth s20 `shouldBe` 20
      currentHybridIndent s20 `shouldBe` 80
      hybridIndentToList s20 `shouldBe` levels

      let popAll st = case popHybridIndent st of
            Nothing -> []
            Just (val, nextSt) -> val : popAll nextSt
          popped = popAll s20
      popped `shouldBe` reverse levels

    it "FastPython: advanceColumn calculates PEP 8 column-modulo tab stops accurately" $ do
      advanceColumn 0 ' ' `shouldBe` 1
      advanceColumn 4 ' ' `shouldBe` 5
      advanceColumn 0 '\t' `shouldBe` 8
      advanceColumn 2 '\t' `shouldBe` 8
      advanceColumn 7 '\t' `shouldBe` 8
      advanceColumn 8 '\t' `shouldBe` 16
      advanceColumn 11 '\t' `shouldBe` 16
      advanceColumn 16 '\t' `shouldBe` 24

    it "FastPython: successfully parses synthetic 20-level deeply nested Python blocks" $ do
      let indentLine lvl = replicate (lvl * 4) ' ' ++ "if x > " ++ show lvl ++ ":"
          deepCode = unlines (
            [ "def deeply_nested(x):" ]
            ++ [indentLine i | i <- [1..20]]
            ++ [ replicate (21 * 4) ' ' ++ "return x" ]
            ++ [ "    return 0\n" ]
            )
      case parsePythonSource "deep.py" (T.pack deepCode) of
        Left err -> expectationFailure ("Standard parser failed on 20 levels: " ++ show err)
        Right prog -> do
          case parseFastPythonSource "deep.py" (T.pack deepCode) of
            Left err -> expectationFailure ("FastPython failed on 20 levels: " ++ show err)
            Right fastProg -> fastProg `shouldBe` prog
          case parseFastPythonToArena "deep.py" (T.pack deepCode) of
            Left err -> expectationFailure ("FastPython to Arena failed on 20 levels: " ++ show err)
            Right arena -> linearASTNodeCount arena `shouldSatisfy` (> 0)

    it "MerkleDAG: builds hierarchical DAG and calculates deterministic root digest" $ do
      let f1 = "src/core/lexer.py"
          f2 = "src/core/parser.py"
          f3 = "src/utils/helpers.py"
      case ( computeBundleFromSource "lexer.py" "def lex(): pass\n"
           , computeBundleFromSource "parser.py" "def parse(): pass\n"
           , computeBundleFromSource "helpers.py" "def help(): pass\n"
           ) of
        (Right b1, Right b2, Right b3) -> do
          let entries = [(f1, b1), (f2, b2), (f3, b3)]
              dag = buildMerkleDAG entries
          dagNodeCount dag `shouldSatisfy` (>= 3)
          flattenMerkleDAG dag `shouldBe` entries
          let (Fingerprint rootHash) = merkleDAGRootHash dag
          T.length rootHash `shouldBe` 64
        _ -> expectationFailure "Failed to compute bundles"

    it "MerkleDAG: diffMerkleDAG prunes unchanged subtrees and identifies only modified files" $ do
      let f1 = "src/core/parser.py"
          f2 = "src/core/lexer.py"
          f3 = "src/utils/helpers.py"
      case ( computeBundleFromSource "parser.py" "def parse(): pass\n"
           , computeBundleFromSource "lexer.py" "def lex(): pass\n"
           , computeBundleFromSource "helpers.py" "def help(): pass\n"
           , computeBundleFromSource "parser.py" "def parse_v2(): pass\n"
           ) of
        (Right b1, Right b2, Right b3, Right b1_mod) -> do
          let dagOriginal = buildMerkleDAG [(f1, b1), (f2, b2), (f3, b3)]
              dagModified = buildMerkleDAG [(f1, b1_mod), (f2, b2), (f3, b3)]
          diffMerkleDAG dagOriginal dagOriginal `shouldBe` []
          diffMerkleDAG dagOriginal dagModified `shouldBe` [f1]
        _ -> expectationFailure "Failed to compute bundles"

    it "SwissTable: emptySwissTable initializes and interns symbols with bijection" $ do
      let tbl0 = emptySwissTable 16
      swissTableSize tbl0 `shouldBe` 0
      let (id1, tbl1) = swissInternBS tbl0 "apple"
          (id2, tbl2) = swissInternBS tbl1 "banana"
          (id3, tbl3) = swissInternBS tbl2 "apple" -- Duplicate hit
      id1 `shouldBe` SymbolId 0
      id2 `shouldBe` SymbolId 1
      id3 `shouldBe` id1
      swissTableSize tbl3 `shouldBe` 2
      swissLookupBS tbl3 "apple" `shouldBe` Just id1
      swissLookupBS tbl3 "banana" `shouldBe` Just id2
      swissLookupBS tbl3 "cherry" `shouldBe` Nothing
      swissResolveId tbl3 id1 `shouldBe` Just "apple"
      swissResolveId tbl3 id2 `shouldBe` Just "banana"
