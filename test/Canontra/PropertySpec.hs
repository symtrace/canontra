{- |
Module      : Canontra.PropertySpec
Description : Property-based tests verifying core algebraic invariants for v0.0.3-alpha.

This module validates the essential mathematical guarantees:
- Zero-span source coordinate invariance
- Idempotence of normalization
- Byte-level determinism of binary serialization
- IEEE-754 float canonicalization (-0.0 == +0.0, NaN normalization)
- Unicode NFC precomposition normalization
- Multi-scope docstring stripping invariance
- Semantic string preservation
- Call graph invariance under formatting
- Structural sensitivity to semantic changes
- Repository order independence
-}
{-# LANGUAGE OverloadedStrings #-}
module Canontra.PropertySpec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Canontra.Cache.Inode (FileMetadata (..))
import Canontra.Cache.MerkleCache (decodeBinaryCache, emptyCache, encodeBinaryCache, insertCache, lookupBinaryCache)
import Canontra.Canonical.Float (canonicalizeFloatWord)
import Canontra.Canonical.Serialize (canonicalizeProgram)
import Canontra.Canonical.Unicode (canonicalizeText)
import Canontra.Fingerprint.Bundle (computeBundleFromSource)
import Canontra.Fingerprint.CallGraph (computeFCG)
import Canontra.Fingerprint.Source (hashBytes)
import Canontra.Fingerprint.Structural (computeF1)
import Canontra.Normalize.Normalize (normalizeProgram)
import Canontra.Parser.Python (parsePythonSource)
import Canontra.Repository.Parallel (parMapChunks)
import Canontra.Repository.Repository (computeRepositoryFingerprint)
import Canontra.Types

spec :: Spec
spec = do
  describe "Algebraic and Engine Properties" $ do

    it "Property: Normalization Idempotence N(N(P)) == N(P)" $ do
      let snippet = T.unlines
            [ "def compute(x: int, y: int = 0) -> int:"
            , "    # inline comment"
            , "    \"\"\"Docstring to strip.\"\"\""
            , "    z = x + y"
            , "    return z * 2"
            ]
      case parsePythonSource "sample.py" snippet of
        Left err -> expectationFailure (show (peReason err))
        Right prog -> do
          let norm1 = normalizeProgram prog
              norm2 = normalizeProgram norm1
          norm1 `shouldBe` norm2

    it "Property: Canonicalization Determinism C(P) == C(P)" $ do
      let snippet = T.unlines
            [ "class User:"
            , "    def __init__(self, name: str):"
            , "        self.name = name"
            , "    async def fetch(self):"
            , "        return self.name"
            ]
      case parsePythonSource "sample.py" snippet of
        Left err -> expectationFailure (show (peReason err))
        Right prog -> do
          let c1 = canonicalizeProgram (normalizeProgram prog)
              c2 = canonicalizeProgram (normalizeProgram prog)
          c1 `shouldBe` c2

    it "Property: IEEE-754 Float Canonicalization (-0.0 == +0.0, NaN normalization)" $ do
      let posZero = 0.0 :: Double
          negZero = -0.0 :: Double
          nan1 = 0/0 :: Double
          nan2 = (-0)/0 :: Double
      canonicalizeFloatWord negZero `shouldBe` canonicalizeFloatWord posZero
      canonicalizeFloatWord nan1 `shouldBe` canonicalizeFloatWord nan2

    it "Property: Unicode NFC Normalization (Precomposed == Decomposed)" $ do
      let decomposed = "caf\x0065\x0301" -- cafe with combining acute
          precomposed = "caf\x00e9"     -- café with precomposed é
      canonicalizeText decomposed `shouldBe` canonicalizeText precomposed

    it "Property: Zero-Span Relocation Invariance F1(P) == F1(T_relocate(P))" $ do
      let p1 = "def calc(a, b):\n    return a + b\n"
          p2 = "\n\n\ndef   calc(  a ,  b  ) :\n    return   a   +   b\n\n\n"
      case (computeBundleFromSource "1.py" p1, computeBundleFromSource "2.py" p2) of
        (Right b1, Right b2) -> f1Structural b1 `shouldBe` f1Structural b2
        (Left e, _) -> expectationFailure (show (peReason e))
        (_, Left e) -> expectationFailure (show (peReason e))

    it "Property: Formatting Invariance F1(P) == F1(T_format(P))" $ do
      let orig = "def add(a, b):\n    return a + b\n"
          fmt  = "def add(a,b):return a+b\n"
      case (computeBundleFromSource "orig.py" orig, computeBundleFromSource "fmt.py" fmt) of
        (Right b1, Right b2) -> f1Structural b1 `shouldBe` f1Structural b2
        (Left e, _) -> expectationFailure (show (peReason e))
        (_, Left e) -> expectationFailure (show (peReason e))

    it "Property: Comment Invariance F1(P) == F1(T_comment(P))" $ do
      let orig = "def process(items):\n    return [x * 2 for x in items]\n"
          comm = T.unlines
            [ "def process(items):"
            , "    # Double each element in items"
            , "    # Note: items must be iterable"
            , "    return [x * 2 for x in items]  # inline comment"
            ]
      case (computeBundleFromSource "orig.py" orig, computeBundleFromSource "comm.py" comm) of
        (Right b1, Right b2) -> f1Structural b1 `shouldBe` f1Structural b2
        (Left e, _) -> expectationFailure (show (peReason e))
        (_, Left e) -> expectationFailure (show (peReason e))

    it "Property: Multi-Scope Docstring Stripping Invariance" $ do
      let withDocs = T.unlines
            [ "\"\"\"Module level docstring.\"\"\""
            , "class Service:"
            , "    \"\"\"Class level docstring.\"\"\""
            , "    def handle(self):"
            , "        \"\"\"Function level docstring.\"\"\""
            , "        return 42"
            ]
          withoutDocs = T.unlines
            [ "class Service:"
            , "    def handle(self):"
            , "        return 42"
            ]
      case (computeBundleFromSource "doc.py" withDocs, computeBundleFromSource "nodoc.py" withoutDocs) of
        (Right b1, Right b2) -> f1Structural b1 `shouldBe` f1Structural b2
        (Left e, _) -> expectationFailure (show (peReason e))
        (_, Left e) -> expectationFailure (show (peReason e))

    it "Property: Semantic String Literal Preservation" $ do
      let p1 = "def get_msg():\n    return \"message A\"\n"
          p2 = "def get_msg():\n    return \"message B\"\n"
      case (computeBundleFromSource "1.py" p1, computeBundleFromSource "2.py" p2) of
        (Right b1, Right b2) -> f1Structural b1 `shouldNotBe` f1Structural b2
        (Left e, _) -> expectationFailure (show (peReason e))
        (_, Left e) -> expectationFailure (show (peReason e))

    it "Property: Call Graph Invariance under Formatting" $ do
      let p1 = "def a(): b()\ndef b(): c()\ndef c(): pass\n"
          p2 = "def a():\n    b()\n\ndef b():\n    c()\n\ndef c():\n    pass\n"
      case (parsePythonSource "1.py" p1, parsePythonSource "2.py" p2) of
        (Right prog1, Right prog2) -> computeFCG prog1 `shouldBe` computeFCG prog2
        _ -> expectationFailure "Parse failed"

    it "Property: Structural Sensitivity F1(P) /= F1(T_structural(P))" $ do
      let p1 = "def calc(a, b):\n    return a + b\n"
          p2 = "def calc(a, b):\n    return a - b\n"
      case (computeBundleFromSource "p1.py" p1, computeBundleFromSource "p2.py" p2) of
        (Right b1, Right b2) -> f1Structural b1 `shouldNotBe` f1Structural b2
        (Left e, _) -> expectationFailure (show (peReason e))
        (_, Left e) -> expectationFailure (show (peReason e))

    it "Property: Repository Order Independence F_R(P_pi) == F_R(P)" $ do
      let b1 = FingerprintBundle (Fingerprint "s1") (Fingerprint "str1") (Fingerprint "d1") (Fingerprint "dp1") (Fingerprint "cg1") (Fingerprint "cf1") (Fingerprint "df1") (Fingerprint "t1") (Fingerprint "c1")
          b2 = FingerprintBundle (Fingerprint "s2") (Fingerprint "str2") (Fingerprint "d2") (Fingerprint "dp2") (Fingerprint "cg2") (Fingerprint "cf2") (Fingerprint "df2") (Fingerprint "t2") (Fingerprint "c2")
          e1 = FileEntry "a.py" b1
          e2 = FileEntry "b.py" b2
          r1 = computeRepositoryFingerprint [e1, e2]
          r2 = computeRepositoryFingerprint [e2, e1]
      r1 `shouldBe` r2

    it "Property: Fused Streaming Hash Parity F1(P) == Hash(Canonicalize(Normalize(P)))" $ do
      let code = "def calculate(a: int, b: int = 10) -> int:\n    '''Docstring'''\n    x = a * 2\n    # Comment\n    if x > 5:\n        return x + b\n    return b\n"
      case parsePythonSource "test.py" code of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let expected = hashBytes (canonicalizeProgram (normalizeProgram prog))
              actual = computeF1 prog
          unFingerprint actual `shouldBe` unFingerprint expected

    it "Property: Binary Merkle Cache Roundtrip Bijection" $
      property $ forAll (listOf (elements (['a'..'z'] ++ ['0'..'9'] ++ ['_', '/']))) $ \rawPath ->
        let path = if null rawPath then "app/main.py" else rawPath
            meta = FileMetadata path 1024 1700000000
            b = FingerprintBundle (Fingerprint "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
                                   (Fingerprint "f1a0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
                                   (Fingerprint "f2a0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
                                   (Fingerprint "f3a0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
                                   (Fingerprint "f4a0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
                                   (Fingerprint "f5a0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
                                   (Fingerprint "f6a0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
                                   (Fingerprint "")
                                   (Fingerprint "f7a0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
            cache = insertCache path meta b emptyCache
        in decodeBinaryCache (encodeBinaryCache cache) === Just cache

    it "Property: Multi-File CNTR v3 Cache Lookup Invariance" $
      property $ forAll (choose (1, 20 :: Int)) $ \n ->
        let indices = [1..n]
            paths = ["src/module_" ++ show i ++ ".py" | i <- indices]
            entries = [(p, FileMetadata p (fromIntegral (i * 100)) (1700000000 + fromIntegral i), FingerprintBundle (Fingerprint (T.pack ("s" ++ show i))) (Fingerprint (T.pack ("st" ++ show i))) (Fingerprint (T.pack ("d" ++ show i))) (Fingerprint (T.pack ("dp" ++ show i))) (Fingerprint (T.pack ("cg" ++ show i))) (Fingerprint (T.pack ("cf" ++ show i))) (Fingerprint (T.pack ("df" ++ show i))) (Fingerprint "") (Fingerprint (T.pack ("c" ++ show i)))) | (i, p) <- zip indices paths]
            cache = foldr (\(p, m, b) c -> insertCache p m b c) emptyCache entries
            bin = encodeBinaryCache cache
        in conjoin [lookupBinaryCache p m bin === Just b | (p, m, b) <- entries]

    it "Property: Work-Stealing List Order Invariance" $
      property $ \xs ->
        ioProperty $ do
          res <- parMapChunks pure (xs :: [Int])
          pure (res === xs)
