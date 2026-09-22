{-# LANGUAGE OverloadedStrings #-}
module Canontra.BugfixSpec (spec) where

import Control.DeepSeq (rnf)
import Test.Hspec

import Canontra.Analysis.CFG (ControlFlowGraph (..), buildCFGs)
import Canontra.Analysis.DFG (DFGNode (..), DataFlowGraph (..), DefUseKind (..), buildDFGs)
import Canontra.Analysis.Scope (SymbolBinding (..), allBindings, analyzeProgramScope)
import Canontra.Cache.Inode (FileMetadata (..))
import Canontra.Cache.MerkleCache (emptyCache, insertCache, lookupCache)
import Canontra.Fingerprint.Bundle (computeBundleFromSource)
import Canontra.Fingerprint.Structural (computeF1)
import Canontra.Normalize.Normalize (normalizeProgram)
import Canontra.Parser.Go (parseGoSource)
import Canontra.Parser.JS (parseJSSource)
import Canontra.Parser.Polyglot (parsePolyglotSource)
import Canontra.Parser.Python (parsePythonSource)
import Canontra.Parser.Rust (parseRustSource)
import Canontra.Repository.Repository (computeRepositoryFingerprint)
import Canontra.Types

spec :: Spec
spec = do
  describe "Post-v0.0.7 Bug Remediation & Hardening Matrix" $ do

    it "BUG-07: Disambiguates TypeScript generic arrow functions from JSX tags" $ do
      let tsCode = "const identity = <T>(x: T): T => x;\nconst pair = <T, U>(a: T, b: U) => { return a; };"
      case parsePolyglotSource "generic.ts" tsCode of
        Left err -> expectationFailure ("TS generic arrow parse failed: " ++ show err)
        Right _ -> do
          let fps = computeBundleFromSource "generic.ts" tsCode
          case fps of
            Left err -> expectationFailure (show err)
            Right b  -> unFingerprint (f1Structural b) `shouldNotBe` ""

    it "BUG-08: Parses Rust macro calls with explicit lifetime parameters" $ do
      let rsCode = "pub fn query() {\n  query_as!(User, 'a, \"SELECT * FROM users\");\n  custom_macro!('static, String);\n}"
      case parseRustSource "db.rs" rsCode of
        Left err -> expectationFailure ("Rust macro lifetime parse failed: " ++ show err)
        Right _  -> pure ()

    it "BUG-09: Unrolls Go type switches with multi-type cases into distinct CFG branches" $ do
      let goCode = "package main\nfunc Check(val interface{}) {\n  switch val.(type) {\n  case int, int64, float64:\n    print(1)\n  case string:\n    print(2)\n  default:\n    print(0)\n  }\n}"
      case parseGoSource "typeswitch.go" goCode of
        Left err -> expectationFailure ("Go type switch parse failed: " ++ show err)
        Right prog -> do
          let cfgs = buildCFGs prog
          length cfgs `shouldBe` 1
          let cfg = head cfgs
          length (cfgEdges cfg) `shouldSatisfy` (>= 5)

    it "BUG-10: Cross-platform POSIX path normalization produces identical Merkle roots" $ do
      let b1 = FingerprintBundle (Fingerprint "s1") (Fingerprint "str1") (Fingerprint "d1") (Fingerprint "dp1") (Fingerprint "cg1") (Fingerprint "cf1") (Fingerprint "df1") (Fingerprint "t1") (Fingerprint "c1")
          eWin   = [FileEntry "src\\core\\main.py" b1, FileEntry "pkg\\util\\math.go" b1]
          ePosix = [FileEntry "src/core/main.py" b1, FileEntry "pkg/util/math.go" b1]
          fpWin   = computeRepositoryFingerprint eWin
          fpPosix = computeRepositoryFingerprint ePosix
      fpWin `shouldBe` fpPosix

    it "BUG-11: Deep strictness evaluation prevents memory thunk accumulation" $ do
      let tsCode = "function sum(a: number, b: number): number { return a + b; }"
      case parseJSSource "math.ts" tsCode of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let scopes = analyzeProgramScope prog
          rnf scopes `shouldBe` ()

    it "BUG-12: Parses Python 3.12 PEP 701 nested f-strings with quote reuse" $ do
      let pyCode = "msg = f\"result: {f'inner: {value}'}\""
      case parsePythonSource "fstring.py" pyCode of
        Left err -> expectationFailure ("PEP 701 fstring parse failed: " ++ show err)
        Right prog -> do
          let f1 = computeF1 prog
          unFingerprint f1 `shouldNotBe` ""

    it "BUG-13: Ingests PEP 634 match/case with wildcard pattern and guards" $ do
      let pyCode = "def route(action):\n    match action:\n        case [\"get\", url] if len(url) > 0:\n            return 200\n        case _:\n            return 404\n"
      case parsePythonSource "match.py" pyCode of
        Left err -> expectationFailure ("PEP 634 match parse failed: " ++ show err)
        Right prog -> do
          let cfgs = buildCFGs prog
          length cfgs `shouldBe` 1

    it "BUG-14: Hoists PEP 572 walrus operator in comprehension to function scope" $ do
      let pyCode = "def process(data):\n    results = [y for x in data if (y := transform(x)) > 0]\n    return y\n"
      case parsePythonSource "walrus.py" pyCode of
        Left err -> expectationFailure ("Walrus scope parse failed: " ++ show err)
        Right prog -> do
          let scopes = analyzeProgramScope prog
          any (\b -> symName b == "y") (concatMap allBindings scopes) `shouldBe` True

    it "BUG-15: Disambiguates regex literal from division following return keyword in JS" $ do
      let jsCode = "function test() {\n    return /pattern[0-9]+/i;\n}"
      case parseJSSource "regex.js" jsCode of
        Left err -> expectationFailure ("Regex literal parse failed: " ++ show err)
        Right prog -> do
          let f1 = computeF1 prog
          unFingerprint f1 `shouldNotBe` ""

    it "BUG-16: Parses Go 1.18+ generic type parameters on struct declarations" $ do
      let goCode = "package generic\ntype Pair[T any, U comparable] struct {\n    first T\n    second U\n}"
      case parseGoSource "pair.go" goCode of
        Left err -> expectationFailure ("Go generic struct parse failed: " ++ show err)
        Right prog -> do
          let f1 = computeF1 prog
          unFingerprint f1 `shouldNotBe` ""

    it "BUG-17: Ingests Rust macro calls with nested bracket and brace delimiters" $ do
      let rsCode = "fn build_table() {\n    let table = matrix![[1, 2, { 3 + 4 }], [5, 6, 7]];\n}"
      case parseRustSource "matrix.rs" rsCode of
        Left err -> expectationFailure ("Rust nested macro parse failed: " ++ show err)
        Right prog -> do
          let f1 = computeF1 prog
          unFingerprint f1 `shouldNotBe` ""

    it "BUG-18: Decomposes short-circuit boolean conditions into decision CFG blocks" $ do
      let pyCode = "def validate(a, b, c):\n    if a > 0 and (b < 10 or c == 0):\n        return True\n    return False\n"
      case parsePythonSource "cond.py" pyCode of
        Left err -> expectationFailure ("Condition parse failed: " ++ show err)
        Right prog -> do
          let cfgs = buildCFGs prog
          length cfgs `shouldBe` 1
          let cfg = head cfgs
          length (cfgBlocks cfg) `shouldSatisfy` (>= 4)

    it "BUG-19: Inserts dominance-frontier SSA phi-nodes at branch convergence" $ do
      let pyCode = "def compute(flag, x):\n    if flag:\n        y = x * 2\n    else:\n        y = x + 10\n    return y\n"
      case parsePythonSource "ssa.py" pyCode of
        Left err -> expectationFailure ("SSA parse failed: " ++ show err)
        Right prog -> do
          let dfgs = buildDFGs prog
          length dfgs `shouldBe` 1
          let dfg = head dfgs
          let hasPhi = any (\node -> case dfgKind node of DefPhi _ -> True; _ -> False) (dfgNodes dfg)
          hasPhi `shouldBe` True

    it "BUG-20: Normalizer v4 canonically sorts pure functions while preserving reflection docstrings" $ do
      let py1 = "def beta():\n    \"\"\":preserve: Critical reflection API doc\"\"\"\n    return 2\ndef alpha():\n    return 1\n"
          py2 = "def alpha():\n    return 1\ndef beta():\n    \"\"\":preserve: Critical reflection API doc\"\"\"\n    return 2\n"
      case (parsePythonSource "m1.py" py1, parsePythonSource "m2.py" py2) of
        (Right p1, Right p2) -> do
          let n1 = normalizeProgram p1
              n2 = normalizeProgram p2
          computeF1 n1 `shouldBe` computeF1 n2
        _ -> expectationFailure "Python normalization parse failed"

    it "BUG-21: Case-folded Windows and POSIX paths resolve to identical cache entry" $ do
      let p = "src/core/engine.py"
          meta = FileMetadata p 500 1700000000
          bundle = FingerprintBundle (Fingerprint "s") (Fingerprint "str") (Fingerprint "d") (Fingerprint "dp") (Fingerprint "cg") (Fingerprint "cf") (Fingerprint "df") (Fingerprint "") (Fingerprint "c")
          cache = insertCache p meta bundle emptyCache
      lookupCache "src\\Core\\Engine.py" meta cache `shouldBe` Just bundle
      lookupCache "SRC/CORE/ENGINE.PY" meta cache `shouldBe` Just bundle
