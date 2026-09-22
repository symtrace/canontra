{-# LANGUAGE OverloadedStrings #-}
module Canontra.CFGSpec (spec) where

import Test.Hspec

import Canontra.Analysis.CFG
import Canontra.Fingerprint.ControlFlow (computeFCF)
import Canontra.Parser.Python (parsePythonSource)
import Canontra.Types

spec :: Spec
spec = do
  describe "Control-Flow Graph Analysis" $ do
    it "partitions simple linear functions into entry and exit basic blocks" $ do
      let pyCode = "def add(a, b):\n    total = a + b\n    return total\n"
      case parsePythonSource "test.py" pyCode of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let cfgs = buildCFGs prog
          length cfgs `shouldBe` 1
          let cfg = head cfgs
          cfgFunction cfg `shouldBe` "add"
          null (cfgBlocks cfg) `shouldBe` False

    it "creates branch edges and basic blocks for conditional if-else statements" $ do
      let pyCode = "def check(x):\n    if x > 0:\n        return 1\n    else:\n        return -1\n"
      case parsePythonSource "test.py" pyCode of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let cfgs = buildCFGs prog
          length cfgs `shouldBe` 1
          let cfg = head cfgs
          length (cfgEdges cfg) `shouldSatisfy` (>= 2)

    it "creates loop back-edges for while loops" $ do
      let pyCode = "def count_up(n):\n    i = 0\n    while i < n:\n        i = i + 1\n    return i\n"
      case parsePythonSource "test.py" pyCode of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let cfgs = buildCFGs prog
          length cfgs `shouldBe` 1
          let cfg = head cfgs
          length (cfgEdges cfg) `shouldSatisfy` (>= 3)

    it "produces deterministic F_CF control-flow fingerprints" $ do
      let pyCode1 = "def calc(x):\n    if x > 0:\n        return x * 2\n    return 0\n"
      let pyCode2 = "# Comment\ndef calc(x):\n    '''Docstring'''\n    if x > 0:\n        return x * 2\n    return 0\n"
      case (parsePythonSource "t1.py" pyCode1, parsePythonSource "t2.py" pyCode2) of
        (Right p1, Right p2) -> do
          let fcf1 = computeFCF p1
          let fcf2 = computeFCF p2
          unFingerprint fcf1 `shouldBe` unFingerprint fcf2
        _ -> expectationFailure "Parse failed"

    it "decomposes short-circuit boolean operators into intermediate decision blocks" $ do
      let pyCode = "def test_short_circuit(a, b):\n    if a and b:\n        return 1\n    return 0\n"
      case parsePythonSource "circuit.py" pyCode of
        Left err -> expectationFailure (show err)
        Right prog -> case buildCFGs prog of
          [cfg] -> do
            -- Should have at least 4 edges (a -> b [true], a -> else [false], b -> then [true], b -> else [false])
            length (cfgEdges cfg) `shouldSatisfy` (>= 4)
            -- At least 3 basic blocks (eval a, eval b, returns)
            length (cfgBlocks cfg) `shouldSatisfy` (>= 3)
          _ -> expectationFailure "Expected 1 CFG"

    it "models sound exception unwinding topology for try-except-finally" $ do
      let pyCode = "def safe_run(f):\n    try:\n        f()\n    except Exception:\n        log_err()\n    finally:\n        cleanup()\n"
      case parsePythonSource "try.py" pyCode of
        Left err -> expectationFailure (show err)
        Right prog -> case buildCFGs prog of
          [cfg] -> do
            -- Contains exception edge to handler and unwind edge to finally
            let excEdges = [e | e <- cfgEdges cfg, case edgeCondition e of CondException _ -> True; _ -> False]
            length excEdges `shouldSatisfy` (>= 1)
            null (cfgBlocks cfg) `shouldBe` False
          _ -> expectationFailure "Expected 1 CFG"
