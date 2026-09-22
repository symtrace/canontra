{-# LANGUAGE OverloadedStrings #-}
module Canontra.DFGSpec (spec) where

import Test.Hspec

import Canontra.Analysis.DFG
import Canontra.Fingerprint.DataFlow (computeFDF)
import Canontra.Parser.Python (parsePythonSource)
import Canontra.Types

spec :: Spec
spec = do
  describe "Data-Flow Graph Analysis" $ do
    it "extracts parameter definitions and variable assignment flows" $ do
      let pyCode = "def process(x, y):\n    z = x + y\n    return z\n"
      case parsePythonSource "test.py" pyCode of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let dfgs = buildDFGs prog
          length dfgs `shouldBe` 1
          let dfg = head dfgs
          dfgFunction dfg `shouldBe` "process"
          null (dfgNodes dfg) `shouldBe` False
          length (dfgEdges dfg) `shouldSatisfy` (>= 2)

    it "tracks Def-Use chains across sequential operations" $ do
      let pyCode = "def transform(a):\n    b = a * 2\n    c = b + 10\n    return c\n"
      case parsePythonSource "test.py" pyCode of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let dfgs = buildDFGs prog
          length dfgs `shouldBe` 1
          let dfg = head dfgs
          length (dfgEdges dfg) `shouldSatisfy` (>= 3)

    it "produces deterministic F_DF data-flow fingerprints" $ do
      let pyCode1 = "def calc(a, b):\n    return a + b\n"
      let pyCode2 = "# Different comment\ndef calc(a, b):\n    '''Docstring'''\n    return a + b\n"
      case (parsePythonSource "t1.py" pyCode1, parsePythonSource "t2.py" pyCode2) of
        (Right p1, Right p2) -> do
          let fdf1 = computeFDF p1
          let fdf2 = computeFDF p2
          unFingerprint fdf1 `shouldBe` unFingerprint fdf2
        _ -> expectationFailure "Parse failed"

    it "inserts SSA phi-nodes at branch convergence points" $ do
      let pyCode = "def branch_val(cond, x, y):\n    if cond:\n        res = x * 2\n    else:\n        res = y * 3\n    return res\n"
      case parsePythonSource "branch.py" pyCode of
        Left err -> expectationFailure (show err)
        Right prog -> case buildDFGs prog of
          [dfg] -> do
            -- Must contain a DefPhi node merging branch definitions
            let phiNodes = [n | n <- dfgNodes dfg, case dfgKind n of DefPhi _ -> True; _ -> False]
            length phiNodes `shouldSatisfy` (>= 1)
            case head phiNodes of
              DFGNode _ (DefPhi inDefs) _ -> length inDefs `shouldSatisfy` (>= 2)
              _                          -> expectationFailure "Expected DefPhi"
          _ -> expectationFailure "Expected 1 DFG"

    it "tracks block-scoped variable shadowing and resolves active definitions across branches" $ do
      let pyCode = "def shadow_test(x):\n    val = 1\n    if x > 0:\n        val = 10\n    return val\n"
      case parsePythonSource "shadow.py" pyCode of
        Left err -> expectationFailure (show err)
        Right prog -> case buildDFGs prog of
          [dfg] -> do
            -- Contains phi node merging val=1 and val=10
            let phiNodes = [n | n <- dfgNodes dfg, case dfgKind n of DefPhi _ -> True; _ -> False]
            length phiNodes `shouldSatisfy` (>= 1)
          _ -> expectationFailure "Expected 1 DFG"
