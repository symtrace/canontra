{- |
Module      : Canontra.CallGraphSpec
Description : Unit test specification for the static intra-module call graph engine.

Tests call graph edge extraction, caller-callee categorization,
method dispatch analysis, and F_CG fingerprint determinism.
-}
{-# LANGUAGE OverloadedStrings #-}
module Canontra.CallGraphSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Canontra.Analysis.CallGraph
import Canontra.Fingerprint.CallGraph (computeFCG)
import Canontra.Parser.Python (parsePythonSource)

spec :: Spec
spec = do
  describe "Call Graph Analysis" $ do
    it "extracts intra-module caller-callee edges" $ do
      let code = T.unlines
            [ "def helper(x):"
            , "    return x * 2"
            , ""
            , "def main_func():"
            , "    a = helper(10)"
            , "    b = helper(20)"
            , "    return a + b"
            ]
      case parsePythonSource "test.py" code of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let cg = buildCallGraph prog
          CallFunction "main_func" `elem` cgNodes cg `shouldBe` True
          CallFunction "helper" `elem` cgNodes cg `shouldBe` True
          let hasMainToHelper = any (\e -> edgeCaller e == CallFunction "main_func" && edgeCallee e == TargetLocal "helper") (cgEdges cg)
          hasMainToHelper `shouldBe` True

    it "identifies imported module calls" $ do
      let code = T.unlines
            [ "import math"
            , "from os import path"
            , ""
            , "def compute(x):"
            , "    r = math.sqrt(x)"
            , "    p = path.exists('/tmp')"
            , "    return r"
            ]
      case parsePythonSource "test.py" code of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let cg = buildCallGraph prog
          let hasMathSqrt = any (\e -> edgeCallee e == TargetImported "math" "sqrt") (cgEdges cg)
          hasMathSqrt `shouldBe` True

    it "produces deterministic F_CG call graph fingerprints" $ do
      let code = T.unlines
            [ "def alpha(): beta()"
            , "def beta(): gamma()"
            , "def gamma(): pass"
            ]
      case (parsePythonSource "1.py" code, parsePythonSource "2.py" code) of
        (Right p1, Right p2) -> do
          computeFCG p1 `shouldBe` computeFCG p2
        _ -> expectationFailure "Parse failed"
