{- |
Module      : Canontra.DiffSpec
Description : Unit test specification for the structural diff diagnostics engine.

Tests declaration diffs, dependency diffs, structural logic diffs,
and call graph topological shift detection.
-}
{-# LANGUAGE OverloadedStrings #-}
module Canontra.DiffSpec (spec) where

import Test.Hspec

import Canontra.Comparison.Diff
import Canontra.Parser.Python (parsePythonSource)
import Canontra.Types

spec :: Spec
spec = do
  describe "Structural Diff Diagnostics" $ do
    it "detects declaration additions and removals" $ do
      let code1 = "def fn_one(): pass\n"
          code2 = "def fn_two(): pass\n"
      case (parsePythonSource "1.py" code1, parsePythonSource "2.py" code2) of
        (Right p1, Right p2) -> do
          let diffRes = diffPrograms p1 p2
          crDeclaration (drComparison diffRes) `shouldBe` Different
          let dDiffs = drDeclarationDiffs diffRes
          any (\d -> ddAction d == "removed" && ddTarget d == "Function: fn_one") dDiffs `shouldBe` True
          any (\d -> ddAction d == "added" && ddTarget d == "Function: fn_two") dDiffs `shouldBe` True
        _ -> expectationFailure "Parse failed"

    it "detects internal function body logic changes while declarations remain identical" $ do
      let code1 = "def calc(a, b):\n    return a + b\n"
          code2 = "def calc(a, b):\n    return a * b\n"
      case (parsePythonSource "1.py" code1, parsePythonSource "2.py" code2) of
        (Right p1, Right p2) -> do
          let diffRes = diffPrograms p1 p2
          crDeclaration (drComparison diffRes) `shouldBe` Identical
          crStructural (drComparison diffRes) `shouldBe` Different
          let sDiffs = drStructuralDiffs diffRes
          length sDiffs `shouldBe` 1
          sdKind (head sDiffs) `shouldBe` "body_logic_modified"
        _ -> expectationFailure "Parse failed"

    it "detects call graph edge additions" $ do
      let code1 = "def helper(): pass\ndef main(): pass\n"
          code2 = "def helper(): pass\ndef main(): helper()\n"
      case (parsePythonSource "1.py" code1, parsePythonSource "2.py" code2) of
        (Right p1, Right p2) -> do
          let diffRes = diffPrograms p1 p2
          crCallGraph (drComparison diffRes) `shouldBe` Different
          let cgDiffs = drCallGraphDiffs diffRes
          any (\d -> cgdAction d == "edge_added" && cgdCaller d == "main" && cgdCallee d == "helper") cgDiffs `shouldBe` True
        _ -> expectationFailure "Parse failed"
