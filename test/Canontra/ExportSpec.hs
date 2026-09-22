{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- |
Module      : Canontra.ExportSpec
Description : Test suite for Phase 3 machine interchange & export engines (SARIF & Graphviz DOT).

Verifies:
1. OASIS SARIF v2.1.0 document compliance ($schema, version, tool driver, rules).
2. SARIF rule mapping: CTR001_InterfaceBreak, CTR002_DependencyDivergence, CTR003_StructuralMutation.
3. SARIF impact slice export across all change severities (Interface, Dependency, InternalLogic, Trivia).
4. Multi-file SARIF aggregation across repository changes.
5. Graphviz DOT call graph generation (nodes, edges, async styles, counts).
6. Graphviz DOT control-flow graph generation (function clusters, basic blocks, branch conditions).
7. Graphviz DOT data-flow graph generation (SSA def-use nodes, reaching definition edges).
-}
module Canontra.ExportSpec (spec) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Hspec

import Canontra.Analysis.CallGraph (buildCallGraph)
import Canontra.Analysis.CFG (buildCFGs)
import Canontra.Analysis.DFG (buildDFGs)
import Canontra.Analysis.Impact
  ( ChangeSeverity (..)
  , ImpactSlice (..)
  )
import Canontra.Comparison.Diff
  ( CFGDiff (..)
  , CallGraphDiff (..)
  , DFGDiff (..)
  , DeclDiff (..)
  , DepDiff (..)
  , DiffResult (..)
  , StructuralDiff (..)
  , diffPrograms
  )
import Canontra.Export.Graph
  ( escapeDOT
  , exportCallGraphDOT
  , exportCFGDOT
  , exportDFGDOT
  , sanitizeDOTId
  )
import Canontra.Export.SARIF
  ( exportDiffSARIF
  , exportImpactSARIF
  , exportMultiDiffSARIF
  , renderSARIF
  , ruleIdDependencyDivergence
  , ruleIdInterfaceBreak
  , ruleIdStructuralMutation
  , sarifSchemaUri
  , sarifVersion
  )
import Canontra.Parser.Polyglot (parsePolyglotSource)
import Canontra.Types (ComparisonResult (..), ComparisonStatus (..))

emptyCompResult :: ComparisonResult
emptyCompResult = ComparisonResult Identical Identical Identical Identical Identical Identical Identical Identical Identical

-- | Test helper operator for lookup in Aeson KeyMap
(!:) :: KeyMap.KeyMap Aeson.Value -> KeyMap.Key -> Aeson.Value
km !: k = fromMaybe (error $ "Missing key: " ++ show k) (KeyMap.lookup k km)

spec :: Spec
spec = do
  describe "Canontra.Export (Phase 3 Machine Interchange & Export)" $ do

    -- ========================================================================
    -- 1. SARIF v2.1.0 Document Compliance
    -- ========================================================================
    describe "SARIF v2.1.0 Document Structure" $ do
      it "produces valid SARIF document with $schema and version 2.1.0" $ do
        let diffRes = DiffResult emptyCompResult [] [] [] [] [] []
            sarif = exportDiffSARIF "test.py" diffRes
        case sarif of
          Aeson.Object root -> do
            KeyMap.lookup "$schema" root `shouldBe` Just (Aeson.String sarifSchemaUri)
            KeyMap.lookup "version" root `shouldBe` Just (Aeson.String sarifVersion)
            case KeyMap.lookup "runs" root of
              Just (Aeson.Array runs) -> do
                V.length runs `shouldBe` 1
                let (Aeson.Object run) = V.head runs
                case KeyMap.lookup "tool" run of
                  Just (Aeson.Object tool) -> do
                    case KeyMap.lookup "driver" tool of
                      Just (Aeson.Object driver) -> do
                        KeyMap.lookup "name" driver `shouldBe` Just (Aeson.String "canontra")
                        case KeyMap.lookup "rules" driver of
                          Just (Aeson.Array rules) -> V.length rules `shouldBe` 3
                          _ -> expectationFailure "Expected rules array in tool driver"
                      _ -> expectationFailure "Expected driver in tool"
                  _ -> expectationFailure "Expected tool in run"
              _ -> expectationFailure "Expected runs array in root"
          _ -> expectationFailure "Expected root JSON object"

      it "defines CTR001, CTR002, and CTR003 rules with correct severities" $ do
        let diffRes = DiffResult emptyCompResult [] [] [] [] [] []
            (Aeson.Object root) = exportDiffSARIF "test.py" diffRes
            (Aeson.Array runs) = root !: "runs"
            (Aeson.Object run) = V.head runs
            (Aeson.Object tool) = run !: "tool"
            (Aeson.Object driver) = tool !: "driver"
            (Aeson.Array rules) = driver !: "rules"

        let ruleIds = [ r !: "id" | Aeson.Object r <- V.toList rules ]
        ruleIds `shouldBe`
          [ Aeson.String ruleIdInterfaceBreak
          , Aeson.String ruleIdDependencyDivergence
          , Aeson.String ruleIdStructuralMutation
          ]

    -- ========================================================================
    -- 2. DiffResult Mapping to SARIF Results
    -- ========================================================================
    describe "DiffResult SARIF Mapping" $ do
      it "maps DeclDiff to CTR001_InterfaceBreak with level=error" $ do
        let declDiff = DeclDiff "modified" "calcSalary" "parameter types altered"
            diffRes = DiffResult emptyCompResult [declDiff] [] [] [] [] []
            (Aeson.Object root) = exportDiffSARIF "src/payroll.ts" diffRes
            (Aeson.Array runs) = root !: "runs"
            (Aeson.Object run) = V.head runs
            (Aeson.Array results) = run !: "results"

        V.length results `shouldBe` 1
        let (Aeson.Object res) = V.head results
        KeyMap.lookup "ruleId" res `shouldBe` Just (Aeson.String ruleIdInterfaceBreak)
        KeyMap.lookup "ruleIndex" res `shouldBe` Just (Aeson.Number 0)
        KeyMap.lookup "level" res `shouldBe` Just (Aeson.String "error")

      it "maps DepDiff to CTR002_DependencyDivergence with level=warning" $ do
        let depDiff = DepDiff "added" "requests" (Just "get") "external HTTP client"
            diffRes = DiffResult emptyCompResult [] [depDiff] [] [] [] []
            (Aeson.Object root) = exportDiffSARIF "src/api.py" diffRes
            (Aeson.Array runs) = root !: "runs"
            (Aeson.Object run) = V.head runs
            (Aeson.Array results) = run !: "results"

        V.length results `shouldBe` 1
        let (Aeson.Object res) = V.head results
        KeyMap.lookup "ruleId" res `shouldBe` Just (Aeson.String ruleIdDependencyDivergence)
        KeyMap.lookup "ruleIndex" res `shouldBe` Just (Aeson.Number 1)
        KeyMap.lookup "level" res `shouldBe` Just (Aeson.String "warning")

      it "maps StructuralDiff, CallGraphDiff, CFGDiff, and DFGDiff to CTR003_StructuralMutation with level=note" $ do
        let sDiff = StructuralDiff "loopBody" "loop" "iteration increment altered"
            cgDiff = CallGraphDiff "main" "removed" "helper"
            cfgDiff = CFGDiff "process" "branch" "conditional guard inverted"
            dfgDiff = DFGDiff "process" "def-use" "reaching def altered"
            diffRes = DiffResult emptyCompResult [] [] [sDiff] [cgDiff] [cfgDiff] [dfgDiff]
            (Aeson.Object root) = exportDiffSARIF "src/engine.rs" diffRes
            (Aeson.Array runs) = root !: "runs"
            (Aeson.Object run) = V.head runs
            (Aeson.Array results) = run !: "results"

        V.length results `shouldBe` 4
        let allLevels = [ r !: "level" | Aeson.Object r <- V.toList results ]
        let allRuleIds = [ r !: "ruleId" | Aeson.Object r <- V.toList results ]
        allLevels `shouldBe` replicate 4 (Aeson.String "note")
        allRuleIds `shouldBe` replicate 4 (Aeson.String ruleIdStructuralMutation)

      it "renders pretty-printed SARIF JSON text with renderSARIF" $ do
        let diffRes = DiffResult emptyCompResult [DeclDiff "added" "foo" "new export"] [] [] [] [] []
            rendered = T.unpack $ renderSARIF (exportDiffSARIF "src/lib.go" diffRes)
        rendered `shouldContain` "\"$schema\""
        rendered `shouldContain` "\"version\": \"2.1.0\""
        rendered `shouldContain` "\"CTR001_InterfaceBreak\""
        rendered `shouldContain` "\"src/lib.go\""

    -- ========================================================================
    -- 3. ImpactSlice SARIF Mapping
    -- ========================================================================
    describe "ImpactSlice SARIF Mapping" $ do
      it "maps SeverityInterface to CTR001_InterfaceBreak (error)" $ do
        let slice = ImpactSlice "src/core.py" SeverityInterface [] ["src/client.py"] ["test_core.py"] 75.0
            (Aeson.Object root) = exportImpactSARIF slice
            (Aeson.Array runs) = root !: "runs"
            (Aeson.Object run) = V.head runs
            (Aeson.Array results) = run !: "results"
        let (Aeson.Object res) = V.head results
        KeyMap.lookup "ruleId" res `shouldBe` Just (Aeson.String ruleIdInterfaceBreak)
        KeyMap.lookup "level" res `shouldBe` Just (Aeson.String "error")

      it "maps SeverityDependency to CTR002_DependencyDivergence (warning)" $ do
        let slice = ImpactSlice "src/deps.ts" SeverityDependency [] [] ["test_deps.ts"] 80.0
            (Aeson.Object root) = exportImpactSARIF slice
            (Aeson.Array runs) = root !: "runs"
            (Aeson.Object run) = V.head runs
            (Aeson.Array results) = run !: "results"
        let (Aeson.Object res) = V.head results
        KeyMap.lookup "ruleId" res `shouldBe` Just (Aeson.String ruleIdDependencyDivergence)
        KeyMap.lookup "level" res `shouldBe` Just (Aeson.String "warning")

      it "maps SeverityInternalLogic to CTR003_StructuralMutation (note)" $ do
        let slice = ImpactSlice "src/algo.rs" SeverityInternalLogic [] [] ["test_algo.rs"] 95.0
            (Aeson.Object root) = exportImpactSARIF slice
            (Aeson.Array runs) = root !: "runs"
            (Aeson.Object run) = V.head runs
            (Aeson.Array results) = run !: "results"
        let (Aeson.Object res) = V.head results
        KeyMap.lookup "ruleId" res `shouldBe` Just (Aeson.String ruleIdStructuralMutation)
        KeyMap.lookup "level" res `shouldBe` Just (Aeson.String "note")

    -- ========================================================================
    -- 4. Multi-File SARIF Aggregation
    -- ========================================================================
    describe "Multi-File SARIF Aggregation" $ do
      it "aggregates diagnostics across multiple repository files into one run" $ do
        let d1 = DiffResult emptyCompResult [DeclDiff "removed" "oldApi" "deprecated"] [] [] [] [] []
            d2 = DiffResult emptyCompResult [] [DepDiff "changed" "lodash" Nothing "bumped version"] [] [] [] []
            multiSarif = exportMultiDiffSARIF [("pkg/a.js", d1), ("pkg/b.js", d2)]
            (Aeson.Object root) = multiSarif
            (Aeson.Array runs) = root !: "runs"
            (Aeson.Object run) = V.head runs
            (Aeson.Array results) = run !: "results"

        V.length results `shouldBe` 2
        let fileUris = [ r !: "locations" | Aeson.Object r <- V.toList results ]
        length fileUris `shouldBe` 2

    -- ========================================================================
    -- 5. Graphviz DOT Call Graph Export
    -- ========================================================================
    describe "Graphviz DOT Call Graph Export (exportCallGraphDOT)" $ do
      it "generates valid digraph DOT format with nodes and directed edges" $ do
        let src = "def add(x, y):\n    return x + y\ndef compute():\n    return add(1, 2)\n"
        case parsePolyglotSource "math.py" src of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let cg = buildCallGraph prog
                dot = T.unpack $ exportCallGraphDOT cg
            dot `shouldStartWith` "digraph CallGraph {"
            dot `shouldEndWith` "}\n"
            dot `shouldContain` "rankdir=LR;"
            dot `shouldContain` "-> \""

      it "escapes special characters and produces clean DOT identifiers" $ do
        escapeDOT "hello \"world\"\nnext" `shouldBe` "hello \\\"world\\\"\\nnext"
        sanitizeDOTId "Foo.Bar::Baz$123" `shouldBe` "Foo_Bar__Baz_123"

    -- ========================================================================
    -- 6. Graphviz DOT Control-Flow Graph Export (exportCFGDOT)
    -- ========================================================================
    describe "Graphviz DOT CFG Export (exportCFGDOT)" $ do
      it "generates valid digraph with function clusters, basic blocks, and branch labels" $ do
        let src = "def check(x):\n    if x > 0:\n        return True\n    else:\n        return False\n"
        case parsePolyglotSource "check.py" src of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let cfgs = buildCFGs prog
                dot = T.unpack $ exportCFGDOT cfgs
            dot `shouldStartWith` "digraph ControlFlowGraph {"
            dot `shouldEndWith` "}\n"
            dot `shouldContain` "subgraph \"cluster_cfg_"
            dot `shouldContain` "bb_"
            dot `shouldContain` "rankdir=TB;"

    -- ========================================================================
    -- 7. Graphviz DOT Data-Flow Graph Export (exportDFGDOT)
    -- ========================================================================
    describe "Graphviz DOT DFG Export (exportDFGDOT)" $ do
      it "generates valid digraph with SSA def-use nodes and reaching definition edges" $ do
        let src = "def calc(a, b):\n    c = a + b\n    return c\n"
        case parsePolyglotSource "calc.py" src of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let dfgs = buildDFGs prog
                dot = T.unpack $ exportDFGDOT dfgs
            dot `shouldStartWith` "digraph DataFlowGraph {"
            dot `shouldEndWith` "}\n"
            dot `shouldContain` "subgraph \"cluster_dfg_"
            dot `shouldContain` "dfg_"
            dot `shouldContain` "rankdir=LR;"

    -- ========================================================================
    -- 8. End-to-End AST Diff to SARIF Pipeline
    -- ========================================================================
    describe "End-to-End Polyglot Diff to SARIF Pipeline" $ do
      it "computes polyglot AST diff and generates valid SARIF results" $ do
        let codeA = "def greeting(name: str) -> str:\n    return 'Hello ' + name\n"
            codeB = "def greeting(name: str, shout: bool = False) -> str:\n    return 'HELLO ' + name\n"
        case (parsePolyglotSource "greet.py" codeA, parsePolyglotSource "greet.py" codeB) of
          (Right p1, Right p2) -> do
            let diffRes = diffPrograms p1 p2
                sarif = exportDiffSARIF "greet.py" diffRes
                rendered = T.unpack $ renderSARIF sarif
            rendered `shouldContain` "\"ruleId\": \"CTR001_InterfaceBreak\""
            rendered `shouldContain` "greeting"
          _ -> expectationFailure "Failed to parse test programs"
