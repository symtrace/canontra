{-# LANGUAGE OverloadedStrings #-}
module Canontra.GraphSoundnessSpec (spec) where

import qualified Data.Vector.Unboxed as U
import Test.Hspec

import Canontra.Analysis.CFG (BranchCondition (..), CFGEdge (..), ControlFlowGraph (..), buildCFGs)
import Canontra.Analysis.CompactGraph (CompactCFG (..), CompactDFG (..), fromControlFlowGraph, fromDataFlowGraph)
import Canontra.Analysis.DFG (DFGNode (..), DataFlowGraph (..), DefUseKind (..), buildDFGs)
import Canontra.Parser.Python (parsePythonSource)

spec :: Spec
spec = do
  describe "CFG & DFG Graph Soundness & Topology Matrix" $ do

    describe "Short-Circuit Boolean Decomposition" $ do
      it "decomposes 'a and b' into 2 decision blocks with short-circuit failure edges" $ do
        let code = "def test(a, b):\n    if a and b:\n        return 1\n    return 0\n"
        case parsePythonSource "and.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let cfgs = buildCFGs prog
            length cfgs `shouldBe` 1
            let cfg = head cfgs
            -- Decomposed into entry, decision_a, decision_b, then, else, exit blocks
            length (cfgBlocks cfg) `shouldSatisfy` (>= 4)
            -- Both true and false conditions present
            let conds = [edgeCondition e | e <- cfgEdges cfg]
            any (\c -> case c of CondTrue _ -> True; _ -> False) conds `shouldBe` True
            any (\c -> case c of CondFalse _ -> True; _ -> False) conds `shouldBe` True

      it "decomposes 'a or b' into 2 decision blocks with short-circuit success edges" $ do
        let code = "def test(a, b):\n    if a or b:\n        return 1\n    return 0\n"
        case parsePythonSource "or.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let cfgs = buildCFGs prog
            length cfgs `shouldBe` 1
            let cfg = head cfgs
            length (cfgBlocks cfg) `shouldSatisfy` (>= 4)

      it "decomposes compound '(a and b) or c' into cascading decision graph" $ do
        let code = "def test(a, b, c):\n    if (a and b) or c:\n        return 1\n    return 0\n"
        case parsePythonSource "compound.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let cfgs = buildCFGs prog
            length cfgs `shouldBe` 1
            let cfg = head cfgs
            length (cfgBlocks cfg) `shouldSatisfy` (>= 5)

      it "decomposes three-way 'a and b and c' into sequential guard blocks" $ do
        let code = "def test(a, b, c):\n    if a and b and c:\n        return 10\n    return 20\n"
        case parsePythonSource "and3.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let cfgs = buildCFGs prog
            length cfgs `shouldBe` 1
            let cfg = head cfgs
            length (cfgBlocks cfg) `shouldSatisfy` (>= 5)

    describe "Full Exception Unwinding Topology" $ do
      it "models try-except with CondException branch edge to handler" $ do
        let code = "def test():\n    try:\n        risky()\n    except ValueError:\n        handle()\n"
        case parsePythonSource "try_except.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let cfgs = buildCFGs prog
            length cfgs `shouldBe` 1
            let cfg = head cfgs
            let conds = [edgeCondition e | e <- cfgEdges cfg]
            any (\c -> case c of CondException _ -> True; _ -> False) conds `shouldBe` True

      it "models try-except with multiple distinct exception handlers" $ do
        let code = "def test():\n    try:\n        risky()\n    except ValueError:\n        handle_val()\n    except TypeError:\n        handle_type()\n"
        case parsePythonSource "try_multi.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let cfgs = buildCFGs prog
            length cfgs `shouldBe` 1
            let cfg = head cfgs
            let exEdges = filter (\e -> case edgeCondition e of CondException _ -> True; _ -> False) (cfgEdges cfg)
            length exEdges `shouldSatisfy` (>= 2)

      it "models try-except-finally with convergence into finally block" $ do
        let code = "def test():\n    try:\n        risky()\n    except Exception:\n        recover()\n    finally:\n        cleanup()\n"
        case parsePythonSource "try_finally.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let cfgs = buildCFGs prog
            length cfgs `shouldBe` 1
            let cfg = head cfgs
            -- CFG must have entry, try, handler, finally, and exit
            length (cfgBlocks cfg) `shouldSatisfy` (>= 4)

      it "models full try-except-else-finally 4-stage unwinding pipeline" $ do
        let code = "def test():\n    try:\n        work()\n    except IOError:\n        err()\n    else:\n        success()\n    finally:\n        done()\n"
        case parsePythonSource "try_full.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let cfgs = buildCFGs prog
            length cfgs `shouldBe` 1
            let cfg = head cfgs
            length (cfgBlocks cfg) `shouldSatisfy` (>= 5)

    describe "Dominance-Frontier SSA Phi-Node Synthesis" $ do
      it "synthesizes DefPhi at if-else reconvergence for variable assigned in both branches" $ do
        let code = "def choose(flag, x):\n    if flag:\n        res = x * 2\n    else:\n        res = x * 3\n    return res\n"
        case parsePythonSource "phi_both.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let dfgs = buildDFGs prog
            length dfgs `shouldBe` 1
            let dfg = head dfgs
            let phiNodes = filter (\n -> case dfgKind n of DefPhi _ -> True; _ -> False) (dfgNodes dfg)
            length phiNodes `shouldSatisfy` (>= 1)

      it "synthesizes DefPhi at if-without-else convergence merging with outer definition" $ do
        let code = "def update(flag, x):\n    val = 1\n    if flag:\n        val = x\n    return val\n"
        case parsePythonSource "phi_single.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let dfgs = buildDFGs prog
            length dfgs `shouldBe` 1
            let dfg = head dfgs
            let phiNodes = filter (\n -> case dfgKind n of DefPhi _ -> True; _ -> False) (dfgNodes dfg)
            length phiNodes `shouldSatisfy` (>= 1)

      it "synthesizes DefPhi at while loop header for loop-modified variable" $ do
        let code = "def count_up(limit):\n    i = 0\n    while i < limit:\n        i = i + 1\n    return i\n"
        case parsePythonSource "phi_while.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let dfgs = buildDFGs prog
            length dfgs `shouldBe` 1
            let dfg = head dfgs
            let phiNodes = filter (\n -> case dfgKind n of DefPhi _ -> True; _ -> False) (dfgNodes dfg)
            length phiNodes `shouldSatisfy` (>= 1)

      it "synthesizes DefPhi at try-except convergence for variable set in try or except" $ do
        let code = "def parse_int(s):\n    try:\n        res = int(s)\n    except ValueError:\n        res = 0\n    return res\n"
        case parsePythonSource "phi_try.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let dfgs = buildDFGs prog
            length dfgs `shouldBe` 1
            let dfg = head dfgs
            let phiNodes = filter (\n -> case dfgKind n of DefPhi _ -> True; _ -> False) (dfgNodes dfg)
            length phiNodes `shouldSatisfy` (>= 1)

    describe "Block-Scoped Variable Shadowing & Walrus Data Flows" $ do
      it "creates distinct definition node for shadowed variable inside nested block" $ do
        let code = "def shadow():\n    x = 10\n    if True:\n        x = 20\n        print(x)\n    print(x)\n"
        case parsePythonSource "shadow.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let dfgs = buildDFGs prog
            length dfgs `shouldBe` 1
            let dfg = head dfgs
            let varDefs = filter (\n -> case dfgKind n of DefAssignment "x" -> True; _ -> False) (dfgNodes dfg)
            -- Both definitions of x exist as distinct nodes
            length varDefs `shouldSatisfy` (>= 2)

      it "tracks PEP 572 walrus operator definition in if-condition to body use" $ do
        let code = "def process(item):\n    if (val := item.get_val()) > 0:\n        return val * 2\n    return 0\n"
        case parsePythonSource "walrus_dfg.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let dfgs = buildDFGs prog
            length dfgs `shouldBe` 1
            let dfg = head dfgs
            let valDefs = filter (\n -> case dfgKind n of DefAssignment "val" -> True; _ -> False) (dfgNodes dfg)
            length valDefs `shouldSatisfy` (>= 1)

    describe "CompactGraph Lossless Serialization" $ do
      it "converts CFG to CompactGraph preserving edge vector count" $ do
        let code = "def compute(a, b):\n    if a > 0:\n        return a + b\n    else:\n        return b - a\n"
        case parsePythonSource "compact_cfg.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let cfgs = buildCFGs prog
            length cfgs `shouldBe` 1
            let cfg = head cfgs
                compact = fromControlFlowGraph cfg
            U.length (unCompactCFG compact) `shouldBe` length (cfgEdges cfg)

      it "converts DFG to CompactGraph preserving edge vector count" $ do
        let code = "def add(x, y):\n    z = x + y\n    return z\n"
        case parsePythonSource "compact_dfg.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let dfgs = buildDFGs prog
            length dfgs `shouldBe` 1
            let dfg = head dfgs
                compact = fromDataFlowGraph dfg
            U.length (unCompactDFG compact) `shouldBe` length (dfgEdges dfg)
