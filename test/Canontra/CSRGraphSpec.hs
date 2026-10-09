{-# LANGUAGE OverloadedStrings #-}
module Canontra.CSRGraphSpec (spec) where

import Data.Bits ((.|.))
import qualified Data.Vector.Unboxed as U
import Test.Hspec

import Canontra.Analysis.CSRGraph
import Canontra.Analysis.CompactGraph (packCFGEdges, packDFGEdges)
import Canontra.Analysis.WholeRepoGraph
  ( GlobalSymbol (..)
  , WholeRepoCallEdge (..)
  , WholeRepoCallGraph (..)
  , buildCSRCallGraph
  , buildCSRDataFlow
  , findDeadSymbols
  , toCSRCallGraph
  )
import Canontra.Parser.Polyglot (parsePolyglotSource)
import Canontra.Types (DeclKind (..), Fingerprint (..), WholeRepoDataFlowGraph (..))

spec :: Spec
spec = do
  describe "Canontra.Analysis.CSRGraph: Unboxed Compressed Sparse Row Graph Engine" $ do

    describe "Step 1.1: Core Representation & CSR Matrix Layout" $ do
      it "constructs empty CSRGraph with sound zero invariants" $ do
        let g = emptyCSRGraph
        csrNodeCount g `shouldBe` 0
        csrEdgeCount g `shouldBe` 0
        U.toList (csrRowOffsets g) `shouldBe` [0]
        U.toList (csrColIndices g) `shouldBe` []
        U.toList (csrEdgeFlags g) `shouldBe` []
        csrOutDegree g 0 `shouldBe` 0
        csrHasEdge g 0 0 `shouldBe` False

      it "constructs single node graph with no edges" $ do
        let g = buildCSRGraph 1 []
        csrNodeCount g `shouldBe` 1
        csrEdgeCount g `shouldBe` 0
        U.toList (csrRowOffsets g) `shouldBe` [0, 0]
        csrOutDegree g 0 `shouldBe` 0
        csrHasEdge g 0 0 `shouldBe` False

      it "normalizes out-of-order edges into sorted row offsets" $ do
        -- Edges: 2 -> 0, 0 -> 2, 0 -> 1
        let rawEdges = [(2, 0, flagCallSync), (0, 2, flagCallAsync), (0, 1, flagCallSync)]
            g = buildCSRGraph 3 rawEdges
        csrNodeCount g `shouldBe` 3
        csrEdgeCount g `shouldBe` 3
        -- Node 0 has 2 edges, Node 1 has 0 edges, Node 2 has 1 edge
        U.toList (csrRowOffsets g) `shouldBe` [0, 2, 2, 3]
        -- Targets for Node 0 must be sorted: 1, 2
        U.toList (csrNeighborIndices g 0) `shouldBe` [1, 2]
        U.toList (csrNeighborIndices g 1) `shouldBe` []
        U.toList (csrNeighborIndices g 2) `shouldBe` [0]
        csrOutDegree g 0 `shouldBe` 2
        csrOutDegree g 1 `shouldBe` 0
        csrOutDegree g 2 `shouldBe` 1

      it "deduplicates parallel edges and combines flags bitwise" $ do
        let rawEdges =
              [ (0, 1, flagCallSync)
              , (0, 1, flagCrossModule)
              , (0, 1, flagCallAsync)
              ]
            g = buildCSRGraph 2 rawEdges
        csrNodeCount g `shouldBe` 2
        csrEdgeCount g `shouldBe` 1
        U.toList (csrNeighborIndices g 0) `shouldBe` [1]
        let expectedFlags = flagCallSync .|. flagCrossModule .|. flagCallAsync
        U.toList (csrNeighborFlags g 0) `shouldBe` [expectedFlags]

      it "executes binary search edge queries (csrHasEdge) in logarithmic time" $ do
        let g = buildCSRGraph 4 [(0, 1, flagNone), (0, 3, flagNone), (2, 0, flagNone)]
        csrHasEdge g 0 1 `shouldBe` True
        csrHasEdge g 0 3 `shouldBe` True
        csrHasEdge g 0 2 `shouldBe` False
        csrHasEdge g 2 0 `shouldBe` True
        csrHasEdge g 1 0 `shouldBe` False
        csrHasEdge g 3 0 `shouldBe` False
        csrHasEdge g 99 99 `shouldBe` False

      it "transposes directed edges in linear time (transposeCSR)" $ do
        -- 0 -> 1 -> 2
        let g = buildCSRGraph 3 [(0, 1, flagCallSync), (1, 2, flagCallAsync)]
            t = transposeCSR g
        csrNodeCount t `shouldBe` 3
        csrEdgeCount t `shouldBe` 2
        -- In transpose: 2 -> 1 -> 0
        U.toList (csrNeighborIndices t 2) `shouldBe` [1]
        U.toList (csrNeighborIndices t 1) `shouldBe` [0]
        U.toList (csrNeighborIndices t 0) `shouldBe` []
        csrHasEdge t 2 1 `shouldBe` True
        csrHasEdge t 1 0 `shouldBe` True
        csrHasEdge t 0 1 `shouldBe` False

    describe "Step 1.2: Linear Tarjan SCC & Canonical Condensation" $ do
      it "returns empty SCC list for empty graph" $ do
        tarjanSCC emptyCSRGraph `shouldBe` []
        let (condG, compMap) = condenseSCC emptyCSRGraph
        csrNodeCount condG `shouldBe` 0
        U.null compMap `shouldBe` True

      it "partitions acyclic DAG into singleton components" $ do
        -- 0 -> 1 -> 2
        let g = buildCSRGraph 3 [(0, 1, flagNone), (1, 2, flagNone)]
            sccs = tarjanSCC g
        sccs `shouldBe` [[0], [1], [2]]

      it "collapses 2-node cycle (0 <-> 1) into a single SCC" $ do
        let g = buildCSRGraph 2 [(0, 1, flagNone), (1, 0, flagNone)]
            sccs = tarjanSCC g
        sccs `shouldBe` [[0, 1]]

      it "collapses 3-node cycle with downstream leaf into 2 components" $ do
        -- Cycle: 0 -> 1 -> 2 -> 0; Leaf: 2 -> 3
        let g = buildCSRGraph 4 [(0, 1, flagNone), (1, 2, flagNone), (2, 0, flagNone), (2, 3, flagNone)]
            sccs = tarjanSCC g
        sccs `shouldBe` [[0, 1, 2], [3]]

      it "collapses two disjoint cycles independently" $ do
        -- Cycle 1: 0 <-> 1; Cycle 2: 2 <-> 3; Inter-cycle edge: 1 -> 2
        let g = buildCSRGraph 4 [(0, 1, flagNone), (1, 0, flagNone), (1, 2, flagNone), (2, 3, flagNone), (3, 2, flagNone)]
            sccs = tarjanSCC g
        sccs `shouldBe` [[0, 1], [2, 3]]

      it "Theorem 1: SCC Cycle Collapse Permutation Invariance" $ do
        -- Permuted edge inputs must produce identical canonical condensation
        let edgesOrder1 = [(0, 1, flagNone), (1, 2, flagNone), (2, 0, flagNone), (2, 3, flagNone)]
            edgesOrder2 = [(2, 3, flagNone), (2, 0, flagNone), (1, 2, flagNone), (0, 1, flagNone)]
            edgesOrder3 = [(1, 2, flagNone), (0, 1, flagNone), (2, 3, flagNone), (2, 0, flagNone)]
            g1 = buildCSRGraph 4 edgesOrder1
            g2 = buildCSRGraph 4 edgesOrder2
            g3 = buildCSRGraph 4 edgesOrder3
            (cond1, map1) = condenseSCC g1
            (cond2, map2) = condenseSCC g2
            (cond3, map3) = condenseSCC g3
        tarjanSCC g1 `shouldBe` tarjanSCC g2
        tarjanSCC g2 `shouldBe` tarjanSCC g3
        cond1 `shouldBe` cond2
        cond2 `shouldBe` cond3
        map1 `shouldBe` map2
        map2 `shouldBe` map3

      it "synthesizes a sound condensed DAG and computes topological ordering" $ do
        -- Cycle 0 <-> 1; Leaf 2; Edge (0 <-> 1) -> 2
        let g = buildCSRGraph 3 [(0, 1, flagNone), (1, 0, flagNone), (1, 2, flagNone)]
            (condDAG, compMap) = condenseSCC g
        csrNodeCount condDAG `shouldBe` 2
        csrEdgeCount condDAG `shouldBe` 1
        -- Supernode 0 has {0, 1}; Supernode 1 has {2}
        compMap U.! 0 `shouldBe` 0
        compMap U.! 1 `shouldBe` 0
        compMap U.! 2 `shouldBe` 1
        csrHasEdge condDAG 0 1 `shouldBe` True
        -- Condensed graph is acyclic; topological sort succeeds
        topologicalSortDAG condDAG `shouldBe` Just (U.fromList [0, 1])

      it "computes forward and backward reachability cones" $ do
        -- 0 -> 1 -> 2; 0 -> 3; 4 is disconnected
        let g = buildCSRGraph 5 [(0, 1, flagNone), (1, 2, flagNone), (0, 3, flagNone)]
            fwdMask = forwardReachabilityCone g [0]
            bwdMask = backwardReachabilityCone g [2]
        reachabilityConeNodes fwdMask `shouldBe` [0, 1, 2, 3]
        reachabilityConeNodes bwdMask `shouldBe` [0, 1, 2]

    describe "Step 1.3: Integration with Compact Graphs & WholeRepoGraph" $ do
      it "converts CompactCFG into unboxed CSRGraph" $ do
        let cfgEdges = [(0, 1), (1, 2), (1, 3)] :: [(Int, Int)]
            compact = packCFGEdges cfgEdges
            csr = fromCompactCFG 4 compact
        csrNodeCount csr `shouldBe` 4
        csrEdgeCount csr `shouldBe` 3
        csrHasEdge csr 0 1 `shouldBe` True
        csrHasEdge csr 1 2 `shouldBe` True
        csrHasEdge csr 1 3 `shouldBe` True
        csrHasEdge csr 1 0 `shouldBe` False

      it "converts CompactDFG into unboxed CSRGraph" $ do
        let dfgEdges = [(0, 2), (1, 2)] :: [(Int, Int)]
            compact = packDFGEdges dfgEdges
            csr = fromCompactDFG 3 compact
        csrNodeCount csr `shouldBe` 3
        csrEdgeCount csr `shouldBe` 2
        csrHasEdge csr 0 2 `shouldBe` True
        csrHasEdge csr 1 2 `shouldBe` True

      it "synthesizes CSR graph from WholeRepoCallGraph with identical topology" $ do
        let sA = GlobalSymbol "a.py" "a" "func_a" KindFunction (Fingerprint "f1")
            sB = GlobalSymbol "b.py" "b" "func_b" KindFunction (Fingerprint "f2")
            sC = GlobalSymbol "c.py" "c" "func_c" KindFunction (Fingerprint "f3")
            edgeAB = WholeRepoCallEdge sA sB 1 False True
            edgeBC = WholeRepoCallEdge sB sC 2 True True
            wcg = WholeRepoCallGraph [sA, sB, sC] [edgeAB, edgeBC] [[sA], [sB], [sC]]
            (csr, nodes) = toCSRCallGraph wcg
        csrNodeCount csr `shouldBe` 3
        csrEdgeCount csr `shouldBe` 2
        nodes `shouldBe` [sA, sB, sC]
        csrHasEdge csr 0 1 `shouldBe` True
        csrHasEdge csr 1 2 `shouldBe` True
        csrHasEdge csr 0 2 `shouldBe` False

      it "computes dead symbols via transposed CSR in-degree 0 queries" $ do
        let sRoot = GlobalSymbol "m.py" "m" "root" KindFunction (Fingerprint "1")
            sUsed = GlobalSymbol "m.py" "m" "used" KindFunction (Fingerprint "2")
            sDead = GlobalSymbol "m.py" "m" "dead" KindFunction (Fingerprint "3")
            edge = WholeRepoCallEdge sRoot sUsed 1 False False
            wcg = WholeRepoCallGraph [sRoot, sUsed, sDead] [edge] [[sRoot], [sUsed], [sDead]]
            deadSyms = findDeadSymbols wcg
        -- sUsed is called by sRoot -> not dead.
        -- sRoot is not called by anything -> dead (unless main/top-level/init).
        -- sDead is not called by anything -> dead.
        sDead `elem` deadSyms `shouldBe` True
        sUsed `elem` deadSyms `shouldBe` False

      it "builds dual WholeRepoCallGraph and CSRGraph on polyglot modules" $ do
        let modASrc = "import b\ndef run(): return b.helper()\n"
            modBSrc = "def helper(): return 42\n"
        case (parsePolyglotSource "a.py" modASrc, parsePolyglotSource "b.py" modBSrc) of
          (Right pA, Right pB) -> do
            let (wcg, csr) = buildCSRCallGraph [("a.py", pA), ("b.py", pB)]
            csrNodeCount csr `shouldSatisfy` (>= 2)
            csrEdgeCount csr `shouldSatisfy` (>= 1)
            -- Both representations match
            length (wcgNodes wcg) `shouldBe` fromIntegral (csrNodeCount csr)
          _ -> expectationFailure "Parse failed"

      it "builds dual WholeRepoDataFlowGraph and CSRGraph on polyglot modules" $ do
        let modASrc = "import b\ndef run(x): return b.compute(x)\n"
            modBSrc = "def compute(n): return n * 2\n"
        case (parsePolyglotSource "a.py" modASrc, parsePolyglotSource "b.py" modBSrc) of
          (Right pA, Right pB) -> do
            let (wdf, csr) = buildCSRDataFlow [("a.py", pA), ("b.py", pB)]
            csrNodeCount csr `shouldSatisfy` (>= 2)
            csrEdgeCount csr `shouldSatisfy` (>= 1)
            length (wdfNodes wdf) `shouldBe` fromIntegral (csrNodeCount csr)
          _ -> expectationFailure "Parse failed"
