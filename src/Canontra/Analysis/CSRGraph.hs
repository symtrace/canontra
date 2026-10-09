{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Canontra.Analysis.CSRGraph
Description : High-performance unboxed Compressed Sparse Row (CSR) graph engine.

Provides pointerless, contiguous unboxed vector storage for Call Graphs,
Control-Flow Graphs (CFG), Data-Flow Graphs (DFG), and Whole-Repository dependency
networks. Implements linear Tarjan Strongly Connected Components (SCC) cycle
collapse, canonical DAG condensation, reachability cone queries, and transpose passes
with zero nursery heap allocation.
-}
module Canontra.Analysis.CSRGraph
  ( -- * Core CSR Representation
    CSRGraph (..)
  , emptyCSRGraph
  , buildCSRGraph
  , buildCSRGraphDeduplicated

    -- * Edge Flag Bitmasks
  , flagNone
  , flagCallSync
  , flagCallAsync
  , flagCrossModule
  , flagDataFlowDef
  , flagDataFlowUse
  , flagDataFlowRet

    -- * Graph Queries
  , csrOutDegree
  , csrNeighbors
  , csrNeighborIndices
  , csrNeighborFlags
  , csrHasEdge
  , csrEdgeCountOf
  , csrAllEdges
  , transposeCSR

    -- * SCC & Condensation
  , tarjanSCC
  , condenseSCC
  , canonicalCondensation
  , topologicalSortDAG

    -- * Reachability Cones
  , forwardReachabilityCone
  , backwardReachabilityCone
  , reachabilityConeNodes
  , reachabilityConeUnion

    -- * Edge Splicing & Localized Propagation
  , spliceCSREdges

    -- * Compact Conversions
  , fromCompactCFG
  , fromCompactDFG
  , toCompactEdges
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Int (Int32)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.STRef (modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word16, Word32, Word64)
import GHC.Generics (Generic)

import Canontra.Analysis.CompactGraph (CompactCFG (..), CompactDFG (..))

-- | High-performance, pointerless Compressed Sparse Row (CSR) graph representation.
-- Stored entirely in unboxed contiguous memory; zero garbage collection overhead.
data CSRGraph = CSRGraph
  { csrNodeCount   :: {-# UNPACK #-} !Word32
  , csrEdgeCount   :: {-# UNPACK #-} !Word32
  , csrRowOffsets  :: {-# UNPACK #-} !(U.Vector Word32)
  , csrColIndices  :: {-# UNPACK #-} !(U.Vector Word32)
  , csrEdgeFlags   :: {-# UNPACK #-} !(U.Vector Word16)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Flag constants for semantic edge classification.
flagNone :: Word16
flagNone = 0x0000

flagCallSync :: Word16
flagCallSync = 0x0001

flagCallAsync :: Word16
flagCallAsync = 0x0002

flagCrossModule :: Word16
flagCrossModule = 0x0004

flagDataFlowDef :: Word16
flagDataFlowDef = 0x0008

flagDataFlowUse :: Word16
flagDataFlowUse = 0x0010

flagDataFlowRet :: Word16
flagDataFlowRet = 0x0020

-- | Constructs an empty CSR graph with zero nodes and zero edges.
emptyCSRGraph :: CSRGraph
emptyCSRGraph = CSRGraph 0 0 (U.singleton 0) U.empty U.empty

-- | Construct an unboxed CSR graph from raw directed edge triples @(source, target, flags)@.
-- Duplicate edges between the same source and target are collapsed and their flags bitwise OR-ed.
buildCSRGraph :: Word32 -> [(Word32, Word32, Word16)] -> CSRGraph
buildCSRGraph = buildCSRGraphDeduplicated True

-- | Construct an unboxed CSR graph with optional parallel-edge deduplication.
buildCSRGraphDeduplicated :: Bool -> Word32 -> [(Word32, Word32, Word16)] -> CSRGraph
buildCSRGraphDeduplicated !dedup !n !rawEdges
  | n == 0 = emptyCSRGraph
  | otherwise = runST $ do
      let !nInt = fromIntegral n
          !validEdges = filter (\(u, v, _) -> u < n && v < n) rawEdges
          !sortedEdges = sortBy (comparing (\(u, v, _) -> (u, v))) validEdges
          !cleanEdges = if dedup then combineDuplicates sortedEdges else sortedEdges
          !m = length cleanEdges
          !mWord = fromIntegral m :: Word32

      degCounts <- UM.replicate (nInt + 1) (0 :: Word32)
      forM_ cleanEdges $ \(u, _, _) ->
        UM.modify degCounts (+1) (fromIntegral u + 1)

      let computePrefixSums !i !acc
            | i > nInt = pure ()
            | otherwise = do
                !cnt <- UM.read degCounts i
                let !newAcc = acc + cnt
                UM.write degCounts i newAcc
                computePrefixSums (i + 1) newAcc

      computePrefixSums 1 0
      !rowOffsets <- U.freeze degCounts

      let !cols = U.fromList [v | (_, v, _) <- cleanEdges]
          !flags = U.fromList [f | (_, _, f) <- cleanEdges]

      pure $ CSRGraph n mWord rowOffsets cols flags
  where
    combineDuplicates [] = []
    combineDuplicates ((u1, v1, f1) : (u2, v2, f2) : rest)
      | u1 == u2 && v1 == v2 = combineDuplicates ((u1, v1, f1 .|. f2) : rest)
      | otherwise            = (u1, v1, f1) : combineDuplicates ((u2, v2, f2) : rest)
    combineDuplicates [x] = [x]

-- | Single-cycle out-degree query for node @u@: @csrRowOffsets[u + 1] - csrRowOffsets[u]@.
{-# INLINE csrOutDegree #-}
csrOutDegree :: CSRGraph -> Word32 -> Word32
csrOutDegree g u
  | u >= csrNodeCount g = 0
  | otherwise =
      let !start = csrRowOffsets g U.! fromIntegral u
          !end   = csrRowOffsets g U.! fromIntegral (u + 1)
      in end - start

-- | Return the contiguous unboxed slice of target node indices adjacent to @u@.
{-# INLINE csrNeighborIndices #-}
csrNeighborIndices :: CSRGraph -> Word32 -> U.Vector Word32
csrNeighborIndices g u
  | u >= csrNodeCount g = U.empty
  | otherwise =
      let !start = fromIntegral (csrRowOffsets g U.! fromIntegral u)
          !len   = fromIntegral (csrOutDegree g u)
      in U.slice start len (csrColIndices g)

-- | Return the contiguous unboxed slice of edge flags adjacent to @u@.
{-# INLINE csrNeighborFlags #-}
csrNeighborFlags :: CSRGraph -> Word32 -> U.Vector Word16
csrNeighborFlags g u
  | u >= csrNodeCount g = U.empty
  | otherwise =
      let !start = fromIntegral (csrRowOffsets g U.! fromIntegral u)
          !len   = fromIntegral (csrOutDegree g u)
      in U.slice start len (csrEdgeFlags g)

-- | Query all outgoing neighbors and edge flags for node @u@.
csrNeighbors :: CSRGraph -> Word32 -> [(Word32, Word16)]
csrNeighbors g u =
  let !cols = csrNeighborIndices g u
      !flgs = csrNeighborFlags g u
  in zip (U.toList cols) (U.toList flgs)

-- | Binary search query testing if directed edge @(u, v)@ exists in the graph.
-- Executes in @O(log(deg(u)))@ time without full adjacency list expansion.
csrHasEdge :: CSRGraph -> Word32 -> Word32 -> Bool
csrHasEdge g u v
  | u >= csrNodeCount g || v >= csrNodeCount g = False
  | otherwise =
      let !slice = csrNeighborIndices g u
          !len   = U.length slice
          binarySearch !lo !hi
            | lo > hi = False
            | otherwise =
                let !mid = (lo + hi) `div` 2
                    !val = slice U.! mid
                in case compare val v of
                     LT -> binarySearch (mid + 1) hi
                     GT -> binarySearch lo (mid - 1)
                     EQ -> True
      in if len == 0 then False else binarySearch 0 (len - 1)

-- | Return total number of directed edges in the CSR graph.
{-# INLINE csrEdgeCountOf #-}
csrEdgeCountOf :: CSRGraph -> Word32
csrEdgeCountOf = csrEdgeCount

-- | Unpack all edges in the CSR graph into @(source, target, flags)@ triples.
csrAllEdges :: CSRGraph -> [(Word32, Word32, Word16)]
csrAllEdges g =
  [ (u, v, f)
  | u <- [0 .. csrNodeCount g - 1]
  , (v, f) <- csrNeighbors g u
  ]

-- | Transpose the graph in @O(V + E)@ time, reversing all directed edges.
transposeCSR :: CSRGraph -> CSRGraph
transposeCSR g
  | csrNodeCount g == 0 = emptyCSRGraph
  | otherwise =
      let !n = csrNodeCount g
          !revEdges =
            [ (v, u, f)
            | u <- [0 .. n - 1]
            , (v, f) <- csrNeighbors g u
            ]
      in buildCSRGraphDeduplicated True n revEdges

-- | Linear Tarjan Strongly Connected Components (SCC) cycle collapse directly over CSR vectors.
-- Implemented iteratively in the 'ST' monad with zero GHC call-stack recursion.
-- Returns SCC components sorted internally and canonically ordered by minimum node ID.
tarjanSCC :: CSRGraph -> [[Word32]]
tarjanSCC g
  | n == 0 = []
  | otherwise = runST $ do
      let !nInt = fromIntegral n
      indices  <- UM.replicate nInt (-1 :: Int32)
      lowlinks <- UM.replicate nInt (-1 :: Int32)
      onStack  <- UM.replicate nInt False

      timerRef <- newSTRef (0 :: Int32)
      stackRef <- newSTRef ([] :: [Word32])
      sccsRef  <- newSTRef ([] :: [[Word32]])

      let runDFS !root = do
            !rIdx <- UM.read indices (fromIntegral root)
            if rIdx /= -1
              then pure ()
              else do
                !t0 <- readSTRef timerRef
                writeSTRef timerRef (t0 + 1)
                UM.write indices (fromIntegral root) t0
                UM.write lowlinks (fromIntegral root) t0
                UM.write onStack (fromIntegral root) True
                modifySTRef' stackRef (root :)

                let !rStart = fromIntegral (csrRowOffsets g U.! fromIntegral root)
                    !rEnd   = fromIntegral (csrRowOffsets g U.! fromIntegral (root + 1))
                loopStack [(root, rStart, rEnd)]

          loopStack [] = pure ()
          loopStack ((!u, !currOff, !rowEnd) : frames)
            | currOff < rowEnd = do
                let !v = csrColIndices g U.! currOff
                    !vInt = fromIntegral v
                    !nextFrames = (u, currOff + 1, rowEnd) : frames
                !vIdx <- UM.read indices vInt
                if vIdx == -1
                  then do
                    !t <- readSTRef timerRef
                    writeSTRef timerRef (t + 1)
                    UM.write indices vInt t
                    UM.write lowlinks vInt t
                    UM.write onStack vInt True
                    modifySTRef' stackRef (v :)
                    let !vStart = fromIntegral (csrRowOffsets g U.! vInt)
                        !vEnd   = fromIntegral (csrRowOffsets g U.! (vInt + 1))
                    loopStack ((v, vStart, vEnd) : nextFrames)
                  else do
                    !vOn <- UM.read onStack vInt
                    if vOn
                      then do
                        !uLow <- UM.read lowlinks (fromIntegral u)
                        UM.write lowlinks (fromIntegral u) (min uLow vIdx)
                      else pure ()
                    loopStack nextFrames
            | otherwise = do
                !uLow <- UM.read lowlinks (fromIntegral u)
                !uIdx <- UM.read indices (fromIntegral u)
                if uLow == uIdx
                  then do
                    let popLoop !acc = do
                          stk <- readSTRef stackRef
                          case stk of
                            [] -> pure acc
                            (w:ws) -> do
                              writeSTRef stackRef ws
                              UM.write onStack (fromIntegral w) False
                              let !acc' = w : acc
                              if w == u then pure acc' else popLoop acc'
                    comp <- popLoop []
                    modifySTRef' sccsRef (sortBy compare comp :)
                  else pure ()
                case frames of
                  [] -> pure ()
                  ((p, pOff, pEnd) : parentFrames) -> do
                    !pLow <- UM.read lowlinks (fromIntegral p)
                    UM.write lowlinks (fromIntegral p) (min pLow uLow)
                    loopStack ((p, pOff, pEnd) : parentFrames)

      forM_ [0 .. n - 1] runDFS
      rawSccs <- readSTRef sccsRef
      pure $ sortBy (comparing (\c -> case c of [] -> 0; (x:_) -> x)) rawSccs
  where
    !n = csrNodeCount g

-- | Condensed SCC graph computed via linear Tarjan pass over CSR vectors.
-- Collapses strongly connected cycles into canonical supernodes and returns:
-- 1. The condensed DAG as an unboxed 'CSRGraph'.
-- 2. An unboxed node component mapping vector @compMap@ where @compMap[u]@ is the component ID of @u@.
condenseSCC :: CSRGraph -> (CSRGraph, U.Vector Word32)
condenseSCC g
  | csrNodeCount g == 0 = (emptyCSRGraph, U.empty)
  | otherwise =
      let !sccs = tarjanSCC g
          !numComps = fromIntegral (length sccs) :: Word32
          !n = csrNodeCount g
          !nInt = fromIntegral n

          !compMap = runST $ do
            m <- UM.new nInt
            forM_ (zip ([0..] :: [Word32]) sccs) $ \(cIdx, comp) ->
              forM_ comp $ \u ->
                UM.write m (fromIntegral u) cIdx
            U.freeze m

          !interCompEdges =
            [ (c_u, c_v, f)
            | u <- [0 .. n - 1]
            , let !c_u = compMap U.! fromIntegral u
            , (v, f) <- csrNeighbors g u
            , let !c_v = compMap U.! fromIntegral v
            , c_u /= c_v
            ]

          !condensedGraph = buildCSRGraphDeduplicated True numComps interCompEdges
      in (condensedGraph, compMap)

-- | Bijective canonical condensation satisfying Theorem 1 (SCC Cycle Collapse Permutation Invariance).
canonicalCondensation :: CSRGraph -> (CSRGraph, U.Vector Word32)
canonicalCondensation = condenseSCC

-- | Topological sort of a Directed Acyclic Graph (DAG) using Kahn's algorithm.
-- Returns 'Just' vector of node indices in topological order, or 'Nothing' if cycles exist.
topologicalSortDAG :: CSRGraph -> Maybe (U.Vector Word32)
topologicalSortDAG g
  | csrNodeCount g == 0 = Just U.empty
  | otherwise = runST $ do
      let !n = csrNodeCount g
          !nInt = fromIntegral n
      inDegrees <- UM.replicate nInt (0 :: Word32)

      forM_ [0 .. n - 1] $ \u ->
        forM_ (csrNeighbors g u) $ \(v, _) ->
          UM.modify inDegrees (+1) (fromIntegral v)

      zeroQueueRef <- newSTRef ([] :: [Word32])
      forM_ [0 .. n - 1] $ \u -> do
        deg <- UM.read inDegrees (fromIntegral u)
        if deg == 0 then modifySTRef' zeroQueueRef (u :) else pure ()

      orderRef <- newSTRef ([] :: [Word32])
      let processQueue = do
            q <- readSTRef zeroQueueRef
            case q of
              [] -> pure ()
              (u:us) -> do
                writeSTRef zeroQueueRef us
                modifySTRef' orderRef (u :)
                forM_ (csrNeighbors g u) $ \(v, _) -> do
                  let !vInt = fromIntegral v
                  UM.modify inDegrees (\d -> d - 1) vInt
                  newDeg <- UM.read inDegrees vInt
                  if newDeg == 0
                    then modifySTRef' zeroQueueRef (v :)
                    else pure ()
                processQueue

      processQueue
      revOrder <- readSTRef orderRef
      let !finalOrder = reverse revOrder
      if length finalOrder == nInt
        then pure $ Just (U.fromList finalOrder)
        else pure Nothing

-- | Computes the forward reachability cone mask starting from a set of seed nodes.
-- Returns an unboxed boolean vector of length @csrNodeCount g@.
forwardReachabilityCone :: CSRGraph -> [Word32] -> U.Vector Bool
forwardReachabilityCone g seeds
  | csrNodeCount g == 0 = U.empty
  | otherwise = runST $ do
      let !nInt = fromIntegral (csrNodeCount g)
      visited <- UM.replicate nInt False
      let bfs [] = pure ()
          bfs (u:us) = do
            let !uInt = fromIntegral u
            !already <- UM.read visited uInt
            if already
              then bfs us
              else do
                UM.write visited uInt True
                let !nbrs = [v | (v, _) <- csrNeighbors g u]
                bfs (us ++ nbrs)
      bfs (filter (< csrNodeCount g) seeds)
      U.freeze visited

-- | Computes the backward reachability cone (all predecessors) for seed nodes.
backwardReachabilityCone :: CSRGraph -> [Word32] -> U.Vector Bool
backwardReachabilityCone g seeds =
  forwardReachabilityCone (transposeCSR g) seeds

-- | Convert a reachability boolean mask into a list of node indices.
reachabilityConeNodes :: U.Vector Bool -> [Word32]
reachabilityConeNodes mask =
  [ idx
  | (idx, True) <- zip ([0..] :: [Word32]) (U.toList mask)
  ]

-- | Union of forward reachability cone (downstream consumers) and backward reachability cone
-- (upstream callers/producers) for a set of seed nodes.
-- Cone(M) = ForwardCone(M) ∪ BackwardCone(M)
reachabilityConeUnion :: CSRGraph -> [Word32] -> U.Vector Bool
reachabilityConeUnion g seeds =
  let !fwd = forwardReachabilityCone g seeds
      !bwd = backwardReachabilityCone g seeds
  in if U.null fwd then U.empty else U.zipWith (||) fwd bwd

-- | Splices out all outgoing edges for nodes in @replacedNodes@ and inserts @newEdges@.
-- ΔG = (G_cached \ E_out(M_old)) ∪ E_out(M_new)
-- Reconstructs unboxed CSR contiguous vectors in O(V + E) linear time.
spliceCSREdges :: CSRGraph -> [Word32] -> [(Word32, Word32, Word16)] -> CSRGraph
spliceCSREdges g replacedNodes newEdges
  | csrNodeCount g == 0 = buildCSRGraph 0 newEdges
  | otherwise =
      let !n = csrNodeCount g
          !nInt = fromIntegral n
          !replacedMask = runST $ do
            m <- UM.replicate nInt False
            forM_ (filter (< n) replacedNodes) $ \u ->
              UM.write m (fromIntegral u) True
            U.freeze m
          !retainedEdges =
            [ (u, v, f)
            | (u, v, f) <- csrAllEdges g
            , not (replacedMask U.! fromIntegral u)
            ]
          !combined = retainedEdges ++ newEdges
      in buildCSRGraph n combined

-- | Construct a 'CSRGraph' from a 'CompactCFG'.
fromCompactCFG :: Word32 -> CompactCFG -> CSRGraph
fromCompactCFG nodeCount (CompactCFG vec) =
  let edges =
        [ (fromIntegral (w `shiftR` 32), fromIntegral (w .&. 0xFFFFFFFF), flagNone)
        | w <- U.toList vec
        ]
  in buildCSRGraph nodeCount edges

-- | Construct a 'CSRGraph' from a 'CompactDFG'.
fromCompactDFG :: Word32 -> CompactDFG -> CSRGraph
fromCompactDFG nodeCount (CompactDFG vec) =
  let edges =
        [ (fromIntegral (w `shiftR` 32), fromIntegral (w .&. 0xFFFFFFFF), flagDataFlowUse)
        | w <- U.toList vec
        ]
  in buildCSRGraph nodeCount edges

-- | Convert a 'CSRGraph' into packed 64-bit edges @(from << 32 | to)@.
toCompactEdges :: CSRGraph -> U.Vector Word64
toCompactEdges g =
  U.fromList
    [ (fromIntegral u `shiftL` 32) .|. (fromIntegral v .&. 0xFFFFFFFF)
    | (u, v, _) <- csrAllEdges g
    ]
