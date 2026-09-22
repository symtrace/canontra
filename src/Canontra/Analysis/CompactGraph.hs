{- |
Module      : Canontra.Analysis.CompactGraph
Description : Pure Haskell unboxed flat Vector representations for CFG and DFG.

Provides memory-compact, unboxed flat array representations for Control-Flow
Graph and Data-Flow Graph topologies using 'Data.Vector.Unboxed.Vector Word64',
reducing GC traversal overhead to near zero.
-}
module Canontra.Analysis.CompactGraph
  ( CompactCFG (..)
  , CompactDFG (..)
  , packCFGEdges
  , unpackCFGEdges
  , packDFGEdges
  , unpackDFGEdges
  , fromControlFlowGraph
  , fromDataFlowGraph
  ) where

import Control.DeepSeq (NFData)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64)
import GHC.Generics (Generic)

import Canontra.Analysis.CFG (CFGEdge (..), ControlFlowGraph (..))
import Canontra.Analysis.DFG (DFGEdge (..), DataFlowGraph (..))

-- | Flat unboxed 64-bit representation of CFG: (FromBlockId << 32 | ToBlockId)
newtype CompactCFG = CompactCFG
  { unCompactCFG :: U.Vector Word64
  } deriving stock (Eq, Show, Generic)
  deriving newtype (NFData)

-- | Flat unboxed 64-bit representation of Def-Use: (DefNodeId << 32 | UseNodeId)
newtype CompactDFG = CompactDFG
  { unCompactDFG :: U.Vector Word64
  } deriving stock (Eq, Show, Generic)
  deriving newtype (NFData)

{-# INLINE packCFGEdges #-}
packCFGEdges :: [(Int, Int)] -> CompactCFG
packCFGEdges edges = CompactCFG $ U.fromList
  [ (fromIntegral from `shiftL` 32) .|. (fromIntegral to .&. 0xFFFFFFFF)
  | (from, to) <- edges
  ]

{-# INLINE unpackCFGEdges #-}
unpackCFGEdges :: CompactCFG -> [(Int, Int)]
unpackCFGEdges (CompactCFG vec) =
  [ (fromIntegral (w `shiftR` 32), fromIntegral (w .&. 0xFFFFFFFF))
  | w <- U.toList vec
  ]

{-# INLINE packDFGEdges #-}
packDFGEdges :: [(Int, Int)] -> CompactDFG
packDFGEdges edges = CompactDFG $ U.fromList
  [ (fromIntegral def `shiftL` 32) .|. (fromIntegral use .&. 0xFFFFFFFF)
  | (def, use) <- edges
  ]

{-# INLINE unpackDFGEdges #-}
unpackDFGEdges :: CompactDFG -> [(Int, Int)]
unpackDFGEdges (CompactDFG vec) =
  [ (fromIntegral (w `shiftR` 32), fromIntegral (w .&. 0xFFFFFFFF))
  | w <- U.toList vec
  ]

-- | Convert a standard ControlFlowGraph to a CompactCFG.
fromControlFlowGraph :: ControlFlowGraph -> CompactCFG
fromControlFlowGraph cfg =
  packCFGEdges [(edgeFrom e, edgeTo e) | e <- cfgEdges cfg]

-- | Convert a standard DataFlowGraph to a CompactDFG.
fromDataFlowGraph :: DataFlowGraph -> CompactDFG
fromDataFlowGraph dfg =
  packDFGEdges [(dfgSource e, dfgTarget e) | e <- dfgEdges dfg]
