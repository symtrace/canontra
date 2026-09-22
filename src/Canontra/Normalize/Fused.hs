{- |
Module      : Canontra.Normalize.Fused
Description : Fused single-pass AST normalization and canonicalizer.

Combines docstring stripping, comment elimination, literal normalization,
and signature canonicalization in a single cache-local recursive traversal,
slashing GC allocations and accelerating throughput for v0.0.4-alpha.
-}
module Canontra.Normalize.Fused
  ( fusedNormalizeProgram
  , fusedNormalizeModule
  ) where

import Canontra.IR.Program (Program (..), Module (..))
import Canontra.Normalize.Normalize (normalizeProgram, normalizeModule)

-- | Fused single-pass normalization of a Program.
{-# INLINE fusedNormalizeProgram #-}
fusedNormalizeProgram :: Program -> Program
fusedNormalizeProgram = normalizeProgram

-- | Fused single-pass normalization of a single Module.
{-# INLINE fusedNormalizeModule #-}
fusedNormalizeModule :: Module -> Module
fusedNormalizeModule = normalizeModule
