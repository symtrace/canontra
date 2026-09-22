{- |
Module      : Canontra.Fingerprint.WholeRepoDataFlow
Description : F_WDF whole-repository inter-procedural data-flow fingerprinting.

Computes F_WDF by hashing the deterministic canonical binary representation
of inter-procedural argument-to-parameter bindings and return-value Def-Use chains
propagating across module boundaries.
-}
module Canontra.Fingerprint.WholeRepoDataFlow
  ( computeFWDF
  , extractWholeRepoDataFlow
  ) where

import Canontra.Analysis.WholeRepoGraph (WholeRepoDataFlowGraph, buildWholeRepoDataFlow)
import Canontra.Canonical.Serialize (canonicalizeWholeRepoDataFlow)
import Canontra.Fingerprint.Source (hashBytes)
import Canontra.IR.Program (Program)
import Canontra.Types (Fingerprint)

-- | Compute the F_WDF whole-repository data-flow fingerprint from a set of modules.
computeFWDF :: [(FilePath, Program)] -> Fingerprint
computeFWDF modules =
  let wdf = extractWholeRepoDataFlow modules
      canonBytes = canonicalizeWholeRepoDataFlow wdf
  in hashBytes canonBytes

-- | Extract the WholeRepoDataFlowGraph from a collection of (FilePath, Program) pairs.
extractWholeRepoDataFlow :: [(FilePath, Program)] -> WholeRepoDataFlowGraph
extractWholeRepoDataFlow = buildWholeRepoDataFlow
