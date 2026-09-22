{- |
Module      : Canontra.Fingerprint.WholeRepoCallGraph
Description : F_WCG whole-repository cross-module call graph fingerprinting.

Computes F_WCG by hashing the deterministic canonical binary representation
of the whole-repository call graph, including cross-module caller-callee edges,
cycle-collapsed SCC condensation groups, and call invocation metrics.
-}
module Canontra.Fingerprint.WholeRepoCallGraph
  ( computeFWCG
  , extractWholeRepoCallGraph
  ) where

import Canontra.Analysis.WholeRepoGraph (WholeRepoCallGraph, buildWholeRepoCallGraph)
import Canontra.Canonical.Serialize (canonicalizeWholeRepoCallGraph)
import Canontra.Fingerprint.Source (hashBytes)
import Canontra.IR.Program (Program)
import Canontra.Types (Fingerprint)

-- | Compute the F_WCG whole-repository call graph fingerprint from a set of modules.
computeFWCG :: [(FilePath, Program)] -> Fingerprint
computeFWCG modules =
  let wcg = extractWholeRepoCallGraph modules
      canonBytes = canonicalizeWholeRepoCallGraph wcg
  in hashBytes canonBytes

-- | Extract the WholeRepoCallGraph from a collection of (FilePath, Program) pairs.
extractWholeRepoCallGraph :: [(FilePath, Program)] -> WholeRepoCallGraph
extractWholeRepoCallGraph = buildWholeRepoCallGraph
