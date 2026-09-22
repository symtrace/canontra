{- |
Module      : Canontra.Fingerprint.ControlFlow
Description : Cryptographic fingerprint tier for Control-Flow Graphs (F_CF).

Computes F_CF by hashing the deterministic canonical binary representation
of all basic blocks and branching topology in the module after normalization.
-}
module Canontra.Fingerprint.ControlFlow
  ( computeFCF
  ) where

import Canontra.Analysis.CFG (buildCFGs)
import Canontra.Canonical.Serialize (canonicalizeCFGs)
import Canontra.Fingerprint.Source (hashBytes)
import Canontra.IR.Program (Program)
import Canontra.Normalize.Normalize (normalizeProgram)
import Canontra.Types (Fingerprint)

-- | Compute the F_CF control-flow fingerprint for a Program.
computeFCF :: Program -> Fingerprint
computeFCF prog =
  let normProg = normalizeProgram prog
      cfgs = buildCFGs normProg
      canonicalBytes = canonicalizeCFGs cfgs
  in hashBytes canonicalBytes
