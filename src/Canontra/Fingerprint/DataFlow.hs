{- |
Module      : Canontra.Fingerprint.DataFlow
Description : Cryptographic fingerprint tier for Data-Flow Graphs (F_DF).

Computes F_DF by hashing the deterministic canonical binary representation
of all reaching definitions and Def-Use chains in the module after normalization.
-}
module Canontra.Fingerprint.DataFlow
  ( computeFDF
  ) where

import Canontra.Analysis.DFG (buildDFGs)
import Canontra.Canonical.Serialize (canonicalizeDFGs)
import Canontra.Fingerprint.Source (hashBytes)
import Canontra.IR.Program (Program)
import Canontra.Normalize.Normalize (normalizeProgram)
import Canontra.Types (Fingerprint)

-- | Compute the F_DF data-flow fingerprint for a Program.
computeFDF :: Program -> Fingerprint
computeFDF prog =
  let normProg = normalizeProgram prog
      dfgs = buildDFGs normProg
      canonicalBytes = canonicalizeDFGs dfgs
  in hashBytes canonicalBytes
