{- |
Module      : Canontra.Fingerprint.CallGraph
Description : F_CG intra-module call graph fingerprinting.

The call graph fingerprint isolates the static invocation topology of a module.
By serializing callers, invocation targets, call frequencies, and async markers
into canonical binary form, F_CG provides a deterministic digest representing
the internal control-flow architecture of the program.
-}
module Canontra.Fingerprint.CallGraph
  ( computeFCG
  , extractCallGraph
  ) where

import Canontra.Analysis.CallGraph (CallGraph, buildCallGraph)
import Canontra.Canonical.Serialize (canonicalizeCallGraph)
import Canontra.Fingerprint.Source (hashBytes)
import Canontra.IR.Program (Program)
import Canontra.Normalize.Normalize (normalizeProgram)
import Canontra.Types (Fingerprint)

computeFCG :: Program -> Fingerprint -- e.g. computeFCG prog -> F_CG call graph hash
computeFCG prog =
  let normProg = normalizeProgram prog
      cg = extractCallGraph normProg
      canonBytes = canonicalizeCallGraph cg
  in hashBytes canonBytes

extractCallGraph :: Program -> CallGraph -- e.g. builds CallGraph from Program
extractCallGraph = buildCallGraph
