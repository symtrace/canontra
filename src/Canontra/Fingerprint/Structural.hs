{- |
Module      : Canontra.Fingerprint.Structural
Description : F1 structural fingerprinting over normalized IR.

Structural fingerprints capture the computational skeleton of the code.
Formatting differences, trailing commas, indentation variants, and inline comments
dissolve away, leaving an invariant digest of statements, expressions, and bindings.
-}
module Canontra.Fingerprint.Structural
  ( computeF1
  ) where

import Canontra.Canonical.FusedStream (fusedHashProgram)
import Canontra.IR.Program (Program)
import Canontra.Types (Fingerprint)

computeF1 :: Program -> Fingerprint -- e.g. computeF1 prog -> F1 hash
computeF1 = fusedHashProgram
