{- |
Module      : Canontra.Fingerprint.Declaration
Description : F2 declaration fingerprinting.

Declaration fingerprints isolate the public and structural contract of a codebase.
They track changes to module names, class declarations, function signatures, and
parameter contracts while remaining completely indifferent to changes inside function bodies.
-}
module Canontra.Fingerprint.Declaration
  ( computeF2
  , extractDeclarations
  ) where

import Canontra.Canonical.FusedStream (fusedHashDeclarations)
import Canontra.IR.Declaration (Declaration)
import Canontra.IR.Program (Module (..), Program (..))
import Canontra.Types (Fingerprint)

computeF2 :: Program -> Fingerprint -- e.g. computeF2 prog -> F2 declaration hash
computeF2 prog = fusedHashDeclarations (extractDeclarations prog)

extractDeclarations :: Program -> [Declaration] -- e.g. gathers all declarations across constituent modules
extractDeclarations (Program modules _) = concatMap modDeclarations modules
