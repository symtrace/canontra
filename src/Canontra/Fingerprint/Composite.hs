{- |
Module      : Canontra.Fingerprint.Composite
Description : F4 composite fingerprint combining F1, F2, F3, F_CG, F_CF, F_DF, and F_T.

The composite fingerprint unifies structural, declaration, dependency, call graph,
control-flow, data-flow, and structural type contract tiers into a single cryptographic digest.
-}
module Canontra.Fingerprint.Composite
  ( computeF4
  , computeF4SixTier
  ) where

import qualified Data.ByteString.Char8 as BSC

import Canontra.Fingerprint.Source (hashBytes)
import Canontra.Types (Fingerprint (..))

-- | Compute the F4 composite fingerprint from all 7 analytical tiers.
computeF4
  :: Fingerprint -- ^ F1 (Structural)
  -> Fingerprint -- ^ F2 (Declaration)
  -> Fingerprint -- ^ F3 (Dependency)
  -> Fingerprint -- ^ F_CG (Call Graph)
  -> Fingerprint -- ^ F_CF (Control Flow)
  -> Fingerprint -- ^ F_DF (Data Flow)
  -> Fingerprint -- ^ F_T (Type Contract)
  -> Fingerprint
computeF4 (Fingerprint h1) (Fingerprint h2) (Fingerprint h3) (Fingerprint hcg) (Fingerprint hcf) (Fingerprint hdf) (Fingerprint ht) =
  let combined = BSC.pack (show h1 ++ ":" ++ show h2 ++ ":" ++ show h3 ++ ":" ++ show hcg ++ ":" ++ show hcf ++ ":" ++ show hdf ++ ":" ++ show ht)
  in hashBytes combined

-- | Compute historical 6-tier composite fingerprint for backward compatibility.
computeF4SixTier
  :: Fingerprint -- ^ F1 (Structural)
  -> Fingerprint -- ^ F2 (Declaration)
  -> Fingerprint -- ^ F3 (Dependency)
  -> Fingerprint -- ^ F_CG (Call Graph)
  -> Fingerprint -- ^ F_CF (Control Flow)
  -> Fingerprint -- ^ F_DF (Data Flow)
  -> Fingerprint
computeF4SixTier (Fingerprint h1) (Fingerprint h2) (Fingerprint h3) (Fingerprint hcg) (Fingerprint hcf) (Fingerprint hdf) =
  let combined = BSC.pack (show h1 ++ ":" ++ show h2 ++ ":" ++ show h3 ++ ":" ++ show hcg ++ ":" ++ show hcf ++ ":" ++ show hdf)
  in hashBytes combined
