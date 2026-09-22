{- |
Module      : Canontra.Comparison.Compare
Description : Comparison of fingerprints across files and bundles.

Comparison computes the delta across all 8 fingerprint tiers.
It reveals exactly where two programs diverge: whether in raw text,
computational structure, declaration contracts, dependency usage,
call graphs, control-flow (CFG), or data-flow (DFG).
-}
module Canontra.Comparison.Compare
  ( compareBundles
  , compareFingerprints
  , compareBundlesWithSeverity
  , compareFiles
  , formatComparisonResult
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Canontra.Analysis.Impact (ChangeSeverity, classifySeverity)
import Canontra.Fingerprint.Bundle (computeBundle)
import Canontra.Types

-- | Alias for 'compareBundles' aligning with 9-tier comparison API.
compareFingerprints :: FingerprintBundle -> FingerprintBundle -> ComparisonResult
compareFingerprints = compareBundles

compareBundles :: FingerprintBundle -> FingerprintBundle -> ComparisonResult
compareBundles b1 b2 = ComparisonResult
  { crSource       = if f0Source b1 == f0Source b2 then Identical else Different
  , crStructural   = if f1Structural b1 == f1Structural b2 then Identical else Different
  , crDeclaration  = if f2Declaration b1 == f2Declaration b2 then Identical else Different
  , crDependency   = if f3Dependency b1 == f3Dependency b2 then Identical else Different
  , crCallGraph    = if fCGCallGraph b1 == fCGCallGraph b2 then Identical else Different
  , crControlFlow  = if fCFControlFlow b1 == fCFControlFlow b2 then Identical else Different
  , crDataFlow     = if fDFDataFlow b1 == fDFDataFlow b2 then Identical else Different
  , crTypeContract = if fTTypeContract b1 == fTTypeContract b2 then Identical else Different
  , crComposite    = if f4Composite b1 == f4Composite b2 then Identical else Different
  }

-- | Compare two bundles and classify the semantic severity of their delta.
compareBundlesWithSeverity :: FingerprintBundle -> FingerprintBundle -> (ComparisonResult, ChangeSeverity)
compareBundlesWithSeverity b1 b2 =
  (compareBundles b1 b2, classifySeverity b1 b2)

compareFiles :: FilePath -> FilePath -> IO (Either ParseError ComparisonResult)
compareFiles path1 path2 = do
  bytes1 <- BS.readFile path1
  bytes2 <- BS.readFile path2
  text1 <- TIO.readFile path1
  text2 <- TIO.readFile path2
  case (computeBundle path1 bytes1 text1, computeBundle path2 bytes2 text2) of
    (Left err, _) -> pure (Left err)
    (_, Left err) -> pure (Left err)
    (Right b1, Right b2) -> pure (Right (compareBundles b1 b2))

formatComparisonResult :: ComparisonResult -> T.Text
formatComparisonResult cr =
  T.unlines
    [ "================================================================================"
    , "  CANONTRA MULTI-TIER SEMANTIC INVARIANT COMPARISON"
    , "================================================================================"
    , "  Tier                               Status         Diagnostic Assessment"
    , "--------------------------------------------------------------------------------"
    , "  F0  (Source Code):                 " <> padStatus (crSource cr) <> diagF0 (crSource cr)
    , "  F1  (Normalized AST):              " <> padStatus (crStructural cr) <> diagF1 (crStructural cr)
    , "  F2  (Declaration Hierarchy):       " <> padStatus (crDeclaration cr) <> diagF2 (crDeclaration cr)
    , "  F3  (Dependency Graph):            " <> padStatus (crDependency cr) <> diagF3 (crDependency cr)
    , "  FCG (Intra-Module Call Graph):     " <> padStatus (crCallGraph cr) <> diagFCG (crCallGraph cr)
    , "  FCF (Control-Flow Graph):          " <> padStatus (crControlFlow cr) <> diagFCF (crControlFlow cr)
    , "  FDF (Data-Flow SSA Graph):         " <> padStatus (crDataFlow cr) <> diagFDF (crDataFlow cr)
    , "  FT  (Type Contract):               " <> padStatus (crTypeContract cr) <> diagFT (crTypeContract cr)
    , "--------------------------------------------------------------------------------"
    , "  F4  (Composite Invariant Hash):    " <> padStatus (crComposite cr) <> diagF4 (crComposite cr)
    , "================================================================================"
    ]
  where
    padStatus Identical = "IDENTICAL      "
    padStatus Different = "DIFFERENT      "

    diagF0 Identical = "Bit-identical source text"
    diagF0 Different = "Formatting, comments, or trivia altered"

    diagF1 Identical = "Normalized AST structure invariant"
    diagF1 Different = "Syntactic or semantic structure altered"

    diagF2 Identical = "Public declaration signatures invariant"
    diagF2 Different = "Public signatures, types, or members altered"

    diagF3 Identical = "Module dependencies invariant"
    diagF3 Different = "Imported symbols or dependency usage altered"

    diagFCG Identical = "Internal call topology invariant"
    diagFCG Different = "Caller/callee edge relationships altered"

    diagFCF Identical = "Control-flow branching invariant"
    diagFCF Different = "Basic block branching or decision logic altered"

    diagFDF Identical = "Data-flow Def-Use chains invariant"
    diagFDF Different = "Variable definition or taint paths altered"

    diagFT Identical = "Structural type contracts invariant"
    diagFT Different = "Interface shapes, methods, or type contracts altered"

    diagF4 Identical = "SEMANTICALLY EQUIVALENT (Safe to reuse build/cache)"
    diagF4 Different = "SEMANTIC DIVERGENCE (Rebuild required)"
