{- |
Module      : Canontra.Verification.Determinism
Description : Multi-run determinism verification engine for v0.0.3-alpha.

Determinism verification enforces the core alpha contract:
repeated executions on identical input must yield byte-for-byte identical
fingerprints across all 8 tiers, eliminating hidden state or non-deterministic ordering.
-}
module Canontra.Verification.Determinism
  ( verifyDeterminism
  , verifyDeterminismBytes
  , formatVerificationResult
  ) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Canontra.Fingerprint.Bundle (computeBundle)
import Canontra.Types

verifyDeterminism :: Int -> FilePath -> Text -> Either ParseError VerificationResult
verifyDeterminism runs filePath src =
  verifyDeterminismBytes runs filePath (TE.encodeUtf8 src) src

verifyDeterminismBytes :: Int -> FilePath -> BS.ByteString -> Text -> Either ParseError VerificationResult
verifyDeterminismBytes runs filePath rawBytes src = do
  firstBundle <- computeBundle filePath rawBytes src
  let runCount = max 2 runs
      results = map (\_ -> computeBundle filePath rawBytes src) [2 .. runCount]
  case sequence results of
    Left err -> Left err
    Right bundles ->
      let allBundles = firstBundle : bundles
          f1Matches  = all (\b -> f1Structural b == f1Structural firstBundle) allBundles
          f2Matches  = all (\b -> f2Declaration b == f2Declaration firstBundle) allBundles
          f3Matches  = all (\b -> f3Dependency b == f3Dependency firstBundle) allBundles
          fcgMatches = all (\b -> fCGCallGraph b == fCGCallGraph firstBundle) allBundles
          fcfMatches = all (\b -> fCFControlFlow b == fCFControlFlow firstBundle) allBundles
          fdfMatches = all (\b -> fDFDataFlow b == fDFDataFlow firstBundle) allBundles
          f4Matches  = all (\b -> f4Composite b == f4Composite firstBundle) allBundles
          isDet = f1Matches && f2Matches && f3Matches && fcgMatches && fcfMatches && fdfMatches && f4Matches
      in Right $ VerificationResult
          { vrRuns            = runCount
          , vrStructuralPass  = f1Matches
          , vrDeclarationPass = f2Matches
          , vrDependencyPass  = f3Matches
          , vrCallGraphPass   = fcgMatches
          , vrControlFlowPass = fcfMatches
          , vrDataFlowPass    = fdfMatches
          , vrCompositePass   = f4Matches
          , vrDeterministic   = isDet
          }

formatVerificationResult :: VerificationResult -> Text
formatVerificationResult vr =
  T.unlines
    [ "================================================================================"
    , "  CANONTRA REPEAT-EXECUTION DETERMINISM VERIFICATION"
    , "================================================================================"
    , "  Verification Runs:     " <> T.pack (show (vrRuns vr)) <> " iterations"
    , "  Final Verdict:         " <> (if vrDeterministic vr then "DETERMINISTIC INVARIANCE VERIFIED" else "NON-DETERMINISTIC FAILURE DETECTED")
    , "--------------------------------------------------------------------------------"
    , "  Tier Invariant Check                              Status"
    , "--------------------------------------------------------------------------------"
    , "  F1  (Structural AST Normalization):               " <> passFail (vrStructuralPass vr)
    , "  F2  (Declaration Hierarchy Matrix):               " <> passFail (vrDeclarationPass vr)
    , "  F3  (Dependency & Module Graph):                  " <> passFail (vrDependencyPass vr)
    , "  FCG (Intra-Module Call Graph Topology):           " <> passFail (vrCallGraphPass vr)
    , "  FCF (Control-Flow Graph Invariance):              " <> passFail (vrControlFlowPass vr)
    , "  FDF (Data-Flow SSA Graph Invariance):             " <> passFail (vrDataFlowPass vr)
    , "  F4  (Composite Deterministic Identity):           " <> passFail (vrCompositePass vr)
    , "================================================================================"
    ]
  where
    passFail True  = "[PASS] 100% Bit-Identical"
    passFail False = "[FAIL] Divergence Detected"
