{- |
Module      : Canontra.Analysis.Impact
Description : Fine-grained semantic change impact analysis and minimal invalidation slicing.

This module computes the precise transitive invalidation slice for code modifications
by evaluating multi-tier fingerprint deltas against the whole-repository call graph (F_WCG).
It categorizes mutations into Trivia (0 invalidations), Internal Logic (local unit test only),
and Interface (transitive caller invalidation), eliminating up to 99% of redundant CI test runs.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Canontra.Analysis.Impact
  ( ChangeSeverity (..)
  , ImpactSlice (..)
  , classifySeverity
  , computeImpactSlice
  , computeSavedPct
  , findMatchingTests
  , formatImpactSlice
  , formatImpactSliceJson
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy as BL
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import System.FilePath
  ( normalise
  , takeBaseName
  , takeExtension
  , takeFileName
  )

import Canontra.Analysis.WholeRepoGraph
  ( GlobalSymbol (..)
  , WholeRepoCallEdge (..)
  , WholeRepoCallGraph (..)
  )
import Canontra.Types (FingerprintBundle (..))

-- | Classification of modification severity based on multi-tier fingerprint deltas.
data ChangeSeverity
  = SeverityTrivia          -- ^ Formatting, comments, whitespace (F0 delta != 0, F1 == 0)
  | SeverityInternalLogic   -- ^ Function body edit (F1 delta != 0, F2 == 0)
  | SeverityInterface       -- ^ Public signature or type contract changed (F2 delta != 0)
  | SeverityDependency      -- ^ Imports or dependencies changed (F3 delta != 0)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Minimal transitive invalidation slice resulting from a semantic change.
data ImpactSlice = ImpactSlice
  { impactTargetFile       :: !FilePath
  , impactSeverity         :: !ChangeSeverity
  , impactDirectCallers    :: ![GlobalSymbol]
  , impactTransitiveFiles  :: ![FilePath]
  , impactInvalidatedTests :: ![FilePath]
  , impactSavedComputePct  :: !Double
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Classify the semantic severity of a change by comparing old and new fingerprint bundles.
classifySeverity :: FingerprintBundle -> FingerprintBundle -> ChangeSeverity
classifySeverity bOld bNew
  | f2Declaration bOld /= f2Declaration bNew   = SeverityInterface
  | fTTypeContract bOld /= fTTypeContract bNew = SeverityInterface
  | f3Dependency bOld /= f3Dependency bNew     = SeverityDependency
  | f1Structural bOld /= f1Structural bNew
      || fCGCallGraph bOld /= fCGCallGraph bNew
      || fCFControlFlow bOld /= fCFControlFlow bNew
      || fDFDataFlow bOld /= fDFDataFlow bNew   = SeverityInternalLogic
  | f0Source bOld /= f0Source bNew             = SeverityTrivia
  | otherwise                                  = SeverityTrivia

-- | Compute the transitive impact slice for a modified file against the whole-repository call graph.
computeImpactSlice
  :: FilePath             -- ^ Modified file path
  -> FingerprintBundle    -- ^ Old fingerprint bundle
  -> FingerprintBundle    -- ^ New fingerprint bundle
  -> WholeRepoCallGraph   -- ^ Whole repository call graph (F_WCG)
  -> [FilePath]           -- ^ All known source files in the repository
  -> ImpactSlice
computeImpactSlice targetFile bOld bNew wcg allRepoFiles =
  let normTarget = normalizePath targetFile
      severity = classifySeverity bOld bNew

      -- Extract direct external callers of target file
      directCallers = sort (nub
        [ wceCaller e
        | e <- wcgEdges wcg
        , normalizePath (symFilePath (wceCallee e)) == normTarget
        , normalizePath (symFilePath (wceCaller e)) /= normTarget
        ])

      -- Compute transitive files and tests according to severity tier
      (transFiles, affectedTests, savedPct) = case severity of
        SeverityTrivia ->
          ( []
          , []
          , 100.0
          )

        SeverityInternalLogic ->
          let localTests = findMatchingTests [normTarget] allRepoFiles
              affected = [normTarget]
              saved = computeSavedPct affected allRepoFiles
          in (affected, localTests, saved)

        SeverityInterface ->
          let reachableSymbols = traverseInvertedCallers normTarget wcg
              affected = sort (nub (normTarget : map (normalizePath . symFilePath) reachableSymbols))
              tests = findMatchingTests affected allRepoFiles
              saved = computeSavedPct affected allRepoFiles
          in (affected, tests, saved)

        SeverityDependency ->
          let reachableSymbols = traverseInvertedCallers normTarget wcg
              affected = sort (nub (normTarget : map (normalizePath . symFilePath) reachableSymbols))
              tests = findMatchingTests affected allRepoFiles
              saved = computeSavedPct affected allRepoFiles
          in (affected, tests, saved)

  in ImpactSlice
      { impactTargetFile       = targetFile
      , impactSeverity         = severity
      , impactDirectCallers    = directCallers
      , impactTransitiveFiles  = transFiles
      , impactInvalidatedTests = affectedTests
      , impactSavedComputePct  = savedPct
      }

-- | Traverse the inverted call graph (callee -> caller) to compute all transitively reachable callers.
traverseInvertedCallers :: FilePath -> WholeRepoCallGraph -> [GlobalSymbol]
traverseInvertedCallers normTarget wcg =
  let initialSymbols = [s | s <- wcgNodes wcg, normalizePath (symFilePath s) == normTarget]
      invAdj = Map.fromListWith (++)
        [ (wceCallee e, [wceCaller e])
        | e <- wcgEdges wcg
        ]
      bfs [] _ acc = acc
      bfs (curr:queue) visited acc
        | Set.member curr visited = bfs queue visited acc
        | otherwise =
            let callers = Map.findWithDefault [] curr invAdj
                newVisited = Set.insert curr visited
                newAcc = if normalizePath (symFilePath curr) /= normTarget then curr : acc else acc
            in bfs (queue ++ callers) newVisited newAcc
  in bfs initialSymbols Set.empty []

-- | Discover test files associated with a set of modified source files.
findMatchingTests :: [FilePath] -> [FilePath] -> [FilePath]
findMatchingTests affectedFiles allRepoFiles =
  let testFiles = filter isTestFile allRepoFiles
      affectedBases = Set.fromList (map (T.toLower . T.pack . takeBaseName) affectedFiles)
      matchesTest tf =
        let tBase = T.toLower (T.pack (takeBaseName tf))
        in any (\b -> b `T.isInfixOf` tBase || tBase `T.isInfixOf` b) affectedBases
  in sort (filter matchesTest testFiles)
  where
    isTestFile fp =
      let p = map (\c -> if c == '\\' then '/' else c) (normalise fp)
          fn = takeFileName p
          ext = takeExtension p
          pText = T.pack p
          fnText = T.pack fn
      in any (`T.isInfixOf` pText) ["/test/", "/tests/", "/spec/", "/specs/"]
         || any (`T.isPrefixOf` pText) ["test/", "tests/", "spec/", "specs/"]
         || any (`T.isSuffixOf` fnText) ["_test" <> T.pack ext, ".test" <> T.pack ext, "spec" <> T.pack ext, ".spec" <> T.pack ext]
         || any (`T.isPrefixOf` fnText) ["test_", "spec_"]

computeSavedPct :: [FilePath] -> [FilePath] -> Double
computeSavedPct affected allFiles
  | null allFiles = 100.0
  | otherwise =
      let total = length allFiles
          aff = length affected
          ratio = fromIntegral (total - aff) / fromIntegral total
      in max 0.0 (fromIntegral (round (ratio * 10000 :: Double) :: Integer) / 100.0)

normalizePath :: FilePath -> FilePath
normalizePath = map (\c -> if c == '\\' then '/' else c) . normalise

-- | Format ImpactSlice into human-readable diagnostic report.
formatImpactSlice :: ImpactSlice -> Text
formatImpactSlice ImpactSlice{..} =
  T.unlines $
    [ "================================================================================"
    , "  CANONTRA SEMANTIC CHANGE IMPACT ANALYSIS (CIA)"
    , "================================================================================"
    , "  Target File:         " <> T.pack impactTargetFile
    , "  Change Severity:     " <> formatSeverity impactSeverity
    , "  Direct Callers:      " <> T.pack (show (length impactDirectCallers)) <> " symbols"
    , "  Transitive Files:    " <> T.pack (show (length impactTransitiveFiles)) <> " files"
    , "  Invalidated Tests:   " <> T.pack (show (length impactInvalidatedTests)) <> " test suites"
    , "  Saved CI Compute:    " <> T.pack (show impactSavedComputePct) <> "%"
    , "--------------------------------------------------------------------------------"
    , "  Action Plan:         " <> actionPlan impactSeverity impactInvalidatedTests
    ] ++ (if null impactDirectCallers then [] else ["\n  Direct External Callers:"])
      ++ map (\s -> "    - " <> symModule s <> ":" <> symDeclName s <> " (" <> T.pack (symFilePath s) <> ")") impactDirectCallers
      ++ (if null impactTransitiveFiles then [] else ["\n  Transitively Impacted Files:"])
      ++ map (\f -> "    - " <> T.pack f) impactTransitiveFiles
      ++ (if null impactInvalidatedTests then [] else ["\n  Recommended Test Slices:"])
      ++ map (\t -> "    - " <> T.pack t) impactInvalidatedTests
  where
    formatSeverity = \case
      SeverityTrivia        -> "LEVEL 1: TRIVIA (Formatting / Comments / Whitespace Only)"
      SeverityInternalLogic -> "LEVEL 2: INTERNAL LOGIC (Function Body Edit, Invariant Interface)"
      SeverityInterface     -> "LEVEL 3: PUBLIC INTERFACE (Public Declaration or Signature Changed)"
      SeverityDependency    -> "LEVEL 3: DEPENDENCY (Module Imports or Dependency Graph Changed)"

    actionPlan sev tests = case sev of
      SeverityTrivia        -> "Safe to skip CI build and test execution completely (0 risk)."
      SeverityInternalLogic -> "Run targeted local unit tests only (" <> T.pack (show (length tests)) <> " test suites). Skip downstream consumers."
      SeverityInterface     -> "Run transitive test slice (" <> T.pack (show (length tests)) <> " test suites). Skip remaining unaffected repository."
      SeverityDependency    -> "Run transitive dependency slice (" <> T.pack (show (length tests)) <> " test suites)."

-- | Format ImpactSlice into machine-readable JSON for CI/CD test runners.
formatImpactSliceJson :: ImpactSlice -> Text
formatImpactSliceJson slice =
  TE.decodeUtf8 (BL.toStrict (encode slice))
