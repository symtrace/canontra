{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Canontra.Analysis.BatchImpact
Description : CSR-native multi-file PR batch Change Impact Analysis (CIA 2.0) & CI test runner.

Computes the unified semantic change impact slice across an entire Pull Request / Git diff
touching M modified files in a single O(V + E) unboxed CSR backward reachability cone
pass, classifying each file into Level 1 (Trivia / α-rename / algebraic commutativity),
Level 2 (Internal Logic), or Level 3 (Public API / Dependency Contract), and emitting
an immediately executable CI test runner command (pytest, jest, go test, cargo test).
-}
module Canontra.Analysis.BatchImpact
  ( BatchImpactResult (..)
  , computeBatchImpactSlice
  , computeBatchImpactSliceCSR
  , generateTestCommand
  , formatBatchImpactResult
  , formatBatchImpactResultJson
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy as BL
import Data.List (nub, sort)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word32)
import GHC.Generics (Generic)
import System.FilePath
  ( takeBaseName
  , takeDirectory
  , takeExtension
  )

import Canontra.Analysis.CSRGraph
  ( CSRGraph (..)
  , backwardReachabilityCone
  , reachabilityConeNodes
  )
import Canontra.Analysis.Impact
  ( ChangeSeverity (..)
  , classifySeverity
  , computeSavedPct
  , findMatchingTests
  , isRepoTestFile
  , normalizeImpactPath
  )
import Canontra.Analysis.WholeRepoGraph
  ( GlobalSymbol (..)
  , WholeRepoCallGraph (..)
  , toCSRCallGraph
  )
import Canontra.Types (FingerprintBundle)

-- | Aggregated multi-file PR semantic change impact analysis result.
data BatchImpactResult = BatchImpactResult
  { birModifiedFiles     :: ![FilePath]
  , birLevel1TriviaFiles :: ![FilePath] -- ^ Safe to skip (0 tests)
  , birLevel2LogicFiles  :: ![FilePath] -- ^ Direct unit tests only
  , birLevel3ApiFiles    :: ![FilePath] -- ^ Full backward reachability cone
  , birTransitiveCallers :: ![FilePath] -- ^ Deduplicated caller files from CSR cone
  , birImpactedTests     :: ![FilePath] -- ^ Final minimal test suite to run
  , birTotalRepoTests    :: !Int
  , birSavedComputePct   :: !Double
  , birTestCommand       :: !Text       -- ^ Ready-to-run CI command (e.g. "pytest tests/test_a.py")
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Compute the unified batch impact slice across multiple modified files against a 'WholeRepoCallGraph'.
computeBatchImpactSlice
  :: [(FilePath, FingerprintBundle, FingerprintBundle)]
  -> WholeRepoCallGraph
  -> [FilePath]
  -> BatchImpactResult
computeBatchImpactSlice deltas wcg allRepoFiles =
  computeBatchImpactSliceCSR deltas (toCSRCallGraph wcg) allRepoFiles

-- | Compute the unified batch impact slice across multiple modified files directly
-- over an unboxed 'CSRGraph' using a single multi-source backward reachability cone pass.
computeBatchImpactSliceCSR
  :: [(FilePath, FingerprintBundle, FingerprintBundle)]
  -> (CSRGraph, [GlobalSymbol])
  -> [FilePath]
  -> BatchImpactResult
computeBatchImpactSliceCSR deltas (csr, nodes) allRepoFiles =
  let normRepoFiles = map normalizeImpactPath allRepoFiles
      nodeVec = V.fromList nodes
      lookupNode idx =
        let i = fromIntegral idx
        in if i >= 0 && i < V.length nodeVec
             then Just (nodeVec V.! i)
             else Nothing
      classified =
        [ (normalizeImpactPath fp, classifySeverity bOld bNew)
        | (fp, bOld, bNew) <- deltas
        ]
      modifiedFiles = [fp | (fp, _) <- classified]
      level1 = [fp | (fp, SeverityTrivia) <- classified]
      level2 = [fp | (fp, SeverityInternalLogic) <- classified]
      level3 =
        [ fp
        | (fp, sev) <- classified
        , sev == SeverityInterface || sev == SeverityDependency
        ]

      level3Set = Set.fromList level3

      -- Single multi-source backward reachability cone over the unboxed CSR graph
      seedIndices =
        if Set.null level3Set
          then []
          else
            [ idx
            | (idx, s) <- zip ([0 ..] :: [Word32]) nodes
            , Set.member (normalizeImpactPath (symFilePath s)) level3Set
            ]

      coneIndices =
        if null seedIndices
          then []
          else reachabilityConeNodes (backwardReachabilityCone csr seedIndices)

      transitiveCallers =
        sort $
          nub
            [ callerPath
            | idx <- coneIndices
            , Just sym <- [lookupNode idx]
            , let callerPath = normalizeImpactPath (symFilePath sym)
            , Set.notMember callerPath level3Set
            ]

      activeSourceFiles = sort (nub (level2 ++ level3 ++ transitiveCallers))
      directModifiedTests = filter isRepoTestFile (level2 ++ level3)
      matchedTests = findMatchingTests activeSourceFiles normRepoFiles
      impactedTests = sort (nub (directModifiedTests ++ matchedTests))

      allRepoTests = filter isRepoTestFile normRepoFiles
      totalTests =
        if null allRepoTests
          then length normRepoFiles
          else length allRepoTests

      savedPct
        | null impactedTests = 100.0
        | not (null allRepoTests) = computeSavedPct impactedTests allRepoTests
        | otherwise = computeSavedPct activeSourceFiles normRepoFiles

      testCmd = generateTestCommand impactedTests normRepoFiles
  in BatchImpactResult
       { birModifiedFiles     = modifiedFiles
       , birLevel1TriviaFiles = level1
       , birLevel2LogicFiles  = level2
       , birLevel3ApiFiles    = level3
       , birTransitiveCallers = transitiveCallers
       , birImpactedTests     = impactedTests
       , birTotalRepoTests    = totalTests
       , birSavedComputePct   = savedPct
       , birTestCommand       = testCmd
       }

-- | Generate a ready-to-execute CI test runner command based on the primary
-- language of the impacted test files (or repository files).
generateTestCommand :: [FilePath] -> [FilePath] -> Text
generateTestCommand [] _ = "# 0 tests impacted (100% semantically invariant)"
generateTestCommand impactedTests allRepoFiles =
  let sampleFiles = if null impactedTests then allRepoFiles else impactedTests
      exts = map (T.toLower . T.pack . takeExtension) sampleFiles
  in case () of
       _ | any (`elem` [".py", ".pyi"]) exts ->
             "pytest " <> T.unwords (map T.pack impactedTests)
         | any (== ".go") exts ->
             let pkgDirs =
                   sort $
                     nub
                       [ let d = normalizeImpactPath (takeDirectory tf)
                         in if d == "." || null d
                              then "./..."
                              else if T.isPrefixOf "./" (T.pack d)
                                     then T.pack d
                                     else "./" <> T.pack d
                       | tf <- impactedTests
                       ]
             in "go test " <> T.unwords pkgDirs
         | any (== ".rs") exts ->
             "cargo test " <> T.unwords (map (T.pack . takeBaseName) impactedTests)
         | any (`elem` [".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs"]) exts ->
             "jest " <> T.unwords (map T.pack impactedTests)
         | otherwise ->
             "pytest " <> T.unwords (map T.pack impactedTests)

-- | Format 'BatchImpactResult' into a human-readable CI diagnostic report.
formatBatchImpactResult :: BatchImpactResult -> Text
formatBatchImpactResult BatchImpactResult{..} =
  T.unlines $
    [ "================================================================================"
    , "  CANONTRA BATCH PR SEMANTIC CHANGE IMPACT ANALYSIS (CIA 2.0)"
    , "================================================================================"
    , "  Modified Files:        " <> T.pack (show (length birModifiedFiles)) <> " files"
    , "  Level 1 (Trivia/Ref):  " <> T.pack (show (length birLevel1TriviaFiles)) <> " files (0 tests)"
    , "  Level 2 (Logic Only):  " <> T.pack (show (length birLevel2LogicFiles)) <> " files (direct unit tests)"
    , "  Level 3 (Public API):  " <> T.pack (show (length birLevel3ApiFiles)) <> " files (CSR transitive cone)"
    , "  Transitive Callers:    " <> T.pack (show (length birTransitiveCallers)) <> " files"
    , "  Impacted Test Suites:  " <> T.pack (show (length birImpactedTests)) <> " / " <> T.pack (show birTotalRepoTests)
    , "  Saved CI Compute:      " <> T.pack (show birSavedComputePct) <> "%"
    , "--------------------------------------------------------------------------------"
    , "  CI Test Command:       " <> birTestCommand
    ]
    ++ section "Level 1 (Semantically Invariant / Safe Skip):" birLevel1TriviaFiles
    ++ section "Level 2 (Internal Logic Modified):" birLevel2LogicFiles
    ++ section "Level 3 (Public Interface / Dependency Modified):" birLevel3ApiFiles
    ++ section "Transitive Caller Files (CSR Cone):" birTransitiveCallers
    ++ section "Impacted Test Files:" birImpactedTests
  where
    section _ [] = []
    section title items =
      ["\n  " <> title] ++ ["    - " <> T.pack item | item <- items]

-- | Format 'BatchImpactResult' into machine-readable JSON for CI/CD pipelines.
formatBatchImpactResultJson :: BatchImpactResult -> Text
formatBatchImpactResultJson res =
  TE.decodeUtf8 (BL.toStrict (encode res))

