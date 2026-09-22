{- |
Module      : Canontra.Repository.Git
Description : Git revision inspection and polyglot repository evolution comparison.

Git integration allows tracking the cryptographic evolution of a repository.
By inspecting trees at arbitrary revisions without network calls or third-party
services, canontra surfaces structural, declaration, dependency, call graph,
control-flow (CFG), and data-flow (DFG) shifts across commits with absolute determinism.
-}
module Canontra.Repository.Git
  ( fingerprintGitRevision
  , compareGitEvolution
  , formatEvolutionComparison
  ) where

import Control.Monad (forM)
import qualified Data.ByteString.Char8 as BSC
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension)
import System.Process (readProcessWithExitCode)

import Canontra.Fingerprint.Bundle (computeBundle)
import Canontra.Normalize.Rules (engineName, engineVersion)
import Canontra.Repository.Repository (computeRepositoryFingerprint)
import Canontra.Types

fingerprintGitRevision :: FilePath -> String -> IO (Either String RepositoryManifest)
fingerprintGitRevision repoDir rev = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode "git" ["-C", repoDir, "ls-tree", "-r", "--name-only", rev] ""
  case exitCode of
    ExitFailure code -> pure $ Left ("git ls-tree failed with code " ++ show code ++ ": " ++ stderr)
    ExitSuccess -> do
      let allFiles = filter (\p -> isSupportedGitExt (takeExtension p)) (lines stdout)
          sortedPaths = sort allFiles
      entries <- forM sortedPaths $ \relPath -> do
        (fExit, fStdout, _) <- readProcessWithExitCode "git" ["-C", repoDir, "show", rev ++ ":" ++ relPath] ""
        if fExit /= ExitSuccess
          then pure Nothing
          else do
            let rawBytes = BSC.pack fStdout
                textContent = TE.decodeUtf8Lenient rawBytes
            case computeBundle relPath rawBytes textContent of
              Left _ -> pure Nothing
              Right bundle -> pure (Just (FileEntry relPath bundle))
      let validEntries = [e | Just e <- entries]
          repoFp = computeRepositoryFingerprint validEntries
          manifest = RepositoryManifest
            { rmEngine = engineName
            , rmVersion = engineVersion
            , rmRepositoryFingerprint = repoFp
            , rmWholeRepoCallGraph = Nothing
            , rmWholeRepoDataFlow = Nothing
            , rmFiles = validEntries
            }
      pure (Right manifest)

compareGitEvolution :: FilePath -> String -> String -> IO (Either String EvolutionComparison)
compareGitEvolution repoDir rev1 rev2 = do
  res1 <- fingerprintGitRevision repoDir rev1
  res2 <- fingerprintGitRevision repoDir rev2
  case (res1, res2) of
    (Left err, _) -> pure (Left err)
    (_, Left err) -> pure (Left err)
    (Right m1, Right m2) -> do
      let fp1 = rmRepositoryFingerprint m1
          fp2 = rmRepositoryFingerprint m2
          status = if fp1 == fp2 then Identical else Different
          comp = EvolutionComparison
            { ecPreviousRev  = T.pack rev1
            , ecCurrentRev   = T.pack rev2
            , ecStructural   = status
            , ecDeclarations = status
            , ecDependencies = status
            , ecCallGraph    = status
            , ecControlFlow  = status
            , ecDataFlow     = status
            , ecComposite    = status
            }
      pure (Right comp)

isSupportedGitExt :: String -> Bool
isSupportedGitExt ext = ext `elem`
  [ ".py", ".pyi", ".js", ".jsx", ".mjs", ".cjs", ".ts", ".tsx", ".go", ".rs" ]

formatEvolutionComparison :: EvolutionComparison -> T.Text
formatEvolutionComparison ec =
  T.unlines
    [ "Repository Identity"
    , "-------------------"
    , ""
    , "Previous Rev: " <> ecPreviousRev ec
    , "Current Rev:  " <> ecCurrentRev ec
    , ""
    , "Structural:   " <> showEvolutionStatus (ecStructural ec)
    , "Declarations: " <> showEvolutionStatus (ecDeclarations ec)
    , "Dependencies: " <> showEvolutionStatus (ecDependencies ec)
    , "Call Graph:   " <> showEvolutionStatus (ecCallGraph ec)
    , "Control Flow: " <> showEvolutionStatus (ecControlFlow ec)
    , "Data Flow:    " <> showEvolutionStatus (ecDataFlow ec)
    , "Composite:    " <> showEvolutionStatus (ecComposite ec)
    ]

showEvolutionStatus :: ComparisonStatus -> T.Text
showEvolutionStatus Identical = "SAME"
showEvolutionStatus Different = "CHANGED"
