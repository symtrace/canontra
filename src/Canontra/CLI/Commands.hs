{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Canontra.CLI.Commands
Description : Command-line argument parsing and command dispatch for v0.1.0.

This module provides the entrypoint parser for all canontra CLI operations,
dispatching commands for polyglot multi-tier fingerprinting (including stdin streaming),
invariant comparison, fine-grained structural diff diagnostics, graph inspections,
determinism verification, repository manifest generation, incremental caching,
cache maintenance (info, verify, clean, prune), machine interchange exports (SARIF, DOT),
shell autocompletions (bash, zsh, fish, powershell), and git history evolution tracking.
-}
module Canontra.CLI.Commands
  ( Command (..)
  , OutputFormat (..)
  , ExportFormat (..)
  , runCLI
  , parseCLIArgs
  , cliParserInfo
  ) where

import Control.Monad (forM)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.FilePath (makeRelative)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

import Canontra.Analysis.CallGraph
import Canontra.Analysis.CFG (buildCFGs, formatCFG)
import Canontra.Analysis.DFG (buildDFGs, formatDFG)
import Canontra.Analysis.Impact
  ( computeImpactSlice
  , findMatchingTests
  , formatImpactSlice
  , formatImpactSliceJson
  )
import Canontra.Analysis.Scope (analyzeProgramScope)
import Canontra.Analysis.WholeRepoGraph (buildWholeRepoCallGraph)
import Canontra.CLI.Cache (CacheAction (..), runCacheCommand)
import Canontra.CLI.Completions
  ( ShellType (..)
  , generateCompletionScript
  , parseShellType
  )
import Canontra.Comparison.Compare
import Canontra.Comparison.Diff
import Canontra.Export.Graph (exportCallGraphDOT, exportCFGDOT, exportDFGDOT)
import Canontra.Export.SARIF (exportDiffSARIF, renderSARIF)
import Canontra.Fingerprint.Bundle (computeBundle, computeManifest)
import Canontra.Fingerprint.Dependency (extractRichDependencyGraph)
import Canontra.IR.Program (Program)
import Canontra.Normalize.Rules (engineName, engineVersion)
import Canontra.Parser.Polyglot (parsePolyglotSource)
import Canontra.Repository.Git
import Canontra.Repository.Repository
import Canontra.Repository.Watcher (WatcherConfig (..), runTerminalWatcher)
import Canontra.Security.Path (canonicalizeSafePath)
import qualified Canontra.Types as CT
import Canontra.Types
import Canontra.Verification.Determinism

data OutputFormat = FormatHuman | FormatJSON | FormatHash
  deriving stock (Eq, Show)

data ExportFormat = ExportSARIF | ExportDOT
  deriving stock (Eq, Show)

data Command
  = CmdFingerprint FilePath (Maybe String) OutputFormat
  | CmdCompare FilePath FilePath Bool Bool
  | CmdDiff FilePath FilePath Bool
  | CmdGraph FilePath Bool Bool Bool Bool Bool Bool -- path, showScope, showCalls, showDeps, showCFG, showDFG, asJson
  | CmdVerify FilePath Int Bool
  | CmdRepository FilePath Bool Bool -- path, asJson, useCache
  | CmdImpact FilePath (Maybe FilePath) FilePath Bool -- targetFile, mBaseFile, repoDir, asJson
  | CmdSlice String FilePath Bool                     -- symbolQuery, repoDir, asJson
  | CmdCommit String Bool
  | CmdEvolution String String Bool
  | CmdWatch FilePath Int Int Bool -- path, debounceMs, pollMs, verbose
  | CmdCache CacheAction FilePath Bool
  | CmdExport FilePath ExportFormat (Maybe FilePath) (Maybe FilePath) (Maybe String)
  | CmdCompletions ShellType
  | CmdVersion
  | CmdAuto FilePath OutputFormat Bool -- path, fmt, useCache
  deriving stock (Eq, Show)

-- | Output security violation to stderr and exit with POSIX Exit Code 4.
outputSecurityError :: String -> IO a
outputSecurityError msg = do
  hPutStrLn stderr "================================================================================"
  hPutStrLn stderr "  CANONTRA SECURITY BOUNDARY VIOLATION"
  hPutStrLn stderr "================================================================================"
  hPutStrLn stderr $ "  " ++ msg
  hPutStrLn stderr "================================================================================"
  exitWith (ExitFailure 4)

-- | Output I/O error to stderr and exit with POSIX Exit Code 4.
outputIOError :: String -> IO a
outputIOError msg = do
  hPutStrLn stderr "================================================================================"
  hPutStrLn stderr "  CANONTRA I/O ERROR"
  hPutStrLn stderr "================================================================================"
  hPutStrLn stderr $ "  " ++ msg
  hPutStrLn stderr "================================================================================"
  exitWith (ExitFailure 4)

-- | Output parse error to stderr and exit with POSIX Exit Code 3.
outputParseError :: CT.ParseError -> IO a
outputParseError err = do
  hPutStrLn stderr "================================================================================"
  hPutStrLn stderr "  CANONTRA PARSE ERROR"
  hPutStrLn stderr "================================================================================"
  hPutStrLn stderr $ "  File:              " ++ peFile err
  hPutStrLn stderr $ "  Location:          Line " ++ show (peLine err) ++ ", Column " ++ show (peColumn err)
  hPutStrLn stderr $ "  Diagnostic:        " ++ T.unpack (peReason err)
  hPutStrLn stderr "================================================================================"
  exitWith (ExitFailure 3)

-- | Validate candidate path against current repository root containment.
validateSafePath :: FilePath -> IO FilePath
validateSafePath p
  | p == "-" = pure p
  | otherwise = do
      res <- canonicalizeSafePath "." p
      case res of
        Left secErr -> outputSecurityError secErr
        Right safeP -> pure safeP

toLowerChar :: Char -> Char
toLowerChar c
  | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
  | otherwise            = c

runCLI :: IO ()
runCLI = do
  cmd <- execParser cliParserInfo
  executeCommand cmd

executeCommand :: Command -> IO ()
executeCommand cmd = case cmd of
  CmdVersion -> do
    putStrLn $ T.unpack (engineName <> " version " <> engineVersion)
    exitSuccess

  CmdFingerprint path mLang fmt -> do
    p <- validateSafePath path
    runFingerprint p mLang fmt

  CmdAuto path fmt useCache -> do
    if path == "-"
      then runFingerprint "-" Nothing fmt
      else do
        p <- validateSafePath path
        isFile <- doesFileExist p
        isDir <- doesDirectoryExist p
        if isFile
          then runFingerprint p Nothing fmt
          else if isDir
            then runRepository p (fmt == FormatJSON) useCache
            else outputIOError ("Path does not exist: " ++ p)

  CmdCompare path1 path2 showDiff asJson -> do
    p1 <- validateSafePath path1
    p2 <- validateSafePath path2
    if showDiff
      then runDiff p1 p2 asJson
      else do
        res <- compareFiles p1 p2
        case res of
          Left err -> outputParseError err
          Right cr -> do
            if asJson
              then LBSC.putStrLn (AesonPretty.encodePretty cr)
              else TIO.putStrLn (formatComparisonResult cr)
            if crComposite cr == Identical
              then exitSuccess
              else exitWith (ExitFailure 1)

  CmdDiff path1 path2 asJson -> do
    p1 <- validateSafePath path1
    p2 <- validateSafePath path2
    runDiff p1 p2 asJson

  CmdGraph path showScope showCalls showDeps showCFG showDFG asJson -> do
    p <- validateSafePath path
    exists <- doesFileExist p
    if not exists
      then outputIOError ("File not found: " ++ p)
      else do
        rawBytes <- BS.readFile p
        let textContent = TE.decodeUtf8Lenient rawBytes
        case parsePolyglotSource p textContent of
          Left err -> outputParseError err
          Right prog -> do
            let defaultAll = not showScope && not showCalls && not showDeps && not showCFG && not showDFG
            if asJson
              then do
                let cg   = buildCallGraph prog
                    sc   = analyzeProgramScope prog
                    rdg  = extractRichDependencyGraph prog
                    cfgs = buildCFGs prog
                    dfgs = buildDFGs prog
                LBSC.putStrLn $ AesonPretty.encodePretty $ Aeson.object
                  [ "call_graph"       .= cg
                  , "scope_tree"       .= sc
                  , "dependency_graph" .= rdg
                  , "control_flow"     .= cfgs
                  , "data_flow"        .= dfgs
                  ]
              else do
                if showCalls || defaultAll
                  then TIO.putStrLn (formatCallGraph (buildCallGraph prog))
                  else pure ()
                if showScope
                  then LBSC.putStrLn (AesonPretty.encodePretty (analyzeProgramScope prog))
                  else pure ()
                if showDeps
                  then LBSC.putStrLn (AesonPretty.encodePretty (extractRichDependencyGraph prog))
                  else pure ()
                if showCFG
                  then mapM_ (TIO.putStrLn . formatCFG) (buildCFGs prog)
                  else pure ()
                if showDFG
                  then mapM_ (TIO.putStrLn . formatDFG) (buildDFGs prog)
                  else pure ()

  CmdVerify path runs asJson -> do
    p <- validateSafePath path
    exists <- doesFileExist p
    if not exists
      then outputIOError ("File not found: " ++ p)
      else do
        rawBytes <- BS.readFile p
        let textContent = TE.decodeUtf8Lenient rawBytes
        case verifyDeterminism runs p textContent of
          Left err -> outputParseError err
          Right vr -> do
            if asJson
              then LBSC.putStrLn (AesonPretty.encodePretty vr)
              else TIO.putStrLn (formatVerificationResult vr)
            if vrDeterministic vr then exitSuccess else exitWith (ExitFailure 1)

  CmdRepository path asJson useCache -> do
    p <- validateSafePath path
    runRepository p asJson useCache

  CmdCommit rev asJson -> do
    res <- fingerprintGitRevision "." rev
    case res of
      Left err -> outputIOError err
      Right manifest ->
        if asJson
          then LBSC.putStrLn (AesonPretty.encodePretty manifest)
          else TIO.putStrLn (formatRepositoryManifest manifest)

  CmdEvolution rev1 rev2 asJson -> do
    res <- compareGitEvolution "." rev1 rev2
    case res of
      Left err -> outputIOError err
      Right comp ->
        if asJson
          then LBSC.putStrLn (AesonPretty.encodePretty comp)
          else TIO.putStrLn (formatEvolutionComparison comp)

  CmdWatch path debounceMs pollMs verbose -> do
    p <- validateSafePath path
    isDir <- doesDirectoryExist p
    if not isDir
      then outputIOError ("Directory does not exist: " ++ p)
      else do
        let cfg = WatcherConfig debounceMs pollMs verbose
        runTerminalWatcher cfg p

  CmdImpact targetPath mBasePath repoDir asJson -> do
    t <- validateSafePath targetPath
    mb <- mapM validateSafePath mBasePath
    r <- validateSafePath repoDir
    runImpact t mb r asJson

  CmdSlice symbolQuery repoDir asJson -> do
    r <- validateSafePath repoDir
    runSlice symbolQuery r asJson

  CmdCache cAct dir asJson -> do
    d <- validateSafePath dir
    runCacheCommand cAct d asJson

  CmdExport path fmt mOut mBase mGraph -> do
    runExport path fmt mOut mBase mGraph

  CmdCompletions shell -> do
    TIO.putStrLn (generateCompletionScript shell)
    exitSuccess

runDiff :: FilePath -> FilePath -> Bool -> IO ()
runDiff path1 path2 asJson = do
  b1 <- if path1 == "-" then BS.getContents else BS.readFile path1
  b2 <- if path2 == "-" then BS.getContents else BS.readFile path2
  let t1 = TE.decodeUtf8Lenient b1
      t2 = TE.decodeUtf8Lenient b2
  case (parsePolyglotSource path1 t1, parsePolyglotSource path2 t2) of
    (Left err, _) -> outputParseError err
    (_, Left err) -> outputParseError err
    (Right p1, Right p2) -> do
      let diffRes = diffPrograms p1 p2
      if asJson
        then LBSC.putStrLn (AesonPretty.encodePretty diffRes)
        else TIO.putStrLn (formatDiffResult diffRes)
      let isClean = crComposite (drComparison diffRes) == Identical
                    && null (drDeclarationDiffs diffRes)
                    && null (drDependencyDiffs diffRes)
                    && null (drStructuralDiffs diffRes)
                    && null (drCallGraphDiffs diffRes)
                    && null (drCFGDiffs diffRes)
                    && null (drDFGDiffs diffRes)
      if isClean
        then exitSuccess
        else exitWith (ExitFailure 1)

runFingerprint :: FilePath -> Maybe String -> OutputFormat -> IO ()
runFingerprint path mLang fmt
  | path == "-" = do
      rawBytes <- BS.getContents
      let textContent = TE.decodeUtf8Lenient rawBytes
          synthPath = case map toLowerChar (maybe "python" id mLang) of
            "python"     -> "stdin.py"
            "py"         -> "stdin.py"
            "typescript" -> "stdin.ts"
            "ts"         -> "stdin.ts"
            "javascript" -> "stdin.js"
            "js"         -> "stdin.js"
            "go"         -> "stdin.go"
            "rust"       -> "stdin.rs"
            "rs"         -> "stdin.rs"
            _            -> "stdin.py"
      case computeManifest synthPath rawBytes textContent of
        Left err -> outputParseError err
        Right manifest -> renderManifest "<stdin>" manifest
  | otherwise = do
      exists <- doesFileExist path
      if not exists
        then outputIOError ("File not found: " ++ path)
        else do
          rawBytes <- BS.readFile path
          let textContent = TE.decodeUtf8Lenient rawBytes
          case computeManifest path rawBytes textContent of
            Left err -> outputParseError err
            Right manifest -> renderManifest path manifest
  where
    renderManifest displayPath manifest = case fmt of
      FormatJSON -> LBSC.putStrLn (AesonPretty.encodePretty manifest)
      FormatHash -> putStrLn $ T.unpack (unFingerprint (f4Composite (mFingerprints manifest)))
      FormatHuman -> do
        let fps = mFingerprints manifest
        putStrLn "  CANONTRA DETERMINISTIC MULTI-TIER FINGERPRINT MANIFEST"
        putStrLn "================================================================================"
        putStrLn $ "  Target File:       " ++ displayPath
        putStrLn $ "  Language:          " ++ T.unpack (mLanguage manifest)
        putStrLn $ "  Engine Version:    " ++ T.unpack engineName ++ " " ++ T.unpack engineVersion
        putStrLn "--------------------------------------------------------------------------------"
        putStrLn "  Tier                               Fingerprint Digest (BLAKE3 / SHA-256)"
        putStrLn "--------------------------------------------------------------------------------"
        putStrLn $ "  F0  (Source Code):                 " ++ T.unpack (unFingerprint (f0Source fps))
        putStrLn $ "  F1  (Normalized AST):              " ++ T.unpack (unFingerprint (f1Structural fps))
        putStrLn $ "  F2  (Declaration Hierarchy):       " ++ T.unpack (unFingerprint (f2Declaration fps))
        putStrLn $ "  F3  (Dependency Graph):            " ++ T.unpack (unFingerprint (f3Dependency fps))
        putStrLn $ "  FCG (Intra-Module Call Graph):     " ++ T.unpack (unFingerprint (fCGCallGraph fps))
        putStrLn $ "  FCF (Control-Flow Graph):          " ++ T.unpack (unFingerprint (fCFControlFlow fps))
        putStrLn $ "  FDF (Data-Flow SSA Graph):         " ++ T.unpack (unFingerprint (fDFDataFlow fps))
        putStrLn $ "  FT  (Type Contract):               " ++ T.unpack (unFingerprint (fTTypeContract fps))
        putStrLn "--------------------------------------------------------------------------------"
        putStrLn $ "  F4  (Composite Program Hash):      " ++ T.unpack (unFingerprint (f4Composite fps))
        putStrLn "================================================================================"

runExport :: FilePath -> ExportFormat -> Maybe FilePath -> Maybe FilePath -> Maybe String -> IO ()
runExport filePath fmt mOut mBase mGraph = case fmt of
  ExportSARIF -> do
    (normTarget, newProg) <- if filePath == "-"
      then do
        rawBytes <- BS.getContents
        let textContent = TE.decodeUtf8Lenient rawBytes
        case parsePolyglotSource "stdin.py" textContent of
          Left err -> outputParseError err
          Right p  -> pure ("<stdin>", p)
      else do
        p <- validateSafePath filePath
        exists <- doesFileExist p
        if not exists
          then outputIOError ("File not found: " ++ p)
          else do
            rawBytes <- BS.readFile p
            let textContent = TE.decodeUtf8Lenient rawBytes
            case parsePolyglotSource p textContent of
              Left err -> outputParseError err
              Right prog -> pure (p, prog)

    diffRes <- case mBase of
      Just baseFile -> do
        b <- validateSafePath baseFile
        bExists <- doesFileExist b
        if not bExists
          then outputIOError ("Base file not found: " ++ b)
          else do
            rawOld <- BS.readFile b
            let txtOld = TE.decodeUtf8Lenient rawOld
            case parsePolyglotSource b txtOld of
              Left err -> outputParseError err
              Right oldProg -> pure (diffPrograms oldProg newProg)
      Nothing -> do
        (exitCode, stdoutStr, _) <- readProcessWithExitCode "git" ["show", "HEAD:" ++ normTarget] ""
        if exitCode == ExitSuccess
          then do
            let rawOld = BSC.pack stdoutStr
                txtOld = TE.decodeUtf8Lenient rawOld
            case parsePolyglotSource normTarget txtOld of
              Left _        -> pure (diffPrograms newProg newProg)
              Right oldProg -> pure (diffPrograms oldProg newProg)
          else pure (diffPrograms newProg newProg)

    let sarifVal = exportDiffSARIF normTarget diffRes
        rendered = renderSARIF sarifVal
    case mOut of
      Just outPath -> TIO.writeFile outPath rendered
      Nothing      -> TIO.putStrLn rendered
    exitSuccess

  ExportDOT -> do
    prog <- if filePath == "-"
      then do
        rawBytes <- BS.getContents
        let textContent = TE.decodeUtf8Lenient rawBytes
        case parsePolyglotSource "stdin.py" textContent of
          Left err -> outputParseError err
          Right p  -> pure p
      else do
        p <- validateSafePath filePath
        exists <- doesFileExist p
        if not exists
          then outputIOError ("File not found: " ++ p)
          else do
            rawBytes <- BS.readFile p
            let textContent = TE.decodeUtf8Lenient rawBytes
            case parsePolyglotSource p textContent of
              Left err -> outputParseError err
              Right pr -> pure pr

    let dotContent = case mGraph of
          Just "cfg"  -> exportCFGDOT (buildCFGs prog)
          Just "dfg"  -> exportDFGDOT (buildDFGs prog)
          _           -> exportCallGraphDOT (buildCallGraph prog)

    case mOut of
      Just outPath -> TIO.writeFile outPath dotContent
      Nothing      -> TIO.putStrLn dotContent
    exitSuccess

runRepository :: FilePath -> Bool -> Bool -> IO ()
runRepository path asJson useCache = do
  res <- fingerprintDirectoryWithCache path useCache
  case res of
    Left err -> outputParseError err
    Right manifest ->
      if asJson
        then LBSC.putStrLn (AesonPretty.encodePretty manifest)
        else TIO.putStrLn (formatRepositoryManifest manifest)

loadRepoPrograms :: FilePath -> [FilePath] -> IO [(FilePath, Program)]
loadRepoPrograms repoDir fullPaths = do
  results <- forM fullPaths $ \full -> do
    exists <- doesFileExist full
    if not exists
      then pure Nothing
      else do
        raw <- BS.readFile full
        let txt = TE.decodeUtf8Lenient raw
            rel = normalizePathPosix (makeRelative repoDir full)
        case parsePolyglotSource rel txt of
          Left _     -> pure Nothing
          Right prog -> pure (Just (rel, prog))
  pure [item | Just item <- results]

runImpact :: FilePath -> Maybe FilePath -> FilePath -> Bool -> IO ()
runImpact targetPath mBasePath repoDir asJson = do
  targetExists <- doesFileExist targetPath
  if not targetExists
    then outputIOError ("Target file does not exist: " ++ targetPath)
    else do
      repoExists <- doesDirectoryExist repoDir
      if not repoExists
        then outputIOError ("Repository directory does not exist: " ++ repoDir)
        else do
          rawNew <- BS.readFile targetPath
          let txtNew = TE.decodeUtf8Lenient rawNew
              normTarget = normalizePathPosix (makeRelative repoDir targetPath)
          case computeBundle normTarget rawNew txtNew of
            Left err -> outputParseError err
            Right newBundle -> do
              mOldBundle <- case mBasePath of
                Just basePath -> do
                  baseExists <- doesFileExist basePath
                  if not baseExists
                    then outputIOError ("Base file does not exist: " ++ basePath)
                    else do
                      rawOld <- BS.readFile basePath
                      let txtOld = TE.decodeUtf8Lenient rawOld
                      case computeBundle normTarget rawOld txtOld of
                        Left err -> outputParseError err
                        Right b  -> pure (Just b)
                Nothing -> do
                  (exitCode, stdoutStr, _) <- readProcessWithExitCode "git" ["-C", repoDir, "show", "HEAD:" ++ normTarget] ""
                  if exitCode == ExitSuccess
                    then do
                      let rawOld = BSC.pack stdoutStr
                          txtOld = TE.decodeUtf8Lenient rawOld
                      case computeBundle normTarget rawOld txtOld of
                        Left _  -> pure Nothing
                        Right b -> pure (Just b)
                    else pure Nothing

              allRepoFullFiles <- discoverSourceFiles repoDir
              let normAllFiles = map (normalizePathPosix . makeRelative repoDir) allRepoFullFiles
              progs <- loadRepoPrograms repoDir allRepoFullFiles
              let wcg = buildWholeRepoCallGraph progs
                  oldBundle = case mOldBundle of
                    Just b  -> b
                    Nothing -> newBundle
                  slice = computeImpactSlice normTarget oldBundle newBundle wcg normAllFiles

              if asJson
                then TIO.putStrLn (formatImpactSliceJson slice)
                else TIO.putStrLn (formatImpactSlice slice)

runSlice :: String -> FilePath -> Bool -> IO ()
runSlice symbolQuery repoDir asJson = do
  repoExists <- doesDirectoryExist repoDir
  if not repoExists
    then outputIOError ("Repository directory does not exist: " ++ repoDir)
    else do
      allRepoFullFiles <- discoverSourceFiles repoDir
      let normAllFiles = map (normalizePathPosix . makeRelative repoDir) allRepoFullFiles
      progs <- loadRepoPrograms repoDir allRepoFullFiles
      let wcg = buildWholeRepoCallGraph progs
          qText = T.pack symbolQuery
          allSyms = wcgNodes wcg
          exactMatches = filter (\s -> symDeclName s == qText || (symModule s <> "." <> symDeclName s) == qText) allSyms
          candidateMatches = if null exactMatches
            then filter (\s -> qText `T.isInfixOf` symDeclName s || qText `T.isInfixOf` symModule s) allSyms
            else exactMatches

      if null candidateMatches
        then do
          if asJson
            then LBSC.putStrLn (AesonPretty.encodePretty (Aeson.object ["error" .= ("Symbol not found: " ++ symbolQuery), "query" .= symbolQuery]))
            else do
              putStrLn "================================================================================"
              putStrLn "  CANONTRA SEMANTIC SYMBOL SLICE"
              putStrLn "================================================================================"
              putStrLn $ "  Query:           " ++ symbolQuery
              putStrLn $ "  Status:          SYMBOL NOT FOUND IN REPOSITORY"
              putStrLn $ "  Indexed Symbols: " ++ show (length allSyms) ++ " total symbols"
              putStrLn "================================================================================"
              exitWith (ExitFailure 1)
        else do
          let targetSym = head candidateMatches
              directCallers = sort (nub [wceCaller e | e <- wcgEdges wcg, wceCallee e == targetSym])
              invAdj = Map.fromListWith (++) [(wceCallee e, [wceCaller e]) | e <- wcgEdges wcg]
              bfs [] _ acc = acc
              bfs (curr:queue) visited acc
                | Set.member curr visited = bfs queue visited acc
                | otherwise =
                    let callers = Map.findWithDefault [] curr invAdj
                        newVisited = Set.insert curr visited
                        newAcc = if curr /= targetSym then curr : acc else acc
                    in bfs (queue ++ callers) newVisited newAcc
              transitiveCallers = sort (nub (bfs [targetSym] Set.empty []))
              impactedFiles = sort (nub (map (normalizePathPosix . symFilePath) (directCallers ++ transitiveCallers)))
              impactedTests = findMatchingTests (normalizePathPosix (symFilePath targetSym) : impactedFiles) normAllFiles
              callees = sort (nub [wceCallee e | e <- wcgEdges wcg, wceCaller e == targetSym])

          if asJson
            then do
              let jsonOutput = Aeson.object
                    [ "query"               .= symbolQuery
                    , "symbol"              .= symDeclName targetSym
                    , "module"             .= symModule targetSym
                    , "file"                .= symFilePath targetSym
                    , "kind"                .= show (symKind targetSym)
                    , "declaration_hash"    .= unFingerprint (symTier2 targetSym)
                    , "direct_callers"      .= directCallers
                    , "transitive_callers"  .= transitiveCallers
                    , "impacted_files"      .= impactedFiles
                    , "impacted_tests"      .= impactedTests
                    , "callees"             .= callees
                    ]
              LBSC.putStrLn (AesonPretty.encodePretty jsonOutput)
            else do
              putStrLn "================================================================================"
              putStrLn "  CANONTRA SEMANTIC SYMBOL SLICE"
              putStrLn "================================================================================"
              putStrLn $ "  Target Symbol:       " ++ T.unpack (symModule targetSym) ++ ":" ++ T.unpack (symDeclName targetSym)
              putStrLn $ "  Declaration Kind:    " ++ show (symKind targetSym)
              putStrLn $ "  Defined In:          " ++ symFilePath targetSym
              putStrLn $ "  Declaration Hash:    " ++ T.unpack (unFingerprint (symTier2 targetSym))
              putStrLn $ "  Direct Callers:      " ++ show (length directCallers) ++ " callers"
              putStrLn $ "  Transitive Callers:  " ++ show (length transitiveCallers) ++ " symbols"
              putStrLn $ "  Impacted Files:      " ++ show (length impactedFiles) ++ " files"
              putStrLn $ "  Covering Tests:      " ++ show (length impactedTests) ++ " test suites"
              putStrLn "--------------------------------------------------------------------------------"
              if null directCallers
                then putStrLn "  Direct External Callers:     None (Root entrypoint or dead symbol)"
                else do
                  putStrLn "  Direct External Callers:"
                  mapM_ (\s -> putStrLn $ "    - " ++ T.unpack (symModule s) ++ ":" ++ T.unpack (symDeclName s) ++ " (" ++ symFilePath s ++ ")") directCallers

              if null transitiveCallers
                then pure ()
                else do
                  putStrLn ""
                  putStrLn "  Transitive Caller Chain:"
                  mapM_ (\s -> putStrLn $ "    - " ++ T.unpack (symModule s) ++ ":" ++ T.unpack (symDeclName s) ++ " (" ++ symFilePath s ++ ")") (take 15 transitiveCallers)
                  if length transitiveCallers > 15
                    then putStrLn $ "      ... and " ++ show (length transitiveCallers - 15) ++ " more"
                    else pure ()

              if null impactedFiles
                then pure ()
                else do
                  putStrLn ""
                  putStrLn "  Impacted Repository Files:"
                  mapM_ (\f -> putStrLn $ "    - " ++ f) impactedFiles

              if null impactedTests
                then pure ()
                else do
                  putStrLn ""
                  putStrLn "  Recommended Test Slices:"
                  mapM_ (\t -> putStrLn $ "    - " ++ t) impactedTests

              if null callees
                then pure ()
                else do
                  putStrLn ""
                  putStrLn "  Outgoing Callee Dependencies:"
                  mapM_ (\s -> putStrLn $ "    - " ++ T.unpack (symModule s) ++ ":" ++ T.unpack (symDeclName s)) callees

              putStrLn "================================================================================"

cliParserInfo :: ParserInfo Command
cliParserInfo = info (parseCLIArgs <**> helper)
  ( fullDesc
  <> progDesc "canontra - High-Throughput Polyglot Deterministic Program Fingerprinting & Deep Semantic Graph Engine"
  <> header "canontra v0.1.0"
  )

parseCLIArgs :: Parser Command
parseCLIArgs =
  subparser
    (  command "fingerprint" (info (parseFingerprint <**> helper) (progDesc "Compute deterministic multi-tier fingerprints for a file or '-' for stdin"))
    <> command "fp"          (info (parseFingerprint <**> helper) (progDesc "Alias for fingerprint"))
    <> command "compare"     (info (parseCompare <**> helper) (progDesc "Compare fingerprints between two files"))
    <> command "diff"        (info (parseDiff <**> helper) (progDesc "Generate fine-grained structural and semantic diff diagnostics"))
    <> command "graph"       (info (parseGraph <**> helper) (progDesc "Inspect call graph, CFG, DFG, scope tree, or dependency graph"))
    <> command "verify"      (info (parseVerify <**> helper) (progDesc "Verify repeat-execution determinism"))
    <> command "repository"  (info (parseRepository <**> helper) (progDesc "Compute aggregated repository fingerprint"))
    <> command "repo"        (info (parseRepository <**> helper) (progDesc "Alias for repository"))
    <> command "impact"      (info (parseImpact <**> helper) (progDesc "Compute fine-grained semantic change impact slice (CIA)"))
    <> command "slice"       (info (parseSlice <**> helper) (progDesc "Trace upstream caller slice and downstream dependencies for a symbol"))
    <> command "watch"       (info (parseWatch <**> helper) (progDesc "Start interactive live terminal Merkle DAG watcher session"))
    <> command "w"           (info (parseWatch <**> helper) (progDesc "Alias for watch"))
    <> command "commit"      (info (parseCommit <**> helper) (progDesc "Fingerprint repository at a git commit"))
    <> command "evolution"   (info (parseEvolution <**> helper) (progDesc "Compare repository evolution across two git revisions"))
    <> command "cache"       (info (parseCache <**> helper) (progDesc "Inspect, verify, clean, or prune incremental binary cache"))
    <> command "export"      (info (parseExport <**> helper) (progDesc "Export diagnostics (SARIF v2.1.0) or graphs (Graphviz DOT)"))
    <> command "completions" (info (parseCompletions <**> helper) (progDesc "Generate shell autocompletions (bash, zsh, fish, powershell)"))
    <> command "version"     (info (pure CmdVersion <**> helper) (progDesc "Display engine version"))
    )
  <|> parseAuto

parseFingerprint :: Parser Command
parseFingerprint = CmdFingerprint
  <$> argument str (metavar "FILE" <> help "Source file (Python, JS, TS, Go, Rust) or '-' for stdin")
  <*> optional (strOption (long "language" <> short 'l' <> metavar "LANG" <> help "Language for stdin stream (python, typescript, javascript, go, rust)"))
  <*> parseOutputFormat

parseCompare :: Parser Command
parseCompare = CmdCompare
  <$> argument str (metavar "FILE1" <> help "First source file")
  <*> argument str (metavar "FILE2" <> help "Second source file")
  <*> switch (long "diff" <> help "Include fine-grained structural diff diagnostics")
  <*> switch (long "json" <> help "Output comparison in JSON format")

parseDiff :: Parser Command
parseDiff = CmdDiff
  <$> argument str (metavar "FILE1" <> help "First source file")
  <*> argument str (metavar "FILE2" <> help "Second source file")
  <*> switch (long "json" <> help "Output diff diagnostics in JSON format")

parseGraph :: Parser Command
parseGraph = CmdGraph
  <$> argument str (metavar "FILE" <> help "Source file")
  <*> switch (long "scope" <> help "Display lexical scope tree")
  <*> switch (long "calls" <> help "Display intra-module call graph")
  <*> switch (long "deps"  <> help "Display resolved dependency graph")
  <*> switch (long "cfg"   <> help "Display control-flow graph (CFG)")
  <*> switch (long "dfg"   <> help "Display data-flow graph (DFG)")
  <*> switch (long "json"  <> help "Output graph representation in JSON")

parseVerify :: Parser Command
parseVerify = CmdVerify
  <$> argument str (metavar "FILE" <> help "Source file to verify")
  <*> option auto (long "runs" <> short 'n' <> value 10 <> showDefault <> help "Number of verification iterations")
  <*> switch (long "json" <> help "Output verification in JSON format")

parseRepository :: Parser Command
parseRepository = CmdRepository
  <$> argument str (metavar "DIR" <> help "Directory to fingerprint")
  <*> switch (long "json" <> help "Output repository manifest in JSON format")
  <*> switch (long "cache" <> help "Enable incremental Merkle state cache (.canontra/cache.bin)")

parseWatch :: Parser Command
parseWatch = CmdWatch
  <$> argument str (metavar "DIR" <> value "." <> showDefault <> help "Target directory to watch live in foreground")
  <*> option auto (long "debounce-ms" <> value 50 <> showDefault <> help "Event debouncing window in ms")
  <*> option auto (long "poll-ms" <> value 100 <> showDefault <> help "Filesystem polling interval in ms")
  <*> switch (long "verbose" <> short 'v' <> help "Enable verbose logging")

parseCommit :: Parser Command
parseCommit = CmdCommit
  <$> argument str (metavar "REVISION" <> help "Git revision (e.g. HEAD, HEAD~1)")
  <*> switch (long "json" <> help "Output manifest in JSON format")

parseEvolution :: Parser Command
parseEvolution = CmdEvolution
  <$> argument str (metavar "REV1" <> help "Earlier Git revision")
  <*> argument str (metavar "REV2" <> help "Later Git revision")
  <*> switch (long "json" <> help "Output evolution in JSON format")

parseImpact :: Parser Command
parseImpact = CmdImpact
  <$> argument str (metavar "FILE" <> help "Target modified source file to analyze")
  <*> optional (strOption (long "base" <> short 'b' <> metavar "BASE_FILE" <> help "Baseline version of the file to compare against (defaults to git HEAD)"))
  <*> strOption (long "repo" <> short 'r' <> metavar "DIR" <> value "." <> showDefault <> help "Repository root directory")
  <*> switch (long "json" <> help "Output impact slice in JSON format for CI/CD test runners")

parseSlice :: Parser Command
parseSlice = CmdSlice
  <$> argument str (metavar "SYMBOL" <> help "Target symbol name (e.g. 'verify' or 'auth.verify') to slice across repository")
  <*> strOption (long "repo" <> short 'r' <> metavar "DIR" <> value "." <> showDefault <> help "Repository root directory")
  <*> switch (long "json" <> help "Output symbol slice in JSON format")

parseCache :: Parser Command
parseCache = CmdCache
  <$> subparser
        (  command "info"   (info (pure CacheInfo)   (progDesc "Inspect cache statistics and slab page counts"))
        <> command "verify" (info (pure CacheVerify) (progDesc "Verify IEEE 802.3 CRC32 integrity across all cache slab pages"))
        <> command "clean"  (info (pure CacheClean)  (progDesc "Remove .canontra/cache.bin"))
        <> command "prune"  (info (pure CachePrune)  (progDesc "Remove orphaned cache entries for files deleted from disk"))
        )
  <*> strOption (long "dir" <> short 'd' <> metavar "DIR" <> value "." <> showDefault <> help "Repository root directory")
  <*> switch (long "json" <> help "Output cache operation in JSON format")

parseExportFormat :: Parser ExportFormat
parseExportFormat =
  option (eitherReader parseFmt)
    ( long "format"
    <> short 'f'
    <> metavar "FORMAT"
    <> value ExportSARIF
    <> showDefaultWith (\case ExportSARIF -> "sarif"; ExportDOT -> "dot")
    <> help "Export format (sarif, dot)"
    )
  where
    parseFmt s = case map toLowerChar s of
      "sarif" -> Right ExportSARIF
      "dot"   -> Right ExportDOT
      _       -> Left $ "Unknown export format: " ++ s ++ " (expected 'sarif' or 'dot')"

parseExport :: Parser Command
parseExport = CmdExport
  <$> argument str (metavar "FILE" <> help "Source file to export diagnostics or graphs for")
  <*> parseExportFormat
  <*> optional (strOption (long "output" <> short 'o' <> metavar "OUT_FILE" <> help "Write export output to file instead of stdout"))
  <*> optional (strOption (long "base" <> short 'b' <> metavar "BASE_FILE" <> help "Baseline file to diff against for SARIF export (defaults to git HEAD)"))
  <*> optional (strOption (long "graph" <> short 'g' <> metavar "GRAPH_TYPE" <> help "Graph to export for DOT: calls (default), cfg, dfg"))

parseCompletions :: Parser Command
parseCompletions = CmdCompletions
  <$> argument (eitherReader parseShell) (metavar "SHELL" <> help "Target shell: bash, zsh, fish, powershell")
  where
    parseShell s = case parseShellType s of
      Just sh -> Right sh
      Nothing -> Left $ "Unknown shell: " ++ s ++ " (supported: bash, zsh, fish, powershell)"

parseAuto :: Parser Command
parseAuto = CmdAuto
  <$> argument str (metavar "TARGET" <> help "File or directory path")
  <*> parseOutputFormat
  <*> switch (long "cache" <> help "Enable incremental Merkle state cache")

parseOutputFormat :: Parser OutputFormat
parseOutputFormat =
  flag' FormatJSON (long "json" <> help "Output as JSON manifest")
  <|> flag' FormatHash (long "hash" <> short 'q' <> help "Output only the composite hash")
  <|> pure FormatHuman
