{- |
Module      : Canontra.Repository.Repository
Description : Deterministic polyglot repository traversal and aggregated Merkle fingerprinting.

A repository is more than the sum of its files: it is an ordered collection.
By sorting file paths and aggregating individual structural fingerprints into
a canonical tree hash (F_R), canontra guarantees that filesystem traversal order
never affects the resulting repository identity across Python, JS, TS, Go, and Rust.
Includes cross-platform POSIX path normalization and incremental caching.
-}
module Canontra.Repository.Repository
  ( fingerprintDirectory
  , fingerprintDirectoryWithCache
  , computeRepositoryFingerprint
  , computeWholeRepoBundle
  , discoverSourceFiles
  , discoverSourceFilesSafe
  , discoverPythonFiles
  , formatRepositoryManifest
  , normalizePathPosix
  , normalizePathCanonical
  ) where

import Control.Exception (IOException, try)
import Control.Monad (forM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (toLower)
import Data.Either (partitionEithers)
import Data.List (foldl', sort)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), makeRelative, takeDirectory, takeExtension)
import System.IO (hPutStrLn, stderr)

import Canontra.Security.Path (canonicalizeSafePath, checkResourceBounds, isSymlinkLoop, maxRecursionDepth)
import Canontra.Cache.Inode (getFileMetadata)
import Canontra.Cache.MerkleCache (defaultCachePath, insertCache, lookupCache, readMerkleCache, writeMerkleCache)
import Canontra.Fingerprint.Bundle (computeBundle)
import Canontra.Fingerprint.Source (hashBytes)
import Canontra.Fingerprint.WholeRepoCallGraph (computeFWCG)
import Canontra.Fingerprint.WholeRepoDataFlow (computeFWDF)
import Canontra.IR.Program (Program)
import Canontra.Normalize.Rules (engineName, engineVersion)
import Canontra.Repository.Parallel (parFingerprintFiles, parFingerprintWithPrograms, parMapChunks)
import Canontra.Types

repoGraphsCachePath :: FilePath -> FilePath
repoGraphsCachePath rootDir = rootDir </> ".canontra" </> "repo_graphs.txt"

saveRepoGraphs :: FilePath -> Fingerprint -> Fingerprint -> IO ()
saveRepoGraphs rootDir fwcg fwdf = do
  let p = repoGraphsCachePath rootDir
  createDirectoryIfMissing True (takeDirectory p)
  writeFile p (T.unpack (unFingerprint fwcg) ++ "\n" ++ T.unpack (unFingerprint fwdf))

loadRepoGraphs :: FilePath -> IO (Maybe (Fingerprint, Fingerprint))
loadRepoGraphs rootDir = do
  let p = repoGraphsCachePath rootDir
  exists <- doesFileExist p
  if not exists
    then pure Nothing
    else do
      content <- readFile p
      case lines content of
        (c:d:_) -> pure (Just (Fingerprint (T.pack c), Fingerprint (T.pack d)))
        _       -> pure Nothing

-- | Universal cross-platform canonical path normalization (POSIX forward slashes + case folding).
normalizePathCanonical :: FilePath -> FilePath
normalizePathCanonical = map (\c -> if c == '\\' then '/' else toLower c)

-- | Normalize Windows backslashes to standard POSIX forward slashes.
normalizePathPosix :: FilePath -> FilePath
normalizePathPosix = map (\c -> if c == '\\' then '/' else c)

fingerprintDirectory :: FilePath -> IO (Either ParseError RepositoryManifest)
fingerprintDirectory rootDir = fingerprintDirectoryWithCache rootDir False

fingerprintDirectoryWithCache :: FilePath -> Bool -> IO (Either ParseError RepositoryManifest)
fingerprintDirectoryWithCache rootDir useCache = do
  srcFiles <- discoverSourceFiles rootDir
  let sortedRelPaths = sort (map (makeRelative rootDir) srcFiles)
  let cachePath = defaultCachePath rootDir
  if not useCache
    then do
      results <- parFingerprintWithPrograms rootDir sortedRelPaths
      let (failures, successes) = partitionEithers results
          fileEntries = map fst successes
          progs = [(normalizePathCanonical (fePath fe), p) | (fe, p) <- successes]
      mapM_ logFailure failures
      if null fileEntries && not (null sortedRelPaths)
        then case failures of
          (e:_) -> pure (Left e)
          []    -> pure (Left (ParseError rootDir 0 0 "No files could be parsed"))
        else do
          let repoFp = computeRepositoryFingerprint fileEntries
              fwcg = computeFWCG progs
              fwdf = computeFWDF progs
          saveRepoGraphs rootDir fwcg fwdf
          let manifest = RepositoryManifest
                { rmEngine = engineName
                , rmVersion = engineVersion
                , rmRepositoryFingerprint = repoFp
                , rmWholeRepoCallGraph = Just fwcg
                , rmWholeRepoDataFlow = Just fwdf
                , rmFiles = fileEntries
                }
          pure (Right manifest)
    else do
      actualCache <- readMerkleCache cachePath
      results <- parMapChunks (processFileCachedConcurrent rootDir actualCache) sortedRelPaths
      let rawEntries = map fst results
          (failures, fileEntries) = partitionEithers rawEntries
          newEntries = [item | (Right _, Just item) <- results]
          newCache = foldl' (\c (p, m, b) -> insertCache p m b c) actualCache newEntries
      mapM_ logFailure failures
      if null fileEntries && not (null sortedRelPaths)
        then case failures of
          (e:_) -> pure (Left e)
          []    -> pure (Left (ParseError rootDir 0 0 "No files could be parsed"))
        else do
          writeMerkleCache cachePath newCache
          let repoFp = computeRepositoryFingerprint fileEntries
          mSaved <- if null newEntries then loadRepoGraphs rootDir else pure Nothing
          (fwcg, fwdf) <- case mSaved of
            Just graphs -> pure graphs
            Nothing -> do
              resProgs <- parFingerprintWithPrograms rootDir sortedRelPaths
              let (_, succs) = partitionEithers resProgs
                  progs = [(normalizePathCanonical (fePath fe), p) | (fe, p) <- succs]
                  c = computeFWCG progs
                  d = computeFWDF progs
              saveRepoGraphs rootDir c d
              pure (c, d)
          let manifest = RepositoryManifest
                { rmEngine = engineName
                , rmVersion = engineVersion
                , rmRepositoryFingerprint = repoFp
                , rmWholeRepoCallGraph = Just fwcg
                , rmWholeRepoDataFlow = Just fwdf
                , rmFiles = fileEntries
                }
          pure (Right manifest)
  where
    logFailure err =
      hPutStrLn stderr $ "[Canontra Parse Warning] " ++ peFile err ++ ":" ++ show (peLine err) ++ ":" ++ show (peColumn err) ++ ": " ++ T.unpack (peReason err)

    processFileCachedConcurrent rDir cache relPath = do
      let fullPath = rDir </> relPath
          normPath = normalizePathCanonical relPath
      mMeta <- getFileMetadata fullPath
      case mMeta of
        Nothing -> do
          rawBytes <- BS.readFile fullPath
          let textContent = TE.decodeUtf8Lenient rawBytes
          case computeBundle relPath rawBytes textContent of
            Left err -> pure (Left err, Nothing)
            Right b  -> pure (Right (FileEntry relPath b), Nothing)
        Just meta -> case lookupCache normPath meta cache of
          Just cachedBundle ->
            pure (Right (FileEntry relPath cachedBundle), Nothing)
          Nothing -> do
            rawBytes <- BS.readFile fullPath
            let textContent = TE.decodeUtf8Lenient rawBytes
            case computeBundle relPath rawBytes textContent of
              Left err -> pure (Left err, Nothing)
              Right b  -> pure (Right (FileEntry relPath b), Just (normPath, meta, b))

computeRepositoryFingerprint :: [FileEntry] -> Fingerprint
computeRepositoryFingerprint entries =
  let pairs = [(normalizePathCanonical (fePath e), unFingerprint (f1Structural (feFingerprints e))) | e <- entries]
      sortedPairs = sort pairs
      serialized = BSC.pack $ concatMap (\(p, f) -> p ++ ":" ++ T.unpack f ++ ";") sortedPairs
  in hashBytes serialized

-- | Compute whole-repository bundle combining F_R, F_WCG, F_WDF, and F_W4.
computeWholeRepoBundle :: [(FilePath, Program)] -> [FileEntry] -> WholeRepoBundle
computeWholeRepoBundle progs entries =
  let fr = computeRepositoryFingerprint entries
      fwcg = computeFWCG progs
      fwdf = computeFWDF progs
      combined = TE.encodeUtf8 (unFingerprint fr <> unFingerprint fwcg <> unFingerprint fwdf)
      fw4 = hashBytes combined
  in WholeRepoBundle fr fwcg fwdf fw4


discoverSourceFilesSafe :: FilePath -> IO (Either String [FilePath])
discoverSourceFilesSafe rootDir
  | any (== '\0') rootDir = pure (Left "Security violation: repository path contains null byte")
  | otherwise = do
      isDir <- doesDirectoryExist rootDir
      if not isDir
        then do
          isFile <- doesFileExist rootDir
          if isFile && isSupportedExt (takeExtension rootDir)
            then do
              bCheck <- checkResourceBounds rootDir
              case bCheck of
                Left err -> pure (Left err)
                Right () -> pure (Right [rootDir])
            else pure (Right [])
        else do
          eCanonRoot <- try (canonicalizePath rootDir) :: IO (Either IOException FilePath)
          case eCanonRoot of
            Left err -> pure (Left ("Failed to canonicalize repository root: " ++ show err))
            Right canonRoot -> do
              (_, initVisited) <- isSymlinkLoop Set.empty canonRoot
              files <- traverseDir canonRoot initVisited 0 rootDir
              pure (Right files)
  where
    traverseDir canonRoot visited depth currentDir
      | depth >= maxRecursionDepth = pure []
      | otherwise = do
          contentsRes <- try (listDirectory currentDir) :: IO (Either IOException [FilePath])
          case contentsRes of
            Left _ -> pure []
            Right contents -> do
              let filtered = filter (`notElem` ignoredDirs) contents
              fpaths <- forM filtered $ \item -> do
                let full = currentDir </> item
                isSubDir <- doesDirectoryExist full
                if isSubDir
                  then do
                    contained <- canonicalizeSafePath canonRoot full
                    case contained of
                      Left _ -> pure [] -- Symlink pointing outside repository root
                      Right _ -> do
                        (isLoop, newVisited) <- isSymlinkLoop visited full
                        if isLoop
                          then pure [] -- Symlink cycle detected and broken safely!
                          else traverseDir canonRoot newVisited (depth + 1) full
                  else if isSupportedExt (takeExtension item)
                    then do
                      contained <- canonicalizeSafePath canonRoot full
                      case contained of
                        Left _ -> pure [] -- Symlink file pointing outside repository root
                        Right _ -> do
                          bounds <- checkResourceBounds full
                          case bounds of
                            Left _ -> pure [] -- File size exceeds 50MB ceiling
                            Right () -> pure [full]
                    else pure []
              pure (concat fpaths)

discoverSourceFiles :: FilePath -> IO [FilePath]
discoverSourceFiles dir = do
  res <- discoverSourceFilesSafe dir
  case res of
    Left _ -> pure []
    Right files -> pure files

discoverPythonFiles :: FilePath -> IO [FilePath]
discoverPythonFiles = discoverSourceFiles

isSupportedExt :: String -> Bool
isSupportedExt ext = ext `elem`
  [ ".py", ".pyi"
  , ".js", ".jsx", ".mjs", ".cjs"
  , ".ts", ".tsx"
  , ".go"
  , ".rs"
  ]

ignoredDirs :: [FilePath]
ignoredDirs =
  [ ".git", ".hg", ".svn", "__pycache__", ".venv", "venv", ".mypy_cache"
  , ".pytest_cache", ".tox", ".stack-work", "node_modules", "target", "vendor", "dist", "build", ".canontra"
  ]

formatRepositoryManifest :: RepositoryManifest -> T.Text
formatRepositoryManifest rm =
  T.unlines $
    [ "================================================================================"
    , "  CANONTRA DETERMINISTIC REPOSITORY MANIFEST"
    , "================================================================================"
    , "  Repository Hash (F_R): " <> unFingerprint (rmRepositoryFingerprint rm)
    , "  Indexed File Count:    " <> T.pack (show (length (rmFiles rm))) <> " source files"
    , "  Engine:                " <> rmEngine rm <> " " <> rmVersion rm
    , "--------------------------------------------------------------------------------"
    , "  Indexed Source File                               Structural Fingerprint (F1)"
    , "--------------------------------------------------------------------------------"
    ]
    ++ map (\fe -> "  " <> T.justifyLeft 50 ' ' (T.pack (fePath fe)) <> unFingerprint (f1Structural (feFingerprints fe))) (rmFiles rm)
    ++ [ "================================================================================" ]
