{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Canontra.Security.Path
Description : Air-gapped zero-trust path sandboxing, symlink cycle breaking, and resource ceilings.

Provides pure Haskell, platform-invariant path containment and security boundaries:
- Canonical root containment to prevent directory traversal escapes (../../etc/passwd).
- Visited (DeviceID, FileID) pair tracking to break recursive symlink / junction loops without stack overflow.
- Resource ceiling enforcement: file size ceiling (50 MB) and directory recursion depth limit (<= 64 levels).
-}
module Canontra.Security.Path
  ( DeviceID
  , FileID
  , maxFileSizeBytes
  , maxRecursionDepth
  , canonicalizeSafePath
  , isSymlinkLoop
  , checkResourceBounds
  , checkResourceBoundsWith
  , isPathContained
  , normalizePathUniversal
  ) where

import Control.Exception (IOException, try)
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import System.Directory (canonicalizePath, doesFileExist, getFileSize)
import System.FilePath (isRelative, splitDirectories, takeDrive, (</>))

import Canontra.Cache.Common (fastPathHash64)

-- | 64-bit Device/Volume identifier.
type DeviceID = Word64

-- | 64-bit File/Inode identifier.
type FileID = Word64

-- | Maximum file size ceiling: 50 MB (52,428,800 bytes).
maxFileSizeBytes :: Integer
maxFileSizeBytes = 50 * 1024 * 1024

-- | Maximum directory recursion nesting depth ceiling: 64 levels.
maxRecursionDepth :: Int
maxRecursionDepth = 64

-- | Universal cross-platform path normalization (forward slashes + lowercasing).
normalizePathUniversal :: FilePath -> FilePath
normalizePathUniversal = map (\c -> if c == '\\' then '/' else toLower c)

-- | Remove any non-root trailing slash from a normalized path.
stripTrailingSlash :: FilePath -> FilePath
stripTrailingSlash p
  | p == "/" = "/"
  | length p == 3 && p !! 1 == ':' && p !! 2 == '/' = p -- e.g. "c:/"
  | not (null p) && last p == '/' = init p
  | otherwise = p

-- | Pure check whether a candidate canonical path is strictly contained within a root canonical path.
isPathContained :: FilePath -> FilePath -> Bool
isPathContained rootPath candidatePath =
  let !normRoot = stripTrailingSlash (normalizePathUniversal rootPath)
      !normCand = stripTrailingSlash (normalizePathUniversal candidatePath)
      !prefix = if normRoot == "/" || (length normRoot == 3 && normRoot !! 1 == ':' && normRoot !! 2 == '/')
                  then normRoot
                  else normRoot ++ "/"
  in normCand == normRoot || prefix `isPrefixOf` normCand

-- | Verifies that a resolved file path resides strictly within the specified root directory.
-- Returns 'Right canonicalPath' on success or 'Left errorMessage' on directory traversal escape.
canonicalizeSafePath :: FilePath -> FilePath -> IO (Either String FilePath)
canonicalizeSafePath rootDir candidatePath
  | any (== '\0') rootDir = pure (Left "Security violation: root directory path contains null byte")
  | any (== '\0') candidatePath = pure (Left "Security violation: candidate path contains null byte")
  | otherwise = do
      eRoot <- try (canonicalizePath rootDir) :: IO (Either IOException FilePath)
      case eRoot of
        Left err -> pure (Left ("Security violation: failed to resolve root directory: " ++ show err))
        Right canonRoot -> do
          let targetPath = if isRelative candidatePath
                             then rootDir </> candidatePath
                             else candidatePath
          eCand <- try (canonicalizePath targetPath) :: IO (Either IOException FilePath)
          case eCand of
            Left err -> pure (Left ("Security violation: failed to resolve candidate path: " ++ show err))
            Right canonCand ->
              if isPathContained canonRoot canonCand
                then pure (Right canonCand)
                else pure (Left ("Security violation: path traverses outside root directory: "
                                 ++ candidatePath ++ " (resolved to " ++ canonCand
                                 ++ ", root directory is " ++ canonRoot ++ ")"))

-- | Detects whether a directory has already been visited in the traversal chain, breaking symlink cycles.
isSymlinkLoop :: Set (DeviceID, FileID) -> FilePath -> IO (Bool, Set (DeviceID, FileID))
isSymlinkLoop visited dir = do
  eCanon <- try (canonicalizePath dir) :: IO (Either IOException FilePath)
  case eCanon of
    Left _ -> pure (True, visited) -- Treat unresolvable/recursive loop as loop
    Right canonDir -> do
      let !norm = stripTrailingSlash (normalizePathUniversal canonDir)
          !drive = takeDrive norm
          !devId = fastPathHash64 (TE.encodeUtf8 (T.pack drive))
          !fileId = fastPathHash64 (TE.encodeUtf8 (T.pack norm))
          !pair = (devId, fileId)
      if Set.member pair visited
        then pure (True, visited)
        else pure (False, Set.insert pair visited)

-- | Verifies resource bounds: file size <= 50MB and directory nesting depth <= 64.
checkResourceBounds :: FilePath -> IO (Either String ())
checkResourceBounds = checkResourceBoundsWith maxFileSizeBytes maxRecursionDepth

-- | Parameterized resource bound verification.
checkResourceBoundsWith :: Integer -> Int -> FilePath -> IO (Either String ())
checkResourceBoundsWith maxBytes maxDepth path = do
  let !normalized = map (\c -> if c == '\\' then '/' else c) path
      !comps = filter (\c -> not (null c) && c /= "." && c /= "/") (splitDirectories normalized)
      !depth = length comps
  if depth > maxDepth
    then pure (Left ("Resource limit exceeded: directory nesting depth ("
                     ++ show depth ++ ") exceeds ceiling of " ++ show maxDepth))
    else do
      isFile <- doesFileExist path
      if isFile
        then do
          sz <- getFileSize path
          if sz > maxBytes
            then pure (Left ("Resource limit exceeded: file size ("
                             ++ show sz ++ " bytes) exceeds ceiling of "
                             ++ show maxBytes ++ " bytes (50MB)"))
            else pure (Right ())
        else pure (Right ())
