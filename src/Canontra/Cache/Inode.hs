{- |
Module      : Canontra.Cache.Inode
Description : Fast OS file metadata, size, and modification timestamp extractor.

Provides rapid file metadata retrieval for validating in-memory and on-disk
cache entries in sub-microsecond time.
-}
module Canontra.Cache.Inode
  ( FileMetadata (..)
  , getFileMetadata
  , isMetadataUnchanged
  ) where

import Control.DeepSeq (NFData)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
import System.Directory (doesFileExist, getFileSize, getModificationTime)

data FileMetadata = FileMetadata
  { fmPath  :: FilePath
  , fmSize  :: Integer
  , fmMtime :: Integer -- POSIX seconds integer
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Extract file size and modification time for a file path.
getFileMetadata :: FilePath -> IO (Maybe FileMetadata)
getFileMetadata path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      sz <- getFileSize path
      mtime <- getModificationTime path
      let posixMtime = round (utcTimeToPOSIXSeconds mtime)
      pure $ Just (FileMetadata path sz posixMtime)

-- | Check if metadata matches existing cached metadata.
isMetadataUnchanged :: FileMetadata -> FileMetadata -> Bool
isMetadataUnchanged m1 m2 =
  fmSize m1 == fmSize m2 && fmMtime m1 == fmMtime m2
