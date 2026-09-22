{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Canontra.CLI.Cache
Description : Cache maintenance tooling for canontra v0.1.0.

Provides subcommands for inspecting, verifying, cleaning, and pruning
the CNTR\x05 memory-mapped paged radix binary cache (.canontra/cache.bin).
-}
module Canontra.CLI.Cache
  ( CacheAction (..)
  , runCacheCommand
  ) where

import Control.Monad (filterM)
import Data.Aeson ((.=), object)
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Canontra.Cache.Common (MerkleCache (..))
import Canontra.Cache.PagedCache
  ( decodeBinaryCacheV5WithRecovery
  , readPagedCacheFileResilient
  , verifyHeaderCRC
  , writePagedCacheFile
  )

-- | Target operation for the 'canontra cache' command.
data CacheAction
  = CacheInfo
  | CacheVerify
  | CacheClean
  | CachePrune
  deriving stock (Eq, Ord, Show)

-- | Executes the requested cache action on the target directory.
runCacheCommand :: CacheAction -> FilePath -> Bool -> IO ()
runCacheCommand action rootDir asJson = do
  let cacheFile = rootDir </> ".canontra" </> "cache.bin"
  case action of
    CacheInfo   -> runInfo cacheFile asJson
    CacheVerify -> runVerify cacheFile asJson
    CacheClean  -> runClean cacheFile asJson
    CachePrune  -> runPrune rootDir cacheFile asJson

-- ============================================================================
-- Cache Info
-- ============================================================================
runInfo :: FilePath -> Bool -> IO ()
runInfo cacheFile asJson = do
  exists <- doesFileExist cacheFile
  if not exists
    then do
      if asJson
        then LBSC.putStrLn $ AesonPretty.encodePretty $ object
          [ "exists" .= False
          , "path"   .= cacheFile
          ]
        else do
          putStrLn "================================================================================"
          putStrLn "  CANONTRA CACHE INFO"
          putStrLn "================================================================================"
          putStrLn $ "  Cache File:          " ++ cacheFile
          putStrLn   "  Status:              Not initialized (no cache file exists)"
          putStrLn "================================================================================"
    else do
      rawBytes <- BS.readFile cacheFile
      let sizeBytes = BS.length rawBytes
          headerOk  = verifyHeaderCRC rawBytes
          (mCache, corrupted) = decodeBinaryCacheV5WithRecovery rawBytes
          entryCount = case mCache of
            Just c  -> Map.size (unMerkleCache c)
            Nothing -> 0
          slabCount = sizeBytes `div` 4096

      if asJson
        then LBSC.putStrLn $ AesonPretty.encodePretty $ object
          [ "exists"               .= True
          , "path"                 .= cacheFile
          , "size_bytes"           .= sizeBytes
          , "entries_count"        .= entryCount
          , "slab_pages_count"     .= slabCount
          , "format_version"       .= ("CNTR\\x05" :: T.Text)
          , "header_crc_valid"     .= headerOk
          , "corrupted_pages_count".= length corrupted
          ]
        else do
          putStrLn "================================================================================"
          putStrLn "  CANONTRA CACHE INFO"
          putStrLn "================================================================================"
          putStrLn $ "  Cache File:          " ++ cacheFile
          putStrLn $ "  Format Version:      CNTR\\x05 (4KB Paged Radix Cache)"
          putStrLn $ "  File Size:           " ++ show sizeBytes ++ " bytes (" ++ show (sizeBytes `div` 1024) ++ " KB)"
          putStrLn $ "  Indexed Files:       " ++ show entryCount ++ " records"
          putStrLn $ "  Slab Pages:          " ++ show slabCount ++ " pages (4096 bytes/page)"
          putStrLn $ "  Header CRC32:        " ++ (if headerOk then "VALID" else "CORRUPT")
          putStrLn $ "  Corrupted Pages:     " ++ show (length corrupted)
          putStrLn "================================================================================"

-- ============================================================================
-- Cache Verify
-- ============================================================================
runVerify :: FilePath -> Bool -> IO ()
runVerify cacheFile asJson = do
  exists <- doesFileExist cacheFile
  if not exists
    then do
      if asJson
        then LBSC.putStrLn $ AesonPretty.encodePretty $ object
          [ "status" .= ("missing" :: T.Text)
          , "path"   .= cacheFile
          , "error"  .= ("Cache file does not exist" :: T.Text)
          ]
        else do
          hPutStrLn stderr $ "Error: Cache file does not exist: " ++ cacheFile
      exitWith (ExitFailure 4)
    else do
      rawBytes <- BS.readFile cacheFile
      let sizeBytes = BS.length rawBytes
          headerOk  = verifyHeaderCRC rawBytes
          (_, corrupted) = decodeBinaryCacheV5WithRecovery rawBytes
          totalSlabs = sizeBytes `div` 4096
          validSlabs = totalSlabs - length corrupted
          isClean = headerOk && null corrupted

      if asJson
        then do
          LBSC.putStrLn $ AesonPretty.encodePretty $ object
            [ "status"            .= (if isClean then ("ok" :: T.Text) else "corrupt")
            , "path"              .= cacheFile
            , "header_crc_valid"  .= headerOk
            , "total_slab_pages"  .= totalSlabs
            , "valid_slab_pages"  .= validSlabs
            , "corrupt_slab_pages".= length corrupted
            , "corrupt_indices"   .= corrupted
            ]
          if isClean then pure () else exitWith (ExitFailure 1)
        else do
          putStrLn "================================================================================"
          putStrLn "  CANONTRA CACHE INTEGRITY VERIFICATION"
          putStrLn "================================================================================"
          putStrLn $ "  Target:              " ++ cacheFile
          putStrLn $ "  Header Checksum:     " ++ (if headerOk then "PASSED" else "FAILED")
          putStrLn $ "  Total Slab Pages:    " ++ show totalSlabs
          putStrLn $ "  Valid Pages:         " ++ show validSlabs
          putStrLn $ "  Corrupted Pages:     " ++ show (length corrupted)
          if not (null corrupted)
            then putStrLn $ "  Corrupted Indices:   " ++ show corrupted
            else pure ()
          putStrLn "--------------------------------------------------------------------------------"
          if isClean
            then do
              putStrLn "  Result:              ALL CHECKS PASSED (100% CRC32 Integrity)"
              putStrLn "================================================================================"
            else do
              putStrLn "  Result:              CORRUPTION DETECTED in cache slabs"
              putStrLn "================================================================================"
              exitWith (ExitFailure 1)

-- ============================================================================
-- Cache Clean
-- ============================================================================
runClean :: FilePath -> Bool -> IO ()
runClean cacheFile asJson = do
  exists <- doesFileExist cacheFile
  if exists
    then do
      removeFile cacheFile
      if asJson
        then LBSC.putStrLn $ AesonPretty.encodePretty $ object
          [ "status" .= ("cleaned" :: T.Text)
          , "path"   .= cacheFile
          ]
        else do
          putStrLn $ "Cache cleared successfully: " ++ cacheFile
    else do
      if asJson
        then LBSC.putStrLn $ AesonPretty.encodePretty $ object
          [ "status" .= ("not_found" :: T.Text)
          , "path"   .= cacheFile
          ]
        else do
          putStrLn $ "No active cache found at: " ++ cacheFile

-- ============================================================================
-- Cache Prune
-- ============================================================================
runPrune :: FilePath -> FilePath -> Bool -> IO ()
runPrune rootDir cacheFile asJson = do
  exists <- doesFileExist cacheFile
  if not exists
    then do
      if asJson
        then LBSC.putStrLn $ AesonPretty.encodePretty $ object
          [ "status" .= ("not_found" :: T.Text)
          , "path"   .= cacheFile
          ]
        else do
          putStrLn $ "No active cache to prune at: " ++ cacheFile
    else do
      cache <- readPagedCacheFileResilient cacheFile
      let allEntries = Map.toList (unMerkleCache cache)
          originalCount = length allEntries
      keptEntries <- filterM (\(normPath, _) -> doesFileExist (rootDir </> normPath)) allEntries
      let retainedCount = length keptEntries
          prunedCount   = originalCount - retainedCount

      if prunedCount > 0
        then do
          let updatedCache = MerkleCache (Map.fromList keptEntries)
          writePagedCacheFile cacheFile updatedCache
        else pure ()

      if asJson
        then LBSC.putStrLn $ AesonPretty.encodePretty $ object
          [ "status"           .= ("pruned" :: T.Text)
          , "path"             .= cacheFile
          , "original_entries" .= originalCount
          , "pruned_entries"   .= prunedCount
          , "retained_entries" .= retainedCount
          ]
        else do
          putStrLn "================================================================================"
          putStrLn "  CANONTRA CACHE PRUNE"
          putStrLn "================================================================================"
          putStrLn $ "  Cache File:          " ++ cacheFile
          putStrLn $ "  Initial Records:     " ++ show originalCount
          putStrLn $ "  Orphaned Records:    " ++ show prunedCount ++ " (removed)"
          putStrLn $ "  Retained Records:    " ++ show retainedCount
          putStrLn "================================================================================"
