{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Canontra.Repository.Parallel
Description : Pure Haskell work-stealing parallel repository processor.

Distributes file fingerprinting tasks dynamically across all available CPU cores
(-N capabilities) using fine-grained 4x over-partitioned Vector slices. Eliminates
thread core starvation caused by uneven file sizes and scales linearly with zero
lock contention.
-}
module Canontra.Repository.Parallel
  ( parMapChunks
  , parFingerprintFiles
  , parFingerprintWorkStealing
  , parFingerprintWithPrograms
  ) where

import Control.Concurrent.Async (forConcurrently)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import GHC.Conc (getNumCapabilities)
import System.FilePath ((</>))

import Canontra.Fingerprint.Bundle (computeBundle, computeBundleAndProgram)
import Canontra.IR.Program (Program)
import Canontra.Types (FileEntry (..), ParseError)

-- | Distribute items across lightweight threads using dynamic capability-aware work-stealing chunks.
parMapChunks :: (a -> IO b) -> [a] -> IO [b]
parMapChunks _ [] = pure []
parMapChunks f items = do
  numCores <- getNumCapabilities
  let !vec = V.fromList items
      !total = V.length vec
      !chunkSize = max 1 (total `quot` (numCores * 4))
      !numChunks = (total + chunkSize - 1) `quot` chunkSize
      !slices = [ V.slice (i * chunkSize) (min chunkSize (total - i * chunkSize)) vec
                | i <- [0 .. numChunks - 1]
                ]
  results <- forConcurrently slices $ \slice ->
    V.mapM f slice
  pure (concatMap V.toList results)

-- | Dynamic work-stealing file fingerprinting across all CPU capabilities.
parFingerprintWorkStealing :: FilePath -> [FilePath] -> IO [Either ParseError FileEntry]
parFingerprintWorkStealing rootDir relPaths =
  parMapChunks processFile relPaths
  where
    processFile relPath = do
      let fullPath = rootDir </> relPath
      rawBytes <- BS.readFile fullPath
      let textContent = TE.decodeUtf8Lenient rawBytes
      case computeBundle relPath rawBytes textContent of
        Left err     -> pure (Left err)
        Right bundle -> pure (Right (FileEntry relPath bundle))

-- | Compute fingerprints for multiple files in parallel using dynamic work-stealing.
parFingerprintFiles :: FilePath -> [FilePath] -> IO [Either ParseError FileEntry]
parFingerprintFiles = parFingerprintWorkStealing

-- | Dynamic work-stealing file fingerprinting returning both FileEntry and parsed Program.
parFingerprintWithPrograms :: FilePath -> [FilePath] -> IO [Either ParseError (FileEntry, Program)]
parFingerprintWithPrograms rootDir relPaths =
  parMapChunks processFile relPaths
  where
    processFile relPath = do
      let fullPath = rootDir </> relPath
      rawBytes <- BS.readFile fullPath
      let textContent = TE.decodeUtf8Lenient rawBytes
      case computeBundleAndProgram relPath rawBytes textContent of
        Left err          -> pure (Left err)
        Right (bundle, p) -> pure (Right (FileEntry relPath bundle, p))
