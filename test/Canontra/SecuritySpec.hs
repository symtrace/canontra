{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Canontra.SecuritySpec
Description : Test suite for Air-Gapped Zero-Trust Security & Path Sandboxing (Phase 2).

Verifies:
1. Canonical Root Containment (canonicalizeSafePath rejects ../../etc/passwd and directory escapes).
2. Symlink Cycle Breaking (isSymlinkLoop detects cyclic directory references and avoids stack overflow).
3. Resource Ceilings (checkResourceBounds enforces <= 50MB file size and <= 64 directory recursion levels).
4. Repository Crawler Integration (discoverSourceFilesSafe skips ignored directories and enforces limits).
5. PagedCache CRC32 Page-Level Recovery (corrupted 4KB slab page discarded, valid pages preserved).
6. Gate 2 Security Boundary Exit Code 4 Verification.
-}
module Canontra.SecuritySpec (spec) where

import Data.Bits (shiftL, xor)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , findExecutable
  , getCurrentDirectory
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.Exit (ExitCode (..))
import System.FilePath ((</>), makeRelative)
import System.Process (readProcessWithExitCode)
import Test.Hspec

import Canontra.Cache.Common (MerkleCache (..), MerkleCacheEntry (..))
import Canontra.Cache.PagedCache
  ( decodeBinaryCacheV5
  , decodeBinaryCacheV5Resilient
  , decodeBinaryCacheV5WithRecovery
  , encodeBinaryCacheV5
  , readPagedCacheFile
  , verifyHeaderCRC
  , verifyPageCRC
  , writePagedCacheFile
  )
import Canontra.Repository.Repository
  ( discoverSourceFiles
  , discoverSourceFilesSafe
  )
import Canontra.Security.Path
  ( canonicalizeSafePath
  , checkResourceBounds
  , checkResourceBoundsWith
  , isPathContained
  , isSymlinkLoop
  , maxFileSizeBytes
  , maxRecursionDepth
  , normalizePathUniversal
  )
import Canontra.Types (Fingerprint (..), FingerprintBundle (..))

makeSampleBundle :: String -> FingerprintBundle
makeSampleBundle tag =
  FingerprintBundle
    (Fingerprint $ T.pack ("f0_" ++ tag))
    (Fingerprint $ T.pack ("f1_" ++ tag))
    (Fingerprint $ T.pack ("f2_" ++ tag))
    (Fingerprint $ T.pack ("f3_" ++ tag))
    (Fingerprint $ T.pack ("fcg_" ++ tag))
    (Fingerprint $ T.pack ("fcf_" ++ tag))
    (Fingerprint $ T.pack ("fdf_" ++ tag))
    (Fingerprint $ T.pack ("ft_" ++ tag))
    (Fingerprint $ T.pack ("f4_" ++ tag))

flipBitAt :: Int -> Int -> BS.ByteString -> BS.ByteString
flipBitAt byteIdx bitIdx bs
  | byteIdx < 0 || byteIdx >= BS.length bs = bs
  | otherwise =
      let (pfx, sfx) = BS.splitAt byteIdx bs
          targetByte = BS.head sfx
          flippedByte = targetByte `xor` (1 `shiftL` (bitIdx `mod` 8))
          rest = BS.tail sfx
      in pfx <> BS.singleton flippedByte <> rest

spec :: Spec
spec = do
  describe "Air-Gapped Zero-Trust Security & Path Sandboxing (Phase 2)" $ do

    -- ========================================================================
    -- 1. Canonical Root Containment & Directory Traversal Escapes
    -- ========================================================================
    describe "Canonical Root Containment (canonicalizeSafePath)" $ do
      it "accepts paths strictly inside the root directory" $ do
        cwd <- getCurrentDirectory
        res <- canonicalizeSafePath cwd "src/Canontra/Types.hs"
        case res of
          Left err -> expectationFailure ("Expected safe path, got error: " ++ err)
          Right safePath -> do
            canonExpected <- canonicalizePath (cwd </> "src/Canontra/Types.hs")
            safePath `shouldBe` canonExpected

      it "accepts paths with internal . and .. references that do not escape root" $ do
        cwd <- getCurrentDirectory
        res <- canonicalizeSafePath cwd "src/../src/Canontra/Types.hs"
        case res of
          Left err -> expectationFailure ("Expected safe path, got error: " ++ err)
          Right safePath -> do
            canonExpected <- canonicalizePath (cwd </> "src/Canontra/Types.hs")
            safePath `shouldBe` canonExpected

      it "rejects directory traversal escape: ../../etc/passwd" $ do
        cwd <- getCurrentDirectory
        res <- canonicalizeSafePath cwd "../../etc/passwd"
        case res of
          Left err -> err `shouldContain` "Security violation: path traverses outside root directory"
          Right path -> expectationFailure ("Expected escape rejection, but got: " ++ path)

      it "rejects deep directory traversal escapes: ../../../../windows/system32" $ do
        cwd <- getCurrentDirectory
        res <- canonicalizeSafePath cwd "../../../../windows/system32"
        case res of
          Left err -> err `shouldContain` "Security violation"
          Right path -> expectationFailure ("Expected escape rejection, but got: " ++ path)

      it "rejects paths containing null bytes" $ do
        cwd <- getCurrentDirectory
        res <- canonicalizeSafePath cwd "src/foo\0bar.py"
        case res of
          Left err -> err `shouldContain` "null byte"
          Right path -> expectationFailure ("Expected null byte rejection, but got: " ++ path)

      it "rejects sibling directories with common prefix" $ do
        tmpDir <- getTemporaryDirectory
        let baseRoot = tmpDir </> "canontra_sec_root"
            siblingDir = tmpDir </> "canontra_sec_root_other"
            siblingFile = siblingDir </> "secret.py"
        createDirectoryIfMissing True baseRoot
        createDirectoryIfMissing True siblingDir
        writeFile siblingFile "SECRET = 42\n"

        res <- canonicalizeSafePath baseRoot siblingFile
        case res of
          Left err -> err `shouldContain` "Security violation"
          Right path -> expectationFailure ("Expected sibling prefix rejection, but got: " ++ path)

        removeFile siblingFile
        removeDirectoryRecursive siblingDir
        removeDirectoryRecursive baseRoot

      it "isPathContained accurately evaluates path prefixes" $ do
        isPathContained "C:/project" "C:/project/src/lib.py" `shouldBe` True
        isPathContained "C:/project" "C:/project_other/lib.py" `shouldBe` False
        isPathContained "/home/user/repo" "/home/user/repo/app.js" `shouldBe` True
        isPathContained "/home/user/repo" "/etc/passwd" `shouldBe` False

    -- ========================================================================
    -- 2. Symlink Cycle Breaking
    -- ========================================================================
    describe "Symlink Cycle Breaker (isSymlinkLoop)" $ do
      it "returns loop=False for the initial visit to a directory" $ do
        cwd <- getCurrentDirectory
        (isLoop, visited1) <- isSymlinkLoop Set.empty cwd
        isLoop `shouldBe` False
        Set.size visited1 `shouldBe` 1

      it "returns loop=True when revisiting an already tracked directory" $ do
        cwd <- getCurrentDirectory
        (_, visited1) <- isSymlinkLoop Set.empty cwd
        (isLoop2, visited2) <- isSymlinkLoop visited1 cwd
        isLoop2 `shouldBe` True
        Set.size visited2 `shouldBe` 1

      it "tracks multiple distinct directories without false positives" $ do
        cwd <- getCurrentDirectory
        let sub1 = cwd </> "src"
            sub2 = cwd </> "test"
        (_, v1) <- isSymlinkLoop Set.empty sub1
        (loop2, v2) <- isSymlinkLoop v1 sub2
        loop2 `shouldBe` False
        Set.size v2 `shouldBe` 2
        (loop3, _) <- isSymlinkLoop v2 sub1
        loop3 `shouldBe` True

    -- ========================================================================
    -- 3. Resource Ceilings (File Size & Recursion Depth)
    -- ========================================================================
    describe "Resource Ceilings (checkResourceBounds)" $ do
      it "enforces constants: 50MB file size ceiling and 64-level directory depth" $ do
        maxFileSizeBytes `shouldBe` 52428800
        maxRecursionDepth `shouldBe` 64

      it "accepts standard repository files within 50MB" $ do
        res <- checkResourceBounds "canontra.cabal"
        res `shouldBe` Right ()

      it "rejects files exceeding configured size ceiling" $ do
        tmpDir <- getTemporaryDirectory
        let testFile = tmpDir </> "canontra_oversized.bin"
        BS.writeFile testFile (BS.replicate 200 0x41) -- 200 bytes
        -- Verify with 100-byte ceiling
        res <- checkResourceBoundsWith 100 64 testFile
        case res of
          Left err -> err `shouldContain` "Resource limit exceeded: file size"
          Right () -> expectationFailure "Expected file size ceiling rejection"
        removeFile testFile

      it "rejects directory paths exceeding 64 recursion levels" $ do
        let deepPath = concat (replicate 68 "nested/") ++ "file.py"
        res <- checkResourceBounds deepPath
        case res of
          Left err -> err `shouldContain` "Resource limit exceeded: directory nesting depth (69) exceeds ceiling of 64"
          Right () -> expectationFailure "Expected directory depth ceiling rejection"

      it "accepts directory paths within 64 recursion levels" $ do
        let safePath = concat (replicate 20 "nested/") ++ "file.py"
        res <- checkResourceBounds safePath
        res `shouldBe` Right ()

    -- ========================================================================
    -- 4. Repository Crawler Integration
    -- ========================================================================
    describe "Repository Crawler Integration (discoverSourceFilesSafe)" $ do
      it "skips standard ignored directories (.git, node_modules, .venv, .stack-work, .canontra)" $ do
        tmpDir <- getTemporaryDirectory
        let repoRoot = tmpDir </> "canontra_crawler_test"
            gitDir = repoRoot </> ".git"
            nodeDir = repoRoot </> "node_modules"
            srcDir = repoRoot </> "src"
            goodFile = srcDir </> "main.py"
            gitFile = gitDir </> "config.py"
            nodeFile = nodeDir </> "pkg.js"
        createDirectoryIfMissing True srcDir
        createDirectoryIfMissing True gitDir
        createDirectoryIfMissing True nodeDir
        writeFile goodFile "print('hello')\n"
        writeFile gitFile "print('git')\n"
        writeFile nodeFile "console.log('node');\n"

        files <- discoverSourceFiles repoRoot
        let normFiles = map (normalizePathUniversal . makeRelative repoRoot) files
        normFiles `shouldContain` ["src/main.py"]
        normFiles `shouldNotContain` [".git/config.py"]
        normFiles `shouldNotContain` ["node_modules/pkg.js"]

        removeDirectoryRecursive repoRoot

      it "rejects repository path with null bytes safely" $ do
        res <- discoverSourceFilesSafe "repo\0bad"
        case res of
          Left err -> err `shouldContain` "null byte"
          Right _ -> expectationFailure "Expected null byte rejection"

    -- ========================================================================
    -- 5. PagedCache CRC32 Page-Level Recovery
    -- ========================================================================
    describe "PagedCache Page-Level CRC32 Recovery" $ do
      it "discards only corrupted 4KB slab page and preserves valid pages with recovery" $ do
        -- Build a 15-entry cache spanning exactly 2 slab pages (14 records on page 1, 1 record on page 2)
        let entries = [ ("src/file" ++ show i ++ ".py", 100 + fromIntegral i, 1000, "f" ++ show i)
                      | i <- [1..15 :: Int]
                      ]
            entryMap = Map.fromList [ (p, MerkleCacheEntry sz mt (makeSampleBundle tag))
                                    | (p, sz, mt, tag) <- entries
                                    ]
            cache15 = MerkleCache entryMap
            bin15 = encodeBinaryCacheV5 cache15

        verifyHeaderCRC bin15 `shouldBe` True
        verifyPageCRC bin15 1 `shouldBe` True
        verifyPageCRC bin15 2 `shouldBe` True

        -- Inject bit flip into Page 1 record data (offset 4096 + 64)
        let corruptedBin = flipBitAt (4096 + 64) 2 bin15

        verifyHeaderCRC corruptedBin `shouldBe` True
        verifyPageCRC corruptedBin 1 `shouldBe` False -- Page 1 corrupted!
        verifyPageCRC corruptedBin 2 `shouldBe` True  -- Page 2 intact!

        -- Strict decodeBinaryCacheV5 rejects entire cache as expected by legacy contract
        decodeBinaryCacheV5 corruptedBin `shouldBe` Nothing

        -- Resilient decodeBinaryCacheV5WithRecovery recovers Page 2 records and reports Page 1 corruption!
        let (mRecovered, corruptedPages) = decodeBinaryCacheV5WithRecovery corruptedBin
        corruptedPages `shouldBe` [1]
        case mRecovered of
          Nothing -> expectationFailure "Expected successful page-level recovery"
          Just (MerkleCache recMap) -> do
            -- Page 1 (records 1..14) was discarded; Page 2 (record 15) was preserved!
            Map.size recMap `shouldBe` 1
            let recoveredPath = head (Map.keys recMap)
            let allPaths = [p | (p, _, _, _) <- entries]
            recoveredPath `shouldSatisfy` (`elem` allPaths)

        -- decodeBinaryCacheV5Resilient also succeeds and recovers without throwing
        mResilient <- decodeBinaryCacheV5Resilient corruptedBin
        case mResilient of
          Nothing -> expectationFailure "Expected resilient recovery"
          Just (MerkleCache resMap) -> do
            Map.size resMap `shouldBe` 1
            let resPath = head (Map.keys resMap)
            let allPaths = [p | (p, _, _, _) <- entries]
            resPath `shouldSatisfy` (`elem` allPaths)

      it "readPagedCacheFile transparently recovers uncorrupted records on slab bit-rot" $ do
        tmpDir <- getTemporaryDirectory
        let cacheDir = tmpDir </> "canontra_recovery_disk_test"
            cachePath = cacheDir </> "cache.bin"
        createDirectoryIfMissing True cacheDir

        let entries = [ ("pkg/module" ++ show i ++ ".go", 200 + fromIntegral i, 2000, "m" ++ show i)
                      | i <- [1..15 :: Int]
                      ]
            entryMap = Map.fromList [ (p, MerkleCacheEntry sz mt (makeSampleBundle tag))
                                    | (p, sz, mt, tag) <- entries
                                    ]
            cache15 = MerkleCache entryMap
        writePagedCacheFile cachePath cache15

        -- Corrupt Page 1 on disk
        rawBytes <- BS.readFile cachePath
        let corruptedOnDisk = flipBitAt (4096 + 70) 1 rawBytes
        BS.writeFile cachePath corruptedOnDisk

        -- readPagedCacheFile should recover valid Page 2 records instead of terminating
        recoveredCache <- readPagedCacheFile cachePath
        let (MerkleCache recMap) = recoveredCache
        Map.size recMap `shouldBe` 1
        let recoveredPath = head (Map.keys recMap)
        let allPaths = [p | (p, _, _, _) <- entries]
        recoveredPath `shouldSatisfy` (`elem` allPaths)

        removeDirectoryRecursive cacheDir

    -- ========================================================================
    -- 6. Gate 2 Security Boundary Exit Code 4 Verification
    -- ========================================================================
    describe "Gate 2 Security Boundary Verification" $ do
      it "exits with Exit Code 4 on directory traversal escape (../../etc/passwd)" $ do
        mExe <- findExecutable "canontra"
        case mExe of
          Nothing -> do
            -- If not installed in PATH, execute via stack exec
            (exitCode, _, errOut) <- readProcessWithExitCode "stack" ["exec", "--", "canontra", "fp", "../../etc/passwd"] ""
            exitCode `shouldBe` ExitFailure 4
            errOut `shouldContain` "CANONTRA SECURITY BOUNDARY VIOLATION"
          Just exePath -> do
            (exitCode, _, errOut) <- readProcessWithExitCode exePath ["fp", "../../etc/passwd"] ""
            exitCode `shouldBe` ExitFailure 4
            errOut `shouldContain` "CANONTRA SECURITY BOUNDARY VIOLATION"

      it "exits with Exit Code 4 when target file does not exist" $ do
        mExe <- findExecutable "canontra"
        case mExe of
          Nothing -> do
            (exitCode, _, errOut) <- readProcessWithExitCode "stack" ["exec", "--", "canontra", "fp", "non_existent_source_file_98765.py"] ""
            exitCode `shouldBe` ExitFailure 4
            errOut `shouldContain` "CANONTRA I/O ERROR"
          Just exePath -> do
            (exitCode, _, errOut) <- readProcessWithExitCode exePath ["fp", "non_existent_source_file_98765.py"] ""
            exitCode `shouldBe` ExitFailure 4
            errOut `shouldContain` "CANONTRA I/O ERROR"
