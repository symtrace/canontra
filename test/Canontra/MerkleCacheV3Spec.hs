{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Canontra.MerkleCacheV3Spec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Bits (shiftL, xor)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, listDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import Test.Hspec
import Test.QuickCheck

import Canontra.Cache.Inode (FileMetadata (..))
import Canontra.Cache.MerkleCache
import Canontra.Repository.Repository (computeRepositoryFingerprint)
import Canontra.Types (FileEntry (..), Fingerprint (..), FingerprintBundle (..))

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
    (Fingerprint "")
    (Fingerprint $ T.pack ("f4_" ++ tag))

-- | Flip a single bit at a given byte offset in a ByteString.
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
  describe "CNTR v4 Ultra-Fast Merkle Cache & CRC32 Guard Engine" $ do

    describe "CNTR v4 Binary Architecture & Header Invariants" $ do
      it "encodes empty cache with valid 64-byte header and 1024-byte radix directory (1088 bytes total)" $ do
        let bin = encodeBinaryCache emptyCache
        BS.length bin `shouldBe` 1088 -- 64 header + 1024 radix table + 0 records + 0 strings
        BS.take 4 bin `shouldBe` "CNTR"
        decodeBinaryCache bin `shouldBe` Just emptyCache
        decodeBinaryCacheV4 bin `shouldBe` Just emptyCache
        lookupBinaryCache "any.py" (FileMetadata "any.py" 100 100) bin `shouldBe` Nothing

      it "encodes header with magic CNTR, version 0x0004, and flags 0x0007" $ do
        let bin = encodeBinaryCache emptyCache
        BS.take 4 bin `shouldBe` "CNTR"
        -- Version 4 (little-endian: 0x04, 0x00)
        BS.index bin 4 `shouldBe` 0x04
        BS.index bin 5 `shouldBe` 0x00
        -- Flags 0x0007 (Radix | CaseFolded | CRC32: 0x07, 0x00)
        BS.index bin 6 `shouldBe` 0x07
        BS.index bin 7 `shouldBe` 0x00

      it "preserves encodeBinaryCacheV3 for legacy generation with version 0x0003" $ do
        let binV3 = encodeBinaryCacheV3 emptyCache
        BS.take 4 binV3 `shouldBe` "CNTR"
        BS.index binV3 4 `shouldBe` 0x03
        BS.index binV3 5 `shouldBe` 0x00
        decodeBinaryCache binV3 `shouldBe` Just emptyCache

      it "evaluates fastPathHash64 deterministically across identical byte streams" $ do
        let bs1 = "src/core/parser.rs" :: BS.ByteString
            bs2 = BSC.pack "src/core/parser.rs"
        fastPathHash64 bs1 `shouldBe` fastPathHash64 bs2
        fastPathHash64 bs1 `shouldNotBe` fastPathHash64 "src/core/parser.go"

    describe "CRC32 Checksum Guard & Bit-Rot Resilience" $ do
      it "computes standard IEEE 802.3 CRC32 deterministically matching test vector" $ do
        computeCRC32 "123456789" `shouldBe` 0xCBF43926
        computeCRC32 "" `shouldBe` 0

      it "detects and rejects header magic tampering with Nothing" $ do
        let bin = encodeBinaryCacheV4 emptyCache
            corrupted = BS.cons 0x58 (BS.tail bin) -- 'X' instead of 'C'
        decodeBinaryCacheV4 corrupted `shouldBe` Nothing
        decodeBinaryCache corrupted `shouldBe` Nothing

      it "detects and rejects header version tampering with Nothing" $ do
        let bin = encodeBinaryCacheV4 emptyCache
            corrupted = flipBitAt 4 0 bin
        decodeBinaryCacheV4 corrupted `shouldBe` Nothing
        decodeBinaryCache corrupted `shouldBe` Nothing

      it "detects and rejects entry count tampering via header CRC mismatch" $ do
        let bin = encodeBinaryCacheV4 emptyCache
            corrupted = flipBitAt 8 0 bin -- entry count offset 8
        decodeBinaryCacheV4 corrupted `shouldBe` Nothing
        decodeBinaryCache corrupted `shouldBe` Nothing

      it "detects and rejects tampered header CRC32 field with Nothing" $ do
        let bin = encodeBinaryCacheV4 emptyCache
            corrupted = flipBitAt 28 0 bin -- header CRC offset 0x1C (28)
        decodeBinaryCacheV4 corrupted `shouldBe` Nothing

      it "detects and rejects tampered body CRC32 field with Nothing" $ do
        let bin = encodeBinaryCacheV4 emptyCache
            corrupted = flipBitAt 32 0 bin -- body CRC offset 0x20 (32)
        decodeBinaryCacheV4 corrupted `shouldBe` Nothing

      it "detects bit-rot in radix directory via body CRC mismatch" $ do
        let p = "src/app.py"
            meta = FileMetadata p 500 1700000000
            b = makeSampleBundle "app"
            cache = insertCache p meta b emptyCache
            bin = encodeBinaryCacheV4 cache
            -- Radix directory starts at offset 64
            corrupted = flipBitAt 64 2 bin
        decodeBinaryCacheV4 corrupted `shouldBe` Nothing
        decodeBinaryCache corrupted `shouldBe` Nothing

      it "detects bit-rot in record data via body CRC mismatch" $ do
        let p = "src/app.py"
            meta = FileMetadata p 500 1700000000
            b = makeSampleBundle "app"
            cache = insertCache p meta b emptyCache
            bin = encodeBinaryCacheV4 cache
            -- Records start at offset 1088 (64 header + 1024 radix table)
            corrupted = flipBitAt 1088 1 bin
        decodeBinaryCacheV4 corrupted `shouldBe` Nothing
        decodeBinaryCache corrupted `shouldBe` Nothing

      it "detects bit-rot in string table via body CRC mismatch" $ do
        let p = "src/app.py"
            meta = FileMetadata p 500 1700000000
            b = makeSampleBundle "app"
            cache = insertCache p meta b emptyCache
            bin = encodeBinaryCacheV4 cache
            -- String table is at the very end of the buffer
            corrupted = flipBitAt (BS.length bin - 1) 0 bin
        decodeBinaryCacheV4 corrupted `shouldBe` Nothing
        decodeBinaryCache corrupted `shouldBe` Nothing

      it "safely rejects truncated or malformed buffers with Nothing" $ do
        decodeBinaryCache "" `shouldBe` Nothing
        decodeBinaryCache "CNTR" `shouldBe` Nothing
        decodeBinaryCache (BS.replicate 50 0) `shouldBe` Nothing
        decodeBinaryCache "NOT_CNTR_HEADER_DATA_123456789012345678901234567890" `shouldBe` Nothing
        lookupBinaryCache "a.py" (FileMetadata "a.py" 10 10) "" `shouldBe` Nothing
        lookupBinaryCache "a.py" (FileMetadata "a.py" 10 10) (BS.replicate 20 0) `shouldBe` Nothing

    describe "Atomic Write Swap Engine" $ do
      it "writes cache file atomically and reads it back faithfully via readMerkleCache" $ do
        tmpBase <- getTemporaryDirectory
        let testDir = tmpBase </> "canontra_atomic_test_v4"
            cacheDir = testDir </> ".canontra"
            cacheFile = cacheDir </> "cache.bin"
            p = "src/module.py"
            meta = FileMetadata p 1234 1700000001
            bundle = makeSampleBundle "module"
            cache = insertCache p meta bundle emptyCache

        createDirectoryIfMissing True testDir
        writeMerkleCacheAtomic cacheFile cache

        -- Cache file exists
        fileExists <- doesFileExist cacheFile
        fileExists `shouldBe` True

        -- No temporary files remain in .canontra directory
        dirContents <- listDirectory cacheDir
        filter (\f -> f /= "cache.bin") dirContents `shouldBe` []

        -- readMerkleCache reads identical cache
        loadedCache <- readMerkleCache cacheFile
        lookupCache p meta loadedCache `shouldBe` Just bundle

        -- Clean up
        removeFile cacheFile
        removeDirectoryRecursive testDir

      it "atomically replaces existing cache file when writing new entries" $ do
        tmpBase <- getTemporaryDirectory
        let testDir = tmpBase </> "canontra_atomic_replace_v4"
            cacheFile = testDir </> "cache.bin"
            p1 = "src/first.py"
            m1 = FileMetadata p1 100 1700000001
            b1 = makeSampleBundle "first"
            c1 = insertCache p1 m1 b1 emptyCache

            p2 = "src/second.py"
            m2 = FileMetadata p2 200 1700000002
            b2 = makeSampleBundle "second"
            c2 = insertCache p2 m2 b2 c1

        createDirectoryIfMissing True testDir
        writeMerkleCacheAtomic cacheFile c1
        writeMerkleCacheAtomic cacheFile c2

        loaded <- readMerkleCache cacheFile
        lookupCache p1 m1 loaded `shouldBe` Just b1
        lookupCache p2 m2 loaded `shouldBe` Just b2

        -- Clean up
        removeFile cacheFile
        removeDirectoryRecursive testDir

    describe "Universal Case-Folded Canonical Path Collation & Invariance" $ do
      it "normalizes Windows backslashes and case-folds paths to lowercase POSIX" $ do
        normalizePathCanonical "src\\Core\\Parser.py" `shouldBe` "src/core/parser.py"
        normalizePathCanonical "SRC/MOD/FOO.RS" `shouldBe` "src/mod/foo.rs"
        normalizePathCanonical "lib\\nested\\deep\\module.ts" `shouldBe` "lib/nested/deep/module.ts"

      it "lookupCache transparently hits regardless of path casing or separator style" $ do
        let p = "src/core/parser.py"
            meta = FileMetadata p 1000 1700000001
            bundle = makeSampleBundle "core"
            cache = insertCache p meta bundle emptyCache

        -- Query with mixed case and Windows backslashes
        lookupCache "src\\Core\\Parser.py" meta cache `shouldBe` Just bundle
        lookupCache "SRC/CORE/PARSER.PY" meta cache `shouldBe` Just bundle
        lookupCache "src/core/parser.py" meta cache `shouldBe` Just bundle

      it "lookupBinaryCache in v4 buffer hits regardless of path casing or separator style" $ do
        let p = "src/core/parser.py"
            meta = FileMetadata p 1000 1700000001
            bundle = makeSampleBundle "core"
            cache = insertCache p meta bundle emptyCache
            bin = encodeBinaryCacheV4 cache

        -- Lookup with uppercase and Windows separators
        lookupBinaryCache "src\\Core\\Parser.py" meta bin `shouldBe` Just bundle
        lookupBinaryCache "SRC/CORE/PARSER.PY" meta bin `shouldBe` Just bundle
        lookupBinaryCache "src/core/parser.py" meta bin `shouldBe` Just bundle

      it "computeRepositoryFingerprint produces identical Merkle root (F_R) regardless of path casing/separators" $ do
        let b1 = makeSampleBundle "file1"
            b2 = makeSampleBundle "file2"
            entriesWin = [FileEntry "src\\Core\\Parser.py" b1, FileEntry "lib\\Util.py" b2]
            entriesUnix = [FileEntry "src/core/parser.py" b1, FileEntry "lib/util.py" b2]
            entriesMixed = [FileEntry "SRC/CORE/PARSER.PY" b1, FileEntry "LIB\\UTIL.PY" b2]

            fpWin = computeRepositoryFingerprint entriesWin
            fpUnix = computeRepositoryFingerprint entriesUnix
            fpMixed = computeRepositoryFingerprint entriesMixed

        fpWin `shouldBe` fpUnix
        fpMixed `shouldBe` fpUnix

    describe "Collision-Proof Radix Directory & Large Scale Lookups" $ do
      it "accurately distributes entries across the 256 radix buckets in v4 format" $ do
        let paths = ["src/component_" ++ show (i :: Int) ++ "/file_" ++ show (j :: Int) ++ ".py" | i <- [1..10], j <- [1..10]]
            entries = [(p, FileMetadata p (fromIntegral (length p * 10)) 1700000000, makeSampleBundle p) | p <- paths]
            cache = foldr (\(p, m, b) c -> insertCache p m b c) emptyCache entries
            bin = encodeBinaryCacheV4 cache
        decodeBinaryCacheV4 bin `shouldBe` Just cache
        mapM_ (\(p, m, b) -> lookupBinaryCache p m bin `shouldBe` Just b) entries

      it "guarantees collision-proof accuracy when paths share common prefixes" $ do
        let p1 = "src/controllers/auth_service.py"
            p2 = "src/controllers/auth_service_v2.py"
            p3 = "src/controllers/auth_service_admin.py"
            m1 = FileMetadata p1 1000 1700000001
            m2 = FileMetadata p2 2000 1700000002
            m3 = FileMetadata p3 3000 1700000003
            b1 = makeSampleBundle "auth1"
            b2 = makeSampleBundle "auth2"
            b3 = makeSampleBundle "auth3"
            cache = insertCache p3 m3 b3 (insertCache p2 m2 b2 (insertCache p1 m1 b1 emptyCache))
            bin = encodeBinaryCacheV4 cache
        lookupBinaryCache p1 m1 bin `shouldBe` Just b1
        lookupBinaryCache p2 m2 bin `shouldBe` Just b2
        lookupBinaryCache p3 m3 bin `shouldBe` Just b3
        lookupBinaryCache "src/controllers/auth_service_other.py" m1 bin `shouldBe` Nothing

      it "scales to 500 files with 100% lookup hit accuracy in v4 format" $ do
        let paths = ["lib/pkg_" ++ show (i :: Int) ++ "/mod_" ++ show (j :: Int) ++ ".py" | i <- [1..25], j <- [1..20]]
            indices = [1..length paths]
            entries = [(p, FileMetadata p (fromIntegral (i * 100)) (1700000000 + fromIntegral (i * 50)), makeSampleBundle (show i)) | (i, p) <- zip indices paths]
            cache = foldr (\(p, m, b) c -> insertCache p m b c) emptyCache entries
            bin = encodeBinaryCacheV4 cache
        decodeBinaryCacheV4 bin `shouldBe` Just cache
        mapM_ (\(p, m, b) -> lookupBinaryCache p m bin `shouldBe` Just b) entries
        lookupBinaryCache "lib/pkg_999/mod_999.py" (FileMetadata "lib/pkg_999/mod_999.py" 100 100) bin `shouldBe` Nothing

    describe "Property-Based QuickCheck Invariants" $ do
      it "Property: Multi-file random cache lossless roundtrip bijection with CRC32" $
        property $ forAll (choose (0, 30 :: Int)) $ \n ->
          forAll (vectorOf n (listOf1 (elements (['a'..'z'] ++ ['0'..'9'] ++ ['_', '/'])))) $ \rawPaths ->
            let indices = [1..length rawPaths]
                paths = [p ++ "_" ++ show (i :: Int) ++ ".py" | (i, p) <- zip indices rawPaths]
                entries = [(p, FileMetadata p (fromIntegral (i * 10)) 1700000000, makeSampleBundle (show i)) | (i, p) <- zip indices paths]
                cache = foldr (\(p, m, b) c -> insertCache p m b c) emptyCache entries
            in decodeBinaryCacheV4 (encodeBinaryCacheV4 cache) === Just cache

      it "Property: 100% hit rate for every inserted key in random multi-file caches" $
        property $ forAll (choose (1, 25 :: Int)) $ \n ->
          forAll (vectorOf n (listOf1 (elements (['a'..'z'] ++ ['0'..'9'] ++ ['_'])))) $ \rawPaths ->
            let indices = [1..length rawPaths]
                paths = ["app/" ++ p ++ "_" ++ show (i :: Int) ++ ".ts" | (i, p) <- zip indices rawPaths]
                entries = [(p, FileMetadata p (fromIntegral (i * 50)) (1700000000 + fromIntegral i), makeSampleBundle (show i)) | (i, p) <- zip indices paths]
                cache = foldr (\(p, m, b) c -> insertCache p m b c) emptyCache entries
                bin = encodeBinaryCacheV4 cache
            in conjoin [lookupBinaryCache p m bin === Just b | (p, m, b) <- entries]

      it "Property: 1-bit corruption anywhere in the v4 buffer is strictly rejected with Nothing" $
        property $ forAll (choose (1, 10 :: Int)) $ \n ->
          forAll (vectorOf n (listOf1 (elements (['a'..'z'] ++ ['0'..'9'] ++ ['_'])))) $ \rawPaths ->
            let indices = [1..length rawPaths]
                paths = ["src/" ++ p ++ "_" ++ show (i :: Int) ++ ".py" | (i, p) <- zip indices rawPaths]
                entries = [(p, FileMetadata p (fromIntegral (i * 20)) 1700000000, makeSampleBundle (show i)) | (i, p) <- zip indices paths]
                cache = foldr (\(p, m, b) c -> insertCache p m b c) emptyCache entries
                bin = encodeBinaryCacheV4 cache
                len = BS.length bin
            in forAll (choose (0, len - 1)) $ \corruptByteIdx ->
               forAll (choose (0, 7 :: Int)) $ \corruptBitIdx ->
                 let corrupted = flipBitAt corruptByteIdx corruptBitIdx bin
                 in decodeBinaryCacheV4 corrupted === Nothing
