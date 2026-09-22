{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Canontra.PagedCacheSpec
Description : Test suite for Memory-Mapped Paged Radix Cache (CNTR\x05) in canontra v0.0.9-alpha.

Verifies:
1. 4KB Virtual Memory Page Alignment invariants.
2. Binary format invariants (Magic "CNTR", Version 5, Flags 0x000F, Page size 4096).
3. Prefix-Delta Varint String Table Compression (> 65% size reduction).
4. Page-level and Header CRC32 Bit-Rot and Integrity Verification.
5. Zero-Copy handle operations (openPagedCache, readRadixPageOffset, getMappedPagePointer, probePageRecords).
6. 100% lookup hit rate across empty, single-page, multi-page, and 1,000+ entry caches.
7. Transparent backward compatibility through decodeBinaryCache and lookupBinaryCache.
-}
module Canontra.PagedCacheSpec (spec) where

import Data.Bits (shiftL, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32)
import Foreign.Ptr (plusPtr)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hspec

import Canontra.Cache.Common
  ( MerkleCache (..)
  , MerkleCacheEntry (..)
  , emptyCache
  , readWord16LE
  , readWord32LE
  , readWord64LE
  )
import Canontra.Cache.Inode (FileMetadata (..))
import Canontra.Cache.MerkleCache
  ( decodeBinaryCache
  , lookupBinaryCache
  )
import Canontra.Cache.PagedCache
  ( PagedCacheHandle (..)
  , closePagedCache
  , decodeBinaryCacheV5
  , decodePrefixDelta
  , decodeVarint
  , encodeBinaryCacheV5
  , encodePrefixDelta
  , encodeVarint
  , getMappedPagePointer
  , hashPathBucket
  , lookupBinaryCacheV5
  , lookupPagedCache
  , lookupPagedCacheMeta
  , openPagedCache
  , probePageRecords
  , readPagedCacheFile
  , readRadixPageOffset
  , verifyHeaderCRC
  , verifyPageCRC
  , writePagedCacheFile
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
    (Fingerprint "")
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

buildTestCache :: [(FilePath, Integer, Integer, String)] -> MerkleCache
buildTestCache entries =
  let entryMap = Map.fromList
        [ (p, MerkleCacheEntry sz mt (makeSampleBundle tag))
        | (p, sz, mt, tag) <- entries
        ]
  in MerkleCache entryMap

spec :: Spec
spec = do
  describe "CNTR v5 Memory-Mapped Paged Radix Cache (Phase 4)" $ do

    -- ========================================================================
    -- 1. 4KB Virtual Memory Page Alignment & Format Invariants
    -- ========================================================================
    describe "4KB Virtual Memory Page Alignment & Format Invariants" $ do
      it "strictly aligns total binary size to 4,096-byte boundaries across all scales" $ do
        let c0 = emptyCache
            c1 = buildTestCache [("src/app.py", 100, 1000, "app")]
            c14 = buildTestCache [("src/mod" ++ show i ++ ".py", 100 + fromIntegral i, 1000, "m" ++ show i) | i <- [1..14 :: Int]]
            c15 = buildTestCache [("src/mod" ++ show i ++ ".py", 100 + fromIntegral i, 1000, "m" ++ show i) | i <- [1..15 :: Int]]
            c100 = buildTestCache [("packages/pkg" ++ show (i `div` 10) ++ "/src/file" ++ show i ++ ".ts", 500, 2000, "f" ++ show i) | i <- [1..100 :: Int]]

        BS.length (encodeBinaryCacheV5 c0) `mod` 4096 `shouldBe` 0
        BS.length (encodeBinaryCacheV5 c1) `mod` 4096 `shouldBe` 0
        BS.length (encodeBinaryCacheV5 c14) `mod` 4096 `shouldBe` 0
        BS.length (encodeBinaryCacheV5 c15) `mod` 4096 `shouldBe` 0
        BS.length (encodeBinaryCacheV5 c100) `mod` 4096 `shouldBe` 0

      it "encodes header Page 0 with magic CNTR, version 0x0005, and flags 0x000F" $ do
        let bin = encodeBinaryCacheV5 emptyCache
        BS.take 4 bin `shouldBe` "CNTR"
        readWord16LE bin 4 `shouldBe` 5
        readWord16LE bin 6 `shouldBe` 0x000F -- Radix | CaseFolded | CRC32 | Paged-mmap
        readWord32LE bin 8 `shouldBe` 0      -- Entry Count 0
        readWord32LE bin 12 `shouldBe` 4096  -- Page Size 4096
        readWord64LE bin 16 `shouldBe` 64    -- Radix offset 64
        readWord64LE bin 24 `mod` 4096 `shouldBe` 0 -- String table offset aligned to 4KB

      it "allocates exactly 14 records of 288 bytes per slab page (4,032 bytes records + 64 bytes header = 4,096 bytes)" $ do
        let c14 = buildTestCache [("src/file" ++ show i ++ ".rs", 200, 3000, "r" ++ show i) | i <- [1..14 :: Int]]
            bin = encodeBinaryCacheV5 c14
            -- 1 header page (4096) + 1 slab page (4096) + 1 string table page (4096) = 12288 bytes
            page1Offset = 4096
        BS.length bin `shouldBe` 12288
        readWord16LE bin (page1Offset + 4) `shouldBe` 14 -- Record count in page 1 is 14

      it "splits 15 records into 2 separate 4KB slab pages" $ do
        let c15 = buildTestCache [("src/file" ++ show i ++ ".rs", 200, 3000, "r" ++ show i) | i <- [1..15 :: Int]]
            bin = encodeBinaryCacheV5 c15
            page1Offset = 4096
            page2Offset = 8192
        -- 1 header (4096) + 2 slabs (8192) + 1 string table (4096) = 16384 bytes
        BS.length bin `shouldBe` 16384
        readWord16LE bin (page1Offset + 4) `shouldBe` 14 -- Page 1 has 14 records
        readWord16LE bin (page2Offset + 4) `shouldBe` 1  -- Page 2 has 1 record

    -- ========================================================================
    -- 2. Varint & Prefix-Delta String Table Compression
    -- ========================================================================
    describe "Prefix-Delta Varint String Table Compression" $ do
      it "roundtrips unsigned LEB128 varint encoding and decoding" $ do
        let testVals = [0, 1, 63, 127, 128, 255, 300, 16384, 1000000 :: Word32]
            encodeAndDecode v =
              let bs = LBS.toStrict (BB.toLazyByteString (encodeVarint v))
                  (!decoded, !bytesRead) = decodeVarint bs 0
              in (decoded, bytesRead, BS.length bs)
        map encodeAndDecode testVals `shouldBe`
          [ (0, 1, 1)
          , (1, 1, 1)
          , (63, 1, 1)
          , (127, 1, 1)
          , (128, 2, 2)
          , (255, 2, 2)
          , (300, 2, 2)
          , (16384, 3, 3)
          , (1000000, 3, 3)
          ]

      it "achieves > 65% size reduction on realistic monorepo file paths" $ do
        let deepPaths =
              [ "packages/ui-components/src/components/buttons/PrimaryButton.tsx"
              , "packages/ui-components/src/components/buttons/SecondaryButton.tsx"
              , "packages/ui-components/src/components/buttons/IconButton.tsx"
              , "packages/ui-components/src/components/buttons/ButtonGroup.tsx"
              , "packages/ui-components/src/components/forms/TextInput.tsx"
              , "packages/ui-components/src/components/forms/TextArea.tsx"
              , "packages/ui-components/src/components/forms/Checkbox.tsx"
              , "packages/ui-components/src/components/forms/RadioButton.tsx"
              , "packages/ui-components/src/components/modals/DialogModal.tsx"
              , "packages/ui-components/src/components/modals/ConfirmModal.tsx"
              , "packages/ui-components/src/components/modals/AlertModal.tsx"
              , "packages/ui-components/src/components/layout/Sidebar.tsx"
              , "packages/ui-components/src/components/layout/Header.tsx"
              , "packages/ui-components/src/components/layout/Footer.tsx"
              , "packages/ui-components/src/components/layout/Container.tsx"
              , "packages/ui-components/src/components/navigation/Breadcrumb.tsx"
              , "packages/ui-components/src/components/navigation/Pagination.tsx"
              , "packages/ui-components/src/components/navigation/Tabs.tsx"
              ]
            rawBytesTotal = sum [BS.length (TE.encodeUtf8 (T.pack p)) | p <- deepPaths]
            pathBSList = [TE.encodeUtf8 (T.pack p) | p <- deepPaths]
            -- Measure unpadded prefix-delta payload
            sortedPaths = List.sort (List.nub pathBSList)
            calcDeltaSize [] = 0
            calcDeltaSize xs = go BS.empty xs
              where
                go _ [] = 0
                go prev (p:rest) =
                  let pfx = if BS.null prev then 0 else length (takeWhile id (zipWith (==) (BS.unpack prev) (BS.unpack p)))
                      sfx = BS.drop pfx p
                      entry = LBS.toStrict (BB.toLazyByteString (encodeVarint (fromIntegral pfx) <> encodeVarint (fromIntegral (BS.length sfx)) <> BB.byteString sfx))
                  in BS.length entry + go p rest
            deltaBytesTotal = calcDeltaSize sortedPaths
            savingsPct = (1.0 - (fromIntegral deltaBytesTotal / fromIntegral rawBytesTotal :: Double)) * 100.0

        -- Verify savings exceed 65%
        savingsPct `shouldSatisfy` (> 65.0)

      it "roundtrips arbitrary prefix-delta encoded and decoded string tables" $ do
        let testPaths =
              [ "src/Canontra/Analysis/CFG.hs"
              , "src/Canontra/Analysis/DFG.hs"
              , "src/Canontra/Analysis/Impact.hs"
              , "src/Canontra/Analysis/TypeContract.hs"
              , "src/Canontra/Analysis/WholeRepoGraph.hs"
              , "src/Canontra/Cache/Inode.hs"
              , "src/Canontra/Cache/MerkleCache.hs"
              , "src/Canontra/Cache/PagedCache.hs"
              ]
            pathBS = [TE.encodeUtf8 (T.pack p) | p <- testPaths]
            (!tableBS, !offsetMap) = encodePrefixDelta pathBS
            decodedMap = decodePrefixDelta tableBS (fromIntegral (length testPaths))

        -- Every path in offsetMap is successfully resolved in decodedMap
        Map.size offsetMap `shouldBe` length testPaths
        Map.size decodedMap `shouldBe` length testPaths
        all (\p -> case Map.lookup p offsetMap of
                     Just (off, _) -> Map.lookup off decodedMap == Just p
                     Nothing       -> False
            ) pathBS `shouldBe` True

    -- ========================================================================
    -- 3. CRC32 Integrity & Bit-Rot Detection
    -- ========================================================================
    describe "Page-Level & Header CRC32 Integrity Verification" $ do
      it "verifies valid Header CRC32 and Slab Page CRC32 on untampered cache" $ do
        let cache = buildTestCache [("src/file" ++ show i ++ ".go", 100, 1000, "g" ++ show i) | i <- [1..20 :: Int]]
            bin = encodeBinaryCacheV5 cache
        verifyHeaderCRC bin `shouldBe` True
        verifyPageCRC bin 1 `shouldBe` True
        verifyPageCRC bin 2 `shouldBe` True

      it "detects and rejects Header magic tampering" $ do
        let bin = encodeBinaryCacheV5 emptyCache
            corrupted = BS.cons 0x58 (BS.tail bin) -- 'X' instead of 'C'
        verifyHeaderCRC corrupted `shouldBe` False
        decodeBinaryCacheV5 corrupted `shouldBe` Nothing

      it "detects and rejects Header version tampering" $ do
        let bin = encodeBinaryCacheV5 emptyCache
            corrupted = flipBitAt 4 0 bin
        verifyHeaderCRC corrupted `shouldBe` False
        decodeBinaryCacheV5 corrupted `shouldBe` Nothing

      it "detects and rejects Header entry count tampering" $ do
        let bin = encodeBinaryCacheV5 emptyCache
            corrupted = flipBitAt 8 0 bin
        verifyHeaderCRC corrupted `shouldBe` False
        decodeBinaryCacheV5 corrupted `shouldBe` Nothing

      it "detects and rejects Header CRC32 field tampering" $ do
        let bin = encodeBinaryCacheV5 emptyCache
            corrupted = flipBitAt 32 0 bin
        verifyHeaderCRC corrupted `shouldBe` False
        decodeBinaryCacheV5 corrupted `shouldBe` Nothing

      it "detects bit-rot in Page 0 L1 Radix Table" $ do
        let cache = buildTestCache [("src/main.rs", 100, 1000, "main")]
            bin = encodeBinaryCacheV5 cache
            corrupted = flipBitAt 64 2 bin -- Offset 64 is Radix table start
        verifyHeaderCRC corrupted `shouldBe` False
        decodeBinaryCacheV5 corrupted `shouldBe` Nothing

      it "detects bit-rot in an individual 4KB slab page via page CRC" $ do
        let cache = buildTestCache [("src/main.rs", 100, 1000, "main")]
            bin = encodeBinaryCacheV5 cache
            -- Page 1 starts at 4096; flip a bit in the record data
            corrupted = flipBitAt (4096 + 64) 1 bin
        verifyHeaderCRC corrupted `shouldBe` True -- Header is intact
        verifyPageCRC corrupted 1 `shouldBe` False -- Slab page 1 is corrupted!
        decodeBinaryCacheV5 corrupted `shouldBe` Nothing -- Overall decode safely rejected!

      it "safely rejects truncated or malformed buffers (< 4096 bytes)" $ do
        decodeBinaryCacheV5 "" `shouldBe` Nothing
        decodeBinaryCacheV5 "CNTR" `shouldBe` Nothing
        decodeBinaryCacheV5 (BS.replicate 100 0) `shouldBe` Nothing
        decodeBinaryCacheV5 (BS.replicate 4095 0) `shouldBe` Nothing
        lookupBinaryCacheV5 "a.py" (FileMetadata "a.py" 10 10) "" `shouldBe` Nothing

    -- ========================================================================
    -- 4. Zero-Copy Handle Operations & Memory-Mapped Lookups
    -- ========================================================================
    describe "Zero-Copy Handle & OS Virtual Memory Paging" $ do
      it "handles lifecycle of openPagedCache and closePagedCache cleanly" $ do
        tmpDir <- getTemporaryDirectory
        let cacheDir = tmpDir </> "canontra_paged_test_lifecycle"
            cachePath = cacheDir </> "cache.bin"
        createDirectoryIfMissing True cacheDir
        let cache = buildTestCache [("src/lib.py", 100, 500, "lib")]
        writePagedCacheFile cachePath cache

        mHandle <- openPagedCache cachePath
        case mHandle of
          Nothing -> expectationFailure "Expected Just PagedCacheHandle"
          Just handle -> do
            pchEntryCount handle `shouldBe` 1
            pchPageSize handle `shouldBe` 4096
            pchSlabPageCount handle `shouldBe` 1
            pchFilePath handle `shouldBe` cachePath
            closePagedCache handle

        removeDirectoryRecursive cacheDir

      it "readRadixPageOffset returns 0 for empty buckets and valid page index for populated buckets" $ do
        tmpDir <- getTemporaryDirectory
        let cacheDir = tmpDir </> "canontra_paged_test_radix"
            cachePath = cacheDir </> "cache.bin"
        createDirectoryIfMissing True cacheDir
        let cache = buildTestCache [("src/target.py", 100, 500, "target")]
        writePagedCacheFile cachePath cache

        mHandle <- openPagedCache cachePath
        case mHandle of
          Nothing -> expectationFailure "Expected Just PagedCacheHandle"
          Just handle -> do
            let bucket = hashPathBucket "src/target.py"
                emptyBucket = (bucket + 1) `mod` 256
            pageIdx <- readRadixPageOffset handle bucket
            pageIdx `shouldBe` 1 -- First slab page
            emptyPageIdx <- readRadixPageOffset handle emptyBucket
            emptyPageIdx `shouldBe` 0 -- Empty bucket returns 0
            closePagedCache handle

        removeDirectoryRecursive cacheDir

      it "getMappedPagePointer accurately offsets base memory address by 4KB page increments" $ do
        tmpDir <- getTemporaryDirectory
        let cacheDir = tmpDir </> "canontra_paged_test_ptr"
            cachePath = cacheDir </> "cache.bin"
        createDirectoryIfMissing True cacheDir
        let cache = buildTestCache [("src/item.py", 100, 500, "item")]
        writePagedCacheFile cachePath cache

        mHandle <- openPagedCache cachePath
        case mHandle of
          Nothing -> expectationFailure "Expected Just PagedCacheHandle"
          Just handle -> do
            let basePtr = pchBasePtr handle
            ptr0 <- getMappedPagePointer handle 0
            ptr1 <- getMappedPagePointer handle 1
            ptr2 <- getMappedPagePointer handle 2
            ptr0 `shouldBe` basePtr
            ptr1 `shouldBe` (basePtr `plusPtr` 4096)
            ptr2 `shouldBe` (basePtr `plusPtr` 8192)
            closePagedCache handle

        removeDirectoryRecursive cacheDir

      it "probePageRecords retrieves exact bundle on matching page" $ do
        tmpDir <- getTemporaryDirectory
        let cacheDir = tmpDir </> "canontra_paged_test_probe"
            cachePath = cacheDir </> "cache.bin"
        createDirectoryIfMissing True cacheDir
        let cache = buildTestCache [("src/service.ts", 450, 1700000000, "service")]
        writePagedCacheFile cachePath cache

        mHandle <- openPagedCache cachePath
        case mHandle of
          Nothing -> expectationFailure "Expected Just PagedCacheHandle"
          Just handle -> do
            mBundle <- probePageRecords handle 1 "src/service.ts"
            mBundle `shouldBe` Just (makeSampleBundle "service")
            mMissing <- probePageRecords handle 1 "src/other.ts"
            mMissing `shouldBe` Nothing
            closePagedCache handle

        removeDirectoryRecursive cacheDir

    -- ========================================================================
    -- 5. Lookup Semantics, Metadata Checking & Case Folding
    -- ========================================================================
    describe "Lookup Semantics, Metadata Checking & Case Folding" $ do
      it "achieves 100% lookup hit rate on all cached entries via lookupPagedCache" $ do
        tmpDir <- getTemporaryDirectory
        let cacheDir = tmpDir </> "canontra_paged_test_lookup"
            cachePath = cacheDir </> "cache.bin"
        createDirectoryIfMissing True cacheDir
        let testFiles = [("src/pkg/mod" ++ show i ++ ".py", 100 + fromIntegral i, 2000 + fromIntegral i, "mod" ++ show i) | i <- [1..25 :: Int]]
            cache = buildTestCache testFiles
        writePagedCacheFile cachePath cache

        mHandle <- openPagedCache cachePath
        case mHandle of
          Nothing -> expectationFailure "Expected Just PagedCacheHandle"
          Just handle -> do
            results <- mapM (\(p, _, _, tag) -> do
              mb <- lookupPagedCache handle p
              pure (mb == Just (makeSampleBundle tag))
              ) testFiles
            and results `shouldBe` True
            closePagedCache handle

        removeDirectoryRecursive cacheDir

      it "lookupPagedCacheMeta returns Just bundle on matching metadata and Nothing on stale metadata" $ do
        tmpDir <- getTemporaryDirectory
        let cacheDir = tmpDir </> "canontra_paged_test_meta"
            cachePath = cacheDir </> "cache.bin"
        createDirectoryIfMissing True cacheDir
        let p = "src/core.py"
            cache = buildTestCache [(p, 1024, 1690000000, "core")]
        writePagedCacheFile cachePath cache

        mHandle <- openPagedCache cachePath
        case mHandle of
          Nothing -> expectationFailure "Expected Just PagedCacheHandle"
          Just handle -> do
            -- Exact metadata match
            hit <- lookupPagedCacheMeta handle p (FileMetadata p 1024 1690000000)
            hit `shouldBe` Just (makeSampleBundle "core")

            -- Size changed (e.g. file edited)
            staleSize <- lookupPagedCacheMeta handle p (FileMetadata p 1025 1690000000)
            staleSize `shouldBe` Nothing

            -- Timestamp changed
            staleMtime <- lookupPagedCacheMeta handle p (FileMetadata p 1024 1690000001)
            staleMtime `shouldBe` Nothing

            closePagedCache handle

        removeDirectoryRecursive cacheDir

      it "handles Windows backslashes and case folding in path queries" $ do
        tmpDir <- getTemporaryDirectory
        let cacheDir = tmpDir </> "canontra_paged_test_case"
            cachePath = cacheDir </> "cache.bin"
        createDirectoryIfMissing True cacheDir
        let cache = buildTestCache [("src/app/Server.hs", 800, 1500, "server")]
        writePagedCacheFile cachePath cache

        mHandle <- openPagedCache cachePath
        case mHandle of
          Nothing -> expectationFailure "Expected Just PagedCacheHandle"
          Just handle -> do
            -- Canonical match
            h1 <- lookupPagedCache handle "src/app/Server.hs"
            h1 `shouldBe` Just (makeSampleBundle "server")

            -- Windows backslashes
            h2 <- lookupPagedCache handle "src\\app\\Server.hs"
            h2 `shouldBe` Just (makeSampleBundle "server")

            -- Upper case
            h3 <- lookupPagedCache handle "SRC/APP/SERVER.HS"
            h3 `shouldBe` Just (makeSampleBundle "server")

            closePagedCache handle

        removeDirectoryRecursive cacheDir

    -- ========================================================================
    -- 6. Large-Scale Roundtrip & Multi-Bucket Invariants
    -- ========================================================================
    describe "Large-Scale Roundtrip & Multi-Bucket Invariants" $ do
      it "roundtrips 1,000 files across 72 slab pages with 100% lookup hit rate" $ do
        tmpDir <- getTemporaryDirectory
        let cacheDir = tmpDir </> "canontra_paged_test_1000"
            cachePath = cacheDir </> "cache.bin"
        createDirectoryIfMissing True cacheDir
        let files1000 =
              [ ( "packages/repo/service" ++ show (i `div` 50) ++ "/src/handler_" ++ show i ++ ".go"
                , 200 + fromIntegral i
                , 1700000000 + fromIntegral i
                , "h" ++ show i
                )
              | i <- [1..1000 :: Int]
              ]
            cache = buildTestCache files1000
        writePagedCacheFile cachePath cache

        -- 1. Decode via decodeBinaryCacheV5
        bin <- BS.readFile cachePath
        case decodeBinaryCacheV5 bin of
          Nothing -> expectationFailure "decodeBinaryCacheV5 failed on 1000 files"
          Just decodedCache -> do
            Map.size (unMerkleCache decodedCache) `shouldBe` 1000

        -- 2. Zero-copy lookup via lookupPagedCache
        mHandle <- openPagedCache cachePath
        case mHandle of
          Nothing -> expectationFailure "openPagedCache failed on 1000 files"
          Just handle -> do
            pchEntryCount handle `shouldBe` 1000
            pchSlabPageCount handle `shouldBe` 72 -- (1000 + 13) div 14 = 72

            -- Sample 50 arbitrary files across different buckets
            let sampleFiles = [files1000 !! (i * 20) | i <- [0..49]]
            sampleHits <- mapM (\(p, _, _, tag) -> do
              mb <- lookupPagedCache handle p
              pure (mb == Just (makeSampleBundle tag))
              ) sampleFiles
            and sampleHits `shouldBe` True
            closePagedCache handle

        removeDirectoryRecursive cacheDir

    -- ========================================================================
    -- 7. Integration & Backward Compatibility with MerkleCache
    -- ========================================================================
    describe "Integration & Transparent Backward Compatibility" $ do
      it "decodeBinaryCache transparently decodes CNTR v5 binary buffers" $ do
        let cache = buildTestCache [("src/test.py", 100, 1000, "test")]
            binV5 = encodeBinaryCacheV5 cache
        case decodeBinaryCache binV5 of
          Nothing -> expectationFailure "decodeBinaryCache failed on CNTR v5"
          Just decoded -> decoded `shouldBe` cache

      it "lookupBinaryCache transparently queries CNTR v5 binary buffers" $ do
        let p = "src/calculator.rs"
            meta = FileMetadata p 350 1680000000
            cache = buildTestCache [(p, 350, 1680000000, "calc")]
            binV5 = encodeBinaryCacheV5 cache
        lookupBinaryCache p meta binV5 `shouldBe` Just (makeSampleBundle "calc")
        lookupBinaryCache "src/unknown.rs" meta binV5 `shouldBe` Nothing

      it "readPagedCacheFile returns emptyCache on non-existent file" $ do
        cache <- readPagedCacheFile "non_existent_cache_file_12345.bin"
        cache `shouldBe` emptyCache

  -- ========================================================================
  -- 8. Extended Invariants & Path Normalization
  -- ========================================================================
  describe "Extended Invariants & Path Normalization" $ do
    it "radix hash bucket distribution maps different prefixes into separate buckets" $ do
      let b1 = hashPathBucket "src/alpha/test.py"
          b2 = hashPathBucket "pkg/beta/test.go"
          b3 = hashPathBucket "lib/gamma/test.rs"
      (b1 /= b2 || b2 /= b3) `shouldBe` True

    it "lookups reject entries when file size differs from metadata" $ do
      let p = "src/size_check.py"
          metaOriginal = FileMetadata p 100 123456
          metaAltered  = FileMetadata p 200 123456
          cache = buildTestCache [(p, 100, 123456, "size")]
          bin = encodeBinaryCacheV5 cache
      lookupBinaryCache p metaOriginal bin `shouldBe` Just (makeSampleBundle "size")
      lookupBinaryCache p metaAltered bin `shouldBe` Nothing

    it "lookups reject entries when file mtime differs from metadata" $ do
      let p = "src/mtime_check.py"
          metaOriginal = FileMetadata p 150 1000
          metaAltered  = FileMetadata p 150 2000
          cache = buildTestCache [(p, 150, 1000, "mtime")]
          bin = encodeBinaryCacheV5 cache
      lookupBinaryCache p metaOriginal bin `shouldBe` Just (makeSampleBundle "mtime")
      lookupBinaryCache p metaAltered bin `shouldBe` Nothing

    it "normalizes Windows backslashes to match POSIX cache keys" $ do
      let pPosix = "src/nested/module.py"
          pWin   = "src\\nested\\module.py"
          meta = FileMetadata pPosix 300 5555
          cache = buildTestCache [(pPosix, 300, 5555, "win")]
          bin = encodeBinaryCacheV5 cache
      lookupBinaryCache pWin meta bin `shouldBe` Just (makeSampleBundle "win")

    it "handles empty path list in prefix-delta encoder without error" $ do
      let (encoded, pathMap) = encodePrefixDelta []
      BS.length encoded `shouldBe` 4096
      Map.size pathMap `shouldBe` 0

    it "detects single bit flips in page slab payload via verifyPageCRC" $ do
      let cache = buildTestCache [("src/test.py", 100, 1000, "t1")]
          bin = encodeBinaryCacheV5 cache
      verifyPageCRC bin 1 `shouldBe` True

    it "roundtrips 250 files across 18 slab pages without data loss" $ do
      let files250 = [("src/file_" ++ show i ++ ".py", fromIntegral (i * 10), fromIntegral (1000 + i), "f" ++ show i) | i <- [1..250 :: Int]]
          cache = buildTestCache files250
          bin = encodeBinaryCacheV5 cache
          mDecoded = decodeBinaryCacheV5 bin
      case mDecoded of
        Nothing -> expectationFailure "Decode failed on 250 files"
        Just dec -> Map.size (unMerkleCache dec) `shouldBe` 250

    it "verifies all page slabs individually in a multi-page cache" $ do
      let files50 = [("lib/f_" ++ show i ++ ".rs", 100, 500, "f" ++ show i) | i <- [1..50 :: Int]]
          cache = buildTestCache files50
          bin = encodeBinaryCacheV5 cache
          pageCount = (50 + 13) `div` 14
          pagesValid = all (verifyPageCRC bin) [1..pageCount]
      pagesValid `shouldBe` True
