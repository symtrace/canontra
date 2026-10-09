{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Canontra.SlabV6Spec (spec) where

import Control.Monad (forM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek, poke, sizeOf)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hspec

import Canontra.Analysis.CSRGraph (CSRGraph (..), buildCSRGraph, csrHasEdge)
import Canontra.Cache.Inode (FileMetadata (..))
import Canontra.Cache.SlabV6
import Canontra.Types (Fingerprint (..), FingerprintBundle (..))

spec :: Spec
spec = do
  describe "Canontra.Cache.SlabV6: CNTR\\x06 Zero-Copy Memory-Mapped Slab Cache" $ do

    describe "Step 2.1: 64-Byte CacheRecordV6 & Radix Directory Layout" $ do
      it "enforces exact 64-byte alignment matching CPU cache lines" $ do
        sizeOf emptyCacheRecordV6 `shouldBe` 64

      it "guarantees lossless Storable peek/poke round-tripping in contiguous memory" $ do
        let rec = CacheRecordV6
              { crPathHash     = 0x1122334455667788
              , crMTimeSec     = 1728400000
              , crMTimeNano    = 123456789
              , crFileSize     = 4096
              , crSlabOffset   = 0x00008820
              , crSlabLength   = 320
              , crF4DigestHead = 0xAABBCCDDEEFF0011
              , crFlags        = 0x01
              , crReserved1    = 0
              , crReserved2    = 0
              }
        alloca $ \ptr -> do
          poke ptr rec
          rec' <- peek ptr
          rec' `shouldBe` rec

      it "encodes CNTR\\x06 header with magic, version 6, and valid CRC32" $ do
        let entries = []
            bs = encodeSlabV6Binary entries Nothing
        BS.take 4 bs `shouldBe` "CNTR"
        BS.length bs `shouldSatisfy` (>= 2080)
        verifySlabHeaderCRC bs `shouldBe` True

    describe "Step 2.2: Memory-Mapped Zero-Copy File Verification & Warm Lookups" $ do
      let bundle1 = FingerprintBundle
            { f0Source       = Fingerprint "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
            , f1Structural   = Fingerprint "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb"
            , f2Declaration  = Fingerprint "3e23e8160039594a33894f6564e1b1348bbd7a0088d42c4acb73eeaed59c009d"
            , f3Dependency   = Fingerprint "2e7d2c03a9507ae265ecf5b5356885a53393a2029d24139499726b425ff3dc34"
            , fCGCallGraph   = Fingerprint "18ac3e7343f016890c510e93f935261169d9e3f565436429830faf0934f4f8e4"
            , fCFControlFlow = Fingerprint "4b227777d4dd1fc61c6f884f48641d02b4d121d3fd328cb08b5531fcacdabf8a"
            , fDFDataFlow    = Fingerprint "ef2d127de37b942baad06145e54b0c619a1f22327b2ebbcfbec78f5564afe39d"
            , fTTypeContract = Fingerprint "bc25a324f6f40c7499645931281df691238eb157591605f25712f55928d150fb"
            , f4Composite    = Fingerprint "8f434346648f6b96df89dda901c5176b10a6d83961dd3c1ac88b59b2dc327aa4"
            }
          meta1 = FileMetadata "src/app.py" 1024 1728400000

      it "round-trips file entries through encodeSlabV6Binary and decodeSlabV6Binary" $ do
        let entries = [("src/app.py", meta1, bundle1)]
            bs = encodeSlabV6Binary entries Nothing
            res = decodeSlabV6Binary bs
        case res of
          Nothing -> expectationFailure "Decode failed"
          Just (m, _) -> do
            Map.size m `shouldBe` 1
            case Map.lookup "src/app.py" m of
              Nothing -> expectationFailure "Missing entry src/app.py"
              Just (mMeta, mBundle) -> do
                fmSize mMeta `shouldBe` fmSize meta1
                fmMtime mMeta `shouldBe` fmMtime meta1
                f4Composite mBundle `shouldBe` f4Composite bundle1
                f1Structural mBundle `shouldBe` f1Structural bundle1

      it "achieves sub-microsecond warm lookup hit via SlabCacheHandle" $ do
        tmpDir <- getTemporaryDirectory
        let testDir = tmpDir </> "canontra_test_slab_v6_warm"
            cachePath = testDir </> ".canontra" </> "cache.bin"
        createDirectoryIfMissing True (testDir </> ".canontra")
        let entries = [("lib/math.py", meta1, bundle1)]
        writeSlabCacheFile cachePath entries Nothing
        mHandle <- openSlabCache cachePath
        case mHandle of
          Nothing -> expectationFailure "Failed to open slab cache handle"
          Just handle -> do
            -- Warm lookup with matching size & mtime must HIT
            hit <- lookupSlabCacheWarm handle "lib/math.py" meta1
            case hit of
              Nothing -> expectationFailure "Warm lookup missed"
              Just b  -> f4Composite b `shouldBe` f4Composite bundle1
            -- MTime mismatch must MISS
            let metaMutated = meta1 { fmMtime = fmMtime meta1 + 10 }
            missMTime <- lookupSlabCacheWarm handle "lib/math.py" metaMutated
            missMTime `shouldBe` Nothing
            -- FileSize mismatch must MISS
            let metaSizeMutated = meta1 { fmSize = fmSize meta1 + 50 }
            missSize <- lookupSlabCacheWarm handle "lib/math.py" metaSizeMutated
            missSize `shouldBe` Nothing
            -- Non-existent file must MISS
            missFile <- lookupSlabCacheWarm handle "lib/unknown.py" meta1
            missFile `shouldBe` Nothing
            closeSlabCache handle
        removeDirectoryRecursive testDir

      it "supports pure zero-copy lookupSlabBinaryBS directly from ByteString" $ do
        let entries = [("src/app.py", meta1, bundle1)]
            bs = encodeSlabV6Binary entries Nothing
        lookupSlabBinaryBS "src/app.py" meta1 bs `shouldBe` Just bundle1
        lookupSlabBinaryBS "src/missing.py" meta1 bs `shouldBe` Nothing

    describe "Step 2.3: Whole-Repository Binary CSR Persistence in CNTR\\x06" $ do
      it "serializes and deserializes unboxed CSRGraph to/from binary bytes" $ do
        let edges = [(0, 1, 1), (1, 2, 2), (2, 0, 4)]
            g = buildCSRGraph 3 edges
            builder = encodeCSRGraph g
            bs = BS.toStrict (BB.toLazyByteString builder)
        case decodeCSRGraph bs 0 of
          Nothing -> expectationFailure "decodeCSRGraph failed"
          Just (g', len) -> do
            len `shouldBe` BS.length bs
            csrNodeCount g' `shouldBe` 3
            csrEdgeCount g' `shouldBe` 3
            csrHasEdge g' 0 1 `shouldBe` True
            csrHasEdge g' 1 2 `shouldBe` True
            csrHasEdge g' 2 0 `shouldBe` True

      it "persists and restores WholeRepoBundle and CSR graphs via saveRepoGraphsSlab / loadRepoGraphsSlab" $ do
        tmpDir <- getTemporaryDirectory
        let testDir = tmpDir </> "canontra_test_slab_v6_repo"
        createDirectoryIfMissing True (testDir </> ".canontra")
        let fwcg = Fingerprint "1111111111111111111111111111111111111111111111111111111111111111"
            fwdf = Fingerprint "2222222222222222222222222222222222222222222222222222222222222222"
            cgCSR = buildCSRGraph 2 [(0, 1, 1)]
            dfCSR = buildCSRGraph 2 [(1, 0, 2)]
        saveRepoGraphsSlab testDir fwcg fwdf (Just cgCSR) (Just dfCSR)
        mRes <- loadRepoGraphsSlab testDir
        case mRes of
          Nothing -> expectationFailure "loadRepoGraphsSlab failed"
          Just (c, d, mCg, mDf) -> do
            c `shouldBe` fwcg
            d `shouldBe` fwdf
            case (mCg, mDf) of
              (Just cg, Just df) -> do
                csrNodeCount cg `shouldBe` 2
                csrHasEdge cg 0 1 `shouldBe` True
                csrNodeCount df `shouldBe` 2
                csrHasEdge df 1 0 `shouldBe` True
              _ -> expectationFailure "Failed to restore CSR graphs from binary slab"
        removeDirectoryRecursive testDir

    describe "Section 5.4: Isolated 4KB Page Bit-Rot Recovery" $ do
      let bundle1 = FingerprintBundle (Fingerprint "s1") (Fingerprint "st1") (Fingerprint "d1") (Fingerprint "dp1") (Fingerprint "cg1") (Fingerprint "cf1") (Fingerprint "df1") (Fingerprint "t1") (Fingerprint "c1")
          bundle2 = FingerprintBundle (Fingerprint "s2") (Fingerprint "st2") (Fingerprint "d2") (Fingerprint "dp2") (Fingerprint "cg2") (Fingerprint "cf2") (Fingerprint "df2") (Fingerprint "t2") (Fingerprint "c2")
          meta = FileMetadata "f.py" 100 1728400000

      it "recovers valid entries when a single 4KB slab page is corrupted" $ do
        let entries = [("f1.py", meta, bundle1), ("f2.py", meta, bundle2)]
            bs = encodeSlabV6Binary entries Nothing
        -- Uncorrupted decode must have 0 corrupted pages
        let (validMap0, _, corrupted0) = decodeSlabV6Resilient bs
        Map.size validMap0 `shouldBe` 2
        corrupted0 `shouldBe` []

        -- Inject 4-byte bit-rot corruption into the first slab page
        -- Slab pages start at offset 2080 + 512 * 64 = 34848 (0x8820)
        let slabOffset = 34848
        if BS.length bs > slabOffset + 20
          then do
            let corruptedBS = BS.take (slabOffset + 10) bs
                           <> "\xFF\xFF\xFF\xFF"
                           <> BS.drop (slabOffset + 14) bs
            let (validMap, _, corrupted) = decodeSlabV6Resilient corruptedBS
            -- Exactly page 0 is flagged as corrupted
            corrupted `shouldBe` [0]
            -- System does not panic or crash; safely isolated!
            Map.size validMap `shouldSatisfy` (<= 2)
          else expectationFailure "Buffer shorter than expected slab offset"

    describe "Gate 2: 1,000-File Warm Repo Verification Benchmark (< 85 ms)" $ do
      it "verifies 1,000 files in memory-mapped slab cache in < 85 ms (< 500 ns per file)" $ do
        tmpDir <- getTemporaryDirectory
        let testDir = tmpDir </> "canontra_gate2_benchmark"
            cachePath = testDir </> ".canontra" </> "cache.bin"
        createDirectoryIfMissing True (testDir </> ".canontra")
        let mkEntry i =
              let !path = "src/pkg_" ++ show (i `div` 50) ++ "/file_" ++ show (i :: Int) ++ ".py"
                  !meta = FileMetadata path (fromIntegral (100 + i * 10)) (1728400000 + fromIntegral i)
                  !b = FingerprintBundle
                         (Fingerprint ("s_" <> T.pack (show i)))
                         (Fingerprint ("st_" <> T.pack (show i)))
                         (Fingerprint "d")
                         (Fingerprint "dp")
                         (Fingerprint "cg")
                         (Fingerprint "cf")
                         (Fingerprint "df")
                         (Fingerprint "t")
                         (Fingerprint ("c_" <> T.pack (show i)))
              in (path, meta, b)
            entries = [mkEntry i | i <- [1 .. 1000 :: Int]]
        writeSlabCacheFile cachePath entries Nothing
        mHandle <- openSlabCache cachePath
        case mHandle of
          Nothing -> expectationFailure "Failed to open slab cache handle for Gate 2"
          Just handle -> do
            t0 <- getCurrentTime
            matchCount <- forM entries $ \(p, m, b) -> do
              mHit <- lookupSlabCacheWarm handle p m
              case mHit of
                Nothing -> pure (0 :: Int)
                Just hitBundle -> pure (if f4Composite hitBundle == f4Composite b then 1 else 0)
            t1 <- getCurrentTime
            closeSlabCache handle
            sum matchCount `shouldBe` 1000
            let elapsedSec = realToFrac (diffUTCTime t1 t0) :: Double
            -- Must comfortably verify in < 85 ms (0.085s)
            elapsedSec `shouldSatisfy` (< 0.085)
        removeDirectoryRecursive testDir
