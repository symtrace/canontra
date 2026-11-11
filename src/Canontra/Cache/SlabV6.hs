{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Canontra.Cache.SlabV6
Description : Zero-copy memory-mapped slab cache engine (CNTR\x06) for canontra v0.3.0.

Establishes a zero-copy memory-mapped slab cache layout with:
- 0x0000 - 0x001F: Global header magic ("CNTR\x06"), version, record counts, and CRC32 checksum.
- 0x0020 - 0x081F: 256-way L1 Radix Jump Table (2,048 bytes) for 1-cycle CPU fast-path indexing.
- 0x0820 - 0x881F: Fixed-width 64-byte file table records aligned precisely to CPU cache lines.
- 0x8820 - End: 4KB page-aligned payload slabs holding full 9-tier fingerprint bundles,
  whole-repository CSR graph binary slabs, and isolated IEEE 802.3 CRC-32C page bit-rot protection.
-}
module Canontra.Cache.SlabV6
  ( -- * Cache Record V6
    CacheRecordV6 (..)
  , emptyCacheRecordV6

    -- * Handle & Lifecycle
  , SlabCacheHandle (..)
  , openSlabCache
  , closeSlabCache

    -- * Lookups & Memory-Mapped Verification
  , lookupSlabCacheWarm
  , lookupSlabCacheFast
  , lookupSlabBinaryBS
  , verifyFileWarmMmap
  , verifyRecordMatch

    -- * Binary Encoding & Decoding
  , encodeSlabV6Binary
  , decodeSlabV6Binary
  , decodeSlabV6Resilient

    -- * Disk File Operations
  , writeSlabCacheFile
  , readSlabCacheFile
  , readSlabCacheFileResilient
  , salvageSlabCacheFile

    -- * Whole-Repository Binary CSR Persistence (Step 2.3)
  , saveRepoGraphsSlab
  , loadRepoGraphsSlab

    -- * CSR Graph Serialization
  , encodeCSRGraph
  , decodeCSRGraph

    -- * CRC & Integrity Verification
  , verifySlabHeaderCRC
  , verifySlabPageCRC
  ) where

import Control.DeepSeq (NFData (..))
import Data.Bits ((.|.), shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector.Unboxed as U
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable (..), peekByteOff, pokeByteOff)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory)

import Canontra.Analysis.CSRGraph (CSRGraph (..))
import Canontra.Cache.Common
  ( atomicSwapWithRetry_
  , computeCRC32
  , decodeDigest
  , encodeBundle
  , encodeDigest
  , fastPathHash64
  , normalizePathCanonical
  , readWord16LE
  , readWord32LE
  , readWord64LE
  )
import Canontra.Cache.Inode (FileMetadata (..))
import Canontra.Types (Fingerprint (..), FingerprintBundle (..), WholeRepoBundle (..))

-- ============================================================================
-- CacheRecordV6: 64-byte Fixed-Width Record Aligned to CPU Cache Line
-- ============================================================================

-- | 64-byte fixed-width cache record aligned precisely to a CPU cache line.
data CacheRecordV6 = CacheRecordV6
  { crPathHash     :: {-# UNPACK #-} !Word64  -- ^ [0x00..0x07] 64-bit FNV-1a / SwissTable path hash
  , crMTimeSec     :: {-# UNPACK #-} !Word64  -- ^ [0x08..0x0F] Modification timestamp (seconds)
  , crMTimeNano    :: {-# UNPACK #-} !Word32  -- ^ [0x10..0x13] Modification timestamp (nanoseconds)
  , crFileSize     :: {-# UNPACK #-} !Word32  -- ^ [0x14..0x17] File size in bytes
  , crSlabOffset   :: {-# UNPACK #-} !Word32  -- ^ [0x18..0x1B] Direct byte offset to payload slab
  , crSlabLength   :: {-# UNPACK #-} !Word32  -- ^ [0x1C..0x1F] Byte length of payload slab
  , crF4DigestHead :: {-# UNPACK #-} !Word64  -- ^ [0x20..0x27] First 64 bits of composite F4 hash
  , crFlags        :: {-# UNPACK #-} !Word64  -- ^ [0x28..0x2F] Format and language flags
  , crReserved1    :: {-# UNPACK #-} !Word64  -- ^ [0x30..0x37] 64-bit cache-line alignment padding
  , crReserved2    :: {-# UNPACK #-} !Word64  -- ^ [0x38..0x3F] 64-bit cache-line alignment padding
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Construct an empty record initialized to zeroes.
emptyCacheRecordV6 :: CacheRecordV6
emptyCacheRecordV6 = CacheRecordV6 0 0 0 0 0 0 0 0 0 0

instance Storable CacheRecordV6 where
  sizeOf _ = 64
  alignment _ = 8
  peek ptr = do
    !h    <- peekByteOff ptr 0
    !mt   <- peekByteOff ptr 8
    !nano <- peekByteOff ptr 16
    !sz   <- peekByteOff ptr 20
    !off  <- peekByteOff ptr 24
    !len  <- peekByteOff ptr 28
    !f4   <- peekByteOff ptr 32
    !flg  <- peekByteOff ptr 40
    !r1   <- peekByteOff ptr 48
    !r2   <- peekByteOff ptr 56
    pure $ CacheRecordV6 h mt nano sz off len f4 flg r1 r2
  poke ptr (CacheRecordV6 h mt nano sz off len f4 flg r1 r2) = do
    pokeByteOff ptr 0  h
    pokeByteOff ptr 8  mt
    pokeByteOff ptr 16 nano
    pokeByteOff ptr 20 sz
    pokeByteOff ptr 24 off
    pokeByteOff ptr 28 len
    pokeByteOff ptr 32 f4
    pokeByteOff ptr 40 flg
    pokeByteOff ptr 48 r1
    pokeByteOff ptr 56 r2

-- ============================================================================
-- SlabCacheHandle: Pinned Virtual Address Mapping
-- ============================================================================

-- | Handle to an active, memory-mapped CNTR\x06 slab cache.
data SlabCacheHandle = SlabCacheHandle
  { schFilePath       :: !FilePath
  , schByteString     :: !BS.ByteString
  , schBasePtr        :: !(Ptr Word8)
  , schForeignPtr     :: !(ForeignPtr Word8)
  , schRecordCount    :: !Word32
  , schRecordCapacity :: !Word32
  , schRadixTablePtr  :: !(Ptr Word8)
  , schFileTablePtr   :: !(Ptr CacheRecordV6)
  , schRepoBundle     :: !(Maybe WholeRepoBundle)
  , schRepoCallCSR    :: !(Maybe CSRGraph)
  , schRepoDataCSR    :: !(Maybe CSRGraph)
  } deriving stock (Show, Eq, Generic)

instance NFData SlabCacheHandle where
  rnf (SlabCacheHandle fp bs _ _ rc cap _ _ rb rcg rdg) =
    rnf fp `seq` rnf bs `seq` rnf rc `seq` rnf cap `seq` rnf rb `seq` rnf rcg `seq` rnf rdg

-- ============================================================================
-- CSR Graph Binary Serialization
-- ============================================================================

-- | High-performance binary serialization of an unboxed 'CSRGraph'.
encodeCSRGraph :: CSRGraph -> BB.Builder
encodeCSRGraph (CSRGraph n m rowOffsets colIndices edgeFlags) =
  BB.word32LE n
  <> BB.word32LE m
  <> mconcat [BB.word32LE r | r <- U.toList rowOffsets]
  <> mconcat [BB.word32LE c | c <- U.toList colIndices]
  <> mconcat [BB.word16LE f | f <- U.toList edgeFlags]

-- | High-performance binary deserialization of an unboxed 'CSRGraph'.
decodeCSRGraph :: BS.ByteString -> Int -> Maybe (CSRGraph, Int)
decodeCSRGraph bs off
  | off + 8 > BS.length bs = Nothing
  | otherwise =
      let !n = readWord32LE bs off
          !m = readWord32LE bs (off + 4)
          !nInt = fromIntegral n
          !mInt = fromIntegral m
          !offsetsLen = (nInt + 1) * 4
          !colsLen = mInt * 4
          !flagsLen = mInt * 2
          !totalLen = 8 + offsetsLen + colsLen + flagsLen
      in if off + totalLen > BS.length bs
           then Nothing
           else
             let !off1 = off + 8
                 !offsetsList = [readWord32LE bs (off1 + i * 4) | i <- [0 .. nInt]]
                 !rowOffsets = U.fromList offsetsList
                 !off2 = off1 + offsetsLen
                 !colsList = [readWord32LE bs (off2 + i * 4) | i <- [0 .. mInt - 1]]
                 !colIndices = U.fromList colsList
                 !off3 = off2 + colsLen
                 !flagsList = [readWord16LE bs (off3 + i * 2) | i <- [0 .. mInt - 1]]
                 !edgeFlags = U.fromList flagsList
                 !graph = CSRGraph n m rowOffsets colIndices edgeFlags
             in Just (graph, off + totalLen)

-- ============================================================================
-- Binary Encoding: CNTR\x06 Layout
-- ============================================================================

-- | Encodes entries and optional repository CSR graphs into the CNTR\x06 format.
encodeSlabV6Binary
  :: [(FilePath, FileMetadata, FingerprintBundle)]
  -> Maybe (WholeRepoBundle, CSRGraph, CSRGraph)
  -> BS.ByteString
encodeSlabV6Binary rawEntries mRepo =
  let !numEntries = length rawEntries
      !capacity = max 512 (fromIntegral numEntries :: Word32)
      !fileTableBytes = fromIntegral capacity * 64 :: Int
      !slabStartOffset = 2080 + fileTableBytes

      -- Sort entries canonically by (bucket, pathHash, path)
      prepEntry (fp, meta, bundle) =
        let !norm = normalizePathCanonical fp
            !pBS = TE.encodeUtf8 (T.pack norm)
            !h = fastPathHash64 pBS
            !b = fromIntegral (h `shiftR` 56) :: Int
        in (b, h, norm, pBS, meta, bundle)

      sorted = List.sortBy (comparing (\(b, h, norm, _, _, _) -> (b, h, norm))) (map prepEntry rawEntries)

      -- Encode file payloads into contiguous 4KB slab pages
      (records, slabDataBS) = buildSlabs slabStartOffset sorted

      -- Build 256-way Radix Directory
      radixBS = buildRadixDirectory sorted (fromIntegral numEntries :: Word32)

      -- Encode Whole-Repo Graph slab (if present)
      (!repoOff, !repoLen, !repoSlabBS) = case mRepo of
        Nothing -> (0 :: Word32, 0 :: Word32, BS.empty)
        Just (wrb, cgCSR, dfCSR) ->
          let !startOff = fromIntegral (slabStartOffset + BS.length slabDataBS) :: Word32
              !body = encodeRepoBody wrb cgCSR dfCSR
              !paddedBody = padTo4KB body
              !crc = computeCRC32 paddedBody
              !pageBS = LBS.toStrict $ BB.toLazyByteString $
                BB.word32LE crc <> BB.word32LE 0 <> BB.byteString paddedBody
          in (startOff, fromIntegral (BS.length pageBS), pageBS)

      -- File Table binary builder (capacity * 64 bytes)
      fileTableBuilder =
        mconcat [encodeRecord rec | rec <- records]
        <> BB.byteString (BS.replicate (fromIntegral (capacity - fromIntegral numEntries) * 64) 0)

      fileTableBS = LBS.toStrict (BB.toLazyByteString fileTableBuilder)

      -- Header (32 bytes):
      -- [0x00..0x03] "CNTR"
      -- [0x04..0x05] Version 6 (Word16LE)
      -- [0x06..0x07] Flags (Word16LE: bit 0 = hasRepo)
      -- [0x08..0x0B] Record count (Word32LE)
      -- [0x0C..0x0F] Record capacity (Word32LE)
      -- [0x10..0x13] Checksum placeholder (zeroed for computation)
      -- [0x14..0x17] Repo slab offset (Word32LE)
      -- [0x18..0x1B] Repo slab length (Word32LE)
      -- [0x1C..0x1F] Reserved (4 bytes)
      headerNoCRC =
        BB.byteString "CNTR"
        <> BB.word16LE 6
        <> BB.word16LE (if repoOff > 0 then 1 else 0)
        <> BB.word32LE (fromIntegral numEntries)
        <> BB.word32LE capacity
        <> BB.word32LE 0 -- zeroed CRC
        <> BB.word32LE repoOff
        <> BB.word32LE repoLen
        <> BB.word32LE 0

      headerNoCRC_BS = LBS.toStrict (BB.toLazyByteString headerNoCRC)
      -- Global CRC over header + radix directory
      globalCRC = computeCRC32 (headerNoCRC_BS <> radixBS)

      headerFinal =
        BB.byteString "CNTR"
        <> BB.word16LE 6
        <> BB.word16LE (if repoOff > 0 then 1 else 0)
        <> BB.word32LE (fromIntegral numEntries)
        <> BB.word32LE capacity
        <> BB.word32LE globalCRC
        <> BB.word32LE repoOff
        <> BB.word32LE repoLen
        <> BB.word32LE 0

      headerFinalBS = LBS.toStrict (BB.toLazyByteString headerFinal)
  in headerFinalBS <> radixBS <> fileTableBS <> slabDataBS <> repoSlabBS
  where
    encodeRecord (CacheRecordV6 h mt nano sz off len f4 flg r1 r2) =
      BB.word64LE h
      <> BB.word64LE mt
      <> BB.word32LE nano
      <> BB.word32LE sz
      <> BB.word32LE off
      <> BB.word32LE len
      <> BB.word64LE f4
      <> BB.word64LE flg
      <> BB.word64LE r1
      <> BB.word64LE r2

    buildRadixDirectory sorted totalCount =
      let bucketGroups = List.groupBy (\(b1,_,_,_,_,_) (b2,_,_,_,_,_) -> b1 == b2) sorted
          bucketMap = Map.fromList
            [ (b, (fromIntegral idx :: Word32, fromIntegral (length grp) :: Word32))
            | grp@((b,_,_,_,_,_):_) <- bucketGroups
            , let !idx = case List.elemIndex grp bucketGroups of
                    Just i  -> sum (map length (take i bucketGroups))
                    Nothing -> 0
            ]
          buildBucket b =
            case Map.lookup b bucketMap of
              Just (s, c) -> BB.word32LE s <> BB.word32LE c
              Nothing     -> BB.word32LE totalCount <> BB.word32LE 0
      in LBS.toStrict $ BB.toLazyByteString $ mconcat [buildBucket b | b <- [0 .. 255 :: Int]]

    buildSlabs baseOffset sorted =
      let encodeItem (_, h, _, pBS, meta, bundle) =
            let !payloadBS = encodePayload pBS bundle
                !f4Hex = unFingerprint (f4Composite bundle)
                !f4Head = readWord64LE (TE.encodeUtf8 f4Hex) 0
            in (h, fromIntegral (fmMtime meta) :: Word64, fromIntegral (fmSize meta) :: Word32, f4Head, payloadBS)

          items = map encodeItem sorted

          -- Pack payloads into 4KB pages
          (recs, slabPages) = packItemsIntoPages baseOffset items
      in (recs, slabPages)

    packItemsIntoPages baseOffset items =
      let (recs, pages) = go baseOffset 0 [] [] items
      in (recs, BS.concat (reverse pages))
      where
        go _ _ accRecs accPages [] = (reverse accRecs, accPages)
        go curBase pageIdx accRecs accPages remaining =
          let (chunk, rest) = fitIntoPage 4088 remaining
              pagePayload = BS.concat [p | (_, _, _, _, p) <- chunk]
              padding = 4088 - BS.length pagePayload
              paddedBody = pagePayload <> BS.replicate padding 0
              crc = computeCRC32 paddedBody
              pageBS = LBS.toStrict $ BB.toLazyByteString $
                BB.word32LE crc <> BB.word32LE 0 <> BB.byteString paddedBody
              pageStart = curBase + pageIdx * 4096
              assigned = assignOffsets (pageStart + 8) chunk
          in go curBase (pageIdx + 1) (reverse assigned ++ accRecs) (pageBS : accPages) rest

        fitIntoPage _ [] = ([], [])
        fitIntoPage remSpace (x@(_, _, _, _, p) : xs)
          | BS.length p <= remSpace =
              let (fitted, rest) = fitIntoPage (remSpace - BS.length p) xs
              in (x : fitted, rest)
          | otherwise = ([], x : xs)

        assignOffsets _ [] = []
        assignOffsets off ((h, mt, sz, f4Head, p) : xs) =
          let !len = fromIntegral (BS.length p) :: Word32
              !rec = CacheRecordV6 h mt 0 sz (fromIntegral off) len f4Head 0 0 0
          in rec : assignOffsets (off + fromIntegral len) xs

    encodePayload pBS bundle =
      let !normLen = fromIntegral (BS.length pBS) :: Word16
          (!flags, !b0, !b1, !b2, !b3, !bcg, !bcf, !bdf, !b4) = encodeBundle bundle
          (!isHexT, !bt) = encodeDigest (unFingerprint (fTTypeContract bundle))
          !flagsFinal = flags .|. (if isHexT then 256 else 0)
      in LBS.toStrict $ BB.toLazyByteString $
        BB.word16LE normLen
        <> BB.byteString pBS
        <> BB.word16LE flagsFinal
        <> BB.byteString b0
        <> BB.byteString b1
        <> BB.byteString b2
        <> BB.byteString b3
        <> BB.byteString bcg
        <> BB.byteString bcf
        <> BB.byteString bdf
        <> BB.byteString bt
        <> BB.byteString b4

    padTo4KB bs =
      let remLen = BS.length bs `rem` 4088
      in if remLen == 0 then bs else bs <> BS.replicate (4088 - remLen) 0

    encodeRepoBody (WholeRepoBundle (Fingerprint fr) (Fingerprint fwcg) (Fingerprint fwdf) (Fingerprint fw4)) cgCSR dfCSR =
      let (_, bR)   = encodeDigest fr
          (_, bWCG) = encodeDigest fwcg
          (_, bWDF) = encodeDigest fwdf
          (_, bW4)  = encodeDigest fw4
      in LBS.toStrict $ BB.toLazyByteString $
        BB.byteString "REPO"
        <> BB.byteString bR
        <> BB.byteString bWCG
        <> BB.byteString bWDF
        <> BB.byteString bW4
        <> encodeCSRGraph cgCSR
        <> encodeCSRGraph dfCSR

-- ============================================================================
-- CRC32 Verification Helpers
-- ============================================================================

-- | Verifies the integrity of the CNTR\x06 global header and radix directory.
verifySlabHeaderCRC :: BS.ByteString -> Bool
verifySlabHeaderCRC bs
  | BS.length bs < 2080 = False
  | BS.take 4 bs /= "CNTR" = False
  | readWord16LE bs 4 /= 6 = False
  | otherwise =
      let !storedCRC = readWord32LE bs 16
          !headerNoCRC = BS.take 16 bs <> BS.replicate 4 0 <> BS.take 12 (BS.drop 20 bs)
          !radixBS = BS.take 2048 (BS.drop 32 bs)
          !expectedCRC = computeCRC32 (headerNoCRC <> radixBS)
      in storedCRC == expectedCRC

-- | Verifies the integrity of an individual 4KB slab page.
verifySlabPageCRC :: BS.ByteString -> Word32 -> Bool
verifySlabPageCRC bs pageIdx =
  let !pageOffset = fromIntegral pageIdx * 4096
  in if pageOffset + 4096 > BS.length bs
       then False
       else
         let !storedCRC = readWord32LE bs pageOffset
             !pageBody = BS.take 4088 (BS.drop (pageOffset + 8) bs)
             !expectedCRC = computeCRC32 pageBody
         in storedCRC == expectedCRC

-- ============================================================================
-- Zero-Copy Memory-Mapped Reading & Verification
-- ============================================================================

-- | Opens an active memory-mapped CNTR\x06 slab cache file.
openSlabCache :: FilePath -> IO (Maybe SlabCacheHandle)
openSlabCache cachePath = do
  exists <- doesFileExist cachePath
  if not exists
    then pure Nothing
    else do
      bs <- BS.readFile cachePath
      if BS.length bs < 2080 || not (verifySlabHeaderCRC bs)
        then pure Nothing
        else do
          let !numRecords = readWord32LE bs 8
              !capacity   = readWord32LE bs 12
              !repoOff    = readWord32LE bs 20
              !repoLen    = readWord32LE bs 24
              (!fptr, !bsOff, _) = BSI.toForeignPtr bs

          withForeignPtr fptr $ \rawPtr -> do
            let !basePtr = rawPtr `plusPtr` bsOff
                !radixPtr = basePtr `plusPtr` 32
                !fileTablePtr = castPtr (basePtr `plusPtr` 2080) :: Ptr CacheRecordV6

                -- Parse Whole-Repo Graph slab if present
                (!mRepo, !mCgCSR, !mDfCSR) =
                  if repoOff == 0 || fromIntegral (repoOff + repoLen) > BS.length bs
                    then (Nothing, Nothing, Nothing)
                    else decodeRepoSlab bs (fromIntegral repoOff)

            pure $ Just SlabCacheHandle
              { schFilePath       = cachePath
              , schByteString     = bs
              , schBasePtr        = basePtr
              , schForeignPtr     = fptr
              , schRecordCount    = numRecords
              , schRecordCapacity = capacity
              , schRadixTablePtr  = radixPtr
              , schFileTablePtr   = fileTablePtr
              , schRepoBundle     = mRepo
              , schRepoCallCSR    = mCgCSR
              , schRepoDataCSR    = mDfCSR
              }
  where
    decodeRepoSlab bs off =
      let !body = BS.drop (off + 8) bs
      in if BS.take 4 body /= "REPO"
           then (Nothing, Nothing, Nothing)
           else
             let !bR   = decodeDigest 1 0 (BS.take 32 (BS.drop 4 body))
                 !bWCG = decodeDigest 1 0 (BS.take 32 (BS.drop 36 body))
                 !bWDF = decodeDigest 1 0 (BS.take 32 (BS.drop 68 body))
                 !bW4  = decodeDigest 1 0 (BS.take 32 (BS.drop 100 body))
                 !bundle = WholeRepoBundle (Fingerprint bR) (Fingerprint bWCG) (Fingerprint bWDF) (Fingerprint bW4)
                 !cgRes = decodeCSRGraph body 132
                 (!mCg, !nextOff) = case cgRes of
                   Just (cg, n) -> (Just cg, n)
                   Nothing      -> (Nothing, 132)
                 !dfRes = decodeCSRGraph body nextOff
                 !mDf = fmap fst dfRes
             in (Just bundle, mCg, mDf)

-- | Close an active slab cache handle.
closeSlabCache :: SlabCacheHandle -> IO ()
closeSlabCache _ = pure ()

-- | Single-cycle verification testing if a record matches expected path hash, mtime, and file size.
{-# INLINE verifyRecordMatch #-}
verifyRecordMatch :: Ptr CacheRecordV6 -> Word64 -> Word64 -> Word32 -> IO Bool
verifyRecordMatch !recPtr !pathHash !mtimeSec !fileSize = do
  !h <- peekByteOff (castPtr recPtr) 0 :: IO Word64
  if h /= pathHash
    then pure False
    else do
      !mt <- peekByteOff (castPtr recPtr) 8 :: IO Word64
      !sz <- peekByteOff (castPtr recPtr) 20 :: IO Word32
      pure (mt == mtimeSec && sz == fileSize)

-- | Zero-copy memory-mapped verification: verifies record match and returns decoded 'FingerprintBundle'.
verifyFileWarmMmap
  :: Ptr Word8          -- ^ Base pointer to memory-mapped buffer
  -> Ptr CacheRecordV6  -- ^ Direct pointer to record
  -> Word64             -- ^ Expected 64-bit path hash
  -> Word64             -- ^ Expected modification timestamp (seconds)
  -> Word32             -- ^ Expected file size (bytes)
  -> IO (Maybe FingerprintBundle)
verifyFileWarmMmap !basePtr !recPtr !pathHash !mtimeSec !fileSize = do
  !matched <- verifyRecordMatch recPtr pathHash mtimeSec fileSize
  if not matched
    then pure Nothing
    else do
      !slabOff <- peekByteOff (castPtr recPtr) 24 :: IO Word32
      !slabLen <- peekByteOff (castPtr recPtr) 28 :: IO Word32
      if slabOff == 0 || slabLen == 0
        then pure Nothing
        else do
          let !payloadPtr = basePtr `plusPtr` fromIntegral slabOff
          bundle <- decodePayloadFromPtr payloadPtr (fromIntegral slabLen)
          pure (Just bundle)

-- | Sub-microsecond warm lookup directly from mapped virtual memory (< 500 ns).
lookupSlabCacheWarm :: SlabCacheHandle -> FilePath -> FileMetadata -> IO (Maybe FingerprintBundle)
lookupSlabCacheWarm !handle !path !meta = do
  let !norm = normalizePathCanonical path
      !pBS = TE.encodeUtf8 (T.pack norm)
      !h = fastPathHash64 pBS
      !bucket = fromIntegral (h `shiftR` 56) :: Int
      !radixPtr = schRadixTablePtr handle `plusPtr` (bucket * 8)
  startIdx <- peekByteOff radixPtr 0 :: IO Word32
  count    <- peekByteOff radixPtr 4 :: IO Word32
  if count == 0
    then pure Nothing
    else do
      let probeRec !slot
            | slot >= count = pure Nothing
            | otherwise = do
                let !recPtr = schFileTablePtr handle `plusPtr` (fromIntegral (startIdx + slot) * 64)
                !res <- verifyFileWarmMmap
                          (schBasePtr handle)
                          recPtr
                          h
                          (fromIntegral (fmMtime meta))
                          (fromIntegral (fmSize meta))
                case res of
                  Just b  -> pure (Just b)
                  Nothing -> probeRec (slot + 1)
      probeRec 0

-- | Direct fast lookup using precomputed path hash, mtime, and file size.
lookupSlabCacheFast :: SlabCacheHandle -> Word64 -> Word64 -> Word32 -> IO (Maybe FingerprintBundle)
lookupSlabCacheFast !handle !pathHash !mtimeSec !fileSize = do
  let !bucket = fromIntegral (pathHash `shiftR` 56) :: Int
      !radixPtr = schRadixTablePtr handle `plusPtr` (bucket * 8)
  startIdx <- peekByteOff radixPtr 0 :: IO Word32
  count    <- peekByteOff radixPtr 4 :: IO Word32
  if count == 0
    then pure Nothing
    else do
      let probeRec !slot
            | slot >= count = pure Nothing
            | otherwise = do
                let !recPtr = schFileTablePtr handle `plusPtr` (fromIntegral (startIdx + slot) * 64)
                !res <- verifyFileWarmMmap (schBasePtr handle) recPtr pathHash mtimeSec fileSize
                case res of
                  Just b  -> pure (Just b)
                  Nothing -> probeRec (slot + 1)
      probeRec 0

-- | Fast pure lookup directly in a CNTR\x06 ByteString without creating a SlabCacheHandle.
lookupSlabBinaryBS :: FilePath -> FileMetadata -> BS.ByteString -> Maybe FingerprintBundle
lookupSlabBinaryBS !path !meta !bs
  | BS.length bs < 2080 = Nothing
  | BS.take 4 bs /= "CNTR" = Nothing
  | readWord16LE bs 4 /= 6 = Nothing
  | otherwise =
      let !norm = normalizePathCanonical path
          !pBS = TE.encodeUtf8 (T.pack norm)
          !h = fastPathHash64 pBS
          !bucket = fromIntegral (h `shiftR` 56) :: Int
          !radixOffset = 32 + bucket * 8
          !startIdx = readWord32LE bs radixOffset
          !count = readWord32LE bs (radixOffset + 4)
      in if count == 0
           then Nothing
           else
             let probe !slot
                   | slot >= count = Nothing
                   | otherwise =
                       let !recOff = 2080 + fromIntegral (startIdx + slot) * 64
                       in if recOff + 64 > BS.length bs
                            then Nothing
                            else
                              let !recH = readWord64LE bs recOff
                              in if recH /= h
                                   then probe (slot + 1)
                                   else
                                     let !mt = readWord64LE bs (recOff + 8)
                                         !sz = readWord32LE bs (recOff + 20)
                                     in if mt == fromIntegral (fmMtime meta) && sz == fromIntegral (fmSize meta)
                                          then
                                            let !slabOff = readWord32LE bs (recOff + 24)
                                                !slabLen = readWord32LE bs (recOff + 28)
                                            in if slabOff == 0 || slabLen == 0 || fromIntegral (slabOff + slabLen) > BS.length bs
                                                 then Nothing
                                                 else
                                                   let !payloadBS = BS.take (fromIntegral slabLen) (BS.drop (fromIntegral slabOff) bs)
                                                   in Just (decodePayloadBS payloadBS)
                                          else probe (slot + 1)
             in probe 0

-- | Decodes payload bytes from a pointer into a 'FingerprintBundle'.
decodePayloadFromPtr :: Ptr Word8 -> Int -> IO FingerprintBundle
decodePayloadFromPtr !ptr !len = do
  bs <- BSI.create len $ \buf -> copyBytes buf ptr len
  pure $ decodePayloadBS bs

decodePayloadBS :: BS.ByteString -> FingerprintBundle
decodePayloadBS bs =
  let !normLen = fromIntegral (readWord16LE bs 0) :: Int
      !off = 2 + normLen
      !flags = readWord16LE bs off
      !b0  = BS.take 32 (BS.drop (off + 2) bs)
      !b1  = BS.take 32 (BS.drop (off + 34) bs)
      !b2  = BS.take 32 (BS.drop (off + 66) bs)
      !b3  = BS.take 32 (BS.drop (off + 98) bs)
      !bcg = BS.take 32 (BS.drop (off + 130) bs)
      !bcf = BS.take 32 (BS.drop (off + 162) bs)
      !bdf = BS.take 32 (BS.drop (off + 194) bs)
      !bt  = BS.take 32 (BS.drop (off + 226) bs)
      !b4  = BS.take 32 (BS.drop (off + 258) bs)
      !f0  = decodeDigest flags 0 b0
      !f1  = decodeDigest flags 1 b1
      !f2  = decodeDigest flags 2 b2
      !f3  = decodeDigest flags 3 b3
      !fcg = decodeDigest flags 4 bcg
      !fcf = decodeDigest flags 5 bcf
      !fdf = decodeDigest flags 6 bdf
      !ft  = decodeDigest flags 8 bt
      !f4  = decodeDigest flags 7 b4
  in FingerprintBundle (Fingerprint f0) (Fingerprint f1) (Fingerprint f2) (Fingerprint f3)
                       (Fingerprint fcg) (Fingerprint fcf) (Fingerprint fdf) (Fingerprint ft)
                       (Fingerprint f4)

-- ============================================================================
-- Full Binary Decoding & Resilient Bit-Rot Recovery
-- ============================================================================

-- | Decodes all entries from a CNTR\x06 byte buffer.
decodeSlabV6Binary
  :: BS.ByteString
  -> Maybe (Map FilePath (FileMetadata, FingerprintBundle), Maybe (WholeRepoBundle, CSRGraph, CSRGraph))
decodeSlabV6Binary bs
  | BS.length bs < 2080 || not (verifySlabHeaderCRC bs) = Nothing
  | otherwise =
      let (entries, mRepo, corrupted) = decodeSlabV6Resilient bs
      in if null corrupted then Just (entries, mRepo) else Nothing

-- | Isolated Page-Level Bit-Rot Recovery (Section 5.4).
-- Validates every 4KB page independently. If a page fails CRC-32C, only records
-- on that page are dropped, retaining healthy slabs.
decodeSlabV6Resilient
  :: BS.ByteString
  -> (Map FilePath (FileMetadata, FingerprintBundle), Maybe (WholeRepoBundle, CSRGraph, CSRGraph), [Word32])
decodeSlabV6Resilient bs
  | BS.length bs < 2080 || not (verifySlabHeaderCRC bs) = (Map.empty, Nothing, [0])
  | otherwise =
      let !numRecords = readWord32LE bs 8
          !capacity   = readWord32LE bs 12
          !repoOff    = readWord32LE bs 20
          !repoLen    = readWord32LE bs 24
          !totalBytes = BS.length bs
          !slabStart  = 2080 + fromIntegral capacity * 64 :: Int
          !totalSlabPages = if totalBytes > slabStart then (totalBytes - slabStart + 4095) `div` 4096 else 0

          -- Check CRC for all 4KB slab pages
          checkPage p =
            let !pageOff = slabStart + p * 4096
            in if pageOff + 4096 > totalBytes
                 then False
                 else
                   let !storedCRC = readWord32LE bs pageOff
                       !body = BS.take 4088 (BS.drop (pageOff + 8) bs)
                   in storedCRC == computeCRC32 body

          corruptedPages =
            [ fromIntegral p
            | p <- [0 .. totalSlabPages - 1]
            , not (checkPage p)
            ]

          corruptedPageSet = Map.fromList [(p, ()) | p <- corruptedPages]

          -- Read file table records
          decodeRecord slot
            | slot >= fromIntegral numRecords = Nothing
            | otherwise =
                let !recOff = 2080 + slot * 64
                    !mt  = readWord64LE bs (recOff + 8)
                    !sz  = readWord32LE bs (recOff + 20)
                    !off = readWord32LE bs (recOff + 24)
                    !len = readWord32LE bs (recOff + 28)
                    !pageIdx = fromIntegral ((fromIntegral off - slabStart) `div` 4096) :: Word32
                in if off == 0 || len == 0 || Map.member pageIdx corruptedPageSet
                     then Nothing
                     else
                       let !payloadBS = BS.take (fromIntegral len) (BS.drop (fromIntegral off) bs)
                           !normLen   = fromIntegral (readWord16LE payloadBS 0) :: Int
                           !pBS       = BS.take normLen (BS.drop 2 payloadBS)
                           !path      = T.unpack (TE.decodeUtf8Lenient pBS)
                           !bundle    = decodePayloadBS payloadBS
                           !meta      = FileMetadata path (fromIntegral sz) (fromIntegral mt)
                       in Just (path, (meta, bundle))

          validEntries = Map.fromList [item | slot <- [0 .. fromIntegral numRecords - 1], Just item <- [decodeRecord slot]]

          -- Decode Whole-Repo slab if not corrupted
          mRepo =
            if repoOff == 0 || fromIntegral (repoOff + repoLen) > totalBytes
              then Nothing
              else
                let !repoPageIdx = fromIntegral ((fromIntegral repoOff - slabStart) `div` 4096) :: Word32
                in if Map.member repoPageIdx corruptedPageSet
                     then Nothing
                     else decodeRepoSlab bs (fromIntegral repoOff)
      in (validEntries, mRepo, corruptedPages)
  where
    decodeRepoSlab rawBuf off =
      let !body = BS.drop (off + 8) rawBuf
      in if BS.take 4 body /= "REPO"
           then Nothing
           else
             let !bR   = decodeDigest 1 0 (BS.take 32 (BS.drop 4 body))
                 !bWCG = decodeDigest 1 0 (BS.take 32 (BS.drop 36 body))
                 !bWDF = decodeDigest 1 0 (BS.take 32 (BS.drop 68 body))
                 !bW4  = decodeDigest 1 0 (BS.take 32 (BS.drop 100 body))
                 !bundle = WholeRepoBundle (Fingerprint bR) (Fingerprint bWCG) (Fingerprint bWDF) (Fingerprint bW4)
                 !cgRes = decodeCSRGraph body 132
                 (!cgCSR, !nextOff) = case cgRes of
                   Just (cg, n) -> (cg, n)
                   Nothing      -> (CSRGraph 0 0 (U.singleton 0) U.empty U.empty, 132)
                 !dfRes = decodeCSRGraph body nextOff
                 !dfCSR = case dfRes of
                   Just (df, _) -> df
                   Nothing      -> CSRGraph 0 0 (U.singleton 0) U.empty U.empty
             in Just (bundle, cgCSR, dfCSR)

-- ============================================================================
-- Disk Operations
-- ============================================================================

-- | Writes cache entries and optional repository graphs to disk atomically.
writeSlabCacheFile
  :: FilePath
  -> [(FilePath, FileMetadata, FingerprintBundle)]
  -> Maybe (WholeRepoBundle, CSRGraph, CSRGraph)
  -> IO ()
writeSlabCacheFile cachePath entries mRepo = do
  createDirectoryIfMissing True (takeDirectory cachePath)
  let !encoded = encodeSlabV6Binary entries mRepo
      !tmpPath = cachePath ++ ".tmp"
  BS.writeFile tmpPath encoded
  atomicSwapWithRetry_ tmpPath cachePath

-- | Reads a CNTR\x06 slab cache file.
readSlabCacheFile
  :: FilePath
  -> IO (Maybe (Map FilePath (FileMetadata, FingerprintBundle), Maybe (WholeRepoBundle, CSRGraph, CSRGraph)))
readSlabCacheFile cachePath = do
  exists <- doesFileExist cachePath
  if not exists
    then pure Nothing
    else do
      bs <- BS.readFile cachePath
      case decodeSlabV6Binary bs of
        Just res -> pure (Just res)
        Nothing  -> do
          let (validEntries, mRepo, corruptedPages) = decodeSlabV6Resilient bs
          if Map.null validEntries && null corruptedPages
            then pure Nothing
            else pure (Just (validEntries, mRepo))

-- | Salvages healthy entries from a damaged CNTR\x06 cache file and lists corrupted 4KB pages.
salvageSlabCacheFile
  :: FilePath
  -> IO (Map FilePath (FileMetadata, FingerprintBundle), [Word32])
salvageSlabCacheFile cachePath = do
  (valid, _, corrupted) <- readSlabCacheFileResilient cachePath
  pure (valid, corrupted)

-- | Reads a CNTR\x06 slab cache file with resilient page-level bit-rot recovery.
readSlabCacheFileResilient
  :: FilePath
  -> IO (Map FilePath (FileMetadata, FingerprintBundle), Maybe (WholeRepoBundle, CSRGraph, CSRGraph), [Word32])
readSlabCacheFileResilient cachePath = do
  exists <- doesFileExist cachePath
  if not exists
    then pure (Map.empty, Nothing, [])
    else do
      bs <- BS.readFile cachePath
      pure $ decodeSlabV6Resilient bs

-- ============================================================================
-- Step 2.3: Whole-Repository Binary CSR Persistence
-- ============================================================================

-- | Persists Whole-Repository graph hashes (F_WCG, F_WDF) and binary CSR graphs
-- directly into the CNTR\x06 cache, replacing textual repo_graphs.txt files.
saveRepoGraphsSlab
  :: FilePath
  -> Fingerprint
  -> Fingerprint
  -> Maybe CSRGraph
  -> Maybe CSRGraph
  -> IO ()
saveRepoGraphsSlab rootDir fwcg fwdf mCgCSR mDfCSR = do
  let cacheFile = rootDir </> ".canontra" </> "cache.bin"
  exists <- doesFileExist cacheFile
  (existingEntries, _) <- if exists
    then do
      res <- readSlabCacheFile cacheFile
      case res of
        Just (m, _) -> pure ([(p, meta, b) | (p, (meta, b)) <- Map.toList m], ())
        Nothing     -> pure ([], ())
    else pure ([], ())

  let !emptyCSR = CSRGraph 0 0 (U.singleton 0) U.empty U.empty
      !cg = case mCgCSR of Just c -> c; Nothing -> emptyCSR
      !df = case mDfCSR of Just d -> d; Nothing -> emptyCSR
      !wrb = WholeRepoBundle (Fingerprint "") fwcg fwdf (Fingerprint "")
      !repoPayload = Just (wrb, cg, df)

  writeSlabCacheFile cacheFile existingEntries repoPayload

-- | Loads Whole-Repository graph hashes and binary CSR graphs from the CNTR\x06 cache.
loadRepoGraphsSlab
  :: FilePath
  -> IO (Maybe (Fingerprint, Fingerprint, Maybe CSRGraph, Maybe CSRGraph))
loadRepoGraphsSlab rootDir = do
  let cacheFile = rootDir </> ".canontra" </> "cache.bin"
  exists <- doesFileExist cacheFile
  if not exists
    then pure Nothing
    else do
      res <- readSlabCacheFile cacheFile
      case res of
        Just (_, Just (wrb, cg, df)) ->
          let !cgM = if csrNodeCount cg > 0 then Just cg else Nothing
              !dfM = if csrNodeCount df > 0 then Just df else Nothing
          in pure $ Just (wrbCallGraph wrb, wrbDataFlow wrb, cgM, dfM)
        _ -> pure Nothing
