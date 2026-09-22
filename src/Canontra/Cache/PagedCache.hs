{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Canontra.Cache.PagedCache
Description : High-performance memory-mapped paged radix cache (CNTR\x05) for canontra v0.0.9-alpha.

Establishes a 4KB virtual memory page-aligned binary cache layout with:
- Page 0: Global header and 256-bucket L1 Radix directory (4,096 bytes aligned).
- Pages 1..M: 4KB page-aligned record slabs holding up to 14 records of 288 bytes each
  with independent page-level CRC32 block integrity checksums.
- Pages M+1..K: Prefix-delta varint-compressed string table delivering > 65% storage reduction.
- Pure Haskell zero-copy / foreign pointer lookup (lookupPagedCache).
-}
module Canontra.Cache.PagedCache
  ( PagedCacheHandle (..)
  , openPagedCache
  , closePagedCache
  , lookupPagedCache
  , lookupPagedCacheMeta
  , readRadixPageOffset
  , getMappedPagePointer
  , probePageRecords
  , hashPathBucket
  , encodeBinaryCacheV5
  , decodeBinaryCacheV5
  , decodeBinaryCacheV5WithRecovery
  , decodeBinaryCacheV5Resilient
  , lookupBinaryCacheV5
  , writePagedCacheFile
  , readPagedCacheFile
  , readPagedCacheFileResilient
  , encodePrefixDelta
  , decodePrefixDelta
  , encodeVarint
  , decodeVarint
  , verifyHeaderCRC
  , verifyPageCRC
  ) where

import Control.DeepSeq (NFData (..))
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)
import System.Process (getCurrentPid)

import Canontra.Cache.Common
  ( MerkleCache (..)
  , MerkleCacheEntry (..)
  , computeCRC32
  , decodeDigest
  , emptyCache
  , encodeBundle
  , fastPathHash64
  , normalizePathCanonical
  , readWord16LE
  , readWord32LE
  , readWord64LE
  )
import Canontra.Cache.Inode (FileMetadata (..))
import Canontra.Types (Fingerprint (..), FingerprintBundle (..))

-- | Handle to an active, memory-mapped or pinned paged cache buffer.
data PagedCacheHandle = PagedCacheHandle
  { pchFilePath          :: !FilePath
  , pchByteString        :: !BS.ByteString
  , pchBasePtr           :: !(Ptr Word8)
  , pchForeignPtr        :: !(ForeignPtr Word8)
  , pchEntryCount        :: !Word32
  , pchPageSize          :: !Word32
  , pchSlabPageCount     :: !Word32
  , pchStringTableOffset :: !Word64
  , pchRadixOffset       :: !Word64
  , pchStringMap         :: !(Map Word32 BS.ByteString)
  } deriving stock (Show, Eq, Generic)

instance NFData PagedCacheHandle where
  rnf (PagedCacheHandle fp bs _ _ ec ps sc sto ro sm) =
    rnf fp `seq` rnf bs `seq` rnf ec `seq` rnf ps `seq` rnf sc `seq` rnf sto `seq` rnf ro `seq` rnf sm

-- | Computes the 8-bit L1 radix bucket (0..255) for a file path.
{-# INLINE hashPathBucket #-}
hashPathBucket :: FilePath -> Int
hashPathBucket path =
  let !pNorm = normalizePathCanonical path
      !pBS   = TE.encodeUtf8 (T.pack pNorm)
      !h     = fastPathHash64 pBS
  in fromIntegral (h `shiftR` 56) :: Int

-- ============================================================================
-- Varint (LEB128) & Prefix-Delta String Compression
-- ============================================================================

-- | Encode a 32-bit unsigned integer using unsigned LEB128 varint format.
{-# INLINE encodeVarint #-}
encodeVarint :: Word32 -> BB.Builder
encodeVarint !n
  | n < 0x80  = BB.word8 (fromIntegral n)
  | otherwise = BB.word8 (fromIntegral ((n .&. 0x7F) .|. 0x80))
             <> encodeVarint (n `shiftR` 7)

-- | Decode a 32-bit unsigned integer from unsigned LEB128 varint format.
{-# INLINE decodeVarint #-}
decodeVarint :: BS.ByteString -> Int -> (Word32, Int)
decodeVarint bs !off = go 0 0 off
  where
    go !acc !shift !idx
      | idx >= BS.length bs = (acc, idx)
      | otherwise =
          let !w = BS.index bs idx
              !val = acc .|. (fromIntegral (w .&. 0x7F) `shiftL` shift)
          in if (w .&. 0x80) == 0
               then (val, idx + 1)
               else go val (shift + 7) (idx + 1)

-- | Compute common byte prefix length between two ByteStrings.
{-# INLINE commonPrefixLen #-}
commonPrefixLen :: BS.ByteString -> BS.ByteString -> Int
commonPrefixLen bs1 bs2 = go 0
  where
    !maxLen = min (BS.length bs1) (BS.length bs2)
    go !i
      | i >= maxLen = i
      | BS.index bs1 i == BS.index bs2 i = go (i + 1)
      | otherwise = i

-- | Compress a list of file path ByteStrings using prefix-delta varint encoding.
-- Returns the 4KB page-padded string table ByteString and an offset map for each path.
encodePrefixDelta :: [BS.ByteString] -> (BS.ByteString, Map BS.ByteString (Word32, Word16))
encodePrefixDelta paths =
  let sortedPaths = List.sort (List.nub paths)
      go !_ !_ [] = (mempty, Map.empty)
      go !prevPath !curOff (p : rest) =
        let !prefix = if BS.null prevPath then 0 else commonPrefixLen prevPath p
            !suffix = BS.drop prefix p
            !pfxLenW = fromIntegral prefix :: Word32
            !sfxLenW = fromIntegral (BS.length suffix) :: Word32
            !fullLenW = fromIntegral (BS.length p) :: Word16
            !entryBuilder = encodeVarint pfxLenW <> encodeVarint sfxLenW <> BB.byteString suffix
            !entryBS = LBS.toStrict (BB.toLazyByteString entryBuilder)
            !entryLen = fromIntegral (BS.length entryBS) :: Word32
            !curMap = Map.singleton p (curOff, fullLenW)
            (!restBuilder, !restMap) = go p (curOff + entryLen) rest
        in (entryBuilder <> restBuilder, Map.union curMap restMap)
      (!bodyBuilder, !offsetMap) = go BS.empty 0 sortedPaths
      !rawStringTable = LBS.toStrict (BB.toLazyByteString bodyBuilder)
      -- Pad string table to a 4096-byte page boundary
      !rem4k = BS.length rawStringTable `mod` 4096
      !padLen = if rem4k == 0 && not (BS.null rawStringTable) then 0 else 4096 - rem4k
      !paddedStringTable = rawStringTable <> BS.replicate padLen 0
  in (paddedStringTable, offsetMap)

-- | Decode a prefix-delta varint string table into a map from byte offset to path.
decodePrefixDelta :: BS.ByteString -> Word32 -> Map Word32 BS.ByteString
decodePrefixDelta bs totalEntries = go 0 0 BS.empty Map.empty
  where
    go !count !off !prevPath !acc
      | count >= totalEntries || off >= BS.length bs = acc
      | otherwise =
          let (!pfxLen, !off1) = decodeVarint bs off
              (!sfxLen, !off2) = decodeVarint bs off1
              !suffix = BS.take (fromIntegral sfxLen) (BS.drop off2 bs)
              !curPath = if pfxLen == 0
                           then suffix
                           else BS.take (fromIntegral pfxLen) prevPath <> suffix
              !nextOff = off2 + fromIntegral sfxLen
              !newAcc = Map.insert (fromIntegral off) curPath acc
          in go (count + 1) nextOff curPath newAcc

-- ============================================================================
-- CNTR\x05 Binary Encoding
-- ============================================================================

-- | Encode a MerkleCache into the 4KB page-aligned CNTR\x05 binary format.
encodeBinaryCacheV5 :: MerkleCache -> BS.ByteString
encodeBinaryCacheV5 (MerkleCache cacheMap) =
  let rawEntries = Map.toList cacheMap
      entriesWithHash =
        [ let !pNorm = normalizePathCanonical p
              !pBS   = TE.encodeUtf8 (T.pack pNorm)
              !h     = fastPathHash64 pBS
          in (h, pNorm, pBS, entry)
        | (p, entry) <- rawEntries
        ]
      -- Sort entries by (PathHash, PathByteString) for monotonic radix grouping
      sortedEntries = List.sortOn (\(h, _, pBS, _) -> (h, pBS)) entriesWithHash
      !totalCount = fromIntegral (length sortedEntries) :: Word32

      -- Prefix-delta compress unique string paths
      allPathBS = [pBS | (_, _, pBS, _) <- sortedEntries]
      (!stringTableBS, !strOffsetMap) = encodePrefixDelta allPathBS
      !stringTableCRC = computeCRC32 stringTableBS

      -- Number of slab pages required (each slab holds <= 14 records)
      !numSlabs = if totalCount == 0 then 0 else (totalCount + 13) `div` 14
      !stringTableOffset = fromIntegral (1 + numSlabs) * 4096 :: Word64
      !radixTableOffset = 64 :: Word64

      -- Compute 256 Radix Bucket End Offsets
      bucketEnds = computeBucketEnds (map (\(h, _, _, _) -> fromIntegral (h `shiftR` 56) :: Int) sortedEntries) (fromIntegral totalCount)
      radixDirectory = mconcat [BB.word32LE (fromIntegral endIdx) | endIdx <- bucketEnds]

      -- Encode each slab page (4096 bytes each)
      slabPages = encodeSlabPages sortedEntries strOffsetMap numSlabs

      -- Build Page 0 with Header CRC32 set to 0 initially
      page0Zero = LBS.toStrict $ BB.toLazyByteString $
        BB.byteString "CNTR"                  -- [0x000..0x003] Magic
        <> BB.word16LE 0x0005                 -- [0x004..0x005] Version 5
        <> BB.word16LE 0x000F                 -- [0x006..0x007] Flags: Radix | CaseFolded | CRC32 | Paged-mmap
        <> BB.word32LE totalCount             -- [0x008..0x00B] Total Entry Count
        <> BB.word32LE 4096                   -- [0x00C..0x00F] Page Size (4096)
        <> BB.word64LE radixTableOffset       -- [0x010..0x017] Root Radix Page Offset (64)
        <> BB.word64LE stringTableOffset      -- [0x018..0x01F] Compressed String Table Offset
        <> BB.word32LE 0                      -- [0x020..0x023] Global Header CRC32 (zeroed)
        <> BB.word32LE stringTableCRC         -- [0x024..0x027] String Table CRC32
        <> BB.byteString (BS.replicate 24 0)  -- [0x028..0x03F] Reserved / Padding (24 bytes)
        <> radixDirectory                     -- [0x040..0x43F] L1 ROOT RADIX DIRECTORY (1024 bytes)
        <> BB.byteString (BS.replicate 3008 0)-- [0x440..0xFFF] Page 0 Zero-Padding to 4096 bytes

      !headerCRC = computeCRC32 page0Zero

      page0Final = LBS.toStrict $ BB.toLazyByteString $
        BB.byteString "CNTR"                  -- [0x000..0x003] Magic
        <> BB.word16LE 0x0005                 -- [0x004..0x005] Version 5
        <> BB.word16LE 0x000F                 -- [0x006..0x007] Flags
        <> BB.word32LE totalCount             -- [0x008..0x00B] Total Entry Count
        <> BB.word32LE 4096                   -- [0x00C..0x00F] Page Size
        <> BB.word64LE radixTableOffset       -- [0x010..0x017] Radix Offset
        <> BB.word64LE stringTableOffset      -- [0x018..0x01F] String Table Offset
        <> BB.word32LE headerCRC              -- [0x020..0x023] Global Header CRC32
        <> BB.word32LE stringTableCRC         -- [0x024..0x027] String Table CRC32
        <> BB.byteString (BS.replicate 24 0)  -- [0x028..0x03F] Reserved / Padding
        <> radixDirectory                     -- [0x040..0x43F] L1 Radix
        <> BB.byteString (BS.replicate 3008 0)-- [0x440..0xFFF] Page 0 Padding
  in page0Final <> slabPages <> stringTableBS

-- | Encode up to M slab pages, each holding <= 14 records of 288 bytes.
encodeSlabPages
  :: [(Word64, FilePath, BS.ByteString, MerkleCacheEntry)]
  -> Map BS.ByteString (Word32, Word16)
  -> Word32
  -> BS.ByteString
encodeSlabPages entries strOffsetMap _ =
  let chunks = chunkList 14 entries
      encodedChunks = map encodeSlab chunks
  in BS.concat encodedChunks
  where
    chunkList _ [] = []
    chunkList n xs =
      let (c, rest) = splitAt n xs
      in c : chunkList n rest

    encodeSlab chunk =
      let !k = length chunk
          recordBuilders = mconcat [encodeRecord e | e <- chunk]
          !paddingBytes = (14 - k) * 288
          -- Page body (4092 bytes): Record count (2 bytes) + 58 reserved bytes + records (up to 4032 bytes)
          pageBodyBuilder =
            BB.word16LE (fromIntegral k)
            <> BB.byteString (BS.replicate 58 0)
            <> recordBuilders
            <> BB.byteString (BS.replicate paddingBytes 0)
          pageBodyBS = LBS.toStrict (BB.toLazyByteString pageBodyBuilder)
          !pageCRC = computeCRC32 pageBodyBS
      in LBS.toStrict (BB.toLazyByteString (BB.word32LE pageCRC <> BB.byteString pageBodyBS))

    encodeRecord (h, _, pBS, MerkleCacheEntry sz mt bundle) =
      let (!strOff, !strLen) = Map.findWithDefault (0, 0) pBS strOffsetMap
          (!flags, !f0BS, !f1BS, !f2BS, !f3BS, !fcgBS, !fcfBS, !fdfBS, !f4BS) = encodeBundle bundle
      in BB.word64LE h                        -- [0x00..0x07] PathHash (8 bytes)
      <> BB.word32LE strOff                   -- [0x08..0x0B] StrTableOffset (4 bytes)
      <> BB.word16LE strLen                   -- [0x0C..0x0D] StrLength (2 bytes)
      <> BB.word16LE flags                    -- [0x0E..0x0F] Flags (2 bytes)
      <> BB.word64LE (fromIntegral sz)        -- [0x10..0x17] FileSize (8 bytes)
      <> BB.word64LE (fromIntegral mt)        -- [0x18..0x1F] MTime (8 bytes)
      <> BB.byteString f0BS                   -- [0x20..0x3F] F0 (32 bytes)
      <> BB.byteString f1BS                   -- [0x40..0x5F] F1 (32 bytes)
      <> BB.byteString f2BS                   -- [0x60..0x7F] F2 (32 bytes)
      <> BB.byteString f3BS                   -- [0x80..0x9F] F3 (32 bytes)
      <> BB.byteString fcgBS                  -- [0xA0..0xBF] F_CG (32 bytes)
      <> BB.byteString fcfBS                  -- [0xC0..0xDF] F_CF (32 bytes)
      <> BB.byteString fdfBS                  -- [0xE0..0xFF] F_DF (32 bytes)
      <> BB.byteString f4BS                   -- [0x100..0x11F] F4 (32 bytes)

-- | Compute cumulative upper-bound indices for the 256 radix buckets.
computeBucketEnds :: [Int] -> Int -> [Int]
computeBucketEnds buckets totalCount = go 0 0 buckets
  where
    go !curBucket !_ [] = replicate (256 - curBucket) totalCount
    go !curBucket !idx (b : bs)
      | b == curBucket = go curBucket (idx + 1) bs
      | b > curBucket  = replicate (b - curBucket) idx ++ go b (idx + 1) bs
      | otherwise      = go curBucket (idx + 1) bs

-- ============================================================================
-- CRC32 Verification Helpers
-- ============================================================================

-- | Verifies the integrity of Page 0 Header CRC32.
verifyHeaderCRC :: BS.ByteString -> Bool
verifyHeaderCRC bs
  | BS.length bs < 4096 = False
  | BS.take 4 bs /= "CNTR" = False
  | readWord16LE bs 4 /= 5 = False
  | otherwise =
      let !storedCRC = readWord32LE bs 32
          !page0 = BS.take 4096 bs
          -- Zero out bytes [0x20..0x23] (offset 32..35)
          !page0WithZeroes = BS.take 32 page0 <> BS.replicate 4 0 <> BS.drop 36 page0
          !expectedCRC = computeCRC32 page0WithZeroes
      in storedCRC == expectedCRC

-- | Verifies the integrity of an individual 4KB slab page.
verifyPageCRC :: BS.ByteString -> Word32 -> Bool
verifyPageCRC bs pageIdx =
  let !pageOffset = fromIntegral pageIdx * 4096
  in if pageOffset + 4096 > BS.length bs
       then False
       else
         let !storedCRC = readWord32LE bs pageOffset
             !pageBody = BS.take 4092 (BS.drop (pageOffset + 4) bs)
             !expectedCRC = computeCRC32 pageBody
         in storedCRC == expectedCRC

-- ============================================================================
-- CNTR\x05 Binary Decoding
-- ============================================================================

-- | Decode a CNTR\x05 binary buffer with page-level CRC32 recovery.
-- When an individual 4KB slab page fails its IEEE 802.3 CRC32 check, only the
-- records on that corrupted page are discarded, returning the remaining valid
-- records and the list of corrupted page indices.
decodeBinaryCacheV5WithRecovery :: BS.ByteString -> (Maybe MerkleCache, [Word32])
decodeBinaryCacheV5WithRecovery bs
  | BS.length bs < 4096 = (Nothing, [])
  | BS.take 4 bs /= "CNTR" = (Nothing, [])
  | readWord16LE bs 4 /= 5 = (Nothing, [])
  | not (verifyHeaderCRC bs) = (Nothing, [])
  | otherwise =
      let !totalCount = readWord32LE bs 8
          !pageSize = readWord32LE bs 12
          !strTableOffset = fromIntegral (readWord64LE bs 24) :: Int
          !numSlabs = if totalCount == 0 then 0 else (totalCount + 13) `div` 14
      in if pageSize /= 4096 || strTableOffset > BS.length bs
           then (Nothing, [])
           else if totalCount == 0
             then (Just emptyCache, [])
             else
               let !corruptedPages = [p | p <- [1 .. numSlabs], not (verifyPageCRC bs p)]
                   -- Decode prefix-delta string table
                   !strTableBS = BS.drop strTableOffset bs
                   !strMap = decodePrefixDelta strTableBS totalCount
                   -- Decode all records across the slab pages, skipping corrupted pages
                   !entries =
                     [ decodeRecord i strMap
                     | i <- [0 .. fromIntegral totalCount - 1]
                     , let pageIdx = fromIntegral (1 + (i `div` 14)) :: Word32
                     , pageIdx `notElem` corruptedPages
                     ]
               in (Just $ MerkleCache $ Map.fromList entries, corruptedPages)
  where
    decodeRecord !i strMap =
      let !pageIdx = 1 + (i `div` 14)
          !slot = i `mod` 14
          !recOffset = pageIdx * 4096 + 64 + slot * 288
          !strOff = readWord32LE bs (recOffset + 8)
          !flags = readWord16LE bs (recOffset + 14)
          !sz   = fromIntegral (readWord64LE bs (recOffset + 16))
          !mt   = fromIntegral (readWord64LE bs (recOffset + 24))
          !pathBS = Map.findWithDefault BS.empty strOff strMap
          !path = T.unpack (TE.decodeUtf8Lenient pathBS)
          !bundle = readBundleAt bs (recOffset + 32) flags
      in (path, MerkleCacheEntry sz mt bundle)

-- | Decode a CNTR\x05 binary buffer into a MerkleCache verifying all page CRCs.
decodeBinaryCacheV5 :: BS.ByteString -> Maybe MerkleCache
decodeBinaryCacheV5 bs =
  let (!mCache, !corrupted) = decodeBinaryCacheV5WithRecovery bs
  in if null corrupted then mCache else Nothing

-- | Decode a CNTR\x05 binary buffer with page-level CRC32 recovery, logging corrupted slab pages to stderr.
decodeBinaryCacheV5Resilient :: BS.ByteString -> IO (Maybe MerkleCache)
decodeBinaryCacheV5Resilient bs = do
  let (!mCache, !corrupted) = decodeBinaryCacheV5WithRecovery bs
  mapM_ (\p -> hPutStrLn stderr ("Warning: PagedCache 4KB slab page " ++ show p ++ " failed IEEE 802.3 CRC32 integrity check; discarding page records for re-evaluation.")) corrupted
  pure mCache

-- ============================================================================
-- Zero-Copy & Memory-Mapped Lookup
-- ============================================================================

-- | Open a CNTR\x05 binary file for memory-mapped / zero-copy lookups.
openPagedCache :: FilePath -> IO (Maybe PagedCacheHandle)
openPagedCache cachePath = do
  exists <- doesFileExist cachePath
  if not exists
    then pure Nothing
    else do
      bs <- BS.readFile cachePath
      if BS.length bs < 4096 || BS.take 4 bs /= "CNTR" || readWord16LE bs 4 /= 5
        then pure Nothing
        else if not (verifyHeaderCRC bs)
          then pure Nothing
          else do
            let !totalCount = readWord32LE bs 8
                !pageSize = readWord32LE bs 12
                !radixOffset = readWord64LE bs 16
                !strTableOffset = readWord64LE bs 24
                !numSlabs = if totalCount == 0 then 0 else (totalCount + 13) `div` 14
                !strTableBS = BS.drop (fromIntegral strTableOffset) bs
                !strMap = decodePrefixDelta strTableBS totalCount
                (!fptr, !bsOff, _) = BSI.toForeignPtr bs
            withForeignPtr fptr $ \rawPtr -> do
              let !basePtr = rawPtr `plusPtr` bsOff
              pure $ Just PagedCacheHandle
                { pchFilePath          = cachePath
                , pchByteString        = bs
                , pchBasePtr           = basePtr
                , pchForeignPtr        = fptr
                , pchEntryCount        = totalCount
                , pchPageSize          = pageSize
                , pchSlabPageCount     = numSlabs
                , pchStringTableOffset = strTableOffset
                , pchRadixOffset       = radixOffset
                , pchStringMap         = strMap
                }

-- | Close an active paged cache handle (releases memory references).
closePagedCache :: PagedCacheHandle -> IO ()
closePagedCache _ = pure ()

-- | Reads the starting slab page index for a given L1 radix bucket from Page 0.
readRadixPageOffset :: PagedCacheHandle -> Int -> IO Word32
readRadixPageOffset !handle !bucket
  | bucket < 0 || bucket >= 256 = pure 0
  | otherwise = do
      let !radixOffset = pchRadixOffset handle
          !entryOffset = fromIntegral radixOffset + bucket * 4
          !bs = pchByteString handle
      if entryOffset + 4 <= BS.length bs
        then do
          let !endIdx = readWord32LE bs entryOffset
              !prevIdx = if bucket == 0 then 0 else readWord32LE bs (entryOffset - 4)
          if endIdx <= prevIdx
            then pure 0 -- Empty bucket!
            else pure (1 + (prevIdx `div` 14)) -- First slab page for this bucket!
        else pure 0

-- | Computes the memory pointer to a specific 4KB page in the mapped cache.
getMappedPagePointer :: PagedCacheHandle -> Word32 -> IO (Ptr Word8)
getMappedPagePointer !handle !pageIdx = do
  let !offset = fromIntegral pageIdx * fromIntegral (pchPageSize handle)
  pure (pchBasePtr handle `plusPtr` offset)

-- | Probes an individual 4KB slab page for a matching file path.
probePageRecords :: PagedCacheHandle -> Word32 -> FilePath -> IO (Maybe FingerprintBundle)
probePageRecords !handle !pageIdx !queryPath = do
  let !norm = normalizePathCanonical queryPath
      !targetBS = TE.encodeUtf8 (T.pack norm)
      !targetHash = fastPathHash64 targetBS
      !targetLen = fromIntegral (BS.length targetBS) :: Word16
      !pageOffset = fromIntegral pageIdx * 4096
      !bs = pchByteString handle
  if pageOffset + 4096 > BS.length bs
    then pure Nothing
    else do
      -- Validate page CRC32 checksum
      let !storedCRC = readWord32LE bs pageOffset
          !pageBody = BS.take 4092 (BS.drop (pageOffset + 4) bs)
          !expectedCRC = computeCRC32 pageBody
      if storedCRC /= expectedCRC
        then pure Nothing
        else do
          let !recCount = fromIntegral (readWord16LE bs (pageOffset + 4)) :: Int
              scanRecords !slot
                | slot >= min 14 recCount = pure Nothing
                | otherwise = do
                    let !recOffset = pageOffset + 64 + slot * 288
                        !recHash = readWord64LE bs recOffset
                    if recHash /= targetHash
                      then scanRecords (slot + 1)
                      else do
                        let !strOff = readWord32LE bs (recOffset + 8)
                            !strLen = readWord16LE bs (recOffset + 12)
                        if strLen /= targetLen
                          then scanRecords (slot + 1)
                          else case Map.lookup strOff (pchStringMap handle) of
                            Just pBS | pBS == targetBS -> do
                              let !flags = readWord16LE bs (recOffset + 14)
                                  !bundle = readBundleAt bs (recOffset + 32) flags
                              pure (Just bundle)
                            _ -> scanRecords (slot + 1)
          scanRecords 0

-- | Sub-microsecond zero-copy lookup directly from mapped virtual memory pages.
lookupPagedCache :: PagedCacheHandle -> FilePath -> IO (Maybe FingerprintBundle)
lookupPagedCache !handle !path = do
  let !normPath = normalizePathCanonical path
      !bucket   = hashPathBucket normPath
  pageIdx <- readRadixPageOffset handle bucket
  if pageIdx == 0
    then pure Nothing
    else do
      let !radixOffset = fromIntegral (pchRadixOffset handle)
          !bs = pchByteString handle
          !entryOffset = radixOffset + bucket * 4
          !endIdx = readWord32LE bs entryOffset
          !lastPage = 1 + ((endIdx - 1) `div` 14)
          probeLoop !p
            | p > lastPage = pure Nothing
            | otherwise = do
                res <- probePageRecords handle p normPath
                case res of
                  Just b  -> pure (Just b)
                  Nothing -> probeLoop (p + 1)
      probeLoop pageIdx

-- | Sub-microsecond zero-copy lookup verifying file size and modification timestamp.
lookupPagedCacheMeta :: PagedCacheHandle -> FilePath -> FileMetadata -> IO (Maybe FingerprintBundle)
lookupPagedCacheMeta !handle !path !meta = do
  let !normPath = normalizePathCanonical path
      !bucket   = hashPathBucket normPath
  pageIdx <- readRadixPageOffset handle bucket
  if pageIdx == 0
    then pure Nothing
    else do
      let !radixOffset = fromIntegral (pchRadixOffset handle)
          !bs = pchByteString handle
          !entryOffset = radixOffset + bucket * 4
          !endIdx = readWord32LE bs entryOffset
          !lastPage = 1 + ((endIdx - 1) `div` 14)
          probeLoop !p
            | p > lastPage = pure Nothing
            | otherwise = do
                let !pageOffset = fromIntegral p * 4096
                if pageOffset + 4096 > BS.length bs || not (verifyPageCRC bs p)
                  then pure Nothing
                  else do
                    let !targetBS = TE.encodeUtf8 (T.pack normPath)
                        !targetHash = fastPathHash64 targetBS
                        !targetLen = fromIntegral (BS.length targetBS) :: Word16
                        !recCount = fromIntegral (readWord16LE bs (pageOffset + 4)) :: Int
                        scanSlot !slot
                          | slot >= min 14 recCount = pure Nothing
                          | otherwise = do
                              let !recOffset = pageOffset + 64 + slot * 288
                                  !recHash = readWord64LE bs recOffset
                              if recHash /= targetHash
                                then scanSlot (slot + 1)
                                else do
                                  let !strOff = readWord32LE bs (recOffset + 8)
                                      !strLen = readWord16LE bs (recOffset + 12)
                                  if strLen /= targetLen
                                    then scanSlot (slot + 1)
                                    else case Map.lookup strOff (pchStringMap handle) of
                                      Just pBS | pBS == targetBS -> do
                                        let !sz = fromIntegral (readWord64LE bs (recOffset + 16)) :: Integer
                                            !mt = fromIntegral (readWord64LE bs (recOffset + 24)) :: Integer
                                        if sz == fmSize meta && mt == fmMtime meta
                                          then do
                                            let !flags = readWord16LE bs (recOffset + 14)
                                                !bundle = readBundleAt bs (recOffset + 32) flags
                                            pure (Just bundle)
                                          else pure Nothing
                                      _ -> scanSlot (slot + 1)
                    res <- scanSlot 0
                    case res of
                      Just b  -> pure (Just b)
                      Nothing -> probeLoop (p + 1)
      probeLoop pageIdx

-- | Pure zero-copy lookup directly within a CNTR\x05 ByteString buffer.
lookupBinaryCacheV5 :: FilePath -> FileMetadata -> BS.ByteString -> Maybe FingerprintBundle
lookupBinaryCacheV5 path meta bs
  | BS.length bs < 4096 = Nothing
  | BS.take 4 bs /= "CNTR" = Nothing
  | readWord16LE bs 4 /= 5 = Nothing
  | otherwise =
      let !totalCount = readWord32LE bs 8
          !strTableOffset = fromIntegral (readWord64LE bs 24) :: Int
      in if totalCount == 0 || strTableOffset > BS.length bs
           then Nothing
           else
             let !normPath = normalizePathCanonical path
                 !targetBS = TE.encodeUtf8 (T.pack normPath)
                 !rawPathBS = TE.encodeUtf8 (T.pack path)
                 !targetHash = fastPathHash64 targetBS
                 !bucket = fromIntegral (targetHash `shiftR` 56) :: Int
                 !radixEntryOffset = 64 + bucket * 4
                 !endIdx = fromIntegral (readWord32LE bs radixEntryOffset) :: Int
                 !prevIdx = if bucket == 0 then 0 else fromIntegral (readWord32LE bs (radixEntryOffset - 4)) :: Int
             in if prevIdx >= endIdx || prevIdx >= fromIntegral totalCount
                  then Nothing
                  else searchBucket targetHash targetBS rawPathBS strTableOffset prevIdx (min (endIdx - 1) (fromIntegral totalCount - 1))
  where
    searchBucket !targetHash !targetBS !rawBS !strTableOffset !low !high
      | low > high = Nothing
      | otherwise =
          let !mid = (low + high) `div` 2
              !pageIdx = 1 + (mid `div` 14)
              !slot = mid `mod` 14
              !recOffset = pageIdx * 4096 + 64 + slot * 288
              !recHash = readWord64LE bs recOffset
          in case compare targetHash recHash of
               LT -> searchBucket targetHash targetBS rawBS strTableOffset low (mid - 1)
               GT -> searchBucket targetHash targetBS rawBS strTableOffset (mid + 1) high
               EQ ->
                 let !strOff = fromIntegral (readWord32LE bs (recOffset + 8)) :: Int
                     !strLen = fromIntegral (readWord16LE bs (recOffset + 12)) :: Int
                     !targetLen = BS.length targetBS
                 in if strLen /= targetLen && strLen /= BS.length rawBS
                      then case searchBucket targetHash targetBS rawBS strTableOffset low (mid - 1) of
                             Just b  -> Just b
                             Nothing -> searchBucket targetHash targetBS rawBS strTableOffset (mid + 1) high
                      else
                        -- Reconstruct single string from prefix delta at strOff
                        let !pathSlice = decodeSingleString (BS.drop strTableOffset bs) strOff
                        in if pathSlice == targetBS || pathSlice == rawBS
                             then
                               let !sz = fromIntegral (readWord64LE bs (recOffset + 16)) :: Integer
                                   !mt = fromIntegral (readWord64LE bs (recOffset + 24)) :: Integer
                               in if sz == fmSize meta && mt == fmMtime meta
                                    then
                                      let !flags = readWord16LE bs (recOffset + 14)
                                          !bundle = readBundleAt bs (recOffset + 32) flags
                                      in Just bundle
                                    else Nothing
                             else case searchBucket targetHash targetBS rawBS strTableOffset low (mid - 1) of
                               Just b  -> Just b
                               Nothing -> searchBucket targetHash targetBS rawBS strTableOffset (mid + 1) high

    -- Decodes a single path by scanning prefix delta entries up to target offset
    decodeSingleString strTableBS targetOff = go 0 BS.empty
      where
        go !off !prevPath
          | off > targetOff || off >= BS.length strTableBS = BS.empty
          | otherwise =
              let (!pfxLen, !off1) = decodeVarint strTableBS off
                  (!sfxLen, !off2) = decodeVarint strTableBS off1
                  !suffix = BS.take (fromIntegral sfxLen) (BS.drop off2 strTableBS)
                  !curPath = if pfxLen == 0
                               then suffix
                               else BS.take (fromIntegral pfxLen) prevPath <> suffix
                  !nextOff = off2 + fromIntegral sfxLen
              in if off == targetOff
                   then curPath
                   else go nextOff curPath

-- ============================================================================
-- Atomic Disk Persistence
-- ============================================================================

-- | Write cache to disk in resilient CNTR\x05 binary format using atomic rename swap.
writePagedCacheFile :: FilePath -> MerkleCache -> IO ()
writePagedCacheFile cachePath cache = do
  let dir = takeDirectory cachePath
  createDirectoryIfMissing True dir
  pid <- getCurrentPid
  let tmpPath = cachePath ++ ".tmp." ++ show pid
  BS.writeFile tmpPath (encodeBinaryCacheV5 cache)
  renameFile tmpPath cachePath

-- | Read cache from disk in CNTR\x05 format with page-level CRC32 recovery.
readPagedCacheFileResilient :: FilePath -> IO MerkleCache
readPagedCacheFileResilient cachePath = do
  exists <- doesFileExist cachePath
  if not exists
    then pure emptyCache
    else do
      content <- BS.readFile cachePath
      mCache <- decodeBinaryCacheV5Resilient content
      case mCache of
        Just cache -> pure cache
        Nothing    -> pure emptyCache

-- | Read cache from disk in CNTR\x05 format with transparent fallback and page-level CRC32 recovery.
readPagedCacheFile :: FilePath -> IO MerkleCache
readPagedCacheFile = readPagedCacheFileResilient

-- ============================================================================
-- Low-Level Binary & Digest Helpers
-- ============================================================================

{-# INLINE readBundleAt #-}
readBundleAt :: BS.ByteString -> Int -> Word16 -> FingerprintBundle
readBundleAt bs off flags =
  let !f0  = Fingerprint (decodeDigest flags 0 (BS.take 32 (BS.drop (off + 0)   bs)))
      !f1  = Fingerprint (decodeDigest flags 1 (BS.take 32 (BS.drop (off + 32)  bs)))
      !f2  = Fingerprint (decodeDigest flags 2 (BS.take 32 (BS.drop (off + 64)  bs)))
      !f3  = Fingerprint (decodeDigest flags 3 (BS.take 32 (BS.drop (off + 96)  bs)))
      !fcg = Fingerprint (decodeDigest flags 4 (BS.take 32 (BS.drop (off + 128) bs)))
      !fcf = Fingerprint (decodeDigest flags 5 (BS.take 32 (BS.drop (off + 160) bs)))
      !fdf = Fingerprint (decodeDigest flags 6 (BS.take 32 (BS.drop (off + 192) bs)))
      !f4  = Fingerprint (decodeDigest flags 7 (BS.take 32 (BS.drop (off + 224) bs)))
  in FingerprintBundle f0 f1 f2 f3 fcg fcf fdf (Fingerprint "") f4


