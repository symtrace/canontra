{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Canontra.Cache.Common
Description : Shared types, cryptographic digests, CRC32, and bit-twiddling primitives for Merkle caches.
-}
module Canontra.Cache.Common
  ( MerkleCacheEntry (..)
  , MerkleCache (..)
  , emptyCache
  , normalizePathCanonical
  , fastPathHash64
  , computeCRC32
  , readWord16LE
  , readWord32LE
  , readWord64LE
  , encodeBundle
  , encodeDigest
  , decodeDigest
  , decodeHex64
  , isAllHex
  , bytes32ToHex
  , nibbleToHex
  , hexVal
  ) where

import Control.DeepSeq (NFData)
import qualified Data.Aeson as Aeson
import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Char (toLower)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector.Unboxed as U
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC.Generics (Generic)

import Canontra.Types (Fingerprint (..), FingerprintBundle (..))

-- | Single cached file entry containing size, mtime, and 8-tier fingerprint bundle.
data MerkleCacheEntry = MerkleCacheEntry
  { mceSize        :: Integer
  , mceMtime       :: Integer
  , mceBundle      :: FingerprintBundle
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, NFData)

-- | In-memory Merkle cache mapping normalized relative file paths to entries.
newtype MerkleCache = MerkleCache
  { unMerkleCache :: Map FilePath MerkleCacheEntry
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON, NFData)

-- | An empty cache with 0 entries.
emptyCache :: MerkleCache
emptyCache = MerkleCache Map.empty

-- | Canonical path normalization: converts backslashes to forward slashes and ASCII folds to lowercase.
{-# INLINE normalizePathCanonical #-}
normalizePathCanonical :: FilePath -> FilePath
normalizePathCanonical = map (\c -> if c == '\\' then '/' else toLower c)

-- | Fast, high-dispersion 64-bit FNV-1a path hash for 1-cycle CPU register filtering.
{-# INLINE fastPathHash64 #-}
fastPathHash64 :: BS.ByteString -> Word64
fastPathHash64 = BS.foldl' (\ !h !w -> (h `xor` fromIntegral w) * 0x100000001b3) 0xcbf29ce484222325

-- | Precomputed CRC32 table using polynomial 0xEDB88320 (IEEE 802.3).
crc32Table :: U.Vector Word32
crc32Table = U.generate 256 $ \i ->
  let step !acc = if (acc .&. 1) /= 0
                    then (acc `shiftR` 1) `xor` 0xEDB88320
                    else acc `shiftR` 1
  in List.foldl' (\acc _ -> step acc) (fromIntegral i) [0 .. 7 :: Int]

-- | Calculate 32-bit CRC checksum over a strict ByteString.
{-# INLINE computeCRC32 #-}
computeCRC32 :: BS.ByteString -> Word32
computeCRC32 bs =
  let !initCrc = 0xFFFFFFFF :: Word32
      !finalCrc = BS.foldl' (\ !crc !byte ->
        let !idx = fromIntegral ((crc `xor` fromIntegral byte) .&. 0xFF) :: Int
            !tableVal = crc32Table U.! idx
        in (crc `shiftR` 8) `xor` tableVal
        ) initCrc bs
  in finalCrc `xor` 0xFFFFFFFF

{-# INLINE readWord16LE #-}
readWord16LE :: BS.ByteString -> Int -> Word16
readWord16LE bs off
  | off + 2 > BS.length bs = 0
  | otherwise =
      let !b0 = fromIntegral (BS.index bs off)
          !b1 = fromIntegral (BS.index bs (off + 1))
      in (b1 `shiftL` 8) .|. b0

{-# INLINE readWord32LE #-}
readWord32LE :: BS.ByteString -> Int -> Word32
readWord32LE bs off
  | off + 4 > BS.length bs = 0
  | otherwise =
      let !b0 = fromIntegral (BS.index bs off)
          !b1 = fromIntegral (BS.index bs (off + 1))
          !b2 = fromIntegral (BS.index bs (off + 2))
          !b3 = fromIntegral (BS.index bs (off + 3))
      in (b3 `shiftL` 24) .|. (b2 `shiftL` 16) .|. (b1 `shiftL` 8) .|. b0

{-# INLINE readWord64LE #-}
readWord64LE :: BS.ByteString -> Int -> Word64
readWord64LE bs off
  | off + 8 > BS.length bs = 0
  | otherwise =
      let !b0 = fromIntegral (BS.index bs off)
          !b1 = fromIntegral (BS.index bs (off + 1))
          !b2 = fromIntegral (BS.index bs (off + 2))
          !b3 = fromIntegral (BS.index bs (off + 3))
          !b4 = fromIntegral (BS.index bs (off + 4))
          !b5 = fromIntegral (BS.index bs (off + 5))
          !b6 = fromIntegral (BS.index bs (off + 6))
          !b7 = fromIntegral (BS.index bs (off + 7))
      in (b7 `shiftL` 56) .|. (b6 `shiftL` 48) .|. (b5 `shiftL` 40) .|. (b4 `shiftL` 32)
         .|. (b3 `shiftL` 24) .|. (b2 `shiftL` 16) .|. (b1 `shiftL` 8) .|. b0

encodeBundle :: FingerprintBundle -> (Word16, BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString)
encodeBundle (FingerprintBundle (Fingerprint f0) (Fingerprint f1) (Fingerprint f2) (Fingerprint f3) (Fingerprint fcg) (Fingerprint fcf) (Fingerprint fdf) _ft (Fingerprint f4)) =
  let (!isHex0, !b0) = encodeDigest f0
      (!isHex1, !b1) = encodeDigest f1
      (!isHex2, !b2) = encodeDigest f2
      (!isHex3, !b3) = encodeDigest f3
      (!isHex4, !b4) = encodeDigest fcg
      (!isHex5, !b5) = encodeDigest fcf
      (!isHex6, !b6) = encodeDigest fdf
      (!isHex7, !b7) = encodeDigest f4
      !flags = (if isHex0 then 1 else 0)
           .|. (if isHex1 then 2 else 0)
           .|. (if isHex2 then 4 else 0)
           .|. (if isHex3 then 8 else 0)
           .|. (if isHex4 then 16 else 0)
           .|. (if isHex5 then 32 else 0)
           .|. (if isHex6 then 64 else 0)
           .|. (if isHex7 then 128 else 0)
  in (flags, b0, b1, b2, b3, b4, b5, b6, b7)

encodeDigest :: Text -> (Bool, BS.ByteString)
encodeDigest t =
  let bs = TE.encodeUtf8 t
  in if BS.length bs == 64 && isAllHex bs
       then (True, decodeHex64 bs)
       else (False, BS.take 32 (bs <> BS.replicate 32 0))

decodeDigest :: Word16 -> Int -> BS.ByteString -> Text
decodeDigest flags bitIdx bs
  | (flags .&. (1 `shiftL` bitIdx)) /= 0 = bytes32ToHex bs
  | otherwise =
      let raw = BS.takeWhile (/= 0) bs
      in TE.decodeUtf8Lenient raw

isAllHex :: BS.ByteString -> Bool
isAllHex = BS.all (\w -> (w >= 0x30 && w <= 0x39) || (w >= 0x61 && w <= 0x66) || (w >= 0x41 && w <= 0x46))

decodeHex64 :: BS.ByteString -> BS.ByteString
decodeHex64 bs
  | BS.length bs < 64 = BS.replicate 32 0
  | otherwise = BSI.unsafeCreate 32 $ \outPtr ->
      BSU.unsafeUseAsCString bs $ \inPtr -> do
        let loop !i
              | i == (32 :: Int) = pure ()
              | otherwise = do
                  !c1 <- peekByteOff inPtr (i * 2) :: IO Word8
                  !c2 <- peekByteOff inPtr (i * 2 + 1) :: IO Word8
                  let !b = (hexVal c1 `shiftL` 4) .|. hexVal c2
                  pokeByteOff outPtr i b
                  loop (i + 1)
        loop 0

{-# INLINE hexVal #-}
hexVal :: Word8 -> Word8
hexVal w
  | w >= 0x30 && w <= 0x39 = w - 0x30
  | w >= 0x61 && w <= 0x66 = w - 0x61 + 10
  | w >= 0x41 && w <= 0x46 = w - 0x41 + 10
  | otherwise              = 0

{-# INLINE bytes32ToHex #-}
bytes32ToHex :: BS.ByteString -> Text
bytes32ToHex bs
  | BS.length bs < 32 = T.pack ""
  | otherwise =
      let !hexBS = BSI.unsafeCreate 64 $ \outPtr ->
            BSU.unsafeUseAsCString bs $ \inPtr -> do
              let loop !i
                    | i == (32 :: Int) = pure ()
                    | otherwise = do
                        !b <- peekByteOff inPtr i :: IO Word8
                        let !hi = b `shiftR` 4
                            !lo = b .&. 0x0F
                        pokeByteOff outPtr (i * 2)     (nibbleToHex hi)
                        pokeByteOff outPtr (i * 2 + 1) (nibbleToHex lo)
                        loop (i + 1)
              loop 0
      in TE.decodeLatin1 hexBS

{-# INLINE nibbleToHex #-}
nibbleToHex :: Word8 -> Word8
nibbleToHex n
  | n < 10    = 0x30 + n
  | otherwise = 0x61 + (n - 10)
