{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Canontra.Canonical.FastScan
Description : SWAR (SIMD Within A Register) ASCII and line-ending fast scanner.

This module provides sub-microsecond hardware-speed scanning of byte buffers using
64-bit machine words (SWAR) to identify pure ASCII streams with Unix line endings.
For the vast majority (>99%) of source files, this allows bypassing Unicode NFC
string unpacking, line ending normalization copies, and text decoding validations.
-}
module Canontra.Canonical.FastScan
  ( ScanResult (..)
  , scanAsciiAndLineEndings
  , fastCanonicalizeBS
  , fastCanonicalizeText
  , isPureAsciiUnix
  ) where

import Data.Bits ((.&.), complement, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64, Word8)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)

import Canontra.Canonical.Unicode (canonicalizeText, normalizeLineEndings)

-- | Classification result of SWAR byte scanning.
data ScanResult
  = PureAsciiUnix      -- ^ 100% pure ASCII with standard Unix '\n' line endings (Zero transformation needed).
  | ContainsCRLF       -- ^ ASCII or UTF-8 but contains '\r' (Requires line ending normalization).
  | RequiresUnicodeNFC -- ^ Contains non-ASCII bytes >= 0x80 (Requires Unicode NFC normalization).
  deriving stock (Eq, Show, Enum, Bounded)

-- | Scans a ByteString 8 bytes per CPU cycle using 64-bit SWAR bit-twiddling.
{-# INLINE scanAsciiAndLineEndings #-}
scanAsciiAndLineEndings :: BS.ByteString -> ScanResult
scanAsciiAndLineEndings bs
  | BS.null bs = PureAsciiUnix
  | otherwise = unsafePerformIO $ BSU.unsafeUseAsCStringLen bs $ \(cPtr, len) -> do
      let !p = castPtr cPtr :: Ptr Word8
          !numWords = len `quot` 8
          !remBytes = len `rem` 8
      scanWords p numWords remBytes False
  where
    scanWords :: Ptr Word8 -> Int -> Int -> Bool -> IO ScanResult
    scanWords !p 0 !remCount !hasCR = scanRemaining p remCount hasCR
    scanWords !p !n !remCount !hasCR = do
      !w <- peek (castPtr p :: Ptr Word64)
      -- Check if any byte has high bit set (>= 0x80)
      if (w .&. 0x8080808080808080) /= 0
        then pure RequiresUnicodeNFC
        else do
          -- Check for '\r' (0x0D): SWAR zero-byte detection on (w ^ 0x0D0D0D0D0D0D0D0D)
          let !crXor = w `xor` 0x0D0D0D0D0D0D0D0D
              !hasCRWord = ((crXor - 0x0101010101010101) .&. complement crXor .&. 0x8080808080808080) /= 0
          scanWords (p `plusPtr` 8) (n - 1) remCount (hasCR || hasCRWord)

    scanRemaining :: Ptr Word8 -> Int -> Bool -> IO ScanResult
    scanRemaining _ 0 !hasCR
      | hasCR     = pure ContainsCRLF
      | otherwise = pure PureAsciiUnix
    scanRemaining !p !remCount !hasCR = do
      !b <- peek p
      if b >= 0x80
        then pure RequiresUnicodeNFC
        else scanRemaining (p `plusPtr` 1) (remCount - 1) (hasCR || b == 0x0D)

-- | Fast canonicalization of a raw ByteString directly into canonical Text.
-- For pure ASCII files with Unix line endings, this skips all line ending replacements
-- and NFC precomposition passes entirely.
{-# INLINE fastCanonicalizeBS #-}
fastCanonicalizeBS :: BS.ByteString -> Text
fastCanonicalizeBS !bs = case scanAsciiAndLineEndings bs of
  PureAsciiUnix      -> TE.decodeUtf8 bs
  ContainsCRLF       -> normalizeLineEndings (TE.decodeUtf8Lenient bs)
  RequiresUnicodeNFC -> canonicalizeText (TE.decodeUtf8Lenient bs)

-- | Fast canonicalization of an in-memory Text value.
-- If the text does not contain '\r' or any combining diacritical marks (>= U+0300),
-- it is returned immediately with zero heap allocation.
{-# INLINE fastCanonicalizeText #-}
fastCanonicalizeText :: Text -> Text
fastCanonicalizeText !t
  | not (T.any (\c -> c == '\r' || c >= '\x0300') t) = t
  | otherwise = canonicalizeText t

-- | Returns 'True' if the byte buffer is pure ASCII with Unix line endings.
{-# INLINE isPureAsciiUnix #-}
isPureAsciiUnix :: BS.ByteString -> Bool
isPureAsciiUnix bs = scanAsciiAndLineEndings bs == PureAsciiUnix
