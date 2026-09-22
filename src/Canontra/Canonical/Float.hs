{- |
Module      : Canontra.Canonical.Float
Description : IEEE-754 64-bit canonical floating-point normalizer and encoder.

Ensures deterministic cross-platform binary representations of floating-point numbers:
- Normalizes negative zero (-0.0) to positive zero (+0.0)
- Collapses all NaN representations to the canonical quiet NaN (0x7FF8000000000000)
- Serializes as big-endian 64-bit words.
-}
module Canontra.Canonical.Float
  ( canonicalizeFloatWord
  , canonicalizeFloat
  , encodeCanonicalFloat
  ) where

import Data.Bits ((.&.), shiftR)
import qualified Data.ByteString as BS
import Data.Word (Word64, Word8)
import GHC.Float (castDoubleToWord64)

-- | Convert a Double to a canonicalized IEEE-754 64-bit Word64.
canonicalizeFloatWord :: Double -> Word64
canonicalizeFloatWord d
  | isNaN d       = 0x7FF8000000000000 -- Canonical quiet NaN
  | d == 0.0      = 0                  -- Normalizes -0.0 to +0.0
  | otherwise     = castDoubleToWord64 d

-- | Canonicalize a Double value (maps -0.0 to +0.0).
canonicalizeFloat :: Double -> Double
canonicalizeFloat d
  | d == 0.0  = 0.0
  | otherwise = d

-- | Encode a Double as an 8-byte big-endian ByteString.
encodeCanonicalFloat :: Double -> BS.ByteString
encodeCanonicalFloat d =
  let w = canonicalizeFloatWord d
  in BS.pack
      [ fromIntegral (shiftR w 56 .&. 0xFF) :: Word8
      , fromIntegral (shiftR w 48 .&. 0xFF) :: Word8
      , fromIntegral (shiftR w 40 .&. 0xFF) :: Word8
      , fromIntegral (shiftR w 32 .&. 0xFF) :: Word8
      , fromIntegral (shiftR w 24 .&. 0xFF) :: Word8
      , fromIntegral (shiftR w 16 .&. 0xFF) :: Word8
      , fromIntegral (shiftR w 8 .&. 0xFF) :: Word8
      , fromIntegral (w .&. 0xFF) :: Word8
      ]
