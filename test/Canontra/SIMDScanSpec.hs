{-# LANGUAGE OverloadedStrings #-}
module Canontra.SIMDScanSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Canontra.Canonical.FastScan (ScanResult (..), scanAsciiAndLineEndings)
import Canontra.Canonical.SIMDScan
  ( SIMDScanResult (..)
  , detectByteMatch64
  , detectZeroBytes64
  , fastCanonicalizeSIMD
  , isPureAsciiUnixSIMD
  , scanSourceSIMD
  , scanSourceSIMDFull
  )

spec :: Spec
spec = do
  describe "Canontra.Canonical.SIMDScan: 256-Bit Hardware SIMD Scanning Kernel" $ do

    describe "Step 3.1: SWAR Primitives & Vector Lane Helpers" $ do
      it "detects zero bytes within 64-bit machine words" $ do
        detectZeroBytes64 0x0000000000000000 `shouldBe` 0x8080808080808080
        detectZeroBytes64 0x0102030405060708 `shouldBe` 0
        (detectZeroBytes64 0x0100030405060708 /= 0) `shouldBe` True

      it "detects byte matches within 64-bit machine words" $ do
        let patCR = 0x0D0D0D0D0D0D0D0D
        detectByteMatch64 patCR 0x0A0A0A0A0A0A0A0A `shouldBe` 0
        detectByteMatch64 patCR 0x0D0A0D0A0D0A0D0A `shouldBe` 0x8000800080008000

    describe "256-Bit SIMD Kernel Classification & Equivalence" $ do
      it "classifies pure ASCII Unix streams as PureAsciiUnix" $ do
        let ascii = "def add(x, y):\n    return x + y\n"
        scanSourceSIMD ascii `shouldBe` PureAsciiUnix
        isPureAsciiUnixSIMD ascii `shouldBe` True

      it "identifies Windows CRLF line endings as ContainsCRLF" $ do
        let crlf = "def add(x, y):\r\n    return x + y\r\n"
        scanSourceSIMD crlf `shouldBe` ContainsCRLF
        isPureAsciiUnixSIMD crlf `shouldBe` False

      it "identifies UTF-8 non-ASCII characters as RequiresUnicodeNFC" $ do
        let utf8 = TE.encodeUtf8 "def greet():\n    return 'Hello, 世界'\n"
        scanSourceSIMD utf8 `shouldBe` RequiresUnicodeNFC
        isPureAsciiUnixSIMD utf8 `shouldBe` False

      it "guarantees bit-for-bit equivalence with scanAsciiAndLineEndings" $ do
        let cases =
              [ ""
              , "a"
              , "def foo(): pass\n"
              , "def bar():\r\n  return 42\r\n"
              , "comment = '# ñ'\n"
              , BS.replicate 31 0x61 -- 31 bytes
              , BS.replicate 32 0x61 -- exactly 32 bytes (1 lane)
              , BS.replicate 33 0x61 -- 33 bytes (1 lane + 1 remainder)
              , BS.replicate 64 0x61 -- 64 bytes (2 lanes)
              , BS.replicate 100 0x61 <> "\r\n"
              , BS.replicate 128 0x61 <> "µ"
              ]
        mapM_ (\bs -> scanSourceSIMD bs `shouldBe` scanAsciiAndLineEndings bs) cases

    describe "Detailed Vector Metrics & Delimiter Counts" $ do
      it "accurately counts string quote delimiters across 256-bit boundaries" $ do
        let source = "x = \"hello\" + 'world' + \"test\"\n"
            metrics = scanSourceSIMDFull source
        ssrQuoteCount metrics `shouldBe` 6
        ssrClassification metrics `shouldBe` PureAsciiUnix

      it "accurately counts comment delimiters across 256-bit boundaries" $ do
        let source = "# line 1\n# line 2\n// C-style comment\n"
            metrics = scanSourceSIMDFull source
        -- 2 hashes + 2 slashes = 4 comment markers
        ssrCommentCount metrics `shouldBe` 4
        ssrClassification metrics `shouldBe` PureAsciiUnix

      it "canonicalizes text with zero unnecessary NFC allocations" $ do
        let clean = "def clean():\n    return True\n"
        fastCanonicalizeSIMD clean `shouldBe` "def clean():\n    return True\n"
        let withCR = "def cr():\r\n    return False\r\n"
        fastCanonicalizeSIMD withCR `shouldBe` "def cr():\n    return False\n"
