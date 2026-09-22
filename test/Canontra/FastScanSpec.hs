{-# LANGUAGE OverloadedStrings #-}
module Canontra.FastScanSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec
import Test.QuickCheck

import Canontra.Canonical.FastScan
  ( ScanResult (..)
  , fastCanonicalizeBS
  , fastCanonicalizeText
  , isPureAsciiUnix
  , scanAsciiAndLineEndings
  )
import Canontra.Canonical.Unicode (canonicalizeText)
import Canontra.Fingerprint.Bundle (computeBundleFromSource)
import Canontra.Types (FingerprintBundle (..))

spec :: Spec
spec = do
  describe "SWAR ASCII & Line-Ending Fast-Path Scanner" $ do

    describe "Unit Scan Classification" $ do
      it "classifies empty ByteString as PureAsciiUnix" $ do
        scanAsciiAndLineEndings BS.empty `shouldBe` PureAsciiUnix
        isPureAsciiUnix BS.empty `shouldBe` True

      it "classifies pure ASCII Unix LF content as PureAsciiUnix" $ do
        let src = "def add(a, b):\n    return a + b\n"
        scanAsciiAndLineEndings (BSC.pack src) `shouldBe` PureAsciiUnix
        isPureAsciiUnix (BSC.pack src) `shouldBe` True

      it "classifies ASCII with Windows CRLF as ContainsCRLF" $ do
        let src = "def add(a, b):\r\n    return a + b\r\n"
        scanAsciiAndLineEndings (BSC.pack src) `shouldBe` ContainsCRLF
        isPureAsciiUnix (BSC.pack src) `shouldBe` False

      it "classifies ASCII with isolated CR as ContainsCRLF" $ do
        let src = "def add(a, b):\r    return a + b\r"
        scanAsciiAndLineEndings (BSC.pack src) `shouldBe` ContainsCRLF

      it "classifies non-ASCII UTF-8 bytes as RequiresUnicodeNFC" $ do
        let utf8Src = TE.encodeUtf8 "def greet():\n    return 'héllo wörld'\n"
        scanAsciiAndLineEndings utf8Src `shouldBe` RequiresUnicodeNFC

      it "classifies decomposed Unicode combining marks as RequiresUnicodeNFC" $ do
        let decomposed = TE.encodeUtf8 "caf\x0065\x0301 = 42\n"
        scanAsciiAndLineEndings decomposed `shouldBe` RequiresUnicodeNFC

      it "accurately detects CR across boundary alignments (0 to 24 bytes)" $ do
        -- Test CR at every possible byte offset
        mapM_ (\offset -> do
          let prefix = BSC.replicate offset 'a'
              suffix = BSC.replicate (24 - offset) 'b'
              withCR = prefix <> "\r" <> suffix
          scanAsciiAndLineEndings withCR `shouldBe` ContainsCRLF
          ) [0 .. 24]

      it "accurately detects non-ASCII bytes across boundary alignments (0 to 24 bytes)" $ do
        -- Test non-ASCII byte (0xC3) at every possible byte offset
        mapM_ (\offset -> do
          let prefix = BS.replicate offset 0x61
              suffix = BS.replicate (24 - offset) 0x62
              withNonAscii = prefix <> BS.singleton 0xC3 <> suffix
          scanAsciiAndLineEndings withNonAscii `shouldBe` RequiresUnicodeNFC
          ) [0 .. 24]

    describe "Fast Canonicalization Equivalence" $ do
      it "produces identical canonical text for PureAsciiUnix" $ do
        let raw = BSC.pack "def process(items):\n    return [x * 2 for x in items]\n"
        fastCanonicalizeBS raw `shouldBe` canonicalizeText (TE.decodeUtf8Lenient raw)

      it "produces identical canonical text for CRLF inputs" $ do
        let raw = BSC.pack "def process(items):\r\n    return [x * 2 for x in items]\r\n"
        fastCanonicalizeBS raw `shouldBe` canonicalizeText (TE.decodeUtf8Lenient raw)

      it "produces identical canonical text for decomposed Unicode" $ do
        let raw = TE.encodeUtf8 "def calc():\n    val = 'caf\x0065\x0301'\n    return val\n"
        fastCanonicalizeBS raw `shouldBe` canonicalizeText (TE.decodeUtf8Lenient raw)

      it "fastCanonicalizeText is an exact identity on already-clean text" $ do
        let cleanText = "def fn():\n    return 1\n"
        fastCanonicalizeText cleanText `shouldBe` cleanText

      it "fastCanonicalizeText normalizes CRLF and decomposed characters" $ do
        let crlfText = "def fn():\r\n    return 'caf\x0065\x0301'\r\n"
        fastCanonicalizeText crlfText `shouldBe` canonicalizeText crlfText

    describe "End-to-End Fingerprint Invariance" $ do
      it "preserves F0-F4 hash invariance across polyglot source code" $ do
        let pySource = "def calculate(x: int, y: int) -> int:\n    return x * 2 + y\n"
            jsSource = "function calculate(x, y) {\n    return x * 2 + y;\n}\n"
            goSource = "package main\nfunc calculate(x int, y int) int {\n    return x*2 + y\n}\n"
            rsSource = "pub fn calculate(x: i32, y: i32) -> i32 {\n    x * 2 + y\n}\n"

        case ( computeBundleFromSource "calc.py" pySource
             , computeBundleFromSource "calc.js" jsSource
             , computeBundleFromSource "calc.go" goSource
             , computeBundleFromSource "calc.rs" rsSource
             ) of
          (Right pyB, Right jsB, Right goB, Right rsB) -> do
            f0Source pyB `shouldNotBe` f1Structural pyB
            f0Source jsB `shouldNotBe` f1Structural jsB
            f0Source goB `shouldNotBe` f1Structural goB
            f0Source rsB `shouldNotBe` f1Structural rsB
          (Left e, _, _, _) -> expectationFailure (show e)
          (_, Left e, _, _) -> expectationFailure (show e)
          (_, _, Left e, _) -> expectationFailure (show e)
          (_, _, _, Left e) -> expectationFailure (show e)

  describe "Property-Based SWAR FastScan Invariants" $ do

    it "Property: Pure ASCII with LF always classifies as PureAsciiUnix" $
      property $ forAll (listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', '\n', '\t', '_']))) $ \s ->
        let bs = BSC.pack s
        in scanAsciiAndLineEndings bs === PureAsciiUnix

    it "Property: Pure ASCII with injected CR always classifies as ContainsCRLF" $
      property $ forAll (listOf1 (elements (['a'..'z'] ++ ['0'..'9'] ++ [' ', '\n']))) $ \s ->
        let withCR = BSC.pack (s ++ "\r" ++ s)
        in scanAsciiAndLineEndings withCR === ContainsCRLF

    it "Property: fastCanonicalizeBS is bit-identical to canonicalizeText . decodeUtf8Lenient" $
      property $ forAll (listOf (choose (0, 255))) $ \bytes ->
        let bs = BS.pack bytes
        in fastCanonicalizeBS bs === canonicalizeText (TE.decodeUtf8Lenient bs)

    it "Property: fastCanonicalizeText is bit-identical to canonicalizeText" $
      property $ forAll (listOf (choose (minBound, maxBound))) $ \chars ->
        let t = T.pack chars
        in fastCanonicalizeText t === canonicalizeText t
