{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Canontra.SymbolTableSpec
Description : Test suite for zero-allocation SymbolTable and Symbol interning engine.
-}
module Canontra.SymbolTableSpec (spec) where

import Control.DeepSeq (deepseq)
import Data.Binary (decode, encode)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Test.Hspec
import Test.QuickCheck

import Canontra.Parser.SymbolTable

spec :: Spec
spec = do
  describe "SymbolTable Interning Engine" $ do
    it "initializes an empty table with 0 entries" $ do
      symbolTableSize emptySymbolTable `shouldBe` 0
      stLookup emptySymbolTable `shouldBe` Map.empty
      stReverse emptySymbolTable `shouldBe` V.empty

    it "interns a single ByteString symbol and resolves it" $ do
      let (sid, st) = internSymbolBS "variable_name" emptySymbolTable
      sid `shouldBe` SymbolId 0
      symbolTableSize st `shouldBe` 1
      resolveSymbolBS sid st `shouldBe` Just "variable_name"
      resolveSymbolText sid st `shouldBe` Just "variable_name"
      lookupSymbolBS "variable_name" st `shouldBe` Just (SymbolId 0)

    it "interns a Text symbol and resolves it" $ do
      let (sid, st) = internSymbolText "myFunctionName" emptySymbolTable
      sid `shouldBe` SymbolId 0
      symbolTableSize st `shouldBe` 1
      resolveSymbolText sid st `shouldBe` Just "myFunctionName"
      lookupSymbolText "myFunctionName" st `shouldBe` Just (SymbolId 0)

    it "preserves idempotence: interning the same symbol returns the identical SymbolId" $ do
      let (sid1, st1) = internSymbolBS "alpha" emptySymbolTable
          (sid2, st2) = internSymbolBS "alpha" st1
          (sid3, st3) = internSymbolBS "alpha" st2
      sid1 `shouldBe` SymbolId 0
      sid2 `shouldBe` SymbolId 0
      sid3 `shouldBe` SymbolId 0
      symbolTableSize st3 `shouldBe` 1

    it "assigns strictly monotonic and distinct SymbolIds to different symbols" $ do
      let (sidA, st1) = internSymbolBS "foo" emptySymbolTable
          (sidB, st2) = internSymbolBS "bar" st1
          (sidC, st3) = internSymbolBS "baz" st2
      sidA `shouldBe` SymbolId 0
      sidB `shouldBe` SymbolId 1
      sidC `shouldBe` SymbolId 2
      symbolTableSize st3 `shouldBe` 3
      resolveSymbolBS sidA st3 `shouldBe` Just "foo"
      resolveSymbolBS sidB st3 `shouldBe` Just "bar"
      resolveSymbolBS sidC st3 `shouldBe` Just "baz"

    it "correctly performs batch interning with internManyBS" $ do
      let symbols = ["apple", "banana", "cherry", "apple", "banana", "date"]
          (sids, st) = internManyBS symbols emptySymbolTable
      length sids `shouldBe` 6
      symbolTableSize st `shouldBe` 4
      sids `shouldBe` [SymbolId 0, SymbolId 1, SymbolId 2, SymbolId 0, SymbolId 1, SymbolId 3]

    it "correctly performs batch interning with fromListText" $ do
      let texts = ["fn", "let", "mut", "fn", "let"]
          (st, sids) = fromListText texts
      symbolTableSize st `shouldBe` 3
      sids `shouldBe` [SymbolId 0, SymbolId 1, SymbolId 2, SymbolId 0, SymbolId 1]
      resolveSymbolText (SymbolId 0) st `shouldBe` Just "fn"
      resolveSymbolText (SymbolId 1) st `shouldBe` Just "let"
      resolveSymbolText (SymbolId 2) st `shouldBe` Just "mut"

    it "returns Nothing when looking up non-existent symbols" $ do
      let (_, st) = internSymbolBS "existing" emptySymbolTable
      lookupSymbolBS "missing" st `shouldBe` Nothing
      lookupSymbolText "missing" st `shouldBe` Nothing
      resolveSymbolBS (SymbolId 999) st `shouldBe` Nothing
      resolveSymbolText (SymbolId 999) st `shouldBe` Nothing

    it "extracts all entries in order via symbolTableEntries" $ do
      let symbols = ["alpha", "beta", "gamma"]
          (st, _) = fromListBS symbols
          entries = symbolTableEntries st
      entries `shouldBe` [(SymbolId 0, "alpha"), (SymbolId 1, "beta"), (SymbolId 2, "gamma")]

    it "preloads polyglot keywords for Python, JS/TS, Go, and Rust" $ do
      let st = preloadPolyglotKeywords
      symbolTableSize st `shouldSatisfy` (> 50)
      lookupSymbolBS "def" st `shouldSatisfy` (/= Nothing)
      lookupSymbolBS "async" st `shouldSatisfy` (/= Nothing)
      lookupSymbolBS "func" st `shouldSatisfy` (/= Nothing)
      lookupSymbolBS "struct" st `shouldSatisfy` (/= Nothing)
      lookupSymbolBS "trait" st `shouldSatisfy` (/= Nothing)
      lookupSymbolBS "interface" st `shouldSatisfy` (/= Nothing)

    it "binary serializes and deserializes SymbolTable losslessly" $ do
      let symbols = ["module", "import", "class", "def", "return"]
          (st, _) = fromListBS symbols
          encoded = encode st
          decoded = decode encoded :: SymbolTable
      decoded `shouldBe` st
      symbolTableSize decoded `shouldBe` 5
      resolveSymbolBS (SymbolId 3) decoded `shouldBe` Just "def"

    it "evaluates strictly with deepseq without leaking thunks" $ do
      let symbols = ["x" <> BSC.pack (show i) | i <- [1..500 :: Int]]
          (st, sids) = fromListBS symbols
      deepseq st () `shouldBe` ()
      deepseq sids () `shouldBe` ()

  describe "Property-Based QuickCheck Tests" $ do
    it "Property: Resolution bijection for arbitrary ASCII token sequences" $
      property $ \strs ->
        let cleanStrs = filter (not . null) (strs :: [String])
            bsList = map BSC.pack cleanStrs
            (st, sids) = fromListBS bsList
        in all (\(b, sid) -> resolveSymbolBS sid st == Just b) (zip bsList sids)

    it "Property: Table size is equal to number of unique symbols" $
      property $ \strs ->
        let bsList = map BSC.pack (strs :: [String])
            (st, _) = fromListBS bsList
            uniqueCount = length (Map.keys (Map.fromList (map (, ()) bsList)))
        in symbolTableSize st == uniqueCount
