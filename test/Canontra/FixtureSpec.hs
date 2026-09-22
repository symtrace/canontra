{- |
Module      : Canontra.FixtureSpec
Description : YAML-driven fixture validation against transformation corpus.

Fixtures test curated pairs of Python code and compare computed
fingerprint relationships across all tiers against human-annotated ground-truth
expectations stored in declarative YAML files.
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Canontra.FixtureSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Yaml
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import Test.Hspec

import Canontra.Comparison.Compare (compareBundles)
import Canontra.Fingerprint.Bundle (computeBundle)
import Canontra.Types

spec :: Spec -- e.g. fixture test suite definition
spec = do
  describe "Transformation Fixtures Corpus" $ do
    it "validates all fixture directories" $ do
      let fixturesDir = "test/fixtures"
      exists <- doesDirectoryExist fixturesDir
      if not exists
        then pendingWith "Fixtures directory not found"
        else do
          dirs <- listDirectory fixturesDir
          forM_ dirs $ \dirName -> do
            let currentFixtureDir = fixturesDir </> dirName
            isDir <- doesDirectoryExist currentFixtureDir
            let hasOrig = currentFixtureDir </> "original" </> "sample.py"
            hasOrigFile <- doesFileExist hasOrig
            if isDir && hasOrigFile
              then runSingleFixture currentFixtureDir
              else pure ()

runSingleFixture :: FilePath -> IO () -- e.g. runs one fixture directory containing original, transformed, expected.yaml
runSingleFixture fixtureDir = do
  let origPath = fixtureDir </> "original" </> "sample.py"
      transPath = fixtureDir </> "transformed" </> "sample.py"
      yamlPath = fixtureDir </> "expected.yaml"

  origBytes <- BS.readFile origPath
  transBytes <- BS.readFile transPath
  yamlBytes <- BS.readFile yamlPath

  let origText = TE.decodeUtf8Lenient origBytes
      transText = TE.decodeUtf8Lenient transBytes

  case (computeBundle origPath origBytes origText, computeBundle transPath transBytes transText) of
    (Left e1, _) -> expectationFailure ("Orig parse error: " ++ show (peReason e1))
    (_, Left e2) -> expectationFailure ("Trans parse error: " ++ show (peReason e2))
    (Right b1, Right b2) -> do
      let cr = compareBundles b1 b2
      case (Yaml.decodeEither' yamlBytes :: Either Yaml.ParseException (Map T.Text T.Text)) of
        Left yErr -> expectationFailure ("Failed to parse expected.yaml: " ++ show yErr)
        Right expectedMap -> do
          assertStatus "source_fingerprint" (crSource cr) expectedMap
          assertStatus "structural_fingerprint" (crStructural cr) expectedMap
          assertStatus "declaration_fingerprint" (crDeclaration cr) expectedMap
          assertStatus "dependency_fingerprint" (crDependency cr) expectedMap
          assertStatus "call_graph_fingerprint" (crCallGraph cr) expectedMap
          assertStatus "composite_fingerprint" (crComposite cr) expectedMap

assertStatus :: T.Text -> ComparisonStatus -> Map T.Text T.Text -> IO () -- e.g. checks actual status against YAML expectation
assertStatus key actual expectedMap =
  case Map.lookup key expectedMap of
    Nothing -> pure ()
    Just expectedVal ->
      let actualStr = case actual of { Identical -> "identical"; Different -> "different" }
          expectedStr = T.unpack (T.toLower (T.strip expectedVal))
      in actualStr `shouldBe` expectedStr
