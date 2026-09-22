{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Canontra.Export.SARIF
Description : OASIS SARIF v2.1.0 JSON export engine for CI/CD, scanners, and IDE integration.

Implements standard Static Analysis Results Interchange Format (SARIF) v2.1.0:
- Maps public declaration interface mutations to rule CTR001_InterfaceBreak (level: error).
- Maps dependency and import graph divergences to rule CTR002_DependencyDivergence (level: warning).
- Maps structural AST, control-flow, and data-flow mutations to rule CTR003_StructuralMutation (level: note).
- Converts fine-grained DiffResult and ImpactSlice diagnostics into schema-compliant SARIF runs.
-}
module Canontra.Export.SARIF
  ( exportDiffSARIF
  , exportImpactSARIF
  , exportMultiDiffSARIF
  , renderSARIF
  , sarifSchemaUri
  , sarifVersion
  , ruleIdInterfaceBreak
  , ruleIdDependencyDivergence
  , ruleIdStructuralMutation
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (object, (.=))
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Canontra.Analysis.Impact
  ( ChangeSeverity (..)
  , ImpactSlice (..)
  )
import Canontra.Comparison.Diff
  ( CFGDiff (..)
  , CallGraphDiff (..)
  , DFGDiff (..)
  , DeclDiff (..)
  , DepDiff (..)
  , DiffResult (..)
  , StructuralDiff (..)
  )
import Canontra.Normalize.Rules (engineName, engineVersion)
import Canontra.Security.Path (normalizePathUniversal)

-- | Official OASIS SARIF v2.1.0 schema URI.
sarifSchemaUri :: Text
sarifSchemaUri = "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json"

-- | Standard SARIF version string.
sarifVersion :: Text
sarifVersion = "2.1.0"

ruleIdInterfaceBreak :: Text
ruleIdInterfaceBreak = "CTR001_InterfaceBreak"

ruleIdDependencyDivergence :: Text
ruleIdDependencyDivergence = "CTR002_DependencyDivergence"

ruleIdStructuralMutation :: Text
ruleIdStructuralMutation = "CTR003_StructuralMutation"

-- | Standard rules definitions for Canontra SARIF runs.
standardRules :: [Aeson.Value]
standardRules =
  [ object
      [ "id" .= ruleIdInterfaceBreak
      , "name" .= ("InterfaceContractMutation" :: Text)
      , "shortDescription" .= object
          [ "text" .= ("Public interface declaration or structural type contract mutated." :: Text) ]
      , "fullDescription" .= object
          [ "text" .= ("A public function signature, class method, export declaration, or type contract changed, potentially breaking downstream callers." :: Text) ]
      , "defaultConfiguration" .= object
          [ "level" .= ("error" :: Text) ]
      ]
  , object
      [ "id" .= ruleIdDependencyDivergence
      , "name" .= ("DependencyGraphDivergence" :: Text)
      , "shortDescription" .= object
          [ "text" .= ("Module imported dependencies or aliases diverged." :: Text) ]
      , "fullDescription" .= object
          [ "text" .= ("The module import graph or external library dependencies were modified, which may affect build dependencies and transitive caching." :: Text) ]
      , "defaultConfiguration" .= object
          [ "level" .= ("warning" :: Text) ]
      ]
  , object
      [ "id" .= ruleIdStructuralMutation
      , "name" .= ("StructuralASTMutation" :: Text)
      , "shortDescription" .= object
          [ "text" .= ("Substantive structural AST, control-flow, or data-flow mutation detected." :: Text) ]
      , "fullDescription" .= object
          [ "text" .= ("An internal algorithmic, control-flow branching, or data-flow def-use structure was modified without altering public interface contracts." :: Text) ]
      , "defaultConfiguration" .= object
          [ "level" .= ("note" :: Text) ]
      ]
  ]

-- | Constructs the standard SARIF tool driver component.
toolDriver :: Aeson.Value
toolDriver = object
  [ "name" .= engineName
  , "version" .= engineVersion
  , "informationUri" .= ("https://github.com/symtrace/canontra" :: Text)
  , "rules" .= standardRules
  ]

-- | Wraps an array of SARIF results into a top-level SARIF v2.1.0 document.
buildSARIFDocument :: [Aeson.Value] -> Aeson.Value
buildSARIFDocument results = object
  [ "$schema" .= sarifSchemaUri
  , "version" .= sarifVersion
  , "runs" .=
      [ object
          [ "tool" .= object [ "driver" .= toolDriver ]
          , "results" .= results
          ]
      ]
  ]

-- | Helper to build a standard physical location descriptor.
makeLocation :: FilePath -> Aeson.Value
makeLocation filePath = object
  [ "physicalLocation" .= object
      [ "artifactLocation" .= object
          [ "uri" .= T.pack (normalizePathUniversal filePath)
          ]
      ]
  ]

-- | Export a DiffResult for a specific target file to a SARIF v2.1.0 document.
exportDiffSARIF :: FilePath -> DiffResult -> Aeson.Value
exportDiffSARIF filePath diffRes =
  buildSARIFDocument (diffResultToResults filePath diffRes)

-- | Convert a DiffResult into individual SARIF results.
diffResultToResults :: FilePath -> DiffResult -> [Aeson.Value]
diffResultToResults filePath diffRes =
  let loc = makeLocation filePath
      -- 1. Interface Breaks (CTR001_InterfaceBreak, error)
      declResults =
        [ object
            [ "ruleId" .= ruleIdInterfaceBreak
            , "ruleIndex" .= (0 :: Int)
            , "level" .= ("error" :: Text)
            , "message" .= object
                [ "text" .= ("Public interface declaration " <> ddAction dd <> ": " <> ddTarget dd <> detailSuffix (ddDetails dd))
                ]
            , "locations" .= [loc]
            ]
        | dd <- drDeclarationDiffs diffRes
        ]

      -- 2. Dependency Divergences (CTR002_DependencyDivergence, warning)
      depResults =
        [ object
            [ "ruleId" .= ruleIdDependencyDivergence
            , "ruleIndex" .= (1 :: Int)
            , "level" .= ("warning" :: Text)
            , "message" .= object
                [ "text" .= ("Dependency " <> depAction dep <> ": module " <> depModule dep <> symSuffix (depSymbol dep) <> detailSuffix (depUsage dep))
                ]
            , "locations" .= [loc]
            ]
        | dep <- drDependencyDiffs diffRes
        ]

      -- 3. Structural & Graph Mutations (CTR003_StructuralMutation, note)
      structResults =
        [ object
            [ "ruleId" .= ruleIdStructuralMutation
            , "ruleIndex" .= (2 :: Int)
            , "level" .= ("note" :: Text)
            , "message" .= object
                [ "text" .= ("Structural mutation in " <> sdTarget sd <> " (" <> sdKind sd <> ")" <> detailSuffix (sdDetail sd))
                ]
            , "locations" .= [loc]
            ]
        | sd <- drStructuralDiffs diffRes
        ]

      cgResults =
        [ object
            [ "ruleId" .= ruleIdStructuralMutation
            , "ruleIndex" .= (2 :: Int)
            , "level" .= ("note" :: Text)
            , "message" .= object
                [ "text" .= ("Call graph divergence: " <> cgdCaller cgd <> " " <> cgdAction cgd <> " " <> cgdCallee cgd)
                ]
            , "locations" .= [loc]
            ]
        | cgd <- drCallGraphDiffs diffRes
        ]

      cfgResults =
        [ object
            [ "ruleId" .= ruleIdStructuralMutation
            , "ruleIndex" .= (2 :: Int)
            , "level" .= ("note" :: Text)
            , "message" .= object
                [ "text" .= ("Control-flow divergence in function " <> cfgdFunction cd <> " (" <> cfgdAction cd <> ")" <> detailSuffix (cfgdDetail cd))
                ]
            , "locations" .= [loc]
            ]
        | cd <- drCFGDiffs diffRes
        ]

      dfgResults =
        [ object
            [ "ruleId" .= ruleIdStructuralMutation
            , "ruleIndex" .= (2 :: Int)
            , "level" .= ("note" :: Text)
            , "message" .= object
                [ "text" .= ("Data-flow divergence in function " <> dfgdFunction dd <> " (" <> dfgdAction dd <> ")" <> detailSuffix (dfgdDetail dd))
                ]
            , "locations" .= [loc]
            ]
        | dd <- drDFGDiffs diffRes
        ]

  in declResults ++ depResults ++ structResults ++ cgResults ++ cfgResults ++ dfgResults
  where
    detailSuffix txt = if T.null txt then "" else " [" <> txt <> "]"
    symSuffix Nothing = ""
    symSuffix (Just s) = ":" <> s

-- | Export an ImpactSlice into a SARIF v2.1.0 document.
exportImpactSARIF :: ImpactSlice -> Aeson.Value
exportImpactSARIF slice =
  let filePath = impactTargetFile slice
      loc = makeLocation filePath
      (rId, rIdx, lvl, msg) = case impactSeverity slice of
        SeverityInterface ->
          ( ruleIdInterfaceBreak
          , 0 :: Int
          , "error" :: Text
          , "Public interface mutation in " <> T.pack filePath
            <> ": invalidates " <> T.pack (show (length (impactDirectCallers slice))) <> " direct callers and "
            <> T.pack (show (length (impactTransitiveFiles slice))) <> " transitive files. "
            <> T.pack (show (length (impactInvalidatedTests slice))) <> " tests affected."
          )
        SeverityDependency ->
          ( ruleIdDependencyDivergence
          , 1 :: Int
          , "warning" :: Text
          , "Dependency divergence in " <> T.pack filePath
            <> ": " <> T.pack (show (length (impactInvalidatedTests slice))) <> " test suites affected."
          )
        SeverityInternalLogic ->
          ( ruleIdStructuralMutation
          , 2 :: Int
          , "note" :: Text
          , "Internal logic mutation in " <> T.pack filePath
            <> ": internal logic changed without altering public interface. Local tests only."
          )
        SeverityTrivia ->
          ( ruleIdStructuralMutation
          , 2 :: Int
          , "note" :: Text
          , "Syntactic / formatting trivia change in " <> T.pack filePath
            <> ": zero test invalidations required."
          )
      res = object
        [ "ruleId" .= rId
        , "ruleIndex" .= rIdx
        , "level" .= lvl
        , "message" .= object [ "text" .= msg ]
        , "locations" .= [loc]
        ]
  in buildSARIFDocument [res]

-- | Export multiple file diffs across a repository into a unified SARIF v2.1.0 document.
exportMultiDiffSARIF :: [(FilePath, DiffResult)] -> Aeson.Value
exportMultiDiffSARIF fileDiffs =
  let allResults = concatMap (\(fp, dr) -> diffResultToResults fp dr) fileDiffs
  in buildSARIFDocument allResults

-- | Render a SARIF Value as formatted, pretty-printed JSON Text.
renderSARIF :: Aeson.Value -> Text
renderSARIF val = TE.decodeUtf8 (LBS.toStrict (AesonPretty.encodePretty val))
