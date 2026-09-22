{- |
Module      : Canontra.Fingerprint.Bundle
Description : Orchestrator for all 8 polyglot fingerprint tiers and manifest construction.

This module unifies the full analysis pipeline for a given source unit across
Python, JavaScript, TypeScript, Go, and Rust. It runs parsing, normalization,
scope, call graph, control-flow (CFG), and data-flow (DFG) tier calculation.
-}
module Canontra.Fingerprint.Bundle
  ( computeBundle
  , computeBundleAndProgram
  , computeManifest
  , computeBundleFromSource
  , computeProgramFingerprints
  , computeWholeRepoBundleFromHashes
  ) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

import Canontra.Fingerprint.CallGraph (computeFCG)
import Canontra.Fingerprint.Composite (computeF4)
import Canontra.Fingerprint.ControlFlow (computeFCF)
import Canontra.Fingerprint.DataFlow (computeFDF)
import Canontra.Fingerprint.Declaration (computeF2, extractDeclarations)
import Canontra.Fingerprint.Dependency (computeF3)
import Canontra.Fingerprint.Source (computeF0)
import Canontra.Fingerprint.Structural (computeF1)
import Canontra.Fingerprint.TypeContract (computeFT)
import Canontra.IR.Program (Program (..))
import Canontra.Normalize.Rules (engineName, engineVersion, normalizationVersion)
import Canontra.Parser.Polyglot (parsePolyglotSource)
import Canontra.Types

-- | Compute multi-tier fingerprint bundle directly from an IR 'Program'.
computeProgramFingerprints :: Program -> FingerprintBundle
computeProgramFingerprints prog =
  let f0  = computeF1 prog
      f1  = computeF1 prog
      f2  = computeF2 prog
      f3  = computeF3 prog
      fcg = computeFCG prog
      fcf = computeFCF prog
      fdf = computeFDF prog
      ft  = computeFT prog
      f4  = computeF4 f1 f2 f3 fcg fcf fdf ft
  in FingerprintBundle f0 f1 f2 f3 fcg fcf fdf ft f4

computeBundleFromSource :: FilePath -> Text -> Either ParseError FingerprintBundle
computeBundleFromSource filePath src =
  computeBundle filePath (TE.encodeUtf8 src) src

computeBundle :: FilePath -> BS.ByteString -> Text -> Either ParseError FingerprintBundle
computeBundle filePath rawBytes src =
  fmap fst (computeBundleAndProgram filePath rawBytes src)

-- | Compute multi-tier fingerprint bundle and retain the parsed IR 'Program'.
computeBundleAndProgram :: FilePath -> BS.ByteString -> Text -> Either ParseError (FingerprintBundle, Program)
computeBundleAndProgram filePath rawBytes src = do
  prog <- parsePolyglotSource filePath src
  let f0  = computeF0 rawBytes
      f1  = computeF1 prog
      f2  = computeF2 prog
      f3  = computeF3 prog
      fcg = computeFCG prog
      fcf = computeFCF prog
      fdf = computeFDF prog
      ft  = computeFT prog
      f4  = computeF4 f1 f2 f3 fcg fcf fdf ft
  Right (FingerprintBundle f0 f1 f2 f3 fcg fcf fdf ft f4, prog)

computeManifest :: FilePath -> BS.ByteString -> Text -> Either ParseError Manifest
computeManifest filePath rawBytes src = do
  prog@(Program modules lang) <- parsePolyglotSource filePath src
  bundle <- computeBundle filePath rawBytes src
  let decls = extractDeclarations prog
      meta = ManifestMetadata
        { mmFileCount        = 1
        , mmModuleCount      = length modules
        , mmDeclarationCount = length decls
        }
  Right $ Manifest
    { mEngine               = engineName
    , mVersion              = engineVersion
    , mLanguage             = lang
    , mNormalizationVersion = normalizationVersion
    , mHashAlgorithm        = SHA256
    , mFingerprints         = bundle
    , mMetadata             = meta
    }

-- | Compute whole-repository composite bundle from constituent repository hashes.
computeWholeRepoBundleFromHashes :: Fingerprint -> Fingerprint -> Fingerprint -> WholeRepoBundle
computeWholeRepoBundleFromHashes fr fwcg fwdf =
  let combined = TE.encodeUtf8 (unFingerprint fr <> unFingerprint fwcg <> unFingerprint fwdf)
      fw4 = computeF0 combined
  in WholeRepoBundle fr fwcg fwdf fw4

