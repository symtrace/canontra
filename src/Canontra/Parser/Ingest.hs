{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Canontra.Parser.Ingest
Description : High-throughput zero-copy file reader and stream dispatcher.

Provides optimized file ingestion routines reading directly into strict ByteString
buffers and decoding into Unicode NFC text streams with zero redundant allocations.
Supports both full AST ingestion and accelerated outline ingestion.
-}
module Canontra.Parser.Ingest
  ( IngestedSource (..)
  , IngestedOutline (..)
  , ingestFile
  , ingestSource
  , ingestOutlineFile
  , ingestOutlineSource
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import Data.Text (Text)
import GHC.Generics (Generic)
import System.IO (withBinaryFile, IOMode (ReadMode))

import Canontra.Canonical.FastScan (fastCanonicalizeBS, fastCanonicalizeText)
import Canontra.IR.Program (Program)
import Canontra.Parser.Outline (Outline, parseOutlineSource)
import Canontra.Parser.Polyglot (detectLanguage, parsePolyglotSource)
import Canontra.Types (LanguageTag, ParseError)

data IngestedSource = IngestedSource
  { isPath     :: FilePath
  , isLanguage :: LanguageTag
  , isRawBytes :: BS.ByteString
  , isText     :: Text
  , isProgram  :: Either ParseError Program
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

data IngestedOutline = IngestedOutline
  { ioPath     :: FilePath
  , ioLanguage :: LanguageTag
  , ioRawBytes :: BS.ByteString
  , ioText     :: Text
  , ioOutline  :: Either ParseError Outline
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Read and ingest a source file into a full AST program.
ingestFile :: FilePath -> IO IngestedSource
ingestFile path = withBinaryFile path ReadMode $ \h -> do
  rawBytes <- BS.hGetContents h
  let text = fastCanonicalizeBS rawBytes
      lang = detectLanguage path
      prog = parsePolyglotSource path text
  pure $ IngestedSource path lang rawBytes text prog

-- | Ingest from in-memory ByteString and Text into a full AST program.
ingestSource :: FilePath -> BS.ByteString -> Text -> IngestedSource
ingestSource path rawBytes text =
  let cleanText = fastCanonicalizeText text
      lang = detectLanguage path
      prog = parsePolyglotSource path cleanText
  in IngestedSource path lang rawBytes cleanText prog

-- | Read and ingest a source file directly into an Outline.
ingestOutlineFile :: FilePath -> IO IngestedOutline
ingestOutlineFile path = withBinaryFile path ReadMode $ \h -> do
  rawBytes <- BS.hGetContents h
  let text = fastCanonicalizeBS rawBytes
      lang = detectLanguage path
      outline = parseOutlineSource path text
  pure $ IngestedOutline path lang rawBytes text outline

-- | Ingest from in-memory ByteString and Text directly into an Outline.
ingestOutlineSource :: FilePath -> BS.ByteString -> Text -> IngestedOutline
ingestOutlineSource path rawBytes text =
  let cleanText = fastCanonicalizeText text
      lang = detectLanguage path
      outline = parseOutlineSource path cleanText
  in IngestedOutline path lang rawBytes cleanText outline
