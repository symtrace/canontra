{- |
Module      : Canontra.Analysis.Symbol
Description : Symbol table indexing, classification, and export boundary analysis.

This module provides symbol-level indexing and export classification for modules.
It evaluates public API surfaces, detects module-level exports, and identifies
symbol usage patterns for dependency and call-graph resolution.
-}
module Canontra.Analysis.Symbol
  ( SymbolTable (..)
  , buildSymbolTable
  , exportedSymbols
  , isExportedName
  , lookupSymbol
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Canontra.Analysis.Scope
import Canontra.IR.Program

newtype SymbolTable = SymbolTable
  { stBindings :: Map Text SymbolBinding
  } deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

buildSymbolTable :: Program -> SymbolTable
buildSymbolTable prog =
  let trees = analyzeProgramScope prog
      allSyms = concatMap allBindings trees
      symMap = Map.fromList [(symName s, s) | s <- allSyms]
  in SymbolTable symMap

exportedSymbols :: SymbolTable -> [SymbolBinding]
exportedSymbols (SymbolTable symMap) =
  filter symIsExported (Map.elems symMap)

isExportedName :: Text -> Bool
isExportedName name =
  not (T.isPrefixOf "_" name)

lookupSymbol :: Text -> SymbolTable -> Maybe SymbolBinding
lookupSymbol name (SymbolTable symMap) =
  Map.lookup name symMap
