{- |
Module      : Canontra.IR.Dependency
Description : Import and dependency graph representations.

Dependencies define the external and internal linkage of a module.
By distilling imports, symbol associations, and resolved usage contracts into a canonical graph,
canontra can tell whether a refactoring touched the external boundary,
altered usage classifications (e.g. type-only vs direct call), or remained purely internal.
-}
module Canontra.IR.Dependency
  ( DependencyUsage (..)
  , ResolvedImport (..)
  , RichDependencyGraph (..)
  , ImportTarget (..)
  , ImportDecl (..)
  , DependencyGraph (..)
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data DependencyUsage
  = DepUnused              -- e.g. Imported but never referenced in AST
  | DepDirectCall [Text]   -- e.g. Functions/methods invoked
  | DepInheritance [Text]  -- e.g. Base classes extended
  | DepTypeOnly [Text]     -- e.g. Referenced only in type annotations
  | DepValueRef [Text]     -- e.g. Passed as argument or assigned
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data ResolvedImport = ResolvedImport
  { impModule      :: Text
  , impSymbol      :: Maybe Text
  , impAlias       :: Maybe Text
  , impIsRelative  :: Int -- e.g. Relative dot count (e.g. 1 for '.', 2 for '..')
  , impUsage       :: DependencyUsage
  } deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data RichDependencyGraph = RichDependencyGraph
  { rdExternalImports :: [ResolvedImport]
  , rdIntraModuleDeps :: [(Text, Text)] -- (Caller / Dependent, Callee / Dependency)
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data ImportTarget
  = ImportAll                              -- e.g. from math import *
  | ImportSymbols [(Text, Maybe Text)]     -- e.g. from os import path as p, environ
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data ImportDecl
  = ImportModule Text (Maybe Text)         -- e.g. import numpy as np
  | ImportFrom Text ImportTarget           -- e.g. from typing import List, Dict
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data DependencyGraph = DependencyGraph
  { depImports    :: [ImportDecl]          -- e.g. list of imports in canonical order
  , depReferences :: [Text]                -- e.g. extracted referenced symbol names
  } deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)
