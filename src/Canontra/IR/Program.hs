{- |
Module      : Canontra.IR.Program
Description : Top-level program and module intermediate representations.

A Program represents a whole compilation unit or standalone script.
It harmonizes modules, their encapsulated declarations, statement flows,
and dependency relationships into a single coherent tree ready for
conservative normalization.
-}
module Canontra.IR.Program
  ( Module (..)
  , Program (..)
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Canontra.IR.Declaration (Declaration)
import Canontra.IR.Dependency (ImportDecl)
import Canontra.IR.Expression (Stmt)

data Module = Module
  { modName         :: Text          -- e.g. "main"
  , modImports      :: [ImportDecl]  -- e.g. imported modules
  , modDeclarations :: [Declaration] -- e.g. functions and classes
  , modStatements   :: [Stmt]        -- e.g. top-level execution statements
  } deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data Program = Program
  { progModules  :: [Module] -- e.g. list of constituent modules
  , progLanguage :: Text     -- e.g. "python"
  } deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)
