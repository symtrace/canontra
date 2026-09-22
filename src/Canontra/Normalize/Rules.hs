{- |
Module      : Canontra.Normalize.Rules
Description : Version constants and AST normalization rewrite rules.

Contains core normalization definitions for canontra v0.0.4-alpha,
including docstring identification predicates and semantic transformation rules.
-}
module Canontra.Normalize.Rules
  ( engineVersion
  , engineName
  , normalizationVersion
  , isDocstring
  ) where

import Data.Text (Text)

import Canontra.IR.Expression

engineVersion :: Text
engineVersion = "0.1.0"

normalizationVersion :: Text
normalizationVersion = "0.1.0"

engineName :: Text
engineName = "canontra"

isDocstring :: Stmt -> Bool
isDocstring (StmtExpr (ExprLit (LitString _))) = True
isDocstring _                                 = False
