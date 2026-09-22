{- |
Module      : Canontra.ScopeSpec
Description : Unit test specification for the lexical scope analysis subsystem.

Tests lexical scope hierarchy construction, parameter binding, global/nonlocal tracking,
def-use reference resolution, and export boundary classification.
-}
{-# LANGUAGE OverloadedStrings #-}
module Canontra.ScopeSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Hspec

import Canontra.Analysis.Scope
import Canontra.Analysis.Symbol
import Canontra.Parser.Python (parsePythonSource)
import Canontra.Types (ParamKind (..))

spec :: Spec
spec = do
  describe "Scope & Symbol Analysis" $ do
    it "constructs module scope with top-level functions and classes" $ do
      let code = T.unlines
            [ "def calculate(a, b):"
            , "    return a + b"
            , ""
            , "class Manager:"
            , "    def run(self):"
            , "        return calculate(1, 2)"
            ]
      case parsePythonSource "test.py" code of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let trees = analyzeProgramScope prog
          case trees of
            [modTree] -> do
              scopeKind modTree `shouldBe` ScopeModule
              Map.member "calculate" (scopeSymbols modTree) `shouldBe` True
              Map.member "Manager" (scopeSymbols modTree) `shouldBe` True
            _ -> expectationFailure "Expected single module scope tree"

    it "resolves function parameters and local variables in child scope" $ do
      let code = T.unlines
            [ "def process(items, count=10):"
            , "    total = count * 2"
            , "    return total"
            ]
      case parsePythonSource "test.py" code of
        Left err -> expectationFailure (show err)
        Right prog -> do
          case analyzeProgramScope prog of
            [modTree] -> case scopeChildren modTree of
              [fnTree] -> do
                scopeKind fnTree `shouldBe` ScopeFunction "process"
                case findBinding "items" fnTree of
                  Just b -> symKind b `shouldBe` SymParameter ParamPositional
                  Nothing -> expectationFailure "Expected 'items' parameter binding"
                case findBinding "total" fnTree of
                  Just b -> symKind b `shouldBe` SymVariable BindingLocal
                  Nothing -> expectationFailure "Expected 'total' local variable binding"
              _ -> expectationFailure "Expected single child function scope"
            _ -> expectationFailure "Expected single module scope"

    it "tracks explicit global and nonlocal directives" $ do
      let code = T.unlines
            [ "counter = 0"
            , "def increment():"
            , "    global counter"
            , "    counter = counter + 1"
            ]
      case parsePythonSource "test.py" code of
        Left err -> expectationFailure (show err)
        Right prog -> do
          case analyzeProgramScope prog of
            [modTree] -> case scopeChildren modTree of
              [fnTree] -> case findBinding "counter" fnTree of
                Just b -> symKind b `shouldBe` SymVariable BindingGlobal
                Nothing -> expectationFailure "Expected 'counter' global binding in fn scope"
              _ -> expectationFailure "Expected single function scope"
            _ -> expectationFailure "Expected single module scope"

    it "detects public vs private exported symbols" $ do
      let code = T.unlines
            [ "public_api = 1"
            , "_private_helper = 2"
            , "def serve(): pass"
            , "def _internal(): pass"
            ]
      case parsePythonSource "test.py" code of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let st = buildSymbolTable prog
          let exports = exportedSymbols st
          let expNames = map symName exports
          expNames `shouldContain` ["public_api", "serve"]
          expNames `shouldNotContain` ["_private_helper", "_internal"]
