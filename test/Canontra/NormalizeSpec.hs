{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Canontra.NormalizeSpec
Description : Unit test specification for Normalizer v4 (Phase 4).

Tests pure declaration canonical sorting, stateful declaration order preservation,
runtime reflection docstring preservation (:preserve:, @preserve, :doc:),
decorator-level docstring retention (@preserve_docstring, @reflect, @doc),
and mathematical idempotence N(N(P)) == N(P).
-}
module Canontra.NormalizeSpec (spec) where

import Test.Hspec

import Canontra.Fingerprint.Declaration (computeF2)
import Canontra.Fingerprint.Structural (computeF1)
import Canontra.IR.Declaration
import Canontra.IR.Expression
import Canontra.IR.Program
import Canontra.Normalize.Normalize
import Canontra.Parser.Python (parsePythonSource)
import Canontra.Types (unFingerprint)

spec :: Spec
spec = do
  describe "Normalizer v4 - Declaration Classification & Ordering" $ do
    it "classifies undecorated functions and interfaces as provably pure" $ do
      let pureFn = DeclFunction (Function "pure_calc" [] Nothing [] [] False)
          decFn  = DeclFunction (Function "cached_calc" [] Nothing ["@lru_cache"] [] False)
          iface  = DeclInterface (Interface "Reader" [] [])
          alias  = DeclTypeAlias "UserID" Nothing
          trait  = DeclTrait (Trait "Display" [] [])
          cls    = DeclClass (Class "Service" [] [] [])
          st     = DeclStruct (Struct "Config" [] [] "pub")
      isProvablyPureDeclaration pureFn `shouldBe` True
      isProvablyPureDeclaration iface `shouldBe` True
      isProvablyPureDeclaration alias `shouldBe` True
      isProvablyPureDeclaration trait `shouldBe` True
      isProvablyPureDeclaration decFn `shouldBe` False
      isProvablyPureDeclaration cls `shouldBe` False
      isProvablyPureDeclaration st `shouldBe` False

    it "canonically sorts pure functions by declIdentifier" $ do
      let fnZ = DeclFunction (Function "zeta" [] Nothing [] [] False)
          fnA = DeclFunction (Function "alpha" [] Nothing [] [] False)
          fnM = DeclFunction (Function "mu" [] Nothing [] [] False)
          sorted = normalizeModuleDeclarations [fnZ, fnA, fnM]
          names = [fnName fn | DeclFunction fn <- sorted]
      names `shouldBe` ["alpha", "mu", "zeta"]

    it "preserves source execution order for stateful declarations (classes, decorated functions)" $ do
      let clsZ = DeclClass (Class "ZetaClass" [] [] [])
          clsA = DeclClass (Class "AlphaClass" [] [] [])
          decZ = DeclFunction (Function "zeta_dec" [] Nothing ["@dec"] [] False)
          decA = DeclFunction (Function "alpha_dec" [] Nothing ["@dec"] [] False)
          normalized = normalizeModuleDeclarations [clsZ, clsA, decZ, decA]
      map declIdentifier normalized `shouldBe` ["cls:ZetaClass", "cls:AlphaClass", "fn:zeta_dec", "fn:alpha_dec"]

    it "partitions pure declarations first (sorted) followed by stateful declarations in source order" $ do
      let fnZ = DeclFunction (Function "zeta_pure" [] Nothing [] [] False)
          clsB = DeclClass (Class "BetaClass" [] [] [])
          fnA = DeclFunction (Function "alpha_pure" [] Nothing [] [] False)
          clsA = DeclClass (Class "AlphaClass" [] [] [])
          normalized = normalizeModuleDeclarations [fnZ, clsB, fnA, clsA]
      map declIdentifier normalized `shouldBe` ["fn:alpha_pure", "fn:zeta_pure", "cls:BetaClass", "cls:AlphaClass"]

    it "guarantees commutativity: reordering pure functions yields identical F1 and F2 fingerprints" $ do
      let code1 = "def gamma(): pass\ndef alpha(): pass\ndef beta(): pass\n"
          code2 = "def alpha(): pass\ndef beta(): pass\ndef gamma(): pass\n"
      case (parsePythonSource "1.py" code1, parsePythonSource "2.py" code2) of
        (Right p1, Right p2) -> do
          let f1_1 = computeF1 p1
              f1_2 = computeF1 p2
              f2_1 = computeF2 p1
              f2_2 = computeF2 p2
          unFingerprint f1_1 `shouldBe` unFingerprint f1_2
          unFingerprint f2_1 `shouldBe` unFingerprint f2_2
        _ -> expectationFailure "Parse failed"

  describe "Normalizer v4 - Runtime Reflection Docstring Preservation" $ do
    it "preserves docstrings marked with :preserve: in AST" $ do
      let code = "def compute():\n    \"\"\":preserve: Critical reflection metadata\"\"\"\n    return 42\n"
      case parsePythonSource "test.py" code of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let normProg = normalizeProgram prog
          case progModules normProg of
            [Module _ _ [DeclFunction fn] _] -> do
              let body = fnBody fn
              length body `shouldBe` 2
              case head body of
                StmtExpr (ExprLit (LitString s)) -> s `shouldBe` ":preserve: Critical reflection metadata"
                _ -> expectationFailure "Expected leading preserved docstring"
            _ -> expectationFailure "Expected single module with single function declaration"

    it "preserves docstrings marked with @preserve" $ do
      let code = "def validate():\n    \"\"\"@preserve runtime validator contract\"\"\"\n    return True\n"
      case parsePythonSource "test.py" code of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let normProg = normalizeProgram prog
          case progModules normProg of
            [Module _ _ [DeclFunction fn] _] -> length (fnBody fn) `shouldBe` 2
            _ -> expectationFailure "Expected single module with single function declaration"

    it "preserves docstrings marked with :doc:" $ do
      let code = "def api_endpoint():\n    \"\"\":doc: OpenAPI specification summary\"\"\"\n    return 200\n"
      case parsePythonSource "test.py" code of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let normProg = normalizeProgram prog
          case progModules normProg of
            [Module _ _ [DeclFunction fn] _] -> length (fnBody fn) `shouldBe` 2
            _ -> expectationFailure "Expected single module with single function declaration"

    it "strips unflagged standard docstrings" $ do
      let code = "def standard():\n    \"\"\"Unflagged docstring to strip.\"\"\"\n    return 100\n"
      case parsePythonSource "test.py" code of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let normProg = normalizeProgram prog
          case progModules normProg of
            [Module _ _ [DeclFunction fn] _] -> do
              let body = fnBody fn
              length body `shouldBe` 1
              case head body of
                StmtReturn _ -> pure ()
                _ -> expectationFailure "Expected docstring to be stripped"
            _ -> expectationFailure "Expected single module with single function declaration"

    it "preserves docstrings in functions decorated with @preserve_docstring" $ do
      let code = "@preserve_docstring\ndef handler():\n    \"\"\"Standard text preserved via decorator\"\"\"\n    return 1\n"
      case parsePythonSource "test.py" code of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let normProg = normalizeProgram prog
          case progModules normProg of
            [Module _ _ [DeclFunction fn] _] -> length (fnBody fn) `shouldBe` 2
            _ -> expectationFailure "Expected single module with single function declaration"

    it "preserves docstrings in classes decorated with @preserve_docstring across methods" $ do
      let code = "@preserve_docstring\nclass Model:\n    def predict(self):\n        \"\"\"Inference docstring\"\"\"\n        return 0\n"
      case parsePythonSource "test.py" code of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let normProg = normalizeProgram prog
          case progModules normProg of
            [Module _ _ [DeclClass cls] _] ->
              case clsMethods cls of
                [m] -> length (fnBody m) `shouldBe` 2
                _ -> expectationFailure "Expected single method"
            _ -> expectationFailure "Expected single class declaration"

    it "differentiates structural hash F1 when reflection docstrings differ" $ do
      let code1 = "def test():\n    \"\"\":preserve: Spec version 1.0\"\"\"\n    return 1\n"
          code2 = "def test():\n    \"\"\":preserve: Spec version 2.0\"\"\"\n    return 1\n"
      case (parsePythonSource "1.py" code1, parsePythonSource "2.py" code2) of
        (Right p1, Right p2) -> do
          computeF1 p1 `shouldNotBe` computeF1 p2
        _ -> expectationFailure "Parse failed"

  describe "Normalizer v4 - Algebraic Idempotence & Pass Parity" $ do
    it "guarantees Normalizer v4 idempotence N(N(P)) == N(P)" $ do
      let code = "class Svc:\n    def a(self): pass\ndef z(): pass\ndef b(): pass\n"
      case parsePythonSource "idemp.py" code of
        Left err -> expectationFailure (show err)
        Right prog -> do
          let n1 = normalizeProgram prog
              n2 = normalizeProgram n1
          n1 `shouldBe` n2

    it "guarantees declaration normalization idempotence N(N(D)) == N(D)" $ do
      let fnZ = DeclFunction (Function "z" [] Nothing [] [] False)
          fnA = DeclFunction (Function "a" [] Nothing [] [] False)
          cls = DeclClass (Class "C" [] [] [])
          decls = [fnZ, cls, fnA]
          norm1 = normalizeModuleDeclarations decls
          norm2 = normalizeModuleDeclarations norm1
      norm1 `shouldBe` norm2
