{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Canontra.OutlineSpec
Description : Test suite for two-phase selective Outline parsing and accelerated F2/F3 fingerprinting.
-}
module Canontra.OutlineSpec (spec) where

import Test.Hspec

import Canontra.Fingerprint.Declaration (computeF2)
import Canontra.Fingerprint.Dependency (computeF3)
import Canontra.Parser.Ingest (IngestedOutline (..), ingestOutlineSource)
import Canontra.Parser.Outline
import Canontra.Parser.Polyglot (parsePolyglotSource)
import Canontra.Types

spec :: Spec
spec = do
  describe "Selective Outline Mode" $ do

    describe "Python Outline Ingestion" $ do
      it "extracts Python declarations and imports with empty bodies" $ do
        let pyCode = "import os\nfrom sys import path\n\n@decorator\ndef calculate(x: int, y: int = 10) -> int:\n    total = x + y\n    return total * 2\n"
        case parseOutlinePython "calc.py" pyCode of
          Left err -> expectationFailure (show err)
          Right outline -> do
            outPath outline `shouldBe` "calc.py"
            outLanguage outline `shouldBe` "python"
            length (outImports outline) `shouldBe` 2
            length (outDeclarations outline) `shouldBe` 1

      it "produces identical F2 & F3 fingerprints between full AST and Outline mode" $ do
        let pyCode = "import math\n\ndef compute(a: float, b: float) -> float:\n    # internal logic\n    temp = a * 2.0\n    return temp + b\n"
        case (parsePolyglotSource "math.py" pyCode, parseOutlineSource "math.py" pyCode) of
          (Right fullProg, Right outline) -> do
            let f2Full = computeF2 fullProg
            let f2Outline = computeF2Outline outline
            unFingerprint f2Full `shouldBe` unFingerprint f2Outline

            let f3Full = computeF3 fullProg
            let f3Outline = computeF3Outline outline
            unFingerprint f3Full `shouldBe` unFingerprint f3Outline
          _ -> expectationFailure "Parse failed"

      it "maintains F2/F3 invariance when internal function bodies change" $ do
        let code1 = "import os\ndef run(x: int) -> int:\n    return x + 1\n"
        let code2 = "import os\ndef run(x: int) -> int:\n    # completely different body\n    temp = x * 100\n    if temp > 0:\n        return temp\n    return 0\n"
        case (parseOutlineSource "mod.py" code1, parseOutlineSource "mod.py" code2) of
          (Right out1, Right out2) -> do
            unFingerprint (computeF2Outline out1) `shouldBe` unFingerprint (computeF2Outline out2)
            unFingerprint (computeF3Outline out1) `shouldBe` unFingerprint (computeF3Outline out2)
          _ -> expectationFailure "Outline parse failed"

    describe "TypeScript & JavaScript Outline Ingestion" $ do
      it "extracts TS interfaces, classes, and imports" $ do
        let tsCode = "import { User } from './models';\nexport interface UserService {\n  findUser(id: string): User;\n}\nexport class ServiceImpl implements UserService {\n  async findUser(id: string) {\n    return fetch(id);\n  }\n}"
        case parseOutlineJS "service.ts" tsCode of
          Left err -> expectationFailure (show err)
          Right outline -> do
            outLanguage outline `shouldBe` "typescript"
            length (outDeclarations outline) `shouldSatisfy` (>= 2)
            length (outImports outline) `shouldBe` 1

    describe "Go Outline Ingestion" $ do
      it "extracts Go receiver methods, structs, and imports" $ do
        let goCode = "package server\nimport \"net/http\"\ntype Handler struct {\n  port int\n}\nfunc (h *Handler) ServeHTTP(w http.ResponseWriter, r *http.Request) {\n  w.Write([]byte(\"ok\"))\n}"
        case parseOutlineGo "server.go" goCode of
          Left err -> expectationFailure (show err)
          Right outline -> do
            outLanguage outline `shouldBe` "go"
            length (outImports outline) `shouldBe` 1
            length (outDeclarations outline) `shouldSatisfy` (>= 2)

    describe "Rust Outline Ingestion" $ do
      it "extracts Rust structs, traits, and impl blocks" $ do
        let rsCode = "use std::sync::Arc;\npub struct Config {\n  workers: usize,\n}\npub trait Runner {\n  fn start(&self) -> bool;\n}\nimpl Runner for Config {\n  fn start(&self) -> bool {\n    println!(\"running\");\n    true\n  }\n}"
        case parseOutlineRust "app.rs" rsCode of
          Left err -> expectationFailure (show err)
          Right outline -> do
            outLanguage outline `shouldBe` "rust"
            length (outImports outline) `shouldBe` 1
            length (outDeclarations outline) `shouldSatisfy` (>= 3)

    describe "IngestedOutline Pipeline" $ do
      it "ingests in-memory source directly into IngestedOutline" $ do
        let code = "def ping():\n    return 'pong'\n"
        let ingested = ingestOutlineSource "ping.py" "def ping(): return 'pong'" code
        ioLanguage ingested `shouldBe` LangPython
        case ioOutline ingested of
          Right out -> length (outDeclarations out) `shouldBe` 1
          Left err  -> expectationFailure (show err)
