{-# LANGUAGE OverloadedStrings #-}
module Canontra.PolyglotSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map

import Canontra.Analysis.Scope (analyzeModuleScope, ScopeTree(..))
import Canontra.Fingerprint.Bundle (computeBundleFromSource)
import Canontra.IR.Declaration
import Canontra.IR.Expression
import Canontra.IR.Program
import Canontra.Parser.Polyglot (detectLanguage, parsePolyglotSource)
import Canontra.Types

spec :: Spec
spec = do
  describe "Polyglot Language Detection" $ do
    it "detects Python extensions" $ do
      detectLanguage "src/main.py" `shouldBe` LangPython
      detectLanguage "types.pyi" `shouldBe` LangPython

    it "detects JavaScript & TypeScript extensions" $ do
      detectLanguage "app/index.js" `shouldBe` LangJavaScript
      detectLanguage "app/component.jsx" `shouldBe` LangJavaScript
      detectLanguage "src/server.ts" `shouldBe` LangTypeScript
      detectLanguage "src/view.tsx" `shouldBe` LangTypeScript

    it "detects Go extensions" $ do
      detectLanguage "pkg/server/main.go" `shouldBe` LangGo

    it "detects Rust extensions" $ do
      detectLanguage "src/lib.rs" `shouldBe` LangRust

  describe "Python 3.8-3.12 Advanced Conformance" $ do
    it "parses Python 3.10 match/case statement with pattern guards" $ do
      let pyCode = "match val:\n    case 1:\n        x = 10\n    case Point(a, b) if a > 0:\n        x = a + b\n    case _:\n        x = 0"
      case parsePolyglotSource "match.py" pyCode of
        Left err -> expectationFailure ("Match parse failed: " ++ show err)
        Right prog -> case progModules prog of
          [m] -> case modStatements m of
            [StmtMatch _ cases] -> do
              length cases `shouldBe` 3
              let c2 = cases !! 1
              mcGuard c2 `shouldNotBe` Nothing
            other -> expectationFailure ("Expected StmtMatch, got: " ++ show other)
          _ -> expectationFailure "Expected 1 module"

    it "hoists walrus bindings inside comprehensions to enclosing scope (PEP 572)" $ do
      let pyCode = "def process(items):\n    res = [y for x in items if (y := x * 2) > 0]\n    return y"
      case parsePolyglotSource "walrus.py" pyCode of
        Left err -> expectationFailure ("Walrus parse failed: " ++ show err)
        Right prog -> case progModules prog of
          [m] -> do
            let tree = analyzeModuleScope 0 m
            case scopeChildren tree of
              [fnScope] -> Map.member "y" (scopeSymbols fnScope) `shouldBe` True
              _         -> expectationFailure "Expected 1 child scope"
          _ -> expectationFailure "Expected 1 module"

    it "parses PEP 701 nested f-strings with internal quotes" $ do
      let pyCode = "msg = f\"Result: {', '.join([x for x in items])}\""
      case parsePolyglotSource "fstr.py" pyCode of
        Left err -> expectationFailure ("FString parse failed: " ++ show err)
        Right prog -> case progModules prog of
          [m] -> case modStatements m of
            [StmtAssign _ (ExprFormattedString parts)] ->
              length parts `shouldSatisfy` (> 1)
            other -> expectationFailure ("Expected ExprFormattedString, got: " ++ show other)
          _ -> expectationFailure "Expected 1 module"

  describe "JavaScript & TypeScript Ingestion" $ do
    it "parses TypeScript functions, interfaces, and classes" $ do
      let tsCode = "import { User } from './models';\ninterface Service {\n  process(id: string): boolean;\n}\nclass UserService implements Service {\n  async process(id: string) {\n    return true;\n  }\n}"
      case parsePolyglotSource "user.ts" tsCode of
        Left err -> expectationFailure ("TS parse failed: " ++ show err)
        Right _  -> pure ()

    it "computes 8-tier fingerprint bundle for TypeScript" $ do
      let tsCode = "const add = (a: number, b: number): number => { return a + b; };"
      case computeBundleFromSource "math.ts" tsCode of
        Left err -> expectationFailure ("TS bundle failed: " ++ show err)
        Right b  -> do
          unFingerprint (f0Source b) `shouldNotBe` ""
          unFingerprint (f1Structural b) `shouldNotBe` ""
          unFingerprint (fCFControlFlow b) `shouldNotBe` ""
          unFingerprint (fDFDataFlow b) `shouldNotBe` ""
          unFingerprint (f4Composite b) `shouldNotBe` ""

    it "disambiguates regex literal from division operator" $ do
      let tsRegex = "const pattern = /^[a-z]+$/i;\nconst ratio = a / b / c;"
      case parsePolyglotSource "regex.ts" tsRegex of
        Left err -> expectationFailure ("Regex/div parse failed: " ++ show err)
        Right prog -> case progModules prog of
          [m] -> length (modStatements m) `shouldBe` 2
          _   -> expectationFailure "Expected 1 module"

    it "enforces Automatic Semicolon Insertion for return on newline" $ do
      let tsASI = "function test() {\n  return\n  x = 1;\n}"
      case parsePolyglotSource "asi.ts" tsASI of
        Left err -> expectationFailure ("ASI parse failed: " ++ show err)
        Right prog -> case progModules prog of
          [m] -> case modDeclarations m of
            [DeclFunction fn] -> case fnBody fn of
              (StmtReturn Nothing : _) -> pure ()
              other -> expectationFailure ("Expected StmtReturn Nothing, got: " ++ show other)
            _ -> expectationFailure "Expected DeclFunction"
          _ -> expectationFailure "Expected 1 module"

    it "extracts constructor parameter properties into class declarations" $ do
      let tsClass = "class Service {\n  constructor(public readonly name: string, private count: number) {}\n}"
      case parsePolyglotSource "service.ts" tsClass of
        Left err -> expectationFailure ("TS class parse failed: " ++ show err)
        Right prog -> case progModules prog of
          [m] -> case modDeclarations m of
            [DeclClass cls] -> length (clsMethods cls) `shouldSatisfy` (>= 3)
            _               -> expectationFailure "Expected DeclClass"
          _ -> expectationFailure "Expected 1 module"

  describe "Go Ingestion" $ do
    it "parses Go packages, receiver methods, and concurrency constructs" $ do
      let goCode = "package worker\nimport (\n  \"fmt\"\n  \"time\"\n)\ntype Worker struct {\n  id int\n}\nfunc (w *Worker) Start(ch chan int) {\n  go func() {\n    defer fmt.Println(\"done\")\n    val := <-ch\n    fmt.Println(val)\n  }()\n}"
      case parsePolyglotSource "worker.go" goCode of
        Left err -> expectationFailure ("Go parse failed: " ++ show err)
        Right _  -> pure ()

    it "computes 8-tier fingerprint bundle for Go" $ do
      let goCode = "package main\nfunc Add(a int, b int) int {\n  return a + b\n}"
      case computeBundleFromSource "math.go" goCode of
        Left err -> expectationFailure ("Go bundle failed: " ++ show err)
        Right b  -> do
          unFingerprint (f1Structural b) `shouldNotBe` ""
          unFingerprint (fCFControlFlow b) `shouldNotBe` ""
          unFingerprint (fDFDataFlow b) `shouldNotBe` ""

    it "parses Go 1.18+ generic type parameters on functions and structs" $ do
      let goCode = "package main\ntype Stack[T any] struct {\n  items []T\n}\nfunc Map[T, U any](ts []T, f func(T) U) []U {\n  return nil\n}"
      case parsePolyglotSource "generic.go" goCode of
        Left err -> expectationFailure ("Go generic parse failed: " ++ show err)
        Right prog -> case progModules prog of
          [m] -> do
            let decls = modDeclarations m
            length decls `shouldBe` 2
            case decls of
              [DeclStruct st, DeclFunction fn] -> do
                stName st `shouldBe` "Stack"
                fnName fn `shouldBe` "Map"
                fnDecorators fn `shouldNotBe` []
              _ -> expectationFailure ("Expected DeclStruct and DeclFunction, got: " ++ show decls)
          _ -> expectationFailure "Expected 1 module"

    it "parses Go factored var blocks and multi-variable assignments" $ do
      let goCode = "package main\nvar (\n  host = \"localhost\"\n  port = 8080\n)\nfunc init() {\n  x, y := 1, 2\n}"
      case parsePolyglotSource "factored.go" goCode of
        Left err -> expectationFailure ("Go factored parse failed: " ++ show err)
        Right prog -> case progModules prog of
          [m] -> length (modStatements m) `shouldSatisfy` (>= 2)
          _   -> expectationFailure "Expected 1 module"

  describe "Rust Ingestion" $ do
    it "parses Rust structs, traits, impls, and functions" $ do
      let rsCode = "use std::collections::HashMap;\npub struct Storage {\n  data: HashMap<String, String>,\n}\ntrait Handler {\n  fn handle(&self) -> bool;\n}\nimpl Handler for Storage {\n  fn handle(&self) -> bool {\n    return true;\n  }\n}"
      case parsePolyglotSource "storage.rs" rsCode of
        Left err -> expectationFailure ("Rust parse failed: " ++ show err)
        Right _  -> pure ()

    it "computes 8-tier fingerprint bundle for Rust" $ do
      let rsCode = "pub fn add(a: i32, b: i32) -> i32 {\n  return a + b;\n}"
      case computeBundleFromSource "math.rs" rsCode of
        Left err -> expectationFailure ("Rust bundle failed: " ++ show err)
        Right b  -> do
          unFingerprint (f1Structural b) `shouldNotBe` ""
          unFingerprint (fCFControlFlow b) `shouldNotBe` ""
          unFingerprint (fDFDataFlow b) `shouldNotBe` ""

    it "parses Rust macro calls with nested balanced delimiters" $ do
      let rsCode = "fn main() {\n  let v = vec![foo(1, 2), bar[3]];\n  println!(\"{}\", format!(\"{:?}\", v));\n}"
      case parsePolyglotSource "macro.rs" rsCode of
        Left err -> expectationFailure ("Rust macro parse failed: " ++ show err)
        Right prog -> case progModules prog of
          [m] -> case modDeclarations m of
            [DeclFunction fn] -> length (fnBody fn) `shouldSatisfy` (>= 2)
            _                 -> expectationFailure "Expected DeclFunction"
          _ -> expectationFailure "Expected 1 module"

    it "parses Rust lifetime parameters, trait bounds, and where clauses" $ do
      let rsCode = "pub fn process<'a, T>(item: &'a T) -> &'a T where T: Display {\n  return item;\n}"
      case parsePolyglotSource "lifetime.rs" rsCode of
        Left err -> expectationFailure ("Rust lifetime parse failed: " ++ show err)
        Right prog -> case progModules prog of
          [m] -> case modDeclarations m of
            [DeclFunction fn] -> do
              fnName fn `shouldBe` "process"
              fnDecorators fn `shouldNotBe` []
              fnReturnType fn `shouldBe` Just "&'aT"
            _ -> expectationFailure "Expected DeclFunction"
          _ -> expectationFailure "Expected 1 module"
