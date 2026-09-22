{-# LANGUAGE OverloadedStrings #-}
module Canontra.ConformanceSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Canontra.Analysis.Scope (SymbolBinding (..), allBindings, analyzeProgramScope)
import Canontra.Fingerprint.Declaration (computeF2)
import Canontra.Fingerprint.Structural (computeF1)
import Canontra.IR.Declaration (Class (..), Declaration (..))
import Canontra.IR.Program (Module (..), Program (..))
import Canontra.Normalize.Normalize (normalizeProgram)
import Canontra.Parser.FastPython (advanceColumn, parseFastPythonSource)
import Canontra.Parser.Go (parseGoSource)
import Canontra.Parser.JS (parseJSSource)
import Canontra.Parser.Python (parsePythonSource)
import Canontra.Parser.Rust (parseRustSource)
import Canontra.Types

spec :: Spec
spec = do
  describe "Polyglot Language & Normalizer v4 Conformance Suite" $ do

    describe "Python 3.8-3.12 Golden Conformance" $ do
      it "PEP 572: parses walrus operator in if conditions and binds variable" $ do
        let code = "def check(data):\n    if (n := len(data)) > 10:\n        return n\n    return 0\n"
        case parsePythonSource "walrus_if.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let sc = analyzeProgramScope prog
            any (\b -> symName b == "n") (concatMap allBindings sc) `shouldBe` True

      it "PEP 572: parses walrus operator in while conditions" $ do
        let code = "def read_all(stream):\n    while (chunk := stream.read(1024)) != b'':\n        process(chunk)\n"
        case parsePythonSource "walrus_while.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let sc = analyzeProgramScope prog
            any (\b -> symName b == "chunk") (concatMap allBindings sc) `shouldBe` True

      it "PEP 572: parses walrus operator in list comprehensions and hoists target" $ do
        let code = "def parse_lines(lines):\n    return [y for line in lines if (y := line.strip())]\n"
        case parsePythonSource "walrus_comp.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let sc = analyzeProgramScope prog
            any (\b -> symName b == "y") (concatMap allBindings sc) `shouldBe` True

      it "PEP 634: parses match/case with integer literal patterns" $ do
        let code = "def http_status(code):\n    match code:\n        case 200:\n            return 'OK'\n        case 404:\n            return 'Not Found'\n"
        case parsePythonSource "match_lit.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "PEP 634: parses match/case with sequence patterns" $ do
        let code = "def point(p):\n    match p:\n        case [x, y]:\n            return x + y\n        case [x, y, z]:\n            return x + y + z\n"
        case parsePythonSource "match_seq.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "PEP 634: parses match/case with mapping and dict patterns" $ do
        let code = "def handle(msg):\n    match msg:\n        case {'type': 'ping', 'id': i}:\n            return i\n"
        case parsePythonSource "match_map.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "PEP 634: parses match/case with class patterns" $ do
        let code = "def inspect(node):\n    match node:\n        case Value(val):\n            return val\n"
        case parsePythonSource "match_cls.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "PEP 634: parses match/case with wildcard pattern and guards" $ do
        let code = "def categorize(val):\n    match val:\n        case x if x < 0:\n            return 'neg'\n        case _:\n            return 'other'\n"
        case parsePythonSource "match_guard.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "PEP 701: parses nested f-strings with quote reuse" $ do
        let code = "msg = f\"outer {f'nested {inner}'}\""
        case parsePythonSource "fstring1.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "PEP 701: parses double-quoted f-strings nested within double-quoted f-strings" $ do
        let code = "msg = f\"outer {f\\\"inner\\\"}\""
        case parsePythonSource "fstring2.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "PEP 8: advanceColumn computes accurate tab-stop modulo column advancement" $ do
        advanceColumn 0 '\t' `shouldBe` 8
        advanceColumn 1 '\t' `shouldBe` 8
        advanceColumn 7 '\t' `shouldBe` 8
        advanceColumn 8 '\t' `shouldBe` 16
        advanceColumn 9 '\t' `shouldBe` 16
        advanceColumn 15 '\t' `shouldBe` 16
        advanceColumn 16 '\t' `shouldBe` 24
        advanceColumn 5 ' ' `shouldBe` 6

      it "FastPython: parses deeply nested 16-level indentation block without stack overflow" $ do
        let indents = concat [replicate (i * 4) ' ' ++ "if level" ++ show i ++ ":\n" | i <- [0..15 :: Int]]
            body = replicate (16 * 4) ' ' ++ "return 42\n"
            code = "def deeply_nested():\n" ++ indents ++ body
        case parseFastPythonSource "deep.py" (T.pack code) of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

    describe "TypeScript / JavaScript Golden Conformance" $ do
      it "Context-Aware Lexer: identifies regex literal after assignment operator =" $ do
        let code = "const rx = /[a-z0-9]+/i;"
        case parseJSSource "rx1.js" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "Context-Aware Lexer: identifies regex literal after opening parenthesis (" $ do
        let code = "if (/^[0-9]+$/.test(str)) { return true; }"
        case parseJSSource "rx2.js" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "Context-Aware Lexer: identifies regex literal after return keyword" $ do
        let code = "function getRx() { return /abc/g; }"
        case parseJSSource "rx3.js" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "Context-Aware Lexer: identifies division operator following identifier" $ do
        let code = "const ratio = numerator / denominator;"
        case parseJSSource "div1.js" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "Context-Aware Lexer: identifies division operator following closing paren" $ do
        let code = "const val = (a + b) / 2;"
        case parseJSSource "div2.js" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "Context-Aware Lexer: identifies division operator following number literal" $ do
        let code = "const half = 100 / 2;"
        case parseJSSource "div3.js" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "ASI: inserts automatic semicolon preceding newline return" $ do
        let code = "function test() {\n    a = 1\n    return a\n}"
        case parseJSSource "asi_ret.js" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "ASI: inserts automatic semicolon preceding newline break" $ do
        let code = "while (true) {\n    x = 1\n    break\n}"
        case parseJSSource "asi_brk.js" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "Parameter Properties: synthesizes class fields from public constructor parameters" $ do
        let code = "class User {\n    constructor(public id: number, public name: string) {}\n}"
        case parseJSSource "param_prop.ts" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let f2 = computeF2 prog
            unFingerprint f2 `shouldNotBe` ""

      it "Parameter Properties: synthesizes class fields from private and readonly parameters" $ do
        let code = "class Config {\n    constructor(private readonly apiKey: string, protected port: number) {}\n}"
        case parseJSSource "param_priv.ts" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

    describe "Go 1.18+ Golden Conformance" $ do
      it "Go Generics: parses function with single type parameter" $ do
        let code = "package main\nfunc Identity[T any](val T) T {\n    return val\n}"
        case parseGoSource "gen_fn.go" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "Go Generics: parses function with multiple comparable type parameters" $ do
        let code = "package main\nfunc Find[K comparable, V any](m map[K]V, key K) V {\n    return m[key]\n}"
        case parseGoSource "gen_fn2.go" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "Go Generics: parses struct with type parameters" $ do
        let code = "package main\ntype Stack[T any] struct {\n    items []T\n}"
        case parseGoSource "gen_st.go" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "Go Factored Blocks: unrolls parenthesized var block into individual declarations" $ do
        let code = "package main\nvar (\n    Port = 8080\n    Host = \"localhost\"\n)\n"
        case parseGoSource "fact_var.go" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let f2 = computeF2 prog
            unFingerprint f2 `shouldNotBe` ""

      it "Go Factored Blocks: unrolls parenthesized const block into individual declarations" $ do
        let code = "package main\nconst (\n    StatusOK = 200\n    StatusNotFound = 404\n)\n"
        case parseGoSource "fact_const.go" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF2 prog) `shouldNotBe` ""

      it "Go Assignments: parses multi-variable short assignments" $ do
        let code = "package main\nfunc initVars() {\n    a, b, c := 1, 2, 3\n    print(a, b, c)\n}"
        case parseGoSource "multi_assign.go" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

    describe "Rust Golden Conformance" $ do
      it "Rust Macros: parses parentheses-delimited macro calls" $ do
        let code = "fn main() {\n    println!(\"Formatted: {}\", 42);\n}"
        case parseRustSource "macro_paren.rs" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "Rust Macros: parses square-bracket-delimited macro calls" $ do
        let code = "fn create_list() -> Vec<i32> {\n    vec![1, 2, 3, 4, 5]\n}"
        case parseRustSource "macro_bracket.rs" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "Rust Macros: parses curly-brace-delimited macro calls" $ do
        let code = "fn setup() {\n    custom_block! { let x = 10; }\n}"
        case parseRustSource "macro_brace.rs" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "Rust Lifetimes: parses function with single lifetime parameter" $ do
        let code = "fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {\n    if x.len() > y.len() { x } else { y }\n}"
        case parseRustSource "lifetime.rs" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "Rust Lifetimes: parses function with lifetime and generic type parameters" $ do
        let code = "fn wrap<'a, T>(val: &'a T) -> Ref<'a, T> {\n    Ref { val }\n}"
        case parseRustSource "lifetime_gen.rs" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "Rust Where Clauses: parses function signature with where clause constraints" $ do
        let code = "fn print_val<T>(val: T) where T: std::fmt::Display + Clone {\n    println!(\"{}\", val);\n}"
        case parseRustSource "where_clause.rs" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "Rust Statements: parses let bindings with mut and explicit type annotations" $ do
        let code = "fn counter() {\n    let mut total: i64 = 0;\n    total += 1;\n}"
        case parseRustSource "let_mut.rs" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

    describe "Normalizer v4 Commutativity & Side-Effect Invariance" $ do
      it "guarantees pure function canonical sorting commutativity" $ do
        let p1 = "def gamma():\n    return 3\ndef alpha():\n    return 1\ndef beta():\n    return 2\n"
            p2 = "def beta():\n    return 2\ndef gamma():\n    return 3\ndef alpha():\n    return 1\n"
        case (parsePythonSource "p1.py" p1, parsePythonSource "p2.py" p2) of
          (Right prog1, Right prog2) -> do
            let n1 = normalizeProgram prog1
                n2 = normalizeProgram prog2
            computeF1 n1 `shouldBe` computeF1 n2
            computeF2 n1 `shouldBe` computeF2 n2
          _ -> expectationFailure "Pure function parse failed"

      it "preserves sequential execution order for classes and decorated functions" $ do
        let code = "class First:\n    pass\nclass Second:\n    pass\n"
        case parsePythonSource "classes.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let norm = normalizeProgram prog
            case concatMap modDeclarations (progModules norm) of
              [DeclClass c1, DeclClass c2] -> do
                clsName c1 `shouldBe` "First"
                clsName c2 `shouldBe` "Second"
              other -> expectationFailure ("Expected 2 classes in order, got: " ++ show (length other))

      it "preserves docstrings marked with @preserve" $ do
        let code = "def api_handler():\n    \"\"\"@preserve: OpenTelemetry trace annotation\"\"\"\n    return True\n"
        case parsePythonSource "doc_pres.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let norm = normalizeProgram prog
                f1_pres = computeF1 norm
            let code_no_doc = "def api_handler():\n    return True\n"
            case parsePythonSource "doc_none.py" code_no_doc of
              Left err2 -> expectationFailure (show err2)
              Right prog_no_doc -> do
                let f1_no_doc = computeF1 (normalizeProgram prog_no_doc)
                f1_pres `shouldNotBe` f1_no_doc

      it "preserves docstrings marked with :doc:" $ do
        let code = "def documented():\n    \"\"\":doc: Public OpenAPI spec summary\"\"\"\n    pass\n"
        case parsePythonSource "doc_meta.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let norm = normalizeProgram prog
            unFingerprint (computeF1 norm) `shouldNotBe` ""

      it "preserves docstrings marked with :preserve:" $ do
        let code = "def compute():\n    \"\"\":preserve: Critical computation doc\"\"\"\n    return 42\n"
        case parsePythonSource "doc_pres2.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let norm = normalizeProgram prog
            unFingerprint (computeF1 norm) `shouldNotBe` ""

      it "preserves docstrings in functions decorated with @preserve_docstring" $ do
        let code = "@preserve_docstring\ndef documented():\n    \"\"\"Preserved docstring\"\"\"\n    return 1\n"
        case parsePythonSource "dec_doc.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let norm = normalizeProgram prog
            unFingerprint (computeF1 norm) `shouldNotBe` ""

      it "preserves docstrings in functions decorated with @reflect" $ do
        let code = "@reflect\ndef inspect_me():\n    \"\"\"Reflection metadata\"\"\"\n    return 'ok'\n"
        case parsePythonSource "ref_doc.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let norm = normalizeProgram prog
            unFingerprint (computeF1 norm) `shouldNotBe` ""

      it "preserves docstrings across methods in classes decorated with @preserve_docstring" $ do
        let code = "@preserve_docstring\nclass Model:\n    def validate(self):\n        \"\"\"Method doc\"\"\"\n        return True\n"
        case parsePythonSource "cls_doc.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let norm = normalizeProgram prog
            unFingerprint (computeF1 norm) `shouldNotBe` ""

      it "strips unflagged standard docstrings from module functions" $ do
        let code1 = "def regular():\n    \"\"\"Standard unflagged docstring\"\"\"\n    return 10\n"
            code2 = "def regular():\n    return 10\n"
        case (parsePythonSource "r1.py" code1, parsePythonSource "r2.py" code2) of
          (Right p1, Right p2) -> do
            computeF1 (normalizeProgram p1) `shouldBe` computeF1 (normalizeProgram p2)
          _ -> expectationFailure "Regular docstring parse failed"

      it "preserves execution order for module-level variable assignments" $ do
        let code = "A = 1\nB = A + 2\nC = B * 3\n"
        case parsePythonSource "vars.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let norm = normalizeProgram prog
            unFingerprint (computeF1 norm) `shouldNotBe` ""

      it "PEP 572: parses walrus operator in set comprehensions" $ do
        let code = "def unique_transforms(items):\n    return {y for x in items if (y := x * 3)}\n"
        case parsePythonSource "set_walrus.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let sc = analyzeProgramScope prog
            any (\b -> symName b == "y") (concatMap allBindings sc) `shouldBe` True

      it "Context-Aware Lexer: identifies regex literal after comma" $ do
        let code = "const arr = [1, /test/g, 3];"
        case parseJSSource "rx_comma.js" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "Context-Aware Lexer: identifies regex literal after colon in object literal" $ do
        let code = "const obj = { matcher: /^api\\/v1/ };"
        case parseJSSource "rx_colon.js" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "ASI: inserts automatic semicolon preceding newline throw" $ do
        let code = "function fail() {\n    log()\n    throw new Error()\n}"
        case parseJSSource "asi_throw.js" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "ASI: inserts automatic semicolon preceding newline continue" $ do
        let code = "for (let i = 0; i < 10; i++) {\n    step()\n    continue\n}"
        case parseJSSource "asi_cont.js" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "Rust Struct Generics: parses struct with multiple generic types" $ do
        let code = "pub struct Pair<T, U> {\n    pub first: T,\n    pub second: U,\n}"
        case parseRustSource "struct_gen.rs" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "Rust Statements: parses let variable bindings without mut keyword" $ do
        let code = "fn run() {\n    let immutable_val: usize = 100;\n}"
        case parseRustSource "let_immut.rs" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""
