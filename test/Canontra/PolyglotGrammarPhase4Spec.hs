{-# LANGUAGE OverloadedStrings #-}
{- |
Module      : Canontra.PolyglotGrammarPhase4Spec
Description : Conformance test suite for Phase 4: Exhaustive Polyglot Grammar Conformance & Soundness.

Covers:
  - Step 4.1: Python PEP 701 nested f-strings with quote reuse, PEP 695 type parameter syntax,
              and walrus scope hoisting across all comprehension variants.
  - Step 4.2: TypeScript 5.2 `using` and `await using` disposal CFG blocks with exceptional edges,
              and strict two-token lookahead regex vs division disambiguation.
  - Step 4.3: Go 1.21+ builtins (`min`, `max`, `clear`), tilde constraint sets (`~T`),
              commutative normalization, and cyclic struct recursion breaking.
  - Step 4.4: Rust Generic Associated Types (GATs) lifetime canonicalization,
              and raw identifier interning (`r#type` == `type`).
-}
module Canontra.PolyglotGrammarPhase4Spec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Canontra.Analysis.CFG
  ( BranchCondition (..)
  , CFGEdge (..)
  , ControlFlowGraph (..)
  , buildCFGs
  )
import Canontra.Analysis.DFG
  ( DFGNode (..)
  , DataFlowGraph (..)
  , DefUseKind (..)
  , buildDFGs
  )
import Canontra.Analysis.Scope (SymbolBinding (..), allBindings, analyzeProgramScope)
import Canontra.Analysis.TypeContract
  ( InterfaceContract (..)
  , StructuralType (..)
  , extractTypeContracts
  , parseTypeString
  )
import Canontra.Fingerprint.Structural (computeF1)
import Canontra.IR.Declaration (Declaration (..), Function (..))
import Canontra.IR.Program (Module (..), Program (..))
import Canontra.Parser.Go (parseGoSource)
import Canontra.Parser.JS (JSToken (..), parseJSSource, tokenizeJS)
import Canontra.Parser.Python (parsePythonSource)
import Canontra.Parser.Rust (parseRustSource)
import Canontra.Parser.SwissTable
  ( emptySwissTable
  , swissInternBS
  , swissLookupBS
  )
import Canontra.Types (unFingerprint)

spec :: Spec
spec = do
  describe "Phase 4: Exhaustive Polyglot Grammar Conformance & Soundness" $ do

    -- =========================================================================
    -- Step 4.1: Python PEP 701, PEP 695, and Walrus Scope Hoisting
    -- =========================================================================
    describe "Step 4.1: Python PEP 701, PEP 695 & Walrus Scope Hoisting" $ do

      it "PEP 701: parses 3-level deeply nested f-strings with quote reuse" $ do
        let code = "msg = f\"level1 {f'level2 {f\"level3 {var}\"}'}\""
        case parsePythonSource "fstring_nest3.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "PEP 701: parses nested f-strings containing inline comments inside expression" $ do
        let code = "msg = f\"result: {x # compute total\n + 10}\""
        case parsePythonSource "fstring_comment.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "PEP 701: parses triple-quoted f-strings with quote reuse in expressions" $ do
        let code = "msg = f\"\"\"outer {f'''inner {val}'''} string\"\"\""
        case parsePythonSource "fstring_triple.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "PEP 695: parses generic type alias statements (type Vector[T: (int, float)] = list[T])" $ do
        let code = "type Vector[T: (int, float)] = list[T]\n"
        case parsePythonSource "type_alias.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let decls = concatMap modDeclarations (progModules prog)
            case decls of
              [DeclTypeAlias name _] -> name `shouldBe` "Vector"
              other -> expectationFailure ("Expected DeclTypeAlias, got: " ++ show (length other))

      it "PEP 695: parses generic functions with type parameter clauses (def func[T, **P](x: T) -> T:)" $ do
        let code = "def func[T, **P](x: T) -> T:\n    return x\n"
        case parsePythonSource "pep695_fn.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let decls = concatMap modDeclarations (progModules prog)
            case decls of
              [DeclFunction fn] -> fnName fn `shouldBe` "func"
              other -> expectationFailure ("Expected DeclFunction, got: " ++ show (length other))

      it "PEP 695: parses generic classes with type parameter clauses (class Store[Key, Value]:)" $ do
        let code = "class Store[Key, Value]:\n    pass\n"
        case parsePythonSource "pep695_cls.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "PEP 572: hoists walrus bindings from list comprehensions to enclosing function scope" $ do
        let code = "def process(items):\n    return [y for x in items if (y := x * 2)]\n"
        case parsePythonSource "walrus_list.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let scopes = analyzeProgramScope prog
            any (\b -> symName b == "y") (concatMap allBindings scopes) `shouldBe` True

      it "PEP 572: hoists walrus bindings from dict comprehensions" $ do
        let code = "def dict_comp(items):\n    return {k: v for x in items if (k := str(x)) and (v := x * 10)}\n"
        case parsePythonSource "walrus_dict.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let scopes = analyzeProgramScope prog
            let bindings = concatMap allBindings scopes
            any (\b -> symName b == "k") bindings `shouldBe` True
            any (\b -> symName b == "v") bindings `shouldBe` True

      it "PEP 572: hoists walrus bindings from generator expressions" $ do
        let code = "def gen_comp(items):\n    return sum(y for x in items if (y := x * 3))\n"
        case parsePythonSource "walrus_gen.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let scopes = analyzeProgramScope prog
            any (\b -> symName b == "y") (concatMap allBindings scopes) `shouldBe` True

      it "PEP 572 DFG: tracks walrus operator target definition in DataFlowGraph" $ do
        let code = "def calc(items):\n    res = [z for x in items if (z := x + 1)]\n    return z\n"
        case parsePythonSource "walrus_dfg.py" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let dfgs = buildDFGs prog
            let hasZ = any (\n -> dfgKind n == DefAssignment "z") (concatMap dfgNodes dfgs)
            hasZ `shouldBe` True

    -- =========================================================================
    -- Step 4.2: TypeScript 5.2 Explicit Resource Management & Lookahead Regex
    -- =========================================================================
    describe "Step 4.2: TypeScript 5.2 Explicit Resource Management & Disambiguation" $ do

      it "TS 5.2: parses synchronous 'using' variable declarations" $ do
        let code = "function handle() {\n    using file = openFile('log.txt');\n    file.write('data');\n}"
        case parseJSSource "using_sync.ts" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "TS 5.2: parses asynchronous 'await using' variable declarations" $ do
        let code = "async function run() {\n    await using client = connectDb();\n    return client.query();\n}"
        case parseJSSource "using_async.ts" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "TS 5.2 CFG: synthesizes synthetic cleanup exit blocks and exceptional edges" $ do
        let code = "function exec() {\n    using res = acquireResource();\n    doWork(res);\n}"
        case parseJSSource "using_cfg.ts" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let cfgs = buildCFGs prog
            case cfgs of
              [cfg] -> do
                -- Should have at least entry, body, and cleanup blocks
                length (cfgBlocks cfg) `shouldSatisfy` (>= 3)
                -- Should have an exceptional edge jumping to the cleanup block
                let hasExceptEdge = any (\e -> edgeCondition e == CondException "*") (cfgEdges cfg)
                hasExceptEdge `shouldBe` True
              other -> expectationFailure ("Expected 1 CFG, got: " ++ show (length other))

      it "Context-Aware Lexer: distinguishes regex following closing brace '}'" $ do
        let code = "if (true) { cleanup(); } /pattern/g.test(str);"
        let tokens = tokenizeJS code
        -- Should identify TokStr for regex, not TokSymbol "/"
        let hasRegex = any (\tok -> case tok of
                                      TokStr s -> T.isPrefixOf "/pattern/" s
                                      _        -> False) tokens
        hasRegex `shouldBe` True

      it "Context-Aware Lexer: distinguishes division operator following closing brace '}'" $ do
        let code = "const obj = { a: 1 }; const half = { b: 2 } / 2;"
        let tokens = tokenizeJS code
        -- Should identify TokSymbol "/" for division
        let hasDiv = any (\tok -> case tok of
                                    TokSymbol "/" -> True
                                    _             -> False) tokens
        hasDiv `shouldBe` True

    -- =========================================================================
    -- Step 4.3: Go 1.21+ Builtins, Tilde Constraint Sets & Cyclic Structs
    -- =========================================================================
    describe "Step 4.3: Go 1.21+ Builtins, Tilde Constraints & Cyclic Structs" $ do

      it "Go 1.21+: parses min, max, and clear builtins in functions" $ do
        let code = T.unlines
              [ "package main"
              , "func compute(a, b int, m map[string]int) int {"
              , "    x := min(a, b)"
              , "    y := max(a, b)"
              , "    clear(m)"
              , "    return x + y"
              , "}"
              ]
        case parseGoSource "builtins.go" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "Go Generics: parses tilde constraint sets in interface definitions (~int | ~float64)" $ do
        let code = T.unlines
              [ "package main"
              , "type Number interface {"
              , "    ~int | ~int64 | ~float64"
              , "}"
              ]
        case parseGoSource "tilde_iface.go" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let ifaces = extractTypeContracts prog
            case ifaces of
              (iface:_) -> null (icFields iface) `shouldBe` False
              []        -> expectationFailure "Expected at least 1 interface contract"

      it "Go Generics: guarantees commutative normalization for tilde constraint sets" $ do
        let t1 = parseTypeString "~int | ~float64"
        let t2 = parseTypeString "~float64 | ~int"
        t1 `shouldBe` t2

      it "Go Generics: breaks cyclic struct recursion producing TypeRecVar 0" $ do
        let code = T.unlines
              [ "package main"
              , "type Node struct {"
              , "    Value int"
              , "    Next *Node"
              , "}"
              ]
        case parseGoSource "cyclic_struct.go" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let ifaces = extractTypeContracts prog
            case ifaces of
              (iface:_) -> do
                let fields = icFields iface
                lookup "Next" fields `shouldBe` Just (TypeRecVar 0)
              []        -> expectationFailure "Expected at least 1 interface contract"

    -- =========================================================================
    -- Step 4.4: Rust Generic Associated Types (GATs) & Raw Identifiers
    -- =========================================================================
    describe "Step 4.4: Rust GATs & Raw Identifiers" $ do

      it "Rust GATs: parses trait with Generic Associated Types (type Item<'a>;)" $ do
        let code = T.unlines
              [ "pub trait StreamingIterator {"
              , "    type Item<'a>;"
              , "    fn next<'a>(&'a mut self) -> Option<Self::Item<'a>>;"
              , "}"
              ]
        case parseRustSource "gat_trait.rs" code of
          Left err -> expectationFailure (show err)
          Right prog -> unFingerprint (computeF1 prog) `shouldNotBe` ""

      it "Rust Raw Identifiers: parses r# keywords as valid identifiers without r# prefix" $ do
        let code = T.unlines
              [ "fn r#match(r#type: i32) -> i32 {"
              , "    let r#fn = r#type + 1;"
              , "    r#fn"
              , "}"
              ]
        case parseRustSource "raw_ident.rs" code of
          Left err -> expectationFailure (show err)
          Right prog -> do
            let decls = concatMap modDeclarations (progModules prog)
            case decls of
              [DeclFunction fn] -> fnName fn `shouldBe` "match"
              other -> expectationFailure ("Expected 1 DeclFunction, got: " ++ show (length other))

      it "Rust SwissTable Interning: interns 'r#type' and 'type' to identical SymbolId" $ do
        let tbl0 = emptySwissTable 32
        let (id1, tbl1) = swissInternBS tbl0 "type"
        let (id2, tbl2) = swissInternBS tbl1 "r#type"
        id1 `shouldBe` id2
        swissLookupBS tbl2 "r#type" `shouldBe` Just id1
        swissLookupBS tbl2 "type" `shouldBe` Just id1

      it "Rust SwissTable Interning: interns 'r#match' and 'match' to identical SymbolId" $ do
        let tbl0 = emptySwissTable 32
        let (id1, tbl1) = swissInternBS tbl0 "match"
        let (id2, _)    = swissInternBS tbl1 "r#match"
        id1 `shouldBe` id2
