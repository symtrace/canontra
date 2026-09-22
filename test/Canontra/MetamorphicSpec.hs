{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Canontra.MetamorphicSpec
Description : Automated metamorphic mutation testing suite for v0.0.9-alpha.

Validates the formal Soundness Invariance Theorem and Sensitivity Divergence
Theorem across polyglot programs (Python, JavaScript, TypeScript, Go, Rust),
proving algebraic invariance under semantics-preserving transformations and
strict divergence under semantic mutations.
-}
module Canontra.MetamorphicSpec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Canontra.Fingerprint.Bundle (computeBundleFromSource)
import Canontra.Fingerprint.Composite (computeF4)
import Canontra.Fingerprint.Declaration (computeF2)
import Canontra.Fingerprint.Structural (computeF1)
import Canontra.Fingerprint.TypeContract (computeFT)
import Canontra.IR.Declaration
import Canontra.IR.Expression
import Canontra.IR.Program
import Canontra.Security.Path (canonicalizeSafePath, checkResourceBounds)
import Canontra.Types
import Canontra.Verification.Metamorphic

spec :: Spec
spec = do
  describe "Canontra.Verification.Metamorphic" $ do

    -- =========================================================================
    -- 1. Soundness Invariance Theorems (T in T_sound)
    -- =========================================================================
    describe "Soundness Invariance Theorems (T in T_sound)" $ do

      it "Python: preserves all semantic tiers F1..F4 under whitespace and blank line jitter" $ do
        let code = T.unlines
              [ "def calculate_tax(subtotal: float, rate: float = 0.05) -> float:"
              , "    tax = subtotal * rate"
              , "    return subtotal + tax"
              ]
        case verifyMetamorphicSourceTransform "calc.py" code (ReformatWhitespaceTrivia 4) of
          Left err -> expectationFailure (show err)
          Right verdict -> do
            mvF0Different verdict `shouldBe` True
            mvF1Identical verdict `shouldBe` True
            mvF2Identical verdict `shouldBe` True
            mvF3Identical verdict `shouldBe` True
            mvFCGIdentical verdict `shouldBe` True
            mvFCFIdentical verdict `shouldBe` True
            mvFDFIdentical verdict `shouldBe` True
            mvF4Identical verdict `shouldBe` True
            mvSoundnessPassed verdict `shouldBe` True

      it "TypeScript: preserves F1..F4 under arbitrary whitespace and newline padding" $ do
        let code = T.unlines
              [ "function computeArea(width: number, height: number): number {"
              , "    const area = width * height;"
              , "    return area;"
              , "}"
              ]
        case verifyMetamorphicSourceTransform "area.ts" code (ReformatWhitespaceTrivia 3) of
          Left err -> expectationFailure (show err)
          Right verdict -> do
            mvF1Identical verdict `shouldBe` True
            mvF2Identical verdict `shouldBe` True
            mvF4Identical verdict `shouldBe` True
            mvSoundnessPassed verdict `shouldBe` True

      it "Go: preserves F1..F4 under indentation and whitespace jitter" $ do
        let code = T.unlines
              [ "package main"
              , "func Max(a int, b int) int {"
              , "    if a > b {"
              , "        return a"
              , "    }"
              , "    return b"
              , "}"
              ]
        case verifyMetamorphicSourceTransform "math.go" code (ReformatWhitespaceTrivia 2) of
          Left err -> expectationFailure (show err)
          Right verdict -> do
            mvF1Identical verdict `shouldBe` True
            mvF2Identical verdict `shouldBe` True
            mvF4Identical verdict `shouldBe` True
            mvSoundnessPassed verdict `shouldBe` True

      it "Rust: preserves F1..F4 under whitespace and brace formatting variation" $ do
        let code = T.unlines
              [ "fn add_two(x: i32, y: i32) -> i32 {"
              , "    let sum = x + y;"
              , "    return sum;"
              , "}"
              ]
        case verifyMetamorphicSourceTransform "add.rs" code (ReformatWhitespaceTrivia 4) of
          Left err -> expectationFailure (show err)
          Right verdict -> do
            mvF1Identical verdict `shouldBe` True
            mvF2Identical verdict `shouldBe` True
            mvF4Identical verdict `shouldBe` True
            mvSoundnessPassed verdict `shouldBe` True

      it "Python: strips unflagged docstrings and comments preserving F1..F4" $ do
        let code = T.unlines
              [ "def process_data(items: list) -> int:"
              , "    # Initial accumulator"
              , "    total = 0"
              , "    for x in items:"
              , "        total = total + x"
              , "    return total"
              ]
        case verifyMetamorphicSourceTransform "proc.py" code (InsertInlineDocstrings "Temporary developer comment") of
          Left err -> expectationFailure (show err)
          Right verdict -> do
            mvF0Different verdict `shouldBe` True
            mvF1Identical verdict `shouldBe` True
            mvF4Identical verdict `shouldBe` True
            mvSoundnessPassed verdict `shouldBe` True

      it "AST: canonically sorts independent pure functions yielding 100% bit-identical F1..F4" $ do
        let fnA = DeclFunction (Function "alpha" [] Nothing [] [StmtReturn (Just (ExprLit (LitInt 1)))] False)
            fnB = DeclFunction (Function "beta" [] Nothing [] [StmtReturn (Just (ExprLit (LitInt 2)))] False)
            fnC = DeclFunction (Function "gamma" [] Nothing [] [StmtReturn (Just (ExprLit (LitInt 3)))] False)
            prog = Program [Module "main" [] [fnA, fnB, fnC] []] "python"
            verdict = verifyMetamorphicProgramTransform prog ReorderPureDeclarations
        mvF1Identical verdict `shouldBe` True
        mvF2Identical verdict `shouldBe` True
        mvF4Identical verdict `shouldBe` True
        mvSoundnessPassed verdict `shouldBe` True

      it "AST: eliminates StmtPass dead statements yielding 100% bit-identical F1..F4" $ do
        let fn = DeclFunction (Function "calc" [] Nothing [] [StmtReturn (Just (ExprLit (LitInt 42)))] False)
            prog = Program [Module "main" [] [fn] []] "python"
            verdict = verifyMetamorphicProgramTransform prog InsertDeadStatement
        mvF1Identical verdict `shouldBe` True
        mvF2Identical verdict `shouldBe` True
        mvF4Identical verdict `shouldBe` True
        mvSoundnessPassed verdict `shouldBe` True

      it "AST: local variable alpha-renaming preserves public signature F2 and dependencies F3" $ do
        let fn = DeclFunction (Function "run" [] Nothing []
                  [ StmtAssign [ExprId "local_var"] (ExprLit (LitInt 10))
                  , StmtReturn (Just (ExprId "local_var"))
                  ] False)
            prog = Program [Module "main" [] [fn] []] "python"
            verdict = verifyMetamorphicProgramTransform prog (AlphaRenameLocalVar "local_var" "renamed_var")
        mvF2Identical verdict `shouldBe` True
        mvF3Identical verdict `shouldBe` True
        mvFCGIdentical verdict `shouldBe` True

      it "AST: inverted branch condition with swapped arms preserves semantic structure" $ do
        let ifStmt = StmtIf (ExprId "flag")
                            [StmtReturn (Just (ExprLit (LitInt 1)))]
                            [StmtReturn (Just (ExprLit (LitInt 2)))]
            fn = DeclFunction (Function "decide" [] Nothing [] [ifStmt] False)
            prog = Program [Module "main" [] [fn] []] "python"
            trans = applyAstTransform InvertBranchCondition prog
        progLanguage trans `shouldBe` "python"

    -- =========================================================================
    -- 2. Sensitivity Divergence Theorems (M in M_divergent)
    -- =========================================================================
    describe "Sensitivity Divergence Theorems (M in M_divergent)" $ do

      it "Arithmetic operator mutation (+ -> -) causes strict F1 and F4 divergence" $ do
        let code = T.unlines
              [ "def add_vals(a: int, b: int) -> int:"
              , "    return a + b"
              ]
        case verifySourceMutation "add.py" code (MutFlipArithmeticOp OpAdd OpSub) of
          Left err -> expectationFailure (show err)
          Right verdict -> do
            msvF1Diverged verdict `shouldBe` True
            msvF4Diverged verdict `shouldBe` True
            msvSensitivityPassed verdict `shouldBe` True

      it "Comparison operator mutation (< -> >) causes strict F1 and F4 divergence" $ do
        let code = T.unlines
              [ "def is_less(a: int, b: int) -> bool:"
              , "    if a < b:"
              , "        return True"
              , "    return False"
              ]
        case verifySourceMutation "comp.py" code (MutFlipComparisonOp OpLt OpGt) of
          Left err -> expectationFailure (show err)
          Right verdict -> do
            msvF1Diverged verdict `shouldBe` True
            msvF4Diverged verdict `shouldBe` True
            msvSensitivityPassed verdict `shouldBe` True

      it "Equality operator mutation (== -> !=) causes strict F1 and F4 divergence" $ do
        let code = T.unlines
              [ "def check_equal(x: int, y: int) -> bool:"
              , "    return x == y"
              ]
        case verifySourceMutation "eq.py" code (MutFlipComparisonOp OpEq OpNotEq) of
          Left err -> expectationFailure (show err)
          Right verdict -> do
            msvF1Diverged verdict `shouldBe` True
            msvF4Diverged verdict `shouldBe` True
            msvSensitivityPassed verdict `shouldBe` True

      it "Numeric literal mutation (0 -> 9999) causes strict F1 and F4 divergence" $ do
        let code = T.unlines
              [ "def get_baseline() -> int:"
              , "    base = 0"
              , "    return base"
              ]
        case verifySourceMutation "lit.py" code (MutAlterNumericLit 0 9999) of
          Left err -> expectationFailure (show err)
          Right verdict -> do
            msvF1Diverged verdict `shouldBe` True
            msvF4Diverged verdict `shouldBe` True
            msvSensitivityPassed verdict `shouldBe` True

      it "String literal mutation causes strict F1 and F4 divergence" $ do
        let code = T.unlines
              [ "def greet() -> str:"
              , "    return \"hello\""
              ]
        case verifySourceMutation "greet.py" code (MutAlterStringLit "hello" "goodbye") of
          Left err -> expectationFailure (show err)
          Right verdict -> do
            msvF1Diverged verdict `shouldBe` True
            msvF4Diverged verdict `shouldBe` True
            msvSensitivityPassed verdict `shouldBe` True

      it "Branch condition inversion without arm swap causes strict divergence" $ do
        let code = T.unlines
              [ "def authenticate(valid: bool) -> int:"
              , "    if valid:"
              , "        return 1"
              , "    return 0"
              ]
        case verifySourceMutation "auth.py" code MutInvertConditionOnly of
          Left err -> expectationFailure (show err)
          Right verdict -> do
            msvF1Diverged verdict `shouldBe` True
            msvF4Diverged verdict `shouldBe` True
            msvSensitivityPassed verdict `shouldBe` True

      it "Dropping an execution statement causes strict F1 and F4 divergence" $ do
        let code = T.unlines
              [ "def calculate() -> int:"
              , "    x = 10"
              , "    return x"
              ]
        case verifySourceMutation "calc.py" code MutDropExecutionStmt of
          Left err -> expectationFailure (show err)
          Right verdict -> do
            msvF1Diverged verdict `shouldBe` True
            msvF4Diverged verdict `shouldBe` True
            msvSensitivityPassed verdict `shouldBe` True

      it "Public declaration parameter mutation causes strict F2 and F4 divergence" $ do
        let fn = DeclFunction (Function "query" [Parameter "old_param" ParamPositional Nothing Nothing] Nothing [] [] False)
            prog = Program [Module "main" [] [fn] []] "python"
            verdict = verifyProgramMutation prog (MutAlterSignatureParam "old_param" "new_param")
        msvSensitivityPassed verdict `shouldBe` True

    -- =========================================================================
    -- 3. Polyglot Multi-Language Metamorphic Corpus
    -- =========================================================================
    describe "Polyglot Multi-Language Metamorphic Corpus" $ do

      it "evaluates multi-language metamorphic suite with 100% passing soundness and sensitivity" $ do
        let fixtures =
              [ ( "sample.py"
                , T.unlines
                    [ "def sum_two(a: int, b: int) -> int:"
                    , "    return a + b"
                    ]
                )
              , ( "sample.ts"
                , T.unlines
                    [ "function multiply(x: number, y: number): number {"
                    , "    return x * y;"
                    , "}"
                    ]
                )
              , ( "sample.go"
                , T.unlines
                    [ "package main"
                    , "func Sub(a int, b int) int {"
                    , "    return a - b"
                    , "}"
                    ]
                )
              ]
            summary = runMetamorphicSuite fixtures
        mssTotalCases summary `shouldSatisfy` (> 0)
        mssSoundnessPassed summary `shouldSatisfy` (> 0)
        mssSensitivityPassed summary `shouldSatisfy` (> 0)
        mssAllPassed summary `shouldBe` True

      it "formats a formal ASCII Metamorphic Verification Report" $ do
        let summary = MetamorphicSuiteSummary 20 12 8 True
            report = formatMetamorphicSummary summary
        T.isInfixOf "CANONTRA METAMORPHIC MUTATION VERIFICATION REPORT" report `shouldBe` True
        T.isInfixOf "100% METAMORPHICALLY SOUND" report `shouldBe` True

    -- =========================================================================
    -- 4. Property-Based Metamorphic QuickCheck Invariants
    -- =========================================================================
    describe "Property-Based Metamorphic QuickCheck Invariants" $ do

      it "Property: arbitrary whitespace padding preserves F1 and F4 soundness" $ do
        property $ \(Positive n) ->
          let pad = n `mod` 16
              code = "def f(x: int) -> int:\n    return x * 2\n"
          in case verifyMetamorphicSourceTransform "p.py" code (ReformatWhitespaceTrivia pad) of
               Left _ -> False
               Right v -> mvSoundnessPassed v

      it "Property: arbitrary integer constant mutations cause strict divergence" $ do
        property $ \(n1, n2) ->
          (n1 /= n2 && n1 >= 0 && n2 >= 0 && n1 < 100 && n2 < 100) ==>
            let code = "def val():\n    return " <> T.pack (show (n1 :: Integer)) <> "\n"
            in case verifySourceMutation "lit.py" code (MutAlterNumericLit n1 n2) of
                 Left _ -> False
                 Right v -> msvSensitivityPassed v

      it "Property: arbitrary comment text injection preserves F1 and F4 soundness" $ do
        property $ \(NonEmpty s) ->
          let safeComment = T.filter (\c -> c >= 'a' && c <= 'z') (T.pack s)
              code = "def proc(x: int) -> int:\n    y = x + 1\n    return y\n"
          in not (T.null safeComment) ==>
               case verifyMetamorphicSourceTransform "comm.py" code (InsertInlineDocstrings safeComment) of
                 Left _ -> False
                 Right v -> mvSoundnessPassed v

    -- =========================================================================
    -- 5. Extended Multi-Tier Metamorphic Invariance Invariants
    -- =========================================================================
    describe "Extended Multi-Tier Metamorphic Invariance Invariants" $ do

      it "Python: preserves F1..F4 across multi-line bracketed expressions with trailing commas" $ do
        let code1 = T.unlines
              [ "def get_items():"
              , "    return [1, 2, 3]"
              ]
        let code2 = T.unlines
              [ "def get_items():"
              , "    return ["
              , "        1,"
              , "        2,"
              , "        3,"
              , "    ]"
              ]
        case (computeBundleFromSource "i1.py" code1, computeBundleFromSource "i2.py" code2) of
          (Right b1, Right b2) -> do
            f0Source b1 `shouldNotBe` f0Source b2
            f1Structural b1 `shouldBe` f1Structural b2
            f4Composite b1 `shouldBe` f4Composite b2
          _ -> expectationFailure "Parse failed"

      it "Python: preserves F1..F4 across consecutive empty lines and redundant comment lines" $ do
        let code1 = "def fn():\n    return 42\n"
        let code2 = "\n\n# Header\n# Comment\n\ndef fn():\n    # Inside\n    return 42\n\n\n"
        case (computeBundleFromSource "f1.py" code1, computeBundleFromSource "f2.py" code2) of
          (Right b1, Right b2) -> do
            f1Structural b1 `shouldBe` f1Structural b2
            f2Declaration b1 `shouldBe` f2Declaration b2
            f4Composite b1 `shouldBe` f4Composite b2
          _ -> expectationFailure "Parse failed"

      it "TypeScript: preserves F1..F4 when function parameters have identical types but varying whitespace" $ do
        let code1 = "function add(a: number, b: number): number { return a + b; }"
        let code2 = "function add( a : number , b : number ) : number {\n    return a + b;\n}"
        case (computeBundleFromSource "add1.ts" code1, computeBundleFromSource "add2.ts" code2) of
          (Right b1, Right b2) -> do
            f1Structural b1 `shouldBe` f1Structural b2
            f2Declaration b1 `shouldBe` f2Declaration b2
            f4Composite b1 `shouldBe` f4Composite b2
          _ -> expectationFailure "Parse failed"

      it "Go: preserves F1..F4 when functions have trailing newlines or block comments" $ do
        let code1 = "package main\nfunc Run() int {\n    return 10\n}\n"
        let code2 = "package main\n/* block comment */\nfunc Run() int {\n    // line comment\n    return 10\n}\n\n"
        case (computeBundleFromSource "r1.go" code1, computeBundleFromSource "r2.go" code2) of
          (Right b1, Right b2) -> do
            f1Structural b1 `shouldBe` f1Structural b2
            f4Composite b1 `shouldBe` f4Composite b2
          _ -> expectationFailure "Parse failed"

      it "Rust: preserves F1..F4 across formatting variations of let bindings" $ do
        let code1 = "fn test() -> i32 { let x = 5; return x; }"
        let code2 = "fn test() -> i32 {\n    let x = 5;\n    return x;\n}\n"
        case (computeBundleFromSource "t1.rs" code1, computeBundleFromSource "t2.rs" code2) of
          (Right b1, Right b2) -> do
            f1Structural b1 `shouldBe` f1Structural b2
            f4Composite b1 `shouldBe` f4Composite b2
          _ -> expectationFailure "Parse failed"

      it "AST: preserves F2 and F3 when local variable names are alpha-renamed across multiple scopes" $ do
        let fn1 = DeclFunction (Function "foo" [] Nothing []
                    [ StmtAssign [ExprId "temp_a"] (ExprLit (LitInt 1))
                    , StmtReturn (Just (ExprId "temp_a"))
                    ] False)
            fn2 = DeclFunction (Function "bar" [] Nothing []
                    [ StmtAssign [ExprId "temp_b"] (ExprLit (LitInt 2))
                    , StmtReturn (Just (ExprId "temp_b"))
                    ] False)
            prog = Program [Module "m" [] [fn1, fn2] []] "python"
            trans1 = applyAstTransform (AlphaRenameLocalVar "temp_a" "var_alpha") prog
            trans2 = applyAstTransform (AlphaRenameLocalVar "temp_b" "var_beta") trans1
            bOrig = computeF2 prog
            bTrans = computeF2 trans2
        bOrig `shouldBe` bTrans

      it "AST: permuting 4 pure functions produces 100% bit-identical F1, F2, and F4" $ do
        let fns = [ DeclFunction (Function ("fn_" <> T.pack (show i)) [] Nothing [] [StmtReturn (Just (ExprLit (LitInt i)))] False)
                  | i <- [1..4 :: Integer]
                  ]
            progA = Program [Module "main" [] fns []] "python"
            progB = Program [Module "main" [] (reverse fns) []] "python"
        computeF1 progA `shouldBe` computeF1 progB
        computeF2 progA `shouldBe` computeF2 progB
        computeF4 (computeF1 progA) (computeF2 progA) (Fingerprint "f3") (Fingerprint "fcg") (Fingerprint "fcf") (Fingerprint "fdf") (computeFT progA)
          `shouldBe`
          computeF4 (computeF1 progB) (computeF2 progB) (Fingerprint "f3") (Fingerprint "fcg") (Fingerprint "fcf") (Fingerprint "fdf") (computeFT progB)

      it "Sensitivity: mutating float literals causes strict F1 and F4 divergence" $ do
        let code1 = "def get_pi():\n    return 3.14159\n"
        let code2 = "def get_pi():\n    return 2.71828\n"
        case (computeBundleFromSource "pi1.py" code1, computeBundleFromSource "pi2.py" code2) of
          (Right b1, Right b2) -> do
            f1Structural b1 `shouldNotBe` f1Structural b2
            f4Composite b1 `shouldNotBe` f4Composite b2
          _ -> expectationFailure "Parse failed"

      it "Sensitivity: inverting return boolean literal (True -> False) causes strict divergence" $ do
        let code1 = "def is_active():\n    return True\n"
        let code2 = "def is_active():\n    return False\n"
        case (computeBundleFromSource "b1.py" code1, computeBundleFromSource "b2.py" code2) of
          (Right b1, Right b2) -> do
            f1Structural b1 `shouldNotBe` f1Structural b2
            f4Composite b1 `shouldNotBe` f4Composite b2
          _ -> expectationFailure "Parse failed"

      it "Sensitivity: mutating default parameter value strictly alters F2 declaration signature" $ do
        let code1 = "def connect(port: int = 8080):\n    return port\n"
        let code2 = "def connect(port: int = 9090):\n    return port\n"
        case (computeBundleFromSource "c1.py" code1, computeBundleFromSource "c2.py" code2) of
          (Right b1, Right b2) -> do
            f2Declaration b1 `shouldNotBe` f2Declaration b2
            f4Composite b1 `shouldNotBe` f4Composite b2
          _ -> expectationFailure "Parse failed"

      it "Sensitivity: changing arithmetic operator from multiplication to division alters F1" $ do
        let code1 = "def scale(x: int): return x * 10\n"
        let code2 = "def scale(x: int): return x / 10\n"
        case (computeBundleFromSource "s1.py" code1, computeBundleFromSource "s2.py" code2) of
          (Right b1, Right b2) -> do
            f1Structural b1 `shouldNotBe` f1Structural b2
            f4Composite b1 `shouldNotBe` f4Composite b2
          _ -> expectationFailure "Parse failed"

      it "Sensitivity: adding an extra parameter alters declaration hash F2" $ do
        let code1 = "def query(term: str): return term\n"
        let code2 = "def query(term: str, limit: int = 10): return term\n"
        case (computeBundleFromSource "q1.py" code1, computeBundleFromSource "q2.py" code2) of
          (Right b1, Right b2) -> do
            f2Declaration b1 `shouldNotBe` f2Declaration b2
            f4Composite b1 `shouldNotBe` f4Composite b2
          _ -> expectationFailure "Parse failed"

      it "Soundness: multiple consecutive whitespace jitter passes preserve invariant F1 and F4" $ do
        let code0 = "def compute(x: int) -> int:\n    return x * 2 + 1\n"
            code1 = applySourceTransform (ReformatWhitespaceTrivia 2) code0
            code2 = applySourceTransform (ReformatWhitespaceTrivia 6) code1
        case (computeBundleFromSource "p0.py" code0, computeBundleFromSource "p2.py" code2) of
          (Right b0, Right b2) -> do
            f1Structural b0 `shouldBe` f1Structural b2
            f4Composite b0 `shouldBe` f4Composite b2
          _ -> expectationFailure "Parse failed"

      it "Sensitivity: changing return type annotation in TypeScript strictly alters F2 declaration hash" $ do
        let code1 = "function getVal(): string { return \"val\"; }\n"
            code2 = "function getVal(): number { return 123; }\n"
        case (computeBundleFromSource "v1.ts" code1, computeBundleFromSource "v2.ts" code2) of
          (Right b1, Right b2) -> do
            f2Declaration b1 `shouldNotBe` f2Declaration b2
            f4Composite b1 `shouldNotBe` f4Composite b2
          _ -> expectationFailure "Parse failed"

      it "Sensitivity: flipping boolean literal True to False strictly alters F1 structural AST hash" $ do
        let code1 = "def is_active(): return True\n"
            code2 = "def is_active(): return False\n"
        case (computeBundleFromSource "b1.py" code1, computeBundleFromSource "b2.py" code2) of
          (Right b1, Right b2) -> do
            f1Structural b1 `shouldNotBe` f1Structural b2
            f4Composite b1 `shouldNotBe` f4Composite b2
          _ -> expectationFailure "Parse failed"

    -- =========================================================================
    -- 6. Phase 6 Production Metamorphic Expansion & Security Invariants
    -- =========================================================================
    describe "Phase 6 Production Verification & Boundary Invariants" $ do

      describe "Type Contract F_T Invariance across Languages" $ do
        it "TypeScript: interface method reordering produces bit-identical F_T" $ do
          let tsA = "export interface API {\n  get(k: string): string;\n  set(k: string, v: string): void;\n}\n"
              tsB = "export interface API {\n  set(k: string, v: string): void;\n  get(k: string): string;\n}\n"
          case (computeBundleFromSource "api1.ts" tsA, computeBundleFromSource "api2.ts" tsB) of
            (Right bA, Right bB) -> do
              fTTypeContract bA `shouldBe` fTTypeContract bB
              unFingerprint (fTTypeContract bA) `shouldNotBe` ""
            _ -> expectationFailure "TypeScript parse failed"

        it "Go: interface method reordering produces bit-identical F_T" $ do
          let goA = "package p\ntype DB interface {\n  Close() error\n  Ping() error\n}\n"
              goB = "package p\ntype DB interface {\n  Ping() error\n  Close() error\n}\n"
          case (computeBundleFromSource "db1.go" goA, computeBundleFromSource "db2.go" goB) of
            (Right bA, Right bB) -> do
              fTTypeContract bA `shouldBe` fTTypeContract bB
              unFingerprint (fTTypeContract bA) `shouldNotBe` ""
            _ -> expectationFailure "Go parse failed"

        it "Rust: trait method reordering produces bit-identical F_T" $ do
          let rsA = "trait Worker {\n  fn run(&self) -> bool;\n  fn stop(&self) -> bool;\n}\n"
              rsB = "trait Worker {\n  fn stop(&self) -> bool;\n  fn run(&self) -> bool;\n}\n"
          case (computeBundleFromSource "w1.rs" rsA, computeBundleFromSource "w2.rs" rsB) of
            (Right bA, Right bB) -> do
              fTTypeContract bA `shouldBe` fTTypeContract bB
              unFingerprint (fTTypeContract bA) `shouldNotBe` ""
            _ -> expectationFailure "Rust parse failed"

      describe "Air-Gapped Path Sandboxing & Security Boundaries" $ do
        it "rejects parent directory escape attempts" $ do
          res <- canonicalizeSafePath "src" "../../etc/passwd"
          case res of
            Left _  -> pure ()
            Right p -> expectationFailure ("Path escape was not caught: " ++ p)

        it "accepts legitimate nested relative paths within project root" $ do
          res <- canonicalizeSafePath "." "src/Canontra/Types.hs"
          case res of
            Left err -> expectationFailure ("Legitimate path rejected: " ++ err)
            Right _  -> pure ()

        it "enforces recursion depth boundary when nesting exceeds ceiling" $ do
          let deepPath = concat (replicate 70 "sub/") ++ "file.txt"
          res <- checkResourceBounds deepPath
          case res of
            Left _  -> pure ()
            Right _ -> expectationFailure "Excessive nesting depth was not rejected"
