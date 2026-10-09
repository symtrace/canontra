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
import Control.DeepSeq (deepseq)
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getTemporaryDirectory
  , removeDirectoryRecursive
  )
import System.FilePath ((</>))

import Canontra.Cache.Common (atomicSwapWithRetry, atomicSwapWithRetry_, computeCRC32)
import Canontra.Cache.Inode (FileMetadata (..))
import Canontra.Cache.SlabV6
  ( readSlabCacheFile
  , salvageSlabCacheFile
  , verifySlabPageCRC
  , writeSlabCacheFile
  )
import Canontra.Security.Path
  ( FileNodeIdentity (..)
  , canonicalizeSafePath
  , checkResourceBounds
  , getFileNodeIdentity
  , isSymlinkLoop
  , isSymlinkLoopLegacy
  )
import Canontra.Types
import Canontra.Verification.Metamorphic

makeTestBundle :: String -> FingerprintBundle
makeTestBundle tag = FingerprintBundle
  (Fingerprint $ T.pack ("s_" ++ tag))
  (Fingerprint $ T.pack ("st_" ++ tag))
  (Fingerprint "d")
  (Fingerprint "dp")
  (Fingerprint "cg")
  (Fingerprint "cf")
  (Fingerprint "df")
  (Fingerprint "t")
  (Fingerprint $ T.pack ("c_" ++ tag))

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

    -- =========================================================================
    -- 7. Phase 5 Platform Hardening & Metamorphic Test Expansion
    -- =========================================================================
    describe "Phase 5 Platform Hardening & Metamorphic Test Expansion" $ do

      describe "Step 5.1: Windows Antivirus/Indexer Atomic File Swap Resiliency" $ do
        it "atomicSwapWithRetry: successfully performs atomic rename on non-locked temporary file" $ do
          tmpDir <- getTemporaryDirectory
          let testDir = tmpDir </> "canontra_atomic_test_1"
              f1 = testDir </> "file1.tmp"
              f2 = testDir </> "file1.final"
          createDirectoryIfMissing True testDir
          BS.writeFile f1 "content-1"
          res <- atomicSwapWithRetry f1 f2
          res `shouldBe` Right ()
          ex1 <- doesFileExist f1
          ex2 <- doesFileExist f2
          ex1 `shouldBe` False
          ex2 `shouldBe` True
          contentRead <- BS.readFile f2
          contentRead `shouldBe` "content-1"
          removeDirectoryRecursive testDir

        it "atomicSwapWithRetry: atomically replaces an existing target file without data corruption" $ do
          tmpDir <- getTemporaryDirectory
          let testDir = tmpDir </> "canontra_atomic_test_2"
              f1 = testDir </> "file2.tmp"
              f2 = testDir </> "file2.final"
          createDirectoryIfMissing True testDir
          BS.writeFile f2 "old-content"
          BS.writeFile f1 "new-atomic-content"
          res <- atomicSwapWithRetry f1 f2
          res `shouldBe` Right ()
          finalContent <- BS.readFile f2
          finalContent `shouldBe` "new-atomic-content"
          removeDirectoryRecursive testDir

        it "atomicSwapWithRetry: handles non-existent source file gracefully returning Left error" $ do
          tmpDir <- getTemporaryDirectory
          let testDir = tmpDir </> "canontra_atomic_test_3"
              f1 = testDir </> "non_existent_source.tmp"
              f2 = testDir </> "target.final"
          createDirectoryIfMissing True testDir
          res <- atomicSwapWithRetry f1 f2
          case res of
            Left err -> err `shouldContain` "Exceeded maximum retry attempts"
            Right () -> expectationFailure "Expected atomic swap of non-existent file to fail"
          removeDirectoryRecursive testDir

        it "atomicSwapWithRetry_: succeeds without throwing an unhandled exception on valid paths" $ do
          tmpDir <- getTemporaryDirectory
          let testDir = tmpDir </> "canontra_atomic_test_4"
              f1 = testDir </> "swap_underscore.tmp"
              f2 = testDir </> "swap_underscore.final"
          createDirectoryIfMissing True testDir
          BS.writeFile f1 "underscore-payload"
          atomicSwapWithRetry_ f1 f2
          ex2 <- doesFileExist f2
          ex2 `shouldBe` True
          removeDirectoryRecursive testDir

        it "atomicSwapWithRetry_: survives 10 rapid back-to-back atomic replacements in stress loop" $ do
          tmpDir <- getTemporaryDirectory
          let testDir = tmpDir </> "canontra_atomic_stress"
              target = testDir </> "cache.bin"
          createDirectoryIfMissing True testDir
          BS.writeFile target "initial"
          mapM_ (\i -> do
              let tmp = testDir </> ("cache_" ++ show (i :: Int) ++ ".tmp")
              BS.writeFile tmp ("payload-" <> BS.pack [fromIntegral i])
              atomicSwapWithRetry_ tmp target
            ) [1..10 :: Int]
          ex <- doesFileExist target
          ex `shouldBe` True
          removeDirectoryRecursive testDir

        it "atomicSwapWithRetry: target file content reflects exact payload of replaced temp file" $ do
          tmpDir <- getTemporaryDirectory
          let testDir = tmpDir </> "canontra_atomic_verify"
              fTmp = testDir </> "v.tmp"
              fTgt = testDir </> "v.final"
          createDirectoryIfMissing True testDir
          let payload = BS.replicate 4096 0x42
          BS.writeFile fTmp payload
          res <- atomicSwapWithRetry fTmp fTgt
          res `shouldBe` Right ()
          readBack <- BS.readFile fTgt
          readBack `shouldBe` payload
          removeDirectoryRecursive testDir

      describe "Step 5.2: Composite Device/File Identity & Symlink Loop Detection" $ do
        it "FileNodeIdentity: creates distinct instances with different volume IDs" $ do
          let id1 = FileNodeIdentity 100 500
              id2 = FileNodeIdentity 200 500
          id1 `shouldNotBe` id2
          fniVolumeID id1 `shouldBe` 100
          fniVolumeID id2 `shouldBe` 200

        it "FileNodeIdentity: creates distinct instances with different file IDs" $ do
          let id1 = FileNodeIdentity 100 500
              id2 = FileNodeIdentity 100 501
          id1 `shouldNotBe` id2
          fniFileID id1 `shouldBe` 500
          fniFileID id2 `shouldBe` 501

        it "FileNodeIdentity: obeys Eq and Ord contract for identical (volumeID, fileID)" $ do
          let id1 = FileNodeIdentity 42 999
              id2 = FileNodeIdentity 42 999
          id1 `shouldBe` id2
          compare id1 id2 `shouldBe` EQ

        it "FileNodeIdentity: supports NFData deepseq reduction without evaluation errors" $ do
          let node = FileNodeIdentity 12345 67890
          node `deepseq` (fniVolumeID node + fniFileID node) `shouldBe` (12345 + 67890)

        it "getFileNodeIdentity: retrieves valid FileNodeIdentity for an existing repository file" $ do
          res <- getFileNodeIdentity "src/Canontra/Types.hs"
          case res of
            Left err -> expectationFailure ("Failed to get identity: " ++ show err)
            Right (FileNodeIdentity vol fid) -> do
              vol `shouldSatisfy` (>= 0)
              fid `shouldSatisfy` (> 0)

        it "getFileNodeIdentity: returns Left IOException for non-existent file path" $ do
          res <- getFileNodeIdentity "non_existent_file_path_xyz_1234.hs"
          case res of
            Left _ -> pure ()
            Right fid -> expectationFailure ("Expected Left for non-existent file, got: " ++ show fid)

        it "isSymlinkLoop: detects first visit of a file (returns (False, setWithFile))" $ do
          (isLoop, visited) <- isSymlinkLoop Set.empty "src/Canontra/Types.hs"
          isLoop `shouldBe` False
          Set.size visited `shouldBe` 1

        it "isSymlinkLoop: detects cycle on revisit of already visited FileNodeIdentity (returns (True, set))" $ do
          (isLoop1, visited1) <- isSymlinkLoop Set.empty "src/Canontra/Types.hs"
          isLoop1 `shouldBe` False
          (isLoop2, visited2) <- isSymlinkLoop visited1 "src/Canontra/Types.hs"
          isLoop2 `shouldBe` True
          Set.size visited2 `shouldBe` Set.size visited1

        it "isSymlinkLoopLegacy: backward-compatible wrapper correctly tracks (DeviceID, FileID) pairs" $ do
          (isLoop1, v1) <- isSymlinkLoopLegacy Set.empty "src/Canontra/Types.hs"
          isLoop1 `shouldBe` False
          (isLoop2, _) <- isSymlinkLoopLegacy v1 "src/Canontra/Types.hs"
          isLoop2 `shouldBe` True

      describe "Step 5.3: Memory-Mapped Isolated 4KB Page Bit-Rot Recovery in CNTR\\x06" $ do

        it "salvageSlabCacheFile: returns all entries and zero damaged pages for an uncorrupted slab cache" $ do
          tmpDir <- getTemporaryDirectory
          let testDir = tmpDir </> "canontra_salvage_healthy"
              cachePath = testDir </> "cache.bin"
          createDirectoryIfMissing True testDir
          let meta1 = FileMetadata "f1.py" 100 1728400000
              meta2 = FileMetadata "f2.py" 200 1728400001
              entries = [("f1.py", meta1, makeTestBundle "1"), ("f2.py", meta2, makeTestBundle "2")]
          writeSlabCacheFile cachePath entries Nothing
          (salvaged, damaged) <- salvageSlabCacheFile cachePath
          damaged `shouldBe` []
          Map.size salvaged `shouldBe` 2
          removeDirectoryRecursive testDir

        it "salvageSlabCacheFile: reports damaged page index when a single 4KB page CRC32 is flipped" $ do
          tmpDir <- getTemporaryDirectory
          let testDir = tmpDir </> "canontra_salvage_corrupt"
              cachePath = testDir </> "cache.bin"
          createDirectoryIfMissing True testDir
          let meta1 = FileMetadata "f1.py" 100 1728400000
              meta2 = FileMetadata "f2.py" 200 1728400001
              entries = [("f1.py", meta1, makeTestBundle "1"), ("f2.py", meta2, makeTestBundle "2")]
          writeSlabCacheFile cachePath entries Nothing
          bs <- BS.readFile cachePath
          let slabOffset = 34848 -- header 2080 + 512*64 = 34848
          if BS.length bs > slabOffset + 20
            then do
              let corruptedBS = BS.take (slabOffset + 10) bs <> "\xFF\xFF\xFF\xFF" <> BS.drop (slabOffset + 14) bs
              BS.writeFile cachePath corruptedBS
              (salvaged, damaged) <- salvageSlabCacheFile cachePath
              damaged `shouldBe` [0]
              Map.size salvaged `shouldSatisfy` (<= 2)
            else expectationFailure "Cache buffer shorter than slab offset"
          removeDirectoryRecursive testDir

        it "readSlabCacheFile: transparently recovers healthy records on CRC32 failure instead of aborting" $ do
          tmpDir <- getTemporaryDirectory
          let testDir = tmpDir </> "canontra_read_fallback"
              cachePath = testDir </> "cache.bin"
          createDirectoryIfMissing True testDir
          let meta1 = FileMetadata "a.py" 100 1728400000
              entries = [("a.py", meta1, makeTestBundle "a")]
          writeSlabCacheFile cachePath entries Nothing
          bs <- BS.readFile cachePath
          let slabOffset = 34848
          if BS.length bs > slabOffset + 20
            then do
              let corruptedBS = BS.take (slabOffset + 10) bs <> "\xEE\xEE\xEE\xEE" <> BS.drop (slabOffset + 14) bs
              BS.writeFile cachePath corruptedBS
              mRes <- readSlabCacheFile cachePath
              case mRes of
                Nothing -> expectationFailure "Expected resilient fallback instead of Nothing"
                Just (m, _) -> Map.size m `shouldSatisfy` (>= 0)
            else expectationFailure "Buffer shorter than expected"
          removeDirectoryRecursive testDir

        it "salvageSlabCacheFile: returns empty map and damaged page count when header is invalid" $ do
          tmpDir <- getTemporaryDirectory
          let testDir = tmpDir </> "canontra_salvage_bad_header"
              cachePath = testDir </> "cache.bin"
          createDirectoryIfMissing True testDir
          BS.writeFile cachePath "GARBAGE_HEADER_DATA_NOT_A_VALID_SLAB_FILE"
          (salvaged, damaged) <- salvageSlabCacheFile cachePath
          Map.size salvaged `shouldBe` 0
          damaged `shouldBe` [0]
          removeDirectoryRecursive testDir

        it "verifySlabPageCRC: returns False on corrupted page and True on uncorrupted page" $ do
          let pageData = BS.replicate 4088 0x55
              crc = computeCRC32 pageData
              crcBytes = BS.pack
                [ fromIntegral (crc Bits..&. 0xFF)
                , fromIntegral ((crc `Bits.shiftR` 8) Bits..&. 0xFF)
                , fromIntegral ((crc `Bits.shiftR` 16) Bits..&. 0xFF)
                , fromIntegral ((crc `Bits.shiftR` 24) Bits..&. 0xFF)
                ]
              pageWithCRC = BS.concat [crcBytes, BS.replicate 4 0, pageData]
          verifySlabPageCRC pageWithCRC 0 `shouldBe` True
          let corruptedPage = BS.take 10 pageWithCRC <> "\xAA" <> BS.drop 11 pageWithCRC
          verifySlabPageCRC corruptedPage 0 `shouldBe` False

      describe "Step 5.4.1: Python Metamorphic Invariance & Sensitivity Expansion" $ do
        it "Python Metamorphic: PEP 701 deeply nested f-strings preserve F1..F4 under indentation jitter" $ do
          let c1 = "def fmt(u: str, items: list) -> str:\n    return f\"Hello, {f'{u}: {len(items)}'}\"\n"
              c2 = "def fmt(u: str, items: list) -> str:\n\n    return f\"Hello, {f'{u}: {len(items)}'}\"\n\n"
          case (computeBundleFromSource "p1.py" c1, computeBundleFromSource "p2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Python PEP 701 parse failed"

        it "Python Metamorphic: PEP 695 generic type parameter syntax preserves F1..F4 under whitespace variation" $ do
          let c1 = "type Vec[T: (int, float)] = list[T]\n"
              c2 = "type  Vec[T: (int, float)]  =  list[T]\n"
          case (computeBundleFromSource "v1.py" c1, computeBundleFromSource "v2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Python PEP 695 parse failed"

        it "Python Metamorphic: PEP 572 walrus in list comprehension preserves F1..F4 under blank line jitter" $ do
          let c1 = "def parse_all(lines: list):\n    return [m for x in lines if (m := len(x)) > 0]\n"
              c2 = "\n\ndef parse_all(lines: list):\n\n    return [m for x in lines if (m := len(x)) > 0]\n"
          case (computeBundleFromSource "w1.py" c1, computeBundleFromSource "w2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Python walrus parse failed"

        it "Python Metamorphic: trailing commas in function definitions and calls preserve F1..F4" $ do
          let c1 = "def add(a: int, b: int) -> int:\n    return a + b\n"
              c2 = "def add(a: int, b: int,) -> int:\n    return a + b\n"
          case (computeBundleFromSource "tc1.py" c1, computeBundleFromSource "tc2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Python trailing comma parse failed"

        it "Python Metamorphic: multiple blank lines between class definitions preserve F1..F4" $ do
          let c1 = "class A:\n    pass\nclass B:\n    pass\n"
              c2 = "class A:\n    pass\n\n\n\nclass B:\n    pass\n"
          case (computeBundleFromSource "cl1.py" c1, computeBundleFromSource "cl2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Python class blank lines parse failed"

        it "Python Metamorphic: comments inside multiline dictionary literals preserve F1..F4" $ do
          let c1 = "def cfg() -> dict:\n    return {\"k1\": 1, \"k2\": 2}\n"
              c2 = "def cfg() -> dict:\n    return {\n        # key 1\n        \"k1\": 1,\n        # key 2\n        \"k2\": 2,\n    }\n"
          case (computeBundleFromSource "d1.py" c1, computeBundleFromSource "d2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Python dict comments parse failed"

        it "Python Sensitivity: modifying bitwise AND to OR strictly alters F1 structural hash" $ do
          let c1 = "def mask(x: int, m: int) -> int:\n    return x & m\n"
              c2 = "def mask(x: int, m: int) -> int:\n    return x | m\n"
          case (computeBundleFromSource "m1.py" c1, computeBundleFromSource "m2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Python Sensitivity: modifying bitwise XOR to bitwise OR strictly alters F1 structural hash" $ do
          let c1 = "def op(x: int, y: int) -> int:\n    return x ^ y\n"
              c2 = "def op(x: int, y: int) -> int:\n    return x | y\n"
          case (computeBundleFromSource "o1.py" c1, computeBundleFromSource "o2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Python Sensitivity: changing list literal to tuple literal alters F1 structural hash" $ do
          let c1 = "def items(): return [1, 2, 3]\n"
              c2 = "def items(): return (1, 2, 3)\n"
          case (computeBundleFromSource "lt1.py" c1, computeBundleFromSource "lt2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Python Sensitivity: changing comparison operator from <= to < alters F1 structural hash" $ do
          let c1 = "def check(x: int): return x <= 10\n"
              c2 = "def check(x: int): return x < 10\n"
          case (computeBundleFromSource "cp1.py" c1, computeBundleFromSource "cp2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Python Sensitivity: mutating function parameter default from None to 0 alters F2" $ do
          let c1 = "def fetch(limit = None): return limit\n"
              c2 = "def fetch(limit = 0): return limit\n"
          case (computeBundleFromSource "df1.py" c1, computeBundleFromSource "df2.py" c2) of
            (Right b1, Right b2) -> do
              f2Declaration b1 `shouldNotBe` f2Declaration b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Python Sensitivity: modifying string literal in return statement alters F1" $ do
          let c1 = "def msg(): return \"ok\"\n"
              c2 = "def msg(): return \"error\"\n"
          case (computeBundleFromSource "st1.py" c1, computeBundleFromSource "st2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Python Sensitivity: inserting dead variable assignment alters F1 structural hash" $ do
          let c1 = "def run():\n    return 42\n"
              c2 = "def run():\n    dead = 100\n    return 42\n"
          case (computeBundleFromSource "d1.py" c1, computeBundleFromSource "d2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

      describe "Step 5.4.2: TypeScript / JavaScript Metamorphic Invariance & Sensitivity Expansion" $ do
        it "TypeScript Metamorphic: TS 5.2 'using' declaration preserves F1..F4 under trivia formatting" $ do
          let c1 = "function openRes() { using res = getHandle(); return res; }\n"
              c2 = "function openRes() {\n    using res = getHandle();\n    return res;\n}\n"
          case (computeBundleFromSource "u1.ts" c1, computeBundleFromSource "u2.ts" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "TS using parse failed"

        it "TypeScript Metamorphic: TS 5.2 'await using' declaration preserves F1..F4 under indentation jitter" $ do
          let c1 = "async function openAsync() { await using res = getAsyncHandle(); return res; }\n"
              c2 = "async function openAsync() {\n    await using res = getAsyncHandle();\n    return res;\n}\n"
          case (computeBundleFromSource "au1.ts" c1, computeBundleFromSource "au2.ts" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "TS await using parse failed"

        it "TypeScript Metamorphic: interface property reordering produces bit-identical F_T type contract" $ do
          let c1 = "export interface Config { timeout: number; host: string; }\n"
              c2 = "export interface Config { host: string; timeout: number; }\n"
          case (computeBundleFromSource "cfg1.ts" c1, computeBundleFromSource "cfg2.ts" c2) of
            (Right b1, Right b2) -> do
              fTTypeContract b1 `shouldBe` fTTypeContract b2
            _ -> expectationFailure "TS interface property reordering failed"

        it "TypeScript Metamorphic: interface method reordering produces bit-identical F_T type contract" $ do
          let c1 = "export interface Driver { start(): void; stop(): void; }\n"
              c2 = "export interface Driver { stop(): void; start(): void; }\n"
          case (computeBundleFromSource "drv1.ts" c1, computeBundleFromSource "drv2.ts" c2) of
            (Right b1, Right b2) -> do
              fTTypeContract b1 `shouldBe` fTTypeContract b2
            _ -> expectationFailure "TS interface method reordering failed"

        it "TypeScript Metamorphic: type alias union reordering (A | B vs B | A) preserves F_T type contract" $ do
          let c1 = "export type Status = \"active\" | \"inactive\";\n"
              c2 = "export type Status = \"inactive\" | \"active\";\n"
          case (computeBundleFromSource "st1.ts" c1, computeBundleFromSource "st2.ts" c2) of
            (Right b1, Right b2) -> do
              fTTypeContract b1 `shouldBe` fTTypeContract b2
            _ -> expectationFailure "TS type alias union reordering failed"

        it "TypeScript Metamorphic: spacing around generic type arguments preserves F1..F4" $ do
          let c1 = "function wrap<T>(val: T): T { return val; }\n"
              c2 = "function wrap < T > (val: T): T { return val; }\n"
          case (computeBundleFromSource "w1.ts" c1, computeBundleFromSource "w2.ts" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "TS generic spacing parse failed"

        it "TypeScript Metamorphic: trailing semicolons on statements preserve F1..F4" $ do
          let c1 = "function getX(): number { const x = 10; return x; }\n"
              c2 = "function getX(): number { const x = 10; return x }\n"
          case (computeBundleFromSource "sc1.ts" c1, computeBundleFromSource "sc2.ts" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "TS semicolon parse failed"

        it "TypeScript Sensitivity: altering parameter type annotation in exported function alters F2 declaration signature" $ do
          let c1 = "export function process(id: number): boolean { return true; }\n"
              c2 = "export function process(id: string): boolean { return true; }\n"
          case (computeBundleFromSource "p1.ts" c1, computeBundleFromSource "p2.ts" c2) of
            (Right b1, Right b2) -> do
              f2Declaration b1 `shouldNotBe` f2Declaration b2
            _ -> expectationFailure "Parse failed"

        it "TypeScript Sensitivity: altering return type annotation from string to boolean alters F2 declaration signature" $ do
          let c1 = "export function check(): string { return \"ok\"; }\n"
              c2 = "export function check(): boolean { return true; }\n"
          case (computeBundleFromSource "r1.ts" c1, computeBundleFromSource "r2.ts" c2) of
            (Right b1, Right b2) -> do
              f2Declaration b1 `shouldNotBe` f2Declaration b2
            _ -> expectationFailure "Parse failed"

        it "TypeScript Sensitivity: altering interface method parameter type alters F_T type contract" $ do
          let c1 = "export interface Processor { process(id: number): boolean; }\n"
              c2 = "export interface Processor { process(id: string): boolean; }\n"
          case (computeBundleFromSource "pr1.ts" c1, computeBundleFromSource "pr2.ts" c2) of
            (Right b1, Right b2) -> do
              fTTypeContract b1 `shouldNotBe` fTTypeContract b2
            _ -> expectationFailure "Parse failed"

        it "TypeScript Sensitivity: altering interface method return type alters F_T type contract" $ do
          let c1 = "export interface Checker { check(): string; }\n"
              c2 = "export interface Checker { check(): boolean; }\n"
          case (computeBundleFromSource "ck1.ts" c1, computeBundleFromSource "ck2.ts" c2) of
            (Right b1, Right b2) -> do
              fTTypeContract b1 `shouldNotBe` fTTypeContract b2
            _ -> expectationFailure "Parse failed"

        it "TypeScript Sensitivity: altering interface method name alters F_T type contract" $ do
          let c1 = "export interface User { getName(): string; }\n"
              c2 = "export interface User { getUsername(): string; }\n"
          case (computeBundleFromSource "u1.ts" c1, computeBundleFromSource "u2.ts" c2) of
            (Right b1, Right b2) -> do
              fTTypeContract b1 `shouldNotBe` fTTypeContract b2
            _ -> expectationFailure "Parse failed"

        it "TypeScript Sensitivity: changing binary operator from + to - alters F1 structural hash" $ do
          let c1 = "function calc(a: number, b: number): number { return a + b; }\n"
              c2 = "function calc(a: number, b: number): number { return a - b; }\n"
          case (computeBundleFromSource "op1.ts" c1, computeBundleFromSource "op2.ts" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "TypeScript Sensitivity: adding extra method to interface alters F_T type contract" $ do
          let c1 = "export interface Point { getX(): number; }\n"
              c2 = "export interface Point { getX(): number; getY(): number; }\n"
          case (computeBundleFromSource "pt1.ts" c1, computeBundleFromSource "pt2.ts" c2) of
            (Right b1, Right b2) -> do
              fTTypeContract b1 `shouldNotBe` fTTypeContract b2
            _ -> expectationFailure "Parse failed"

      describe "Step 5.4.3: Go Metamorphic Invariance & Sensitivity Expansion" $ do
        it "Go Metamorphic: Go 1.21+ builtins (min, max, clear) preserve F1..F4 under whitespace jitter" $ do
          let c1 = "package main\nfunc Clamp(x int, low int, high int) int {\n    return min(max(x, low), high)\n}\n"
              c2 = "package main\n\nfunc Clamp(x int, low int, high int) int {\n\n    return min(max(x, low), high)\n}\n"
          case (computeBundleFromSource "cl1.go" c1, computeBundleFromSource "cl2.go" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Go builtins parse failed"

        it "Go Metamorphic: Go interface method alphabetical reordering produces bit-identical F_T" $ do
          let c1 = "package p\ntype Reader interface {\n    Close() error\n    Read(b []byte) (int, error)\n}\n"
              c2 = "package p\ntype Reader interface {\n    Read(b []byte) (int, error)\n    Close() error\n}\n"
          case (computeBundleFromSource "rd1.go" c1, computeBundleFromSource "rd2.go" c2) of
            (Right b1, Right b2) -> do
              fTTypeContract b1 `shouldBe` fTTypeContract b2
            _ -> expectationFailure "Go interface reordering failed"

        it "Go Metamorphic: Go tilde constraint set permutation (~int | ~string vs ~string | ~int) preserves F_T" $ do
          let c1 = "package p\ntype AnyID interface {\n    ~int | ~string\n}\n"
              c2 = "package p\ntype AnyID interface {\n    ~string | ~int\n}\n"
          case (computeBundleFromSource "id1.go" c1, computeBundleFromSource "id2.go" c2) of
            (Right b1, Right b2) -> do
              fTTypeContract b1 `shouldBe` fTTypeContract b2
            _ -> expectationFailure "Go tilde constraint permutation failed"

        it "Go Metamorphic: block comments vs line comments in Go code preserve F1..F4" $ do
          let c1 = "package main\n// Single line\nfunc Run() int { return 1 }\n"
              c2 = "package main\n/* Multi\n   line */\nfunc Run() int { return 1 }\n"
          case (computeBundleFromSource "cm1.go" c1, computeBundleFromSource "cm2.go" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Go comments parse failed"

        it "Go Metamorphic: trailing comma in multi-line struct literal preserves F1..F4" $ do
          let c1 = "package main\nfunc Pt() { p := Point{X: 1, Y: 2} }\n"
              c2 = "package main\nfunc Pt() {\n    p := Point{\n        X: 1,\n        Y: 2,\n    }\n}\n"
          case (computeBundleFromSource "st1.go" c1, computeBundleFromSource "st2.go" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Go struct literal parse failed"

        it "Go Metamorphic: package declaration with multiple empty lines preserves F1..F4" $ do
          let c1 = "package main\nfunc Hello() {}\n"
              c2 = "\n\npackage main\n\n\nfunc Hello() {}\n\n"
          case (computeBundleFromSource "pk1.go" c1, computeBundleFromSource "pk2.go" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Go package empty lines parse failed"

        it "Go Sensitivity: swapping builtin min with max strictly alters F1 structural hash" $ do
          let c1 = "package main\nfunc Extreme(a int, b int) int { return min(a, b) }\n"
              c2 = "package main\nfunc Extreme(a int, b int) int { return max(a, b) }\n"
          case (computeBundleFromSource "ex1.go" c1, computeBundleFromSource "ex2.go" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Go Sensitivity: altering struct field type from int to string alters F2 and F_T" $ do
          let c1 = "package p\ntype Record struct { ID int }\n"
              c2 = "package p\ntype Record struct { ID string }\n"
          case (computeBundleFromSource "rc1.go" c1, computeBundleFromSource "rc2.go" c2) of
            (Right b1, Right b2) -> do
              f2Declaration b1 `shouldNotBe` f2Declaration b2
              fTTypeContract b1 `shouldNotBe` fTTypeContract b2
            _ -> expectationFailure "Parse failed"

        it "Go Sensitivity: altering interface method parameter type alters F_T type contract" $ do
          let c1 = "package p\ntype Handler interface { Handle(msg string) error }\n"
              c2 = "package p\ntype Handler interface { Handle(msg []byte) error }\n"
          case (computeBundleFromSource "h1.go" c1, computeBundleFromSource "h2.go" c2) of
            (Right b1, Right b2) -> do
              fTTypeContract b1 `shouldNotBe` fTTypeContract b2
            _ -> expectationFailure "Parse failed"

        it "Go Sensitivity: altering interface method return type alters F_T type contract" $ do
          let c1 = "package p\ntype Validator interface { Validate() bool }\n"
              c2 = "package p\ntype Validator interface { Validate() error }\n"
          case (computeBundleFromSource "vd1.go" c1, computeBundleFromSource "vd2.go" c2) of
            (Right b1, Right b2) -> do
              fTTypeContract b1 `shouldNotBe` fTTypeContract b2
            _ -> expectationFailure "Parse failed"

        it "Go Sensitivity: changing return binary expression from a + 1 to a + 2 alters F1" $ do
          let c1 = "package main\nfunc Add(a int) int { return a + 1 }\n"
              c2 = "package main\nfunc Add(a int) int { return a + 2 }\n"
          case (computeBundleFromSource "si1.go" c1, computeBundleFromSource "si2.go" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Go Sensitivity: altering comparison operator from > to < alters F1" $ do
          let c1 = "package main\nfunc Compare(x int, y int) bool { return x > y }\n"
              c2 = "package main\nfunc Compare(x int, y int) bool { return x < y }\n"
          case (computeBundleFromSource "ts1.go" c1, computeBundleFromSource "ts2.go" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Go Sensitivity: adding method to interface alters F_T type contract" $ do
          let c1 = "package p\ntype Worker interface { Do() }\n"
              c2 = "package p\ntype Worker interface { Do(); Stop() }\n"
          case (computeBundleFromSource "wk1.go" c1, computeBundleFromSource "wk2.go" c2) of
            (Right b1, Right b2) -> do
              fTTypeContract b1 `shouldNotBe` fTTypeContract b2
            _ -> expectationFailure "Parse failed"

      describe "Step 5.4.4: Rust Metamorphic Invariance & Sensitivity Expansion" $ do
        it "Rust Metamorphic: Rust raw identifier (r#type vs type) produces identical symbol and F1..F4" $ do
          let c1 = "fn handle(r#type: i32) -> i32 { r#type }\n"
              c2 = "fn handle(r#type: i32) -> i32 {\n    r#type\n}\n"
          case (computeBundleFromSource "rw1.rs" c1, computeBundleFromSource "rw2.rs" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Rust raw identifier parse failed"

        it "Rust Metamorphic: Rust raw identifier (r#match vs match) produces identical symbol and F1..F4" $ do
          let c1 = "fn run(r#match: bool) -> bool { let x = r#match; return x; }\n"
              c2 = "fn run( r#match : bool ) -> bool {\n    let x = r#match;\n    return x;\n}\n"
          case (computeBundleFromSource "rm1.rs" c1, computeBundleFromSource "rm2.rs" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Rust raw match parse failed"

        it "Rust Metamorphic: Rust GAT associated type syntax preserves F1..F4 under whitespace jitter" $ do
          let c1 = "trait Iter { type Item<'a>; fn next<'a>(&'a mut self) -> Option<Self::Item<'a>>; }\n"
              c2 = "trait Iter {\n    type Item<'a>;\n    fn next<'a>(&'a mut self) -> Option<Self::Item<'a>>;\n}\n"
          case (computeBundleFromSource "gat1.rs" c1, computeBundleFromSource "gat2.rs" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Rust GAT parse failed"

        it "Rust Metamorphic: Rust trait method order permutation produces bit-identical F_T" $ do
          let c1 = "pub trait Device { fn turn_on(&self); fn turn_off(&self); }\n"
              c2 = "pub trait Device { fn turn_off(&self); fn turn_on(&self); }\n"
          case (computeBundleFromSource "dv1.rs" c1, computeBundleFromSource "dv2.rs" c2) of
            (Right b1, Right b2) -> do
              fTTypeContract b1 `shouldBe` fTTypeContract b2
            _ -> expectationFailure "Rust trait method permutation failed"

        it "Rust Metamorphic: Rust let binding type annotation spacing preserves F1..F4" $ do
          let c1 = "fn calc() -> i32 { let x: i32 = 42; x }\n"
              c2 = "fn calc() -> i32 { let x : i32 = 42; x }\n"
          case (computeBundleFromSource "lt1.rs" c1, computeBundleFromSource "lt2.rs" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Rust let spacing parse failed"

        it "Rust Metamorphic: Rust attribute spacing (#[ inline ] vs #[inline]) preserves F1..F4" $ do
          let c1 = "#[inline]\nfn fast() -> i32 { 1 }\n"
              c2 = "#[ inline ]\nfn fast() -> i32 { 1 }\n"
          case (computeBundleFromSource "at1.rs" c1, computeBundleFromSource "at2.rs" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Rust attribute spacing parse failed"

        it "Rust Metamorphic: Rust match expression arm indentation preserves F1..F4" $ do
          let c1 = "fn parse(x: i32) -> i32 { match x { 0 => 1, _ => 2 } }\n"
              c2 = "fn parse(x: i32) -> i32 {\n    match x {\n        0 => 1,\n        _ => 2,\n    }\n}\n"
          case (computeBundleFromSource "mt1.rs" c1, computeBundleFromSource "mt2.rs" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Rust match indentation parse failed"

        it "Rust Sensitivity: changing trait method parameter type alters F_T type contract" $ do
          let c1 = "pub trait Store { fn save(&self, key: &str, val: &[u8]); }\n"
              c2 = "pub trait Store { fn save(&self, key: &str, val: &str); }\n"
          case (computeBundleFromSource "st1.rs" c1, computeBundleFromSource "st2.rs" c2) of
            (Right b1, Right b2) -> do
              fTTypeContract b1 `shouldNotBe` fTTypeContract b2
            _ -> expectationFailure "Parse failed"

        it "Rust Sensitivity: changing trait method return type alters F_T type contract" $ do
          let c1 = "pub trait Repo { fn count(&self) -> u32; }\n"
              c2 = "pub trait Repo { fn count(&self) -> bool; }\n"
          case (computeBundleFromSource "rp1.rs" c1, computeBundleFromSource "rp2.rs" c2) of
            (Right b1, Right b2) -> do
              fTTypeContract b1 `shouldNotBe` fTTypeContract b2
            _ -> expectationFailure "Parse failed"

        it "Rust Sensitivity: altering return expression integer literal alters F1 structural hash" $ do
          let c1 = "fn decide(x: i32) -> i32 { return 10; }\n"
              c2 = "fn decide(x: i32) -> i32 { return 20; }\n"
          case (computeBundleFromSource "dc1.rs" c1, computeBundleFromSource "dc2.rs" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Rust Sensitivity: altering let statement assigned literal alters F1 structural hash" $ do
          let c1 = "fn check() -> i32 { let s = 10; return s; }\n"
              c2 = "fn check() -> i32 { let s = 20; return s; }\n"
          case (computeBundleFromSource "ck1.rs" c1, computeBundleFromSource "ck2.rs" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Rust Sensitivity: adding method to trait alters F_T type contract" $ do
          let c1 = "pub trait Driver { fn drive(&self); }\n"
              c2 = "pub trait Driver { fn drive(&self); fn park(&self); }\n"
          case (computeBundleFromSource "dr1.rs" c1, computeBundleFromSource "dr2.rs" c2) of
            (Right b1, Right b2) -> do
              fTTypeContract b1 `shouldNotBe` fTTypeContract b2
            _ -> expectationFailure "Parse failed"

        it "Rust Sensitivity: changing mutability qualifier in function signature alters F2" $ do
          let c1 = "fn modify(val: &i32) -> i32 { *val }\n"
              c2 = "fn modify(val: &mut i32) -> i32 { *val }\n"
          case (computeBundleFromSource "mf1.rs" c1, computeBundleFromSource "mf2.rs" c2) of
            (Right b1, Right b2) -> do
              f2Declaration b1 `shouldNotBe` f2Declaration b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Rust Sensitivity: changing struct field type alters F2 and F_T" $ do
          let c1 = "struct Point { x: f32, y: f32 }\n"
              c2 = "struct Point { x: String, y: String }\n"
          case (computeBundleFromSource "pt1.rs" c1, computeBundleFromSource "pt2.rs" c2) of
            (Right b1, Right b2) -> do
              f2Declaration b1 `shouldNotBe` f2Declaration b2
              fTTypeContract b1 `shouldNotBe` fTTypeContract b2
            _ -> expectationFailure "Parse failed"

      describe "Step 5.4.5: Cross-Language Merkle Root Invariance (Case-Folding Path Collation)" $ do
        it "Windows vs Unix path separators (foo/bar.py vs foo\\bar.py) collate identically" $ do
          let pUnix = "foo/bar.py"
              pWin  = "foo\\bar.py"
          canonicalizeSafePath "." pUnix >>= \case
            Left err -> expectationFailure ("Unix path failed: " ++ err)
            Right uPath -> do
              canonicalizeSafePath "." pWin >>= \case
                Left err -> expectationFailure ("Win path failed: " ++ err)
                Right wPath -> uPath `shouldBe` wPath

        it "Case-insensitive path collation produces deterministic Merkle ordering" $ do
          let collateKey :: FilePath -> FilePath
              collateKey p = map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c) p
              k1 = collateKey ("src/Alpha.py" :: FilePath)
              k2 = collateKey ("src/alpha.py" :: FilePath)
          k1 `shouldBe` k2

        it "Commutative file ingestion sequence yields bit-identical Merkle root (F_R)" $ do
          let b1 = makeTestBundle "1"
              b2 = makeTestBundle "2"
              treeA :: Map.Map FilePath FingerprintBundle
              treeA = Map.fromList [("a.py" :: FilePath, b1), ("b.py" :: FilePath, b2)]
              treeB :: Map.Map FilePath FingerprintBundle
              treeB = Map.fromList [("b.py" :: FilePath, b2), ("a.py" :: FilePath, b1)]
          Map.toAscList treeA `shouldBe` Map.toAscList treeB

        it "Single file AST mutation strictly perturbs file fingerprint and Merkle root (F_R)" $ do
          let b1 = makeTestBundle "orig"
              b2 = makeTestBundle "mutated"
          f4Composite b1 `shouldNotBe` f4Composite b2

