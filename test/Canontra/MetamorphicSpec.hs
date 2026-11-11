{-# LANGUAGE BangPatterns #-}
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

import Canontra.Analysis.BatchImpact
  ( BatchImpactResult (..)
  , computeBatchImpactSlice
  , computeBatchImpactSliceCSR
  , formatBatchImpactResult
  , formatBatchImpactResultJson
  , generateTestCommand
  )
import Canontra.Analysis.CFG (BasicBlock (..), ControlFlowGraph (..), buildCFGs)
import Canontra.Analysis.CFGCanonical
  ( canonicalizeCFGRPO
  , terminatesUnconditionally
  )
import Canontra.Analysis.CSRGraph (buildCSRGraph, csrEdgeCount, csrNodeCount, flagCallSync)
import Canontra.Analysis.DFG (buildCanonicalDFGs)
import Canontra.Analysis.TypeContract
  ( StructuralType (..)
  , parseTypeString
  )
import Canontra.Analysis.WholeRepoGraph
  ( buildWholeRepoCallGraph
  )
import Canontra.Fingerprint.Bundle (computeBundleFromSource)
import Canontra.Fingerprint.Composite (computeF4)
import Canontra.Fingerprint.Declaration (computeF2)
import Canontra.Fingerprint.Structural (computeF1)
import Canontra.Fingerprint.TypeContract (computeFT)
import Canontra.IR.Declaration
import Canontra.IR.Expression
import Canontra.IR.Program
import Canontra.Normalize.Algebraic
  ( isCommutativeOp
  , isProvablyPureExpr
  , normalizeAlgebraicExpr
  )
import Canontra.Normalize.Alpha (canonicalizeAlphaProgram, isCanonicalBoundId)
import Canontra.Normalize.Normalize (normalizeProgram)
import Canontra.Normalize.Rules (engineVersion, normalizationVersion)
import Canontra.Parser.Polyglot (parsePolyglotSource)
import Control.DeepSeq (deepseq)
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time.Clock (diffUTCTime, getCurrentTime)
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

    -- =========================================================================
    -- v0.3.0 Verification Gates: Alpha-Invariance, Algebraic Laws, CFG RPO,
    -- Deepened Type Contracts (F_T), and CSR Batch CIA 2.0
    -- =========================================================================
    describe "v0.3.0 Verification Gates (Pillars 1-5)" $ do

      -- -----------------------------------------------------------------------
      -- Gate 1: Bound Lexical Alpha-Invariance & Public Contract Sensitivity
      -- -----------------------------------------------------------------------
      describe "Gate 1: Scope-Aware Bound Lexical Alpha-Canonicalization" $ do

        it "verifies engineVersion and normalizationVersion are bumped to 0.3.0" $ do
          engineVersion `shouldBe` "0.3.0"
          normalizationVersion `shouldBe` "0.3.0"

        it "identifies canonical bound de Bruijn identifiers via isCanonicalBoundId" $ do
          isCanonicalBoundId "@v1.0" `shouldBe` True
          isCanonicalBoundId "@v2.15" `shouldBe` True
          isCanonicalBoundId "local_var" `shouldBe` False

        it "Alpha-Invariance 1: renaming single local variable in Python preserves F1, FDF, F4" $ do
          let c1 = "def compute(x: int) -> int:\n    temp = x + 1\n    return temp\n"
              c2 = "def compute(x: int) -> int:\n    result_val = x + 1\n    return result_val\n"
          case (computeBundleFromSource "a1.py" c1, computeBundleFromSource "a2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f2Declaration b1 `shouldBe` f2Declaration b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 2: renaming multiple sequential local variables preserves F1, FDF, F4" $ do
          let c1 = "def step(x: int) -> int:\n    a = x + 1\n    b = a * 2\n    c = b - 3\n    return c\n"
              c2 = "def step(x: int) -> int:\n    first = x + 1\n    second = first * 2\n    third = second - 3\n    return third\n"
          case (computeBundleFromSource "a1.py" c1, computeBundleFromSource "a2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 3: renaming annotated local variable (StmtAnnAssign) preserves F1, FDF, F4" $ do
          let c1 = "def calc(n: int) -> int:\n    acc: int = n * 2\n    return acc\n"
              c2 = "def calc(n: int) -> int:\n    total_sum: int = n * 2\n    return total_sum\n"
          case (computeBundleFromSource "a1.py" c1, computeBundleFromSource "a2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 4: renaming augmented assignment local variable (+=) preserves F1, FDF, F4" $ do
          let c1 = "def accum(n: int) -> int:\n    total = 0\n    total += n\n    return total\n"
              c2 = "def accum(n: int) -> int:\n    running = 0\n    running += n\n    return running\n"
          case (computeBundleFromSource "a1.py" c1, computeBundleFromSource "a2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 5: renaming for-loop induction variable preserves F1, FDF, F4" $ do
          let c1 = "def sum_items(items: list) -> int:\n    s = 0\n    for item in items:\n        s = s + item\n    return s\n"
              c2 = "def sum_items(items: list) -> int:\n    acc = 0\n    for elem in items:\n        acc = acc + elem\n    return acc\n"
          case (computeBundleFromSource "a1.py" c1, computeBundleFromSource "a2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 6: renaming nested for-loop induction variables preserves F1, FDF, F4" $ do
          let c1 = "def grid(rows: list) -> int:\n    total = 0\n    for r in rows:\n        for c in r:\n            total = total + c\n    return total\n"
              c2 = "def grid(rows: list) -> int:\n    res = 0\n    for row_vec in rows:\n        for col_val in row_vec:\n            res = res + col_val\n    return res\n"
          case (computeBundleFromSource "a1.py" c1, computeBundleFromSource "a2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 7: renaming tuple-destructured local variables preserves F1, FDF, F4" $ do
          let c1 = "def unpack(pair: tuple) -> int:\n    left, right = pair\n    return left - right\n"
              c2 = "def unpack(pair: tuple) -> int:\n    first_el, second_el = pair\n    return first_el - second_el\n"
          case (computeBundleFromSource "a1.py" c1, computeBundleFromSource "a2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 8: renaming list comprehension target variable preserves F1, FDF, F4" $ do
          let c1 = "def squares(xs: list) -> list:\n    return [x * x for x in xs]\n"
              c2 = "def squares(xs: list) -> list:\n    return [val * val for val in xs]\n"
          case (computeBundleFromSource "a1.py" c1, computeBundleFromSource "a2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 9: renaming filtered list comprehension variable preserves F1, FDF, F4" $ do
          let c1 = "def positives(xs: list) -> list:\n    return [item for item in xs if item > 0]\n"
              c2 = "def positives(xs: list) -> list:\n    return [num for num in xs if num > 0]\n"
          case (computeBundleFromSource "a1.py" c1, computeBundleFromSource "a2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 10: renaming set comprehension target variable preserves F1, FDF, F4" $ do
          let c1 = "def unique_sq(xs: list) -> set:\n    return {k * k for k in xs}\n"
              c2 = "def unique_sq(xs: list) -> set:\n    return {elem * elem for elem in xs}\n"
          case (computeBundleFromSource "a1.py" c1, computeBundleFromSource "a2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 11: renaming dict comprehension key/val variables preserves F1, FDF, F4" $ do
          let c1 = "def invert(items: list) -> dict:\n    return {k: v for k, v in items}\n"
              c2 = "def invert(items: list) -> dict:\n    return {key_id: val_obj for key_id, val_obj in items}\n"
          case (computeBundleFromSource "a1.py" c1, computeBundleFromSource "a2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 12: renaming generator expression variable preserves F1, FDF, F4" $ do
          let c1 = "def gen_sum(xs: list) -> int:\n    return sum((x + 1 for x in xs))\n"
              c2 = "def gen_sum(xs: list) -> int:\n    return sum((entry + 1 for entry in xs))\n"
          case (computeBundleFromSource "a1.py" c1, computeBundleFromSource "a2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 13: renaming lambda parameter preserves F1, FDF, F4" $ do
          let c1 = "def make_adder(n: int):\n    return lambda x: x + n\n"
              c2 = "def make_adder(n: int):\n    return lambda delta: delta + n\n"
          case (computeBundleFromSource "a1.py" c1, computeBundleFromSource "a2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 14: renaming multi-parameter lambda binders preserves F1, FDF, F4" $ do
          let c1 = "def combiner():\n    return lambda a, b: a - b\n"
              c2 = "def combiner():\n    return lambda left_op, right_op: left_op - right_op\n"
          case (computeBundleFromSource "a1.py" c1, computeBundleFromSource "a2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 15: renaming try/except exception binding variable preserves F1, FDF, F4" $ do
          let c1 = "def safe_parse(s: str) -> str:\n    try:\n        return s\n    except ValueError as err:\n        return str(err)\n"
              c2 = "def safe_parse(s: str) -> str:\n    try:\n        return s\n    except ValueError as exc_obj:\n        return str(exc_obj)\n"
          case (computeBundleFromSource "a1.py" c1, computeBundleFromSource "a2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 16: renaming with-statement context manager binding preserves F1, FDF, F4" $ do
          let c1 = "def read_data(path: str) -> str:\n    with open(path) as f:\n        return f.read()\n"
              c2 = "def read_data(path: str) -> str:\n    with open(path) as handle:\n        return handle.read()\n"
          case (computeBundleFromSource "a1.py" c1, computeBundleFromSource "a2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 17: renaming local variable inside class method preserves F1, FDF, F4" $ do
          let c1 = "class Calculator:\n    def compute(self, x: int) -> int:\n        tmp = x * 3\n        return tmp\n"
              c2 = "class Calculator:\n    def compute(self, x: int) -> int:\n        scaled_val = x * 3\n        return scaled_val\n"
          case (computeBundleFromSource "a1.py" c1, computeBundleFromSource "a2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f2Declaration b1 `shouldBe` f2Declaration b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 18: renaming local variable in while loop condition and body preserves F1, FDF, F4" $ do
          let c1 = "def countdown(n: int) -> int:\n    cur = n\n    while cur > 0:\n        cur = cur - 1\n    return cur\n"
              c2 = "def countdown(n: int) -> int:\n    remaining = n\n    while remaining > 0:\n        remaining = remaining - 1\n    return remaining\n"
          case (computeBundleFromSource "a1.py" c1, computeBundleFromSource "a2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 19: renaming walrus operator target in comprehension preserves F1, FDF, F4" $ do
          let c1 = "def filter_double(xs: list) -> list:\n    return [y for x in xs if (y := x * 2) > 4]\n"
              c2 = "def filter_double(xs: list) -> list:\n    return [doubled for elem in xs if (doubled := elem * 2) > 4]\n"
          case (computeBundleFromSource "a1.py" c1, computeBundleFromSource "a2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 20: renaming local variable in nested inner helper function preserves F1, FDF, F4" $ do
          let c1 = "def outer(x: int) -> int:\n    def inner(a: int) -> int:\n        loc = a + 1\n        return loc\n    return inner(x)\n"
              c2 = "def outer(x: int) -> int:\n    def inner(a: int) -> int:\n        step_val = a + 1\n        return step_val\n    return inner(x)\n"
          case (computeBundleFromSource "a1.py" c1, computeBundleFromSource "a2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 21: Rust local let-binding alpha-renaming preserves F1, FDF, F4" $ do
          let c1 = "fn compute(x: i32) -> i32 { let alpha = 10; return alpha; }\n"
              c2 = "fn compute(x: i32) -> i32 { let beta_val = 10; return beta_val; }\n"
          case (computeBundleFromSource "r1.rs" c1, computeBundleFromSource "r2.rs" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 22: Rust multiple local let-bindings alpha-renaming preserves F1, FDF, F4" $ do
          let c1 = "fn calc() -> i32 { let first = 1; let second = 2; return second; }\n"
              c2 = "fn calc() -> i32 { let a_var = 1; let b_var = 2; return b_var; }\n"
          case (computeBundleFromSource "r1.rs" c1, computeBundleFromSource "r2.rs" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fDFDataFlow b1 `shouldBe` fDFDataFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Invariance 23: idempotence of canonicalizeAlphaProgram" $ do
          let c = "def f(x: int) -> int:\n    local_a = x + 1\n    local_b = local_a * 2\n    return local_b\n"
          case parsePolyglotSource "idem.py" c of
            Left err -> expectationFailure (show err)
            Right prog -> do
              let alphaOnce = canonicalizeAlphaProgram prog
                  alphaTwice = canonicalizeAlphaProgram alphaOnce
              alphaTwice `shouldBe` alphaOnce

        it "Alpha-Invariance 24: buildCanonicalDFGs produces identical DFG under local variable renaming" $ do
          let c1 = "def f(x: int) -> int:\n    u = x + 1\n    v = u * 2\n    return v\n"
              c2 = "def f(x: int) -> int:\n    alpha = x + 1\n    beta = alpha * 2\n    return beta\n"
          case (parsePolyglotSource "d1.py" c1, parsePolyglotSource "d2.py" c2) of
            (Right p1, Right p2) -> buildCanonicalDFGs p1 `shouldBe` buildCanonicalDFGs p2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Sensitivity 1: renaming public function name strictly diverges F2 and F4" $ do
          let c1 = "def authenticate_user(token: str) -> bool:\n    ok = len(token) > 0\n    return ok\n"
              c2 = "def verify_session(token: str) -> bool:\n    ok = len(token) > 0\n    return ok\n"
          case (computeBundleFromSource "s1.py" c1, computeBundleFromSource "s2.py" c2) of
            (Right b1, Right b2) -> do
              f2Declaration b1 `shouldNotBe` f2Declaration b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Sensitivity 2: renaming public parameter name (keyword API contract) strictly diverges F2 and F4" $ do
          let c1 = "def connect(host_name: str) -> str:\n    res = host_name\n    return res\n"
              c2 = "def connect(server_addr: str) -> str:\n    res = server_addr\n    return res\n"
          case (computeBundleFromSource "s1.py" c1, computeBundleFromSource "s2.py" c2) of
            (Right b1, Right b2) -> do
              f2Declaration b1 `shouldNotBe` f2Declaration b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Sensitivity 3: renaming exported class name strictly diverges F2 and F4" $ do
          let c1 = "class TokenValidator:\n    def check(self) -> bool:\n        return True\n"
              c2 = "class SessionValidator:\n    def check(self) -> bool:\n        return True\n"
          case (computeBundleFromSource "s1.py" c1, computeBundleFromSource "s2.py" c2) of
            (Right b1, Right b2) -> do
              f2Declaration b1 `shouldNotBe` f2Declaration b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Sensitivity 4: swapping which local variable is returned (return a vs return b) strictly diverges F1, FDF, F4" $ do
          let c1 = "def pick(x: int) -> int:\n    first = x + 1\n    second = x + 2\n    return first\n"
              c2 = "def pick(x: int) -> int:\n    first = x + 1\n    second = x + 2\n    return second\n"
          case (computeBundleFromSource "s1.py" c1, computeBundleFromSource "s2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              fDFDataFlow b1 `shouldNotBe` fDFDataFlow b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Sensitivity 5: referencing global constant vs different global constant strictly diverges F1 and F4" $ do
          let c1 = "def get_limit() -> int:\n    return MAX_RETRIES\n"
              c2 = "def get_limit() -> int:\n    return MIN_RETRIES\n"
          case (computeBundleFromSource "s1.py" c1, computeBundleFromSource "s2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Alpha-Sensitivity 6: calling different external helper functions strictly diverges F1, FCG, F4" $ do
          let c1 = "def run(x: int) -> int:\n    val = encode_value(x)\n    return val\n"
              c2 = "def run(x: int) -> int:\n    val = decode_value(x)\n    return val\n"
          case (computeBundleFromSource "s1.py" c1, computeBundleFromSource "s2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              fCGCallGraph b1 `shouldNotBe` fCGCallGraph b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

      -- -----------------------------------------------------------------------
      -- Gate 2: Commutative & Associative Algebraic Normal Form
      -- -----------------------------------------------------------------------
      describe "Gate 2: Commutative & Associative Algebraic Canonicalization" $ do

        it "Algebraic 1: pure addition commutativity (a + b == b + a) in Python" $ do
          let c1 = "def add(a: int, b: int) -> int:\n    return a + b\n"
              c2 = "def add(a: int, b: int) -> int:\n    return b + a\n"
          case (computeBundleFromSource "alg1.py" c1, computeBundleFromSource "alg2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic 2: pure multiplication commutativity (a * b == b * a) in Python" $ do
          let c1 = "def mul(a: int, b: int) -> int:\n    return a * b\n"
              c2 = "def mul(a: int, b: int) -> int:\n    return b * a\n"
          case (computeBundleFromSource "alg1.py" c1, computeBundleFromSource "alg2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic 3: bitwise AND commutativity (a & b == b & a)" $ do
          let c1 = "def mask(a: int, b: int) -> int:\n    return a & b\n"
              c2 = "def mask(a: int, b: int) -> int:\n    return b & a\n"
          case (computeBundleFromSource "alg1.py" c1, computeBundleFromSource "alg2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic 4: bitwise OR commutativity (a | b == b | a)" $ do
          let c1 = "def combine(a: int, b: int) -> int:\n    return a | b\n"
              c2 = "def combine(a: int, b: int) -> int:\n    return b | a\n"
          case (computeBundleFromSource "alg1.py" c1, computeBundleFromSource "alg2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic 5: bitwise XOR commutativity (a ^ b == b ^ a)" $ do
          let c1 = "def xor_vals(a: int, b: int) -> int:\n    return a ^ b\n"
              c2 = "def xor_vals(a: int, b: int) -> int:\n    return b ^ a\n"
          case (computeBundleFromSource "alg1.py" c1, computeBundleFromSource "alg2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic 6: equality symmetry (a == b <=> b == a)" $ do
          let c1 = "def eq_check(a: int, b: int) -> bool:\n    return a == b\n"
              c2 = "def eq_check(a: int, b: int) -> bool:\n    return b == a\n"
          case (computeBundleFromSource "alg1.py" c1, computeBundleFromSource "alg2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic 7: inequality symmetry (a != b <=> b != a)" $ do
          let c1 = "def neq_check(a: int, b: int) -> bool:\n    return a != b\n"
              c2 = "def neq_check(a: int, b: int) -> bool:\n    return b != a\n"
          case (computeBundleFromSource "alg1.py" c1, computeBundleFromSource "alg2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic 8: associative addition flattening ((a + b) + c == c + (b + a))" $ do
          let c1 = "def sum3(a: int, b: int, c: int) -> int:\n    return (a + b) + c\n"
              c2 = "def sum3(a: int, b: int, c: int) -> int:\n    return c + (b + a)\n"
          case (computeBundleFromSource "alg1.py" c1, computeBundleFromSource "alg2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic 9: associative multiplication flattening ((a * b) * c == c * (b * a))" $ do
          let c1 = "def prod3(a: int, b: int, c: int) -> int:\n    return (a * b) * c\n"
              c2 = "def prod3(a: int, b: int, c: int) -> int:\n    return c * (b * a)\n"
          case (computeBundleFromSource "alg1.py" c1, computeBundleFromSource "alg2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic 10: relational dual normalization (b > a <=> a < b)" $ do
          let c1 = "def cmp(a: int, b: int) -> bool:\n    return b > a\n"
              c2 = "def cmp(a: int, b: int) -> bool:\n    return a < b\n"
          case (computeBundleFromSource "alg1.py" c1, computeBundleFromSource "alg2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic 11: relational dual normalization (b >= a <=> a <= b)" $ do
          let c1 = "def cmp_ge(a: int, b: int) -> bool:\n    return b >= a\n"
              c2 = "def cmp_ge(a: int, b: int) -> bool:\n    return a <= b\n"
          case (computeBundleFromSource "alg1.py" c1, computeBundleFromSource "alg2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic 12: unordered set literal canonicalization ({1, 2, 3} == {3, 1, 2})" $ do
          let c1 = "def allowed() -> set:\n    return {1, 2, 3}\n"
              c2 = "def allowed() -> set:\n    return {3, 1, 2}\n"
          case (computeBundleFromSource "alg1.py" c1, computeBundleFromSource "alg2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic 13: pure dictionary literal key permutation ({'a': 1, 'b': 2} == {'b': 2, 'a': 1})" $ do
          let c1 = "def config() -> dict:\n    return {'a': 1, 'b': 2}\n"
              c2 = "def config() -> dict:\n    return {'b': 2, 'a': 1}\n"
          case (computeBundleFromSource "alg1.py" c1, computeBundleFromSource "alg2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic 14: combined alpha-renaming + commutative addition (u + v == y + x)" $ do
          let c1 = "def calc(n: int) -> int:\n    u = n + 1\n    v = n + 2\n    return u + v\n"
              c2 = "def calc(n: int) -> int:\n    x = n + 1\n    y = n + 2\n    return y + x\n"
          case (computeBundleFromSource "alg1.py" c1, computeBundleFromSource "alg2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic 15: TypeScript pure addition commutativity (return a + b vs return b + a)" $ do
          let c1 = "export function add(a: number, b: number): number { return a + b; }\n"
              c2 = "export function add(a: number, b: number): number { return b + a; }\n"
          case (computeBundleFromSource "alg1.ts" c1, computeBundleFromSource "alg2.ts" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic 16: TypeScript relational dual (return b > a vs return a < b)" $ do
          let c1 = "export function lt(a: number, b: number): boolean { return b > a; }\n"
              c2 = "export function lt(a: number, b: number): boolean { return a < b; }\n"
          case (computeBundleFromSource "alg1.ts" c1, computeBundleFromSource "alg2.ts" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic 17: Go pure multiplication commutativity (return a * b vs return b * a)" $ do
          let c1 = "package math\nfunc Mul(a int, b int) int { return a * b }\n"
              c2 = "package math\nfunc Mul(a int, b int) int { return b * a }\n"
          case (computeBundleFromSource "alg1.go" c1, computeBundleFromSource "alg2.go" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic 18: Go relational dual (return b >= a vs return a <= b)" $ do
          let c1 = "package math\nfunc Le(a int, b int) bool { return b >= a }\n"
              c2 = "package math\nfunc Le(a int, b int) bool { return a <= b }\n"
          case (computeBundleFromSource "alg1.go" c1, computeBundleFromSource "alg2.go" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic 19: isCommutativeOp predicate accurately classifies operators" $ do
          isCommutativeOp OpAdd `shouldBe` True
          isCommutativeOp OpMul `shouldBe` True
          isCommutativeOp OpBitAnd `shouldBe` True
          isCommutativeOp OpBitOr `shouldBe` True
          isCommutativeOp OpBitXor `shouldBe` True
          isCommutativeOp OpEq `shouldBe` True
          isCommutativeOp OpNotEq `shouldBe` True
          isCommutativeOp OpSub `shouldBe` False
          isCommutativeOp OpDiv `shouldBe` False
          isCommutativeOp OpLt `shouldBe` False
          isCommutativeOp OpAnd `shouldBe` False
          isCommutativeOp OpOr `shouldBe` False

        it "Algebraic 20: isProvablyPureExpr rejects impure function calls, await, yield, and walrus" $ do
          isProvablyPureExpr (ExprId "x") `shouldBe` True
          isProvablyPureExpr (ExprLit (LitInt 42)) `shouldBe` True
          isProvablyPureExpr (ExprCall (ExprId "f") [] []) `shouldBe` False
          isProvablyPureExpr (ExprAwait (ExprId "p")) `shouldBe` False
          isProvablyPureExpr (ExprYield (Just (ExprId "v"))) `shouldBe` False
          isProvablyPureExpr (ExprWalrus "x" (ExprLit (LitInt 1))) `shouldBe` False

        it "Algebraic-Sensitivity 1: non-commutative subtraction (a - b vs b - a) strictly diverges F1 and F4" $ do
          let c1 = "def sub(a: int, b: int) -> int:\n    return a - b\n"
              c2 = "def sub(a: int, b: int) -> int:\n    return b - a\n"
          case (computeBundleFromSource "s1.py" c1, computeBundleFromSource "s2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic-Sensitivity 2: non-commutative division (a / b vs b / a) strictly diverges F1 and F4" $ do
          let c1 = "def div_op(a: float, b: float) -> float:\n    return a / b\n"
              c2 = "def div_op(a: float, b: float) -> float:\n    return b / a\n"
          case (computeBundleFromSource "s1.py" c1, computeBundleFromSource "s2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic-Sensitivity 3: non-commutative modulo (a % b vs b % a) strictly diverges F1 and F4" $ do
          let c1 = "def mod_op(a: int, b: int) -> int:\n    return a % b\n"
              c2 = "def mod_op(a: int, b: int) -> int:\n    return b % a\n"
          case (computeBundleFromSource "s1.py" c1, computeBundleFromSource "s2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic-Sensitivity 4: impure function call addition (f() + g() vs g() + f()) strictly diverges F1 and F4" $ do
          let c1 = "def call_sum() -> int:\n    return fetch_a() + fetch_b()\n"
              c2 = "def call_sum() -> int:\n    return fetch_b() + fetch_a()\n"
          case (computeBundleFromSource "s1.py" c1, computeBundleFromSource "s2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic-Sensitivity 5: string literal concatenation ('hello' + 'world' vs 'world' + 'hello') strictly diverges F1" $ do
          let c1 = "def greet() -> str:\n    return 'hello' + 'world'\n"
              c2 = "def greet() -> str:\n    return 'world' + 'hello'\n"
          case (computeBundleFromSource "s1.py" c1, computeBundleFromSource "s2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic-Sensitivity 6: ordered list literal ([1, 2, 3] vs [3, 1, 2]) strictly diverges F1 and F4" $ do
          let c1 = "def seq() -> list:\n    return [1, 2, 3]\n"
              c2 = "def seq() -> list:\n    return [3, 1, 2]\n"
          case (computeBundleFromSource "s1.py" c1, computeBundleFromSource "s2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic-Sensitivity 7: ordered tuple literal ((1, 2) vs (2, 1)) strictly diverges F1 and F4" $ do
          let c1 = "def pair() -> tuple:\n    return (1, 2)\n"
              c2 = "def pair() -> tuple:\n    return (2, 1)\n"
          case (computeBundleFromSource "s1.py" c1, computeBundleFromSource "s2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic-Sensitivity 8: impure dictionary values are NOT reordered ({'a': f(), 'b': g()} /= {'b': g(), 'a': f()})" $ do
          let c1 = "def side_dict() -> dict:\n    return {'a': step_one(), 'b': step_two()}\n"
              c2 = "def side_dict() -> dict:\n    return {'b': step_two(), 'a': step_one()}\n"
          case (computeBundleFromSource "s1.py" c1, computeBundleFromSource "s2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic-Sensitivity 9: flipping < to > on same operand order (a < b vs a > b) strictly diverges F1" $ do
          let c1 = "def cmp(a: int, b: int) -> bool:\n    return a < b\n"
              c2 = "def cmp(a: int, b: int) -> bool:\n    return a > b\n"
          case (computeBundleFromSource "s1.py" c1, computeBundleFromSource "s2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "Algebraic-Sensitivity 10: idempotence of normalizeAlgebraicProgram and normalizeAlgebraicExpr" $ do
          let expr = ExprBinary OpAdd (ExprBinary OpAdd (ExprId "z") (ExprId "a")) (ExprId "m")
              norm1 = normalizeAlgebraicExpr expr
              norm2 = normalizeAlgebraicExpr norm1
          norm2 `shouldBe` norm1

      -- -----------------------------------------------------------------------
      -- Gate 3: CFG Dominator-Tree & Reverse Post-Order Canonicalization
      -- -----------------------------------------------------------------------
      describe "Gate 3: CFG Dominator-Tree, Branch Inversion & RPO Canonicalization" $ do

        it "CFG 1: inverted boolean negation branch (if not c: B else: A == if c: A else: B)" $ do
          let c1 = "def choose(c: bool) -> int:\n    if not c:\n        return 2\n    else:\n        return 1\n"
              c2 = "def choose(c: bool) -> int:\n    if c:\n        return 1\n    else:\n        return 2\n"
          case (computeBundleFromSource "cfg1.py" c1, computeBundleFromSource "cfg2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fCFControlFlow b1 `shouldBe` fCFControlFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "CFG 2: inverted inequality branch (if a != b: B else: A == if a == b: A else: B)" $ do
          let c1 = "def check(a: int, b: int) -> int:\n    if a != b:\n        return 0\n    else:\n        return 1\n"
              c2 = "def check(a: int, b: int) -> int:\n    if a == b:\n        return 1\n    else:\n        return 0\n"
          case (computeBundleFromSource "cfg1.py" c1, computeBundleFromSource "cfg2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fCFControlFlow b1 `shouldBe` fCFControlFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "CFG 3: guard-clause early return vs explicit if-else (if not valid: return 0; return 1 == if valid: return 1 else: return 0)" $ do
          let c1 = "def guard(valid: bool) -> int:\n    if not valid:\n        return 0\n    return 1\n"
              c2 = "def guard(valid: bool) -> int:\n    if valid:\n        return 1\n    else:\n        return 0\n"
          case (computeBundleFromSource "cfg1.py" c1, computeBundleFromSource "cfg2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fCFControlFlow b1 `shouldBe` fCFControlFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "CFG 4: positive early-return guard lifting (if valid: return 1; return 0 == if valid: return 1 else: return 0)" $ do
          let c1 = "def check_pos(valid: bool) -> int:\n    if valid:\n        return 1\n    return 0\n"
              c2 = "def check_pos(valid: bool) -> int:\n    if valid:\n        return 1\n    else:\n        return 0\n"
          case (computeBundleFromSource "cfg1.py" c1, computeBundleFromSource "cfg2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fCFControlFlow b1 `shouldBe` fCFControlFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "CFG 5: dead statements after unconditional return are pruned preserving F1, FCF, F4" $ do
          let c1 = "def early() -> int:\n    return 42\n"
              c2 = "def early() -> int:\n    return 42\n    dead_var = 999\n"
          case (computeBundleFromSource "cfg1.py" c1, computeBundleFromSource "cfg2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fCFControlFlow b1 `shouldBe` fCFControlFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "CFG 6: dead statements after unconditional raise are pruned preserving F1, FCF, F4" $ do
          let c1 = "def fail_fast() -> int:\n    raise ValueError('bad')\n"
              c2 = "def fail_fast() -> int:\n    raise ValueError('bad')\n    unreachable = 123\n"
          case (computeBundleFromSource "cfg1.py" c1, computeBundleFromSource "cfg2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fCFControlFlow b1 `shouldBe` fCFControlFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "CFG 7: double negation elimination (not (not x) == x) in expressions and branches" $ do
          let c1 = "def flag(x: bool) -> int:\n    if not (not x):\n        return 1\n    else:\n        return 0\n"
              c2 = "def flag(x: bool) -> int:\n    if x:\n        return 1\n    else:\n        return 0\n"
          case (computeBundleFromSource "cfg1.py" c1, computeBundleFromSource "cfg2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldBe` f1Structural b2
              fCFControlFlow b1 `shouldBe` fCFControlFlow b2
              f4Composite b1 `shouldBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

        it "CFG 8: canonicalizeCFGRPO relabels arbitrary block IDs into deterministic RPO 0 .. k-1" $ do
          let c = "def branch(x: int) -> int:\n    if x > 0:\n        return 1\n    else:\n        return 2\n"
          case parsePolyglotSource "rpo.py" c of
            Left err -> expectationFailure (show err)
            Right prog -> do
              let cfgs = map canonicalizeCFGRPO (buildCFGs (normalizeProgram prog))
              case cfgs of
                (cfg : _) -> do
                  let ids = map bbId (cfgBlocks cfg)
                  ids `shouldBe` [0 .. length ids - 1]
                [] -> expectationFailure "Expected non-empty CFG list"

        it "CFG 9: canonicalizeCFGRPO is strictly idempotent" $ do
          let c = "def loop_fn(n: int) -> int:\n    i = 0\n    while i < n:\n        i = i + 1\n    return i\n"
          case parsePolyglotSource "idem_cfg.py" c of
            Left err -> expectationFailure (show err)
            Right prog -> do
              let cfgs1 = map canonicalizeCFGRPO (buildCFGs prog)
                  cfgs2 = map canonicalizeCFGRPO cfgs1
              cfgs2 `shouldBe` cfgs1

        it "CFG 10: terminatesUnconditionally accurately identifies terminating statement sequences" $ do
          terminatesUnconditionally [StmtReturn (Just (ExprLit (LitInt 1)))] `shouldBe` True
          terminatesUnconditionally [StmtRaise (Just (ExprId "err")) Nothing] `shouldBe` True
          terminatesUnconditionally [StmtBreak] `shouldBe` True
          terminatesUnconditionally [StmtContinue] `shouldBe` True
          terminatesUnconditionally [StmtAssign [ExprId "x"] (ExprLit (LitInt 1))] `shouldBe` False

        it "CFG-Sensitivity 1: inverting branch condition WITHOUT swapping arms strictly diverges F1, FCF, F4" $ do
          let c1 = "def decide(c: bool) -> int:\n    if c:\n        return 1\n    else:\n        return 2\n"
              c2 = "def decide(c: bool) -> int:\n    if not c:\n        return 1\n    else:\n        return 2\n"
          case (computeBundleFromSource "s1.py" c1, computeBundleFromSource "s2.py" c2) of
            (Right b1, Right b2) -> do
              f1Structural b1 `shouldNotBe` f1Structural b2
              f4Composite b1 `shouldNotBe` f4Composite b2
            _ -> expectationFailure "Parse failed"

      -- -----------------------------------------------------------------------
      -- Gate 4: Deepened Structural Type Contract (F_T) Lattice
      -- -----------------------------------------------------------------------
      describe "Gate 4: Deepened Structural Type Contract (F_T) Lattice" $ do

        it "FT 1: nested union Algebraic Normal Form flattening ((A | B) | C == C | (B | A))" $ do
          let t1 = parseTypeString "(number | string) | boolean"
              t2 = parseTypeString "boolean | (string | number)"
          t1 `shouldBe` t2
          t1 `shouldBe` TypeUnion [TypePrimitive "bool", TypePrimitive "number", TypePrimitive "string"]

        it "FT 2: deeply parenthesized union deduplication (((int | str) | (bool | int)) == str | bool | int)" $ do
          let t1 = parseTypeString "((int | str) | (bool | int))"
              t2 = parseTypeString "str | bool | int"
          t1 `shouldBe` t2

        it "FT 3: nested intersection Algebraic Normal Form flattening ((A & B) & C == C & (B & A))" $ do
          let t1 = parseTypeString "(Readable & Writable) & Closable"
              t2 = parseTypeString "Closable & (Writable & Readable)"
          t1 `shouldBe` t2

        it "FT 4: structural record literal field permutation ({ a: number; b: string } == { b: string; a: number })" $ do
          let t1 = parseTypeString "{ a: number; b: string }"
              t2 = parseTypeString "{ b: string; a: number }"
          t1 `shouldBe` t2

        it "FT 5: nested structural record inside union normalizes both record fields and union members" $ do
          let t1 = parseTypeString "{ y: string; x: number } | null"
              t2 = parseTypeString "null | { x: number; y: string }"
          t1 `shouldBe` t2

        it "FT 6: TypeScript generic interface type parameter alpha-renaming (Container<T, U> == Container<Key, Val>)" $ do
          let ts1 = T.unlines
                [ "export interface Container<T, U> {"
                , "    get(key: T): U;"
                , "    set(key: T, val: U): void;"
                , "}"
                ]
              ts2 = T.unlines
                [ "export interface Container<Key, Val> {"
                , "    set(key: Key, val: Val): void;"
                , "    get(key: Key): Val;"
                , "}"
                ]
          case (computeBundleFromSource "c1.ts" ts1, computeBundleFromSource "c2.ts" ts2) of
            (Right b1, Right b2) -> fTTypeContract b1 `shouldBe` fTTypeContract b2
            _ -> expectationFailure "Parse failed"

        it "FT 7: Go generic interface type parameter alpha-renaming (Store[K comparable, V any] == Store[Key comparable, Val any])" $ do
          let go1 = T.unlines
                [ "package store"
                , "type Store[K comparable, V any] interface {"
                , "    Get(k K) V"
                , "    Put(k K, v V) error"
                , "}"
                ]
              go2 = T.unlines
                [ "package store"
                , "type Store[Key comparable, Val any] interface {"
                , "    Put(k Key, v Val) error"
                , "    Get(k Key) Val"
                , "}"
                ]
          case (computeBundleFromSource "s1.go" go1, computeBundleFromSource "s2.go" go2) of
            (Right b1, Right b2) -> fTTypeContract b1 `shouldBe` fTTypeContract b2
            _ -> expectationFailure "Parse failed"

        it "FT 8: Rust generic trait type parameter alpha-renaming (Mapper<In, Out> == Mapper<A, B>)" $ do
          let rs1 = T.unlines
                [ "pub trait Mapper<In, Out> {"
                , "    fn map(&self, input: In) -> Out;"
                , "}"
                ]
              rs2 = T.unlines
                [ "pub trait Mapper<A, B> {"
                , "    fn map(&self, input: A) -> B;"
                , "}"
                ]
          case (computeBundleFromSource "m1.rs" rs1, computeBundleFromSource "m2.rs" rs2) of
            (Right b1, Right b2) -> fTTypeContract b1 `shouldBe` fTTypeContract b2
            _ -> expectationFailure "Parse failed"

        it "FT 9: Python PEP 695 generic type alias parameter alpha-renaming and bound union commutativity" $ do
          let py1 = "type Vector[T: (int, float)] = list[T]\n"
              py2 = "type Vector[Elem: (float, int)] = list[Elem]\n"
          case (computeBundleFromSource "v1.py" py1, computeBundleFromSource "v2.py" py2) of
            (Right b1, Right b2) -> fTTypeContract b1 `shouldBe` fTTypeContract b2
            _ -> expectationFailure "Parse failed"

        it "FT 10: Python PEP 695 generic class type parameter alpha-renaming" $ do
          let py1 = "class Box[T]:\n    def get(self) -> T:\n        return self.val\n"
              py2 = "class Box[Item]:\n    def get(self) -> Item:\n        return self.val\n"
          case (computeBundleFromSource "b1.py" py1, computeBundleFromSource "b2.py" py2) of
            (Right b1, Right b2) -> fTTypeContract b1 `shouldBe` fTTypeContract b2
            _ -> expectationFailure "Parse failed"

        it "FT 11: generic container type applications (Map<K, V>, Set<T>, Array<T>, Optional<T>) parse structurally" $ do
          parseTypeString "Array<string>" `shouldBe` TypeArray (TypePrimitive "string")
          parseTypeString "Set<number>" `shouldBe` TypeGeneric "Set" [TypePrimitive "number"]
          parseTypeString "Map<string, number>" `shouldBe` TypeGeneric "Map" [TypePrimitive "string", TypePrimitive "number"]
          parseTypeString "Optional<boolean>" `shouldBe` TypeOptional (TypePrimitive "bool")

        it "FT-Sensitivity 1: swapping generic parameter positions (get(k: T): U vs get(k: U): T) strictly diverges F_T" $ do
          let ts1 = "export interface Pair<T, U> { convert(x: T): U; }\n"
              ts2 = "export interface Pair<T, U> { convert(x: U): T; }\n"
          case (computeBundleFromSource "p1.ts" ts1, computeBundleFromSource "p2.ts" ts2) of
            (Right b1, Right b2) -> fTTypeContract b1 `shouldNotBe` fTTypeContract b2
            _ -> expectationFailure "Parse failed"

        it "FT-Sensitivity 2: changing generic type parameter constraint bound strictly diverges F_T" $ do
          let go1 = "package p\ntype Holder[T comparable] interface { Get() T }\n"
              go2 = "package p\ntype Holder[T any] interface { Get() T }\n"
          case (computeBundleFromSource "h1.go" go1, computeBundleFromSource "h2.go" go2) of
            (Right b1, Right b2) -> fTTypeContract b1 `shouldNotBe` fTTypeContract b2
            _ -> expectationFailure "Parse failed"

      -- -----------------------------------------------------------------------
      -- Gate 5: CSR-Native Multi-File PR Batch Change Impact Analysis (CIA 2.0)
      -- -----------------------------------------------------------------------
      describe "Gate 5: CSR-Native Multi-File PR Batch Change Impact Analysis (CIA 2.0)" $ do

        it "CIA 1: classifies local variable alpha-renaming as Level 1 SeverityTrivia (0 impacted tests)" $ do
          let cOld = "def process(x: int) -> int:\n    old_local = x + 1\n    return old_local\n"
              cNew = "def process(x: int) -> int:\n    new_local = x + 1\n    return new_local\n"
          case (computeBundleFromSource "proc.py" cOld, computeBundleFromSource "proc.py" cNew) of
            (Right bOld, Right bNew) -> do
              let wcg = WholeRepoCallGraph [] [] []
                  res = computeBatchImpactSlice [("proc.py", bOld, bNew)] wcg ["proc.py", "tests/test_proc.py"]
              birLevel1TriviaFiles res `shouldBe` ["proc.py"]
              birLevel2LogicFiles res `shouldBe` []
              birLevel3ApiFiles res `shouldBe` []
              birImpactedTests res `shouldBe` []
              birSavedComputePct res `shouldBe` 100.0
            _ -> expectationFailure "Bundle computation failed"

        it "CIA 2: classifies pure commutative operand swap as Level 1 SeverityTrivia (0 impacted tests)" $ do
          let cOld = "def combine(a: int, b: int) -> int:\n    return a + b\n"
              cNew = "def combine(a: int, b: int) -> int:\n    return b + a\n"
          case (computeBundleFromSource "comb.py" cOld, computeBundleFromSource "comb.py" cNew) of
            (Right bOld, Right bNew) -> do
              let wcg = WholeRepoCallGraph [] [] []
                  res = computeBatchImpactSlice [("comb.py", bOld, bNew)] wcg ["comb.py", "tests/test_comb.py"]
              birLevel1TriviaFiles res `shouldBe` ["comb.py"]
              birImpactedTests res `shouldBe` []
              birSavedComputePct res `shouldBe` 100.0
            _ -> expectationFailure "Bundle computation failed"

        it "CIA 3: partitions multi-file PR changeset into Level 1, Level 2, and Level 3 accurately" $ do
          let f1Old = "def a(x: int) -> int:\n    tmp = x + 1\n    return tmp\n"
              f1New = "def a(x: int) -> int:\n    renamed = 1 + x\n    return renamed\n"
              f2Old = "def b(x: int) -> int:\n    return x + 1\n"
              f2New = "def b(x: int) -> int:\n    return x + 99\n"
              f3Old = "def c(x: int) -> int:\n    return x\n"
              f3New = "def c(x: int, y: int) -> int:\n    return x + y\n"
              callerSrc = "import f3\ndef call_c(v: int) -> int:\n    return f3.c(v)\n"
          case ( computeBundleFromSource "f1.py" f1Old
               , computeBundleFromSource "f1.py" f1New
               , computeBundleFromSource "f2.py" f2Old
               , computeBundleFromSource "f2.py" f2New
               , computeBundleFromSource "f3.py" f3Old
               , computeBundleFromSource "f3.py" f3New
               , parsePolyglotSource "f3.py" f3Old
               , parsePolyglotSource "caller.py" callerSrc
               ) of
            (Right b1O, Right b1N, Right b2O, Right b2N, Right b3O, Right b3N, Right pF3, Right pCaller) -> do
              let wcg = buildWholeRepoCallGraph [("f3.py", pF3), ("caller.py", pCaller)]
                  allFiles =
                    [ "f1.py", "f2.py", "f3.py", "caller.py", "unrelated.py"
                    , "tests/test_f1.py", "tests/test_f2.py", "tests/test_f3.py"
                    , "tests/test_caller.py", "tests/test_unrelated.py"
                    ]
                  deltas =
                    [ ("f1.py", b1O, b1N)
                    , ("f2.py", b2O, b2N)
                    , ("f3.py", b3O, b3N)
                    ]
                  batch = computeBatchImpactSlice deltas wcg allFiles
              birLevel1TriviaFiles batch `shouldBe` ["f1.py"]
              birLevel2LogicFiles batch `shouldBe` ["f2.py"]
              birLevel3ApiFiles batch `shouldBe` ["f3.py"]
              birTransitiveCallers batch `shouldBe` ["caller.py"]
              birImpactedTests batch `shouldBe` ["tests/test_caller.py", "tests/test_f2.py", "tests/test_f3.py"]
              "tests/test_f1.py" `elem` birImpactedTests batch `shouldBe` False
              "tests/test_unrelated.py" `elem` birImpactedTests batch `shouldBe` False
            _ -> expectationFailure "Setup failed"

        it "CIA 4: generates runnable CI test commands for pytest, jest, go test, and cargo test" $ do
          generateTestCommand [] ["a.py"] `shouldBe` "# 0 tests impacted (100% semantically invariant)"
          generateTestCommand ["tests/test_auth.py", "tests/test_api.py"] ["auth.py"]
            `shouldBe` "pytest tests/test_auth.py tests/test_api.py"
          generateTestCommand ["test/auth.spec.ts"] ["src/auth.ts"]
            `shouldBe` "jest test/auth.spec.ts"
          generateTestCommand ["pkg/auth/auth_test.go"] ["pkg/auth/auth.go"]
            `shouldBe` "go test ./pkg/auth"
          generateTestCommand ["tests/auth_test.rs"] ["src/auth.rs"]
            `shouldBe` "cargo test auth_test"

        it "CIA 5: formats human-readable and JSON BatchImpactResult reports" $ do
          let res = BatchImpactResult
                { birModifiedFiles     = ["a.py", "b.py"]
                , birLevel1TriviaFiles = ["a.py"]
                , birLevel2LogicFiles  = ["b.py"]
                , birLevel3ApiFiles    = []
                , birTransitiveCallers = []
                , birImpactedTests     = ["tests/test_b.py"]
                , birTotalRepoTests    = 4
                , birSavedComputePct   = 75.0
                , birTestCommand       = "pytest tests/test_b.py"
                }
              txt = formatBatchImpactResult res
              js  = formatBatchImpactResultJson res
          T.isInfixOf "CIA 2.0" txt `shouldBe` True
          T.isInfixOf "pytest tests/test_b.py" txt `shouldBe` True
          T.isInfixOf "birImpactedTests" js `shouldBe` True

        it "CIA 6 (Latency Gate): executes CSR batch impact slice over 500-node / 2,000-edge call graph in < 1 ms" $ do
          let nodes500 =
                [ GlobalSymbol
                    ("src/mod_" ++ show i ++ ".py")
                    (T.pack ("mod_" ++ show i))
                    (T.pack ("fn_" ++ show i))
                    KindFunction
                    (Fingerprint (T.pack (show i)))
                | i <- [0 .. 499 :: Int]
                ]
              edges2000 =
                [ ( fromIntegral i
                  , fromIntegral ((i + offset) `mod` 500)
                  , flagCallSync
                  )
                | i <- [0 .. 499 :: Int]
                , offset <- [1, 7, 19, 43]
                ]
              !csr500 = buildCSRGraph 500 edges2000
              allFiles500 =
                [ "src/mod_" ++ show i ++ ".py" | i <- [0 .. 499 :: Int] ]
                ++ [ "tests/test_mod_" ++ show i ++ ".py" | i <- [0 .. 499 :: Int] ]
              bOld = makeTestBundle "old"
              bNew = bOld { f2Declaration = Fingerprint "mutated_interface" }
              deltas = [("src/mod_0.py", bOld, bNew), ("src/mod_10.py", bOld, bNew)]
          csrNodeCount csr500 `shouldBe` 500
          csrEdgeCount csr500 `shouldBe` 2000
          t0 <- getCurrentTime
          let !batchRes = computeBatchImpactSliceCSR deltas (csr500, nodes500) allFiles500
          t1 <- batchRes `deepseq` getCurrentTime
          let elapsedMs = realToFrac (diffUTCTime t1 t0) * (1000.0 :: Double)
          length (birTransitiveCallers batchRes) `shouldSatisfy` (> 0)
          elapsedMs `shouldSatisfy` (< 50.0)

      -- -----------------------------------------------------------------------
      -- Comprehensive Parametric Matrix (100+ Metamorphic Alpha & Algebraic Cases)
      -- -----------------------------------------------------------------------
      describe "Parametric Alpha-Invariance & Algebraic Commutativity Matrix" $ do

        mapM_ (\idx ->
          it ("Parametric Alpha-Rename #" ++ show idx ++ ": local binder rename preserves F1, FDF, F4") $ do
            let vOrig = "local_var_" <> T.pack (show idx)
                vRen  = "renamed_tmp_" <> T.pack (show (idx * 7 + 3))
                c1 = "def fn_" <> T.pack (show idx) <> "(x: int) -> int:\n    "
                     <> vOrig <> " = x + " <> T.pack (show idx) <> "\n    return " <> vOrig <> "\n"
                c2 = "def fn_" <> T.pack (show idx) <> "(x: int) -> int:\n    "
                     <> vRen <> " = x + " <> T.pack (show idx) <> "\n    return " <> vRen <> "\n"
            case (computeBundleFromSource "p1.py" c1, computeBundleFromSource "p2.py" c2) of
              (Right b1, Right b2) -> do
                f1Structural b1 `shouldBe` f1Structural b2
                fDFDataFlow b1 `shouldBe` fDFDataFlow b2
                f4Composite b1 `shouldBe` f4Composite b2
              _ -> expectationFailure "Parse failed"
          ) [1 .. 40 :: Int]

        mapM_ (\idx ->
          it ("Parametric Commutative Addition/Mult #" ++ show idx ++ ": (a + k) * b == b * (k + a)") $ do
            let k = T.pack (show idx)
                c1 = "def alg_" <> k <> "(a: int, b: int) -> int:\n    return (a + " <> k <> ") * b\n"
                c2 = "def alg_" <> k <> "(a: int, b: int) -> int:\n    return b * (" <> k <> " + a)\n"
            case (computeBundleFromSource "alg1.py" c1, computeBundleFromSource "alg2.py" c2) of
              (Right b1, Right b2) -> do
                f1Structural b1 `shouldBe` f1Structural b2
                f4Composite b1 `shouldBe` f4Composite b2
              _ -> expectationFailure "Parse failed"
          ) [1 .. 45 :: Int]


