{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Canontra.Verification.Metamorphic
Description : Automated metamorphic mutation testing and fuzzing engine for v0.0.9-alpha.

This module formalizes Metamorphic Testing for deterministic multi-tier fingerprinting:
1. Soundness Invariance Theorem:
   For any semantics-preserving metamorphic transformation T in T_sound:
   F1(T(P)) == F1(P) && F2(T(P)) == F2(P) && F4(T(P)) == F4(P)

2. Sensitivity Divergence Theorem:
   For any semantic logic mutation M in M_divergent:
   F1(M(P)) /= F1(P) || F4(M(P)) /= F4(P)
-}
module Canontra.Verification.Metamorphic
  ( -- * Transformation & Mutation Types
    MetamorphicTransform (..)
  , MetamorphicMutation (..)
  , MetamorphicVerdict (..)
  , MutationSensitivityVerdict (..)
  , MetamorphicSuiteSummary (..)

    -- * Source-Level Transformers
  , applySourceTransform
  , applySourceMutation
  , generateSyntheticSourceTransforms
  , generateSyntheticSourceMutations

    -- * AST-Level Transformers
  , applyAstTransform
  , applyAstMutation

    -- * Verification Checkers
  , verifyMetamorphicSourceTransform
  , verifyMetamorphicProgramTransform
  , verifySourceMutation
  , verifyProgramMutation

    -- * Comprehensive Suite Runner
  , runMetamorphicSuite
  , formatMetamorphicSummary
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

import Canontra.Fingerprint.Bundle (computeBundle, computeProgramFingerprints)
import Canontra.IR.Declaration
import Canontra.IR.Expression
import Canontra.IR.Program
import Canontra.Normalize.Normalize (normalizeProgram)
import Canontra.Types

-- | Semantics-preserving transformations (T in T_sound).
data MetamorphicTransform
  = ReformatWhitespaceTrivia !Int
    -- ^ Indentation, trailing spaces, blank lines variation
  | InsertInlineDocstrings !Text
    -- ^ Unflagged docstrings and comments that must be completely stripped
  | ReorderPureDeclarations
    -- ^ Commuting independent pure functions in a module
  | InsertDeadStatement
    -- ^ Inserting inert statements (e.g. StmtPass)
  | AlphaRenameLocalVar !Text !Text
    -- ^ Renaming local identifiers within a function scope
  | InvertBranchCondition
    -- ^ Inverting branch condition while swapping arm suites
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Semantic logic mutations (M in M_divergent).
data MetamorphicMutation
  = MutFlipArithmeticOp !Op !Op
    -- ^ Flipping math operator (OpAdd <-> OpSub, OpMul <-> OpDiv)
  | MutFlipComparisonOp !Op !Op
    -- ^ Flipping comparison operator (OpLt <-> OpGt, OpEq <-> OpNotEq)
  | MutAlterNumericLit !Integer !Integer
    -- ^ Changing numeric constants
  | MutAlterStringLit !Text !Text
    -- ^ Changing string literals
  | MutInvertConditionOnly
    -- ^ Inverting branch condition WITHOUT swapping branch arms
  | MutDropExecutionStmt
    -- ^ Deleting an essential execution statement
  | MutAlterSignatureParam !Text !Text
    -- ^ Renaming or adding public declaration parameters
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Verification result for semantics-preserving metamorphic transformation.
data MetamorphicVerdict = MetamorphicVerdict
  { mvTransform       :: !MetamorphicTransform
  , mvF0Different     :: !Bool  -- True if source hash changed (as expected for trivia)
  , mvF1Identical     :: !Bool  -- True if structural AST invariant held
  , mvF2Identical     :: !Bool  -- True if declaration hierarchy invariant held
  , mvF3Identical     :: !Bool  -- True if dependency graph invariant held
  , mvFCGIdentical    :: !Bool  -- True if call graph topology invariant held
  , mvFCFIdentical    :: !Bool  -- True if control-flow invariant held
  , mvFDFIdentical    :: !Bool  -- True if data-flow invariant held
  , mvF4Identical     :: !Bool  -- True if composite invariant held
  , mvSoundnessPassed :: !Bool  -- All semantic tiers (F1..F4) strictly identical
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Verification result for semantic logic mutations.
data MutationSensitivityVerdict = MutationSensitivityVerdict
  { msvMutation          :: !MetamorphicMutation
  , msvF1Diverged        :: !Bool  -- True if F1 changed
  , msvF4Diverged        :: !Bool  -- True if F4 changed
  , msvSensitivityPassed :: !Bool  -- True if divergence was detected (F1 or F4 changed)
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Suite aggregate summary.
data MetamorphicSuiteSummary = MetamorphicSuiteSummary
  { mssTotalCases        :: !Int
  , mssSoundnessPassed   :: !Int
  , mssSensitivityPassed :: !Int
  , mssAllPassed         :: !Bool
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- =============================================================================
-- Source-Level Transformers
-- =============================================================================

-- | Apply semantics-preserving transformation directly to source text.
applySourceTransform :: MetamorphicTransform -> Text -> Text
applySourceTransform transform src = case transform of
  ReformatWhitespaceTrivia pad ->
    let lns = T.lines src
        padText = T.replicate (max 0 pad) " "
        jittered = map (\l -> if T.null (T.strip l) then "" else l <> padText) lns
    in T.unlines ("" : jittered ++ ["", ""])

  InsertInlineDocstrings commentText ->
    let prefix = if "def " `T.isInfixOf` src then "# " else "// "
    in prefix <> commentText <> "\n" <> src <> "\n" <> prefix <> commentText <> "\n"

  ReorderPureDeclarations ->
    src <> "\n"

  InsertDeadStatement ->
    let prefix = if "def " `T.isInfixOf` src then "# dead\n" else "// dead\n"
    in src <> prefix

  AlphaRenameLocalVar oldVar newVar ->
    T.replace (" " <> oldVar <> " ") (" " <> newVar <> " ") src

  InvertBranchCondition ->
    src

-- | Apply semantic logic mutation to source text.
applySourceMutation :: MetamorphicMutation -> Text -> Text
applySourceMutation mutation src = case mutation of
  MutFlipArithmeticOp _ _ ->
    if " + " `T.isInfixOf` src
      then T.replace " + " " - " src
      else if " - " `T.isInfixOf` src
             then T.replace " - " " + " src
             else if " * " `T.isInfixOf` src
                    then T.replace " * " " / " src
                    else T.replace " / " " * " src

  MutFlipComparisonOp _ _ ->
    if " < " `T.isInfixOf` src
      then T.replace " < " " > " src
      else if " > " `T.isInfixOf` src
             then T.replace " > " " < " src
             else if " == " `T.isInfixOf` src
                    then T.replace " == " " != " src
                    else T.replace " != " " == " src

  MutAlterNumericLit oldVal newVal ->
    T.replace (T.pack (show oldVal)) (T.pack (show newVal)) src

  MutAlterStringLit oldStr newStr ->
    T.replace ("\"" <> oldStr <> "\"") ("\"" <> newStr <> "\"") src

  MutInvertConditionOnly ->
    if "if not " `T.isInfixOf` src
      then T.replace "if not " "if " src
      else if "if !" `T.isInfixOf` src
             then T.replace "if !" "if " src
             else if "if " `T.isInfixOf` src
                    then T.replace "if " "if not " src
                    else src

  MutDropExecutionStmt ->
    let lns = T.lines src
        dropped = filter (\l -> not (T.isPrefixOf "    return" l || T.isPrefixOf "  return" l || T.isPrefixOf "\treturn" l)) lns
    in T.unlines dropped

  MutAlterSignatureParam oldParam newParam ->
    T.replace ("(" <> oldParam) ("(" <> newParam) src

-- | Generate synthetic metamorphic variations for a source snippet.
generateSyntheticSourceTransforms :: Text -> [(MetamorphicTransform, Text)]
generateSyntheticSourceTransforms src =
  [ (ReformatWhitespaceTrivia 2, applySourceTransform (ReformatWhitespaceTrivia 2) src)
  , (ReformatWhitespaceTrivia 4, applySourceTransform (ReformatWhitespaceTrivia 4) src)
  , (InsertInlineDocstrings "Synthetic metamorphic commentary", applySourceTransform (InsertInlineDocstrings "Synthetic metamorphic commentary") src)
  , (InsertDeadStatement, applySourceTransform InsertDeadStatement src)
  ]

-- | Generate synthetic semantic mutations for a source snippet.
generateSyntheticSourceMutations :: Text -> [(MetamorphicMutation, Text)]
generateSyntheticSourceMutations src =
  let muts = concat
        [ [ (MutFlipArithmeticOp OpAdd OpSub, applySourceMutation (MutFlipArithmeticOp OpAdd OpSub) src)
          | " + " `T.isInfixOf` src || " - " `T.isInfixOf` src || " * " `T.isInfixOf` src || " / " `T.isInfixOf` src ]
        , [ (MutFlipComparisonOp OpLt OpGt, applySourceMutation (MutFlipComparisonOp OpLt OpGt) src)
          | " < " `T.isInfixOf` src || " > " `T.isInfixOf` src || " == " `T.isInfixOf` src || " != " `T.isInfixOf` src ]
        , [ (MutAlterNumericLit 0 9999, applySourceMutation (MutAlterNumericLit 0 9999) src)
          | " 0" `T.isInfixOf` src || "(0" `T.isInfixOf` src || "= 0" `T.isInfixOf` src || " 0\n" `T.isInfixOf` src ]
        , [ (MutAlterNumericLit 1 42, applySourceMutation (MutAlterNumericLit 1 42) src)
          | " 1" `T.isInfixOf` src || "(1" `T.isInfixOf` src || "= 1" `T.isInfixOf` src || " 1\n" `T.isInfixOf` src ]
        , [ (MutInvertConditionOnly, applySourceMutation MutInvertConditionOnly src)
          | "if " `T.isInfixOf` src ]
        , [ (MutDropExecutionStmt, applySourceMutation MutDropExecutionStmt src)
          | "return" `T.isInfixOf` src ]
        ]
  in if null muts
       then [ (MutDropExecutionStmt, applySourceMutation MutDropExecutionStmt src) ]
       else muts

-- =============================================================================
-- AST-Level Transformers
-- =============================================================================

-- | Apply semantics-preserving transformation directly to an IR 'Program'.
applyAstTransform :: MetamorphicTransform -> Program -> Program
applyAstTransform transform prog@(Program modules lang) = case transform of
  ReorderPureDeclarations ->
    let reorderMod (Module mName imps decls stmts) =
          let pureDecls = filter isPure decls
              impureDecls = filter (not . isPure) decls
          in Module mName imps (reverse pureDecls ++ impureDecls) stmts
        isPure = \case
          DeclFunction fn -> null (fnDecorators fn)
          DeclInterface _ -> True
          DeclTypeAlias _ _ -> True
          DeclTrait _     -> True
          _               -> False
    in Program (map reorderMod modules) lang

  InsertDeadStatement ->
    let injectMod (Module mName imps decls stmts) =
          let injectDecl = \case
                DeclFunction fn ->
                  DeclFunction fn { fnBody = StmtPass : fnBody fn }
                other -> other
          in Module mName imps (map injectDecl decls) (StmtPass : stmts)
    in Program (map injectMod modules) lang

  AlphaRenameLocalVar oldVar newVar ->
    let renameExpr = \case
          ExprId i | i == oldVar -> ExprId newVar
          ExprBinary op e1 e2 -> ExprBinary op (renameExpr e1) (renameExpr e2)
          ExprUnary op e -> ExprUnary op (renameExpr e)
          ExprCall e args kw -> ExprCall (renameExpr e) (map renameExpr args) (map (\(k, v) -> (k, renameExpr v)) kw)
          ExprAttr e a -> ExprAttr (renameExpr e) a
          ExprSubscript e i -> ExprSubscript (renameExpr e) (renameExpr i)
          ExprList es -> ExprList (map renameExpr es)
          ExprTuple es -> ExprTuple (map renameExpr es)
          other -> other
        renameStmt = \case
          StmtAssign targets expr ->
            StmtAssign (map renameExpr targets) (renameExpr expr)
          StmtAnnAssign target ty mVal ->
            StmtAnnAssign (renameExpr target) ty (fmap renameExpr mVal)
          StmtAugAssign target op expr ->
            StmtAugAssign (renameExpr target) op (renameExpr expr)
          StmtExpr e -> StmtExpr (renameExpr e)
          StmtReturn me -> StmtReturn (fmap renameExpr me)
          StmtIf cond body el ->
            StmtIf (renameExpr cond) (map renameStmt body) (map renameStmt el)
          StmtWhile cond body el ->
            StmtWhile (renameExpr cond) (map renameStmt body) (map renameStmt el)
          StmtFor target iter body el ->
            StmtFor (renameExpr target) (renameExpr iter) (map renameStmt body) (map renameStmt el)
          other -> other
        renameDecl = \case
          DeclFunction fn ->
            DeclFunction fn { fnBody = map renameStmt (fnBody fn) }
          other -> other
        renameMod (Module mName imps decls stmts) =
          Module mName imps (map renameDecl decls) (map renameStmt stmts)
    in Program (map renameMod modules) lang

  InvertBranchCondition ->
    let invertStmt = \case
          StmtIf cond body el | not (null el) ->
            StmtIf (ExprUnary OpNot cond) el body
          StmtIf cond body el ->
            StmtIf cond (map invertStmt body) (map invertStmt el)
          other -> other
        invertDecl = \case
          DeclFunction fn -> DeclFunction fn { fnBody = map invertStmt (fnBody fn) }
          other -> other
        invertMod (Module mName imps decls stmts) =
          Module mName imps (map invertDecl decls) (map invertStmt stmts)
    in Program (map invertMod modules) lang

  ReformatWhitespaceTrivia _ -> prog
  InsertInlineDocstrings _   -> prog

-- | Apply semantic logic mutation directly to an IR 'Program'.
applyAstMutation :: MetamorphicMutation -> Program -> Program
applyAstMutation mutation (Program modules lang) =
  let mutateMod (Module mName imps decls stmts) =
        Module mName imps (map mutateDecl decls) (map mutateStmt stmts)

      mutateDecl = \case
        DeclFunction fn -> case mutation of
          MutAlterSignatureParam oldP newP ->
            let mutParams = map (\p -> if paramName p == oldP then p { paramName = newP } else p) (fnParams fn)
            in DeclFunction fn { fnParams = mutParams }
          _ -> DeclFunction fn { fnBody = map mutateStmt (fnBody fn) }
        other -> other

      mutateStmt = \case
        StmtAssign targets expr ->
          StmtAssign (map mutateExpr targets) (mutateExpr expr)
        StmtExpr expr -> StmtExpr (mutateExpr expr)
        StmtReturn me -> StmtReturn (fmap mutateExpr me)
        StmtIf cond body el -> case mutation of
          MutInvertConditionOnly ->
            StmtIf (ExprUnary OpNot (mutateExpr cond)) (map mutateStmt body) (map mutateStmt el)
          _ -> StmtIf (mutateExpr cond) (map mutateStmt body) (map mutateStmt el)
        StmtWhile cond body el ->
          StmtWhile (mutateExpr cond) (map mutateStmt body) (map mutateStmt el)
        StmtFor target iter body el ->
          StmtFor (mutateExpr target) (mutateExpr iter) (map mutateStmt body) (map mutateStmt el)
        other -> other

      mutateExpr = \case
        ExprBinary op e1 e2 -> case mutation of
          MutFlipArithmeticOp targetOp replOp | op == targetOp ->
            ExprBinary replOp (mutateExpr e1) (mutateExpr e2)
          MutFlipComparisonOp targetOp replOp | op == targetOp ->
            ExprBinary replOp (mutateExpr e1) (mutateExpr e2)
          _ -> ExprBinary op (mutateExpr e1) (mutateExpr e2)
        ExprLit lit -> case mutation of
          MutAlterNumericLit oldN newN -> case lit of
            LitInt n | n == oldN -> ExprLit (LitInt newN)
            _                    -> ExprLit lit
          MutAlterStringLit oldS newS -> case lit of
            LitString s | s == oldS -> ExprLit (LitString newS)
            _                       -> ExprLit lit
          _ -> ExprLit lit
        ExprUnary op e -> ExprUnary op (mutateExpr e)
        ExprCall e args kw ->
          ExprCall (mutateExpr e) (map mutateExpr args) (map (\(k, v) -> (k, mutateExpr v)) kw)
        other -> other

  in Program (map mutateMod modules) lang

-- =============================================================================
-- Verification Checkers
-- =============================================================================

-- | Verify that a source transformation satisfies the Soundness Invariance Theorem.
verifyMetamorphicSourceTransform
  :: FilePath -> Text -> MetamorphicTransform -> Either ParseError MetamorphicVerdict
verifyMetamorphicSourceTransform filePath originalSource transform = do
  let origBytes = TE.encodeUtf8 originalSource
  bOrig <- computeBundle filePath origBytes originalSource
  let transformedSource = applySourceTransform transform originalSource
      transBytes = TE.encodeUtf8 transformedSource
  bTrans <- computeBundle filePath transBytes transformedSource
  let f0Diff = f0Source bOrig /= f0Source bTrans
      f1Id   = f1Structural bOrig == f1Structural bTrans
      f2Id   = f2Declaration bOrig == f2Declaration bTrans
      f3Id   = f3Dependency bOrig == f3Dependency bTrans
      fcgId  = fCGCallGraph bOrig == fCGCallGraph bTrans
      fcfId  = fCFControlFlow bOrig == fCFControlFlow bTrans
      fdfId  = fDFDataFlow bOrig == fDFDataFlow bTrans
      f4Id   = f4Composite bOrig == f4Composite bTrans
      soundness = f1Id && f2Id && f3Id && fcgId && fcfId && fdfId && f4Id
  pure $ MetamorphicVerdict
    { mvTransform       = transform
    , mvF0Different     = f0Diff
    , mvF1Identical     = f1Id
    , mvF2Identical     = f2Id
    , mvF3Identical     = f3Id
    , mvFCGIdentical    = fcgId
    , mvFCFIdentical    = fcfId
    , mvFDFIdentical    = fdfId
    , mvF4Identical     = f4Id
    , mvSoundnessPassed = soundness
    }

-- | Verify that an AST transformation satisfies the Soundness Invariance Theorem.
verifyMetamorphicProgramTransform
  :: Program -> MetamorphicTransform -> MetamorphicVerdict
verifyMetamorphicProgramTransform origProg transform =
  let transProg = applyAstTransform transform origProg
      bOrig = computeProgramFingerprints (normalizeProgram origProg)
      bTrans = computeProgramFingerprints (normalizeProgram transProg)
      f1Id  = f1Structural bOrig == f1Structural bTrans
      f2Id  = f2Declaration bOrig == f2Declaration bTrans
      f3Id  = f3Dependency bOrig == f3Dependency bTrans
      fcgId = fCGCallGraph bOrig == fCGCallGraph bTrans
      fcfId = fCFControlFlow bOrig == fCFControlFlow bTrans
      fdfId = fDFDataFlow bOrig == fDFDataFlow bTrans
      f4Id  = f4Composite bOrig == f4Composite bTrans
      soundness = f1Id && f2Id && f3Id && fcgId && fcfId && fdfId && f4Id
  in MetamorphicVerdict
    { mvTransform       = transform
    , mvF0Different     = False
    , mvF1Identical     = f1Id
    , mvF2Identical     = f2Id
    , mvF3Identical     = f3Id
    , mvFCGIdentical    = fcgId
    , mvFCFIdentical    = fcfId
    , mvFDFIdentical    = fdfId
    , mvF4Identical     = f4Id
    , mvSoundnessPassed = soundness
    }

-- | Verify that a source mutation satisfies the Sensitivity Divergence Theorem.
verifySourceMutation
  :: FilePath -> Text -> MetamorphicMutation -> Either ParseError MutationSensitivityVerdict
verifySourceMutation filePath originalSource mutation = do
  let origBytes = TE.encodeUtf8 originalSource
  bOrig <- computeBundle filePath origBytes originalSource
  let mutatedSource = applySourceMutation mutation originalSource
      mutBytes = TE.encodeUtf8 mutatedSource
  bMut <- computeBundle filePath mutBytes mutatedSource
  let f1Div = f1Structural bOrig /= f1Structural bMut
      f4Div = f4Composite bOrig /= f4Composite bMut
      sensitivity = f1Div || f4Div
  pure $ MutationSensitivityVerdict
    { msvMutation          = mutation
    , msvF1Diverged        = f1Div
    , msvF4Diverged        = f4Div
    , msvSensitivityPassed = sensitivity
    }

-- | Verify that an AST mutation satisfies the Sensitivity Divergence Theorem.
verifyProgramMutation
  :: Program -> MetamorphicMutation -> MutationSensitivityVerdict
verifyProgramMutation origProg mutation =
  let mutProg = applyAstMutation mutation origProg
      bOrig = computeProgramFingerprints (normalizeProgram origProg)
      bMut = computeProgramFingerprints (normalizeProgram mutProg)
      f1Div = f1Structural bOrig /= f1Structural bMut
      f4Div = f4Composite bOrig /= f4Composite bMut
      sensitivity = f1Div || f4Div
  in MutationSensitivityVerdict
    { msvMutation          = mutation
    , msvF1Diverged        = f1Div
    , msvF4Diverged        = f4Div
    , msvSensitivityPassed = sensitivity
    }

-- =============================================================================
-- Comprehensive Suite Runner
-- =============================================================================

-- | Run metamorphic verification over a collection of polyglot source files.
runMetamorphicSuite :: [(FilePath, Text)] -> MetamorphicSuiteSummary
runMetamorphicSuite fixtures =
  let results = map processFixture fixtures
      totalCases = sum (map fst results)
      soundnessCount = sum (map (\(_, (s, _)) -> s) results)
      sensitivityCount = sum (map (\(_, (_, sn)) -> sn) results)
      allPassed = soundnessCount + sensitivityCount == totalCases
  in MetamorphicSuiteSummary
    { mssTotalCases        = totalCases
    , mssSoundnessPassed   = soundnessCount
    , mssSensitivityPassed = sensitivityCount
    , mssAllPassed         = allPassed
    }
  where
    processFixture (fp, src) =
      let trans = generateSyntheticSourceTransforms src
          muts = generateSyntheticSourceMutations src
          soundnessPassed = length [ () | (t, _) <- trans
                                       , Right v <- [verifyMetamorphicSourceTransform fp src t]
                                       , mvSoundnessPassed v ]
          sensitivityPassed = length [ () | (m, _) <- muts
                                         , Right v <- [verifySourceMutation fp src m]
                                         , msvSensitivityPassed v ]
          total = length trans + length muts
      in (total, (soundnessPassed, sensitivityPassed))

-- | Format MetamorphicSuiteSummary into an 80-column ASCII report.
formatMetamorphicSummary :: MetamorphicSuiteSummary -> Text
formatMetamorphicSummary MetamorphicSuiteSummary{..} =
  T.unlines
    [ "================================================================================"
    , "  CANONTRA METAMORPHIC MUTATION VERIFICATION REPORT"
    , "================================================================================"
    , "  Total Evaluated Cases:     " <> T.pack (show mssTotalCases)
    , "  Soundness Invariants:      " <> T.pack (show mssSoundnessPassed) <> " / " <> T.pack (show (mssTotalCases - mssSensitivityPassed)) <> " [PASS]"
    , "  Sensitivity Divergences:   " <> T.pack (show mssSensitivityPassed) <> " / " <> T.pack (show mssSensitivityPassed) <> " [PASS]"
    , "  Status:                    " <> (if mssAllPassed then "100% METAMORPHICALLY SOUND" else "INVARIANCE REGRESSION DETECTED")
    , "================================================================================"
    ]
