{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Canontra.Normalize.Algebraic
Description : Pure commutative, associative, and relational algebraic canonicalizer.

Canonicalizes provably pure, side-effect-free expressions:
  - Commutative & associative binary operators (+, *, &, |, ^, ==, !=)
  - Relational dual operators (b > a -> a < b, b >= a -> a <= b)
  - Unordered collection literals (ExprSet and pure ExprDict)
-}
module Canontra.Normalize.Algebraic
  ( normalizeAlgebraicProgram
  , normalizeAlgebraicModule
  , normalizeAlgebraicDecl
  , normalizeAlgebraicStmt
  , normalizeAlgebraicExpr
  , normalizeBinaryExpr
  , normalizeCollectionLiteral
  , isProvablyPureExpr
  , isCommutativeOp
  , hashExprB
  ) where

import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Word (Word64)

import Canontra.Canonical.Serialize (canonicalizeExpr)
import Canontra.IR.Declaration
  ( Class (..)
  , Declaration (..)
  , Function (..)
  , Impl (..)
  , Struct (..)
  , Trait (..)
  )
import Canontra.IR.Expression
  ( CompFor (..)
  , Expr (..)
  , FStringPart (..)
  , Lit (..)
  , MatchCase (..)
  , Op (..)
  , SelectCase (..)
  , Stmt (..)
  )
import Canontra.IR.Program
  ( Module (..)
  , Program (..)
  )

-- | Compute a deterministic 64-bit FNV-1a hash of an 'Expr' for canonical ordering.
hashExprB :: Expr -> Word64
hashExprB = fnv1aHash64 . canonicalizeExpr
  where
    fnv1aHash64 :: BS.ByteString -> Word64
    fnv1aHash64 =
      BS.foldl'
        (\h b -> (h `xor` fromIntegral b) * 1099511628211)
        14695981039346656037

-- | Check whether an expression is provably pure (free of function calls,
-- method invocations, walrus assignments, await, yield, or macro invocations).
isProvablyPureExpr :: Expr -> Bool
isProvablyPureExpr = \case
  ExprLit _             -> True
  ExprId _              -> True
  ExprBinary _ e1 e2    -> isProvablyPureExpr e1 && isProvablyPureExpr e2
  ExprUnary _ e         -> isProvablyPureExpr e
  ExprTuple es          -> all isProvablyPureExpr es
  ExprList es           -> all isProvablyPureExpr es
  ExprSet es            -> all isProvablyPureExpr es
  ExprDict pairs        -> all (\(k, v) -> isProvablyPureExpr k && isProvablyPureExpr v) pairs
  ExprAttr e _          -> isProvablyPureExpr e
  ExprOptChain e _      -> isProvablyPureExpr e
  ExprNullish e1 e2     -> isProvablyPureExpr e1 && isProvablyPureExpr e2
  ExprSubscript e1 e2   -> isProvablyPureExpr e1 && isProvablyPureExpr e2
  ExprTernary c t f     -> isProvablyPureExpr c && isProvablyPureExpr t && isProvablyPureExpr f
  _                     -> False

-- | Check whether an expression is a string, bytes, or sequence literal for
-- which '+' represents non-commutative concatenation rather than addition.
isConcatenationOperand :: Expr -> Bool
isConcatenationOperand = \case
  ExprLit (LitString _)   -> True
  ExprLit (LitBytes _)    -> True
  ExprFormattedString _   -> True
  ExprList _              -> True
  ExprTuple _             -> True
  ExprBinary OpAdd a b    -> isConcatenationOperand a || isConcatenationOperand b
  _                       -> False

-- | True for commutative binary operators.
isCommutativeOp :: Op -> Bool
isCommutativeOp = \case
  OpAdd    -> True
  OpMul    -> True
  OpBitAnd -> True
  OpBitOr  -> True
  OpBitXor -> True
  OpEq     -> True
  OpNotEq  -> True
  _        -> False

-- | True for operators that are both associative and commutative on pure expressions.
isAssociativeCommutativeOp :: Op -> Bool
isAssociativeCommutativeOp = \case
  OpAdd    -> True
  OpMul    -> True
  OpBitAnd -> True
  OpBitOr  -> True
  OpBitXor -> True
  _        -> False

-- | Apply algebraic normalization across an entire 'Program'.
normalizeAlgebraicProgram :: Program -> Program
normalizeAlgebraicProgram (Program modules lang) =
  Program (map normalizeAlgebraicModule modules) lang

-- | Apply algebraic normalization across a 'Module'.
normalizeAlgebraicModule :: Module -> Module
normalizeAlgebraicModule (Module name imps decls stmts) =
  Module
    name
    imps
    (map normalizeAlgebraicDecl decls)
    (map normalizeAlgebraicStmt stmts)

-- | Apply algebraic normalization to a 'Declaration'.
normalizeAlgebraicDecl :: Declaration -> Declaration
normalizeAlgebraicDecl = \case
  DeclFunction fn ->
    DeclFunction (normalizeFunction fn)
  DeclClass cls ->
    DeclClass cls { clsMethods = map normalizeFunction (clsMethods cls) }
  DeclStruct st ->
    DeclStruct st { stMethods = map normalizeFunction (stMethods st) }
  DeclTrait tr ->
    DeclTrait tr { trMethods = map normalizeFunction (trMethods tr) }
  DeclImpl imp ->
    DeclImpl imp { impMethods = map normalizeFunction (impMethods imp) }
  DeclReceiver rc fn ->
    DeclReceiver rc (normalizeFunction fn)
  other -> other
  where
    normalizeFunction fn =
      fn { fnBody = map normalizeAlgebraicStmt (fnBody fn) }

-- | Apply algebraic normalization to a 'Stmt'.
normalizeAlgebraicStmt :: Stmt -> Stmt
normalizeAlgebraicStmt = \case
  StmtAssign targets val ->
    StmtAssign (map normalizeAlgebraicExpr targets) (normalizeAlgebraicExpr val)
  StmtAugAssign target op val ->
    StmtAugAssign (normalizeAlgebraicExpr target) op (normalizeAlgebraicExpr val)
  StmtAnnAssign target ann mVal ->
    StmtAnnAssign (normalizeAlgebraicExpr target) ann (fmap normalizeAlgebraicExpr mVal)
  StmtReturn mExpr ->
    StmtReturn (fmap normalizeAlgebraicExpr mExpr)
  StmtExpr expr ->
    StmtExpr (normalizeAlgebraicExpr expr)
  StmtIf cond thenB elseB ->
    StmtIf
      (normalizeAlgebraicExpr cond)
      (map normalizeAlgebraicStmt thenB)
      (map normalizeAlgebraicStmt elseB)
  StmtWhile cond body elseB ->
    StmtWhile
      (normalizeAlgebraicExpr cond)
      (map normalizeAlgebraicStmt body)
      (map normalizeAlgebraicStmt elseB)
  StmtFor target iter body elseB ->
    StmtFor
      (normalizeAlgebraicExpr target)
      (normalizeAlgebraicExpr iter)
      (map normalizeAlgebraicStmt body)
      (map normalizeAlgebraicStmt elseB)
  StmtAsyncFor target iter body elseB ->
    StmtAsyncFor
      (normalizeAlgebraicExpr target)
      (normalizeAlgebraicExpr iter)
      (map normalizeAlgebraicStmt body)
      (map normalizeAlgebraicStmt elseB)
  StmtWith items body ->
    StmtWith
      [ (normalizeAlgebraicExpr ctx, fmap normalizeAlgebraicExpr mVar)
      | (ctx, mVar) <- items
      ]
      (map normalizeAlgebraicStmt body)
  StmtAsyncWith items body ->
    StmtAsyncWith
      [ (normalizeAlgebraicExpr ctx, fmap normalizeAlgebraicExpr mVar)
      | (ctx, mVar) <- items
      ]
      (map normalizeAlgebraicStmt body)
  StmtTry tryB handlers elseB finB ->
    StmtTry
      (map normalizeAlgebraicStmt tryB)
      [ (fmap normalizeAlgebraicExpr mExc, mAlias, map normalizeAlgebraicStmt hBody)
      | (mExc, mAlias, hBody) <- handlers
      ]
      (map normalizeAlgebraicStmt elseB)
      (map normalizeAlgebraicStmt finB)
  StmtRaise mExc mCause ->
    StmtRaise (fmap normalizeAlgebraicExpr mExc) (fmap normalizeAlgebraicExpr mCause)
  StmtAssert test mMsg ->
    StmtAssert (normalizeAlgebraicExpr test) (fmap normalizeAlgebraicExpr mMsg)
  StmtDelete targets ->
    StmtDelete (map normalizeAlgebraicExpr targets)
  StmtMatch subject cases ->
    StmtMatch
      (normalizeAlgebraicExpr subject)
      [ MatchCase
          (normalizeAlgebraicExpr pat)
          (fmap normalizeAlgebraicExpr mGuard)
          (map normalizeAlgebraicStmt body)
      | MatchCase pat mGuard body <- cases
      ]
  StmtSwitch subject cases defStmts ->
    StmtSwitch
      (normalizeAlgebraicExpr subject)
      [ (normalizeAlgebraicExpr cExpr, map normalizeAlgebraicStmt cStmts)
      | (cExpr, cStmts) <- cases
      ]
      (map normalizeAlgebraicStmt defStmts)
  StmtSelect clauses ->
    StmtSelect
      [ (normalizeComm comm, map normalizeAlgebraicStmt body)
      | (comm, body) <- clauses
      ]
  StmtChanSend ch val ->
    StmtChanSend (normalizeAlgebraicExpr ch) (normalizeAlgebraicExpr val)
  StmtGo expr ->
    StmtGo (normalizeAlgebraicExpr expr)
  StmtDefer expr ->
    StmtDefer (normalizeAlgebraicExpr expr)
  StmtLoop body ->
    StmtLoop (map normalizeAlgebraicStmt body)
  other -> other
  where
    normalizeComm = \case
      SelectSend ch val -> SelectSend (normalizeAlgebraicExpr ch) (normalizeAlgebraicExpr val)
      SelectRecv mV ch  -> SelectRecv mV (normalizeAlgebraicExpr ch)
      SelectDefault     -> SelectDefault

-- | Recursively canonicalize an 'Expr' bottom-up using algebraic identities.
normalizeAlgebraicExpr :: Expr -> Expr
normalizeAlgebraicExpr = \case
  ExprLit lit -> ExprLit lit
  ExprId ident -> ExprId ident
  ExprBinary op lhs rhs ->
    let lhs' = normalizeAlgebraicExpr lhs
        rhs' = normalizeAlgebraicExpr rhs
    in normalizeBinaryExpr op lhs' rhs'
  ExprUnary op operand ->
    let op' = normalizeAlgebraicExpr operand
    in case (op, op') of
         (OpNot, ExprUnary OpNot inner) -> inner
         _                              -> ExprUnary op op'
  ExprCall callee args kwArgs ->
    ExprCall
      (normalizeAlgebraicExpr callee)
      (map normalizeAlgebraicExpr args)
      [(k, normalizeAlgebraicExpr v) | (k, v) <- kwArgs]
  ExprLambda params body ->
    ExprLambda params (normalizeAlgebraicExpr body)
  ExprTernary cond thenE elseE ->
    ExprTernary
      (normalizeAlgebraicExpr cond)
      (normalizeAlgebraicExpr thenE)
      (normalizeAlgebraicExpr elseE)
  ExprList items ->
    ExprList (map normalizeAlgebraicExpr items)
  ExprTuple items ->
    ExprTuple (map normalizeAlgebraicExpr items)
  ExprSet items ->
    normalizeCollectionLiteral (ExprSet (map normalizeAlgebraicExpr items))
  ExprDict pairs ->
    normalizeCollectionLiteral
      (ExprDict [(normalizeAlgebraicExpr k, normalizeAlgebraicExpr v) | (k, v) <- pairs])
  ExprListComp elt comps ->
    ExprListComp (normalizeAlgebraicExpr elt) (map normalizeComp comps)
  ExprSetComp elt comps ->
    ExprSetComp (normalizeAlgebraicExpr elt) (map normalizeComp comps)
  ExprGenerator elt comps ->
    ExprGenerator (normalizeAlgebraicExpr elt) (map normalizeComp comps)
  ExprDictComp kExpr vExpr comps ->
    ExprDictComp
      (normalizeAlgebraicExpr kExpr)
      (normalizeAlgebraicExpr vExpr)
      (map normalizeComp comps)
  ExprAttr obj attr ->
    ExprAttr (normalizeAlgebraicExpr obj) attr
  ExprOptChain obj attr ->
    ExprOptChain (normalizeAlgebraicExpr obj) attr
  ExprNullish lhs rhs ->
    ExprNullish (normalizeAlgebraicExpr lhs) (normalizeAlgebraicExpr rhs)
  ExprSubscript obj idx ->
    ExprSubscript (normalizeAlgebraicExpr obj) (normalizeAlgebraicExpr idx)
  ExprSlice mLower mUpper mStep ->
    ExprSlice
      (fmap normalizeAlgebraicExpr mLower)
      (fmap normalizeAlgebraicExpr mUpper)
      (fmap normalizeAlgebraicExpr mStep)
  ExprAwait operand ->
    ExprAwait (normalizeAlgebraicExpr operand)
  ExprYield mOperand ->
    ExprYield (fmap normalizeAlgebraicExpr mOperand)
  ExprYieldFrom operand ->
    ExprYieldFrom (normalizeAlgebraicExpr operand)
  ExprStarred operand ->
    ExprStarred (normalizeAlgebraicExpr operand)
  ExprKwStarred operand ->
    ExprKwStarred (normalizeAlgebraicExpr operand)
  ExprWalrus name val ->
    ExprWalrus name (normalizeAlgebraicExpr val)
  ExprFormattedString parts ->
    ExprFormattedString (map normalizeFPart parts)
  ExprChanRecv ch ->
    ExprChanRecv (normalizeAlgebraicExpr ch)
  ExprTryOp operand ->
    ExprTryOp (normalizeAlgebraicExpr operand)
  ExprJSX tag attrs children ->
    ExprJSX
      tag
      [(k, normalizeAlgebraicExpr v) | (k, v) <- attrs]
      (map normalizeAlgebraicExpr children)
  ExprMacroCall name args ->
    ExprMacroCall name (map normalizeAlgebraicExpr args)
  where
    normalizeComp (CompFor tgt iter ifs) =
      CompFor
        (normalizeAlgebraicExpr tgt)
        (normalizeAlgebraicExpr iter)
        (map normalizeAlgebraicExpr ifs)
    normalizeFPart = \case
      FStringText txt -> FStringText txt
      FStringExpr e mC mS -> FStringExpr (normalizeAlgebraicExpr e) mC mS

-- | Normalize a binary expression (assuming children are already normalized):
--   1. Relational duals: @b > a -> a < b@, @b >= a -> a <= b@ (when pure)
--   2. Associative + commutative flattening and sorting for @+, *, &, |, ^@ (when pure)
--   3. Commutative sorting for @==, !=@ (when pure)
normalizeBinaryExpr :: Op -> Expr -> Expr -> Expr
normalizeBinaryExpr op e1 e2 = case op of
  OpGt
    | isProvablyPureExpr e1 && isProvablyPureExpr e2 ->
        ExprBinary OpLt e2 e1
    | otherwise ->
        ExprBinary OpGt e1 e2

  OpGtE
    | isProvablyPureExpr e1 && isProvablyPureExpr e2 ->
        ExprBinary OpLtE e2 e1
    | otherwise ->
        ExprBinary OpGtE e1 e2

  _
    | isAssociativeCommutativeOp op && canCommute op e1 e2 ->
        let terms = flattenAssocOp op e1 ++ flattenAssocOp op e2
            sortedTerms = sortBy (comparing hashExprB) terms
        in case sortedTerms of
             (t:ts) -> foldl (ExprBinary op) t ts
             []     -> ExprBinary op e1 e2
    | isCommutativeOp op && canCommute op e1 e2 ->
        if hashExprB e1 <= hashExprB e2
          then ExprBinary op e1 e2
          else ExprBinary op e2 e1
    | otherwise ->
        ExprBinary op e1 e2
  where
    canCommute OpAdd a b =
      isProvablyPureExpr a
      && isProvablyPureExpr b
      && not (isConcatenationOperand a)
      && not (isConcatenationOperand b)
    canCommute _ a b =
      isProvablyPureExpr a && isProvablyPureExpr b

    flattenAssocOp targetOp expr = case expr of
      ExprBinary innerOp a b
        | innerOp == targetOp && canCommute targetOp a b ->
            flattenAssocOp targetOp a ++ flattenAssocOp targetOp b
      _ -> [expr]

-- | Normalize unordered collection literals ('ExprSet' and 'ExprDict') by sorting
-- elements/entries by their canonical FNV-1a structural hash when all elements are pure.
normalizeCollectionLiteral :: Expr -> Expr
normalizeCollectionLiteral = \case
  ExprSet items
    | all isProvablyPureExpr items ->
        ExprSet (sortBy (comparing hashExprB) items)
    | otherwise ->
        ExprSet items
  ExprDict pairs
    | all (\(k, v) -> isProvablyPureExpr k && isProvablyPureExpr v) pairs ->
        ExprDict (sortBy (comparing (\(k, v) -> (hashExprB k, hashExprB v))) pairs)
    | otherwise ->
        ExprDict pairs
  other -> other