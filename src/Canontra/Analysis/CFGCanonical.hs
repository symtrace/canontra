{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Canontra.Analysis.CFGCanonical
Description : CFG dominator-tree, Reverse Post-Order (RPO), and branch inversion canonicalizer.

Canonicalizes Control-Flow Graphs (CFGs) and conditional statement structures before
computing the F_CF fingerprint:
  1. Guard-clause & inverted branch statement normalization:
     - @if (!c) { return E; } rest@ -> @if (c) { rest } else { return E; }@
     - @if (!c) { B } else { A }@   -> @if (c) { A } else { B }@
  2. CFG branch condition inversion normalization:
     - Flips @TermBranch (OpNot c) t f@ to @TermBranch c f t@ and swaps @CondTrue@/@CondFalse@ edges.
  3. Dead basic-block and spurious post-return fallthrough edge pruning.
  4. Deterministic Reverse Post-Order (RPO) topological basic-block relabeling (0 .. k-1).
-}
module Canontra.Analysis.CFGCanonical
  ( canonicalizeCFGRPO
  , normalizeBranchConditions
  , pruneDeadBlocks
  , relabelRPO
  , normalizeBranchProgram
  , normalizeBranchModule
  , normalizeBranchDecl
  , normalizeBranchStmts
  , terminatesUnconditionally
  ) where

import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set

import Canontra.Analysis.CFG
  ( BasicBlock (..)
  , BlockId
  , BlockTerminator (..)
  , BranchCondition (..)
  , CFGEdge (..)
  , ControlFlowGraph (..)
  )
import Canontra.IR.Declaration
  ( Class (..)
  , Declaration (..)
  , Function (..)
  , Impl (..)
  , Struct (..)
  , Trait (..)
  )
import Canontra.IR.Expression
  ( Expr (..)
  , MatchCase (..)
  , Op (..)
  , Stmt (..)
  )
import Canontra.IR.Program
  ( Module (..)
  , Program (..)
  )
import Canontra.Normalize.Algebraic ( isProvablyPureExpr, normalizeAlgebraicExpr, normalizeAlgebraicStmt )

-- ---------------------------------------------------------------------------
-- 1. AST-Level Guard-Clause & Branch Inversion Normal Form
-- ---------------------------------------------------------------------------

-- | Normalize guard clauses and inverted @if-else@ branches across a 'Program'.
normalizeBranchProgram :: Program -> Program
normalizeBranchProgram (Program modules entry) =
  Program (map normalizeBranchModule modules) entry

-- | Normalize guard clauses and inverted @if-else@ branches across a 'Module'.
normalizeBranchModule :: Module -> Module
normalizeBranchModule (Module name imps decls stmts) =
  Module
    name
    imps
    (map normalizeBranchDecl decls)
    (normalizeBranchStmts stmts)

-- | Normalize guard clauses and inverted @if-else@ branches in a 'Declaration'.
normalizeBranchDecl :: Declaration -> Declaration
normalizeBranchDecl = \case
  DeclFunction fn ->
    DeclFunction (normalizeFn fn)
  DeclClass cls ->
    DeclClass cls { clsMethods = map normalizeFn (clsMethods cls) }
  DeclStruct st ->
    DeclStruct st { stMethods = map normalizeFn (stMethods st) }
  DeclTrait tr ->
    DeclTrait tr { trMethods = map normalizeFn (trMethods tr) }
  DeclImpl imp ->
    DeclImpl imp { impMethods = map normalizeFn (impMethods imp) }
  DeclReceiver rc fn ->
    DeclReceiver rc (normalizeFn fn)
  other -> other
  where
    normalizeFn fn = fn { fnBody = normalizeBranchStmts (fnBody fn) }

-- | Check whether a statement block unconditionally exits control flow
-- (via @return@, @raise@/@throw@, @break@, or @continue@).
terminatesUnconditionally :: [Stmt] -> Bool
terminatesUnconditionally [] = False
terminatesUnconditionally stmts =
  case last stmts of
    StmtReturn _   -> True
    StmtRaise _ _  -> True
    StmtBreak      -> True
    StmtContinue   -> True
    StmtIf _ t e   -> not (null e) && terminatesUnconditionally t && terminatesUnconditionally e
    _              -> False

-- | Check whether a statement block is empty or contains only no-op pass statements.
isEmptyElse :: [Stmt] -> Bool
isEmptyElse b = null b || b == [StmtPass]

-- | Normalize a list of statements:
--   - Prunes unreachable statements following an unconditional terminator in the same block.
--   - Lifts guard-clause continuation statements into the @else@ branch when @then@ terminates unconditionally.
--   - Canonicalizes @if (!c) { B } else { A }@ into @if (c) { A } else { B }@ when both branches are present.
normalizeBranchStmts :: [Stmt] -> [Stmt]
normalizeBranchStmts [] = []
normalizeBranchStmts (stmt : rest) =
  case stmt of
    StmtReturn _  -> [stmt]
    StmtRaise _ _ -> [stmt]
    StmtBreak     -> [stmt]
    StmtContinue  -> [stmt]

    StmtIf cond thenB elseB ->
      let thenB' = normalizeBranchStmts thenB
          elseB' = normalizeBranchStmts elseB
          rest'  = normalizeBranchStmts rest
          -- Guard-clause lifting:
          -- If elseB' is empty, thenB' terminates unconditionally, and rest' is non-empty,
          -- then rest' executes if and only if cond is False (i.e. it is the else branch).
          (mergedThen, mergedElse, remainingRest)
            | isEmptyElse elseB' && not (null rest') && terminatesUnconditionally thenB' =
                (thenB', rest', [])
            | not (isEmptyElse elseB') && not (null rest') && terminatesUnconditionally elseB' && not (terminatesUnconditionally thenB') =
                (normalizeBranchStmts (thenB' ++ rest'), elseB', [])
            | otherwise =
                (thenB', elseB', rest')
          normalizedIf = invertIfCondition cond mergedThen mergedElse
      in normalizedIf : remainingRest

    StmtWhile cond body elseB ->
      StmtWhile cond (normalizeBranchStmts body) (normalizeBranchStmts elseB)
        : normalizeBranchStmts rest

    StmtFor target iter body elseB ->
      StmtFor target iter (normalizeBranchStmts body) (normalizeBranchStmts elseB)
        : normalizeBranchStmts rest

    StmtAsyncFor target iter body elseB ->
      StmtAsyncFor target iter (normalizeBranchStmts body) (normalizeBranchStmts elseB)
        : normalizeBranchStmts rest

    StmtWith items body ->
      StmtWith items (normalizeBranchStmts body) : normalizeBranchStmts rest

    StmtAsyncWith items body ->
      StmtAsyncWith items (normalizeBranchStmts body) : normalizeBranchStmts rest

    StmtTry tryB handlers elseB finB ->
      StmtTry
        (normalizeBranchStmts tryB)
        [(mExc, mAlias, normalizeBranchStmts hBody) | (mExc, mAlias, hBody) <- handlers]
        (normalizeBranchStmts elseB)
        (normalizeBranchStmts finB)
        : normalizeBranchStmts rest

    StmtMatch subject cases ->
      StmtMatch
        subject
        [mc { mcBody = normalizeBranchStmts (mcBody mc) } | mc <- cases]
        : normalizeBranchStmts rest

    StmtSwitch subject cases defStmts ->
      StmtSwitch
        subject
        [(cExpr, normalizeBranchStmts cStmts) | (cExpr, cStmts) <- cases]
        (normalizeBranchStmts defStmts)
        : normalizeBranchStmts rest

    StmtSelect clauses ->
      StmtSelect
        [(comm, normalizeBranchStmts body) | (comm, body) <- clauses]
        : normalizeBranchStmts rest

    StmtLoop body ->
      StmtLoop (normalizeBranchStmts body) : normalizeBranchStmts rest

    other ->
      other : normalizeBranchStmts rest

-- | Invert @if (!c) { thenB } else { elseB }@ to @if (c) { elseB } else { thenB }@
-- when @elseB@ is non-empty. Also normalizes @if (a != b) { thenB } else { elseB }@
-- to @if (a == b) { elseB } else { thenB }@ when @a@ and @b@ are provably pure.
invertIfCondition :: Expr -> [Stmt] -> [Stmt] -> Stmt
invertIfCondition cond thenB elseB
  | not (isEmptyElse elseB) =
      case stripConditionNegation cond of
        Just posCond -> StmtIf posCond elseB thenB
        Nothing      -> StmtIf cond thenB elseB
  | otherwise =
      StmtIf cond thenB elseB

stripConditionNegation :: Expr -> Maybe Expr
stripConditionNegation = \case
  ExprUnary OpNot inner ->
    -- Handle double negation: !!c -> c (without swapping branches), !c -> c (swapping)
    case stripConditionNegation inner of
      Just _  -> Nothing
      Nothing -> Just inner
  ExprBinary OpNotEq a b
    | isProvablyPureExpr a && isProvablyPureExpr b ->
        Just (ExprBinary OpEq a b)
  _ -> Nothing

-- ---------------------------------------------------------------------------
-- 2. CFG-Level Dominator / RPO & Branch Inversion Canonicalization
-- ---------------------------------------------------------------------------

-- | Canonicalize a 'ControlFlowGraph' by:
--   1. Normalizing inverted branch terminators and edge conditions
--   2. Pruning spurious post-terminator edges and unreachable basic blocks
--   3. Contracting trivial empty jump-only trampoline blocks
--   4. Relabeling all reachable basic blocks in deterministic Reverse Post-Order (RPO)
canonicalizeCFGRPO :: ControlFlowGraph -> ControlFlowGraph
canonicalizeCFGRPO =
    relabelRPO
  . contractTrampolineBlocks
  . pruneDeadBlocks
  . normalizeBranchConditions

-- | Normalize inverted branch conditions in 'BasicBlock' terminators and 'CFGEdge's,
-- and apply algebraic expression normalization to all statements and conditions.
normalizeBranchConditions :: ControlFlowGraph -> ControlFlowGraph
normalizeBranchConditions cfg =
  let blocks' = map normalizeBlock (cfgBlocks cfg)
      flippedBlocks =
        Map.fromList
          [ (bbId b, posCond)
          | b <- cfgBlocks cfg
          , TermBranch cond _ _ <- [bbTerminator b]
          , Just posCond <- [stripConditionNegation (normalizeAlgebraicExpr cond)]
          ]
      edges' = map (normalizeEdge flippedBlocks) (cfgEdges cfg)
  in cfg { cfgBlocks = blocks', cfgEdges = edges' }
  where
    normalizeBlock (BasicBlock bid stmts term) =
      let stmts' = map normalizeAlgebraicStmt stmts
          term' = case term of
            TermBranch cond tTgt fTgt ->
              let normCond = normalizeAlgebraicExpr cond
              in case stripConditionNegation normCond of
                   Just posCond -> TermBranch posCond fTgt tTgt
                   Nothing      -> TermBranch normCond tTgt fTgt
            TermReturn mE ->
              TermReturn (fmap normalizeAlgebraicExpr mE)
            TermRaise mE ->
              TermRaise (fmap normalizeAlgebraicExpr mE)
            TermSwitch subj cases mDef ->
              TermSwitch
                (normalizeAlgebraicExpr subj)
                [(normalizeAlgebraicExpr c, tgt) | (c, tgt) <- cases]
                mDef
            other -> other
      in BasicBlock bid stmts' term'

    normalizeEdge flippedMap (CFGEdge src tgt cond) =
      case Map.lookup src flippedMap of
        Just posCond ->
          case cond of
            CondTrue _  -> CFGEdge src tgt (CondFalse posCond)
            CondFalse _ -> CFGEdge src tgt (CondTrue posCond)
            other       -> CFGEdge src tgt (normalizeCondExpr other)
        Nothing ->
          CFGEdge src tgt (normalizeCondExpr cond)

    normalizeCondExpr = \case
      CondTrue e  -> CondTrue (normalizeAlgebraicExpr e)
      CondFalse e -> CondFalse (normalizeAlgebraicExpr e)
      CondCase e  -> CondCase (normalizeAlgebraicExpr e)
      other       -> other

-- | Prune spurious fallthrough edges leaving blocks that already returned/raised/exited,
-- and remove all basic blocks unreachable from 'cfgEntry'.
pruneDeadBlocks :: ControlFlowGraph -> ControlFlowGraph
pruneDeadBlocks cfg =
  let blockMap = Map.fromList [(bbId b, b) | b <- cfgBlocks cfg]
      isTerminalBlock bid =
        case Map.lookup bid blockMap of
          Just b -> case bbTerminator b of
            TermReturn _ -> True
            TermRaise _  -> True
            TermExit     -> True
            _            -> False
          Nothing -> True
      validEdges =
        [ e
        | e <- cfgEdges cfg
        , Map.member (edgeFrom e) blockMap
        , Map.member (edgeTo e) blockMap
        , not (isTerminalBlock (edgeFrom e) && edgeCondition e == CondUnconditional)
        ]
      adjMap =
        Map.fromListWith (++)
          [ (edgeFrom e, [edgeTo e])
          | e <- validEdges
          ]
      reachable = bfsReachable adjMap (Set.singleton (cfgEntry cfg)) [cfgEntry cfg]
      liveBlocks = [b | b <- cfgBlocks cfg, Set.member (bbId b) reachable]
      liveEdges =
        [ e
        | e <- validEdges
        , Set.member (edgeFrom e) reachable
        , Set.member (edgeTo e) reachable
        ]
  in cfg { cfgBlocks = liveBlocks, cfgEdges = liveEdges }
  where
    bfsReachable _ visited [] = visited
    bfsReachable adj visited (u:queue) =
      let succs = Map.findWithDefault [] u adj
          newSuccs = filter (`Set.notMember` visited) succs
          visited' = foldl (flip Set.insert) visited newSuccs
      in bfsReachable adj visited' (queue ++ newSuccs)

-- | Contract empty intermediate jump-only blocks (blocks with @null bbStmts@ and @TermJump tgt@)
-- that are not the entry block and do not form self-loops.
contractTrampolineBlocks :: ControlFlowGraph -> ControlFlowGraph
contractTrampolineBlocks cfg =
  let trampolines =
        Map.fromList
          [ (bbId b, tgt)
          | b <- cfgBlocks cfg
          , bbId b /= cfgEntry cfg
          , null (bbStatements b)
          , TermJump tgt <- [bbTerminator b]
          , tgt /= bbId b
          ]
      resolveTarget bid = go (Set.singleton bid) bid
        where
          go seen cur =
            case Map.lookup cur trampolines of
              Just next | Set.notMember next seen -> go (Set.insert next seen) next
              _ -> cur
  in if Map.null trampolines
       then cfg
       else
         let blocks' =
               [ b { bbTerminator = remapTerminator resolveTarget (bbTerminator b) }
               | b <- cfgBlocks cfg
               , Map.notMember (bbId b) trampolines
               ]
             edges' =
               [ CFGEdge (edgeFrom e) (resolveTarget (edgeTo e)) (edgeCondition e)
               | e <- cfgEdges cfg
               , Map.notMember (edgeFrom e) trampolines
               ]
         in pruneDeadBlocks (cfg { cfgBlocks = blocks', cfgEdges = edges' })

-- | Relabel all basic blocks and edges in deterministic Reverse Post-Order (RPO)
-- starting from 'cfgEntry' (assigned BlockId 0).
relabelRPO :: ControlFlowGraph -> ControlFlowGraph
relabelRPO cfg =
  let blockMap = Map.fromList [(bbId b, b) | b <- cfgBlocks cfg]
      -- Order outgoing edges deterministically: CondTrue before CondFalse, then cases, then unconditional, then exceptions
      outEdgesMap =
        Map.map (sortBy (comparing edgeOrderKey)) $
          Map.fromListWith (++)
            [ (edgeFrom e, [e])
            | e <- cfgEdges cfg
            ]
      (_, postOrder) = dfsPostOrder outEdgesMap Set.empty [] (cfgEntry cfg)
      rpoOrder = reverse postOrder
      -- Include any remaining blocks (if any) in deterministic ID order
      remainingIds =
        [ bbId b
        | b <- sortBy (comparing bbId) (cfgBlocks cfg)
        , bbId b `notElem` rpoOrder
        ]
      allOrderedIds = rpoOrder ++ remainingIds
      idRenameMap = Map.fromList (zip allOrderedIds [0 ..])
      renameId bid = Map.findWithDefault bid bid idRenameMap

      newBlocks =
        mapMaybe
          (\oldId -> do
            b <- Map.lookup oldId blockMap
            let newId = renameId oldId
            pure (BasicBlock newId (bbStatements b) (remapTerminator renameId (bbTerminator b))))
          allOrderedIds

      newEdges =
        sortBy (comparing (\e -> (edgeFrom e, edgeOrderKey e, edgeTo e)))
          [ CFGEdge (renameId (edgeFrom e)) (renameId (edgeTo e)) (edgeCondition e)
          | e <- cfgEdges cfg
          ]
  in cfg
       { cfgEntry  = renameId (cfgEntry cfg)
       , cfgBlocks = newBlocks
       , cfgEdges  = newEdges
       }
  where
    edgeOrderKey :: CFGEdge -> (Int, BranchCondition)
    edgeOrderKey e = case edgeCondition e of
      CondTrue c        -> (0, CondTrue c)
      CondFalse c       -> (1, CondFalse c)
      CondCase c        -> (2, CondCase c)
      CondDefault       -> (3, CondDefault)
      CondUnconditional -> (4, CondUnconditional)
      CondException ex  -> (5, CondException ex)

    dfsPostOrder
      :: Map BlockId [CFGEdge]
      -> Set BlockId
      -> [BlockId]
      -> BlockId
      -> (Set BlockId, [BlockId])
    dfsPostOrder adj visited post u
      | Set.member u visited = (visited, post)
      | otherwise =
          let visited1 = Set.insert u visited
              succs = map edgeTo (Map.findWithDefault [] u adj)
              (visited2, post2) =
                foldl
                  (\(visAcc, postAcc) v -> dfsPostOrder adj visAcc postAcc v)
                  (visited1, post)
                  succs
          in (visited2, u : post2)

remapTerminator :: (BlockId -> BlockId) -> BlockTerminator -> BlockTerminator
remapTerminator f = \case
  TermJump tgt             -> TermJump (f tgt)
  TermBranch c tTgt fTgt   -> TermBranch c (f tTgt) (f fTgt)
  TermSwitch s cases mDef  -> TermSwitch s [(c, f tgt) | (c, tgt) <- cases] (fmap f mDef)
  other                    -> other