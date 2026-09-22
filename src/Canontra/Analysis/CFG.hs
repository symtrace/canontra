{- |
Module      : Canontra.Analysis.CFG
Description : Control-Flow Graph (CFG) builder and basic block partitioner.

This module partitions function bodies into basic blocks, tracks conditional
branch edges, loop back-edges, switches, match cases, and exception jumps
in linear time O(|V| + |E|).
-}
module Canontra.Analysis.CFG
  ( BlockId
  , BranchCondition (..)
  , BlockTerminator (..)
  , BasicBlock (..)
  , CFGEdge (..)
  , ControlFlowGraph (..)
  , buildCFGs
  , buildFunctionCFG
  , formatCFG
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Canontra.IR.Declaration
import Canontra.IR.Expression
import Canontra.IR.Program

type BlockId = Int

data BranchCondition
  = CondTrue Expr
  | CondFalse Expr
  | CondCase Expr
  | CondDefault
  | CondUnconditional
  | CondException Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data BlockTerminator
  = TermReturn (Maybe Expr)
  | TermBranch Expr BlockId BlockId
  | TermJump BlockId
  | TermSwitch Expr [(Expr, BlockId)] (Maybe BlockId)
  | TermRaise (Maybe Expr)
  | TermExit
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data BasicBlock = BasicBlock
  { bbId         :: BlockId
  , bbStatements :: [Stmt]
  , bbTerminator :: BlockTerminator
  } deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data CFGEdge = CFGEdge
  { edgeFrom      :: BlockId
  , edgeTo        :: BlockId
  , edgeCondition :: BranchCondition
  } deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data ControlFlowGraph = ControlFlowGraph
  { cfgFunction :: Text
  , cfgEntry    :: BlockId
  , cfgBlocks   :: [BasicBlock]
  , cfgEdges    :: [CFGEdge]
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Build CFGs for all functions, methods, and receivers in a Program.
buildCFGs :: Program -> [ControlFlowGraph]
buildCFGs (Program modules _) =
  concatMap extractModuleCFGs modules

extractModuleCFGs :: Module -> [ControlFlowGraph]
extractModuleCFGs (Module modName _ decls stmts) =
  let topCFG = if null stmts then [] else [buildFunctionCFG ("<top-level:" <> modName <> ">") stmts]
      declCFGs = concatMap extractDeclCFGs decls
  in topCFG ++ declCFGs

extractDeclCFGs :: Declaration -> [ControlFlowGraph]
extractDeclCFGs decl = case decl of
  DeclFunction fn ->
    [buildFunctionCFG (fnName fn) (fnBody fn)]
  DeclClass cls ->
    [buildFunctionCFG (clsName cls <> "." <> fnName m) (fnBody m) | m <- clsMethods cls]
  DeclStruct st ->
    [buildFunctionCFG (stName st <> "." <> fnName m) (fnBody m) | m <- stMethods st]
  DeclTrait tr ->
    [buildFunctionCFG (trName tr <> "." <> fnName m) (fnBody m) | m <- trMethods tr]
  DeclImpl imp ->
    [buildFunctionCFG (impTarget imp <> "." <> fnName m) (fnBody m) | m <- impMethods imp]
  DeclReceiver rc fn ->
    [buildFunctionCFG (rcTypeName rc <> "." <> fnName fn) (fnBody fn)]
  _ -> []

-- | Build a ControlFlowGraph for a sequence of statements with entry block 0.
buildFunctionCFG :: Text -> [Stmt] -> ControlFlowGraph
buildFunctionCFG fnName stmts =
  let (blocks, edges, _) = partitionBlocks 0 stmts 1
      sortedBlocks = sortBy (comparing bbId) blocks
      sortedEdges  = sortBy (comparing (\e -> (edgeFrom e, edgeTo e, edgeCondition e))) edges
  in ControlFlowGraph fnName 0 sortedBlocks sortedEdges

partitionBlocks :: BlockId -> [Stmt] -> BlockId -> ([BasicBlock], [CFGEdge], BlockId)
partitionBlocks curId [] nextId =
  ([BasicBlock curId [] TermExit], [], nextId)

partitionBlocks curId (s:ss) nextId = case s of
  StmtReturn me ->
    let block = BasicBlock curId [StmtReturn me] (TermReturn me)
    in ([block], [], nextId)

  StmtRaise me _ ->
    let block = BasicBlock curId [StmtRaise me Nothing] (TermRaise me)
    in ([block], [], nextId)

  StmtIf cond thenStmts elseStmts ->
    let thenBlockId = nextId
        (thenBlocks, thenEdges, nextId1) = partitionBlocks thenBlockId thenStmts (thenBlockId + 1)
        elseBlockId = nextId1
        (elseBlocks, elseEdges, nextId2) = partitionBlocks elseBlockId elseStmts (elseBlockId + 1)
        joinBlockId = nextId2
        (joinBlocks, joinEdges, nextId3) = partitionBlocks joinBlockId ss (joinBlockId + 1)

        (condBlocks, condEdges, nextId4) = decomposeCondition curId cond thenBlockId elseBlockId nextId3
        joinFromThen = [CFGEdge thenBlockId joinBlockId CondUnconditional | not (null ss)]
        joinFromElse = [CFGEdge elseBlockId joinBlockId CondUnconditional | not (null ss)]

        allBlocks = condBlocks ++ thenBlocks ++ elseBlocks ++ joinBlocks
        allEdges  = condEdges ++ joinFromThen ++ joinFromElse ++ thenEdges ++ elseEdges ++ joinEdges
    in (allBlocks, allEdges, nextId4)

  StmtWhile cond bodyStmts elseStmts ->
    let bodyBlockId = nextId
        (bodyBlocks, bodyEdges, nextId1) = partitionBlocks bodyBlockId bodyStmts (bodyBlockId + 1)
        exitBlockId = nextId1
        (exitBlocks, exitEdges, nextId2) = partitionBlocks exitBlockId (elseStmts ++ ss) (exitBlockId + 1)

        (condBlocks, condEdges, nextId3) = decomposeCondition curId cond bodyBlockId exitBlockId nextId2
        edges = condEdges ++ [CFGEdge bodyBlockId curId CondUnconditional] ++ bodyEdges ++ exitEdges
        blocks = condBlocks ++ bodyBlocks ++ exitBlocks
    in (blocks, edges, nextId3)

  StmtFor _ iter bodyStmts elseStmts ->
    let bodyBlockId = nextId
        (bodyBlocks, bodyEdges, nextId1) = partitionBlocks bodyBlockId bodyStmts (bodyBlockId + 1)
        exitBlockId = nextId1
        (exitBlocks, exitEdges, nextId2) = partitionBlocks exitBlockId (elseStmts ++ ss) (exitBlockId + 1)

        headerBlock = BasicBlock curId [] (TermBranch iter bodyBlockId exitBlockId)
        edges =
          [ CFGEdge curId bodyBlockId (CondTrue iter)
          , CFGEdge curId exitBlockId (CondFalse iter)
          , CFGEdge bodyBlockId curId CondUnconditional
          ] ++ bodyEdges ++ exitEdges
        blocks = headerBlock : (bodyBlocks ++ exitBlocks)
    in (blocks, edges, nextId2)

  StmtLoop bodyStmts ->
    let bodyBlockId = nextId
        (bodyBlocks, bodyEdges, nextId1) = partitionBlocks bodyBlockId bodyStmts (bodyBlockId + 1)
        headerBlock = BasicBlock curId [] (TermJump bodyBlockId)
        edges = [CFGEdge curId bodyBlockId CondUnconditional, CFGEdge bodyBlockId curId CondUnconditional] ++ bodyEdges
    in (headerBlock : bodyBlocks, edges, nextId1)

  StmtSwitch expr cases defaultStmts ->
    let (caseBlocks, caseEdges, caseTerms, nextId1) = foldl stepCase ([], [], [], nextId) cases
        exitBlockId = nextId1
        (defBlocks, defEdges, nextId2) = partitionBlocks exitBlockId (defaultStmts ++ ss) (exitBlockId + 1)
        headerBlock = BasicBlock curId [] (TermSwitch expr caseTerms (Just exitBlockId))
        caseHeaderEdges = [CFGEdge curId bId (CondCase cExpr) | (cExpr, bId) <- caseTerms]
        defHeaderEdge = CFGEdge curId exitBlockId CondDefault
        allBlocks = headerBlock : (caseBlocks ++ defBlocks)
        allEdges = caseHeaderEdges ++ [defHeaderEdge] ++ caseEdges ++ defEdges
    in (allBlocks, allEdges, nextId2)
    where
      stepCase (bAcc, eAcc, tAcc, nId) (cExpr, cStmts) =
        let cBlockId = nId
            (cBlocks, cEdges, nId1) = partitionBlocks cBlockId cStmts (cBlockId + 1)
        in (bAcc ++ cBlocks, eAcc ++ cEdges, tAcc ++ [(cExpr, cBlockId)], nId1)

  StmtMatch expr cases ->
    let (caseBlocks, caseEdges, caseTerms, nextId1) = foldl stepCase ([], [], [], nextId) cases
        exitBlockId = nextId1
        (exitBlocks, exitEdges, nextId2) = partitionBlocks exitBlockId ss (exitBlockId + 1)
        headerBlock = BasicBlock curId [] (TermSwitch expr caseTerms (Just exitBlockId))
        caseHeaderEdges = [CFGEdge curId bId (CondCase cExpr) | (cExpr, bId) <- caseTerms]
        allBlocks = headerBlock : (caseBlocks ++ exitBlocks)
        allEdges = caseHeaderEdges ++ caseEdges ++ exitEdges
    in (allBlocks, allEdges, nextId2)
    where
      stepCase (bAcc, eAcc, tAcc, nId) mc =
        let cBlockId = nId
            (cBlocks, cEdges, nId1) = partitionBlocks cBlockId (mcBody mc) (cBlockId + 1)
        in (bAcc ++ cBlocks, eAcc ++ cEdges, tAcc ++ [(mcPattern mc, cBlockId)], nId1)

  StmtTry tryBody handlers elseBody finallyBody ->
    let joinBlockId = nextId
        (joinBlocks, joinEdges, nextId1) = partitionBlocks joinBlockId ss (joinBlockId + 1)

        (hasFinally, finBlockId, finBlocks, finEdges, nextId2) =
          if null finallyBody
          then (False, joinBlockId, [], [], nextId1)
          else
            let fId = nextId1
                (fBlocks, fEdges, n2) = partitionBlocks fId finallyBody (fId + 1)
                fJoinEdge = [CFGEdge fId joinBlockId CondUnconditional | not (null ss)]
            in (True, fId, fBlocks, fEdges ++ fJoinEdge, n2)

        afterTryTarget = finBlockId

        (elseBlockId, elseBlocks, elseEdges, nextId3) =
          if null elseBody
          then (afterTryTarget, [], [], nextId2)
          else
            let eId = nextId2
                (eBlocks, eEdges, n3) = partitionBlocks eId elseBody (eId + 1)
                eTargetEdge = [CFGEdge eId afterTryTarget CondUnconditional]
            in (eId, eBlocks, eEdges ++ eTargetEdge, n3)

        (caseBlocks, caseEdges, handlerEntries, nextId4) =
          foldl (stepHandler afterTryTarget) ([], [], [], nextId3) handlers

        tryBlockId = nextId4
        (tryBlocks, tryEdges, nextId5) = partitionBlocks tryBlockId tryBody (tryBlockId + 1)

        entryBlock = BasicBlock curId [] (TermJump tryBlockId)
        entryEdge = CFGEdge curId tryBlockId CondUnconditional

        tryToElseEdge = [CFGEdge tryBlockId elseBlockId CondUnconditional]

        handlerEdges =
          [ CFGEdge tryBlockId hId (CondException (formatExcExpr mExpr))
          | (mExpr, hId) <- handlerEntries
          ]

        unwindEdge =
          [ CFGEdge tryBlockId finBlockId (CondException "*")
          | hasFinally
          ]

        allBlocks = entryBlock : (tryBlocks ++ elseBlocks ++ caseBlocks ++ finBlocks ++ joinBlocks)
        allEdges =
          [entryEdge]
          ++ tryToElseEdge
          ++ handlerEdges
          ++ unwindEdge
          ++ tryEdges
          ++ elseEdges
          ++ caseEdges
          ++ finEdges
          ++ joinEdges
    in (allBlocks, allEdges, nextId5)
    where
      stepHandler targetId (bAcc, eAcc, hAcc, nId) (mExcExpr, _, hStmts) =
        let hBlockId = nId
            (hBlocks, hEdges, nId1) = partitionBlocks hBlockId hStmts (hBlockId + 1)
            hExitEdge = [CFGEdge hBlockId targetId CondUnconditional]
        in (bAcc ++ hBlocks, eAcc ++ hEdges ++ hExitEdge, hAcc ++ [(mExcExpr, hBlockId)], nId1)

  _ ->
    -- Collect non-branching statements into current block
    let (linear, rest) = span isLinearStmt (s:ss)
    in case rest of
      [] ->
        ([BasicBlock curId linear TermExit], [], nextId)
      (r:rs) ->
        let nextBlockId = nextId
            (nextBlocks, nextEdges, nextId1) = partitionBlocks nextBlockId (r:rs) (nextBlockId + 1)
            thisBlock = BasicBlock curId linear (TermJump nextBlockId)
            edge = CFGEdge curId nextBlockId CondUnconditional
        in (thisBlock : nextBlocks, edge : nextEdges, nextId1)

decomposeCondition :: BlockId -> Expr -> BlockId -> BlockId -> BlockId -> ([BasicBlock], [CFGEdge], BlockId)
decomposeCondition curBId (ExprBinary OpAnd left right) trueTarget falseTarget nextAvailId =
  let rightBlockId = nextAvailId
      (leftBlocks, leftEdges, nextId1) = decomposeCondition curBId left rightBlockId falseTarget (rightBlockId + 1)
      (rightBlocks, rightEdges, nextId2) = decomposeCondition rightBlockId right trueTarget falseTarget nextId1
  in (leftBlocks ++ rightBlocks, leftEdges ++ rightEdges, nextId2)
decomposeCondition curBId (ExprBinary OpOr left right) trueTarget falseTarget nextAvailId =
  let rightBlockId = nextAvailId
      (leftBlocks, leftEdges, nextId1) = decomposeCondition curBId left trueTarget rightBlockId (rightBlockId + 1)
      (rightBlocks, rightEdges, nextId2) = decomposeCondition rightBlockId right trueTarget falseTarget nextId1
  in (leftBlocks ++ rightBlocks, leftEdges ++ rightEdges, nextId2)
decomposeCondition curBId expr trueTarget falseTarget nextAvailId =
  let thisBlock = BasicBlock curBId [] (TermBranch expr trueTarget falseTarget)
      branchEdges =
        [ CFGEdge curBId trueTarget (CondTrue expr)
        , CFGEdge curBId falseTarget (CondFalse expr)
        ]
  in ([thisBlock], branchEdges, nextAvailId)

formatExcExpr :: Maybe Expr -> Text
formatExcExpr Nothing = "*"
formatExcExpr (Just (ExprId name)) = name
formatExcExpr (Just (ExprAttr _ name)) = name
formatExcExpr (Just _) = "*"

isLinearStmt :: Stmt -> Bool
isLinearStmt = \case
  StmtIf {}       -> False
  StmtWhile {}    -> False
  StmtFor {}      -> False
  StmtAsyncFor {} -> False
  StmtLoop {}     -> False
  StmtReturn {}   -> False
  StmtRaise {}    -> False
  StmtMatch {}    -> False
  StmtSwitch {}   -> False
  StmtTry {}      -> False
  _               -> True

formatCFG :: ControlFlowGraph -> Text
formatCFG cfg =
  T.unlines $
    [ "CFG: " <> cfgFunction cfg <> " (Blocks: " <> T.pack (show (length (cfgBlocks cfg))) <> ", Edges: " <> T.pack (show (length (cfgEdges cfg))) <> ")"
    , "---------------------------------------------------------"
    ] ++
    map formatBlock (cfgBlocks cfg) ++
    [ "Edges:" ] ++
    map formatEdge (cfgEdges cfg)
  where
    formatBlock b =
      "  [Block " <> T.pack (show (bbId b)) <> "] (" <> T.pack (show (length (bbStatements b))) <> " stmts) -> " <> formatTerm (bbTerminator b)

    formatTerm = \case
      TermReturn _       -> "Return"
      TermBranch _ t f   -> "Branch -> True: " <> T.pack (show t) <> ", False: " <> T.pack (show f)
      TermJump j         -> "Jump -> " <> T.pack (show j)
      TermSwitch _ _ _   -> "Switch"
      TermRaise _        -> "Raise"
      TermExit           -> "Exit"

    formatEdge e =
      "    " <> T.pack (show (edgeFrom e)) <> " ---> " <> T.pack (show (edgeTo e)) <> " [" <> formatCond (edgeCondition e) <> "]"

    formatCond = \case
      CondTrue _        -> "true"
      CondFalse _       -> "false"
      CondCase _        -> "case"
      CondDefault       -> "default"
      CondUnconditional -> "uncond"
      CondException ex  -> "except: " <> ex
