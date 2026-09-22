{- |
Module      : Canontra.Analysis.DFG
Description : Data-Flow Graph (DFG) builder and reaching definitions analyzer.

This module extracts reaching definitions, Def-Use chains, and SSA-style
value flows across parameters, assignments, and expression evaluations.
-}
module Canontra.Analysis.DFG
  ( NodeId
  , DefUseKind (..)
  , DFGNode (..)
  , DFGEdge (..)
  , DataFlowGraph (..)
  , ScopeStack
  , buildDFGs
  , buildFunctionDFG
  , formatDFG
  , processStmts
  , processStmtsWithStack
  , extractExprUses
  , extractExprUsesStack
  , lookupStack
  , updateStack
  , pushScope
  , popScope
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Canontra.IR.Declaration
import Canontra.IR.Expression
import Canontra.IR.Program

type NodeId = Int

data DefUseKind
  = DefParam Int          -- e.g. Parameter at index i
  | DefAssignment Text    -- e.g. Variable assigned a value
  | DefPhi [NodeId]       -- e.g. SSA phi node
  | UseRead Text          -- e.g. Variable read/evaluated
  | UseArgument Int       -- e.g. Passed into function call
  | UseBranchGuard        -- e.g. Guard in branch condition
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data DFGNode = DFGNode
  { dfgNodeId :: NodeId
  , dfgKind   :: DefUseKind
  , dfgExpr   :: Maybe Expr
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data DFGEdge = DFGEdge
  { dfgSource  :: NodeId
  , dfgTarget  :: NodeId
  , dfgVarName :: Text
  } deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data DataFlowGraph = DataFlowGraph
  { dfgFunction :: Text
  , dfgNodes    :: [DFGNode]
  , dfgEdges    :: [DFGEdge]
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Build DFGs for all callable entities in a Program.
buildDFGs :: Program -> [DataFlowGraph]
buildDFGs (Program modules _) =
  concatMap extractModuleDFGs modules

extractModuleDFGs :: Module -> [DataFlowGraph]
extractModuleDFGs (Module modName _ decls stmts) =
  let topDFG = if null stmts then [] else [buildFunctionDFG ("<top-level:" <> modName <> ">") [] stmts]
      declDFGs = concatMap extractDeclDFGs decls
  in topDFG ++ declDFGs

extractDeclDFGs :: Declaration -> [DataFlowGraph]
extractDeclDFGs decl = case decl of
  DeclFunction fn ->
    [buildFunctionDFG (fnName fn) (fnParams fn) (fnBody fn)]
  DeclClass cls ->
    [buildFunctionDFG (clsName cls <> "." <> fnName m) (fnParams m) (fnBody m) | m <- clsMethods cls]
  DeclStruct st ->
    [buildFunctionDFG (stName st <> "." <> fnName m) (fnParams m) (fnBody m) | m <- stMethods st]
  DeclTrait tr ->
    [buildFunctionDFG (trName tr <> "." <> fnName m) (fnParams m) (fnBody m) | m <- trMethods tr]
  DeclImpl imp ->
    [buildFunctionDFG (impTarget imp <> "." <> fnName m) (fnParams m) (fnBody m) | m <- impMethods imp]
  DeclReceiver rc fn ->
    [buildFunctionDFG (rcTypeName rc <> "." <> fnName fn) (fnParams fn) (fnBody fn)]
  _ -> []

type ScopeStack = [Map Text NodeId]

lookupStack :: Text -> ScopeStack -> Maybe NodeId
lookupStack _ [] = Nothing
lookupStack v (s:ss) = case Map.lookup v s of
  Just nid -> Just nid
  Nothing  -> lookupStack v ss

updateStack :: Text -> NodeId -> ScopeStack -> ScopeStack
updateStack v nid [] = [Map.singleton v nid]
updateStack v nid (s:ss) = Map.insert v nid s : ss

pushScope :: ScopeStack -> ScopeStack
pushScope s = Map.empty : s

popScope :: ScopeStack -> ScopeStack
popScope (_:ss) = ss
popScope []     = []

-- | Build a DataFlowGraph for a function with parameters and body statements.
buildFunctionDFG :: Text -> [Parameter] -> [Stmt] -> DataFlowGraph
buildFunctionDFG fnName params stmts =
  let (paramNodes, initialDefs, nextId) = setupParams params 0
      (stmtNodes, stmtEdges, _, _) = processStmtsWithStack [initialDefs] stmts nextId
      allNodes = sortBy (comparing dfgNodeId) (paramNodes ++ stmtNodes)
      allEdges = sortBy (comparing (\e -> (dfgSource e, dfgTarget e, dfgVarName e))) stmtEdges
  in DataFlowGraph fnName allNodes allEdges

setupParams :: [Parameter] -> NodeId -> ([DFGNode], Map Text NodeId, NodeId)
setupParams params startId =
  foldl step ([], Map.empty, startId) (zip [0..] params)
  where
    step (nodes, defs, curId) (idx, p) =
      let pName = paramName p
          node = DFGNode curId (DefParam idx) (Just (ExprId pName))
          newDefs = Map.insert pName curId defs
      in (nodes ++ [node], newDefs, curId + 1)

processStmts :: Map Text NodeId -> [Stmt] -> NodeId -> ([DFGNode], [DFGEdge], NodeId)
processStmts activeDefs stmts startId =
  let (nodes, edges, _, nextId) = processStmtsWithStack [activeDefs] stmts startId
  in (nodes, edges, nextId)

processStmtsWithStack :: ScopeStack -> [Stmt] -> NodeId -> ([DFGNode], [DFGEdge], ScopeStack, NodeId)
processStmtsWithStack initialStack stmts startId =
  foldl step ([], [], initialStack, startId) stmts
  where
    step (nodesAcc, edgesAcc, curStack, curId) stmt = case stmt of
      StmtAssign targets val ->
        let (useNodes, useEdges, nextId1) = extractExprUsesStack curStack val curId
            assignedVars = concatMap extractTargetVars targets
            (defNodes, nextId2) = foldl (\(ns, cId) v ->
              (ns ++ [DFGNode cId (DefAssignment v) (Just val)], cId + 1)) ([], nextId1) assignedVars
            defEdges = [ DFGEdge (dfgNodeId srcNode) (dfgNodeId targetNode) v
                       | (targetNode, v) <- zip defNodes assignedVars
                       , srcNode <- useNodes
                       ]
            newStack = foldl (\s (dNode, v) -> updateStack v (dfgNodeId dNode) s) curStack (zip defNodes assignedVars)
        in (nodesAcc ++ useNodes ++ defNodes, edgesAcc ++ useEdges ++ defEdges, newStack, nextId2)

      StmtAnnAssign target _ mVal ->
        let assignedVars = extractTargetVars target
            (useNodes, useEdges, nextId1) = maybe ([], [], curId) (\val -> extractExprUsesStack curStack val curId) mVal
            (defNodes, nextId2) = foldl (\(ns, cId) v ->
              (ns ++ [DFGNode cId (DefAssignment v) mVal], cId + 1)) ([], nextId1) assignedVars
            defEdges = [ DFGEdge (dfgNodeId srcNode) (dfgNodeId targetNode) v
                       | (targetNode, v) <- zip defNodes assignedVars
                       , srcNode <- useNodes
                       ]
            newStack = foldl (\s (dNode, v) -> updateStack v (dfgNodeId dNode) s) curStack (zip defNodes assignedVars)
        in (nodesAcc ++ useNodes ++ defNodes, edgesAcc ++ useEdges ++ defEdges, newStack, nextId2)

      StmtReturn mVal ->
        let (useNodes, useEdges, nextId1) = maybe ([], [], curId) (\val -> extractExprUsesStack curStack val curId) mVal
        in (nodesAcc ++ useNodes, edgesAcc ++ useEdges, curStack, nextId1)

      StmtIf cond thenB elseB ->
        let (cNodes, cEdges, nextId1) = extractExprUsesStack curStack cond curId
            walrusVars = collectWalrusDefs cond
            (wNodes, nextId1_w) = foldl (\(ns, cId) v ->
              (ns ++ [DFGNode cId (DefAssignment v) (Just cond)], cId + 1)) ([], nextId1) walrusVars
            condStack = foldl (\s (dNode, v) -> updateStack v (dfgNodeId dNode) s) curStack (zip wNodes walrusVars)
            (tNodes, tEdges, thenStack, nextId2) = processStmtsWithStack condStack thenB nextId1_w
            (eNodes, eEdges, elseStack, nextId3) = processStmtsWithStack condStack elseB nextId2
            (phiNodes, phiEdges, mergedStack, nextId4) = mergeBranchDefs condStack thenStack elseStack nextId3
        in ( nodesAcc ++ cNodes ++ wNodes ++ tNodes ++ eNodes ++ phiNodes
           , edgesAcc ++ cEdges ++ tEdges ++ eEdges ++ phiEdges
           , mergedStack
           , nextId4
           )

      StmtWhile cond body elseB ->
        let (cNodes, cEdges, nextId1) = extractExprUsesStack curStack cond curId
            walrusVars = collectWalrusDefs cond
            (wNodes, nextId1_w) = foldl (\(ns, cId) v ->
              (ns ++ [DFGNode cId (DefAssignment v) (Just cond)], cId + 1)) ([], nextId1) walrusVars
            condStack = foldl (\s (dNode, v) -> updateStack v (dfgNodeId dNode) s) curStack (zip wNodes walrusVars)
            (bNodes, bEdges, bodyStack, nextId2) = processStmtsWithStack condStack body nextId1_w
            (eNodes, eEdges, elseStack, nextId3) = processStmtsWithStack condStack elseB nextId2
            (phiNodes, phiEdges, mergedStack, nextId4) = mergeBranchDefs condStack bodyStack elseStack nextId3
        in ( nodesAcc ++ cNodes ++ wNodes ++ bNodes ++ eNodes ++ phiNodes
           , edgesAcc ++ cEdges ++ bEdges ++ eEdges ++ phiEdges
           , mergedStack
           , nextId4
           )

      StmtFor target iter body elseB ->
        let (iNodes, iEdges, nextId1) = extractExprUsesStack curStack iter curId
            vars = extractTargetVars target
            (defNodes, nextId2) = foldl (\(ns, cId) v ->
              (ns ++ [DFGNode cId (DefAssignment v) (Just iter)], cId + 1)) ([], nextId1) vars
            targetStack = foldl (\s (dNode, v) -> updateStack v (dfgNodeId dNode) s) curStack (zip defNodes vars)
            (bNodes, bEdges, bodyStack, nextId3) = processStmtsWithStack targetStack body nextId2
            (eNodes, eEdges, elseStack, nextId4) = processStmtsWithStack targetStack elseB nextId3
            (phiNodes, phiEdges, mergedStack, nextId5) = mergeBranchDefs targetStack bodyStack elseStack nextId4
        in ( nodesAcc ++ iNodes ++ defNodes ++ bNodes ++ eNodes ++ phiNodes
           , edgesAcc ++ iEdges ++ bEdges ++ eEdges ++ phiEdges
           , mergedStack
           , nextId5
           )

      StmtTry tryB handlers elseB finB ->
        let (tNodes, tEdges, tryStack, nextId1) = processStmtsWithStack curStack tryB curId
            (hNodes, hEdges, hStacks, nextId2) = foldl stepHandler ([], [], [], nextId1) handlers
            (eNodes, eEdges, elseStack, nextId3) = processStmtsWithStack tryStack elseB nextId2
            branchStacks = (if null elseB then tryStack else elseStack) : hStacks
            (phiNodes, phiEdges, mergedStack, nextId4) = mergeMultiBranchDefs curStack branchStacks nextId3
            (fNodes, fEdges, finStack, nextId5) = processStmtsWithStack mergedStack finB nextId4
        in ( nodesAcc ++ tNodes ++ hNodes ++ eNodes ++ phiNodes ++ fNodes
           , edgesAcc ++ tEdges ++ hEdges ++ eEdges ++ phiEdges ++ fEdges
           , finStack
           , nextId5
           )
        where
          stepHandler (nsAcc, esAcc, stAcc, cId) (_, mExcName, hStmts) =
            let excStack = case mExcName of
                  Just name -> updateStack name cId curStack
                  Nothing   -> curStack
                excNode = [DFGNode cId (DefAssignment name) Nothing | Just name <- [mExcName]]
                cId1 = if null excNode then cId else cId + 1
                (hN, hE, hS, cId2) = processStmtsWithStack excStack hStmts cId1
            in (nsAcc ++ excNode ++ hN, esAcc ++ hE, stAcc ++ [hS], cId2)

      StmtSwitch expr cases defaultStmts ->
        let (eNodes, eEdges, nextId1) = extractExprUsesStack curStack expr curId
            (cNodes, cEdges, caseStacks, nextId2) = foldl stepCase ([], [], [], nextId1) cases
            (defNodes, defEdges, defStack, nextId3) = processStmtsWithStack curStack defaultStmts nextId2
            allBranchStacks = defStack : caseStacks
            (phiNodes, phiEdges, mergedStack, nextId4) = mergeMultiBranchDefs curStack allBranchStacks nextId3
        in ( nodesAcc ++ eNodes ++ cNodes ++ defNodes ++ phiNodes
           , edgesAcc ++ eEdges ++ cEdges ++ defEdges ++ phiEdges
           , mergedStack
           , nextId4
           )
        where
          stepCase (nsAcc, esAcc, stAcc, cId) (_, cStmts) =
            let (cN, cE, cS, nId) = processStmtsWithStack curStack cStmts cId
            in (nsAcc ++ cN, esAcc ++ cE, stAcc ++ [cS], nId)

      StmtMatch expr cases ->
        let (eNodes, eEdges, nextId1) = extractExprUsesStack curStack expr curId
            (cNodes, cEdges, caseStacks, nextId2) = foldl stepCase ([], [], [], nextId1) cases
            (phiNodes, phiEdges, mergedStack, nextId3) = mergeMultiBranchDefs curStack caseStacks nextId2
        in ( nodesAcc ++ eNodes ++ cNodes ++ phiNodes
           , edgesAcc ++ eEdges ++ cEdges ++ phiEdges
           , mergedStack
           , nextId3
           )
        where
          stepCase (nsAcc, esAcc, stAcc, cId) mc =
            let (cN, cE, cS, nId) = processStmtsWithStack curStack (mcBody mc) cId
            in (nsAcc ++ cN, esAcc ++ cE, stAcc ++ [cS], nId)

      StmtExpr e ->
        let (useNodes, useEdges, nextId1) = extractExprUsesStack curStack e curId
            walrusVars = collectWalrusDefs e
            (defNodes, nextId2) = foldl (\(ns, cId) v ->
              (ns ++ [DFGNode cId (DefAssignment v) (Just e)], cId + 1)) ([], nextId1) walrusVars
            walrusEdges = [ DFGEdge (dfgNodeId srcNode) (dfgNodeId targetNode) v
                          | (targetNode, v) <- zip defNodes walrusVars
                          , srcNode <- useNodes
                          ]
            newStack = foldl (\s (dNode, v) -> updateStack v (dfgNodeId dNode) s) curStack (zip defNodes walrusVars)
        in (nodesAcc ++ useNodes ++ defNodes, edgesAcc ++ useEdges ++ walrusEdges, newStack, nextId2)

      _ -> (nodesAcc, edgesAcc, curStack, curId)

extractTargetVars :: Expr -> [Text]
extractTargetVars = \case
  ExprId v        -> [v]
  ExprTuple es    -> concatMap extractTargetVars es
  ExprList es     -> concatMap extractTargetVars es
  ExprStarred e   -> extractTargetVars e
  ExprWalrus v _  -> [v]
  _               -> []

mergeBranchDefs :: ScopeStack -> ScopeStack -> ScopeStack -> NodeId -> ([DFGNode], [DFGEdge], ScopeStack, NodeId)
mergeBranchDefs inStack thenStack elseStack startId =
  mergeMultiBranchDefs inStack [thenStack, elseStack] startId

mergeMultiBranchDefs :: ScopeStack -> [ScopeStack] -> NodeId -> ([DFGNode], [DFGEdge], ScopeStack, NodeId)
mergeMultiBranchDefs inStack branchStacks startId =
  let allVars = Set.toList (Set.unions (map Map.keysSet branchTops))
      diffVars = filter isDiff allVars
      (phiNodes, phiEdges, newDefs, nextId) = foldl step ([], [], inTop, startId) diffVars
      finalStack = case inStack of
        (_:rest) -> newDefs : rest
        []       -> [newDefs]
  in (phiNodes, phiEdges, finalStack, nextId)
  where
    inTop = case inStack of (s:_) -> s; [] -> Map.empty
    branchTops = [case s of (t:_) -> t; [] -> Map.empty | s <- branchStacks]

    isDiff v =
      let defs = [Map.lookup v t | t <- branchTops]
          firstDef = case defs of (d:_) -> d; [] -> Nothing
      in any (/= firstDef) defs || any (/= Map.lookup v inTop) defs

    step (nsAcc, esAcc, defsAcc, cId) v =
      let incomingDefs = Set.toList $ Set.fromList
            [ nid
            | t <- branchTops
            , let mDef = case Map.lookup v t of
                           Just d  -> Just d
                           Nothing -> Map.lookup v inTop
            , Just nid <- [mDef]
            ]
      in if length incomingDefs < 2 && all (== Map.lookup v inTop) (map Just incomingDefs)
         then (nsAcc, esAcc, defsAcc, cId)
         else
           let phiNode = DFGNode cId (DefPhi incomingDefs) (Just (ExprId v))
               phiEdges = [DFGEdge src cId v | src <- incomingDefs]
               newDefsAcc = Map.insert v cId defsAcc
           in (nsAcc ++ [phiNode], esAcc ++ phiEdges, newDefsAcc, cId + 1)

extractExprUses :: Map Text NodeId -> Expr -> NodeId -> ([DFGNode], [DFGEdge], NodeId)
extractExprUses activeDefs expr startId = extractExprUsesStack [activeDefs] expr startId

extractExprUsesStack :: ScopeStack -> Expr -> NodeId -> ([DFGNode], [DFGEdge], NodeId)
extractExprUsesStack stack expr startId =
  let readVars = collectVarReads expr
      (nodes, edges, nextId) = foldl step ([], [], startId) (Set.toList readVars)
  in (nodes, edges, nextId)
  where
    step (ns, es, curId) v =
      let useNode = DFGNode curId (UseRead v) (Just expr)
          edge = case lookupStack v stack of
            Just defNodeId -> [DFGEdge defNodeId curId v]
            Nothing        -> []
      in (ns ++ [useNode], es ++ edge, curId + 1)

collectWalrusDefs :: Expr -> [Text]
collectWalrusDefs = \case
  ExprWalrus v e     -> v : collectWalrusDefs e
  ExprBinary _ e1 e2 -> collectWalrusDefs e1 ++ collectWalrusDefs e2
  ExprUnary _ e      -> collectWalrusDefs e
  ExprCall f args kw -> collectWalrusDefs f ++ concatMap collectWalrusDefs args ++ concatMap (collectWalrusDefs . snd) kw
  ExprList es        -> concatMap collectWalrusDefs es
  ExprTuple es       -> concatMap collectWalrusDefs es
  ExprTernary c t f  -> collectWalrusDefs c ++ collectWalrusDefs t ++ collectWalrusDefs f
  _                  -> []

collectVarReads :: Expr -> Set Text
collectVarReads = \case
  ExprId v           -> Set.singleton v
  ExprBinary _ e1 e2 -> Set.union (collectVarReads e1) (collectVarReads e2)
  ExprUnary _ e      -> collectVarReads e
  ExprCall t args kw -> Set.unions (collectVarReads t : map collectVarReads args ++ map (collectVarReads . snd) kw)
  ExprAttr e _       -> collectVarReads e
  ExprSubscript e idx-> Set.union (collectVarReads e) (collectVarReads idx)
  ExprTernary c t f  -> Set.unions [collectVarReads c, collectVarReads t, collectVarReads f]
  ExprList es        -> foldMap collectVarReads es
  ExprTuple es       -> foldMap collectVarReads es
  ExprDict pairs     -> foldMap (\(k, v) -> Set.union (collectVarReads k) (collectVarReads v)) pairs
  ExprSet es         -> foldMap collectVarReads es
  _                  -> Set.empty

formatDFG :: DataFlowGraph -> Text
formatDFG dfg =
  T.unlines $
    [ "DFG: " <> dfgFunction dfg <> " (Nodes: " <> T.pack (show (length (dfgNodes dfg))) <> ", Edges: " <> T.pack (show (length (dfgEdges dfg))) <> ")"
    , "---------------------------------------------------------"
    ] ++
    map formatNode (dfgNodes dfg) ++
    [ "Edges:" ] ++
    map formatEdge (dfgEdges dfg)
  where
    formatNode n =
      "  [Node " <> T.pack (show (dfgNodeId n)) <> "] " <> formatKind (dfgKind n)

    formatKind = \case
      DefParam idx      -> "DefParam (" <> T.pack (show idx) <> ")"
      DefAssignment v   -> "DefAssignment (" <> v <> ")"
      DefPhi _          -> "DefPhi"
      UseRead v         -> "UseRead (" <> v <> ")"
      UseArgument idx   -> "UseArgument (" <> T.pack (show idx) <> ")"
      UseBranchGuard    -> "UseBranchGuard"

    formatEdge e =
      "    " <> T.pack (show (dfgSource e)) <> " ---> " <> T.pack (show (dfgTarget e)) <> " (var: " <> dfgVarName e <> ")"
