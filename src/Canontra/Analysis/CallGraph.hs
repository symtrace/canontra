{- |
Module      : Canontra.Analysis.CallGraph
Description : Static intra-module call graph extractor and topology analyzer.

This module extracts caller-to-callee invocation graphs from normalized IR.
It distinguishes local function calls, method dispatches, external module invocations,
and async await edges, and uses Tarjan's SCC algorithm to detect recursion cycles.
-}
module Canontra.Analysis.CallGraph
  ( CallerNode (..)
  , CalleeTarget (..)
  , CallEdge (..)
  , CallGraph (..)
  , buildCallGraph
  , formatCallGraph
  , findCallGraphSCCs
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (sort, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Canontra.IR.Declaration
import Canontra.IR.Dependency
import Canontra.IR.Expression
import Canontra.IR.Program

data CallerNode
  = CallTopLevel
  | CallFunction Text
  | CallMethod Text Text -- e.g. (Class name, Method name)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data CalleeTarget
  = TargetLocal Text
  | TargetMethod Text Text
  | TargetImported Text Text -- e.g. (Module, Symbol)
  | TargetDynamic Expr
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data CallEdge = CallEdge
  { edgeCaller    :: CallerNode
  , edgeCallee    :: CalleeTarget
  , edgeCallCount :: Int
  , edgeIsAsync   :: Bool
  } deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data CallGraph = CallGraph
  { cgNodes :: [CallerNode]
  , cgEdges :: [CallEdge]
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Extract the static intra-module call graph from a Program.
buildCallGraph :: Program -> CallGraph
buildCallGraph (Program modules _) =
  let allImports = concatMap modImports modules
      importMap = buildImportMap allImports
      (nodesList, edgesList) = foldMap (extractModuleCalls importMap) modules
      uniqueNodes = sort (Set.toList (Set.fromList (CallTopLevel : nodesList)))
      consolidatedEdges = consolidateEdges edgesList
  in CallGraph uniqueNodes consolidatedEdges

buildImportMap :: [ImportDecl] -> Map Text (Text, Text)
buildImportMap imps = Map.fromList (concatMap toEntry imps)
  where
    toEntry (ImportModule m alias) =
      let bound = maybe (lastPart m) id alias
      in [(bound, (m, ""))]
    toEntry (ImportFrom m target) = case target of
      ImportAll -> []
      ImportSymbols syms ->
        [ (maybe sym id alias, (m, sym))
        | (sym, alias) <- syms
        ]

    lastPart m = case T.splitOn "." m of
      [] -> m
      xs -> last xs

extractModuleCalls :: Map Text (Text, Text) -> Module -> ([CallerNode], [(CallerNode, CalleeTarget, Bool)])
extractModuleCalls impMap (Module _ _ decls stmts) =
  let topCalls = extractStmtCalls impMap CallTopLevel stmts
      (declNodes, declCalls) = foldMap (extractDeclCalls impMap) decls
  in (declNodes, topCalls ++ declCalls)

extractDeclCalls :: Map Text (Text, Text) -> Declaration -> ([CallerNode], [(CallerNode, CalleeTarget, Bool)])
extractDeclCalls impMap decl = case decl of
  DeclFunction fn ->
    let node = CallFunction (fnName fn)
        calls = extractStmtCalls impMap node (fnBody fn)
    in ([node], calls)

  DeclClass cls ->
    let (mNodes, mCalls) = foldMap (extractMethodCalls impMap (clsName cls)) (clsMethods cls)
    in (mNodes, mCalls)

  DeclStruct st ->
    let (mNodes, mCalls) = foldMap (extractMethodCalls impMap (stName st)) (stMethods st)
    in (mNodes, mCalls)

  DeclTrait tr ->
    let (mNodes, mCalls) = foldMap (extractMethodCalls impMap (trName tr)) (trMethods tr)
    in (mNodes, mCalls)

  DeclImpl imp ->
    let (mNodes, mCalls) = foldMap (extractMethodCalls impMap (impTarget imp)) (impMethods imp)
    in (mNodes, mCalls)

  DeclReceiver rc fn ->
    let node = CallMethod (rcTypeName rc) (fnName fn)
        calls = extractStmtCalls impMap node (fnBody fn)
    in ([node], calls)

  DeclVariable _ _ -> ([], [])
  DeclInterface _ -> ([], [])
  DeclTypeAlias _ _ -> ([], [])

extractMethodCalls :: Map Text (Text, Text) -> Text -> Function -> ([CallerNode], [(CallerNode, CalleeTarget, Bool)])
extractMethodCalls impMap className fn =
  let node = CallMethod className (fnName fn)
      calls = extractStmtCalls impMap node (fnBody fn)
  in ([node], calls)

extractStmtCalls :: Map Text (Text, Text) -> CallerNode -> [Stmt] -> [(CallerNode, CalleeTarget, Bool)]
extractStmtCalls impMap caller stmts =
  concatMap (extractSingleStmtCalls impMap caller) stmts

extractSingleStmtCalls :: Map Text (Text, Text) -> CallerNode -> Stmt -> [(CallerNode, CalleeTarget, Bool)]
extractSingleStmtCalls impMap caller stmt = case stmt of
  StmtAssign targets val     -> concatMap (extractExprCalls impMap caller False) (val : targets)
  StmtAnnAssign target ty v  -> concatMap (extractExprCalls impMap caller False) (target : ty : maybe [] pure v)
  StmtAugAssign t _ v        -> concatMap (extractExprCalls impMap caller False) [t, v]
  StmtExpr e                 -> extractExprCalls impMap caller False e
  StmtReturn me              -> maybe [] (extractExprCalls impMap caller False) me
  StmtIf c b e               -> extractExprCalls impMap caller False c ++ extractStmtCalls impMap caller b ++ extractStmtCalls impMap caller e
  StmtWhile c b e            -> extractExprCalls impMap caller False c ++ extractStmtCalls impMap caller b ++ extractStmtCalls impMap caller e
  StmtFor t i b e            -> extractExprCalls impMap caller False t ++ extractExprCalls impMap caller False i ++ extractStmtCalls impMap caller b ++ extractStmtCalls impMap caller e
  StmtAsyncFor t i b e       -> extractExprCalls impMap caller True t ++ extractExprCalls impMap caller True i ++ extractStmtCalls impMap caller b ++ extractStmtCalls impMap caller e
  StmtTry b h e f            ->
    extractStmtCalls impMap caller b ++
    concatMap (\(me, _, hb) -> maybe [] (extractExprCalls impMap caller False) me ++ extractStmtCalls impMap caller hb) h ++
    extractStmtCalls impMap caller e ++
    extractStmtCalls impMap caller f
  StmtWith items b           ->
    concatMap (\(e, ma) -> extractExprCalls impMap caller False e ++ maybe [] (extractExprCalls impMap caller False) ma) items ++
    extractStmtCalls impMap caller b
  StmtAsyncWith items b      ->
    concatMap (\(e, ma) -> extractExprCalls impMap caller True e ++ maybe [] (extractExprCalls impMap caller True) ma) items ++
    extractStmtCalls impMap caller b
  StmtAssert e me            -> extractExprCalls impMap caller False e ++ maybe [] (extractExprCalls impMap caller False) me
  StmtRaise me mc            -> maybe [] (extractExprCalls impMap caller False) me ++ maybe [] (extractExprCalls impMap caller False) mc
  StmtDelete es              -> concatMap (extractExprCalls impMap caller False) es
  StmtMatch s cs             ->
    extractExprCalls impMap caller False s ++
    concatMap (\mc -> extractExprCalls impMap caller False (mcPattern mc) ++ maybe [] (extractExprCalls impMap caller False) (mcGuard mc) ++ extractStmtCalls impMap caller (mcBody mc)) cs
  StmtGo e                   -> extractExprCalls impMap caller True e
  StmtDefer e                -> extractExprCalls impMap caller False e
  StmtChanSend ch val        -> extractExprCalls impMap caller False ch ++ extractExprCalls impMap caller False val
  StmtSelect cases           -> concatMap (\(sc, b) -> extractSelectCalls impMap caller sc ++ extractStmtCalls impMap caller b) cases
  _                          -> []
  where
    extractSelectCalls m c = \case
      SelectSend ch val -> extractExprCalls m c False ch ++ extractExprCalls m c False val
      SelectRecv _ ch   -> extractExprCalls m c False ch
      SelectDefault     -> []

extractExprCalls :: Map Text (Text, Text) -> CallerNode -> Bool -> Expr -> [(CallerNode, CalleeTarget, Bool)]
extractExprCalls impMap caller isAsync expr = case expr of
  ExprCall target args kwargs ->
    let targetCallee = resolveTarget impMap target
        thisEdge = (caller, targetCallee, isAsync)
        nestedTarget = extractExprCalls impMap caller isAsync target
        nestedArgs = concatMap (extractExprCalls impMap caller False) args
        nestedKwargs = concatMap (extractExprCalls impMap caller False . snd) kwargs
    in thisEdge : (nestedTarget ++ nestedArgs ++ nestedKwargs)

  ExprAwait inner ->
    extractExprCalls impMap caller True inner

  ExprBinary _ e1 e2 ->
    extractExprCalls impMap caller isAsync e1 ++ extractExprCalls impMap caller isAsync e2

  ExprUnary _ e ->
    extractExprCalls impMap caller isAsync e

  ExprAttr e _ ->
    extractExprCalls impMap caller isAsync e

  ExprSubscript e idx ->
    extractExprCalls impMap caller isAsync e ++ extractExprCalls impMap caller isAsync idx

  ExprSlice ms me mst ->
    concatMap (maybe [] (extractExprCalls impMap caller isAsync)) [ms, me, mst]

  ExprList es -> concatMap (extractExprCalls impMap caller isAsync) es
  ExprTuple es -> concatMap (extractExprCalls impMap caller isAsync) es
  ExprDict pairs -> concatMap (\(k, v) -> extractExprCalls impMap caller isAsync k ++ extractExprCalls impMap caller isAsync v) pairs
  ExprSet es -> concatMap (extractExprCalls impMap caller isAsync) es
  ExprLambda _ body -> extractExprCalls impMap caller isAsync body
  ExprTernary c t f -> extractExprCalls impMap caller isAsync c ++ extractExprCalls impMap caller isAsync t ++ extractExprCalls impMap caller isAsync f
  ExprListComp item comps -> extractExprCalls impMap caller isAsync item ++ concatMap (extractCompCalls impMap caller isAsync) comps
  ExprDictComp k v comps -> extractExprCalls impMap caller isAsync k ++ extractExprCalls impMap caller isAsync v ++ concatMap (extractCompCalls impMap caller isAsync) comps
  ExprSetComp item comps -> extractExprCalls impMap caller isAsync item ++ concatMap (extractCompCalls impMap caller isAsync) comps
  ExprGenerator item comps -> extractExprCalls impMap caller isAsync item ++ concatMap (extractCompCalls impMap caller isAsync) comps
  ExprWalrus _ val -> extractExprCalls impMap caller isAsync val
  ExprYield me -> maybe [] (extractExprCalls impMap caller isAsync) me
  ExprYieldFrom e -> extractExprCalls impMap caller isAsync e
  ExprStarred e -> extractExprCalls impMap caller isAsync e
  ExprKwStarred e -> extractExprCalls impMap caller isAsync e
  ExprOptChain e _ -> extractExprCalls impMap caller isAsync e
  ExprNullish e1 e2 -> extractExprCalls impMap caller isAsync e1 ++ extractExprCalls impMap caller isAsync e2
  ExprChanRecv ch -> extractExprCalls impMap caller isAsync ch
  ExprTryOp e -> extractExprCalls impMap caller isAsync e
  ExprMacroCall _ args -> concatMap (extractExprCalls impMap caller isAsync) args
  ExprJSX _ attrs children -> concatMap (extractExprCalls impMap caller isAsync . snd) attrs ++ concatMap (extractExprCalls impMap caller isAsync) children
  _ -> []
  where
    extractCompCalls m c a (CompFor t iter ifs) =
      extractExprCalls m c a t ++ extractExprCalls m c a iter ++ concatMap (extractExprCalls m c a) ifs

resolveTarget :: Map Text (Text, Text) -> Expr -> CalleeTarget
resolveTarget impMap expr = case expr of
  ExprId name ->
    case Map.lookup name impMap of
      Just (modName, symName) ->
        let actualSym = if T.null symName then name else symName
        in TargetImported modName actualSym
      Nothing -> TargetLocal name

  ExprAttr (ExprId obj) method ->
    case Map.lookup obj impMap of
      Just (modName, _) -> TargetImported modName method
      Nothing           -> TargetMethod obj method

  ExprAttr target method ->
    TargetMethod (T.pack (show target)) method

  _ -> TargetDynamic expr

consolidateEdges :: [(CallerNode, CalleeTarget, Bool)] -> [CallEdge]
consolidateEdges rawEdges =
  let grouped = Map.fromListWith (+) [ ((c, t, a), 1 :: Int) | (c, t, a) <- rawEdges ]
      edges = [ CallEdge c t cnt a | ((c, t, a), cnt) <- Map.toList grouped ]
  in sortBy (comparing (\e -> (edgeCaller e, edgeCallee e, edgeIsAsync e))) edges

-- | Find strongly connected components in the CallGraph using Tarjan's SCC algorithm.
findCallGraphSCCs :: CallGraph -> [[CallerNode]]
findCallGraphSCCs cg =
  let adj = buildAdjacency (cgEdges cg)
      nodes = cgNodes cg
  in sccTarjan nodes adj

buildAdjacency :: [CallEdge] -> Map CallerNode [CallerNode]
buildAdjacency edges =
  Map.fromListWith (++) [ (edgeCaller e, [calleeToCaller (edgeCallee e)]) | e <- edges ]
  where
    calleeToCaller (TargetLocal name) = CallFunction name
    calleeToCaller (TargetMethod cls m) = CallMethod cls m
    calleeToCaller _ = CallTopLevel

sccTarjan :: [CallerNode] -> Map CallerNode [CallerNode] -> [[CallerNode]]
sccTarjan nodes adj =
  let step (visited, sccs) node
        | Set.member node visited = (visited, sccs)
        | otherwise =
            let comp = dfs node visited []
                newVisited = Set.union visited (Set.fromList comp)
            in (newVisited, comp : sccs)
      (_, allSccs) = foldl step (Set.empty, []) nodes
  in filter (not . null) allSccs
  where
    dfs curr vis acc
      | Set.member curr vis = acc
      | otherwise =
          let neighbors = Map.findWithDefault [] curr adj
              newVis = Set.insert curr vis
          in foldl (\a n -> dfs n newVis a) (curr : acc) neighbors

formatCallGraph :: CallGraph -> Text
formatCallGraph cg =
  T.unlines $
    [ "Call Graph (" <> T.pack (show (length (cgNodes cg))) <> " nodes, " <> T.pack (show (length (cgEdges cg))) <> " edges)"
    , "---------------------------------------------------------"
    ] ++
    map formatEdge (cgEdges cg)
  where
    formatEdge (CallEdge caller callee cnt isAsync) =
      let asyncStr = if isAsync then " [async]" else ""
          countStr = if cnt > 1 then " (" <> T.pack (show cnt) <> "x)" else ""
      in "  " <> formatCaller caller <> " --> " <> formatCallee callee <> asyncStr <> countStr

    formatCaller CallTopLevel = "<top-level>"
    formatCaller (CallFunction fn) = "def " <> fn
    formatCaller (CallMethod cls m) = cls <> "." <> m

    formatCallee (TargetLocal name) = name
    formatCallee (TargetMethod cls m) = if T.null cls then m else cls <> "." <> m
    formatCallee (TargetImported m s) = m <> "." <> s
    formatCallee (TargetDynamic e) = "<dynamic: " <> T.pack (show e) <> ">"
