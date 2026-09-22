{- |
Module      : Canontra.Analysis.Scope
Description : Lexical scope analysis and symbol definition-use resolver.

This module constructs the language-independent lexical scope tree for a program.
It resolves variable bindings, parameters, imports, global/nonlocal boundaries,
and references across nested function, class, struct, trait, lambda, and comprehension scopes.
-}
module Canontra.Analysis.Scope
  ( ScopeId
  , ScopeKind (..)
  , LocalOrGlobal (..)
  , SymbolKind (..)
  , SymbolBinding (..)
  , ScopeTree (..)
  , analyzeProgramScope
  , analyzeModuleScope
  , findBinding
  , findBindingInHierarchy
  , allBindings
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Canontra.IR.Declaration
import Canontra.IR.Dependency
import Canontra.IR.Expression
import Canontra.IR.Program

type ScopeId = Int

data ScopeKind
  = ScopeModule
  | ScopeClass Text
  | ScopeStruct Text
  | ScopeTrait Text
  | ScopeImpl Text
  | ScopeFunction Text
  | ScopeLambda
  | ScopeComprehension
  | ScopeBlock
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data LocalOrGlobal
  = BindingLocal
  | BindingGlobal
  | BindingNonLocal
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data SymbolKind
  = SymFunction
  | SymClass
  | SymStruct
  | SymTrait
  | SymParameter ParamKind
  | SymVariable LocalOrGlobal
  | SymImported Text (Maybe Text)  -- e.g. (Original name, Source module)
  | SymTypeAlias
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data SymbolBinding = SymbolBinding
  { symName       :: Text
  , symKind       :: SymbolKind
  , symDefinedAt  :: ScopeId
  , symReferences :: [ScopeId]
  , symIsExported :: Bool
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data ScopeTree = ScopeTree
  { scopeId       :: ScopeId
  , scopeKind     :: ScopeKind
  , scopeSymbols  :: Map Text SymbolBinding
  , scopeParent   :: Maybe ScopeId
  , scopeChildren :: [ScopeTree]
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Analyze a whole Program into a list of ScopeTrees (one per module).
analyzeProgramScope :: Program -> [ScopeTree]
analyzeProgramScope (Program modules _) =
  map (analyzeModuleScope 0) modules

-- | Analyze a single Module starting from a given ScopeId.
analyzeModuleScope :: ScopeId -> Module -> ScopeTree
analyzeModuleScope rootId (Module _ imps decls stmts) =
  let initialSymbols = collectImports rootId imps
      (globals, nonlocals) = collectExplicitDirectives stmts
      topLevelVars = collectStmtBindings rootId globals nonlocals stmts
      (declBindings, childTrees, _) = processDeclarations (rootId + 1) rootId decls
      combinedSymbols = Map.unions [declBindings, topLevelVars, initialSymbols]
      refs = collectStmtRefs stmts
      updatedSymbols = recordReferences rootId refs combinedSymbols
  in ScopeTree
      { scopeId       = rootId
      , scopeKind     = ScopeModule
      , scopeSymbols  = updatedSymbols
      , scopeParent   = Nothing
      , scopeChildren = childTrees
      }

collectImports :: ScopeId -> [ImportDecl] -> Map Text SymbolBinding
collectImports sid imps = Map.fromList $ concatMap (importToBindings sid) imps
  where
    importToBindings sId (ImportModule modName maybeAlias) =
      let boundName = maybe (lastModulePart modName) id maybeAlias
      in [(boundName, SymbolBinding boundName (SymImported modName Nothing) sId [] (isPublic boundName))]
    importToBindings sId (ImportFrom modName target) = case target of
      ImportAll -> []
      ImportSymbols syms ->
        [ (boundName, SymbolBinding boundName (SymImported symName (Just modName)) sId [] (isPublic boundName))
        | (symName, maybeAlias) <- syms
        , let boundName = maybe symName id maybeAlias
        ]

    lastModulePart m = case T.splitOn "." m of
      [] -> m
      xs -> last xs

processDeclarations :: ScopeId -> ScopeId -> [Declaration] -> (Map Text SymbolBinding, [ScopeTree], ScopeId)
processDeclarations startId parentId decls =
  foldl step (Map.empty, [], startId) decls
  where
    step (symAcc, treesAcc, curId) decl = case decl of
      DeclFunction fn ->
        let (fnTree, nextId) = analyzeFunction curId (Just parentId) fn
            binding = SymbolBinding (fnName fn) SymFunction parentId [] (isPublic (fnName fn))
        in (Map.insert (fnName fn) binding symAcc, treesAcc ++ [fnTree], nextId)

      DeclClass cls ->
        let (clsTree, nextId) = analyzeClass curId (Just parentId) cls
            binding = SymbolBinding (clsName cls) SymClass parentId [] (isPublic (clsName cls))
        in (Map.insert (clsName cls) binding symAcc, treesAcc ++ [clsTree], nextId)

      DeclStruct st ->
        let (stTree, nextId) = analyzeStruct curId (Just parentId) st
            binding = SymbolBinding (stName st) SymStruct parentId [] (isPublic (stName st))
        in (Map.insert (stName st) binding symAcc, treesAcc ++ [stTree], nextId)

      DeclTrait tr ->
        let (trTree, nextId) = analyzeTrait curId (Just parentId) tr
            binding = SymbolBinding (trName tr) SymTrait parentId [] (isPublic (trName tr))
        in (Map.insert (trName tr) binding symAcc, treesAcc ++ [trTree], nextId)

      DeclImpl imp ->
        let (impTree, nextId) = analyzeImpl curId (Just parentId) imp
        in (symAcc, treesAcc ++ [impTree], nextId)

      DeclReceiver _ fn ->
        let (fnTree, nextId) = analyzeFunction curId (Just parentId) fn
        in (symAcc, treesAcc ++ [fnTree], nextId)

      DeclVariable varName _ ->
        let binding = SymbolBinding varName (SymVariable BindingLocal) parentId [] (isPublic varName)
        in (Map.insert varName binding symAcc, treesAcc, curId)

      DeclInterface iface ->
        let binding = SymbolBinding (ifName iface) SymTrait parentId [] (isPublic (ifName iface))
        in (Map.insert (ifName iface) binding symAcc, treesAcc, curId)

      DeclTypeAlias aliasName _ ->
        let binding = SymbolBinding aliasName SymTypeAlias parentId [] (isPublic aliasName)
        in (Map.insert aliasName binding symAcc, treesAcc, curId)

analyzeFunction :: ScopeId -> Maybe ScopeId -> Function -> (ScopeTree, ScopeId)
analyzeFunction curId mParent fn =
  let pBindings = Map.fromList
        [ (paramName p, SymbolBinding (paramName p) (SymParameter (paramKind p)) curId [] False)
        | p <- fnParams fn
        ]
      (globals, nonlocals) = collectExplicitDirectives (fnBody fn)
      bodyVars = collectStmtBindings curId globals nonlocals (fnBody fn)
      (exprTrees, nextId) = extractExprScopeTrees (curId + 1) curId (fnBody fn)
      combinedSyms = Map.unions [bodyVars, pBindings]
      refs = collectStmtRefs (fnBody fn)
      updatedSyms = recordReferences curId refs combinedSyms
      fnTree = ScopeTree
        { scopeId       = curId
        , scopeKind     = ScopeFunction (fnName fn)
        , scopeSymbols  = updatedSyms
        , scopeParent   = mParent
        , scopeChildren = exprTrees
        }
  in (fnTree, nextId)

analyzeClass :: ScopeId -> Maybe ScopeId -> Class -> (ScopeTree, ScopeId)
analyzeClass curId mParent cls =
  let (methodTrees, nextId) = foldl step ([], curId + 1) (clsMethods cls)
      methodBindings = Map.fromList
        [ (fnName m, SymbolBinding (fnName m) SymFunction curId [] (isPublic (fnName m)))
        | m <- clsMethods cls
        ]
      clsTree = ScopeTree
        { scopeId       = curId
        , scopeKind     = ScopeClass (clsName cls)
        , scopeSymbols  = methodBindings
        , scopeParent   = mParent
        , scopeChildren = methodTrees
        }
  in (clsTree, nextId)
  where
    step (acc, cId) m =
      let (mTree, nId) = analyzeFunction cId (Just curId) m
      in (acc ++ [mTree], nId)

analyzeStruct :: ScopeId -> Maybe ScopeId -> Struct -> (ScopeTree, ScopeId)
analyzeStruct curId mParent st =
  let (methodTrees, nextId) = foldl step ([], curId + 1) (stMethods st)
      fieldBindings = Map.fromList
        [ (fName, SymbolBinding fName (SymVariable BindingLocal) curId [] (isPublic fName))
        | (fName, _) <- stFields st
        ]
      stTree = ScopeTree
        { scopeId       = curId
        , scopeKind     = ScopeStruct (stName st)
        , scopeSymbols  = fieldBindings
        , scopeParent   = mParent
        , scopeChildren = methodTrees
        }
  in (stTree, nextId)
  where
    step (acc, cId) m =
      let (mTree, nId) = analyzeFunction cId (Just curId) m
      in (acc ++ [mTree], nId)

analyzeTrait :: ScopeId -> Maybe ScopeId -> Trait -> (ScopeTree, ScopeId)
analyzeTrait curId mParent tr =
  let (methodTrees, nextId) = foldl step ([], curId + 1) (trMethods tr)
      methodBindings = Map.fromList
        [ (fnName m, SymbolBinding (fnName m) SymFunction curId [] (isPublic (fnName m)))
        | m <- trMethods tr
        ]
      trTree = ScopeTree
        { scopeId       = curId
        , scopeKind     = ScopeTrait (trName tr)
        , scopeSymbols  = methodBindings
        , scopeParent   = mParent
        , scopeChildren = methodTrees
        }
  in (trTree, nextId)
  where
    step (acc, cId) m =
      let (mTree, nId) = analyzeFunction cId (Just curId) m
      in (acc ++ [mTree], nId)

analyzeImpl :: ScopeId -> Maybe ScopeId -> Impl -> (ScopeTree, ScopeId)
analyzeImpl curId mParent imp =
  let (methodTrees, nextId) = foldl step ([], curId + 1) (impMethods imp)
      impTree = ScopeTree
        { scopeId       = curId
        , scopeKind     = ScopeImpl (impTarget imp)
        , scopeSymbols  = Map.empty
        , scopeParent   = mParent
        , scopeChildren = methodTrees
        }
  in (impTree, nextId)
  where
    step (acc, cId) m =
      let (mTree, nId) = analyzeFunction cId (Just curId) m
      in (acc ++ [mTree], nId)

extractExprScopeTrees :: ScopeId -> ScopeId -> [Stmt] -> ([ScopeTree], ScopeId)
extractExprScopeTrees startId parentId stmts =
  let allExprs = concatMap getStmtExprs stmts
  in foldl processExpr ([], startId) allExprs
  where
    processExpr (trees, curId) expr = case expr of
      ExprLambda params body ->
        let pBinds = Map.fromList
              [ (paramName p, SymbolBinding (paramName p) (SymParameter (paramKind p)) curId [] False)
              | p <- params
              ]
            refs = collectExprRefs body
            upBinds = recordReferences curId refs pBinds
            tree = ScopeTree curId ScopeLambda upBinds (Just parentId) []
        in (trees ++ [tree], curId + 1)

      ExprListComp body comps ->
        let (compTree, nextId) = buildCompTree curId parentId body comps
        in (trees ++ [compTree], nextId)

      ExprDictComp k v comps ->
        let (compTree, nextId) = buildCompTree curId parentId (ExprTuple [k, v]) comps
        in (trees ++ [compTree], nextId)

      ExprSetComp body comps ->
        let (compTree, nextId) = buildCompTree curId parentId body comps
        in (trees ++ [compTree], nextId)

      ExprGenerator body comps ->
        let (compTree, nextId) = buildCompTree curId parentId body comps
        in (trees ++ [compTree], nextId)

      _ -> (trees, curId)

    buildCompTree cId pId body comps =
      let targets = concatMap (getExprIds . compTarget) comps
          binds = Map.fromList
            [ (t, SymbolBinding t (SymVariable BindingLocal) cId [] False)
            | t <- targets
            ]
          refs = collectExprRefs body
          upBinds = recordReferences cId refs binds
          tree = ScopeTree cId ScopeComprehension upBinds (Just pId) []
      in (tree, cId + 1)

collectExplicitDirectives :: [Stmt] -> (Set Text, Set Text)
collectExplicitDirectives stmts =
  foldl checkDirective (Set.empty, Set.empty) stmts
  where
    checkDirective (g, nl) s = case s of
      StmtGlobal vars   -> (Set.union g (Set.fromList vars), nl)
      StmtNonlocal vars -> (g, Set.union nl (Set.fromList vars))
      StmtIf _ b e      ->
        let (g1, n1) = collectExplicitDirectives b
            (g2, n2) = collectExplicitDirectives e
        in (Set.unions [g, g1, g2], Set.unions [nl, n1, n2])
      StmtWhile _ b e   ->
        let (g1, n1) = collectExplicitDirectives b
            (g2, n2) = collectExplicitDirectives e
        in (Set.unions [g, g1, g2], Set.unions [nl, n1, n2])
      StmtFor _ _ b e   ->
        let (g1, n1) = collectExplicitDirectives b
            (g2, n2) = collectExplicitDirectives e
        in (Set.unions [g, g1, g2], Set.unions [nl, n1, n2])
      StmtAsyncFor _ _ b e ->
        let (g1, n1) = collectExplicitDirectives b
            (g2, n2) = collectExplicitDirectives e
        in (Set.unions [g, g1, g2], Set.unions [nl, n1, n2])
      StmtTry b h e f   ->
        let (g1, n1) = collectExplicitDirectives b
            (g2, n2) = foldl (\(ga, na) (_, _, hb) ->
                                let (gb, nb) = collectExplicitDirectives hb
                                in (Set.union ga gb, Set.union na nb)) (Set.empty, Set.empty) h
            (g3, n3) = collectExplicitDirectives e
            (g4, n4) = collectExplicitDirectives f
        in (Set.unions [g, g1, g2, g3, g4], Set.unions [nl, n1, n2, n3, n4])
      StmtWith _ b      ->
        let (g1, n1) = collectExplicitDirectives b in (Set.union g g1, Set.union nl n1)
      StmtAsyncWith _ b ->
        let (g1, n1) = collectExplicitDirectives b in (Set.union g g1, Set.union nl n1)
      StmtLoop b        ->
        let (g1, n1) = collectExplicitDirectives b in (Set.union g g1, Set.union nl n1)
      StmtSwitch _ cases defStmts ->
        let casePairs = concatMap snd cases
            (g1, n1) = collectExplicitDirectives (casePairs ++ defStmts)
        in (Set.union g g1, Set.union nl n1)
      _                 -> (g, nl)

collectStmtBindings :: ScopeId -> Set Text -> Set Text -> [Stmt] -> Map Text SymbolBinding
collectStmtBindings sid globals nonlocals stmts =
  Map.fromList $ map createBinding (Set.toList (foldMap getStmtTargets stmts))
  where
    createBinding varName
      | Set.member varName globals   = (varName, SymbolBinding varName (SymVariable BindingGlobal) sid [] (isPublic varName))
      | Set.member varName nonlocals = (varName, SymbolBinding varName (SymVariable BindingNonLocal) sid [] (isPublic varName))
      | otherwise                    = (varName, SymbolBinding varName (SymVariable BindingLocal) sid [] (isPublic varName))

getStmtTargets :: Stmt -> Set Text
getStmtTargets stmt =
  let direct = case stmt of
        StmtAssign targets _       -> Set.fromList (concatMap getExprIds targets)
        StmtAnnAssign target _ _   -> Set.fromList (getExprIds target)
        StmtAugAssign target _ _   -> Set.fromList (getExprIds target)
        StmtFor target _ body els  -> Set.unions [Set.fromList (getExprIds target), foldMap getStmtTargets body, foldMap getStmtTargets els]
        StmtAsyncFor target _ body els -> Set.unions [Set.fromList (getExprIds target), foldMap getStmtTargets body, foldMap getStmtTargets els]
        StmtIf _ body els          -> Set.union (foldMap getStmtTargets body) (foldMap getStmtTargets els)
        StmtWhile _ body els       -> Set.union (foldMap getStmtTargets body) (foldMap getStmtTargets els)
        StmtLoop body              -> foldMap getStmtTargets body
        StmtTry b h els fin        -> Set.unions [foldMap getStmtTargets b, foldMap (\(_, _, hb) -> foldMap getStmtTargets hb) h, foldMap getStmtTargets els, foldMap getStmtTargets fin]
        StmtWith items body        ->
          let itemTargets = [name | (_, Just alias) <- items, name <- getExprIds alias]
          in Set.union (Set.fromList itemTargets) (foldMap getStmtTargets body)
        StmtAsyncWith items body   ->
          let itemTargets = [name | (_, Just alias) <- items, name <- getExprIds alias]
          in Set.union (Set.fromList itemTargets) (foldMap getStmtTargets body)
        StmtMatch _ cases          -> foldMap (\mc -> Set.union (Set.fromList (getExprIds (mcPattern mc))) (foldMap getStmtTargets (mcBody mc))) cases
        StmtSwitch _ cases defS    -> Set.union (foldMap (\(_, ss) -> foldMap getStmtTargets ss) cases) (foldMap getStmtTargets defS)
        _                          -> Set.empty
      -- PEP 572: Walrus operator (:=) targets are hoisted to the enclosing function/module scope
      walrusHoisted = foldMap collectWalrusTargets (getStmtExprs stmt)
  in Set.union direct walrusHoisted

-- | Recursively collect targets of walrus expressions (:=) inside any sub-expressions (PEP 572).
collectWalrusTargets :: Expr -> Set Text
collectWalrusTargets expr = case expr of
  ExprWalrus name val     -> Set.insert name (collectWalrusTargets val)
  ExprBinary _ e1 e2      -> Set.union (collectWalrusTargets e1) (collectWalrusTargets e2)
  ExprUnary _ e           -> collectWalrusTargets e
  ExprCall t args kwargs  -> Set.unions (collectWalrusTargets t : map collectWalrusTargets args ++ map (collectWalrusTargets . snd) kwargs)
  ExprAttr t _            -> collectWalrusTargets t
  ExprSubscript t idx     -> Set.union (collectWalrusTargets t) (collectWalrusTargets idx)
  ExprSlice ms me mst     -> Set.unions [maybe Set.empty collectWalrusTargets ms, maybe Set.empty collectWalrusTargets me, maybe Set.empty collectWalrusTargets mst]
  ExprList es             -> foldMap collectWalrusTargets es
  ExprTuple es            -> foldMap collectWalrusTargets es
  ExprDict pairs          -> foldMap (\(k, v) -> Set.union (collectWalrusTargets k) (collectWalrusTargets v)) pairs
  ExprSet es              -> foldMap collectWalrusTargets es
  ExprLambda _ body       -> collectWalrusTargets body
  ExprTernary c t f       -> Set.unions [collectWalrusTargets c, collectWalrusTargets t, collectWalrusTargets f]
  ExprListComp item comps -> Set.union (collectWalrusTargets item) (foldMap compWalrus comps)
  ExprDictComp k v comps  -> Set.unions [collectWalrusTargets k, collectWalrusTargets v, foldMap compWalrus comps]
  ExprSetComp item comps  -> Set.union (collectWalrusTargets item) (foldMap compWalrus comps)
  ExprGenerator item comps-> Set.union (collectWalrusTargets item) (foldMap compWalrus comps)
  ExprAwait e             -> collectWalrusTargets e
  ExprYield me            -> maybe Set.empty collectWalrusTargets me
  ExprYieldFrom e         -> collectWalrusTargets e
  ExprFormattedString ps  -> foldMap fpartWalrus ps
  ExprStarred e           -> collectWalrusTargets e
  ExprKwStarred e         -> collectWalrusTargets e
  ExprOptChain e _        -> collectWalrusTargets e
  ExprNullish e1 e2       -> Set.union (collectWalrusTargets e1) (collectWalrusTargets e2)
  ExprChanRecv ch         -> collectWalrusTargets ch
  ExprTryOp e             -> collectWalrusTargets e
  ExprMacroCall _ args    -> foldMap collectWalrusTargets args
  ExprJSX _ attrs children-> Set.unions (map (collectWalrusTargets . snd) attrs ++ map collectWalrusTargets children)
  _                       -> Set.empty
  where
    compWalrus (CompFor _ iter ifs) = Set.union (collectWalrusTargets iter) (foldMap collectWalrusTargets ifs)
    fpartWalrus (FStringExpr e _ _) = collectWalrusTargets e
    fpartWalrus _                  = Set.empty

getExprIds :: Expr -> [Text]
getExprIds expr = case expr of
  ExprId name         -> [name]
  ExprTuple es        -> concatMap getExprIds es
  ExprList es         -> concatMap getExprIds es
  ExprStarred e       -> getExprIds e
  ExprWalrus name _   -> [name]
  ExprCall _ args _   -> concatMap getExprIds args
  _                   -> []

getStmtExprs :: Stmt -> [Expr]
getStmtExprs stmt = case stmt of
  StmtAssign targets val     -> val : targets
  StmtAnnAssign target ty v  -> target : ty : maybe [] pure v
  StmtAugAssign t _ v        -> [t, v]
  StmtExpr e                 -> [e]
  StmtReturn me              -> maybe [] pure me
  StmtIf c b e               -> c : concatMap getStmtExprs b ++ concatMap getStmtExprs e
  StmtWhile c b e            -> c : concatMap getStmtExprs b ++ concatMap getStmtExprs e
  StmtFor t i b e            -> t : i : concatMap getStmtExprs b ++ concatMap getStmtExprs e
  StmtAsyncFor t i b e       -> t : i : concatMap getStmtExprs b ++ concatMap getStmtExprs e
  StmtTry b h e f            -> concatMap getStmtExprs b ++ concatMap (\(me, _, hb) -> maybe [] pure me ++ concatMap getStmtExprs hb) h ++ concatMap getStmtExprs e ++ concatMap getStmtExprs f
  StmtWith items b           -> concatMap (\(e, ma) -> e : maybe [] pure ma) items ++ concatMap getStmtExprs b
  StmtAsyncWith items b      -> concatMap (\(e, ma) -> e : maybe [] pure ma) items ++ concatMap getStmtExprs b
  StmtAssert e me            -> e : maybe [] pure me
  StmtRaise me mc            -> maybe [] pure me ++ maybe [] pure mc
  StmtDelete es              -> es
  StmtMatch s cs             -> s : concatMap (\mc -> mcPattern mc : maybe [] pure (mcGuard mc) ++ concatMap getStmtExprs (mcBody mc)) cs
  StmtGo e                   -> [e]
  StmtDefer e                -> [e]
  StmtChanSend ch val        -> [ch, val]
  StmtLoop b                 -> concatMap getStmtExprs b
  StmtSwitch s cases defS    -> s : concatMap (getStmtExprs . StmtExpr . fst) cases ++ concatMap (concatMap getStmtExprs . snd) cases ++ concatMap getStmtExprs defS
  _                          -> []

collectStmtRefs :: [Stmt] -> Set Text
collectStmtRefs stmts = foldMap (collectExprRefs . fst) [(e, ()) | e <- concatMap getStmtExprs stmts]

collectExprRefs :: Expr -> Set Text
collectExprRefs expr = case expr of
  ExprId name            -> Set.singleton name
  ExprLit _               -> Set.empty
  ExprBinary _ e1 e2      -> Set.union (collectExprRefs e1) (collectExprRefs e2)
  ExprUnary _ e           -> collectExprRefs e
  ExprCall t args kwargs  -> Set.unions (collectExprRefs t : map collectExprRefs args ++ map (collectExprRefs . snd) kwargs)
  ExprAttr t _            -> collectExprRefs t
  ExprSubscript t idx     -> Set.union (collectExprRefs t) (collectExprRefs idx)
  ExprSlice ms me mst     -> Set.unions [maybe Set.empty collectExprRefs ms, maybe Set.empty collectExprRefs me, maybe Set.empty collectExprRefs mst]
  ExprList es             -> foldMap collectExprRefs es
  ExprTuple es            -> foldMap collectExprRefs es
  ExprDict pairs          -> foldMap (\(k, v) -> Set.union (collectExprRefs k) (collectExprRefs v)) pairs
  ExprSet es              -> foldMap collectExprRefs es
  ExprLambda _ body       -> collectExprRefs body
  ExprTernary c t f       -> Set.unions [collectExprRefs c, collectExprRefs t, collectExprRefs f]
  ExprListComp item comps -> Set.union (collectExprRefs item) (foldMap compRefs comps)
  ExprDictComp k v comps  -> Set.unions [collectExprRefs k, collectExprRefs v, foldMap compRefs comps]
  ExprSetComp item comps  -> Set.union (collectExprRefs item) (foldMap compRefs comps)
  ExprGenerator item comps-> Set.union (collectExprRefs item) (foldMap compRefs comps)
  ExprWalrus name val     -> Set.insert name (collectExprRefs val)
  ExprAwait e             -> collectExprRefs e
  ExprYield me            -> maybe Set.empty collectExprRefs me
  ExprYieldFrom e         -> collectExprRefs e
  ExprFormattedString ps  -> foldMap fpartRefs ps
  ExprStarred e           -> collectExprRefs e
  ExprKwStarred e         -> collectExprRefs e
  ExprOptChain e _        -> collectExprRefs e
  ExprNullish e1 e2       -> Set.union (collectExprRefs e1) (collectExprRefs e2)
  ExprChanRecv ch         -> collectExprRefs ch
  ExprTryOp e             -> collectExprRefs e
  ExprMacroCall _ args    -> foldMap collectExprRefs args
  ExprJSX _ attrs children-> Set.unions (map (collectExprRefs . snd) attrs ++ map collectExprRefs children)
  where
    compRefs (CompFor _ iter ifs) = Set.union (collectExprRefs iter) (foldMap collectExprRefs ifs)
    fpartRefs (FStringText _)     = Set.empty
    fpartRefs (FStringExpr e _ _) = collectExprRefs e

recordReferences :: ScopeId -> Set Text -> Map Text SymbolBinding -> Map Text SymbolBinding
recordReferences sid refs syms =
  Map.mapWithKey updateBinding syms
  where
    updateBinding name b
      | Set.member name refs = b { symReferences = sid : symReferences b }
      | otherwise            = b

isPublic :: Text -> Bool
isPublic name = not (T.isPrefixOf "_" name)

findBinding :: Text -> ScopeTree -> Maybe SymbolBinding
findBinding name tree = Map.lookup name (scopeSymbols tree)

-- | Resolve a binding by name walking up the hierarchy of scope trees.
findBindingInHierarchy :: Text -> [ScopeTree] -> Maybe SymbolBinding
findBindingInHierarchy _ [] = Nothing
findBindingInHierarchy name (t:ts) = case Map.lookup name (scopeSymbols t) of
  Just b  -> Just b
  Nothing -> findBindingInHierarchy name ts

allBindings :: ScopeTree -> [SymbolBinding]
allBindings tree =
  Map.elems (scopeSymbols tree) ++ concatMap allBindings (scopeChildren tree)
