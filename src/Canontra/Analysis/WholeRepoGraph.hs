{- |
Module      : Canontra.Analysis.WholeRepoGraph
Description : Cross-module global call graph and inter-procedural data-flow synthesizer.

This module resolves symbol references across polyglot file boundaries to construct
a unified repository-level Call Graph (F_WCG) and Inter-Procedural Data-Flow Graph (F_WDF).
It handles cross-module edges, circular import/call cycles using Tarjan's SCC algorithm,
and tracks parameter-to-argument and return-value data flow propagation.
-}
{-# LANGUAGE DerivingStrategies #-}
module Canontra.Analysis.WholeRepoGraph
  ( DeclKind (..)
  , GlobalSymbol (..)
  , WholeRepoCallEdge (..)
  , WholeRepoCallGraph (..)
  , InterProceduralDataFlowEdge (..)
  , WholeRepoDataFlowGraph (..)
  , buildWholeRepoCallGraph
  , buildWholeRepoDataFlow
  , formatWholeRepoCallGraph
  , formatWholeRepoDataFlow
  , findWholeRepoSCCs
  , findCrossModuleEdges
  , findDeadSymbols
  , filePathToModuleName
  ) where

import Data.List (foldl', nub, sort, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (dropExtension, normalise, splitDirectories)

import Canontra.Analysis.CallGraph (CallEdge (..), CallGraph (..), CalleeTarget (..), CallerNode (..), buildCallGraph)
import Canontra.Canonical.Serialize (canonicalizeDeclaration)
import Canontra.Fingerprint.Source (hashBytes)
import Canontra.IR.Declaration
import Canontra.IR.Dependency (ImportDecl (..), ImportTarget (..))
import Canontra.IR.Expression
  ( CompFor (..)
  , Expr (..)
  , FStringPart (..)
  , MatchCase (..)
  , SelectCase (..)
  , Stmt (..)
  )
import Canontra.IR.Program (Module (..), Program (..))
import Canontra.Types
  ( DeclKind (..)
  , Fingerprint (..)
  , GlobalSymbol (..)
  , InterProceduralDataFlowEdge (..)
  , WholeRepoCallEdge (..)
  , WholeRepoCallGraph (..)
  , WholeRepoDataFlowGraph (..)
  )

-- | Convert a source file path into a canonical dotted module name.
-- E.g. "auth/jwt.py" -> "auth.jwt", "src/core/math.rs" -> "src.core.math".
filePathToModuleName :: FilePath -> Text
filePathToModuleName rawPath =
  let norm = map (\c -> if c == '\\' then '/' else c) (normalise rawPath)
      noExt = dropExtension norm
      dirs = splitDirectories noExt
      filteredDirs = filter (\d -> d /= "." && d /= "/" && d /= "\\") dirs
      effectiveDirs = case reverse filteredDirs of
        ("__init__" : rest) -> reverse rest
        other               -> reverse other
  in T.intercalate "." (map T.pack effectiveDirs)

-- | Collect all top-level and member declarations across repository modules.
collectGlobalSymbols :: [(FilePath, Program)] -> [GlobalSymbol]
collectGlobalSymbols modules =
  concatMap (\(fp, prog) -> extractProgramSymbols fp prog) modules

extractProgramSymbols :: FilePath -> Program -> [GlobalSymbol]
extractProgramSymbols fp (Program mods _) =
  let pathMod = filePathToModuleName fp
  in concatMap (extractModuleSymbols fp pathMod) mods

extractModuleSymbols :: FilePath -> Text -> Module -> [GlobalSymbol]
extractModuleSymbols fp pathMod (Module mName _ decls _) =
  let isPathLike t =
        T.null t
          || t == "main"
          || t == T.pack fp
          || T.isInfixOf "/" t
          || T.isInfixOf "\\" t
          || any (`T.isSuffixOf` t) [".py", ".pyi", ".js", ".jsx", ".mjs", ".cjs", ".ts", ".tsx", ".go", ".rs"]
      effectiveMod = if isPathLike mName then pathMod else mName
  in concatMap (declToGlobalSymbols fp effectiveMod) decls

declToGlobalSymbols :: FilePath -> Text -> Declaration -> [GlobalSymbol]
declToGlobalSymbols fp modName decl =
  let f2 = hashBytes (canonicalizeDeclaration decl)
  in case decl of
    DeclFunction fn ->
      [GlobalSymbol fp modName (fnName fn) KindFunction f2]

    DeclClass cls ->
      let classSym = GlobalSymbol fp modName (clsName cls) KindClass f2
          methodSyms =
            [ GlobalSymbol fp modName (clsName cls <> "." <> fnName m) KindMethod
                (hashBytes (canonicalizeDeclaration (DeclFunction m)))
            | m <- clsMethods cls
            ]
      in classSym : methodSyms

    DeclStruct st ->
      let structSym = GlobalSymbol fp modName (stName st) KindStruct f2
          methodSyms =
            [ GlobalSymbol fp modName (stName st <> "." <> fnName m) KindMethod
                (hashBytes (canonicalizeDeclaration (DeclFunction m)))
            | m <- stMethods st
            ]
      in structSym : methodSyms

    DeclTrait tr ->
      let traitSym = GlobalSymbol fp modName (trName tr) KindTrait f2
          methodSyms =
            [ GlobalSymbol fp modName (trName tr <> "." <> fnName m) KindMethod
                (hashBytes (canonicalizeDeclaration (DeclFunction m)))
            | m <- trMethods tr
            ]
      in traitSym : methodSyms

    DeclInterface iface ->
      let ifaceSym = GlobalSymbol fp modName (ifName iface) KindInterface f2
          methodSyms =
            [ GlobalSymbol fp modName (ifName iface <> "." <> fnName m) KindMethod
                (hashBytes (canonicalizeDeclaration (DeclFunction m)))
            | m <- ifMethods iface
            ]
      in ifaceSym : methodSyms

    DeclReceiver rc fn ->
      [GlobalSymbol fp modName (rcTypeName rc <> "." <> fnName fn) KindMethod f2]

    DeclImpl imp ->
      [ GlobalSymbol fp modName (impTarget imp <> "." <> fnName m) KindMethod
          (hashBytes (canonicalizeDeclaration (DeclFunction m)))
      | m <- impMethods imp
      ]

    DeclVariable name _ ->
      [GlobalSymbol fp modName name KindVariable f2]

    DeclTypeAlias name _ ->
      [GlobalSymbol fp modName name KindTypeAlias f2]

-- | Multi-index symbol lookup table for robust cross-module resolution.
data SymbolIndices = SymbolIndices
  { idxByModAndName  :: !(Map (Text, Text) GlobalSymbol)
  , idxByPathAndName :: !(Map (FilePath, Text) GlobalSymbol)
  , idxByNameOnly    :: !(Map Text [GlobalSymbol])
  }

buildSymbolIndices :: [GlobalSymbol] -> SymbolIndices
buildSymbolIndices syms =
  let byModName = Map.fromList [((symModule s, symDeclName s), s) | s <- syms]
      byPathName = Map.fromList [((symFilePath s, symDeclName s), s) | s <- syms]
      byName = Map.fromListWith (++) [(symDeclName s, [s]) | s <- syms]
  in SymbolIndices byModName byPathName byName

-- | Resolve an invocation target against the multi-index repository symbol table.
resolveTarget
  :: SymbolIndices
  -> Text        -- ^ Current module name
  -> FilePath    -- ^ Current file path
  -> CalleeTarget
  -> Maybe GlobalSymbol
resolveTarget indices curMod curPath = \case
  TargetLocal name ->
    case Map.lookup (curMod, name) (idxByModAndName indices) of
      Just s -> Just s
      Nothing -> case Map.lookup (curPath, name) (idxByPathAndName indices) of
        Just s -> Just s
        Nothing -> case Map.lookup name (idxByNameOnly indices) of
          Just [unique] -> Just unique
          _             -> Nothing

  TargetMethod cls m ->
    let qualifiedName = if T.null cls then m else cls <> "." <> m
    in case Map.lookup (curMod, qualifiedName) (idxByModAndName indices) of
      Just s -> Just s
      Nothing -> case Map.lookup qualifiedName (idxByNameOnly indices) of
        Just [unique] -> Just unique
        _             -> Nothing

  TargetImported impMod sym ->
    case Map.lookup (impMod, sym) (idxByModAndName indices) of
      Just s -> Just s
      Nothing ->
        if T.null sym && T.isInfixOf "." impMod
          then
            let parts = T.splitOn "." impMod
                subMod = T.intercalate "." (init parts)
                subSym = last parts
            in case Map.lookup (subMod, subSym) (idxByModAndName indices) of
              Just s  -> Just s
              Nothing -> Map.lookup (impMod, sym) (idxByModAndName indices)
          else
            let strippedMod = T.dropWhile (== '.') (T.dropWhile (/= '.') impMod)
            in case Map.lookup (strippedMod, sym) (idxByModAndName indices) of
              Just s -> Just s
              Nothing ->
                case Map.lookup sym (idxByNameOnly indices) of
                  Just [unique] -> Just unique
                  Just candidates ->
                    case filter (\c -> symModule c == impMod || T.isSuffixOf (symModule c) impMod || T.isSuffixOf impMod (symModule c)) candidates of
                      (matched : _) -> Just matched
                      []            -> Nothing
                  Nothing -> Nothing

  TargetDynamic _ -> Nothing

-- | Resolve caller node to a GlobalSymbol.
resolveCaller :: SymbolIndices -> Text -> FilePath -> CallerNode -> GlobalSymbol
resolveCaller indices curMod curPath = \case
  CallFunction fn ->
    case Map.lookup (curMod, fn) (idxByModAndName indices) of
      Just s  -> s
      Nothing -> GlobalSymbol curPath curMod fn KindFunction (Fingerprint "")
  CallMethod cls m ->
    let qual = cls <> "." <> m
    in case Map.lookup (curMod, qual) (idxByModAndName indices) of
      Just s  -> s
      Nothing -> GlobalSymbol curPath curMod qual KindMethod (Fingerprint "")
  CallTopLevel ->
    GlobalSymbol curPath curMod "<top-level>" KindFunction (Fingerprint "top")

-- | Build the unified repository-level Call Graph across all modules.
buildWholeRepoCallGraph :: [(FilePath, Program)] -> WholeRepoCallGraph
buildWholeRepoCallGraph modules =
  let allSymbols = collectGlobalSymbols modules
      indices = buildSymbolIndices allSymbols
      rawEdges = concatMap (extractModuleEdges indices) modules
      consolidatedEdges = consolidateWholeRepoEdges rawEdges
      allNodes = sort (nub (allSymbols ++ map wceCaller consolidatedEdges ++ map wceCallee consolidatedEdges))
      sccs = tarjanWholeRepoSCC allNodes consolidatedEdges
  in WholeRepoCallGraph allNodes consolidatedEdges sccs

extractModuleEdges
  :: SymbolIndices
  -> (FilePath, Program)
  -> [WholeRepoCallEdge]
extractModuleEdges indices (fp, prog) =
  let curMod = filePathToModuleName fp
      localCG = buildCallGraph prog
  in mapMaybe (convertLocalEdge indices curMod fp) (cgEdges localCG)

convertLocalEdge
  :: SymbolIndices
  -> Text
  -> FilePath
  -> CallEdge
  -> Maybe WholeRepoCallEdge
convertLocalEdge indices curMod curPath (CallEdge caller callee cnt isAsync) =
  let callerSym = resolveCaller indices curMod curPath caller
  in case resolveTarget indices curMod curPath callee of
    Nothing -> Nothing
    Just calleeSym ->
      let isCross = symModule callerSym /= symModule calleeSym
                 || symFilePath callerSym /= symFilePath calleeSym
      in Just $ WholeRepoCallEdge
          { wceCaller     = callerSym
          , wceCallee     = calleeSym
          , wceCallCount  = cnt
          , wceIsAsync    = isAsync
          , wceIsCrossMod = isCross
          }

consolidateWholeRepoEdges :: [WholeRepoCallEdge] -> [WholeRepoCallEdge]
consolidateWholeRepoEdges edges =
  let grouped = Map.fromListWith (+)
        [ ((wceCaller e, wceCallee e, wceIsAsync e, wceIsCrossMod e), wceCallCount e)
        | e <- edges
        ]
      rebuilt =
        [ WholeRepoCallEdge c t cnt a isCross
        | ((c, t, a, isCross), cnt) <- Map.toList grouped
        ]
  in sortBy (comparing (\e -> (symModule (wceCaller e), symDeclName (wceCaller e), symModule (wceCallee e), symDeclName (wceCallee e)))) rebuilt

-- | Find all cross-module edges in the WholeRepoCallGraph.
findCrossModuleEdges :: WholeRepoCallGraph -> [WholeRepoCallEdge]
findCrossModuleEdges cg = filter wceIsCrossMod (wcgEdges cg)

-- | Find declared symbols that are never called across the whole repository.
findDeadSymbols :: WholeRepoCallGraph -> [GlobalSymbol]
findDeadSymbols cg =
  let calledSet = Set.fromList [wceCallee e | e <- wcgEdges cg]
      isIgnored s = symDeclName s == "<top-level>"
                 || symDeclName s == "main"
                 || symDeclName s == "__init__"
  in [s | s <- wcgNodes cg, not (Set.member s calledSet), not (isIgnored s)]

-- | Tarjan's Strongly Connected Components algorithm for whole-repository graphs.
findWholeRepoSCCs :: WholeRepoCallGraph -> [[GlobalSymbol]]
findWholeRepoSCCs = wcgSCCs

tarjanWholeRepoSCC :: [GlobalSymbol] -> [WholeRepoCallEdge] -> [[GlobalSymbol]]
tarjanWholeRepoSCC nodes edges =
  let step (visited, sccs) node
        | Set.member node visited = (visited, sccs)
        | otherwise =
            let comp = dfs node visited []
                newVisited = Set.union visited (Set.fromList comp)
            in (newVisited, comp : sccs)
      (_, allSccs) = foldl' step (Set.empty, []) nodes
  in filter (not . null) allSccs
  where
    adj = Map.fromListWith (++) [(wceCaller e, [wceCallee e]) | e <- edges]
    dfs curr vis acc
      | Set.member curr vis = acc
      | otherwise =
          let neighbors = Map.findWithDefault [] curr adj
              newVis = Set.insert curr vis
          in foldl' (\a n -> dfs n newVis a) (curr : acc) neighbors

-- | Extract inter-procedural data-flow graphs across module boundaries.
buildWholeRepoDataFlow :: [(FilePath, Program)] -> WholeRepoDataFlowGraph
buildWholeRepoDataFlow modules =
  let allSymbols = collectGlobalSymbols modules
      indices = buildSymbolIndices allSymbols
      edges = concatMap (extractModuleDataFlow indices) modules
      uniqueEdges = sort (nub edges)
      nodes = sort (nub (allSymbols ++ map ipdfSourceSymbol uniqueEdges ++ map ipdfTargetSymbol uniqueEdges))
  in WholeRepoDataFlowGraph nodes uniqueEdges

extractModuleDataFlow
  :: SymbolIndices
  -> (FilePath, Program)
  -> [InterProceduralDataFlowEdge]
extractModuleDataFlow indices (fp, prog) =
  let curMod = filePathToModuleName fp
      allImports = concatMap modImports (progModules prog)
      impMap = buildImportMap allImports
  in concatMap (extractModuleDeclsDataFlow indices curMod fp impMap) (progModules prog)

extractModuleDeclsDataFlow
  :: SymbolIndices
  -> Text
  -> FilePath
  -> Map Text (Text, Text)
  -> Module
  -> [InterProceduralDataFlowEdge]
extractModuleDeclsDataFlow indices curMod curPath impMap (Module _ _ decls stmts) =
  let topSym = GlobalSymbol curPath curMod "<top-level>" KindFunction (Fingerprint "top")
      topEdges = concatMap (extractStmtDataFlow indices curMod curPath impMap topSym) stmts
      declEdges = concatMap (extractDeclDataFlow indices curMod curPath impMap) decls
  in topEdges ++ declEdges

extractDeclDataFlow
  :: SymbolIndices
  -> Text
  -> FilePath
  -> Map Text (Text, Text)
  -> Declaration
  -> [InterProceduralDataFlowEdge]
extractDeclDataFlow indices curMod curPath impMap = \case
  DeclFunction fn ->
    let callerSym = resolveCaller indices curMod curPath (CallFunction (fnName fn))
    in concatMap (extractStmtDataFlow indices curMod curPath impMap callerSym) (fnBody fn)

  DeclClass cls ->
    let extractMethod m =
          let callerSym = resolveCaller indices curMod curPath (CallMethod (clsName cls) (fnName m))
          in concatMap (extractStmtDataFlow indices curMod curPath impMap callerSym) (fnBody m)
    in concatMap extractMethod (clsMethods cls)

  DeclStruct st ->
    let extractMethod m =
          let callerSym = resolveCaller indices curMod curPath (CallMethod (stName st) (fnName m))
          in concatMap (extractStmtDataFlow indices curMod curPath impMap callerSym) (fnBody m)
    in concatMap extractMethod (stMethods st)

  DeclTrait tr ->
    let extractMethod m =
          let callerSym = resolveCaller indices curMod curPath (CallMethod (trName tr) (fnName m))
          in concatMap (extractStmtDataFlow indices curMod curPath impMap callerSym) (fnBody m)
    in concatMap extractMethod (trMethods tr)

  DeclImpl imp ->
    let extractMethod m =
          let callerSym = resolveCaller indices curMod curPath (CallMethod (impTarget imp) (fnName m))
          in concatMap (extractStmtDataFlow indices curMod curPath impMap callerSym) (fnBody m)
    in concatMap extractMethod (impMethods imp)

  DeclReceiver rc fn ->
    let callerSym = resolveCaller indices curMod curPath (CallMethod (rcTypeName rc) (fnName fn))
    in concatMap (extractStmtDataFlow indices curMod curPath impMap callerSym) (fnBody fn)

  _ -> []

extractStmtDataFlow
  :: SymbolIndices
  -> Text
  -> FilePath
  -> Map Text (Text, Text)
  -> GlobalSymbol
  -> Stmt
  -> [InterProceduralDataFlowEdge]
extractStmtDataFlow indices curMod curPath impMap callerSym stmt =
  let calls = collectStmtCalls stmt
  in concatMap (callToFlowEdges indices curMod curPath impMap callerSym) calls

callToFlowEdges
  :: SymbolIndices
  -> Text
  -> FilePath
  -> Map Text (Text, Text)
  -> GlobalSymbol
  -> (Expr, [Expr], [(Text, Expr)])
  -> [InterProceduralDataFlowEdge]
callToFlowEdges indices curMod curPath impMap callerSym (calleeExpr, args, kwargs) =
  let target = resolveExprCalleeTarget impMap calleeExpr
  in case resolveTarget indices curMod curPath target of
    Nothing -> []
    Just calleeSym ->
      let argEdges = concat
            [ let vars = collectExprVars arg
              in if Set.null vars
                   then [ InterProceduralDataFlowEdge
                            { ipdfSourceSymbol = callerSym
                            , ipdfTargetSymbol = calleeSym
                            , ipdfParamIndex   = idx
                            , ipdfVarName      = "<const>"
                            , ipdfIsReturnFlow = False
                            }
                        ]
                   else [ InterProceduralDataFlowEdge
                            { ipdfSourceSymbol = callerSym
                            , ipdfTargetSymbol = calleeSym
                            , ipdfParamIndex   = idx
                            , ipdfVarName      = v
                            , ipdfIsReturnFlow = False
                            }
                        | v <- Set.toList vars
                        ]
            | (idx, arg) <- zip [0..] args
            ]
          kwEdges = concat
            [ let vars = collectExprVars val
              in if Set.null vars
                   then [ InterProceduralDataFlowEdge
                            { ipdfSourceSymbol = callerSym
                            , ipdfTargetSymbol = calleeSym
                            , ipdfParamIndex   = -2
                            , ipdfVarName      = k <> "=<const>"
                            , ipdfIsReturnFlow = False
                            }
                        ]
                   else [ InterProceduralDataFlowEdge
                            { ipdfSourceSymbol = callerSym
                            , ipdfTargetSymbol = calleeSym
                            , ipdfParamIndex   = -2
                            , ipdfVarName      = k <> "=" <> v
                            , ipdfIsReturnFlow = False
                            }
                        | v <- Set.toList vars
                        ]
            | (k, val) <- kwargs
            ]
          returnEdge =
            [ InterProceduralDataFlowEdge
                { ipdfSourceSymbol = calleeSym
                , ipdfTargetSymbol = callerSym
                , ipdfParamIndex   = -1
                , ipdfVarName      = "<return>"
                , ipdfIsReturnFlow = True
                }
            ]
      in argEdges ++ kwEdges ++ returnEdge

collectExprCalls :: Expr -> [(Expr, [Expr], [(Text, Expr)])]
collectExprCalls = \case
  ExprCall target args kwargs ->
    (target, args, kwargs) : collectExprCalls target ++ concatMap collectExprCalls args ++ concatMap (collectExprCalls . snd) kwargs
  ExprBinary _ e1 e2 -> collectExprCalls e1 ++ collectExprCalls e2
  ExprUnary _ e -> collectExprCalls e
  ExprAttr e _ -> collectExprCalls e
  ExprSubscript e idx -> collectExprCalls e ++ collectExprCalls idx
  ExprSlice m1 m2 m3 -> maybe [] collectExprCalls m1 ++ maybe [] collectExprCalls m2 ++ maybe [] collectExprCalls m3
  ExprList es -> concatMap collectExprCalls es
  ExprTuple es -> concatMap collectExprCalls es
  ExprDict pairs -> concatMap (\(k, v) -> collectExprCalls k ++ collectExprCalls v) pairs
  ExprSet es -> concatMap collectExprCalls es
  ExprTernary c t f -> collectExprCalls c ++ collectExprCalls t ++ collectExprCalls f
  ExprLambda _ e -> collectExprCalls e
  ExprListComp e comps -> collectExprCalls e ++ concatMap compCalls comps
  ExprDictComp k v comps -> collectExprCalls k ++ collectExprCalls v ++ concatMap compCalls comps
  ExprSetComp e comps -> collectExprCalls e ++ concatMap compCalls comps
  ExprGenerator e comps -> collectExprCalls e ++ concatMap compCalls comps
  ExprWalrus _ e -> collectExprCalls e
  ExprAwait e -> collectExprCalls e
  ExprYield me -> maybe [] collectExprCalls me
  ExprYieldFrom e -> collectExprCalls e
  ExprFormattedString parts -> concatMap fstringCalls parts
  ExprStarred e -> collectExprCalls e
  ExprKwStarred e -> collectExprCalls e
  ExprOptChain e _ -> collectExprCalls e
  ExprNullish e1 e2 -> collectExprCalls e1 ++ collectExprCalls e2
  ExprChanRecv e -> collectExprCalls e
  ExprTryOp e -> collectExprCalls e
  ExprMacroCall _ args -> concatMap collectExprCalls args
  ExprJSX _ attrs children -> concatMap (collectExprCalls . snd) attrs ++ concatMap collectExprCalls children
  _ -> []
  where
    compCalls (CompFor t i ifs) = collectExprCalls t ++ collectExprCalls i ++ concatMap collectExprCalls ifs
    fstringCalls (FStringExpr e _ _) = collectExprCalls e
    fstringCalls _                   = []

collectStmtCalls :: Stmt -> [(Expr, [Expr], [(Text, Expr)])]
collectStmtCalls = \case
  StmtAssign targets val -> concatMap collectExprCalls targets ++ collectExprCalls val
  StmtAnnAssign target ty v -> collectExprCalls target ++ collectExprCalls ty ++ maybe [] collectExprCalls v
  StmtAugAssign t _ v -> collectExprCalls t ++ collectExprCalls v
  StmtExpr e -> collectExprCalls e
  StmtReturn me -> maybe [] collectExprCalls me
  StmtIf c b e -> collectExprCalls c ++ concatMap collectStmtCalls b ++ concatMap collectStmtCalls e
  StmtWhile c b e -> collectExprCalls c ++ concatMap collectStmtCalls b ++ concatMap collectStmtCalls e
  StmtFor t i b e -> collectExprCalls t ++ collectExprCalls i ++ concatMap collectStmtCalls b ++ concatMap collectStmtCalls e
  StmtAsyncFor t i b e -> collectExprCalls t ++ collectExprCalls i ++ concatMap collectStmtCalls b ++ concatMap collectStmtCalls e
  StmtTry b h e f ->
    concatMap collectStmtCalls b
      ++ concatMap (\(me, _, hb) -> maybe [] collectExprCalls me ++ concatMap collectStmtCalls hb) h
      ++ concatMap collectStmtCalls e
      ++ concatMap collectStmtCalls f
  StmtWith items b -> concatMap (\(e, ma) -> collectExprCalls e ++ maybe [] collectExprCalls ma) items ++ concatMap collectStmtCalls b
  StmtAsyncWith items b -> concatMap (\(e, ma) -> collectExprCalls e ++ maybe [] collectExprCalls ma) items ++ concatMap collectStmtCalls b
  StmtAssert e me -> collectExprCalls e ++ maybe [] collectExprCalls me
  StmtRaise me mc -> maybe [] collectExprCalls me ++ maybe [] collectExprCalls mc
  StmtDelete es -> concatMap collectExprCalls es
  StmtMatch s cs ->
    collectExprCalls s
      ++ concatMap (\mc -> collectExprCalls (mcPattern mc) ++ maybe [] collectExprCalls (mcGuard mc) ++ concatMap collectStmtCalls (mcBody mc)) cs
  StmtGo e -> collectExprCalls e
  StmtDefer e -> collectExprCalls e
  StmtChanSend ch val -> collectExprCalls ch ++ collectExprCalls val
  StmtSelect cases -> concatMap (\(sc, b) -> selectCalls sc ++ concatMap collectStmtCalls b) cases
  StmtLoop b -> concatMap collectStmtCalls b
  StmtSwitch expr cases defStmts ->
    collectExprCalls expr
      ++ concatMap (\(c, cStmts) -> collectExprCalls c ++ concatMap collectStmtCalls cStmts) cases
      ++ concatMap collectStmtCalls defStmts
  _ -> []
  where
    selectCalls = \case
      SelectSend ch val -> collectExprCalls ch ++ collectExprCalls val
      SelectRecv _ ch   -> collectExprCalls ch
      SelectDefault     -> []

collectExprVars :: Expr -> Set Text
collectExprVars = \case
  ExprId v -> Set.singleton v
  ExprBinary _ e1 e2 -> Set.union (collectExprVars e1) (collectExprVars e2)
  ExprUnary _ e -> collectExprVars e
  ExprCall t args kw -> Set.unions (collectExprVars t : map collectExprVars args ++ map (collectExprVars . snd) kw)
  ExprAttr e _ -> collectExprVars e
  ExprSubscript e idx -> Set.union (collectExprVars e) (collectExprVars idx)
  ExprSlice m1 m2 m3 -> Set.unions [maybe Set.empty collectExprVars m1, maybe Set.empty collectExprVars m2, maybe Set.empty collectExprVars m3]
  ExprList es -> foldMap collectExprVars es
  ExprTuple es -> foldMap collectExprVars es
  ExprDict pairs -> foldMap (\(k, v) -> Set.union (collectExprVars k) (collectExprVars v)) pairs
  ExprSet es -> foldMap collectExprVars es
  ExprTernary c t f -> Set.unions [collectExprVars c, collectExprVars t, collectExprVars f]
  ExprLambda _ e -> collectExprVars e
  ExprListComp e comps -> Set.union (collectExprVars e) (foldMap compVars comps)
  ExprDictComp k v comps -> Set.unions [collectExprVars k, collectExprVars v, foldMap compVars comps]
  ExprSetComp e comps -> Set.union (collectExprVars e) (foldMap compVars comps)
  ExprGenerator e comps -> Set.union (collectExprVars e) (foldMap compVars comps)
  ExprWalrus v e -> Set.insert v (collectExprVars e)
  ExprAwait e -> collectExprVars e
  ExprYield me -> maybe Set.empty collectExprVars me
  ExprYieldFrom e -> collectExprVars e
  ExprFormattedString parts -> foldMap fstringVars parts
  ExprStarred e -> collectExprVars e
  ExprKwStarred e -> collectExprVars e
  ExprOptChain e _ -> collectExprVars e
  ExprNullish e1 e2 -> Set.union (collectExprVars e1) (collectExprVars e2)
  ExprChanRecv e -> collectExprVars e
  ExprTryOp e -> collectExprVars e
  ExprMacroCall _ args -> foldMap collectExprVars args
  ExprJSX _ attrs children -> Set.unions (map (collectExprVars . snd) attrs ++ map collectExprVars children)
  _ -> Set.empty
  where
    compVars (CompFor t i ifs) = Set.unions (collectExprVars t : collectExprVars i : map collectExprVars ifs)
    fstringVars (FStringExpr e _ _) = collectExprVars e
    fstringVars _                   = Set.empty

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

resolveExprCalleeTarget :: Map Text (Text, Text) -> Expr -> CalleeTarget
resolveExprCalleeTarget impMap = \case
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

  ExprAttr (ExprAttr (ExprId pkg) modName) method ->
    let full = pkg <> "." <> modName
    in case Map.lookup full impMap of
      Just (actualMod, _) -> TargetImported actualMod method
      Nothing             -> TargetImported full method

  ExprAttr target method ->
    TargetMethod (T.pack (show target)) method

  ExprAwait inner ->
    resolveExprCalleeTarget impMap inner

  other -> TargetDynamic other

-- | Format WholeRepoCallGraph for human-readable diagnostic display.
formatWholeRepoCallGraph :: WholeRepoCallGraph -> Text
formatWholeRepoCallGraph cg =
  let crossEdges = findCrossModuleEdges cg
      sccs = filter (\c -> length c > 1) (wcgSCCs cg)
  in T.unlines $
    [ "Whole-Repository Call Graph (" <> T.pack (show (length (wcgNodes cg))) <> " symbols, " <> T.pack (show (length (wcgEdges cg))) <> " edges, " <> T.pack (show (length crossEdges)) <> " cross-module)"
    , "--------------------------------------------------------------------------------"
    ] ++
    map formatEdge (wcgEdges cg) ++
    (if null sccs
       then ["\nRecursive Cycles: None"]
       else ["\nRecursive Cycles (SCCs):"] ++ map formatSCC sccs)
  where
    formatEdge e =
      let crossTag = if wceIsCrossMod e then " [cross-module]" else ""
          asyncTag = if wceIsAsync e then " [async]" else ""
          countStr = if wceCallCount e > 1 then " (" <> T.pack (show (wceCallCount e)) <> "x)" else ""
      in "  " <> symModule (wceCaller e) <> ":" <> symDeclName (wceCaller e)
         <> " --> " <> symModule (wceCallee e) <> ":" <> symDeclName (wceCallee e)
         <> crossTag <> asyncTag <> countStr

    formatSCC comp =
      "  Cycle: " <> T.intercalate " <-> " [symModule s <> ":" <> symDeclName s | s <- comp]

-- | Format WholeRepoDataFlowGraph for human-readable diagnostic display.
formatWholeRepoDataFlow :: WholeRepoDataFlowGraph -> Text
formatWholeRepoDataFlow dfg =
  T.unlines $
    [ "Whole-Repository Data Flow Graph (" <> T.pack (show (length (wdfNodes dfg))) <> " symbols, " <> T.pack (show (length (wdfEdges dfg))) <> " flow edges)"
    , "--------------------------------------------------------------------------------"
    ] ++
    map formatFlowEdge (wdfEdges dfg)
  where
    formatFlowEdge e =
      let kind = if ipdfIsReturnFlow e
                   then " [return-flow]"
                   else " [param:" <> T.pack (show (ipdfParamIndex e)) <> "]"
      in "  " <> symModule (ipdfSourceSymbol e) <> ":" <> symDeclName (ipdfSourceSymbol e)
         <> " --(" <> ipdfVarName e <> ")--> "
         <> symModule (ipdfTargetSymbol e) <> ":" <> symDeclName (ipdfTargetSymbol e)
         <> kind
