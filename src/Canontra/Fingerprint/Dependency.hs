{- |
Module      : Canontra.Fingerprint.Dependency
Description : F3 rich dependency fingerprinting.

Dependency fingerprints focus exclusively on external and internal linkages.
By distilling the import graph, relative references, and resolved usage classifications
(unused, direct call, inheritance, type-only, value ref) independently of local business logic,
canontra enables instant detection of dependency updates or modular coupling changes.
-}
module Canontra.Fingerprint.Dependency
  ( computeF3
  , extractDependencies
  , extractRichDependencyGraph
  ) where

import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Canontra.Canonical.Serialize (canonicalizeRichDependencyGraph)
import Canontra.Fingerprint.Source (hashBytes)
import Canontra.IR.Declaration
import Canontra.IR.Dependency
import Canontra.IR.Expression
import Canontra.IR.Program
import Canontra.Normalize.Normalize (normalizeProgram)
import Canontra.Types (Fingerprint)

computeF3 :: Program -> Fingerprint -- e.g. computeF3 prog -> F3 rich dependency hash
computeF3 prog =
  let normProg = normalizeProgram prog
      rdg = extractRichDependencyGraph normProg
      canonBytes = canonicalizeRichDependencyGraph rdg
  in hashBytes canonBytes

extractDependencies :: Program -> [ImportDecl] -- e.g. gathers all import statements across modules
extractDependencies (Program modules _) = concatMap modImports modules

extractRichDependencyGraph :: Program -> RichDependencyGraph
extractRichDependencyGraph prog@(Program modules _) =
  let allImps = concatMap modImports modules
      allCalls = collectCallNames prog
      allBases = collectBaseNames prog
      allTypes = collectTypeNames prog
      allValRefs = collectValRefNames prog
      resolvedImps = sort $ concatMap (toResolvedImports allCalls allBases allTypes allValRefs) allImps
      intraDeps = sort $ collectIntraModuleDeps prog
  in RichDependencyGraph resolvedImps intraDeps

toResolvedImports :: Set Text -> Set Text -> Set Text -> Set Text -> ImportDecl -> [ResolvedImport]
toResolvedImports calls bases types valRefs imp = case imp of
  ImportModule modName maybeAlias ->
    let sym = maybe (lastPart modName) id maybeAlias
        usage = classifyUsage sym calls bases types valRefs
    in [ResolvedImport modName Nothing maybeAlias (countLeadingDots modName) usage]

  ImportFrom modName target -> case target of
    ImportAll ->
      [ResolvedImport modName (Just "*") Nothing (countLeadingDots modName) (DepValueRef ["*"])]
    ImportSymbols syms ->
      [ let boundName = maybe symName id maybeAlias
            usage = classifyUsage boundName calls bases types valRefs
        in ResolvedImport modName (Just symName) maybeAlias (countLeadingDots modName) usage
      | (symName, maybeAlias) <- syms
      ]
  where
    lastPart m = case T.splitOn "." m of
      [] -> m
      xs -> last xs

classifyUsage :: Text -> Set Text -> Set Text -> Set Text -> Set Text -> DependencyUsage
classifyUsage sym calls bases types valRefs
  | Set.member sym bases   = DepInheritance [sym]
  | Set.member sym calls   = DepDirectCall [sym]
  | Set.member sym valRefs = DepValueRef [sym]
  | Set.member sym types   = DepTypeOnly [sym]
  | otherwise              = DepUnused

countLeadingDots :: Text -> Int
countLeadingDots t = T.length (T.takeWhile (== '.') t)

collectCallNames :: Program -> Set Text
collectCallNames (Program modules _) =
  foldMap modCalls modules
  where
    modCalls (Module _ _ decls stmts) =
      foldMap declCalls decls `Set.union` foldMap stmtCalls stmts

    declCalls (DeclFunction fn) = foldMap stmtCalls (fnBody fn)
    declCalls (DeclClass cls) = foldMap (foldMap stmtCalls . fnBody) (clsMethods cls)
    declCalls (DeclStruct st) = foldMap (foldMap stmtCalls . fnBody) (stMethods st)
    declCalls (DeclTrait tr) = foldMap (foldMap stmtCalls . fnBody) (trMethods tr)
    declCalls (DeclImpl imp) = foldMap (foldMap stmtCalls . fnBody) (impMethods imp)
    declCalls (DeclReceiver _ fn) = foldMap stmtCalls (fnBody fn)
    declCalls (DeclVariable _ _) = Set.empty
    declCalls (DeclInterface _) = Set.empty
    declCalls (DeclTypeAlias _ _) = Set.empty

    stmtCalls stmt = case stmt of
      StmtAssign targets val -> foldMap exprCalls (val : targets)
      StmtAnnAssign target ty v -> foldMap exprCalls (target : ty : maybe [] pure v)
      StmtAugAssign t _ v -> foldMap exprCalls [t, v]
      StmtExpr e -> exprCalls e
      StmtReturn me -> maybe Set.empty exprCalls me
      StmtIf c b e -> Set.unions [exprCalls c, foldMap stmtCalls b, foldMap stmtCalls e]
      StmtWhile c b e -> Set.unions [exprCalls c, foldMap stmtCalls b, foldMap stmtCalls e]
      StmtFor t i b e -> Set.unions [exprCalls t, exprCalls i, foldMap stmtCalls b, foldMap stmtCalls e]
      StmtAsyncFor t i b e -> Set.unions [exprCalls t, exprCalls i, foldMap stmtCalls b, foldMap stmtCalls e]
      StmtTry b h e f -> Set.unions [foldMap stmtCalls b, foldMap (\(me, _, hb) -> maybe Set.empty exprCalls me `Set.union` foldMap stmtCalls hb) h, foldMap stmtCalls e, foldMap stmtCalls f]
      StmtWith items b -> Set.unions (map (\(e, ma) -> exprCalls e `Set.union` maybe Set.empty exprCalls ma) items ++ [foldMap stmtCalls b])
      StmtAsyncWith items b -> Set.unions (map (\(e, ma) -> exprCalls e `Set.union` maybe Set.empty exprCalls ma) items ++ [foldMap stmtCalls b])
      StmtAssert e me -> exprCalls e `Set.union` maybe Set.empty exprCalls me
      StmtRaise me mc -> maybe Set.empty exprCalls me `Set.union` maybe Set.empty exprCalls mc
      StmtDelete es -> foldMap exprCalls es
      StmtMatch s cs -> exprCalls s `Set.union` foldMap (\mc -> exprCalls (mcPattern mc) `Set.union` maybe Set.empty exprCalls (mcGuard mc) `Set.union` foldMap stmtCalls (mcBody mc)) cs
      _ -> Set.empty

    exprCalls expr = case expr of
      ExprCall (ExprId name) args kwargs ->
        Set.insert name (Set.unions (map exprCalls args ++ map (exprCalls . snd) kwargs))
      ExprCall (ExprAttr (ExprId obj) _) args kwargs ->
        Set.insert obj (Set.unions (map exprCalls args ++ map (exprCalls . snd) kwargs))
      ExprCall target args kwargs ->
        Set.unions (exprCalls target : map exprCalls args ++ map (exprCalls . snd) kwargs)
      ExprBinary _ e1 e2 -> exprCalls e1 `Set.union` exprCalls e2
      ExprUnary _ e -> exprCalls e
      ExprAttr e _ -> exprCalls e
      ExprSubscript e idx -> exprCalls e `Set.union` exprCalls idx
      ExprList es -> foldMap exprCalls es
      ExprTuple es -> foldMap exprCalls es
      ExprDict pairs -> foldMap (\(k, v) -> exprCalls k `Set.union` exprCalls v) pairs
      ExprSet es -> foldMap exprCalls es
      ExprLambda _ body -> exprCalls body
      ExprTernary c t f -> Set.unions [exprCalls c, exprCalls t, exprCalls f]
      ExprListComp item comps -> exprCalls item `Set.union` foldMap compCalls comps
      ExprDictComp k v comps -> exprCalls k `Set.union` exprCalls v `Set.union` foldMap compCalls comps
      ExprSetComp item comps -> exprCalls item `Set.union` foldMap compCalls comps
      ExprGenerator item comps -> exprCalls item `Set.union` foldMap compCalls comps
      ExprWalrus _ val -> exprCalls val
      ExprAwait e -> exprCalls e
      ExprYield me -> maybe Set.empty exprCalls me
      ExprYieldFrom e -> exprCalls e
      ExprStarred e -> exprCalls e
      ExprKwStarred e -> exprCalls e
      _ -> Set.empty

    compCalls (CompFor t iter ifs) = Set.unions [exprCalls t, exprCalls iter, foldMap exprCalls ifs]

collectBaseNames :: Program -> Set Text
collectBaseNames (Program modules _) =
  Set.fromList [b | Module _ _ decls _ <- modules, DeclClass cls <- decls, b <- clsBases cls]

collectTypeNames :: Program -> Set Text
collectTypeNames (Program modules _) =
  Set.fromList $ concatMap modTypes modules
  where
    modTypes (Module _ _ decls _) = concatMap declTypes decls
    declTypes (DeclFunction fn) =
      maybe [] pure (fnReturnType fn) ++ [t | p <- fnParams fn, Just t <- [paramType p]]
    declTypes (DeclClass cls) =
      concatMap (declTypes . DeclFunction) (clsMethods cls)
    declTypes (DeclStruct st) =
      [t | (_, Just t) <- stFields st] ++ concatMap (declTypes . DeclFunction) (stMethods st)
    declTypes (DeclInterface iface) =
      concatMap (declTypes . DeclFunction) (ifMethods iface)
    declTypes (DeclReceiver _ fn) =
      declTypes (DeclFunction fn)
    declTypes (DeclTrait tr) =
      concatMap (declTypes . DeclFunction) (trMethods tr)
    declTypes (DeclImpl imp) =
      concatMap (declTypes . DeclFunction) (impMethods imp)
    declTypes (DeclVariable _ mTy) = maybe [] pure mTy
    declTypes (DeclTypeAlias _ mTy) = maybe [] pure mTy

collectValRefNames :: Program -> Set Text
collectValRefNames (Program modules _) =
  Set.fromList $ concatMap modValRefs modules
  where
    modValRefs (Module _ _ decls stmts) =
      concatMap declValRefs decls ++ concatMap stmtValRefs stmts

    declValRefs (DeclFunction fn) = concatMap stmtValRefs (fnBody fn)
    declValRefs (DeclClass cls) = concatMap (concatMap stmtValRefs . fnBody) (clsMethods cls)
    declValRefs (DeclStruct st) = concatMap (concatMap stmtValRefs . fnBody) (stMethods st)
    declValRefs (DeclTrait tr) = concatMap (concatMap stmtValRefs . fnBody) (trMethods tr)
    declValRefs (DeclImpl imp) = concatMap (concatMap stmtValRefs . fnBody) (impMethods imp)
    declValRefs (DeclReceiver _ fn) = concatMap stmtValRefs (fnBody fn)
    declValRefs (DeclVariable _ _) = []
    declValRefs (DeclInterface _) = []
    declValRefs (DeclTypeAlias _ _) = []

    stmtValRefs stmt = case stmt of
      StmtAssign targets val -> concatMap exprValRefs (val : targets)
      StmtExpr e -> exprValRefs e
      _ -> []

    exprValRefs expr = case expr of
      ExprId name -> [name]
      ExprAttr (ExprId obj) _ -> [obj]
      _ -> []

collectIntraModuleDeps :: Program -> [(Text, Text)]
collectIntraModuleDeps (Program modules _) =
  concatMap modIntraDeps modules
  where
    modIntraDeps (Module _ _ decls _) =
      let topFnNames = Set.fromList [fnName fn | DeclFunction fn <- decls]
          topClsNames = Set.fromList [clsName cls | DeclClass cls <- decls]
          allTopNames = Set.union topFnNames topClsNames
      in [ (callerName, target)
         | DeclFunction fn <- decls
         , let callerName = fnName fn
         , target <- Set.toList (collectCallNames (Program [Module "" [] [DeclFunction fn] []] ""))
         , Set.member target allTopNames
         , target /= callerName
         ]
