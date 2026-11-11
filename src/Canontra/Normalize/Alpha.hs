{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Canontra.Normalize.Alpha
Description : Scope-aware bound lexical alpha-canonicalization engine.

Normalizes bound local variable names, loop induction variables, comprehension
targets, lambda parameters, exception bindings, and context-manager bindings
to canonical de Bruijn lexical indices (@v<depth>.<ordinal>) while preserving:
  - Module-level declarations (functions, classes, structs, interfaces, traits)
  - Public function parameter names (preserving keyword-argument & signature contracts)
  - Imported module and symbol bindings
  - Explicit global / nonlocal references
  - Object attribute / member names (obj.field)
-}
module Canontra.Normalize.Alpha
  ( AlphaEnv (..)
  , emptyAlphaEnv
  , canonicalizeAlphaProgram
  , canonicalizeAlphaModule
  , canonicalizeFunctionLocals
  , canonicalizeStmtAlpha
  , canonicalizeExprAlpha
  , isCanonicalBoundId
  ) where

import Data.Char (isDigit)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Canontra.IR.Declaration
  ( Class (..)
  , Declaration (..)
  , Function (..)
  , Impl (..)
  , Interface (..)
  , Parameter (..)
  , Receiver (..)
  , Struct (..)
  , Trait (..)
  )
import Canontra.IR.Dependency
  ( ImportDecl (..)
  , ImportTarget (..)
  )
import Canontra.IR.Expression
  ( CompFor (..)
  , Expr (..)
  , FStringPart (..)
  , MatchCase (..)
  , SelectCase (..)
  , Stmt (..)
  )
import Canontra.IR.Program
  ( Module (..)
  , Program (..)
  )

-- | Lexical scoping environment for de Bruijn alpha-canonicalization.
data AlphaEnv = AlphaEnv
  { aeDepth     :: !Int
  -- ^ Current lexical nesting depth (0 = module, 1 = function body, >=2 = nested blocks).
  , aeOrd       :: !Int
  -- ^ Monotonic binder ordinal within the current lexical depth.
  , aeBindings  :: !(Map Text Text)
  -- ^ Active mapping from source variable name to canonical @v<depth>.<ord>.
  , aeGlobals   :: !(Set Text)
  -- ^ Protected global, imported, parameter, and receiver symbols (never renamed).
  , aeDepthOrds :: !(Map Int Int)
  -- ^ Per-depth monotonic ordinal watermark within a function to prevent cross-block collisions.
  }
  deriving stock (Eq, Show)

-- | Construct an initial 'AlphaEnv' with the given protected global symbol set.
emptyAlphaEnv :: Set Text -> AlphaEnv
emptyAlphaEnv globals = AlphaEnv
  { aeDepth     = 1
  , aeOrd       = 0
  , aeBindings  = Map.empty
  , aeGlobals   = Set.union builtinProtected globals
  , aeDepthOrds = Map.empty
  }

builtinProtected :: Set Text
builtinProtected = Set.fromList
  [ "_", "self", "this", "cls", "super", "nil", "null", "undefined"
  , "true", "false", "True", "False", "None", "iota", "Ok", "Err", "Some"
  ]

-- | Check whether an identifier is already in canonical bound format (@v<depth>.<ord>).
isCanonicalBoundId :: Text -> Bool
isCanonicalBoundId ident =
  case T.stripPrefix "@v" ident of
    Nothing -> False
    Just rest ->
      let (dPart, afterDot) = T.span isDigit rest
      in not (T.null dPart)
         && case T.uncons afterDot of
              Just ('.', oPart) -> not (T.null oPart) && T.all isDigit oPart
              _                 -> False

-- | Apply scope-aware bound lexical alpha-canonicalization across an entire 'Program'.
canonicalizeAlphaProgram :: Program -> Program
canonicalizeAlphaProgram (Program modules lang) =
  Program (map canonicalizeAlphaModule modules) lang

-- | Apply scope-aware bound lexical alpha-canonicalization across a 'Module'.
-- Top-level module statements and module-level exported symbols are preserved;
-- bound local variables inside function and method bodies are canonicalized.
canonicalizeAlphaModule :: Module -> Module
canonicalizeAlphaModule (Module name imps decls stmts) =
  let modGlobals = collectModuleGlobals imps decls stmts
      decls' = map (canonicalizeDeclAlpha modGlobals) decls
  in Module name imps decls' stmts

collectModuleGlobals :: [ImportDecl] -> [Declaration] -> [Stmt] -> Set Text
collectModuleGlobals imps decls stmts =
  Set.unions
    [ builtinProtected
    , Set.fromList (concatMap extractImportSymbols imps)
    , Set.fromList (concatMap extractDeclSymbol decls)
    , Set.fromList (concatMap extractTopStmtSymbols stmts)
    ]

extractImportSymbols :: ImportDecl -> [Text]
extractImportSymbols = \case
  ImportModule modPath mAlias ->
    let base = lastDotSegment modPath
    in case mAlias of
         Just alias -> [alias, modPath, base]
         Nothing    -> [modPath, base]
  ImportFrom modPath target ->
    let base = lastDotSegment modPath
        syms = case target of
          ImportAll -> []
          ImportSymbols pairs ->
            concat [[s, maybe s id mA] | (s, mA) <- pairs]
    in modPath : base : syms
  where
    lastDotSegment txt =
      case reverse (T.splitOn "." (lastSlashSegment txt)) of
        (x:_) -> x
        []    -> txt
    lastSlashSegment txt =
      case reverse (T.splitOn "/" txt) of
        (x:_) -> x
        []    -> txt

extractDeclSymbol :: Declaration -> [Text]
extractDeclSymbol = \case
  DeclFunction fn     -> [fnName fn]
  DeclClass cls       -> clsName cls : map fnName (clsMethods cls)
  DeclStruct st       -> stName st : map fnName (stMethods st) ++ map fst (stFields st)
  DeclInterface iface -> ifName iface : map fnName (ifMethods iface)
  DeclReceiver rc fn  -> [rcTypeName rc, fnName fn]
  DeclTrait tr        -> trName tr : map fnName (trMethods tr)
  DeclImpl imp        -> impTarget imp : maybe [] pure (impTrait imp) ++ map fnName (impMethods imp)
  DeclVariable v _    -> [v]
  DeclTypeAlias n _   -> [n]

extractTopStmtSymbols :: Stmt -> [Text]
extractTopStmtSymbols = \case
  StmtAssign targets _     -> concatMap extractTargetNames targets
  StmtAnnAssign target _ _ -> extractTargetNames target
  _                        -> []

canonicalizeDeclAlpha :: Set Text -> Declaration -> Declaration
canonicalizeDeclAlpha modGlobals = \case
  DeclFunction fn ->
    DeclFunction (canonicalizeFunctionLocals modGlobals fn)
  DeclClass cls ->
    let clsGlobals = Set.insert (clsName cls) modGlobals
    in DeclClass cls
         { clsMethods = map (canonicalizeFunctionLocals clsGlobals) (clsMethods cls)
         }
  DeclStruct st ->
    let stGlobals = Set.insert (stName st) modGlobals
    in DeclStruct st
         { stMethods = map (canonicalizeFunctionLocals stGlobals) (stMethods st)
         }
  DeclTrait tr ->
    let trGlobals = Set.insert (trName tr) modGlobals
    in DeclTrait tr
         { trMethods = map (canonicalizeFunctionLocals trGlobals) (trMethods tr)
         }
  DeclImpl imp ->
    let impGlobals = Set.insert (impTarget imp) modGlobals
    in DeclImpl imp
         { impMethods = map (canonicalizeFunctionLocals impGlobals) (impMethods imp)
         }
  DeclReceiver rc fn ->
    let rcGlobals = Set.fromList (filter (not . T.null) [rcVarName rc, rcTypeName rc])
                    `Set.union` modGlobals
    in DeclReceiver rc (canonicalizeFunctionLocals rcGlobals fn)
  other -> other

-- | Canonicalize all bound local variables inside a 'Function' declaration
-- while preserving its public signature (function name and parameter names).
canonicalizeFunctionLocals :: Set Text -> Function -> Function
canonicalizeFunctionLocals outerGlobals fn =
  let paramNames = concatMap extractParamNames (fnParams fn)
      explicitGlobals = collectExplicitGlobals (fnBody fn)
      fnGlobals = Set.unions
        [ outerGlobals
        , Set.singleton (fnName fn)
        , Set.fromList paramNames
        , explicitGlobals
        ]
      env0 = emptyAlphaEnv fnGlobals
      (body', _) = canonicalizeStmts env0 (fnBody fn)
  in fn { fnBody = body' }

extractParamNames :: Parameter -> [Text]
extractParamNames p =
  let raw = paramName p
      stripped = stripParamModifiers raw
  in filter (not . T.null) [raw, stripped]

stripParamModifiers :: Text -> Text
stripParamModifiers txt =
  let t1 = T.dropWhile (\c -> c == '*' || c == '&' || c == '.') (T.strip txt)
      t2 = case T.stripPrefix "mut " t1 of
             Just r  -> T.strip r
             Nothing -> t1
  in t2

collectExplicitGlobals :: [Stmt] -> Set Text
collectExplicitGlobals = foldMap stmtGlobals
  where
    stmtGlobals = \case
      StmtGlobal names         -> Set.fromList names
      StmtNonlocal names       -> Set.fromList names
      StmtIf _ t e             -> collectExplicitGlobals t `Set.union` collectExplicitGlobals e
      StmtWhile _ b e          -> collectExplicitGlobals b `Set.union` collectExplicitGlobals e
      StmtFor _ _ b e          -> collectExplicitGlobals b `Set.union` collectExplicitGlobals e
      StmtAsyncFor _ _ b e     -> collectExplicitGlobals b `Set.union` collectExplicitGlobals e
      StmtTry b hs e f         ->
        Set.unions
          ( collectExplicitGlobals b
          : collectExplicitGlobals e
          : collectExplicitGlobals f
          : [collectExplicitGlobals hb | (_, _, hb) <- hs]
          )
      StmtWith _ b             -> collectExplicitGlobals b
      StmtAsyncWith _ b        -> collectExplicitGlobals b
      StmtMatch _ cs           -> foldMap (collectExplicitGlobals . mcBody) cs
      StmtSwitch _ cs d        -> foldMap (collectExplicitGlobals . snd) cs `Set.union` collectExplicitGlobals d
      StmtSelect cs            -> foldMap (collectExplicitGlobals . snd) cs
      StmtLoop b               -> collectExplicitGlobals b
      _                        -> Set.empty

-- ---------------------------------------------------------------------------
-- Lexical Scope & Binder Helpers
-- ---------------------------------------------------------------------------

enterDepth :: AlphaEnv -> AlphaEnv
enterDepth env =
  let d' = aeDepth env + 1
      o' = Map.findWithDefault 0 d' (aeDepthOrds env)
  in env { aeDepth = d', aeOrd = o' }

restoreDepth :: Int -> AlphaEnv -> AlphaEnv
restoreDepth d env =
  let o = Map.findWithDefault 0 d (aeDepthOrds env)
  in env { aeDepth = d, aeOrd = o }

-- | Bind a local variable in the current scope if not already bound or protected.
bindLocal :: Text -> AlphaEnv -> (Text, AlphaEnv)
bindLocal ident env
  | T.null ident || Set.member ident (aeGlobals env) = (ident, env)
  | Just existing <- Map.lookup ident (aeBindings env) = (existing, env)
  | otherwise = allocateFresh ident env

-- | Force-allocate a fresh binder at the current lexical depth (used for loop
-- targets, comprehension induction variables, lambda parameters, and exception aliases)
-- unless the identifier is protected in 'aeGlobals'.
bindFreshLocal :: Text -> AlphaEnv -> (Text, AlphaEnv)
bindFreshLocal ident env
  | T.null ident || Set.member ident (aeGlobals env) = (ident, env)
  | otherwise = allocateFresh ident env

allocateFresh :: Text -> AlphaEnv -> (Text, AlphaEnv)
allocateFresh ident env =
  let d = aeDepth env
      o = aeOrd env
      canon = "@v" <> T.pack (show d) <> "." <> T.pack (show o)
      nextOrd = o + 1
      env' = env
        { aeOrd       = nextOrd
        , aeBindings  = Map.insert ident canon (aeBindings env)
        , aeDepthOrds = Map.insert d nextOrd (aeDepthOrds env)
        }
  in (canon, env')

resolveRead :: AlphaEnv -> Text -> Text
resolveRead env ident =
  case Map.lookup ident (aeBindings env) of
    Just canon -> canon
    Nothing    -> ident

extractTargetNames :: Expr -> [Text]
extractTargetNames = \case
  ExprId name     -> [name]
  ExprTuple es    -> concatMap extractTargetNames es
  ExprList es     -> concatMap extractTargetNames es
  ExprStarred e   -> extractTargetNames e
  ExprWalrus v _  -> [v]
  _               -> []

bindTargetExpr :: Bool -> AlphaEnv -> Expr -> (Expr, AlphaEnv)
bindTargetExpr forceFresh env = \case
  ExprId name ->
    let (name', env') =
          if forceFresh
            then bindFreshLocal name env
            else bindLocal name env
    in (ExprId name', env')
  ExprTuple es ->
    let (es', env') = mapAccumAlpha (bindTargetExpr forceFresh) env es
    in (ExprTuple es', env')
  ExprList es ->
    let (es', env') = mapAccumAlpha (bindTargetExpr forceFresh) env es
    in (ExprList es', env')
  ExprStarred e ->
    let (e', env') = bindTargetExpr forceFresh env e
    in (ExprStarred e', env')
  ExprAttr obj attr ->
    let (obj', env') = canonicalizeExprWithEnv env obj
    in (ExprAttr obj' attr, env')
  ExprSubscript obj idx ->
    let (obj', env1) = canonicalizeExprWithEnv env obj
        (idx', env2) = canonicalizeExprWithEnv env1 idx
    in (ExprSubscript obj' idx', env2)
  other ->
    canonicalizeExprWithEnv env other

mapAccumAlpha :: (AlphaEnv -> a -> (b, AlphaEnv)) -> AlphaEnv -> [a] -> ([b], AlphaEnv)
mapAccumAlpha _ env [] = ([], env)
mapAccumAlpha f env (x:xs) =
  let (y, env1) = f env x
      (ys, env2) = mapAccumAlpha f env1 xs
  in (y : ys, env2)

-- ---------------------------------------------------------------------------
-- Statement Alpha-Canonicalization
-- ---------------------------------------------------------------------------

-- | Canonicalize a single 'Stmt' under the given 'AlphaEnv'.
canonicalizeStmtAlpha :: AlphaEnv -> Stmt -> (Stmt, AlphaEnv)
canonicalizeStmtAlpha env stmt =
  let d0 = aeDepth env
  in case stmt of
    StmtAssign targets val ->
      let (val', env1) = canonicalizeExprWithEnv env val
          (targets', env2) = mapAccumAlpha (bindTargetExpr False) env1 targets
      in (StmtAssign targets' val', env2)

    StmtAugAssign target op val ->
      let (val', env1) = canonicalizeExprWithEnv env val
          (target', env2) = bindTargetExpr False env1 target
      in (StmtAugAssign target' op val', env2)

    StmtAnnAssign target ann mVal ->
      let (mVal', env1) = canonicalizeMaybeExpr env mVal
          (target', env2) = bindTargetExpr False env1 target
      in (StmtAnnAssign target' ann mVal', env2)

    StmtReturn mExpr ->
      let (mExpr', env1) = canonicalizeMaybeExpr env mExpr
      in (StmtReturn mExpr', env1)

    StmtExpr expr ->
      let (expr', env1) = canonicalizeExprWithEnv env expr
      in (StmtExpr expr', env1)

    StmtIf cond thenB elseB ->
      let (cond', env1) = canonicalizeExprWithEnv env cond
          (thenB', env2) = canonicalizeStmts (enterDepth env1) thenB
          (elseB', env3) = canonicalizeStmts env2 elseB
      in (StmtIf cond' thenB' elseB', restoreDepth d0 env3)

    StmtWhile cond body elseB ->
      let (cond', env1) = canonicalizeExprWithEnv env cond
          (body', env2) = canonicalizeStmts (enterDepth env1) body
          (elseB', env3) = canonicalizeStmts env2 elseB
      in (StmtWhile cond' body' elseB', restoreDepth d0 env3)

    StmtFor target iter body elseB ->
      let (iter', env1) = canonicalizeExprWithEnv env iter
          envLoop0 = enterDepth env1
          (target', envLoop1) = bindTargetExpr True envLoop0 target
          (body', envLoop2) = canonicalizeStmts envLoop1 body
          (elseB', envLoop3) = canonicalizeStmts envLoop2 elseB
      in (StmtFor target' iter' body' elseB', restoreDepth d0 envLoop3)

    StmtAsyncFor target iter body elseB ->
      let (iter', env1) = canonicalizeExprWithEnv env iter
          envLoop0 = enterDepth env1
          (target', envLoop1) = bindTargetExpr True envLoop0 target
          (body', envLoop2) = canonicalizeStmts envLoop1 body
          (elseB', envLoop3) = canonicalizeStmts envLoop2 elseB
      in (StmtAsyncFor target' iter' body' elseB', restoreDepth d0 envLoop3)

    StmtWith items body ->
      let envW0 = enterDepth env
          (items', envW1) = mapAccumAlpha canonicalizeWithItem envW0 items
          (body', envW2) = canonicalizeStmts envW1 body
      in (StmtWith items' body', restoreDepth d0 envW2)

    StmtAsyncWith items body ->
      let envW0 = enterDepth env
          (items', envW1) = mapAccumAlpha canonicalizeWithItem envW0 items
          (body', envW2) = canonicalizeStmts envW1 body
      in (StmtAsyncWith items' body', restoreDepth d0 envW2)

    StmtTry tryB handlers elseB finB ->
      let (tryB', env1) = canonicalizeStmts (enterDepth env) tryB
          (handlers', env2) = mapAccumAlpha canonicalizeHandler env1 handlers
          (elseB', env3) = canonicalizeStmts env2 elseB
          (finB', env4) = canonicalizeStmts env3 finB
      in (StmtTry tryB' handlers' elseB' finB', restoreDepth d0 env4)

    StmtRaise mExc mCause ->
      let (mExc', env1) = canonicalizeMaybeExpr env mExc
          (mCause', env2) = canonicalizeMaybeExpr env1 mCause
      in (StmtRaise mExc' mCause', env2)

    StmtAssert test mMsg ->
      let (test', env1) = canonicalizeExprWithEnv env test
          (mMsg', env2) = canonicalizeMaybeExpr env1 mMsg
      in (StmtAssert test' mMsg', env2)

    StmtDelete targets ->
      let (targets', env1) = mapAccumAlpha canonicalizeExprWithEnv env targets
      in (StmtDelete targets', env1)

    StmtMatch subject cases ->
      let (subject', env1) = canonicalizeExprWithEnv env subject
          (cases', env2) = mapAccumAlpha canonicalizeMatchCase (enterDepth env1) cases
      in (StmtMatch subject' cases', restoreDepth d0 env2)

    StmtSwitch subject cases defStmts ->
      let (subject', env1) = canonicalizeExprWithEnv env subject
          envS0 = enterDepth env1
          (cases', envS1) = mapAccumAlpha canonicalizeSwitchCase envS0 cases
          (defStmts', envS2) = canonicalizeStmts envS1 defStmts
      in (StmtSwitch subject' cases' defStmts', restoreDepth d0 envS2)

    StmtSelect clauses ->
      let (clauses', env1) = mapAccumAlpha canonicalizeSelectClause (enterDepth env) clauses
      in (StmtSelect clauses', restoreDepth d0 env1)

    StmtChanSend ch val ->
      let (ch', env1) = canonicalizeExprWithEnv env ch
          (val', env2) = canonicalizeExprWithEnv env1 val
      in (StmtChanSend ch' val', env2)

    StmtGo expr ->
      let (expr', env1) = canonicalizeExprWithEnv env expr
      in (StmtGo expr', env1)

    StmtDefer expr ->
      let (expr', env1) = canonicalizeExprWithEnv env expr
      in (StmtDefer expr', env1)

    StmtLoop body ->
      let (body', env1) = canonicalizeStmts (enterDepth env) body
      in (StmtLoop body', restoreDepth d0 env1)

    other -> (other, env)

canonicalizeStmts :: AlphaEnv -> [Stmt] -> ([Stmt], AlphaEnv)
canonicalizeStmts = mapAccumAlpha canonicalizeStmtAlpha

canonicalizeMaybeExpr :: AlphaEnv -> Maybe Expr -> (Maybe Expr, AlphaEnv)
canonicalizeMaybeExpr env Nothing = (Nothing, env)
canonicalizeMaybeExpr env (Just e) =
  let (e', env') = canonicalizeExprWithEnv env e
  in (Just e', env')

canonicalizeWithItem :: AlphaEnv -> (Expr, Maybe Expr) -> ((Expr, Maybe Expr), AlphaEnv)
canonicalizeWithItem env (ctxExpr, mTarget) =
  let (ctxExpr', env1) = canonicalizeExprWithEnv env ctxExpr
  in case mTarget of
       Nothing -> ((ctxExpr', Nothing), env1)
       Just tgt ->
         let (tgt', env2) = bindTargetExpr False env1 tgt
         in ((ctxExpr', Just tgt'), env2)

canonicalizeHandler
  :: AlphaEnv
  -> (Maybe Expr, Maybe Text, [Stmt])
  -> ((Maybe Expr, Maybe Text, [Stmt]), AlphaEnv)
canonicalizeHandler env (mExcType, mAlias, body) =
  let savedBindings = aeBindings env
      (mAlias', env1) = case mAlias of
        Nothing -> (Nothing, env)
        Just aliasName ->
          let (canonAlias, e') = bindFreshLocal aliasName env
          in (Just canonAlias, e')
      (body', env2) = canonicalizeStmts env1 body
      restoredBindings = case mAlias of
        Nothing -> aeBindings env2
        Just aliasName ->
          case Map.lookup aliasName savedBindings of
            Just oldCanon -> Map.insert aliasName oldCanon (aeBindings env2)
            Nothing       -> Map.delete aliasName (aeBindings env2)
  in ((mExcType, mAlias', body'), env2 { aeBindings = restoredBindings })

canonicalizeMatchCase :: AlphaEnv -> MatchCase -> (MatchCase, AlphaEnv)
canonicalizeMatchCase env (MatchCase pat mGuard body) =
  let (pat', env1) = canonicalizePattern env pat
      (mGuard', env2) = canonicalizeMaybeExpr env1 mGuard
      (body', env3) = canonicalizeStmts env2 body
  in (MatchCase pat' mGuard' body', env3)

canonicalizePattern :: AlphaEnv -> Expr -> (Expr, AlphaEnv)
canonicalizePattern env = \case
  ExprId name
    | name == "_" || Set.member name (aeGlobals env) -> (ExprId name, env)
    | isConstructorName name -> (ExprId name, env)
    | otherwise ->
        let (name', env') = bindLocal name env
        in (ExprId name', env')
  ExprTuple es ->
    let (es', env') = mapAccumAlpha canonicalizePattern env es
    in (ExprTuple es', env')
  ExprList es ->
    let (es', env') = mapAccumAlpha canonicalizePattern env es
    in (ExprList es', env')
  ExprStarred e ->
    let (e', env') = canonicalizePattern env e
    in (ExprStarred e', env')
  ExprCall ctor args kwArgs ->
    let (args', env1) = mapAccumAlpha canonicalizePattern env args
        (kwArgs', env2) =
          mapAccumAlpha
            (\e (k, v) -> let (v', e') = canonicalizePattern e v in ((k, v'), e'))
            env1
            kwArgs
    in (ExprCall ctor args' kwArgs', env2)
  ExprDict pairs ->
    let (pairs', env1) =
          mapAccumAlpha
            (\e (k, v) -> let (v', e') = canonicalizePattern e v in ((k, v'), e'))
            env
            pairs
    in (ExprDict pairs', env1)
  other -> (other, env)
  where
    isConstructorName txt =
      case T.uncons txt of
        Just (c, _) -> c >= 'A' && c <= 'Z'
        Nothing     -> False

canonicalizeSwitchCase :: AlphaEnv -> (Expr, [Stmt]) -> ((Expr, [Stmt]), AlphaEnv)
canonicalizeSwitchCase env (caseExpr, stmts) =
  let (caseExpr', env1) = canonicalizeExprWithEnv env caseExpr
      (stmts', env2) = canonicalizeStmts env1 stmts
  in ((caseExpr', stmts'), env2)

canonicalizeSelectClause :: AlphaEnv -> (SelectCase, [Stmt]) -> ((SelectCase, [Stmt]), AlphaEnv)
canonicalizeSelectClause env (comm, body) =
  let (comm', env1) = case comm of
        SelectSend chExpr valExpr ->
          let (chExpr', e1) = canonicalizeExprWithEnv env chExpr
              (valExpr', e2) = canonicalizeExprWithEnv e1 valExpr
          in (SelectSend chExpr' valExpr', e2)
        SelectRecv mVar chExpr ->
          let (chExpr', e1) = canonicalizeExprWithEnv env chExpr
          in case mVar of
               Nothing -> (SelectRecv Nothing chExpr', e1)
               Just vName ->
                 let (vName', e2) = bindLocal vName e1
                 in (SelectRecv (Just vName') chExpr', e2)
        SelectDefault -> (SelectDefault, env)
      (body', env2) = canonicalizeStmts env1 body
  in ((comm', body'), env2)

-- ---------------------------------------------------------------------------
-- Expression Alpha-Canonicalization
-- ---------------------------------------------------------------------------

-- | Canonicalize an 'Expr' under the given 'AlphaEnv'.
canonicalizeExprAlpha :: AlphaEnv -> Expr -> Expr
canonicalizeExprAlpha env expr = fst (canonicalizeExprWithEnv env expr)

canonicalizeExprWithEnv :: AlphaEnv -> Expr -> (Expr, AlphaEnv)
canonicalizeExprWithEnv env = \case
  ExprLit lit -> (ExprLit lit, env)

  ExprId ident -> (ExprId (resolveRead env ident), env)

  ExprBinary op lhs rhs ->
    let (lhs', env1) = canonicalizeExprWithEnv env lhs
        (rhs', env2) = canonicalizeExprWithEnv env1 rhs
    in (ExprBinary op lhs' rhs', env2)

  ExprUnary op operand ->
    let (operand', env1) = canonicalizeExprWithEnv env operand
    in (ExprUnary op operand', env1)

  ExprCall callee args kwArgs ->
    let (callee', env1) = canonicalizeExprWithEnv env callee
        (args', env2) = mapAccumAlpha canonicalizeExprWithEnv env1 args
        (kwArgs', env3) =
          mapAccumAlpha
            (\e (k, v) -> let (v', e') = canonicalizeExprWithEnv e v in ((k, v'), e'))
            env2
            kwArgs
    in (ExprCall callee' args' kwArgs', env3)

  ExprLambda params body ->
    let savedBindings = aeBindings env
        savedDepth = aeDepth env
        envL0 = enterDepth env
        (params', envL1) = mapAccumAlpha bindLambdaParam envL0 params
        (body', envL2) = canonicalizeExprWithEnv envL1 body
        envAfter = restoreDepth savedDepth (envL2 { aeBindings = savedBindings })
    in (ExprLambda params' body', envAfter)

  ExprTernary cond thenE elseE ->
    let (cond', env1) = canonicalizeExprWithEnv env cond
        (thenE', env2) = canonicalizeExprWithEnv env1 thenE
        (elseE', env3) = canonicalizeExprWithEnv env2 elseE
    in (ExprTernary cond' thenE' elseE', env3)

  ExprList items ->
    let (items', env1) = mapAccumAlpha canonicalizeExprWithEnv env items
    in (ExprList items', env1)

  ExprTuple items ->
    let (items', env1) = mapAccumAlpha canonicalizeExprWithEnv env items
    in (ExprTuple items', env1)

  ExprSet items ->
    let (items', env1) = mapAccumAlpha canonicalizeExprWithEnv env items
    in (ExprSet items', env1)

  ExprDict pairs ->
    let (pairs', env1) =
          mapAccumAlpha
            (\e (k, v) ->
              let (k', e1) = canonicalizeExprWithEnv e k
                  (v', e2) = canonicalizeExprWithEnv e1 v
              in ((k', v'), e2))
            env
            pairs
    in (ExprDict pairs', env1)

  ExprListComp elt comps ->
    let (elt', comps', envAfter) = canonicalizeComprehension env [elt] comps
    in case elt' of
         (e:_) -> (ExprListComp e comps', envAfter)
         []    -> (ExprListComp elt comps', envAfter)

  ExprSetComp elt comps ->
    let (elt', comps', envAfter) = canonicalizeComprehension env [elt] comps
    in case elt' of
         (e:_) -> (ExprSetComp e comps', envAfter)
         []    -> (ExprSetComp elt comps', envAfter)

  ExprGenerator elt comps ->
    let (elt', comps', envAfter) = canonicalizeComprehension env [elt] comps
    in case elt' of
         (e:_) -> (ExprGenerator e comps', envAfter)
         []    -> (ExprGenerator elt comps', envAfter)

  ExprDictComp kExpr vExpr comps ->
    let (elts', comps', envAfter) = canonicalizeComprehension env [kExpr, vExpr] comps
    in case elts' of
         [k', v'] -> (ExprDictComp k' v' comps', envAfter)
         _        -> (ExprDictComp kExpr vExpr comps', envAfter)

  ExprAttr obj attr ->
    let (obj', env1) = canonicalizeExprWithEnv env obj
    in (ExprAttr obj' attr, env1)

  ExprOptChain obj attr ->
    let (obj', env1) = canonicalizeExprWithEnv env obj
    in (ExprOptChain obj' attr, env1)

  ExprNullish lhs rhs ->
    let (lhs', env1) = canonicalizeExprWithEnv env lhs
        (rhs', env2) = canonicalizeExprWithEnv env1 rhs
    in (ExprNullish lhs' rhs', env2)

  ExprSubscript obj idx ->
    let (obj', env1) = canonicalizeExprWithEnv env obj
        (idx', env2) = canonicalizeExprWithEnv env1 idx
    in (ExprSubscript obj' idx', env2)

  ExprSlice mLower mUpper mStep ->
    let (mLower', env1) = canonicalizeMaybeExpr env mLower
        (mUpper', env2) = canonicalizeMaybeExpr env1 mUpper
        (mStep', env3) = canonicalizeMaybeExpr env2 mStep
    in (ExprSlice mLower' mUpper' mStep', env3)

  ExprAwait operand ->
    let (operand', env1) = canonicalizeExprWithEnv env operand
    in (ExprAwait operand', env1)

  ExprYield mOperand ->
    let (mOperand', env1) = canonicalizeMaybeExpr env mOperand
    in (ExprYield mOperand', env1)

  ExprYieldFrom operand ->
    let (operand', env1) = canonicalizeExprWithEnv env operand
    in (ExprYieldFrom operand', env1)

  ExprStarred operand ->
    let (operand', env1) = canonicalizeExprWithEnv env operand
    in (ExprStarred operand', env1)

  ExprKwStarred operand ->
    let (operand', env1) = canonicalizeExprWithEnv env operand
    in (ExprKwStarred operand', env1)

  ExprWalrus name val ->
    -- PEP 572: Walrus target binds in the enclosing function scope (depth 1).
    let (val', env1) = canonicalizeExprWithEnv env val
        savedDepth = aeDepth env1
        envAtFn = restoreDepth 1 env1
        (name', envAtFn') = bindLocal name envAtFn
        env2 = restoreDepth savedDepth envAtFn'
    in (ExprWalrus name' val', env2)

  ExprFormattedString parts ->
    let (parts', env1) = mapAccumAlpha canonicalizeFStringPart env parts
    in (ExprFormattedString parts', env1)

  ExprChanRecv ch ->
    let (ch', env1) = canonicalizeExprWithEnv env ch
    in (ExprChanRecv ch', env1)

  ExprTryOp operand ->
    let (operand', env1) = canonicalizeExprWithEnv env operand
    in (ExprTryOp operand', env1)

  ExprJSX tag attrs children ->
    let (attrs', env1) =
          mapAccumAlpha
            (\e (k, v) ->
              let (v', e') = canonicalizeExprWithEnv e v
              in ((k, v'), e'))
            env
            attrs
        (children', env2) = mapAccumAlpha canonicalizeExprWithEnv env1 children
    in (ExprJSX tag attrs' children', env2)

  ExprMacroCall name args ->
    let (args', env1) = mapAccumAlpha canonicalizeExprWithEnv env args
    in (ExprMacroCall name args', env1)

canonicalizeFStringPart :: AlphaEnv -> FStringPart -> (FStringPart, AlphaEnv)
canonicalizeFStringPart env = \case
  FStringText txt -> (FStringText txt, env)
  FStringExpr expr mConv mSpec ->
    let (expr', env1) = canonicalizeExprWithEnv env expr
    in (FStringExpr expr' mConv mSpec, env1)

bindLambdaParam :: AlphaEnv -> Parameter -> (Parameter, AlphaEnv)
bindLambdaParam env p =
  let (name', env1) = bindFreshLocal (paramName p) env
  in (p { paramName = name' }, env1)

-- | Canonicalize comprehension clauses, scoping induction variables locally to
-- the comprehension while preserving PEP 572 walrus assignments in the enclosing
-- function scope.
canonicalizeComprehension
  :: AlphaEnv
  -> [Expr]
  -> [CompFor]
  -> ([Expr], [CompFor], AlphaEnv)
canonicalizeComprehension env elts comps =
  let savedBindings = aeBindings env
      savedDepth = aeDepth env
      envC0 = enterDepth env
      (comps', compTargets, envC1) = canonicalizeCompClauses envC0 Set.empty comps
      (elts', envC2) = mapAccumAlpha canonicalizeExprWithEnv envC1 elts
      finalBindings =
        Map.foldlWithKey'
          (\acc k v ->
            if Set.member k compTargets
              then acc
              else Map.insert k v acc)
          savedBindings
          (aeBindings envC2)
      envAfter = restoreDepth savedDepth (envC2 { aeBindings = finalBindings })
  in (elts', comps', envAfter)

canonicalizeCompClauses
  :: AlphaEnv
  -> Set Text
  -> [CompFor]
  -> ([CompFor], Set Text, AlphaEnv)
canonicalizeCompClauses env targetsAcc [] = ([], targetsAcc, env)
canonicalizeCompClauses env targetsAcc (CompFor tgt iter ifs : rest) =
  let (iter', env1) = canonicalizeExprWithEnv env iter
      tgtNames = Set.fromList (extractTargetNames tgt)
      (tgt', env2) = bindTargetExpr True env1 tgt
      (ifs', env3) = mapAccumAlpha canonicalizeExprWithEnv env2 ifs
      (rest', targetsAcc', env4) =
        canonicalizeCompClauses env3 (Set.union targetsAcc tgtNames) rest
  in (CompFor tgt' iter' ifs' : rest', targetsAcc', env4)
