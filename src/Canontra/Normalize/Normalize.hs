{- |
Module      : Canontra.Normalize.Normalize
Description : Polyglot AST normalization pass v3.

Normalizer v3 strips away multi-scope docstrings, comments, redundant passes,
and formatting trivia across Python, JavaScript, TypeScript, Go, and Rust
while strictly preserving semantic literals, arithmetic operator semantics,
and execution control flow.
-}
module Canontra.Normalize.Normalize
  ( normalizeProgram
  , normalizeModule
  , normalizeDeclaration
  , normalizeModuleDeclarations
  , isProvablyPureDeclaration
  , declIdentifier
  , normalizeFunction
  , normalizeStmt
  , normalizeExpr
  , isReflectionDocstring
  , preservesDocstrings
  , cleanStmtSuite
  , cleanStmtSuiteWithPreserve
  , stripLeadingDocstring
  ) where

import Data.List (partition, sort, sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T

import Canontra.IR.Declaration
import Canontra.IR.Dependency
import Canontra.IR.Expression
import Canontra.IR.Program

normalizeProgram :: Program -> Program
normalizeProgram (Program modules lang) =
  let normModules = map normalizeModule modules
  in Program (sortBy (comparing modName) normModules) lang

normalizeModule :: Module -> Module
normalizeModule (Module _ imps decls stmts) =
  let normImps = sort (map normalizeImport imps)
      normDecls = normalizeModuleDeclarations decls
      strippedStmts = stripLeadingDocstring stmts
      normStmts = cleanStmtSuite (map normalizeStmt strippedStmts)
  in Module "" normImps normDecls normStmts

-- | Check if a declaration is provably pure and free of module-level side effects.
isProvablyPureDeclaration :: Declaration -> Bool
isProvablyPureDeclaration = \case
  DeclFunction fn   -> null (fnDecorators fn) -- Simple functions without decorators are pure
  DeclInterface _   -> True
  DeclTypeAlias _ _ -> True
  DeclTrait _       -> True
  _                 -> False

-- | Unique canonical sorting key for declarations.
declIdentifier :: Declaration -> Text
declIdentifier = \case
  DeclFunction fn       -> "fn:" <> fnName fn
  DeclClass cls         -> "cls:" <> clsName cls
  DeclStruct st         -> "st:" <> stName st
  DeclVariable v _      -> "var:" <> v
  DeclInterface iface   -> "if:" <> ifName iface
  DeclTrait tr          -> "tr:" <> trName tr
  DeclImpl imp          -> "imp:" <> impTarget imp
  DeclReceiver rc fn    -> "rc:" <> rcTypeName rc <> "." <> fnName fn
  DeclTypeAlias alias _ -> "alias:" <> alias

-- | Normalize module declarations: pure declarations are canonically sorted;
-- stateful or decorated declarations preserve source execution order.
normalizeModuleDeclarations :: [Declaration] -> [Declaration]
normalizeModuleDeclarations decls =
  let (pureDecls, statefulDecls) = partition isProvablyPureDeclaration decls
      sortedPure = sortBy (comparing declIdentifier) (map normalizeDeclaration pureDecls)
      normStateful = map normalizeDeclaration statefulDecls
  in sortedPure ++ normStateful

normalizeImport :: ImportDecl -> ImportDecl
normalizeImport imp = case imp of
  ImportModule modName alias -> ImportModule modName alias
  ImportFrom modName (ImportSymbols syms) -> ImportFrom modName (ImportSymbols (sort syms))
  ImportFrom modName ImportAll -> ImportFrom modName ImportAll

normalizeDeclaration :: Declaration -> Declaration
normalizeDeclaration decl = case decl of
  DeclFunction fn -> DeclFunction (normalizeFunction fn)
  DeclClass (Class name bases methods decs) ->
    let classPreserve = preservesDocstrings decs
        normMethods = map (normalizeClassFunction classPreserve) methods
    in DeclClass (Class name bases normMethods (sort decs))
  DeclStruct (Struct name fields methods vis) ->
    let normFields = map (\(f, t) -> (f, fmap T.strip t)) fields
        normMethods = map normalizeFunction methods
    in DeclStruct (Struct name normFields normMethods vis)
  DeclInterface (Interface name methods bases) ->
    let normMethods = map normalizeFunction methods
    in DeclInterface (Interface name normMethods (sort bases))
  DeclReceiver rc fn ->
    DeclReceiver rc (normalizeFunction fn)
  DeclTrait (Trait name methods supers) ->
    let normMethods = map normalizeFunction methods
    in DeclTrait (Trait name normMethods (sort supers))
  DeclImpl (Impl mTr tgt methods) ->
    let normMethods = map normalizeFunction methods
    in DeclImpl (Impl mTr tgt normMethods)
  DeclVariable v mTy -> DeclVariable v (fmap T.strip mTy)
  DeclTypeAlias a mTy -> DeclTypeAlias a (fmap T.strip mTy)

-- | Check if a docstring is marked for runtime reflection preservation
isReflectionDocstring :: Text -> Bool
isReflectionDocstring doc =
  let stripped = T.strip doc
  in T.isInfixOf ":preserve:" stripped
     || T.isInfixOf "@preserve" stripped
     || T.isPrefixOf ":doc:" stripped
     || T.isInfixOf ":doc:" stripped

-- | Check if a declaration is marked for runtime reflection docstring preservation
preservesDocstrings :: [Text] -> Bool
preservesDocstrings decs =
  any (\d -> d `elem` ["@preserve_docstring", "@reflect", "@doc", "preserve_docstring", "reflect", "doc"]) decs

normalizeFunction :: Function -> Function
normalizeFunction = normalizeClassFunction False

normalizeClassFunction :: Bool -> Function -> Function
normalizeClassFunction classPreserve (Function name params retType decs body isAsync) =
  let normParams = map normalizeParam params
      normRetType = fmap T.strip retType
      normDecs = sort decs
      preserve = classPreserve || preservesDocstrings decs
      strippedBody = if preserve then body else stripLeadingDocstring body
      normBody = cleanStmtSuiteWithPreserve preserve (map normalizeStmt strippedBody)
  in Function name normParams normRetType normDecs normBody isAsync

normalizeParam :: Parameter -> Parameter
normalizeParam (Parameter name kind defVal mType) =
  Parameter name kind (fmap T.strip defVal) (fmap T.strip mType)

normalizeStmt :: Stmt -> Stmt
normalizeStmt stmt = case stmt of
  StmtAssign targets expr ->
    StmtAssign (map normalizeExpr targets) (normalizeExpr expr)
  StmtAnnAssign target ty maybeVal ->
    StmtAnnAssign (normalizeExpr target) (normalizeExpr ty) (fmap normalizeExpr maybeVal)
  StmtAugAssign target op expr ->
    StmtAugAssign (normalizeExpr target) op (normalizeExpr expr)
  StmtExpr expr ->
    StmtExpr (normalizeExpr expr)
  StmtReturn maybeExpr ->
    StmtReturn (fmap normalizeExpr maybeExpr)
  StmtIf cond body elseSuite ->
    StmtIf (normalizeExpr cond) (cleanStmtSuite (map normalizeStmt (stripLeadingDocstring body))) (cleanStmtSuite (map normalizeStmt (stripLeadingDocstring elseSuite)))
  StmtWhile cond body elseSuite ->
    StmtWhile (normalizeExpr cond) (cleanStmtSuite (map normalizeStmt (stripLeadingDocstring body))) (cleanStmtSuite (map normalizeStmt (stripLeadingDocstring elseSuite)))
  StmtFor target iter body elseSuite ->
    StmtFor (normalizeExpr target) (normalizeExpr iter) (cleanStmtSuite (map normalizeStmt (stripLeadingDocstring body))) (cleanStmtSuite (map normalizeStmt (stripLeadingDocstring elseSuite)))
  StmtAsyncFor target iter body elseSuite ->
    StmtAsyncFor (normalizeExpr target) (normalizeExpr iter) (cleanStmtSuite (map normalizeStmt (stripLeadingDocstring body))) (cleanStmtSuite (map normalizeStmt (stripLeadingDocstring elseSuite)))
  StmtTry body handlers elseSuite finalSuite ->
    let normHandlers = map (\(clause, alias, hBody) ->
                                (fmap normalizeExpr clause, alias, cleanStmtSuite (map normalizeStmt (stripLeadingDocstring hBody)))) handlers
    in StmtTry (cleanStmtSuite (map normalizeStmt (stripLeadingDocstring body))) normHandlers (cleanStmtSuite (map normalizeStmt (stripLeadingDocstring elseSuite))) (cleanStmtSuite (map normalizeStmt (stripLeadingDocstring finalSuite)))
  StmtWith items body ->
    StmtWith (map (\(e, a) -> (normalizeExpr e, fmap normalizeExpr a)) items) (cleanStmtSuite (map normalizeStmt (stripLeadingDocstring body)))
  StmtAsyncWith items body ->
    StmtAsyncWith (map (\(e, a) -> (normalizeExpr e, fmap normalizeExpr a)) items) (cleanStmtSuite (map normalizeStmt (stripLeadingDocstring body)))
  StmtAssert expr maybeMsg ->
    StmtAssert (normalizeExpr expr) (fmap normalizeExpr maybeMsg)
  StmtRaise maybeExpr maybeCause ->
    StmtRaise (fmap normalizeExpr maybeExpr) (fmap normalizeExpr maybeCause)
  StmtBreak -> StmtBreak
  StmtContinue -> StmtContinue
  StmtPass -> StmtPass
  StmtDelete exprs -> StmtDelete (map normalizeExpr exprs)
  StmtGlobal vars -> StmtGlobal (sort vars)
  StmtNonlocal vars -> StmtNonlocal (sort vars)
  StmtMatch expr cases ->
    StmtMatch (normalizeExpr expr) (map normalizeMatchCase cases)
  StmtGo expr ->
    StmtGo (normalizeExpr expr)
  StmtDefer expr ->
    StmtDefer (normalizeExpr expr)
  StmtChanSend ch val ->
    StmtChanSend (normalizeExpr ch) (normalizeExpr val)
  StmtSelect cases ->
    StmtSelect (map (\(sc, b) -> (normalizeSelectCase sc, cleanStmtSuite (map normalizeStmt (stripLeadingDocstring b)))) cases)
  StmtLoop body ->
    StmtLoop (cleanStmtSuite (map normalizeStmt (stripLeadingDocstring body)))
  StmtSwitch expr cases defStmts ->
    StmtSwitch (normalizeExpr expr)
               (map (\(c, b) -> (normalizeExpr c, cleanStmtSuite (map normalizeStmt (stripLeadingDocstring b)))) cases)
               (cleanStmtSuite (map normalizeStmt (stripLeadingDocstring defStmts)))

normalizeSelectCase :: SelectCase -> SelectCase
normalizeSelectCase = \case
  SelectSend ch val -> SelectSend (normalizeExpr ch) (normalizeExpr val)
  SelectRecv mV ch  -> SelectRecv mV (normalizeExpr ch)
  SelectDefault     -> SelectDefault

normalizeMatchCase :: MatchCase -> MatchCase
normalizeMatchCase (MatchCase pat guard body) =
  MatchCase (normalizeExpr pat) (fmap normalizeExpr guard) (cleanStmtSuite (map normalizeStmt (stripLeadingDocstring body)))

normalizeExpr :: Expr -> Expr
normalizeExpr expr = case expr of
  ExprId ident -> ExprId ident
  ExprLit lit -> ExprLit lit
  ExprBinary op e1 e2 -> ExprBinary op (normalizeExpr e1) (normalizeExpr e2)
  ExprUnary op e -> ExprUnary op (normalizeExpr e)
  ExprCall target args kwArgs ->
    let normArgs = map normalizeExpr args
        normKwArgs = sortBy (comparing fst) (map (\(k, v) -> (k, normalizeExpr v)) kwArgs)
    in ExprCall (normalizeExpr target) normArgs normKwArgs
  ExprAttr target attr -> ExprAttr (normalizeExpr target) attr
  ExprSubscript target idx -> ExprSubscript (normalizeExpr target) (normalizeExpr idx)
  ExprSlice ms me mst -> ExprSlice (fmap normalizeExpr ms) (fmap normalizeExpr me) (fmap normalizeExpr mst)
  ExprList items -> ExprList (map normalizeExpr items)
  ExprTuple items -> ExprTuple (map normalizeExpr items)
  ExprDict items -> ExprDict (map (\(k, v) -> (normalizeExpr k, normalizeExpr v)) items)
  ExprSet items -> ExprSet (map normalizeExpr items)
  ExprLambda params body -> ExprLambda (map normalizeParam params) (normalizeExpr body)
  ExprTernary cond trueExpr falseExpr -> ExprTernary (normalizeExpr cond) (normalizeExpr trueExpr) (normalizeExpr falseExpr)
  ExprListComp item comps -> ExprListComp (normalizeExpr item) (map normalizeComp comps)
  ExprDictComp k v comps -> ExprDictComp (normalizeExpr k) (normalizeExpr v) (map normalizeComp comps)
  ExprSetComp item comps -> ExprSetComp (normalizeExpr item) (map normalizeComp comps)
  ExprGenerator item comps -> ExprGenerator (normalizeExpr item) (map normalizeComp comps)
  ExprWalrus name val -> ExprWalrus name (normalizeExpr val)
  ExprAwait e -> ExprAwait (normalizeExpr e)
  ExprYield me -> ExprYield (fmap normalizeExpr me)
  ExprYieldFrom e -> ExprYieldFrom (normalizeExpr e)
  ExprFormattedString parts -> ExprFormattedString (map normalizeFStringPart parts)
  ExprStarred e -> ExprStarred (normalizeExpr e)
  ExprKwStarred e -> ExprKwStarred (normalizeExpr e)
  ExprOptChain e prop -> ExprOptChain (normalizeExpr e) prop
  ExprNullish e1 e2 -> ExprNullish (normalizeExpr e1) (normalizeExpr e2)
  ExprChanRecv ch -> ExprChanRecv (normalizeExpr ch)
  ExprTryOp e -> ExprTryOp (normalizeExpr e)
  ExprMacroCall name args -> ExprMacroCall name (map normalizeExpr args)
  ExprJSX tagElem attrs children ->
    let normAttrs = sortBy (comparing fst) (map (\(k, v) -> (k, normalizeExpr v)) attrs)
        normChildren = map normalizeExpr children
    in ExprJSX tagElem normAttrs normChildren

normalizeFStringPart :: FStringPart -> FStringPart
normalizeFStringPart = \case
  FStringText t -> FStringText t
  FStringExpr e conv fmt -> FStringExpr (normalizeExpr e) conv fmt

normalizeComp :: CompFor -> CompFor
normalizeComp (CompFor target iter ifs) = CompFor (normalizeExpr target) (normalizeExpr iter) (map normalizeExpr ifs)

-- | Strip leading string literal statement (docstring) from a statement list
stripLeadingDocstring :: [Stmt] -> [Stmt]
stripLeadingDocstring [] = []
stripLeadingDocstring (StmtExpr (ExprLit (LitString s)) : rest)
  | isReflectionDocstring s = StmtExpr (ExprLit (LitString s)) : rest
  | otherwise = rest
stripLeadingDocstring stmts = stmts

-- | Drops lone docstring expressions throughout block and eliminates redundant passes
cleanStmtSuite :: [Stmt] -> [Stmt]
cleanStmtSuite = cleanStmtSuiteWithPreserve False

cleanStmtSuiteWithPreserve :: Bool -> [Stmt] -> [Stmt]
cleanStmtSuiteWithPreserve preserve rawStmts =
  let nonDoc = if preserve then rawStmts else filter (not . isDocstringStmt) rawStmts
      elimPass = if length nonDoc > 1 then filter (not . isPass) nonDoc else nonDoc
  in if null elimPass then [StmtPass] else elimPass
  where
    isDocstringStmt (StmtExpr (ExprLit (LitString s))) = not (isReflectionDocstring s)
    isDocstringStmt _ = False

    isPass StmtPass = True
    isPass _ = False
