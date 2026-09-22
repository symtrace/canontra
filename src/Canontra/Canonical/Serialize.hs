{- |
Module      : Canontra.Canonical.Serialize
Description : Deterministic binary serialization for normalized IR, CFGs, and DFGs.

Canonicalization transforms abstract syntax into an unambiguous byte stream.
Every constructor, collection, primitive scalar, float (IEEE-754 normalized),
Unicode string (NFC precomposed), CFG basic block, and DFG Def-Use chain is encoded
with deterministic length prefixes and explicit tag bytes.
-}
module Canontra.Canonical.Serialize
  ( canonicalizeProgram
  , canonicalizeDeclarations
  , canonicalizeDependencies
  , canonicalizeRichDependencyGraph
  , canonicalizeCallGraph
  , canonicalizeCFGs
  , canonicalizeDFGs
  , canonicalizeWholeRepoCallGraph
  , canonicalizeWholeRepoDataFlow
  , canonicalizeModule
  , canonicalizeDeclaration
  , canonicalizeDeclarationStructural
  , canonicalizeStmt
  , canonicalizeExpr
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)

import Canontra.Analysis.CallGraph
import Canontra.Analysis.CFG
import Canontra.Analysis.DFG
import Canontra.Types
  ( DeclKind (..)
  , Fingerprint (..)
  , GlobalSymbol (..)
  , InterProceduralDataFlowEdge (..)
  , WholeRepoCallEdge (..)
  , WholeRepoCallGraph (..)
  , WholeRepoDataFlowGraph (..)
  )
import Canontra.Canonical.Float (encodeCanonicalFloat)
import Canontra.Canonical.Unicode (canonicalizeText)
import Canontra.IR.Declaration
import Canontra.IR.Dependency
import Canontra.IR.Expression
import Canontra.IR.Program

canonicalizeProgram :: Program -> BS.ByteString
canonicalizeProgram (Program modules lang) =
  LBS.toStrict $ BB.toLazyByteString $
    tag 0x01 <> encodeText lang <> encodeList canonicalizeModuleB modules

canonicalizeDeclarations :: [Declaration] -> BS.ByteString
canonicalizeDeclarations decls =
  LBS.toStrict $ BB.toLazyByteString $
    tag 0x02 <> encodeList canonicalizeDeclarationB decls

canonicalizeDependencies :: [ImportDecl] -> BS.ByteString
canonicalizeDependencies imps =
  LBS.toStrict $ BB.toLazyByteString $
    tag 0x03 <> encodeList canonicalizeImportB imps

canonicalizeRichDependencyGraph :: RichDependencyGraph -> BS.ByteString
canonicalizeRichDependencyGraph (RichDependencyGraph exts intras) =
  LBS.toStrict $ BB.toLazyByteString $
    tag 0x04 <>
    encodeList canonicalizeResolvedImportB exts <>
    encodeList (\(a, b) -> encodeText a <> encodeText b) intras

canonicalizeCallGraph :: CallGraph -> BS.ByteString
canonicalizeCallGraph (CallGraph nodes edges) =
  LBS.toStrict $ BB.toLazyByteString $
    tag 0x05 <>
    encodeList canonicalizeCallerNodeB nodes <>
    encodeList canonicalizeCallEdgeB edges

canonicalizeCFGs :: [ControlFlowGraph] -> BS.ByteString
canonicalizeCFGs cfgs =
  LBS.toStrict $ BB.toLazyByteString $
    tag 0x06 <> encodeList canonicalizeCFGB cfgs

canonicalizeDFGs :: [DataFlowGraph] -> BS.ByteString
canonicalizeDFGs dfgs =
  LBS.toStrict $ BB.toLazyByteString $
    tag 0x07 <> encodeList canonicalizeDFGB dfgs

canonicalizeWholeRepoCallGraph :: WholeRepoCallGraph -> BS.ByteString
canonicalizeWholeRepoCallGraph (WholeRepoCallGraph nodes edges sccs) =
  LBS.toStrict $ BB.toLazyByteString $
    tag 0x08 <>
    encodeList canonicalizeGlobalSymbolB nodes <>
    encodeList canonicalizeWholeRepoCallEdgeB edges <>
    encodeList (encodeList canonicalizeGlobalSymbolB) sccs

canonicalizeGlobalSymbolB :: GlobalSymbol -> BB.Builder
canonicalizeGlobalSymbolB (GlobalSymbol fp modName name kind (Fingerprint f2)) =
  tag 0x18 <>
  encodeText (T.pack fp) <>
  encodeText modName <>
  encodeText name <>
  encodeDeclKindB kind <>
  encodeText f2

encodeDeclKindB :: DeclKind -> BB.Builder
encodeDeclKindB = \case
  KindFunction  -> tag 0x01
  KindMethod    -> tag 0x02
  KindClass     -> tag 0x03
  KindStruct    -> tag 0x04
  KindInterface -> tag 0x05
  KindTrait     -> tag 0x06
  KindImpl      -> tag 0x07
  KindVariable  -> tag 0x08
  KindTypeAlias -> tag 0x09

canonicalizeWholeRepoCallEdgeB :: WholeRepoCallEdge -> BB.Builder
canonicalizeWholeRepoCallEdgeB (WholeRepoCallEdge caller callee cnt isAsync isCross) =
  tag 0x28 <>
  canonicalizeGlobalSymbolB caller <>
  canonicalizeGlobalSymbolB callee <>
  BB.int64BE (fromIntegral cnt) <>
  tag (if isAsync then 0x01 else 0x00) <>
  tag (if isCross then 0x01 else 0x00)

canonicalizeWholeRepoDataFlow :: WholeRepoDataFlowGraph -> BS.ByteString
canonicalizeWholeRepoDataFlow (WholeRepoDataFlowGraph nodes edges) =
  LBS.toStrict $ BB.toLazyByteString $
    tag 0x09 <>
    encodeList canonicalizeGlobalSymbolB nodes <>
    encodeList canonicalizeInterProceduralDataFlowEdgeB edges

canonicalizeInterProceduralDataFlowEdgeB :: InterProceduralDataFlowEdge -> BB.Builder
canonicalizeInterProceduralDataFlowEdgeB (InterProceduralDataFlowEdge src tgt pIdx vName isRet) =
  tag 0x38 <>
  canonicalizeGlobalSymbolB src <>
  canonicalizeGlobalSymbolB tgt <>
  BB.int64BE (fromIntegral pIdx) <>
  encodeText vName <>
  tag (if isRet then 0x01 else 0x00)


canonicalizeModule :: Module -> BS.ByteString
canonicalizeModule = LBS.toStrict . BB.toLazyByteString . canonicalizeModuleB

canonicalizeModuleB :: Module -> BB.Builder
canonicalizeModuleB (Module name imps decls stmts) =
  tag 0x10 <>
  encodeText name <>
  encodeList canonicalizeImportB imps <>
  encodeList canonicalizeDeclarationStructuralB decls <>
  encodeList canonicalizeStmtB stmts

canonicalizeImportB :: ImportDecl -> BB.Builder
canonicalizeImportB imp = case imp of
  ImportModule modName maybeAlias ->
    tag 0x20 <> encodeText modName <> encodeMaybe encodeText maybeAlias
  ImportFrom modName (ImportSymbols syms) ->
    tag 0x21 <> encodeText modName <> encodeList (\(s, a) -> encodeText s <> encodeMaybe encodeText a) syms
  ImportFrom modName ImportAll ->
    tag 0x22 <> encodeText modName

canonicalizeResolvedImportB :: ResolvedImport -> BB.Builder
canonicalizeResolvedImportB (ResolvedImport modName sym alias rel usage) =
  tag 0x25 <>
  encodeText modName <>
  encodeMaybe encodeText sym <>
  encodeMaybe encodeText alias <>
  BB.int64BE (fromIntegral rel) <>
  canonicalizeUsageB usage

canonicalizeUsageB :: DependencyUsage -> BB.Builder
canonicalizeUsageB = \case
  DepUnused           -> tag 0x26
  DepDirectCall syms  -> tag 0x27 <> encodeList encodeText syms
  DepInheritance syms -> tag 0x28 <> encodeList encodeText syms
  DepTypeOnly syms    -> tag 0x29 <> encodeList encodeText syms
  DepValueRef syms    -> tag 0x2A <> encodeList encodeText syms

canonicalizeCallerNodeB :: CallerNode -> BB.Builder
canonicalizeCallerNodeB = \case
  CallTopLevel        -> tag 0x01
  CallFunction fn     -> tag 0x02 <> encodeText fn
  CallMethod cls m    -> tag 0x03 <> encodeText cls <> encodeText m

canonicalizeCalleeTargetB :: CalleeTarget -> BB.Builder
canonicalizeCalleeTargetB = \case
  TargetLocal name       -> tag 0x01 <> encodeText name
  TargetMethod cls m     -> tag 0x02 <> encodeText cls <> encodeText m
  TargetImported mod' s  -> tag 0x03 <> encodeText mod' <> encodeText s
  TargetDynamic expr     -> tag 0x04 <> canonicalizeExprB expr

canonicalizeCallEdgeB :: CallEdge -> BB.Builder
canonicalizeCallEdgeB (CallEdge caller callee cnt isAsync) =
  canonicalizeCallerNodeB caller <>
  canonicalizeCalleeTargetB callee <>
  BB.int64BE (fromIntegral cnt) <>
  (if isAsync then tag 0x01 else tag 0x00)

-- | Serializer for Control-Flow Graphs
canonicalizeCFGB :: ControlFlowGraph -> BB.Builder
canonicalizeCFGB (ControlFlowGraph fn entry blocks edges) =
  encodeText fn <>
  BB.int64BE (fromIntegral entry) <>
  encodeList canonicalizeBasicBlockB blocks <>
  encodeList canonicalizeCFGEdgeB edges

canonicalizeBasicBlockB :: BasicBlock -> BB.Builder
canonicalizeBasicBlockB (BasicBlock bId stmts term) =
  BB.int64BE (fromIntegral bId) <>
  encodeList canonicalizeStmtB stmts <>
  canonicalizeTerminatorB term

canonicalizeTerminatorB :: BlockTerminator -> BB.Builder
canonicalizeTerminatorB = \case
  TermReturn me             -> tag 0x01 <> encodeMaybe canonicalizeExprB me
  TermBranch c t f          -> tag 0x02 <> canonicalizeExprB c <> BB.int64BE (fromIntegral t) <> BB.int64BE (fromIntegral f)
  TermJump j                -> tag 0x03 <> BB.int64BE (fromIntegral j)
  TermSwitch e cases mDef   -> tag 0x04 <> canonicalizeExprB e <> encodeList (\(p, b) -> canonicalizeExprB p <> BB.int64BE (fromIntegral b)) cases <> encodeMaybe (BB.int64BE . fromIntegral) mDef
  TermRaise me              -> tag 0x05 <> encodeMaybe canonicalizeExprB me
  TermExit                  -> tag 0x06

canonicalizeCFGEdgeB :: CFGEdge -> BB.Builder
canonicalizeCFGEdgeB (CFGEdge fromB toB cond) =
  BB.int64BE (fromIntegral fromB) <>
  BB.int64BE (fromIntegral toB) <>
  canonicalizeBranchCondB cond

canonicalizeBranchCondB :: BranchCondition -> BB.Builder
canonicalizeBranchCondB = \case
  CondTrue e        -> tag 0x01 <> canonicalizeExprB e
  CondFalse e       -> tag 0x02 <> canonicalizeExprB e
  CondCase e        -> tag 0x03 <> canonicalizeExprB e
  CondDefault       -> tag 0x04
  CondUnconditional -> tag 0x05
  CondException ex  -> tag 0x06 <> encodeText ex

-- | Serializer for Data-Flow Graphs
canonicalizeDFGB :: DataFlowGraph -> BB.Builder
canonicalizeDFGB (DataFlowGraph fn nodes edges) =
  encodeText fn <>
  encodeList canonicalizeDFGNodeB nodes <>
  encodeList canonicalizeDFGEdgeB edges

canonicalizeDFGNodeB :: DFGNode -> BB.Builder
canonicalizeDFGNodeB (DFGNode nId kind expr) =
  BB.int64BE (fromIntegral nId) <>
  canonicalizeDefUseKindB kind <>
  encodeMaybe canonicalizeExprB expr

canonicalizeDefUseKindB :: DefUseKind -> BB.Builder
canonicalizeDefUseKindB = \case
  DefParam idx      -> tag 0x01 <> BB.int64BE (fromIntegral idx)
  DefAssignment v   -> tag 0x02 <> encodeText v
  DefPhi nodeIds    -> tag 0x03 <> encodeList (BB.int64BE . fromIntegral) nodeIds
  UseRead v         -> tag 0x04 <> encodeText v
  UseArgument idx   -> tag 0x05 <> BB.int64BE (fromIntegral idx)
  UseBranchGuard    -> tag 0x06

canonicalizeDFGEdgeB :: DFGEdge -> BB.Builder
canonicalizeDFGEdgeB (DFGEdge src tgt var) =
  BB.int64BE (fromIntegral src) <>
  BB.int64BE (fromIntegral tgt) <>
  encodeText var

canonicalizeDeclaration :: Declaration -> BS.ByteString
canonicalizeDeclaration = LBS.toStrict . BB.toLazyByteString . canonicalizeDeclarationB

canonicalizeDeclarationStructural :: Declaration -> BS.ByteString
canonicalizeDeclarationStructural = LBS.toStrict . BB.toLazyByteString . canonicalizeDeclarationStructuralB

canonicalizeDeclarationB :: Declaration -> BB.Builder
canonicalizeDeclarationB decl = case decl of
  DeclFunction (Function name params retType decs _ isAsync) ->
    tag 0x30 <>
    encodeText name <>
    encodeList canonicalizeParamB params <>
    encodeMaybe encodeText retType <>
    encodeList encodeText decs <>
    (if isAsync then tag 0x01 else tag 0x00)
  DeclClass (Class name bases methods decs) ->
    tag 0x31 <>
    encodeText name <>
    encodeList encodeText bases <>
    encodeList canonicalizeDeclarationB [DeclFunction m | m <- methods] <>
    encodeList encodeText decs
  DeclStruct (Struct name fields methods vis) ->
    tag 0x32 <>
    encodeText name <>
    encodeList (\(f, t) -> encodeText f <> encodeMaybe encodeText t) fields <>
    encodeList canonicalizeDeclarationB [DeclFunction m | m <- methods] <>
    encodeText vis
  DeclInterface (Interface name methods bases) ->
    tag 0x33 <>
    encodeText name <>
    encodeList canonicalizeDeclarationB [DeclFunction m | m <- methods] <>
    encodeList encodeText bases
  DeclReceiver (Receiver var ty ptr) fn ->
    tag 0x34 <>
    encodeText var <> encodeText ty <> (if ptr then tag 0x01 else tag 0x00) <>
    canonicalizeDeclarationB (DeclFunction fn)
  DeclTrait (Trait name methods superTrs) ->
    tag 0x35 <>
    encodeText name <>
    encodeList canonicalizeDeclarationB [DeclFunction m | m <- methods] <>
    encodeList encodeText superTrs
  DeclImpl (Impl mTr tgt methods) ->
    tag 0x36 <>
    encodeMaybe encodeText mTr <>
    encodeText tgt <>
    encodeList canonicalizeDeclarationB [DeclFunction m | m <- methods]
  DeclVariable varName maybeType ->
    tag 0x37 <> encodeText varName <> encodeMaybe encodeText maybeType
  DeclTypeAlias aliasName origType ->
    tag 0x38 <> encodeText aliasName <> encodeMaybe encodeText origType

canonicalizeDeclarationStructuralB :: Declaration -> BB.Builder
canonicalizeDeclarationStructuralB decl = case decl of
  DeclFunction (Function name params retType decs body isAsync) ->
    tag 0x30 <>
    encodeText name <>
    encodeList canonicalizeParamB params <>
    encodeMaybe encodeText retType <>
    encodeList encodeText decs <>
    encodeList canonicalizeStmtB body <>
    (if isAsync then tag 0x01 else tag 0x00)
  DeclClass (Class name bases methods decs) ->
    tag 0x31 <>
    encodeText name <>
    encodeList encodeText bases <>
    encodeList canonicalizeDeclarationStructuralB [DeclFunction m | m <- methods] <>
    encodeList encodeText decs
  DeclStruct (Struct name fields methods vis) ->
    tag 0x32 <>
    encodeText name <>
    encodeList (\(f, t) -> encodeText f <> encodeMaybe encodeText t) fields <>
    encodeList canonicalizeDeclarationStructuralB [DeclFunction m | m <- methods] <>
    encodeText vis
  DeclInterface (Interface name methods bases) ->
    tag 0x33 <>
    encodeText name <>
    encodeList canonicalizeDeclarationStructuralB [DeclFunction m | m <- methods] <>
    encodeList encodeText bases
  DeclReceiver (Receiver var ty ptr) fn ->
    tag 0x34 <>
    encodeText var <> encodeText ty <> (if ptr then tag 0x01 else tag 0x00) <>
    canonicalizeDeclarationStructuralB (DeclFunction fn)
  DeclTrait (Trait name methods superTrs) ->
    tag 0x35 <>
    encodeText name <>
    encodeList canonicalizeDeclarationStructuralB [DeclFunction m | m <- methods] <>
    encodeList encodeText superTrs
  DeclImpl (Impl mTr tgt methods) ->
    tag 0x36 <>
    encodeMaybe encodeText mTr <>
    encodeText tgt <>
    encodeList canonicalizeDeclarationStructuralB [DeclFunction m | m <- methods]
  DeclVariable varName maybeType ->
    tag 0x37 <> encodeText varName <> encodeMaybe encodeText maybeType
  DeclTypeAlias aliasName origType ->
    tag 0x38 <> encodeText aliasName <> encodeMaybe encodeText origType

canonicalizeParamB :: Parameter -> BB.Builder
canonicalizeParamB (Parameter name kind defVal mType) =
  encodeText name <>
  tag (paramKindTag kind) <>
  encodeMaybe encodeText defVal <>
  encodeMaybe encodeText mType

paramKindTag :: ParamKind -> Word8
paramKindTag = \case
  ParamPositional     -> 0x01
  ParamKeywordOnly    -> 0x02
  ParamVarArgs        -> 0x03
  ParamKwArgs         -> 0x04
  ParamPositionalOnly -> 0x05

canonicalizeStmt :: Stmt -> BS.ByteString
canonicalizeStmt = LBS.toStrict . BB.toLazyByteString . canonicalizeStmtB

canonicalizeStmtB :: Stmt -> BB.Builder
canonicalizeStmtB stmt = case stmt of
  StmtAssign targets expr ->
    tag 0x40 <> encodeList canonicalizeExprB targets <> canonicalizeExprB expr
  StmtAugAssign target op expr ->
    tag 0x41 <> canonicalizeExprB target <> tag (opTag op) <> canonicalizeExprB expr
  StmtExpr expr ->
    tag 0x42 <> canonicalizeExprB expr
  StmtReturn maybeExpr ->
    tag 0x43 <> encodeMaybe canonicalizeExprB maybeExpr
  StmtIf cond body elseSuite ->
    tag 0x44 <> canonicalizeExprB cond <> encodeList canonicalizeStmtB body <> encodeList canonicalizeStmtB elseSuite
  StmtWhile cond body elseSuite ->
    tag 0x45 <> canonicalizeExprB cond <> encodeList canonicalizeStmtB body <> encodeList canonicalizeStmtB elseSuite
  StmtFor target iter body elseSuite ->
    tag 0x46 <> canonicalizeExprB target <> canonicalizeExprB iter <> encodeList canonicalizeStmtB body <> encodeList canonicalizeStmtB elseSuite
  StmtTry body handlers elseSuite finalSuite ->
    tag 0x47 <> encodeList canonicalizeStmtB body <>
    encodeList (\(c, a, b) -> encodeMaybe canonicalizeExprB c <> encodeMaybe encodeText a <> encodeList canonicalizeStmtB b) handlers <>
    encodeList canonicalizeStmtB elseSuite <> encodeList canonicalizeStmtB finalSuite
  StmtWith items body ->
    tag 0x48 <> encodeList (\(e, a) -> canonicalizeExprB e <> encodeMaybe canonicalizeExprB a) items <> encodeList canonicalizeStmtB body
  StmtAssert expr maybeMsg ->
    tag 0x49 <> canonicalizeExprB expr <> encodeMaybe canonicalizeExprB maybeMsg
  StmtRaise maybeExpr maybeCause ->
    tag 0x4A <> encodeMaybe canonicalizeExprB maybeExpr <> encodeMaybe canonicalizeExprB maybeCause
  StmtBreak -> tag 0x4B
  StmtContinue -> tag 0x4C
  StmtPass -> tag 0x4D
  StmtDelete exprs -> tag 0x4E <> encodeList canonicalizeExprB exprs
  StmtGlobal vars -> tag 0x4F <> encodeList encodeText vars
  StmtNonlocal vars -> tag 0x50 <> encodeList encodeText vars
  StmtAnnAssign target ty maybeVal ->
    tag 0x51 <> canonicalizeExprB target <> canonicalizeExprB ty <> encodeMaybe canonicalizeExprB maybeVal
  StmtAsyncFor target iter body elseSuite ->
    tag 0x52 <> canonicalizeExprB target <> canonicalizeExprB iter <> encodeList canonicalizeStmtB body <> encodeList canonicalizeStmtB elseSuite
  StmtAsyncWith items body ->
    tag 0x53 <> encodeList (\(e, a) -> canonicalizeExprB e <> encodeMaybe canonicalizeExprB a) items <> encodeList canonicalizeStmtB body
  StmtMatch expr cases ->
    tag 0x54 <> canonicalizeExprB expr <> encodeList canonicalizeMatchCaseB cases
  StmtGo expr ->
    tag 0x55 <> canonicalizeExprB expr
  StmtDefer expr ->
    tag 0x56 <> canonicalizeExprB expr
  StmtChanSend ch val ->
    tag 0x57 <> canonicalizeExprB ch <> canonicalizeExprB val
  StmtSelect cases ->
    tag 0x58 <> encodeList (\(sc, b) -> canonicalizeSelectCaseB sc <> encodeList canonicalizeStmtB b) cases
  StmtLoop body ->
    tag 0x59 <> encodeList canonicalizeStmtB body
  StmtSwitch expr cases defStmts ->
    tag 0x5A <> canonicalizeExprB expr <> encodeList (\(c, b) -> canonicalizeExprB c <> encodeList canonicalizeStmtB b) cases <> encodeList canonicalizeStmtB defStmts

canonicalizeSelectCaseB :: SelectCase -> BB.Builder
canonicalizeSelectCaseB = \case
  SelectSend ch val -> tag 0x01 <> canonicalizeExprB ch <> canonicalizeExprB val
  SelectRecv mV ch  -> tag 0x02 <> encodeMaybe encodeText mV <> canonicalizeExprB ch
  SelectDefault     -> tag 0x03

canonicalizeMatchCaseB :: MatchCase -> BB.Builder
canonicalizeMatchCaseB (MatchCase pat guard body) =
  canonicalizeExprB pat <> encodeMaybe canonicalizeExprB guard <> encodeList canonicalizeStmtB body

canonicalizeExpr :: Expr -> BS.ByteString
canonicalizeExpr = LBS.toStrict . BB.toLazyByteString . canonicalizeExprB

canonicalizeExprB :: Expr -> BB.Builder
canonicalizeExprB expr = case expr of
  ExprId ident -> tag 0x60 <> encodeText ident
  ExprLit lit -> tag 0x61 <> canonicalizeLitB lit
  ExprBinary op e1 e2 -> tag 0x62 <> tag (opTag op) <> canonicalizeExprB e1 <> canonicalizeExprB e2
  ExprUnary op e -> tag 0x63 <> tag (opTag op) <> canonicalizeExprB e
  ExprCall target args kwArgs ->
    tag 0x64 <> canonicalizeExprB target <> encodeList canonicalizeExprB args <> encodeList (\(k, v) -> encodeText k <> canonicalizeExprB v) kwArgs
  ExprAttr target attr -> tag 0x65 <> canonicalizeExprB target <> encodeText attr
  ExprSubscript target idx -> tag 0x66 <> canonicalizeExprB target <> canonicalizeExprB idx
  ExprList items -> tag 0x67 <> encodeList canonicalizeExprB items
  ExprTuple items -> tag 0x68 <> encodeList canonicalizeExprB items
  ExprDict items -> tag 0x69 <> encodeList (\(k, v) -> canonicalizeExprB k <> canonicalizeExprB v) items
  ExprSet items -> tag 0x6A <> encodeList canonicalizeExprB items
  ExprLambda params body -> tag 0x6B <> encodeList canonicalizeParamB params <> canonicalizeExprB body
  ExprTernary cond t f -> tag 0x6C <> canonicalizeExprB cond <> canonicalizeExprB t <> canonicalizeExprB f
  ExprListComp item comps -> tag 0x6D <> canonicalizeExprB item <> encodeList canonicalizeCompB comps
  ExprDictComp k v comps -> tag 0x6E <> canonicalizeExprB k <> canonicalizeExprB v <> encodeList canonicalizeCompB comps
  ExprGenerator item comps -> tag 0x6F <> canonicalizeExprB item <> encodeList canonicalizeCompB comps
  ExprSetComp item comps -> tag 0x80 <> canonicalizeExprB item <> encodeList canonicalizeCompB comps
  ExprWalrus name val -> tag 0x81 <> encodeText name <> canonicalizeExprB val
  ExprAwait e -> tag 0x82 <> canonicalizeExprB e
  ExprYield me -> tag 0x83 <> encodeMaybe canonicalizeExprB me
  ExprYieldFrom e -> tag 0x84 <> canonicalizeExprB e
  ExprFormattedString parts -> tag 0x85 <> encodeList canonicalizeFStringPartB parts
  ExprStarred e -> tag 0x86 <> canonicalizeExprB e
  ExprKwStarred e -> tag 0x87 <> canonicalizeExprB e
  ExprSlice ms me mst -> tag 0x88 <> encodeMaybe canonicalizeExprB ms <> encodeMaybe canonicalizeExprB me <> encodeMaybe canonicalizeExprB mst
  ExprOptChain e prop -> tag 0x89 <> canonicalizeExprB e <> encodeText prop
  ExprNullish e1 e2 -> tag 0x8A <> canonicalizeExprB e1 <> canonicalizeExprB e2
  ExprChanRecv ch -> tag 0x8B <> canonicalizeExprB ch
  ExprTryOp e -> tag 0x8C <> canonicalizeExprB e
  ExprMacroCall name args -> tag 0x8D <> encodeText name <> encodeList canonicalizeExprB args
  ExprJSX tagElem attrs children ->
    tag 0x8E <> encodeText tagElem <>
    encodeList (\(k, v) -> encodeText k <> canonicalizeExprB v) attrs <>
    encodeList canonicalizeExprB children

canonicalizeFStringPartB :: FStringPart -> BB.Builder
canonicalizeFStringPartB = \case
  FStringText t -> tag 0x01 <> encodeText t
  FStringExpr e conv fmt -> tag 0x02 <> canonicalizeExprB e <> encodeMaybe encodeText conv <> encodeMaybe encodeText fmt

canonicalizeLitB :: Lit -> BB.Builder
canonicalizeLitB lit = case lit of
  LitInt n     -> tag 0x70 <> BB.integerDec n
  LitFloat f   -> tag 0x71 <> BB.byteString (encodeCanonicalFloat f)
  LitString s  -> tag 0x72 <> encodeText s
  LitBytes b   -> tag 0x73 <> encodeText b
  LitBool True -> tag 0x74
  LitBool False-> tag 0x75
  LitNone      -> tag 0x76
  LitEllipsis  -> tag 0x77

canonicalizeCompB :: CompFor -> BB.Builder
canonicalizeCompB (CompFor target iter ifs) =
  canonicalizeExprB target <> canonicalizeExprB iter <> encodeList canonicalizeExprB ifs

opTag :: Op -> Word8
opTag = \case
  OpAdd -> 0x01; OpSub -> 0x02; OpMul -> 0x03; OpDiv -> 0x04; OpFloorDiv -> 0x05; OpMod -> 0x06; OpPow -> 0x07
  OpBitAnd -> 0x08; OpBitOr -> 0x09; OpBitXor -> 0x0A; OpShiftL -> 0x0B; OpShiftR -> 0x0C
  OpEq -> 0x0D; OpNotEq -> 0x0E; OpLt -> 0x0F; OpLtE -> 0x10; OpGt -> 0x11; OpGtE -> 0x12
  OpAnd -> 0x13; OpOr -> 0x14; OpNot -> 0x15; OpInvert -> 0x16; OpIn -> 0x17; OpNotIn -> 0x18; OpIs -> 0x19; OpIsNot -> 0x1A
  OpMatMult -> 0x1B

tag :: Word8 -> BB.Builder
tag = BB.word8

encodeText :: T.Text -> BB.Builder
encodeText t =
  let canonicalT = canonicalizeText t
      bs = TE.encodeUtf8 canonicalT
  in BB.int64BE (fromIntegral (BS.length bs)) <> BB.byteString bs

encodeList :: (a -> BB.Builder) -> [a] -> BB.Builder
encodeList itemSerializer xs =
  BB.int64BE (fromIntegral (length xs)) <> mconcat (map itemSerializer xs)

encodeMaybe :: (a -> BB.Builder) -> Maybe a -> BB.Builder
encodeMaybe _ Nothing = tag 0x00
encodeMaybe itemSerializer (Just x) = tag 0x01 <> itemSerializer x
