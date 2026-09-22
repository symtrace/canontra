{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Canontra.Canonical.FusedStream
Description : Fused single-pass direct-to-hash and direct-to-builder serialization.

Inlines all Normalizer v3 rules (multi-scope docstring stripping, comment elimination,
parameter canonicalization, decorator/symbol ordering, IEEE-754 float normalization,
and Unicode NFC canonicalization) directly into the binary serialization stream.
Eliminates intermediate AST materialization on the GHC nursery heap and connects
IR nodes directly to SHA-256 context folds.
-}
module Canontra.Canonical.FusedStream
  ( fusedStreamProgram
  , fusedStreamDeclarations
  , fusedStreamModule
  , fusedStreamDeclarationStructural
  , fusedStreamDeclaration
  , fusedStreamStmt
  , fusedStreamExpr
  , fusedHashProgram
  , fusedHashDeclarations
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import Data.List (sort, sortBy)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)

import Canontra.Canonical.Float (encodeCanonicalFloat)
import Canontra.Canonical.StreamingHash (hashBuilderDirect)
import Canontra.Canonical.Unicode (canonicalizeText)
import Canontra.IR.Declaration
import Canontra.IR.Dependency
import Canontra.IR.Expression
import Canontra.IR.Program
import Canontra.Normalize.Normalize (isReflectionDocstring, normalizeModuleDeclarations, preservesDocstrings)
import Canontra.Types (Fingerprint)

-- | Compute F1 Structural fingerprint directly from IR with fused single-pass normalization and hashing.
{-# INLINE fusedHashProgram #-}
fusedHashProgram :: Program -> Fingerprint
fusedHashProgram = hashBuilderDirect . fusedStreamProgram

-- | Compute F2 Declaration fingerprint directly from declarations with fused single-pass normalization and hashing.
{-# INLINE fusedHashDeclarations #-}
fusedHashDeclarations :: [Declaration] -> Fingerprint
fusedHashDeclarations = hashBuilderDirect . fusedStreamDeclarations

-- | Serialize a Program directly to a Builder applying normalization on-the-fly.
{-# INLINE fusedStreamProgram #-}
fusedStreamProgram :: Program -> BB.Builder
fusedStreamProgram (Program modules lang) =
  tag 0x01 <>
  encodeText lang <>
  encodeList fusedStreamModuleB modules

-- | Serialize a list of declarations directly to a Builder applying normalization on-the-fly (F2 format).
{-# INLINE fusedStreamDeclarations #-}
fusedStreamDeclarations :: [Declaration] -> BB.Builder
fusedStreamDeclarations decls =
  tag 0x02 <> encodeList fusedStreamDeclarationB (normalizeModuleDeclarations decls)

-- | Serialize a Module directly to a strict ByteString.
fusedStreamModule :: Module -> BB.Builder
fusedStreamModule = fusedStreamModuleB

fusedStreamModuleB :: Module -> BB.Builder
fusedStreamModuleB (Module _ imps decls stmts) =
  let normImps = sort (map normalizeImport imps)
      normDecls = normalizeModuleDeclarations decls
      cleanStmts = fusedCleanStmts stmts
  in tag 0x10 <>
     encodeText "" <>
     encodeList fusedStreamImportB normImps <>
     encodeList fusedStreamDeclarationStructuralB normDecls <>
     encodeList fusedStreamStmtB cleanStmts

normalizeImport :: ImportDecl -> ImportDecl
normalizeImport = \case
  ImportModule modName alias -> ImportModule modName alias
  ImportFrom modName (ImportSymbols syms) -> ImportFrom modName (ImportSymbols (sort syms))
  ImportFrom modName ImportAll -> ImportFrom modName ImportAll

fusedStreamImportB :: ImportDecl -> BB.Builder
fusedStreamImportB = \case
  ImportModule modName maybeAlias ->
    tag 0x20 <> encodeText modName <> encodeMaybe encodeText maybeAlias
  ImportFrom modName (ImportSymbols syms) ->
    tag 0x21 <> encodeText modName <> encodeList (\(s, a) -> encodeText s <> encodeMaybe encodeText a) syms
  ImportFrom modName ImportAll ->
    tag 0x22 <> encodeText modName

-- | Serialize a Declaration with full structural body (F1 format).
fusedStreamDeclarationStructural :: Declaration -> BB.Builder
fusedStreamDeclarationStructural = fusedStreamDeclarationStructuralB

fusedStreamDeclarationStructuralB :: Declaration -> BB.Builder
fusedStreamDeclarationStructuralB = fusedStreamDeclarationStructuralWithPreserveB False

fusedStreamDeclarationStructuralWithPreserveB :: Bool -> Declaration -> BB.Builder
fusedStreamDeclarationStructuralWithPreserveB classPreserve = \case
  DeclFunction (Function name params retType decs body isAsync) ->
    let preserve = classPreserve || preservesDocstrings decs
    in tag 0x30 <>
       encodeText name <>
       encodeList fusedStreamParamB params <>
       encodeMaybe (encodeText . T.strip) retType <>
       encodeList encodeText (sort decs) <>
       encodeList fusedStreamStmtB (fusedCleanStmtsWithPreserve preserve body) <>
       (if isAsync then tag 0x01 else tag 0x00)
  DeclClass (Class name bases methods decs) ->
    let isClsPreserved = classPreserve || preservesDocstrings decs
    in tag 0x31 <>
       encodeText name <>
       encodeList encodeText bases <>
       encodeList (fusedStreamDeclarationStructuralWithPreserveB isClsPreserved . DeclFunction) methods <>
       encodeList encodeText (sort decs)
  DeclStruct (Struct name fields methods vis) ->
    tag 0x32 <>
    encodeText name <>
    encodeList (\(f, t) -> encodeText f <> encodeMaybe (encodeText . T.strip) t) fields <>
    encodeList (fusedStreamDeclarationStructuralB . DeclFunction) methods <>
    encodeText vis
  DeclInterface (Interface name methods bases) ->
    tag 0x33 <>
    encodeText name <>
    encodeList (fusedStreamDeclarationStructuralB . DeclFunction) methods <>
    encodeList encodeText (sort bases)
  DeclReceiver (Receiver var ty ptr) fn ->
    tag 0x34 <>
    encodeText var <> encodeText ty <> (if ptr then tag 0x01 else tag 0x00) <>
    fusedStreamDeclarationStructuralB (DeclFunction fn)
  DeclTrait (Trait name methods superTrs) ->
    tag 0x35 <>
    encodeText name <>
    encodeList (fusedStreamDeclarationStructuralB . DeclFunction) methods <>
    encodeList encodeText (sort superTrs)
  DeclImpl (Impl mTr tgt methods) ->
    tag 0x36 <>
    encodeMaybe encodeText mTr <>
    encodeText tgt <>
    encodeList (fusedStreamDeclarationStructuralB . DeclFunction) methods
  DeclVariable varName maybeType ->
    tag 0x37 <> encodeText varName <> encodeMaybe (encodeText . T.strip) maybeType
  DeclTypeAlias aliasName origType ->
    tag 0x38 <> encodeText aliasName <> encodeMaybe (encodeText . T.strip) origType

-- | Serialize a Declaration signature without body (F2 format).
fusedStreamDeclaration :: Declaration -> BB.Builder
fusedStreamDeclaration = fusedStreamDeclarationB

fusedStreamDeclarationB :: Declaration -> BB.Builder
fusedStreamDeclarationB = \case
  DeclFunction (Function name params retType decs _ isAsync) ->
    tag 0x30 <>
    encodeText name <>
    encodeList fusedStreamParamB params <>
    encodeMaybe (encodeText . T.strip) retType <>
    encodeList encodeText (sort decs) <>
    (if isAsync then tag 0x01 else tag 0x00)
  DeclClass (Class name bases methods decs) ->
    tag 0x31 <>
    encodeText name <>
    encodeList encodeText bases <>
    encodeList (fusedStreamDeclarationB . DeclFunction) methods <>
    encodeList encodeText (sort decs)
  DeclStruct (Struct name fields methods vis) ->
    tag 0x32 <>
    encodeText name <>
    encodeList (\(f, t) -> encodeText f <> encodeMaybe (encodeText . T.strip) t) fields <>
    encodeList (fusedStreamDeclarationB . DeclFunction) methods <>
    encodeText vis
  DeclInterface (Interface name methods bases) ->
    tag 0x33 <>
    encodeText name <>
    encodeList (fusedStreamDeclarationB . DeclFunction) methods <>
    encodeList encodeText (sort bases)
  DeclReceiver (Receiver var ty ptr) fn ->
    tag 0x34 <>
    encodeText var <> encodeText ty <> (if ptr then tag 0x01 else tag 0x00) <>
    fusedStreamDeclarationB (DeclFunction fn)
  DeclTrait (Trait name methods superTrs) ->
    tag 0x35 <>
    encodeText name <>
    encodeList (fusedStreamDeclarationB . DeclFunction) methods <>
    encodeList encodeText (sort superTrs)
  DeclImpl (Impl mTr tgt methods) ->
    tag 0x36 <>
    encodeMaybe encodeText mTr <>
    encodeText tgt <>
    encodeList (fusedStreamDeclarationB . DeclFunction) methods
  DeclVariable varName maybeType ->
    tag 0x37 <> encodeText varName <> encodeMaybe (encodeText . T.strip) maybeType
  DeclTypeAlias aliasName origType ->
    tag 0x38 <> encodeText aliasName <> encodeMaybe (encodeText . T.strip) origType

fusedStreamParamB :: Parameter -> BB.Builder
fusedStreamParamB (Parameter name kind defVal mType) =
  encodeText name <>
  tag (paramKindTag kind) <>
  encodeMaybe (encodeText . T.strip) defVal <>
  encodeMaybe (encodeText . T.strip) mType

paramKindTag :: ParamKind -> Word8
paramKindTag = \case
  ParamPositional     -> 0x01
  ParamKeywordOnly    -> 0x02
  ParamVarArgs        -> 0x03
  ParamKwArgs         -> 0x04
  ParamPositionalOnly -> 0x05

-- | Serialize a Statement directly to a Builder.
fusedStreamStmt :: Stmt -> BB.Builder
fusedStreamStmt = fusedStreamStmtB

fusedStreamStmtB :: Stmt -> BB.Builder
fusedStreamStmtB = \case
  StmtAssign targets expr ->
    tag 0x40 <> encodeList fusedStreamExprB targets <> fusedStreamExprB expr
  StmtAugAssign target op expr ->
    tag 0x41 <> fusedStreamExprB target <> tag (opTag op) <> fusedStreamExprB expr
  StmtExpr expr ->
    tag 0x42 <> fusedStreamExprB expr
  StmtReturn maybeExpr ->
    tag 0x43 <> encodeMaybe fusedStreamExprB maybeExpr
  StmtIf cond body elseSuite ->
    tag 0x44 <> fusedStreamExprB cond <> encodeList fusedStreamStmtB (fusedCleanStmts body) <> encodeList fusedStreamStmtB (fusedCleanStmts elseSuite)
  StmtWhile cond body elseSuite ->
    tag 0x45 <> fusedStreamExprB cond <> encodeList fusedStreamStmtB (fusedCleanStmts body) <> encodeList fusedStreamStmtB (fusedCleanStmts elseSuite)
  StmtFor target iter body elseSuite ->
    tag 0x46 <> fusedStreamExprB target <> fusedStreamExprB iter <> encodeList fusedStreamStmtB (fusedCleanStmts body) <> encodeList fusedStreamStmtB (fusedCleanStmts elseSuite)
  StmtTry body handlers elseSuite finalSuite ->
    tag 0x47 <> encodeList fusedStreamStmtB (fusedCleanStmts body) <>
    encodeList (\(c, a, b) -> encodeMaybe fusedStreamExprB c <> encodeMaybe encodeText a <> encodeList fusedStreamStmtB (fusedCleanStmts b)) handlers <>
    encodeList fusedStreamStmtB (fusedCleanStmts elseSuite) <> encodeList fusedStreamStmtB (fusedCleanStmts finalSuite)
  StmtWith items body ->
    tag 0x48 <> encodeList (\(e, a) -> fusedStreamExprB e <> encodeMaybe fusedStreamExprB a) items <> encodeList fusedStreamStmtB (fusedCleanStmts body)
  StmtAssert expr maybeMsg ->
    tag 0x49 <> fusedStreamExprB expr <> encodeMaybe fusedStreamExprB maybeMsg
  StmtRaise maybeExpr maybeCause ->
    tag 0x4A <> encodeMaybe fusedStreamExprB maybeExpr <> encodeMaybe fusedStreamExprB maybeCause
  StmtBreak -> tag 0x4B
  StmtContinue -> tag 0x4C
  StmtPass -> tag 0x4D
  StmtDelete exprs -> tag 0x4E <> encodeList fusedStreamExprB exprs
  StmtGlobal vars -> tag 0x4F <> encodeList encodeText (sort vars)
  StmtNonlocal vars -> tag 0x50 <> encodeList encodeText (sort vars)
  StmtAnnAssign target ty maybeVal ->
    tag 0x51 <> fusedStreamExprB target <> fusedStreamExprB ty <> encodeMaybe fusedStreamExprB maybeVal
  StmtAsyncFor target iter body elseSuite ->
    tag 0x52 <> fusedStreamExprB target <> fusedStreamExprB iter <> encodeList fusedStreamStmtB (fusedCleanStmts body) <> encodeList fusedStreamStmtB (fusedCleanStmts elseSuite)
  StmtAsyncWith items body ->
    tag 0x53 <> encodeList (\(e, a) -> fusedStreamExprB e <> encodeMaybe fusedStreamExprB a) items <> encodeList fusedStreamStmtB (fusedCleanStmts body)
  StmtMatch expr cases ->
    tag 0x54 <> fusedStreamExprB expr <> encodeList fusedStreamMatchCaseB cases
  StmtGo expr ->
    tag 0x55 <> fusedStreamExprB expr
  StmtDefer expr ->
    tag 0x56 <> fusedStreamExprB expr
  StmtChanSend ch val ->
    tag 0x57 <> fusedStreamExprB ch <> fusedStreamExprB val
  StmtSelect cases ->
    tag 0x58 <> encodeList (\(sc, b) -> fusedStreamSelectCaseB sc <> encodeList fusedStreamStmtB (fusedCleanStmts b)) cases
  StmtLoop body ->
    tag 0x59 <> encodeList fusedStreamStmtB (fusedCleanStmts body)
  StmtSwitch expr cases defStmts ->
    tag 0x5A <> fusedStreamExprB expr <> encodeList (\(c, b) -> fusedStreamExprB c <> encodeList fusedStreamStmtB (fusedCleanStmts b)) cases <> encodeList fusedStreamStmtB (fusedCleanStmts defStmts)

fusedStreamSelectCaseB :: SelectCase -> BB.Builder
fusedStreamSelectCaseB = \case
  SelectSend ch val -> tag 0x01 <> fusedStreamExprB ch <> fusedStreamExprB val
  SelectRecv mV ch  -> tag 0x02 <> encodeMaybe encodeText mV <> fusedStreamExprB ch
  SelectDefault     -> tag 0x03

fusedStreamMatchCaseB :: MatchCase -> BB.Builder
fusedStreamMatchCaseB (MatchCase pat guard body) =
  fusedStreamExprB pat <> encodeMaybe fusedStreamExprB guard <> encodeList fusedStreamStmtB (fusedCleanStmts body)

-- | Serialize an Expression directly to a Builder.
fusedStreamExpr :: Expr -> BB.Builder
fusedStreamExpr = fusedStreamExprB

fusedStreamExprB :: Expr -> BB.Builder
fusedStreamExprB = \case
  ExprId ident -> tag 0x60 <> encodeText ident
  ExprLit lit -> tag 0x61 <> canonicalizeLitB lit
  ExprBinary op e1 e2 -> tag 0x62 <> tag (opTag op) <> fusedStreamExprB e1 <> fusedStreamExprB e2
  ExprUnary op e -> tag 0x63 <> tag (opTag op) <> fusedStreamExprB e
  ExprCall target args kwArgs ->
    let normKwArgs = sortBy (comparing fst) kwArgs
    in tag 0x64 <> fusedStreamExprB target <> encodeList fusedStreamExprB args <> encodeList (\(k, v) -> encodeText k <> fusedStreamExprB v) normKwArgs
  ExprAttr target attr -> tag 0x65 <> fusedStreamExprB target <> encodeText attr
  ExprSubscript target idx -> tag 0x66 <> fusedStreamExprB target <> fusedStreamExprB idx
  ExprList items -> tag 0x67 <> encodeList fusedStreamExprB items
  ExprTuple items -> tag 0x68 <> encodeList fusedStreamExprB items
  ExprDict items -> tag 0x69 <> encodeList (\(k, v) -> fusedStreamExprB k <> fusedStreamExprB v) items
  ExprSet items -> tag 0x6A <> encodeList fusedStreamExprB items
  ExprLambda params body -> tag 0x6B <> encodeList fusedStreamParamB params <> fusedStreamExprB body
  ExprTernary cond t f -> tag 0x6C <> fusedStreamExprB cond <> fusedStreamExprB t <> fusedStreamExprB f
  ExprListComp item comps -> tag 0x6D <> fusedStreamExprB item <> encodeList fusedStreamCompB comps
  ExprDictComp k v comps -> tag 0x6E <> fusedStreamExprB k <> fusedStreamExprB v <> encodeList fusedStreamCompB comps
  ExprGenerator item comps -> tag 0x6F <> fusedStreamExprB item <> encodeList fusedStreamCompB comps
  ExprSetComp item comps -> tag 0x80 <> fusedStreamExprB item <> encodeList fusedStreamCompB comps
  ExprWalrus name val -> tag 0x81 <> encodeText name <> fusedStreamExprB val
  ExprAwait e -> tag 0x82 <> fusedStreamExprB e
  ExprYield me -> tag 0x83 <> encodeMaybe fusedStreamExprB me
  ExprYieldFrom e -> tag 0x84 <> fusedStreamExprB e
  ExprFormattedString parts -> tag 0x85 <> encodeList fusedStreamFStringPartB parts
  ExprStarred e -> tag 0x86 <> fusedStreamExprB e
  ExprKwStarred e -> tag 0x87 <> fusedStreamExprB e
  ExprSlice ms me mst -> tag 0x88 <> encodeMaybe fusedStreamExprB ms <> encodeMaybe fusedStreamExprB me <> encodeMaybe fusedStreamExprB mst
  ExprOptChain e prop -> tag 0x89 <> fusedStreamExprB e <> encodeText prop
  ExprNullish e1 e2 -> tag 0x8A <> fusedStreamExprB e1 <> fusedStreamExprB e2
  ExprChanRecv ch -> tag 0x8B <> fusedStreamExprB ch
  ExprTryOp e -> tag 0x8C <> fusedStreamExprB e
  ExprMacroCall name args -> tag 0x8D <> encodeText name <> encodeList fusedStreamExprB args
  ExprJSX tagElem attrs children ->
    let normAttrs = sortBy (comparing fst) attrs
    in tag 0x8E <> encodeText tagElem <>
       encodeList (\(k, v) -> encodeText k <> fusedStreamExprB v) normAttrs <>
       encodeList fusedStreamExprB children

fusedStreamFStringPartB :: FStringPart -> BB.Builder
fusedStreamFStringPartB = \case
  FStringText t -> tag 0x01 <> encodeText t
  FStringExpr e conv fmt -> tag 0x02 <> fusedStreamExprB e <> encodeMaybe encodeText conv <> encodeMaybe encodeText fmt

fusedStreamCompB :: CompFor -> BB.Builder
fusedStreamCompB (CompFor target iter ifs) =
  fusedStreamExprB target <> fusedStreamExprB iter <> encodeList fusedStreamExprB ifs

canonicalizeLitB :: Lit -> BB.Builder
canonicalizeLitB = \case
  LitInt n      -> tag 0x70 <> BB.integerDec n
  LitFloat f    -> tag 0x71 <> BB.byteString (encodeCanonicalFloat f)
  LitString s   -> tag 0x72 <> encodeText s
  LitBytes b    -> tag 0x73 <> encodeText b
  LitBool True  -> tag 0x74
  LitBool False -> tag 0x75
  LitNone       -> tag 0x76
  LitEllipsis   -> tag 0x77

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

-- | Drops leading docstring and filters redundant passes / docstring expressions in a suite.
fusedCleanStmts :: [Stmt] -> [Stmt]
fusedCleanStmts = fusedCleanStmtsWithPreserve False

fusedCleanStmtsWithPreserve :: Bool -> [Stmt] -> [Stmt]
fusedCleanStmtsWithPreserve preserve rawStmts =
  let stripped = if preserve then rawStmts else stripLeadingDocstring rawStmts
      nonDoc = if preserve then stripped else filter (not . isDocstringStmt) stripped
      elimPass = if length nonDoc > 1 then filter (not . isPass) nonDoc else nonDoc
  in if null elimPass then [StmtPass] else elimPass
  where
    isDocstringStmt (StmtExpr (ExprLit (LitString s))) = not (isReflectionDocstring s)
    isDocstringStmt _ = False

    isPass StmtPass = True
    isPass _ = False

    stripLeadingDocstring [] = []
    stripLeadingDocstring (StmtExpr (ExprLit (LitString s)) : rest)
      | isReflectionDocstring s = StmtExpr (ExprLit (LitString s)) : rest
      | otherwise = rest
    stripLeadingDocstring xs = xs
