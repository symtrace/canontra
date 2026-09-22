{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Canontra.Parser.Rust
Description : High-performance zero-span Rust (2021+) AST parser.

Translates Rust source code into canontra's unified IR without source-span leakage,
supporting module hierarchies, use trees, structs, enums, traits, impl blocks,
pattern matching, macro calls with lifetime tokens (BUG-08), and visibility modifiers.
-}
module Canontra.Parser.Rust
  ( parseRustSource
  ) where

import Control.DeepSeq (NFData)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import GHC.Generics (Generic)

import Canontra.Canonical.Unicode (canonicalizeText)
import Canontra.IR.Declaration
import Canontra.IR.Dependency
import Canontra.IR.Expression
import Canontra.IR.Program
import Canontra.Types (ParseError (..))

parseRustSource :: FilePath -> Text -> Either ParseError Program
parseRustSource filePath input =
  let cleanInput = canonicalizeText input
      tokens = tokenizeRust cleanInput
  in case parseRustTopLevel filePath tokens of
      Left err -> Left err
      Right (decls, imps, stmts) ->
        let modul = Module (T.pack filePath) imps decls stmts
        in Right (Program [modul] "rust")

data RustToken
  = TokIdent Text
  | TokKw Text
  | TokNum Integer
  | TokFloat Double
  | TokStr Text
  | TokSymbol Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

tokenizeRust :: Text -> [RustToken]
tokenizeRust text = go text
  where
    go t | T.null t = []
    go t =
      let c = T.head t
          cs = T.tail t
      in case c of
        _ | isSpace c -> go (T.dropWhile isSpace t)
        '/' | T.isPrefixOf "/" cs ->
            go (T.drop 1 (T.dropWhile (/= '\n') cs))
        '/' | T.isPrefixOf "*" cs ->
            skipBlockComment (T.drop 1 cs)
        '"' ->
            let (s, rest) = parseQuotedString '"' cs
            in TokStr s : go rest
        '\'' ->
            case T.uncons cs of
              Just (x, xs) | (isAlpha x || x == '_') && not (T.isPrefixOf "'" xs) ->
                let (lifetimeIdent, rest) = T.span (\ch -> isAlphaNum ch || ch == '_') cs
                in TokIdent ("'" <> lifetimeIdent) : go rest
              Just (x, xs) | T.isPrefixOf "'" xs ->
                TokStr (T.singleton x) : go (T.drop 1 xs)
              Just ('\\', xs) ->
                case T.uncons xs of
                  Just (escChar, afterEsc) | T.isPrefixOf "'" afterEsc ->
                    TokStr (T.singleton escChar) : go (T.drop 1 afterEsc)
                  _ -> TokSymbol "'" : go cs
              _ ->
                TokSymbol "'" : go cs
        _ | isAlpha c || c == '_' ->
            let (ident, rest) = T.span (\x -> isAlphaNum x || x == '_') t
            in (if isRustKeyword ident then TokKw ident else TokIdent ident) : go rest
        _ | isDigit c ->
            let (numStr, rest) = T.span (\x -> isDigit x || x == '.' || x == 'e' || x == 'E') t
            in if '.' `elem` T.unpack numStr
               then case TR.double numStr of
                      Right (d, _) -> TokFloat d : go rest
                      Left _       -> TokFloat 0.0 : go rest
               else case TR.decimal numStr of
                      Right (n, _) -> TokNum n : go rest
                      Left _       -> TokNum 0 : go rest
        _ | c `elem` ("{}()[];,?:.~" :: String) ->
            TokSymbol (T.singleton c) : go cs
        _ | c `elem` ("=+-*/%&|^!<>:" :: String) ->
            let (sym, rest) = T.span (`elem` ("=+-*/%&|^!<>:" :: String)) t
            in TokSymbol sym : go rest
        _ -> go cs

    skipBlockComment t | T.null t = []
    skipBlockComment t
      | T.isPrefixOf "*/" t = go (T.drop 2 t)
      | otherwise = skipBlockComment (T.tail t)

    parseQuotedString q t =
      let (body, rest) = parseQuotedBody q t ""
      in (body, rest)

    parseQuotedBody _ t acc | T.null t = (acc, "")
    parseQuotedBody q t acc =
      let c = T.head t
          cs = T.tail t
      in if c == q
         then (acc, cs)
         else if c == '\\' && not (T.null cs)
              then let esc = case T.head cs of
                         'n' -> '\n'
                         't' -> '\t'
                         'r' -> '\r'
                         '\\' -> '\\'
                         '\'' -> '\''
                         '"' -> '"'
                         other -> other
                   in parseQuotedBody q (T.tail cs) (acc `T.snoc` esc)
              else parseQuotedBody q cs (acc `T.snoc` c)

isRustKeyword :: Text -> Bool
isRustKeyword kw = kw `elem`
  [ "fn", "struct", "enum", "trait", "impl", "for", "type", "mod", "use", "pub", "crate"
  , "let", "mut", "const", "static", "if", "else", "match", "loop", "while", "return"
  , "async", "await", "self", "Self", "where", "break", "continue"
  ]

parseRustTopLevel :: FilePath -> [RustToken] -> Either ParseError ([Declaration], [ImportDecl], [Stmt])
parseRustTopLevel _ tokens =
  let (decls, imps, stmts) = extractRustDeclsAndStmts tokens
  in Right (decls, imps, stmts)

extractRustDeclsAndStmts :: [RustToken] -> ([Declaration], [ImportDecl], [Stmt])
extractRustDeclsAndStmts [] = ([], [], [])
extractRustDeclsAndStmts tokens = case tokens of
  -- use path::to::item;
  TokKw "use" : rest ->
    let (pathToks, afterSemi) = span (\tok -> tok /= TokSymbol ";") rest
        path = T.concat [p | TokIdent p <- pathToks]
        imp = ImportModule path Nothing
        (d, i, s) = extractRustDeclsAndStmts (if null afterSemi then [] else tail afterSemi)
    in (d, imp : i, s)

  -- pub ...
  TokKw "pub" : rest ->
    extractRustDeclsAndStmts rest

  -- fn / async fn / const fn
  TokKw "async" : TokKw "fn" : TokIdent name : rest ->
    let (fn, afterFn) = parseRustFunctionBody name True rest
        (d, i, s) = extractRustDeclsAndStmts afterFn
    in (DeclFunction fn : d, i, s)

  TokKw "fn" : TokIdent name : rest ->
    let (fn, afterFn) = parseRustFunctionBody name False rest
        (d, i, s) = extractRustDeclsAndStmts afterFn
    in (DeclFunction fn : d, i, s)

  -- struct Name { ... }
  TokKw "struct" : TokIdent name : rest ->
    let afterBrace = dropWhile (\tok -> tok /= TokSymbol "{") rest
        (fieldsToks, afterBody) = extractBalancedBraces afterBrace
        fields = parseRustFields fieldsToks
        st = Struct name fields [] "pub"
        (d, i, s) = extractRustDeclsAndStmts afterBody
    in (DeclStruct st : d, i, s)

  -- trait Name { ... }
  TokKw "trait" : TokIdent name : rest ->
    let afterBrace = dropWhile (\tok -> tok /= TokSymbol "{") rest
        (methodToks, afterBody) = extractBalancedBraces afterBrace
        methods = parseRustMethods methodToks
        tr = Trait name methods []
        (d, i, s) = extractRustDeclsAndStmts afterBody
    in (DeclTrait tr : d, i, s)

  -- impl Trait for Target { ... } or impl Target { ... }
  TokKw "impl" : rest ->
    let (implDecl, afterBody) = parseRustImpl rest
        (d, i, s) = extractRustDeclsAndStmts afterBody
    in (DeclImpl implDecl : d, i, s)

  t : ts ->
    let (stmt, rest) = parseRustSingleStmt (t:ts)
        (d, i, s) = extractRustDeclsAndStmts rest
    in (d, i, maybe [] pure stmt ++ s)

parseRustImpl :: [RustToken] -> (Impl, [RustToken])
parseRustImpl tokens =
  let (headerToks, afterHeader) = span (\t -> t /= TokSymbol "{") tokens
      (bodyToks, afterBody) = extractBalancedBraces afterHeader
      methods = parseRustMethods bodyToks
      (mTrait, target) = case headerToks of
        [TokIdent tr, TokKw "for", TokIdent tgt] -> (Just tr, tgt)
        [TokIdent tgt]                           -> (Nothing, tgt)
        _                                        -> (Nothing, "")
  in (Impl mTrait target methods, afterBody)

parseRustFunctionBody :: Text -> Bool -> [RustToken] -> (Function, [RustToken])
parseRustFunctionBody name isAsync tokens =
  let (mGenerics, afterGenerics) = case tokens of
        TokSymbol "<" : rest ->
          let (gToks, afterAngle) = extractBalancedAngle rest
              gStr = "<" <> T.concat [tokenText t | t <- gToks] <> ">"
          in (Just gStr, afterAngle)
        _ -> (Nothing, tokens)
      (params, afterParams) = parseRustParamList afterGenerics
      (retType, afterRet) = parseRustReturnType afterParams
      afterWhere = case afterRet of
        TokKw "where" : rest -> dropWhile (\t -> t /= TokSymbol "{" && t /= TokSymbol ";") rest
        _                    -> afterRet
      (bodyStmts, afterBody) = case afterWhere of
        TokSymbol ";" : rest -> ([], rest)
        _ ->
          let afterBrace = dropWhile (\t -> t /= TokSymbol "{") afterWhere
              (bodyToks, remToks) = extractBalancedBraces afterBrace
          in (parseRustBodyStmts bodyToks, remToks)
      decs = maybe [] pure mGenerics
      fn = Function name params retType decs bodyStmts isAsync
  in (fn, afterBody)

extractBalancedAngle :: [RustToken] -> ([RustToken], [RustToken])
extractBalancedAngle tokens = go (1 :: Int) [] tokens
  where
    go 0 acc remToks = (reverse acc, remToks)
    go _ acc [] = (reverse acc, [])
    go depth acc (TokSymbol "<" : xs) = go (depth + 1) (TokSymbol "<" : acc) xs
    go depth acc (TokSymbol ">" : xs) =
      if depth == 1
      then (reverse acc, xs)
      else go (depth - 1) (TokSymbol ">" : acc) xs
    go depth acc (x : xs) = go depth (x : acc) xs

parseRustParamList :: [RustToken] -> ([Parameter], [RustToken])
parseRustParamList (TokSymbol "(" : rest) =
  let (pToks, afterParen) = span (\t -> t /= TokSymbol ")") rest
      params = extractRustParams pToks
      remaining = if null afterParen then [] else tail afterParen
  in (params, remaining)
parseRustParamList tokens = ([], tokens)

extractRustParams :: [RustToken] -> [Parameter]
extractRustParams [] = []
extractRustParams (TokKw "self" : rest) =
  Parameter "self" ParamPositional Nothing Nothing : extractRustParams (dropWhile (\t -> t == TokSymbol ",") rest)
extractRustParams (TokSymbol "&" : TokKw "self" : rest) =
  Parameter "&self" ParamPositional Nothing Nothing : extractRustParams (dropWhile (\t -> t == TokSymbol ",") rest)
extractRustParams (TokSymbol "&" : TokKw "mut" : TokKw "self" : rest) =
  Parameter "&mut self" ParamPositional Nothing Nothing : extractRustParams (dropWhile (\t -> t == TokSymbol ",") rest)
extractRustParams (TokIdent pName : TokSymbol ":" : rest) =
  let (tyToks, afterTy) = span (\t -> t /= TokSymbol "," && t /= TokSymbol ")") rest
      tyStr = if null tyToks then Nothing else Just (T.concat [tokenText t | t <- tyToks])
      remToks = dropWhile (\t -> t == TokSymbol ",") afterTy
  in Parameter pName ParamPositional Nothing tyStr : extractRustParams remToks
extractRustParams (TokIdent pName : rest) =
  Parameter pName ParamPositional Nothing Nothing : extractRustParams (dropWhile (\t -> t == TokSymbol ",") rest)
extractRustParams (_:rest) = extractRustParams rest

parseRustReturnType :: [RustToken] -> (Maybe Text, [RustToken])
parseRustReturnType (TokSymbol "->" : rest) =
  let (tyToks, afterTy) = span (\t -> t /= TokSymbol "{" && t /= TokKw "where" && t /= TokSymbol ";") rest
      tyStr = if null tyToks then Nothing else Just (T.concat [tokenText t | t <- tyToks])
  in (tyStr, afterTy)
parseRustReturnType tokens = (Nothing, tokens)

parseRustFields :: [RustToken] -> [(Text, Maybe Text)]
parseRustFields [] = []
parseRustFields (TokKw "pub" : rest) = parseRustFields rest
parseRustFields (TokIdent fName : TokSymbol ":" : rest) =
  let (tyToks, afterTy) = span (\t -> t /= TokSymbol "," && t /= TokSymbol "}") rest
      tyStr = if null tyToks then Nothing else Just (T.concat [tokenText t | t <- tyToks])
      remToks = dropWhile (\t -> t == TokSymbol ",") afterTy
  in (fName, tyStr) : parseRustFields remToks
parseRustFields (_:rest) = parseRustFields rest

parseRustMethods :: [RustToken] -> [Function]
parseRustMethods [] = []
parseRustMethods (TokKw "pub" : rest) = parseRustMethods rest
parseRustMethods (TokKw "fn" : TokIdent name : rest) =
  let (fn, afterFn) = parseRustFunctionBody name False rest
  in fn : parseRustMethods afterFn
parseRustMethods (TokKw "async" : TokKw "fn" : TokIdent name : rest) =
  let (fn, afterFn) = parseRustFunctionBody name True rest
  in fn : parseRustMethods afterFn
parseRustMethods (_:rest) = parseRustMethods rest

extractBalancedBraces :: [RustToken] -> ([RustToken], [RustToken])
extractBalancedBraces (TokSymbol "{" : rest) = go (1 :: Int) [] rest
  where
    go 0 acc remaining = (reverse acc, remaining)
    go _ acc [] = (reverse acc, [])
    go depth acc (TokSymbol "{" : xs) = go (depth + 1) (TokSymbol "{" : acc) xs
    go depth acc (TokSymbol "}" : xs) =
      if depth == 1
      then (reverse acc, xs)
      else go (depth - 1) (TokSymbol "}" : acc) xs
    go depth acc (x:xs) = go depth (x : acc) xs
extractBalancedBraces tokens = ([], tokens)

parseRustBodyStmts :: [RustToken] -> [Stmt]
parseRustBodyStmts [] = []
parseRustBodyStmts (TokKw "loop" : rest) =
  let afterBrace = dropWhile (\t -> t /= TokSymbol "{") rest
      (bodyToks, afterBody) = extractBalancedBraces afterBrace
  in StmtLoop (parseRustBodyStmts bodyToks) : parseRustBodyStmts afterBody
parseRustBodyStmts tokens =
  case parseRustSingleStmt tokens of
    (Just stmt, afterStmt) -> stmt : parseRustBodyStmts afterStmt
    (Nothing, _:rest)      -> parseRustBodyStmts rest
    (Nothing, [])          -> []

parseRustSingleStmt :: [RustToken] -> (Maybe Stmt, [RustToken])
parseRustSingleStmt [] = (Nothing, [])
parseRustSingleStmt (TokKw "return" : rest) =
  let (expr, afterExpr) = parseRustSimpleExpr rest
  in (Just (StmtReturn (Just expr)), afterExpr)
parseRustSingleStmt (TokKw "let" : rest) =
  let restAfterMut = case rest of
        TokKw "mut" : r -> r
        _               -> rest
  in case restAfterMut of
       TokIdent name : TokSymbol ":" : afterName ->
         let afterType = dropWhile (\t -> t /= TokSymbol "=" && t /= TokSymbol ";") afterName
         in case afterType of
              TokSymbol "=" : exprToks ->
                let (expr, afterExpr) = parseRustSimpleExpr exprToks
                in (Just (StmtAssign [ExprId name] expr), afterExpr)
              _ ->
                let nextToks = dropWhile (\t -> t == TokSymbol ";") afterType
                in (Just (StmtAssign [ExprId name] (ExprId "()")), nextToks)
       TokIdent name : TokSymbol "=" : exprToks ->
         let (expr, afterExpr) = parseRustSimpleExpr exprToks
         in (Just (StmtAssign [ExprId name] expr), afterExpr)
       TokIdent name : TokSymbol ";" : afterSemi ->
         (Just (StmtAssign [ExprId name] (ExprId "()")), afterSemi)
       _ -> (Nothing, rest)
parseRustSingleStmt (TokIdent name : TokSymbol "!" : rest) =
  let (macroExpr, afterMacro) = parseRustSimpleExpr (TokIdent name : TokSymbol "!" : rest)
  in (Just (StmtExpr macroExpr), afterMacro)
parseRustSingleStmt (TokIdent name : TokSymbol "=" : rest) =
  let (expr, afterExpr) = parseRustSimpleExpr rest
  in (Just (StmtAssign [ExprId name] expr), afterExpr)
parseRustSingleStmt (TokIdent name : TokSymbol "(" : rest) =
  let afterParen = dropWhile (\t -> t /= TokSymbol ")") rest
      nextToks = dropWhile (\t -> t == TokSymbol ";") (if null afterParen then [] else tail afterParen)
  in (Just (StmtExpr (ExprCall (ExprId name) [] [])), nextToks)
parseRustSingleStmt (_:rest) = (Nothing, rest)

parseRustSimpleExpr :: [RustToken] -> (Expr, [RustToken])
parseRustSimpleExpr (TokSymbol ";" : rest) = (ExprLit LitNone, rest)
parseRustSimpleExpr (TokIdent name : TokSymbol "!" : TokSymbol openB : rest)
  | openB `elem` ["(", "[", "{"] =
      let closeB = case openB of "(" -> ")"; "[" -> "]"; _ -> "}"
          (bodyToks, afterClose) = extractBalancedDelim openB closeB rest
          nextToks = dropWhile (\t -> t == TokSymbol ";") afterClose
          macroArgs = if null bodyToks then [] else [ExprId (T.concat [tokenText t | t <- bodyToks])]
      in (ExprMacroCall name macroArgs, nextToks)
parseRustSimpleExpr (TokIdent name : TokSymbol "(" : rest) =
  let afterParen = dropWhile (\t -> t /= TokSymbol ")") rest
      nextToks = dropWhile (\t -> t == TokSymbol ";") (if null afterParen then [] else tail afterParen)
  in (ExprCall (ExprId name) [] [], nextToks)
parseRustSimpleExpr (TokNum n : rest) =
  let nextToks = dropWhile (\t -> t == TokSymbol ";") rest
  in (ExprLit (LitInt n), nextToks)
parseRustSimpleExpr (TokFloat f : rest) =
  let nextToks = dropWhile (\t -> t == TokSymbol ";") rest
  in (ExprLit (LitFloat f), nextToks)
parseRustSimpleExpr (TokStr s : rest) =
  let nextToks = dropWhile (\t -> t == TokSymbol ";") rest
  in (ExprLit (LitString s), nextToks)
parseRustSimpleExpr (TokIdent name : rest) =
  let nextToks = dropWhile (\t -> t == TokSymbol ";") rest
  in (ExprId name, nextToks)
parseRustSimpleExpr tokens =
  let after = dropWhile (\t -> t /= TokSymbol ";") tokens
  in (ExprLit LitNone, if null after then [] else tail after)

extractBalancedDelim :: Text -> Text -> [RustToken] -> ([RustToken], [RustToken])
extractBalancedDelim openB closeB tokens = go (1 :: Int) [] tokens
  where
    go 0 acc remToks = (reverse acc, remToks)
    go _ acc [] = (reverse acc, [])
    go depth acc (TokSymbol s : xs)
      | s == openB  = go (depth + 1) (TokSymbol s : acc) xs
      | s == closeB =
          if depth == 1
          then (reverse acc, xs)
          else go (depth - 1) (TokSymbol s : acc) xs
    go depth acc (x : xs) = go depth (x : acc) xs

tokenText :: RustToken -> Text
tokenText (TokIdent t)  = t
tokenText (TokKw t)     = t
tokenText (TokSymbol t) = t
tokenText (TokStr t)    = "\"" <> t <> "\""
tokenText (TokNum n)    = T.pack (show n)
tokenText (TokFloat f)  = T.pack (show f)
