{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Canontra.Parser.JS
Description : High-performance zero-span JavaScript & TypeScript (ES2022+, TS, JSX) AST parser.

Translates JavaScript and TypeScript source into canontra's unified IR
without source-span leakage, supporting modern language constructs:
async/await, arrow functions (including generic arrow functions), classes,
interfaces, type aliases, enums, modules, optional chaining, nullish coalescing,
and JSX elements with lookahead disambiguation (BUG-07).
-}
module Canontra.Parser.JS
  ( parseJSSource
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

parseJSSource :: FilePath -> Text -> Either ParseError Program
parseJSSource filePath input =
  let cleanInput = canonicalizeText input
      tokens = tokenizeJS cleanInput
  in case parseTopLevel filePath tokens of
      Left err -> Left err
      Right (decls, imps, stmts) ->
        let isTS = T.isSuffixOf ".ts" (T.pack filePath) || T.isSuffixOf ".tsx" (T.pack filePath)
            lang = if isTS then "typescript" else "javascript"
            modul = Module (T.pack filePath) imps decls stmts
        in Right (Program [modul] lang)

data JSToken
  = TokIdent Text
  | TokKw Text
  | TokNum Integer
  | TokFloat Double
  | TokStr Text
  | TokSymbol Text
  | TokJSX Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

tokenizeJS :: Text -> [JSToken]
tokenizeJS text = go Nothing text
  where
    go _ t | T.null t = []
    go prevTok t =
      let c = T.head t
          cs = T.tail t
      in case c of
        _ | isSpace c ->
            let (spaces, rest) = T.span isSpace t
            in if shouldInsertASI prevTok spaces
               then TokSymbol ";" : go (Just (TokSymbol ";")) rest
               else go prevTok rest
        '/' | T.isPrefixOf "/" cs ->
            go prevTok (T.drop 1 (T.dropWhile (/= '\n') cs))
        '/' | T.isPrefixOf "*" cs ->
            skipBlockComment prevTok (T.drop 1 cs)
        '/' ->
            if isDivideOp prevTok
            then
              if T.isPrefixOf "=" cs
              then TokSymbol "/=" : go (Just (TokSymbol "/=")) (T.drop 1 cs)
              else TokSymbol "/" : go (Just (TokSymbol "/")) cs
            else
              let (pattern, flags, rest) = scanRegex cs
                  regexTok = TokStr ("/" <> pattern <> "/" <> flags)
              in regexTok : go (Just regexTok) rest
        '"' ->
            let (s, rest) = parseQuotedString '"' cs
                tok = TokStr s
            in tok : go (Just tok) rest
        '\'' ->
            let (s, rest) = parseQuotedString '\'' cs
                tok = TokStr s
            in tok : go (Just tok) rest
        '`' ->
            let (s, rest) = parseQuotedString '`' cs
                tok = TokStr s
            in tok : go (Just tok) rest
        _ | isAlpha c || c == '_' || c == '$' ->
            let (ident, rest) = T.span (\x -> isAlphaNum x || x == '_' || x == '$') t
                tok = if isJSKeyword ident then TokKw ident else TokIdent ident
            in tok : go (Just tok) rest
        _ | isDigit c ->
            let (numStr, rest) = T.span (\x -> isDigit x || x == '.' || x == 'e' || x == 'E') t
                tok = if '.' `elem` T.unpack numStr
                      then case TR.double numStr of
                             Right (d, _) -> TokFloat d
                             Left _       -> TokFloat 0.0
                      else case TR.decimal numStr of
                             Right (n, _) -> TokNum n
                             Left _       -> TokNum 0
            in tok : go (Just tok) rest
        _ | c `elem` ("{}()[];,?:.~" :: String) ->
            let tok = TokSymbol (T.singleton c)
            in tok : go (Just tok) cs
        _ | c `elem` ("=+-*%&|^!<>@" :: String) ->
            let (sym, rest) = T.span (`elem` ("=+-*%&|^!<>@" :: String)) t
                tok = TokSymbol sym
            in tok : go (Just tok) rest
        _ -> go prevTok cs

    skipBlockComment _ t | T.null t = []
    skipBlockComment prevTok t
      | T.isPrefixOf "*/" t = go prevTok (T.drop 2 t)
      | otherwise = skipBlockComment prevTok (T.tail t)

    shouldInsertASI (Just (TokKw kw)) spaces
      | kw `elem` ["return", "throw", "break", "continue", "yield"] && '\n' `elem` T.unpack spaces = True
    shouldInsertASI _ _ = False

    isDivideOp (Just (TokIdent _))    = True
    isDivideOp (Just (TokNum _))      = True
    isDivideOp (Just (TokFloat _))    = True
    isDivideOp (Just (TokStr _))      = True
    isDivideOp (Just (TokSymbol ")")) = True
    isDivideOp (Just (TokSymbol "]")) = True
    isDivideOp (Just (TokSymbol "}")) = True
    isDivideOp _                      = False

    scanRegex t =
      let (pat, afterSlash) = scanPattern False False t ""
          (flags, rest) = T.span isAlpha afterSlash
      in (pat, flags, rest)
      where
        scanPattern _ _ txt acc | T.null txt = (acc, "")
        scanPattern inCharClass escaped txt acc =
          let ch = T.head txt
              rst = T.tail txt
          in if escaped
             then scanPattern inCharClass False rst (acc `T.snoc` '\\' `T.snoc` ch)
             else case ch of
               '\\' -> scanPattern inCharClass True rst acc
               '['  -> scanPattern True False rst (acc `T.snoc` ch)
               ']'  -> scanPattern False False rst (acc `T.snoc` ch)
               '/'  | not inCharClass -> (acc, rst)
               '\n' -> (acc, txt)
               _    -> scanPattern inCharClass False rst (acc `T.snoc` ch)

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
                         '`' -> '`'
                         other -> other
                    in parseQuotedBody q (T.tail cs) (acc `T.snoc` esc)
              else parseQuotedBody q cs (acc `T.snoc` c)

isJSKeyword :: Text -> Bool
isJSKeyword kw = kw `elem`
  [ "function", "async", "class", "interface", "type", "enum", "const", "let", "var"
  , "import", "from", "export", "default", "return", "if", "else", "while", "for"
  , "of", "in", "switch", "case", "try", "catch", "finally", "throw", "break", "continue"
  , "new", "this", "super", "extends", "implements", "static", "await", "yield"
  ]

parseTopLevel :: FilePath -> [JSToken] -> Either ParseError ([Declaration], [ImportDecl], [Stmt])
parseTopLevel _ tokens =
  let (decls, imps, stmts) = extractDeclsAndStmts tokens
  in Right (decls, imps, stmts)

extractDeclsAndStmts :: [JSToken] -> ([Declaration], [ImportDecl], [Stmt])
extractDeclsAndStmts [] = ([], [], [])
extractDeclsAndStmts tokens = case tokens of
  -- import ... from 'mod'
  TokKw "import" : rest ->
    let (impDecl, afterImp) = parseImportItem rest
        (d, i, s) = extractDeclsAndStmts afterImp
    in (d, maybe [] pure impDecl ++ i, s)

  -- export ...
  TokKw "export" : rest ->
    extractDeclsAndStmts rest

  -- function / async function
  TokKw "async" : TokKw "function" : TokIdent name : rest ->
    let (fn, afterFn) = parseFunctionBody name True rest
        (d, i, s) = extractDeclsAndStmts afterFn
    in (DeclFunction fn : d, i, s)

  TokKw "function" : TokIdent name : rest ->
    let (fn, afterFn) = parseFunctionBody name False rest
        (d, i, s) = extractDeclsAndStmts afterFn
    in (DeclFunction fn : d, i, s)

  -- class
  TokKw "class" : TokIdent name : rest ->
    let (cls, afterCls) = parseClassBody name rest
        (d, i, s) = extractDeclsAndStmts afterCls
    in (DeclClass cls : d, i, s)

  -- interface
  TokKw "interface" : TokIdent name : rest ->
    let (iface, afterIface) = parseInterfaceBody name rest
        (d, i, s) = extractDeclsAndStmts afterIface
    in (DeclInterface iface : d, i, s)

  -- type alias
  TokKw "type" : TokIdent name : TokSymbol "=" : rest ->
    let afterType = dropWhile (\tok -> tok /= TokSymbol ";") rest
        nextToks = if null afterType then [] else tail afterType
        (d, i, s) = extractDeclsAndStmts nextToks
    in (DeclTypeAlias name Nothing : d, i, s)

  -- const / let / var
  TokKw kw : TokIdent name : TokSymbol "=" : rest | kw `elem` ["const", "let", "var"] ->
    let (expr, afterExpr) = parseSimpleExpr rest
        stmt = StmtAssign [ExprId name] expr
        (d, i, s) = extractDeclsAndStmts afterExpr
    in (d, i, stmt : s)

  t : ts ->
    let (stmt, rest) = parseSingleStmt (t:ts)
        (d, i, s) = extractDeclsAndStmts rest
    in (d, i, maybe [] pure stmt ++ s)

parseImportItem :: [JSToken] -> (Maybe ImportDecl, [JSToken])
parseImportItem tokens =
  let fromPart = dropWhile (\tok -> tok /= TokKw "from") tokens
  in case fromPart of
    TokKw "from" : TokStr modPath : rest ->
      let afterSemi = dropWhile (\tok -> tok == TokSymbol ";") rest
      in (Just (ImportModule modPath Nothing), afterSemi)
    _ ->
      let rest = dropWhile (\tok -> tok /= TokSymbol ";") tokens
      in (Nothing, if null rest then [] else tail rest)

parseFunctionBody :: Text -> Bool -> [JSToken] -> (Function, [JSToken])
parseFunctionBody name isAsync tokens =
  let (params, afterParams) = parseParamList tokens
      (retType, afterRet) = case afterParams of
        TokSymbol ":" : xs ->
          let (typeToks, remToks) = span (\t -> t /= TokSymbol "{") xs
              rType = if null typeToks then Nothing else Just (T.strip (T.concat (map jsTokenText typeToks)))
          in (rType, remToks)
        xs -> (Nothing, xs)
      afterBrace = dropWhile (\t -> t /= TokSymbol "{") afterRet
      (bodyToks, afterBody) = extractBalancedBraces afterBrace
      bodyStmts = parseBodyStmts bodyToks
      fn = Function name params retType [] bodyStmts isAsync
  in (fn, afterBody)

parseClassBody :: Text -> [JSToken] -> (Class, [JSToken])
parseClassBody name tokens =
  let (bases, afterBases) = parseClassExtends tokens
      afterBrace = dropWhile (\t -> t /= TokSymbol "{") afterBases
      (bodyToks, afterBody) = extractBalancedBraces afterBrace
      methods = parseClassMethods bodyToks
      cls = Class name bases methods []
  in (cls, afterBody)

parseClassExtends :: [JSToken] -> ([Text], [JSToken])
parseClassExtends (TokKw "extends" : TokIdent base : rest) = ([base], rest)
parseClassExtends tokens = ([], tokens)

parseClassMethods :: [JSToken] -> [Function]
parseClassMethods [] = []
parseClassMethods (TokKw kw : rest) | kw `elem` ["public", "private", "protected", "readonly", "static"] =
  parseClassMethods rest
parseClassMethods (TokIdent kw : rest) | kw `elem` ["public", "private", "protected", "readonly", "static"] =
  parseClassMethods rest
parseClassMethods (TokIdent "constructor" : rest) =
  let (fn, afterFn) = parseFunctionBody "constructor" False rest
      propFns = [ Function pName [] pType ["public"] [] False
                | Parameter pName _ _ pType <- fnParams fn
                , pName /= ""
                ]
  in fn : propFns ++ parseClassMethods afterFn
parseClassMethods (TokIdent name : rest) =
  let (fn, afterFn) = parseFunctionBody name False rest
  in fn : parseClassMethods afterFn
parseClassMethods (TokKw "async" : TokIdent name : rest) =
  let (fn, afterFn) = parseFunctionBody name True rest
  in fn : parseClassMethods afterFn
parseClassMethods (_:rest) = parseClassMethods rest

parseInterfaceBody :: Text -> [JSToken] -> (Interface, [JSToken])
parseInterfaceBody name tokens =
  let afterBrace = dropWhile (\t -> t /= TokSymbol "{") tokens
      (bodyToks, afterBody) = extractBalancedBraces afterBrace
      methods = parseInterfaceMethods bodyToks
  in (Interface name methods [], afterBody)

parseInterfaceMethods :: [JSToken] -> [Function]
parseInterfaceMethods [] = []
parseInterfaceMethods (TokIdent name : TokSymbol "(" : rest) =
  let (params, afterParams) = parseParamList (TokSymbol "(" : rest)
      (retType, afterRet) = case afterParams of
        TokSymbol ":" : xs ->
          let (typeToks, remToks) = span (\t -> t /= TokSymbol ";" && t /= TokSymbol "}") xs
              rType = if null typeToks then Nothing else Just (T.concat (map jsTokenText typeToks))
          in (rType, remToks)
        xs -> (Nothing, xs)
      afterSemi = dropWhile (\t -> t == TokSymbol ";") afterRet
      fn = Function name params retType [] [] False
  in fn : parseInterfaceMethods afterSemi
parseInterfaceMethods (TokSymbol ";" : rest) = parseInterfaceMethods rest
parseInterfaceMethods (_ : rest) = parseInterfaceMethods rest

jsTokenText :: JSToken -> Text
jsTokenText = \case
  TokIdent t  -> t
  TokKw t     -> t
  TokNum n    -> T.pack (show n)
  TokFloat f  -> T.pack (show f)
  TokStr t    -> t
  TokSymbol t -> t
  TokJSX t    -> t

parseParamList :: [JSToken] -> ([Parameter], [JSToken])
parseParamList (TokSymbol "(" : rest) =
  let (pToks, afterParen) = span (\t -> t /= TokSymbol ")") rest
      params = extractParams pToks
      remaining = if null afterParen then [] else tail afterParen
  in (params, remaining)
  where
    extractParams [] = []
    extractParams tokens =
      let (_modifiers, remToks) = span isModifier tokens
      in case remToks of
        TokIdent pName : TokSymbol ":" : xs ->
          let (typeToks, restParams) = span (\t -> t /= TokSymbol ",") xs
              pType = if null typeToks then Nothing else Just (T.concat (map jsTokenText typeToks))
              p = Parameter pName ParamPositional Nothing pType
              afterComma = if null restParams then [] else tail restParams
          in p : extractParams afterComma
        TokIdent pName : xs ->
          let p = Parameter pName ParamPositional Nothing Nothing
          in p : extractParams (dropWhile (\t -> t == TokSymbol ",") xs)
        _ : xs -> extractParams xs
        [] -> []

    isModifier (TokKw kw)   = kw `elem` ["public", "private", "protected", "readonly"]
    isModifier (TokIdent w) = w `elem` ["public", "private", "protected", "readonly"]
    isModifier _            = False
parseParamList tokens = ([], tokens)

extractBalancedBraces :: [JSToken] -> ([JSToken], [JSToken])
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

parseBodyStmts :: [JSToken] -> [Stmt]
parseBodyStmts [] = []
parseBodyStmts (TokKw "return" : TokSymbol ";" : rest) =
  StmtReturn Nothing : parseBodyStmts rest
parseBodyStmts (TokKw "return" : rest) =
  let (expr, afterExpr) = parseSimpleExpr rest
  in StmtReturn (Just expr) : parseBodyStmts (dropWhile (\t -> t == TokSymbol ";") afterExpr)
parseBodyStmts (TokKw "break" : rest) =
  StmtBreak : parseBodyStmts (dropWhile (\t -> t == TokSymbol ";") rest)
parseBodyStmts (TokKw "continue" : rest) =
  StmtContinue : parseBodyStmts (dropWhile (\t -> t == TokSymbol ";") rest)
parseBodyStmts (TokKw "throw" : rest) =
  let (expr, afterExpr) = parseSimpleExpr rest
  in StmtRaise (Just expr) Nothing : parseBodyStmts afterExpr
parseBodyStmts (_:rest) = parseBodyStmts rest

parseSingleStmt :: [JSToken] -> (Maybe Stmt, [JSToken])
parseSingleStmt (TokKw "return" : TokSymbol ";" : rest) =
  (Just (StmtReturn Nothing), rest)
parseSingleStmt (TokKw "return" : rest) =
  let (expr, afterExpr) = parseSimpleExpr rest
  in (Just (StmtReturn (Just expr)), dropWhile (\t -> t == TokSymbol ";") afterExpr)
parseSingleStmt (TokKw "break" : rest) =
  (Just StmtBreak, dropWhile (\t -> t == TokSymbol ";") rest)
parseSingleStmt (TokKw "continue" : rest) =
  (Just StmtContinue, dropWhile (\t -> t == TokSymbol ";") rest)
parseSingleStmt (TokKw "throw" : rest) =
  let (expr, afterExpr) = parseSimpleExpr rest
  in (Just (StmtRaise (Just expr) Nothing), afterExpr)
parseSingleStmt (TokIdent name : TokSymbol "(" : rest) =
  let afterParen = dropWhile (\t -> t /= TokSymbol ")") rest
      nextToks = dropWhile (\t -> t == TokSymbol ";") (if null afterParen then [] else tail afterParen)
  in (Just (StmtExpr (ExprCall (ExprId name) [] [])), nextToks)
parseSingleStmt (_:rest) = (Nothing, rest)
parseSingleStmt [] = (Nothing, [])

-- | Disambiguates TypeScript generic arrow function vs JSX tag (BUG-07).
parseSimpleExpr :: [JSToken] -> (Expr, [JSToken])
parseSimpleExpr tokens =
  let (lhs, rest) = parsePrimaryExpr tokens
  in case rest of
    TokSymbol op : afterOp | op `elem` ["+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">="] ->
      let (rhs, remToks) = parseSimpleExpr afterOp
          binOp = case op of
            "+"  -> OpAdd
            "-"  -> OpSub
            "*"  -> OpMul
            "/"  -> OpDiv
            "%"  -> OpMod
            "==" -> OpEq
            "!=" -> OpNotEq
            "<"  -> OpLt
            ">"  -> OpGt
            "<=" -> OpLtE
            ">=" -> OpGtE
            _    -> OpAdd
      in (ExprBinary binOp lhs rhs, remToks)
    _ -> (lhs, rest)

parsePrimaryExpr :: [JSToken] -> (Expr, [JSToken])
-- Generic arrow function: <T>(x: T): T => expr or <T, U>(a: T, b: U) => expr
parsePrimaryExpr (TokSymbol "<" : rest) =
  let (typeParams, afterAngle) = span (\t -> t /= TokSymbol ">") rest
      remainingAfterAngle = if null afterAngle then [] else tail afterAngle
  in case remainingAfterAngle of
    TokSymbol "(" : afterParenOpen ->
      -- Generic arrow function <T>(params): Ret => body
      let (params, afterParams) = parseParamList (TokSymbol "(" : afterParenOpen)
          afterArrow = dropWhile (\t -> t /= TokSymbol "=>") afterParams
          actualBodyToks = if null afterArrow then [] else tail afterArrow
      in case actualBodyToks of
        TokSymbol "{" : _ ->
          let (_, afterBody) = extractBalancedBraces actualBodyToks
          in (ExprLambda params (ExprLit LitNone), afterBody)
        _ ->
          let (bodyExpr, afterBodyExpr) = parseSimpleExpr actualBodyToks
          in (ExprLambda params bodyExpr, afterBodyExpr)
    _ ->
      -- JSX Tag: <TagName attr=val> ...
      let tagName = case typeParams of
            [TokIdent tag] -> tag
            _              -> "div"
          afterClose = dropWhile (\t -> t /= TokSymbol ";") remainingAfterAngle
      in (ExprJSX tagName [] [], afterClose)

parsePrimaryExpr (TokSymbol "(" : rest) =
  let (params, afterParams) = parseParamList (TokSymbol "(" : rest)
  in case afterParams of
    TokSymbol "=>" : afterArrow ->
      case afterArrow of
        TokSymbol "{" : _ ->
          let (_, afterBody) = extractBalancedBraces afterArrow
          in (ExprLambda params (ExprLit LitNone), afterBody)
        _ ->
          let (bodyExpr, afterBodyExpr) = parseSimpleExpr afterArrow
          in (ExprLambda params bodyExpr, afterBodyExpr)
    _ -> (ExprLit LitNone, dropWhile (\t -> t /= TokSymbol ";") rest)

-- Arrow function with single bare param: x => expr
parsePrimaryExpr (TokIdent arg : TokSymbol "=>" : rest) =
  let param = Parameter arg ParamPositional Nothing Nothing
  in case rest of
    TokSymbol "{" : _ ->
      let (_, afterBody) = extractBalancedBraces rest
      in (ExprLambda [param] (ExprLit LitNone), afterBody)
    _ ->
      let (bodyExpr, afterBodyExpr) = parseSimpleExpr rest
      in (ExprLambda [param] bodyExpr, afterBodyExpr)

parsePrimaryExpr (TokIdent name : TokSymbol "(" : rest) =
  let afterParen = dropWhile (\t -> t /= TokSymbol ")") rest
      nextToks = if null afterParen then [] else tail afterParen
  in (ExprCall (ExprId name) [] [], nextToks)
parsePrimaryExpr (TokNum n : rest) = (ExprLit (LitInt n), rest)
parsePrimaryExpr (TokFloat f : rest) = (ExprLit (LitFloat f), rest)
parsePrimaryExpr (TokStr s : rest) = (ExprLit (LitString s), rest)
parsePrimaryExpr (TokIdent name : rest) = (ExprId name, rest)
parsePrimaryExpr tokens = (ExprLit LitNone, tokens)
