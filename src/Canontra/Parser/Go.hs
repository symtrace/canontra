{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Canontra.Parser.Go
Description : High-performance zero-span Go (1.20+) AST parser.

Translates Go source code into canontra's unified IR without source-span leakage,
supporting package structures, factored imports, receiver methods, structs,
interfaces, goroutines, channels, defer, select, and type switches (BUG-09).
-}
module Canontra.Parser.Go
  ( parseGoSource
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

parseGoSource :: FilePath -> Text -> Either ParseError Program
parseGoSource filePath input =
  let cleanInput = canonicalizeText input
      tokens = tokenizeGo cleanInput
  in case parseGoTopLevel filePath tokens of
      Left err -> Left err
      Right (pkgName, decls, imps, stmts) ->
        let modName = if T.null pkgName then T.pack filePath else pkgName
            modul = Module modName imps decls stmts
        in Right (Program [modul] "go")

data GoToken
  = TokIdent Text
  | TokKw Text
  | TokNum Integer
  | TokFloat Double
  | TokStr Text
  | TokSymbol Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

tokenizeGo :: Text -> [GoToken]
tokenizeGo text = go text
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
        '`' ->
            let (s, rest) = parseQuotedString '`' cs
            in TokStr s : go rest
        _ | isAlpha c || c == '_' ->
            let (ident, rest) = T.span (\x -> isAlphaNum x || x == '_') t
            in (if isGoKeyword ident then TokKw ident else TokIdent ident) : go rest
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
                         '`' -> '`'
                         other -> other
                   in parseQuotedBody q (T.tail cs) (acc `T.snoc` esc)
              else parseQuotedBody q cs (acc `T.snoc` c)

isGoKeyword :: Text -> Bool
isGoKeyword kw = kw `elem`
  [ "package", "import", "func", "type", "struct", "interface", "var", "const"
  , "return", "if", "else", "for", "range", "switch", "case", "default"
  , "go", "defer", "select", "chan", "map", "fallthrough", "break", "continue"
  ]

parseGoTopLevel :: FilePath -> [GoToken] -> Either ParseError (Text, [Declaration], [ImportDecl], [Stmt])
parseGoTopLevel _ tokens =
  let (pkg, afterPkg) = case tokens of
        TokKw "package" : TokIdent p : rest -> (p, rest)
        _                                   -> ("", tokens)
      (decls, imps, stmts) = extractGoDeclsAndStmts afterPkg
  in Right (pkg, decls, imps, stmts)

extractGoDeclsAndStmts :: [GoToken] -> ([Declaration], [ImportDecl], [Stmt])
extractGoDeclsAndStmts [] = ([], [], [])
extractGoDeclsAndStmts tokens = case tokens of
  -- import "fmt" or import ( ... )
  TokKw "import" : rest ->
    let (impDecls, afterImp) = parseGoImports rest
        (d, i, s) = extractGoDeclsAndStmts afterImp
    in (d, impDecls ++ i, s)

  -- func (r *Receiver) Method(params) ret { body }
  TokKw "func" : TokSymbol "(" : rest ->
    let (rc, afterRc) = parseReceiver rest
    in case afterRc of
      TokIdent mName : TokSymbol "[" : afterName ->
        let (tParams, afterTParams) = span (\t -> t /= TokSymbol "]") afterName
            tParamText = "[" <> T.unwords [tokenText t | t <- tParams] <> "]"
            afterBracket = if null afterTParams then [] else tail afterTParams
            (fn, afterFn) = parseGoFunctionBody mName afterBracket
            fnWithGeneric = fn { fnDecorators = [tParamText] }
            (d, i, s) = extractGoDeclsAndStmts afterFn
        in (DeclReceiver rc fnWithGeneric : d, i, s)
      TokIdent mName : afterName ->
        let (fn, afterFn) = parseGoFunctionBody mName afterName
            (d, i, s) = extractGoDeclsAndStmts afterFn
        in (DeclReceiver rc fn : d, i, s)
      _ -> extractGoDeclsAndStmts afterRc

  -- func FunctionName[T any](params) ret { body }
  TokKw "func" : TokIdent fnName : TokSymbol "[" : rest ->
    let (tParams, afterTParams) = span (\t -> t /= TokSymbol "]") rest
        tParamText = "[" <> T.unwords [tokenText t | t <- tParams] <> "]"
        afterBracket = if null afterTParams then [] else tail afterTParams
        (fn, afterFn) = parseGoFunctionBody fnName afterBracket
        fnWithGeneric = fn { fnDecorators = [tParamText] }
        (d, i, s) = extractGoDeclsAndStmts afterFn
    in (DeclFunction fnWithGeneric : d, i, s)

  -- func FunctionName(params) ret { body }
  TokKw "func" : TokIdent fnName : rest ->
    let (fn, afterFn) = parseGoFunctionBody fnName rest
        (d, i, s) = extractGoDeclsAndStmts afterFn
    in (DeclFunction fn : d, i, s)

  -- type Name[T any] struct { ... }
  TokKw "type" : TokIdent stName : TokSymbol "[" : rest ->
    let (tParams, afterTParams) = span (\t -> t /= TokSymbol "]") rest
        tParamText = "[" <> T.unwords [tokenText t | t <- tParams] <> "]"
        afterBracket = if null afterTParams then [] else tail afterTParams
    in case afterBracket of
      TokKw "struct" : afterStruct ->
        let afterBrace = dropWhile (\tok -> tok /= TokSymbol "{") afterStruct
            (fieldsToks, afterBody) = extractBalancedBraces afterBrace
            fields = parseStructFields fieldsToks
            st = Struct stName fields [Function tParamText [] Nothing [] [] False] "pub"
            (d, i, s) = extractGoDeclsAndStmts afterBody
        in (DeclStruct st : d, i, s)
      _ -> extractGoDeclsAndStmts afterBracket

  -- type Name struct { ... }
  TokKw "type" : TokIdent stName : TokKw "struct" : rest ->
    let afterBrace = dropWhile (\tok -> tok /= TokSymbol "{") rest
        (fieldsToks, afterBody) = extractBalancedBraces afterBrace
        fields = parseStructFields fieldsToks
        st = Struct stName fields [] "pub"
        (d, i, s) = extractGoDeclsAndStmts afterBody
    in (DeclStruct st : d, i, s)

  -- type Name interface { ... }
  TokKw "type" : TokIdent ifName : TokKw "interface" : rest ->
    let afterBrace = dropWhile (\tok -> tok /= TokSymbol "{") rest
        (_, afterBody) = extractBalancedBraces afterBrace
        iface = Interface ifName [] []
        (d, i, s) = extractGoDeclsAndStmts afterBody
    in (DeclInterface iface : d, i, s)

  -- type Name = Original
  TokKw "type" : TokIdent aliasName : TokSymbol "=" : TokIdent orig : rest ->
    let (d, i, s) = extractGoDeclsAndStmts rest
    in (DeclTypeAlias aliasName (Just orig) : d, i, s)

  -- factored var ( ... ) and const ( ... )
  TokKw kw : TokSymbol "(" : rest | kw == "var" || kw == "const" ->
    let (blockToks, afterParen) = span (\t -> t /= TokSymbol ")") rest
        remaining = if null afterParen then [] else tail afterParen
        blockStmts = parseFactoredVarBlock blockToks
        (d, i, s) = extractGoDeclsAndStmts remaining
    in (d, i, blockStmts ++ s)

  -- single var / const declaration at top level
  TokKw kw : TokIdent name : rest | kw == "var" || kw == "const" ->
    let (stmt, afterStmt) = parseGoVarDecl name rest
        (d, i, s) = extractGoDeclsAndStmts afterStmt
    in (d, i, stmt : s)

  t : ts ->
    let (stmt, rest) = parseGoSingleStmt (t:ts)
        (d, i, s) = extractGoDeclsAndStmts rest
    in (d, i, maybe [] pure stmt ++ s)

parseFactoredVarBlock :: [GoToken] -> [Stmt]
parseFactoredVarBlock [] = []
parseFactoredVarBlock (TokIdent name : rest) =
  let (stmt, afterStmt) = parseGoVarDecl name rest
  in stmt : parseFactoredVarBlock afterStmt
parseFactoredVarBlock (_ : rest) = parseFactoredVarBlock rest

parseGoVarDecl :: Text -> [GoToken] -> (Stmt, [GoToken])
parseGoVarDecl name tokens =
  case tokens of
    TokSymbol "=" : rest ->
      let (expr, afterExpr) = parseGoSimpleExpr rest
      in (StmtAssign [ExprId name] expr, afterExpr)
    TokSymbol ":=" : rest ->
      let (expr, afterExpr) = parseGoSimpleExpr rest
      in (StmtAssign [ExprId name] expr, afterExpr)
    TokIdent ty : TokSymbol "=" : rest ->
      let (expr, afterExpr) = parseGoSimpleExpr rest
      in (StmtAnnAssign (ExprId name) (ExprId ty) (Just expr), afterExpr)
    TokIdent ty : rest ->
      (StmtAnnAssign (ExprId name) (ExprId ty) Nothing, rest)
    _ ->
      (StmtAssign [ExprId name] (ExprLit LitNone), tokens)

parseGoImports :: [GoToken] -> ([ImportDecl], [GoToken])
parseGoImports (TokSymbol "(" : rest) =
  let (impToks, afterParen) = span (\tok -> tok /= TokSymbol ")") rest
      imps = [ ImportModule path alias
             | (path, alias) <- extractImportPairs impToks
             ]
      remaining = if null afterParen then [] else tail afterParen
  in (imps, remaining)
parseGoImports (TokStr path : rest) =
  ([ImportModule path Nothing], rest)
parseGoImports (TokIdent alias : TokStr path : rest) =
  ([ImportModule path (Just alias)], rest)
parseGoImports tokens = ([], tokens)

extractImportPairs :: [GoToken] -> [(Text, Maybe Text)]
extractImportPairs [] = []
extractImportPairs (TokStr path : rest) = (path, Nothing) : extractImportPairs rest
extractImportPairs (TokIdent alias : TokStr path : rest) = (path, Just alias) : extractImportPairs rest
extractImportPairs (_:rest) = extractImportPairs rest

parseReceiver :: [GoToken] -> (Receiver, [GoToken])
parseReceiver tokens =
  let (rcToks, afterParen) = span (\t -> t /= TokSymbol ")") tokens
      remaining = if null afterParen then [] else tail afterParen
  in case rcToks of
      (TokIdent v : TokSymbol "*" : TokIdent ty : rest) ->
        let gen = if null rest then "" else T.concat [tokenText t | t <- rest]
        in (Receiver v (ty <> gen) True, remaining)
      (TokIdent v : TokIdent ty : rest) ->
        let gen = if null rest then "" else T.concat [tokenText t | t <- rest]
        in (Receiver v (ty <> gen) False, remaining)
      (TokSymbol "*" : TokIdent ty : rest) ->
        let gen = if null rest then "" else T.concat [tokenText t | t <- rest]
        in (Receiver "" (ty <> gen) True, remaining)
      (TokIdent ty : rest) ->
        let gen = if null rest then "" else T.concat [tokenText t | t <- rest]
        in (Receiver "" (ty <> gen) False, remaining)
      _ -> (Receiver "" "" False, remaining)

parseGoFunctionBody :: Text -> [GoToken] -> (Function, [GoToken])
parseGoFunctionBody name tokens =
  let (params, afterParams) = parseGoParamList tokens
      (retType, afterRet) = parseGoReturnType afterParams
      afterBrace = dropWhile (\t -> t /= TokSymbol "{") afterRet
      (bodyToks, afterBody) = extractBalancedBraces afterBrace
      bodyStmts = parseGoBodyStmts bodyToks
      fn = Function name params retType [] bodyStmts False
  in (fn, afterBody)

parseGoParamList :: [GoToken] -> ([Parameter], [GoToken])
parseGoParamList (TokSymbol "(" : rest) =
  let (pToks, afterParen) = span (\t -> t /= TokSymbol ")") rest
      params = extractGoParams pToks
      remaining = if null afterParen then [] else tail afterParen
  in (params, remaining)
parseGoParamList tokens = ([], tokens)

extractGoParams :: [GoToken] -> [Parameter]
extractGoParams [] = []
extractGoParams (TokIdent pName : xs) =
  let (tyToks, rest) = consumeGoType xs
      tyStr = if null tyToks then Nothing else Just (T.concat [tokenText t | t <- tyToks])
      remToks = dropWhile (\t -> t == TokSymbol ",") rest
  in Parameter pName ParamPositional Nothing tyStr : extractGoParams remToks
extractGoParams (_:rest) = extractGoParams rest

parseGoReturnType :: [GoToken] -> (Maybe Text, [GoToken])
parseGoReturnType tokens =
  let (tyToks, rest) = consumeGoType tokens
  in if null tyToks
     then (Nothing, tokens)
     else (Just (T.concat [tokenText t | t <- tyToks]), rest)

parseStructFields :: [GoToken] -> [(Text, Maybe Text)]
parseStructFields [] = []
parseStructFields (TokIdent fName : xs) =
  let (tyToks, afterField) = consumeGoType xs
      tyStr = if null tyToks then Nothing else Just (T.concat [tokenText t | t <- tyToks])
      remToks = dropWhile (\t -> t == TokSymbol ";" || t == TokSymbol ",") afterField
  in (fName, tyStr) : parseStructFields remToks
parseStructFields (_:rest) = parseStructFields rest

consumeGoType :: [GoToken] -> ([GoToken], [GoToken])
consumeGoType (TokSymbol "*" : rest) =
  let (t, r) = consumeGoType rest
  in (TokSymbol "*" : t, r)
consumeGoType (TokSymbol "[" : TokSymbol "]" : rest) =
  let (t, r) = consumeGoType rest
  in (TokSymbol "[" : TokSymbol "]" : t, r)
consumeGoType (TokKw "chan" : rest) =
  let (t, r) = consumeGoType rest
  in (TokKw "chan" : t, r)
consumeGoType (TokKw "map" : TokSymbol "[" : rest) =
  let (keyToks, afterKey) = span (\t -> t /= TokSymbol "]") rest
      afterClose = if null afterKey then [] else tail afterKey
      (valToks, afterVal) = consumeGoType afterClose
  in (TokKw "map" : TokSymbol "[" : keyToks ++ [TokSymbol "]"] ++ valToks, afterVal)
consumeGoType (TokIdent ty : rest) = ([TokIdent ty], rest)
consumeGoType ts = ([], ts)

tokenText :: GoToken -> Text
tokenText (TokIdent t)  = t
tokenText (TokKw t)     = t
tokenText (TokSymbol t) = t
tokenText (TokStr t)    = "\"" <> t <> "\""
tokenText (TokNum n)    = T.pack (show n)
tokenText (TokFloat f)  = T.pack (show f)

extractBalancedBraces :: [GoToken] -> ([GoToken], [GoToken])
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

parseGoBodyStmts :: [GoToken] -> [Stmt]
parseGoBodyStmts [] = []
parseGoBodyStmts (TokKw "return" : rest) =
  let (expr, afterExpr) = parseGoSimpleExpr rest
  in StmtReturn (Just expr) : parseGoBodyStmts afterExpr
parseGoBodyStmts (TokKw "go" : rest) =
  let (expr, afterExpr) = parseGoSimpleExpr rest
  in StmtGo expr : parseGoBodyStmts afterExpr
parseGoBodyStmts (TokKw "defer" : rest) =
  let (expr, afterExpr) = parseGoSimpleExpr rest
  in StmtDefer expr : parseGoBodyStmts afterExpr
parseGoBodyStmts (TokKw "switch" : rest) =
  let (switchStmt, afterSwitch) = parseGoSwitch rest
  in maybe [] pure switchStmt ++ parseGoBodyStmts afterSwitch
parseGoBodyStmts (_:rest) = parseGoBodyStmts rest

parseGoSwitch :: [GoToken] -> (Maybe Stmt, [GoToken])
parseGoSwitch tokens =
  let (targetToks, afterTarget) = span (\t -> t /= TokSymbol "{") tokens
      (bodyToks, afterBody) = extractBalancedBraces afterTarget
      targetExpr = case targetToks of
        [TokIdent name] -> ExprId name
        _               -> ExprLit LitNone
      (cases, defStmts) = parseGoSwitchCases bodyToks
  in (Just (StmtSwitch targetExpr cases defStmts), afterBody)

-- | Unrolls comma-separated multi-type switch cases into individual branch targets (BUG-09).
parseGoSwitchCases :: [GoToken] -> ([(Expr, [Stmt])], [Stmt])
parseGoSwitchCases [] = ([], [])
parseGoSwitchCases (TokKw "case" : rest) =
  let (caseHead, afterColon) = span (\t -> t /= TokSymbol ":") rest
      remaining = if null afterColon then [] else tail afterColon
      (caseStmts, nextCases) = span (\t -> t /= TokKw "case" && t /= TokKw "default") remaining
      stmts = parseGoBodyStmts caseStmts
      (otherCases, defStmts) = parseGoSwitchCases nextCases
      -- Extract all comma-separated case labels (e.g. case int, int64, string:)
      caseExprs = extractCaseLabels caseHead
      unrolled = [(cExpr, stmts) | cExpr <- caseExprs]
  in (unrolled ++ otherCases, defStmts)
parseGoSwitchCases (TokKw "default" : TokSymbol ":" : rest) =
  let (defStmtsToks, nextCases) = span (\t -> t /= TokKw "case") rest
      stmts = parseGoBodyStmts defStmtsToks
      (otherCases, _) = parseGoSwitchCases nextCases
  in (otherCases, stmts)
parseGoSwitchCases (_:rest) = parseGoSwitchCases rest

extractCaseLabels :: [GoToken] -> [Expr]
extractCaseLabels [] = []
extractCaseLabels (TokIdent name : rest) =
  ExprId name : extractCaseLabels (dropWhile (\t -> t == TokSymbol ",") rest)
extractCaseLabels (TokNum n : rest) =
  ExprLit (LitInt n) : extractCaseLabels (dropWhile (\t -> t == TokSymbol ",") rest)
extractCaseLabels (TokStr s : rest) =
  ExprLit (LitString s) : extractCaseLabels (dropWhile (\t -> t == TokSymbol ",") rest)
extractCaseLabels (_:rest) = extractCaseLabels rest

parseGoSingleStmt :: [GoToken] -> (Maybe Stmt, [GoToken])
parseGoSingleStmt (TokKw "return" : rest) =
  let (expr, afterExpr) = parseGoSimpleExpr rest
  in (Just (StmtReturn (Just expr)), afterExpr)
parseGoSingleStmt (TokKw "go" : rest) =
  let (expr, afterExpr) = parseGoSimpleExpr rest
  in (Just (StmtGo expr), afterExpr)
parseGoSingleStmt (TokKw "defer" : rest) =
  let (expr, afterExpr) = parseGoSimpleExpr rest
  in (Just (StmtDefer expr), afterExpr)
parseGoSingleStmt (TokKw "switch" : rest) =
  parseGoSwitch rest
parseGoSingleStmt (TokIdent n1 : TokSymbol "," : TokIdent n2 : TokSymbol ":=" : rest) =
  let (expr, afterExpr) = parseGoSimpleExpr rest
  in (Just (StmtAssign [ExprId n1, ExprId n2] expr), afterExpr)
parseGoSingleStmt (TokIdent n1 : TokSymbol "," : TokIdent n2 : TokSymbol "=" : rest) =
  let (expr, afterExpr) = parseGoSimpleExpr rest
  in (Just (StmtAssign [ExprId n1, ExprId n2] expr), afterExpr)
parseGoSingleStmt (TokIdent name : TokSymbol ":=" : rest) =
  let (expr, afterExpr) = parseGoSimpleExpr rest
  in (Just (StmtAssign [ExprId name] expr), afterExpr)
parseGoSingleStmt (TokIdent name : TokSymbol "=" : rest) =
  let (expr, afterExpr) = parseGoSimpleExpr rest
  in (Just (StmtAssign [ExprId name] expr), afterExpr)
parseGoSingleStmt (TokIdent name : TokSymbol "(" : rest) =
  let afterParen = dropWhile (\t -> t /= TokSymbol ")") rest
      nextToks = if null afterParen then [] else tail afterParen
  in (Just (StmtExpr (ExprCall (ExprId name) [] [])), nextToks)
parseGoSingleStmt (_:rest) = (Nothing, rest)
parseGoSingleStmt [] = (Nothing, [])

parseGoSimpleExpr :: [GoToken] -> (Expr, [GoToken])
parseGoSimpleExpr tokens =
  let (lhs, rest) = parseGoPrimaryExpr tokens
  in case rest of
    TokSymbol op : afterOp | op `elem` ["+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">="] ->
      let (rhs, remToks) = parseGoSimpleExpr afterOp
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

parseGoPrimaryExpr :: [GoToken] -> (Expr, [GoToken])
parseGoPrimaryExpr (TokIdent name : TokSymbol "(" : rest) =
  let afterParen = dropWhile (\t -> t /= TokSymbol ")") rest
      nextToks = if null afterParen then [] else tail afterParen
  in (ExprCall (ExprId name) [] [], nextToks)
parseGoPrimaryExpr (TokNum n : rest) = (ExprLit (LitInt n), rest)
parseGoPrimaryExpr (TokFloat f : rest) = (ExprLit (LitFloat f), rest)
parseGoPrimaryExpr (TokStr s : rest) = (ExprLit (LitString s), rest)
parseGoPrimaryExpr (TokIdent name : rest) = (ExprId name, rest)
parseGoPrimaryExpr tokens = (ExprLit LitNone, tokens)
