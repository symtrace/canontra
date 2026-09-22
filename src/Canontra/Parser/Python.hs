{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Canontra.Parser.Python
Description : High-performance zero-span Python 3.8+ AST parser.

Translates modern Python source code directly into canontra's unified IR
without source-span leakage, supporting functions, PEP 484 type annotations,
PEP 492 async/await, generators/yield, classes/methods/decorators, PEP 572 walrus,
f-strings, slices, comprehensions, and structured parse error diagnostics.
-}
module Canontra.Parser.Python
  ( parsePythonSource
  , tokenizePython
  , advanceColumn
  ) where

import Control.DeepSeq (NFData)
import Data.Char (digitToInt, isAlpha, isAlphaNum, isDigit, isHexDigit, isOctDigit, isSpace)
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

-- | Main entry point: parses a Python source string into a 'Program'.
parsePythonSource :: FilePath -> Text -> Either ParseError Program
parsePythonSource filePath input =
  let cleanInput = canonicalizeText input
  in case tokenizePython cleanInput of
      Left (line, col, msg) ->
        Left (ParseError filePath line col (T.pack msg))
      Right tokens ->
        case parsePythonTopLevel filePath tokens of
          Left err -> Left err
          Right (decls, imps, stmts) ->
            let modul = Module
                  { modName         = T.pack filePath
                  , modImports      = imps
                  , modDeclarations = decls
                  , modStatements   = stmts
                  }
            in Right (Program [modul] "python")

-- ============================================================================
-- Lexer Types & Token Definition
-- ============================================================================

data PyToken
  = TokIdent Text
  | TokKw Text
  | TokNum Integer
  | TokFloat Double
  | TokStr Text
  | TokBytes Text
  | TokFStr [FStringPart]
  | TokSymbol Text
  | TokNewline
  | TokIndent
  | TokDedent
  | TokEOF
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data LocatedToken = LocatedToken
  { ltToken :: PyToken
  , ltLine  :: Int
  , ltCol   :: Int
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Advance visual column according to PEP 8 / POSIX standard tab stops (every 8 columns).
{-# INLINE advanceColumn #-}
advanceColumn :: Int -> Char -> Int
advanceColumn !col '\t' = ((col `div` 8) + 1) * 8
advanceColumn !col _    = col + 1

-- | Lex a Python source code text into located tokens with indentation tracking.
tokenizePython :: Text -> Either (Int, Int, String) [LocatedToken]
tokenizePython input =
  let rawLines = zip [1..] (T.lines input)
  in processLines 1 [0] rawLines 0 Nothing
  where
    processLines _ indentStack [] _ _ =
      let dedents = [LocatedToken TokDedent 1 1 | _ <- drop 1 indentStack]
          eofTok = [LocatedToken TokEOF 1 1]
      in Right (dedents ++ eofTok)

    processLines lineNum indentStack ((lNum, lineText):rest) parenDepth (Just q) =
      let tq = T.replicate 3 (T.singleton q)
      in case T.breakOn tq lineText of
        (_, restTQ)
          | T.null restTQ ->
              -- Still inside triple quote across this entire line
              processLines (lineNum + 1) indentStack rest parenDepth (Just q)
          | otherwise ->
              -- Triple quote ends on this line
              let afterTQ = T.drop 3 restTQ
                  (indent, nonSpace) = T.span (\c -> c == ' ' || c == '\t') afterTQ
                  isCommentOrBlank = T.null nonSpace || T.isPrefixOf "#" nonSpace
              in if isCommentOrBlank
                 then processLines (lineNum + 1) indentStack rest parenDepth Nothing
                 else
                   case lexLine lNum (T.length lineText - T.length nonSpace + 1) nonSpace parenDepth of
                     Left err -> Left err
                     Right (lineTokens, newParenDepth, newOpenTQ) ->
                       case processLines (lNum + 1) indentStack rest newParenDepth newOpenTQ of
                         Left err -> Left err
                         Right nextTokens ->
                           let finalLineTokens =
                                 if newParenDepth == 0 && not (null lineTokens)
                                 then lineTokens ++ [LocatedToken TokNewline lNum (T.length lineText + 1)]
                                 else lineTokens
                           in Right (finalLineTokens ++ nextTokens)

    processLines lineNum indentStack ((lNum, lineText):rest) parenDepth Nothing
      | T.isSuffixOf "\\" (T.stripEnd lineText) =
          let stripped = T.dropEnd 1 (T.stripEnd lineText)
          in case rest of
            ((_, nextText):restLines) ->
              processLines lineNum indentStack ((lNum, stripped <> " " <> nextText) : restLines) parenDepth Nothing
            [] ->
              processLines lineNum indentStack [(lNum, stripped)] parenDepth Nothing
      | otherwise =
          let (indent, nonSpace) = T.span (\c -> c == ' ' || c == '\t') lineText
              indentWidth = T.foldl' advanceColumn 0 indent
              isCommentOrBlank = T.null nonSpace || T.isPrefixOf "#" nonSpace
          in if isCommentOrBlank
             then processLines (lineNum + 1) indentStack rest parenDepth Nothing
             else
               -- Indentation changes only occur when not inside parentheses/brackets
               let (newStack, indentTokens) =
                     if parenDepth == 0
                     then handleIndent lNum indentWidth indentStack
                     else (indentStack, [])
               in case lexLine lNum (indentWidth + 1) nonSpace parenDepth of
                    Left err -> Left err
                    Right (lineTokens, newParenDepth, newOpenTQ) ->
                      case processLines (lNum + 1) newStack rest newParenDepth newOpenTQ of
                        Left err -> Left err
                        Right nextTokens ->
                          let finalLineTokens =
                                if newParenDepth == 0 && not (null lineTokens)
                                then lineTokens ++ [LocatedToken TokNewline lNum (T.length lineText + 1)]
                                else lineTokens
                          in Right (indentTokens ++ finalLineTokens ++ nextTokens)

    handleIndent lNum currentIndent stack@(top:_)
      | currentIndent > top =
          (currentIndent : stack, [LocatedToken TokIndent lNum 1])
      | currentIndent < top =
          let (popped, remaining) = span (> currentIndent) stack
              dedents = [LocatedToken TokDedent lNum 1 | _ <- popped]
          in (remaining, dedents)
      | otherwise = (stack, [])
    handleIndent lNum currentIndent [] =
      ([currentIndent], [LocatedToken TokIndent lNum 1])

lexLine :: Int -> Int -> Text -> Int -> Either (Int, Int, String) ([LocatedToken], Int, Maybe Char)
lexLine lineNum startCol text initDepth = go startCol text initDepth []
  where
    go _ t depth acc | T.null t = Right (reverse acc, depth, Nothing)
    go col t depth acc =
      let c = T.head t
          cs = T.tail t
      in case c of
        ' '  -> go (col + 1) cs depth acc
        '\t' -> go (advanceColumn col '\t') cs depth acc
        '#'  -> Right (reverse acc, depth, Nothing)
        '\\' -> go (col + 1) (T.dropWhile isSpace cs) depth acc
        '('  -> go (col + 1) cs (depth + 1) (LocatedToken (TokSymbol "(") lineNum col : acc)
        '['  -> go (col + 1) cs (depth + 1) (LocatedToken (TokSymbol "[") lineNum col : acc)
        '{'  -> go (col + 1) cs (depth + 1) (LocatedToken (TokSymbol "{") lineNum col : acc)
        ')'  -> go (col + 1) cs (max 0 (depth - 1)) (LocatedToken (TokSymbol ")") lineNum col : acc)
        ']'  -> go (col + 1) cs (max 0 (depth - 1)) (LocatedToken (TokSymbol "]") lineNum col : acc)
        '}'  -> go (col + 1) cs (max 0 (depth - 1)) (LocatedToken (TokSymbol "}") lineNum col : acc)
        _ | isMultiCharSymbol t ->
            let (sym, rest) = extractMultiCharSymbol t
                len = T.length sym
            in go (col + len) rest depth (LocatedToken (TokSymbol sym) lineNum col : acc)
        _ | c `elem` (":,;.~@`" :: String) ->
            go (col + 1) cs depth (LocatedToken (TokSymbol (T.singleton c)) lineNum col : acc)
        _ | c `elem` ("+-*/%^&|<>!=~" :: String) ->
            go (col + 1) cs depth (LocatedToken (TokSymbol (T.singleton c)) lineNum col : acc)
        _ | c == '"' || c == '\'' ->
            case lexStringLit c t of
              Left err -> Left (lineNum, col, err)
              Right (strTok, rest, len, isOpen) ->
                let acc' = LocatedToken strTok lineNum col : acc
                in if isOpen
                   then Right (reverse acc', depth, Just c)
                   else go (col + len) rest depth acc'
        _ | (c == 'f' || c == 'F') && (T.isPrefixOf "\"" cs || T.isPrefixOf "'" cs) ->
            case lexFStringLit (T.head cs) cs of
              Left err -> Left (lineNum, col, err)
              Right (fstrTok, rest, len) ->
                go (col + len + 1) rest depth (LocatedToken fstrTok lineNum col : acc)
        _ | (c == 'r' || c == 'R' || c == 'b' || c == 'B') && (T.isPrefixOf "\"" cs || T.isPrefixOf "'" cs) ->
            case lexStringLit (T.head cs) cs of
              Left err -> Left (lineNum, col, err)
              Right (strTok, rest, len, isOpen) ->
                let tok = case strTok of
                      TokStr s | c == 'b' || c == 'B' -> TokBytes s
                      _ -> strTok
                    acc' = LocatedToken tok lineNum col : acc
                in if isOpen
                   then Right (reverse acc', depth, Just (T.head cs))
                   else go (col + len + 1) rest depth acc'
        _ | isDigit c ->
            let (numTok, rest, len) = lexNumber t
            in go (col + len) rest depth (LocatedToken numTok lineNum col : acc)
        _ | isAlpha c || c == '_' ->
            let (ident, rest) = T.span (\x -> isAlphaNum x || x == '_') t
                len = T.length ident
                tok = if isPyKeyword ident then TokKw ident else TokIdent ident
            in go (col + len) rest depth (LocatedToken tok lineNum col : acc)
        _ ->
            Left (lineNum, col, "Unexpected character: " ++ [c])

isMultiCharSymbol :: Text -> Bool
isMultiCharSymbol t =
  any (`T.isPrefixOf` t)
    [ "->", ":=", "==", "!=", "<=", ">=", "+=", "-=", "*=", "/=", "//="
    , "%=", "**=", "&=", "|=", "^=", "<<=", ">>=", "@="
    , "//", "**", "<<", ">>", "..."
    ]

extractMultiCharSymbol :: Text -> (Text, Text)
extractMultiCharSymbol t =
  let candidates =
        [ "->", ":=", "==", "!=", "<=", ">=", "+=", "-=", "*=", "/=", "//="
        , "%=", "**=", "&=", "|=", "^=", "<<=", ">>=", "@="
        , "//", "**", "<<", ">>", "..."
        ]
  in case filter (`T.isPrefixOf` t) candidates of
      (m:_) -> (m, T.drop (T.length m) t)
      []    -> (T.take 1 t, T.drop 1 t)

lexStringLit :: Char -> Text -> Either String (PyToken, Text, Int, Bool)
lexStringLit quoteChar t
  | T.isPrefixOf (T.replicate 3 (T.singleton quoteChar)) t =
      -- Triple-quoted string
      let bodyWithPrefix = T.drop 3 t
          tripleQuote = T.replicate 3 (T.singleton quoteChar)
      in case T.breakOn tripleQuote bodyWithPrefix of
          (content, rest)
            | T.null rest ->
                -- Single-line triple quote without close in this line (opened multi-line string)
                Right (TokStr content, "", T.length t, True)
            | otherwise ->
                Right (TokStr content, T.drop 3 rest, T.length content + 6, False)
  | otherwise =
      let body = T.drop 1 t
          (content, rest) = parseQuotedBody quoteChar body ""
      in if T.null rest && not (T.isPrefixOf (T.singleton quoteChar) body) && not (T.null body)
         then Right (TokStr content, "", T.length t, False)
         else Right (TokStr content, T.drop 1 rest, T.length content + 2, False)

parseQuotedBody :: Char -> Text -> Text -> (Text, Text)
parseQuotedBody _ t acc | T.null t = (acc, "")
parseQuotedBody q t acc =
  let c = T.head t
      cs = T.tail t
  in if c == q
     then (acc, t)
     else if c == '\\' && not (T.null cs)
          then let escChar = case T.head cs of
                     'n' -> '\n'
                     't' -> '\t'
                     'r' -> '\r'
                     '\\' -> '\\'
                     '\'' -> '\''
                     '"' -> '"'
                     other -> other
               in parseQuotedBody q (T.tail cs) (acc `T.snoc` escChar)
          else parseQuotedBody q cs (acc `T.snoc` c)

lexFStringLit :: Char -> Text -> Either String (PyToken, Text, Int)
lexFStringLit quoteChar t =
  let isTriple = T.isPrefixOf (T.replicate 3 (T.singleton quoteChar)) t
      prefixLen = if isTriple then 3 else 1
      body = T.drop prefixLen t
      (parts, rest, consumedLen) = scanFStringBody quoteChar isTriple body (prefixLen + prefixLen)
  in Right (TokFStr parts, rest, consumedLen)

scanFStringBody :: Char -> Bool -> Text -> Int -> ([FStringPart], Text, Int)
scanFStringBody quoteChar isTriple input initialLen = go input "" [] initialLen
  where
    tripleQuote = T.replicate 3 (T.singleton quoteChar)

    go t textAcc partsAcc len
      | T.null t =
          let finalParts = if T.null textAcc then reverse partsAcc else reverse (FStringText textAcc : partsAcc)
          in (finalParts, "", len)
      | isTriple && T.isPrefixOf tripleQuote t =
          let finalParts = if T.null textAcc then reverse partsAcc else reverse (FStringText textAcc : partsAcc)
          in (finalParts, T.drop 3 t, len + T.length textAcc)
      | not isTriple && T.head t == quoteChar =
          let finalParts = if T.null textAcc then reverse partsAcc else reverse (FStringText textAcc : partsAcc)
          in (finalParts, T.tail t, len + T.length textAcc)
      | T.isPrefixOf "{{" t =
          go (T.drop 2 t) (textAcc `T.snoc` '{') partsAcc (len + 2)
      | T.isPrefixOf "}}" t =
          go (T.drop 2 t) (textAcc `T.snoc` '}') partsAcc (len + 2)
      | T.head t == '{' =
          let textParts = if T.null textAcc then partsAcc else FStringText textAcc : partsAcc
              (exprStr, afterExpr, exprLen) = scanFStringExpr (T.tail t)
              exprPart = FStringExpr (ExprId exprStr) Nothing Nothing
          in go afterExpr "" (exprPart : textParts) (len + 1 + exprLen)
      | T.head t == '\\' && T.length t > 1 =
          let esc = T.take 2 t
          in go (T.drop 2 t) (textAcc <> esc) partsAcc (len + 2)
      | otherwise =
          go (T.tail t) (textAcc `T.snoc` T.head t) partsAcc (len + 1)

    scanFStringExpr t = scanExprDepth (1 :: Int) t "" 0
      where
        scanExprDepth 0 remToks acc l = (acc, remToks, l)
        scanExprDepth _ remToks acc l | T.null remToks = (acc, "", l)
        scanExprDepth d remToks acc l =
          let c = T.head remToks
              cs = T.tail remToks
          in case c of
            '{' -> scanExprDepth (d + 1) cs (acc `T.snoc` c) (l + 1)
            '}' ->
                if d == 1
                then (acc, cs, l + 1)
                else scanExprDepth (d - 1) cs (acc `T.snoc` c) (l + 1)
            '"' ->
                let (strBody, rest) = scanInnerString '"' cs
                in scanExprDepth d rest (acc `T.snoc` '"' <> strBody `T.snoc` '"') (l + 2 + T.length strBody)
            '\'' ->
                let (strBody, rest) = scanInnerString '\'' cs
                in scanExprDepth d rest (acc `T.snoc` '\'' <> strBody `T.snoc` '\'') (l + 2 + T.length strBody)
            '\\' | not (T.null cs) ->
                scanExprDepth d (T.tail cs) (acc `T.snoc` '\\' `T.snoc` T.head cs) (l + 2)
            _   -> scanExprDepth d cs (acc `T.snoc` c) (l + 1)

        scanInnerString q txt =
          let (s, r) = parseQuotedBody q txt ""
          in (s, if T.null r then "" else T.tail r)

lexNumber :: Text -> (PyToken, Text, Int)
lexNumber t
  | T.isPrefixOf "0x" t || T.isPrefixOf "0X" t =
      let hexPart = T.takeWhile isHexDigit (T.drop 2 t)
          val = case TR.hexadecimal hexPart of
                  Right (n, _) -> n
                  Left _       -> 0
          len = 2 + T.length hexPart
      in (TokNum val, T.drop len t, len)
  | T.isPrefixOf "0o" t || T.isPrefixOf "0O" t =
      let octPart = T.takeWhile isOctDigit (T.drop 2 t)
          val = T.foldl' (\acc c -> acc * 8 + fromIntegral (digitToInt c)) 0 octPart
          len = 2 + T.length octPart
      in (TokNum val, T.drop len t, len)
  | T.isPrefixOf "0b" t || T.isPrefixOf "0B" t =
      let binPart = T.takeWhile (\c -> c == '0' || c == '1') (T.drop 2 t)
          val = T.foldl' (\acc c -> acc * 2 + if c == '1' then 1 else 0) 0 binPart
          len = 2 + T.length binPart
      in (TokNum val, T.drop len t, len)
  | otherwise =
      let numStr = T.takeWhile (\c -> isDigit c || c == '.' || c == 'e' || c == 'E' || c == '_') t
          cleanNum = T.filter (/= '_') numStr
          len = T.length numStr
      in if '.' `elem` T.unpack cleanNum || 'e' `elem` T.unpack cleanNum || 'E' `elem` T.unpack cleanNum
         then case TR.double cleanNum of
                Right (d, _) -> (TokFloat d, T.drop len t, len)
                Left _       -> (TokFloat 0.0, T.drop len t, len)
         else case TR.decimal cleanNum of
                Right (n, _) -> (TokNum n, T.drop len t, len)
                Left _       -> (TokNum 0, T.drop len t, len)

isPyKeyword :: Text -> Bool
isPyKeyword k = k `elem`
  [ "False", "None", "True", "and", "as", "assert", "async", "await"
  , "break", "class", "continue", "def", "del", "elif", "else", "except"
  , "finally", "for", "from", "global", "if", "import", "in", "is"
  , "lambda", "nonlocal", "not", "or", "pass", "raise", "return", "try"
  , "while", "with", "yield"
  ]

-- ============================================================================
-- Recursive Descent Parser
-- ============================================================================

parsePythonTopLevel :: FilePath -> [LocatedToken] -> Either ParseError ([Declaration], [ImportDecl], [Stmt])
parsePythonTopLevel fp toks = go [] [] [] (skipNewlines toks)
  where
    go decls imps stmts [] = Right (reverse decls, reverse imps, reverse stmts)
    go decls imps stmts (LocatedToken TokEOF _ _ : _) = Right (reverse decls, reverse imps, reverse stmts)
    go decls imps stmts ts =
      case parseTopStatement fp ts of
        Left err -> Left err
        Right (TopDecl d, rest) -> go (d ++ decls) imps stmts (skipNewlines rest)
        Right (TopImport i, rest) -> go decls (i ++ imps) stmts (skipNewlines rest)
        Right (TopStmt s, rest) -> go decls imps (s ++ stmts) (skipNewlines rest)

data TopItem
  = TopDecl [Declaration]
  | TopImport [ImportDecl]
  | TopStmt [Stmt]
  deriving stock (Show)

skipNewlines :: [LocatedToken] -> [LocatedToken]
skipNewlines = dropWhile (\(LocatedToken t _ _) -> t == TokNewline)

parseTopStatement :: FilePath -> [LocatedToken] -> Either ParseError (TopItem, [LocatedToken])
parseTopStatement fp toks =
  let cleanToks = skipNewlines toks
  in case cleanToks of
      [] -> Right (TopStmt [], [])
      (LocatedToken TokEOF _ _ : rest) -> Right (TopStmt [], rest)

      -- Decorators (@...)
      (LocatedToken (TokSymbol "@") _ _ : _) ->
        parseDecorated fp cleanToks

      -- def / async def
      (LocatedToken (TokKw "async") _ _ : LocatedToken (TokKw "def") _ _ : _) ->
        parseFunctionDecl fp cleanToks [] True
      (LocatedToken (TokKw "def") _ _ : _) ->
        parseFunctionDecl fp cleanToks [] False

      -- class
      (LocatedToken (TokKw "class") _ _ : _) ->
        parseClassDecl fp cleanToks []

      -- imports
      (LocatedToken (TokKw "import") _ _ : _) -> do
        (imps, rest) <- parseImportStmt fp cleanToks
        pure (TopImport imps, rest)
      (LocatedToken (TokKw "from") _ _ : _) -> do
        (imp, rest) <- parseFromImportStmt fp cleanToks
        pure (TopImport [imp], rest)

      -- statements
      _ -> do
        (stmts, rest) <- parseStatement fp cleanToks
        pure (TopStmt stmts, rest)

parseDecorated :: FilePath -> [LocatedToken] -> Either ParseError (TopItem, [LocatedToken])
parseDecorated fp toks = do
  (decs, rest) <- collectDecorators toks []
  let next = skipNewlines rest
  case next of
    (LocatedToken (TokKw "async") _ _ : LocatedToken (TokKw "def") _ _ : _) ->
      parseFunctionDecl fp next decs True
    (LocatedToken (TokKw "def") _ _ : _) ->
      parseFunctionDecl fp next decs False
    (LocatedToken (TokKw "class") _ _ : _) ->
      parseClassDecl fp next decs
    _ ->
      parseErrorAt fp (head next) "Expected function or class after decorator"
  where
    collectDecorators (LocatedToken (TokSymbol "@") _ _ : rest) acc =
      let (decTokens, afterLine) = span (\(LocatedToken t _ _) -> t /= TokNewline && t /= TokEOF) rest
          decStr = "@" <> T.unwords [tokenToText t | LocatedToken t _ _ <- decTokens]
          cleanAfter = skipNewlines afterLine
      in collectDecorators cleanAfter (acc ++ [decStr])
    collectDecorators ts acc = Right (acc, ts)

parseFunctionDecl :: FilePath -> [LocatedToken] -> [Text] -> Bool -> Either ParseError (TopItem, [LocatedToken])
parseFunctionDecl fp toks decs isAsync = do
  -- skip [async] def
  let afterDef = if isAsync then drop 2 toks else drop 1 toks
  case afterDef of
    (LocatedToken (TokIdent name) _ _ : LocatedToken (TokSymbol "(") _ _ : rest) -> do
      (params, afterParams) <- parseParamList fp rest
      (retType, afterRet) <- parseReturnType fp afterParams
      afterColon <- expectSymbol fp ":" afterRet
      (body, afterBody) <- parseSuite fp afterColon
      let fn = Function name params retType decs body isAsync
      pure (TopDecl [DeclFunction fn], afterBody)
    (tok:_) ->
      parseErrorAt fp tok "Expected function name and parameter list in def"
    [] ->
      Left (ParseError fp 1 1 "Unexpected end of input in function declaration")

parseParamList :: FilePath -> [LocatedToken] -> Either ParseError ([Parameter], [LocatedToken])
parseParamList fp toks = go toks []
  where
    go (LocatedToken (TokSymbol ")") _ _ : rest) acc = Right (reverse acc, rest)
    go (LocatedToken (TokSymbol ",") _ _ : rest) acc = go rest acc
    go (LocatedToken (TokSymbol "/") _ _ : rest) acc =
      go rest (Parameter "/" ParamPositionalOnly Nothing Nothing : acc)
    go (LocatedToken (TokSymbol "*") _ _ : LocatedToken (TokIdent name) _ _ : rest) acc = do
      (mTy, r1) <- parseOptionalTypeAnnot fp rest
      go r1 (Parameter name ParamVarArgs Nothing mTy : acc)
    go (LocatedToken (TokSymbol "*") _ _ : rest) acc =
      go rest (Parameter "*" ParamKwArgs Nothing Nothing : acc)
    go (LocatedToken (TokSymbol "**") _ _ : LocatedToken (TokIdent name) _ _ : rest) acc = do
      (mTy, r1) <- parseOptionalTypeAnnot fp rest
      go r1 (Parameter name ParamKwArgs Nothing mTy : acc)
    go (LocatedToken (TokIdent name) _ _ : rest) acc = do
      (mTy, r1) <- parseOptionalTypeAnnot fp rest
      (mDef, r2) <- parseOptionalDefault fp r1
      go r2 (Parameter name ParamPositional mDef mTy : acc)
    go (tok:_) _ =
      parseErrorAt fp tok "Unexpected token in parameter list"
    go [] _ =
      Left (ParseError fp 1 1 "Unclosed parameter list")

parseOptionalTypeAnnot :: FilePath -> [LocatedToken] -> Either ParseError (Maybe Text, [LocatedToken])
parseOptionalTypeAnnot _ (LocatedToken (TokSymbol ":") _ _ : rest) =
  let (typeTokens, afterType) = span (\(LocatedToken t _ _) -> t /= TokSymbol "," && t /= TokSymbol "=" && t /= TokSymbol ")" && t /= TokNewline) rest
      typeStr = T.unwords [tokenToText t | LocatedToken t _ _ <- typeTokens]
  in Right (Just typeStr, afterType)
parseOptionalTypeAnnot _ ts = Right (Nothing, ts)

parseOptionalDefault :: FilePath -> [LocatedToken] -> Either ParseError (Maybe Text, [LocatedToken])
parseOptionalDefault _ (LocatedToken (TokSymbol "=") _ _ : rest) =
  let (valTokens, afterVal) = span (\(LocatedToken t _ _) -> t /= TokSymbol "," && t /= TokSymbol ")" && t /= TokNewline) rest
      valStr = T.unwords [tokenToText t | LocatedToken t _ _ <- valTokens]
  in Right (Just valStr, afterVal)
parseOptionalDefault _ ts = Right (Nothing, ts)

parseReturnType :: FilePath -> [LocatedToken] -> Either ParseError (Maybe Text, [LocatedToken])
parseReturnType _ (LocatedToken (TokSymbol "->") _ _ : rest) =
  let (retTokens, afterRet) = span (\(LocatedToken t _ _) -> t /= TokSymbol ":" && t /= TokNewline) rest
      retStr = T.unwords [tokenToText t | LocatedToken t _ _ <- retTokens]
  in Right (Just retStr, afterRet)
parseReturnType _ ts = Right (Nothing, ts)

parseClassDecl :: FilePath -> [LocatedToken] -> [Text] -> Either ParseError (TopItem, [LocatedToken])
parseClassDecl fp toks decs = do
  let afterClass = drop 1 toks
  case afterClass of
    (LocatedToken (TokIdent name) _ _ : rest) -> do
      (bases, afterBases) <- parseBases fp rest
      afterColon <- expectSymbol fp ":" afterBases
      (bodyDecls, _, afterBody) <- parseClassSuite fp afterColon
      let methods = [fn | DeclFunction fn <- bodyDecls]
          cls = Class name bases methods decs
      pure (TopDecl [DeclClass cls], afterBody)
    (tok:_) ->
      parseErrorAt fp tok "Expected class name after 'class'"
    [] ->
      Left (ParseError fp 1 1 "Unexpected end of input in class declaration")

parseBases :: FilePath -> [LocatedToken] -> Either ParseError ([Text], [LocatedToken])
parseBases fp (LocatedToken (TokSymbol "(") _ _ : rest) = go rest []
  where
    go (LocatedToken (TokSymbol ")") _ _ : r) acc = Right (reverse acc, r)
    go (LocatedToken (TokSymbol ",") _ _ : r) acc = go r acc
    go (LocatedToken (TokIdent base) _ _ : r) acc = go r (base : acc)
    go (LocatedToken (TokSymbol ".") _ _ : LocatedToken (TokIdent sub) _ _ : r) (b:acc) =
      go r ((b <> "." <> sub) : acc)
    go (tok:_) _ = parseErrorAt fp tok "Unexpected token in base class list"
    go [] _ = Left (ParseError fp 1 1 "Unclosed base class list")
parseBases _ ts = Right ([], ts)

parseClassSuite :: FilePath -> [LocatedToken] -> Either ParseError ([Declaration], [ImportDecl], [LocatedToken])
parseClassSuite fp toks =
  let cleanToks = skipNewlines toks
  in case cleanToks of
      (LocatedToken TokIndent _ _ : rest) ->
        collectClassMembers fp rest [] []
      _ ->
        case parseTopStatement fp cleanToks of
          Right (TopDecl d, r) -> Right (d, [], r)
          _ -> Right ([], [], cleanToks)
  where
    collectClassMembers _ (LocatedToken TokDedent _ _ : rest) decls imps =
      Right (reverse decls, reverse imps, rest)
    collectClassMembers _ (LocatedToken TokEOF _ _ : rest) decls imps =
      Right (reverse decls, reverse imps, rest)
    collectClassMembers _ [] decls imps =
      Right (reverse decls, reverse imps, [])
    collectClassMembers fpPath ts decls imps =
      let clean = skipNewlines ts
      in case clean of
          (LocatedToken TokDedent _ _ : rest) ->
            Right (reverse decls, reverse imps, rest)
          _ ->
            case parseTopStatement fpPath clean of
              Left err -> Left err
              Right (TopDecl d, r) -> collectClassMembers fpPath (skipNewlines r) (d ++ decls) imps
              Right (TopImport i, r) -> collectClassMembers fpPath (skipNewlines r) decls (i ++ imps)
              Right (TopStmt _, r) -> collectClassMembers fpPath (skipNewlines r) decls imps

parseSuite :: FilePath -> [LocatedToken] -> Either ParseError ([Stmt], [LocatedToken])
parseSuite fp toks =
  let cleanToks = skipNewlines toks
  in case cleanToks of
      (LocatedToken TokIndent _ _ : rest) ->
        collectSuiteStmts fp rest []
      _ ->
        parseStatement fp cleanToks
  where
    collectSuiteStmts _ (LocatedToken TokDedent _ _ : rest) acc =
      Right (reverse acc, rest)
    collectSuiteStmts _ (LocatedToken TokEOF _ _ : rest) acc =
      Right (reverse acc, rest)
    collectSuiteStmts _ [] acc =
      Right (reverse acc, [])
    collectSuiteStmts fpPath ts acc =
      let clean = skipNewlines ts
      in case clean of
          (LocatedToken TokDedent _ _ : rest) ->
            Right (reverse acc, rest)
          _ ->
            case parseStatement fpPath clean of
              Left err -> Left err
              Right (stmts, r) -> collectSuiteStmts fpPath (skipNewlines r) (reverse stmts ++ acc)

-- ============================================================================
-- Import Parsing
-- ============================================================================

parseImportStmt :: FilePath -> [LocatedToken] -> Either ParseError ([ImportDecl], [LocatedToken])
parseImportStmt fp (LocatedToken (TokKw "import") _ _ : rest) = do
  (imps, after) <- parseImportItems fp rest []
  pure (imps, skipToNewline after)
parseImportStmt fp (tok:_) = parseErrorAt fp tok "Expected 'import'"
parseImportStmt fp [] = Left (ParseError fp 1 1 "Unexpected end of input in import")

parseImportItems :: FilePath -> [LocatedToken] -> [ImportDecl] -> Either ParseError ([ImportDecl], [LocatedToken])
parseImportItems fp toks acc = do
  (modName, afterMod) <- parseDottedName fp toks
  let (mAlias, afterAlias) = case afterMod of
        (LocatedToken (TokKw "as") _ _ : LocatedToken (TokIdent a) _ _ : r) -> (Just a, r)
        _ -> (Nothing, afterMod)
      decl = ImportModule modName mAlias
  case afterAlias of
    (LocatedToken (TokSymbol ",") _ _ : rest) ->
      parseImportItems fp rest (decl : acc)
    _ ->
      Right (reverse (decl : acc), afterAlias)

parseFromImportStmt :: FilePath -> [LocatedToken] -> Either ParseError (ImportDecl, [LocatedToken])
parseFromImportStmt fp (LocatedToken (TokKw "from") _ _ : rest) = do
  (dots, afterDots) <- parseLeadingDots rest ""
  (modName, afterMod) <- if not (null afterDots) && isIdentTok (head afterDots)
                         then parseDottedName fp afterDots
                         else Right ("", afterDots)
  let fullMod = dots <> modName
  afterImport <- expectKw fp "import" afterMod
  case afterImport of
    (LocatedToken (TokSymbol "*") _ _ : r) ->
      Right (ImportFrom fullMod ImportAll, skipToNewline r)
    (LocatedToken (TokSymbol "(") _ _ : r) -> do
      (syms, afterClose) <- parseFromSymbols fp r []
      pure (ImportFrom fullMod (ImportSymbols syms), skipToNewline afterClose)
    _ -> do
      (syms, afterSyms) <- parseFromSymbols fp afterImport []
      pure (ImportFrom fullMod (ImportSymbols syms), skipToNewline afterSyms)
parseFromImportStmt fp (tok:_) = parseErrorAt fp tok "Expected 'from'"
parseFromImportStmt fp [] = Left (ParseError fp 1 1 "Unexpected end of input in from-import")

parseLeadingDots :: [LocatedToken] -> Text -> Either ParseError (Text, [LocatedToken])
parseLeadingDots (LocatedToken (TokSymbol ".") _ _ : rest) acc =
  parseLeadingDots rest (acc <> ".")
parseLeadingDots (LocatedToken (TokSymbol "...") _ _ : rest) acc =
  parseLeadingDots rest (acc <> "...")
parseLeadingDots ts acc = Right (acc, ts)

parseFromSymbols :: FilePath -> [LocatedToken] -> [(Text, Maybe Text)] -> Either ParseError ([(Text, Maybe Text)], [LocatedToken])
parseFromSymbols _ (LocatedToken (TokSymbol ")") _ _ : rest) acc = Right (reverse acc, rest)
parseFromSymbols fp (LocatedToken (TokIdent name) _ _ : rest) acc =
  let (mAlias, afterAlias) = case rest of
        (LocatedToken (TokKw "as") _ _ : LocatedToken (TokIdent a) _ _ : r) -> (Just a, r)
        _ -> (Nothing, rest)
      item = (name, mAlias)
  in case afterAlias of
      (LocatedToken (TokSymbol ",") _ _ : r) -> parseFromSymbols fp r (item : acc)
      _ -> Right (reverse (item : acc), afterAlias)
parseFromSymbols _ ts acc = Right (reverse acc, ts)

parseDottedName :: FilePath -> [LocatedToken] -> Either ParseError (Text, [LocatedToken])
parseDottedName _ (LocatedToken (TokIdent name) _ _ : rest) = go rest name
  where
    go (LocatedToken (TokSymbol ".") _ _ : LocatedToken (TokIdent nextPart) _ _ : r) acc =
      go r (acc <> "." <> nextPart)
    go ts acc = Right (acc, ts)
parseDottedName fp (tok:_) = parseErrorAt fp tok "Expected identifier in module name"
parseDottedName fp [] = Left (ParseError fp 1 1 "Expected module name")

-- ============================================================================
-- Statement Parsing
-- ============================================================================

parseStatement :: FilePath -> [LocatedToken] -> Either ParseError ([Stmt], [LocatedToken])
parseStatement fp toks =
  let cleanToks = skipNewlines toks
  in case cleanToks of
      [] -> Right ([], [])
      (LocatedToken TokEOF _ _ : rest) -> Right ([], rest)

      -- Control Flow
      (LocatedToken (TokKw "return") _ _ : rest) -> do
        let (exprToks, afterExpr) = spanUntilStmtEnd rest
        if null exprToks
          then Right ([StmtReturn Nothing], skipToNewline afterExpr)
          else do
            expr <- parseExpr fp exprToks
            pure ([StmtReturn (Just expr)], skipToNewline afterExpr)

      (LocatedToken (TokKw "pass") _ _ : rest) ->
        Right ([StmtPass], skipToNewline rest)

      (LocatedToken (TokKw "break") _ _ : rest) ->
        Right ([StmtBreak], skipToNewline rest)

      (LocatedToken (TokKw "continue") _ _ : rest) ->
        Right ([StmtContinue], skipToNewline rest)

      (LocatedToken (TokKw "if") _ _ : rest) ->
        parseIfStatement fp rest

      (LocatedToken (TokKw "while") _ _ : rest) -> do
        (cond, afterCond) <- parseExprUntilColon fp rest
        (body, afterBody) <- parseSuite fp afterCond
        (elseSuite, afterElse) <- parseOptionalElse fp afterBody
        pure ([StmtWhile cond body elseSuite], afterElse)

      (LocatedToken (TokKw "for") _ _ : rest) ->
        parseForStatement fp rest False

      (LocatedToken (TokKw "async") _ _ : LocatedToken (TokKw "for") _ _ : rest) ->
        parseForStatement fp rest True

      (LocatedToken (TokKw "try") _ _ : rest) ->
        parseTryStatement fp rest

      (LocatedToken (TokKw "with") _ _ : rest) ->
        parseWithStatement fp rest False

      (LocatedToken (TokKw "async") _ _ : LocatedToken (TokKw "with") _ _ : rest) ->
        parseWithStatement fp rest True

      (LocatedToken (TokKw "assert") _ _ : rest) -> do
        let (exprToks, afterExpr) = spanUntilStmtEnd rest
        expr <- parseExpr fp exprToks
        pure ([StmtAssert expr Nothing], skipToNewline afterExpr)

      (LocatedToken (TokKw "raise") _ _ : rest) -> do
        let (exprToks, afterExpr) = spanUntilStmtEnd rest
        if null exprToks
          then Right ([StmtRaise Nothing Nothing], skipToNewline afterExpr)
          else do
            expr <- parseExpr fp exprToks
            pure ([StmtRaise (Just expr) Nothing], skipToNewline afterExpr)

      (LocatedToken (TokKw "global") _ _ : rest) -> do
        let (idents, after) = parseIdentList rest
        pure ([StmtGlobal idents], skipToNewline after)

      (LocatedToken (TokKw "nonlocal") _ _ : rest) -> do
        let (idents, after) = parseIdentList rest
        pure ([StmtNonlocal idents], skipToNewline after)

      (LocatedToken (TokKw "del") _ _ : rest) -> do
        let (exprToks, after) = spanUntilStmtEnd rest
        expr <- parseExpr fp exprToks
        pure ([StmtDelete [expr]], skipToNewline after)

      (LocatedToken (TokKw "import") _ _ : _) -> do
        (_, after) <- parseImportStmt fp cleanToks
        pure ([], after)

      (LocatedToken (TokKw "from") _ _ : _) -> do
        (_, after) <- parseFromImportStmt fp cleanToks
        pure ([], after)

      (LocatedToken (TokSymbol "@") _ _ : _) -> do
        (_, after) <- parseDecorated fp cleanToks
        pure ([], after)

      (LocatedToken (TokKw "def") _ _ : _) -> do
        (_, after) <- parseFunctionDecl fp cleanToks [] False
        pure ([], after)

      (LocatedToken (TokKw "async") _ _ : LocatedToken (TokKw "def") _ _ : _) -> do
        (_, after) <- parseFunctionDecl fp (drop 1 cleanToks) [] True
        pure ([], after)

      (LocatedToken (TokKw "class") _ _ : _) -> do
        (_, after) <- parseClassDecl fp cleanToks []
        pure ([], after)

      -- Match / Case (Python 3.10+ PEP 634)
      (LocatedToken (TokIdent "match") _ _ : rest) ->
        case tryParseMatchStatement fp rest of
          Just res -> res
          Nothing  -> parseAssignOrExprStmt fp cleanToks

      -- Assignment or Expression statement
      _ -> parseAssignOrExprStmt fp cleanToks

parseIfStatement :: FilePath -> [LocatedToken] -> Either ParseError ([Stmt], [LocatedToken])
parseIfStatement fp toks = do
  (cond, afterCond) <- parseExprUntilColon fp toks
  (body, afterBody) <- parseSuite fp afterCond
  let cleanAfter = skipNewlines afterBody
  case cleanAfter of
    (LocatedToken (TokKw "elif") _ _ : elifRest) -> do
      (elifStmts, finalRest) <- parseIfStatement fp elifRest
      pure ([StmtIf cond body elifStmts], finalRest)
    (LocatedToken (TokKw "else") _ _ : elseRest) -> do
      afterColon <- expectSymbol fp ":" elseRest
      (elseBody, finalRest) <- parseSuite fp afterColon
      pure ([StmtIf cond body elseBody], finalRest)
    _ ->
      pure ([StmtIf cond body []], cleanAfter)

parseForStatement :: FilePath -> [LocatedToken] -> Bool -> Either ParseError ([Stmt], [LocatedToken])
parseForStatement fp toks isAsync = do
  let (targetToks, afterTarget) = span (\(LocatedToken t _ _) -> t /= TokKw "in") toks
  targetExpr <- parseExpr fp targetToks
  let afterIn = drop 1 afterTarget
  (iterExpr, afterColon) <- parseExprUntilColon fp afterIn
  (body, afterBody) <- parseSuite fp afterColon
  (elseSuite, afterElse) <- parseOptionalElse fp afterBody
  let stmt = if isAsync
             then StmtAsyncFor targetExpr iterExpr body elseSuite
             else StmtFor targetExpr iterExpr body elseSuite
  pure ([stmt], afterElse)

parseTryStatement :: FilePath -> [LocatedToken] -> Either ParseError ([Stmt], [LocatedToken])
parseTryStatement fp toks = do
  afterColon <- expectSymbol fp ":" toks
  (body, afterBody) <- parseSuite fp afterColon
  (handlers, afterHandlers) <- parseExceptHandlers fp (skipNewlines afterBody) []
  (elseSuite, afterElse) <- parseOptionalElse fp afterHandlers
  (finalSuite, afterFinal) <- parseOptionalFinally fp afterElse
  pure ([StmtTry body handlers elseSuite finalSuite], afterFinal)

parseExceptHandlers :: FilePath -> [LocatedToken] -> [(Maybe Expr, Maybe Text, [Stmt])] -> Either ParseError ([(Maybe Expr, Maybe Text, [Stmt])], [LocatedToken])
parseExceptHandlers fp (LocatedToken (TokKw "except") _ _ : rest) acc = do
  let (clauseToks, afterClause) = span (\(LocatedToken t _ _) -> t /= TokSymbol ":" && t /= TokNewline) rest
  afterColon <- expectSymbol fp ":" afterClause
  (hBody, afterBody) <- parseSuite fp afterColon
  let (mExc, mAlias) = case clauseToks of
        [] -> (Nothing, Nothing)
        _ ->
          let (excPart, aliasPart) = span (\(LocatedToken t _ _) -> t /= TokKw "as") clauseToks
              aliasName = case aliasPart of
                (LocatedToken (TokKw "as") _ _ : LocatedToken (TokIdent a) _ _ : _) -> Just a
                _ -> Nothing
          in case parseExpr fp excPart of
              Right e -> (Just e, aliasName)
              Left _  -> (Nothing, aliasName)
      handler = (mExc, mAlias, hBody)
  parseExceptHandlers fp (skipNewlines afterBody) (acc ++ [handler])
parseExceptHandlers _ ts acc = Right (acc, ts)

parseOptionalElse :: FilePath -> [LocatedToken] -> Either ParseError ([Stmt], [LocatedToken])
parseOptionalElse fp toks =
  let clean = skipNewlines toks
  in case clean of
      (LocatedToken (TokKw "else") _ _ : rest) -> do
        afterColon <- expectSymbol fp ":" rest
        parseSuite fp afterColon
      _ -> Right ([], clean)

parseOptionalFinally :: FilePath -> [LocatedToken] -> Either ParseError ([Stmt], [LocatedToken])
parseOptionalFinally fp toks =
  let clean = skipNewlines toks
  in case clean of
      (LocatedToken (TokKw "finally") _ _ : rest) -> do
        afterColon <- expectSymbol fp ":" rest
        parseSuite fp afterColon
      _ -> Right ([], clean)

parseWithStatement :: FilePath -> [LocatedToken] -> Bool -> Either ParseError ([Stmt], [LocatedToken])
parseWithStatement fp toks isAsync = do
  (items, afterColon) <- parseWithItems fp toks []
  (body, afterBody) <- parseSuite fp afterColon
  let stmt = if isAsync then StmtAsyncWith items body else StmtWith items body
  pure ([stmt], afterBody)

parseWithItems :: FilePath -> [LocatedToken] -> [(Expr, Maybe Expr)] -> Either ParseError ([(Expr, Maybe Expr)], [LocatedToken])
parseWithItems fp toks acc = do
  let (itemToks, afterItem) = span (\(LocatedToken t _ _) -> t /= TokSymbol "," && t /= TokSymbol ":" && t /= TokNewline) toks
      (exprPart, aliasPart) = span (\(LocatedToken t _ _) -> t /= TokKw "as") itemToks
  itemExpr <- parseExpr fp exprPart
  let mAliasExpr = case aliasPart of
        (LocatedToken (TokKw "as") _ _ : restAlias) ->
          case parseExpr fp restAlias of
            Right ae -> Just ae
            Left _   -> Nothing
        _ -> Nothing
      item = (itemExpr, mAliasExpr)
  case afterItem of
    (LocatedToken (TokSymbol ",") _ _ : rest) ->
      parseWithItems fp rest (acc ++ [item])
    (LocatedToken (TokSymbol ":") _ _ : rest) ->
      Right (acc ++ [item], rest)
    _ ->
      Right (acc ++ [item], afterItem)

-- | Attempt to parse a Python 3.10+ match/case statement suite.
tryParseMatchStatement :: FilePath -> [LocatedToken] -> Maybe (Either ParseError ([Stmt], [LocatedToken]))
tryParseMatchStatement fp toks =
  let (subjectToks, atColon) = spanUntilColonDepth toks
  in case atColon of
    (LocatedToken (TokSymbol ":") _ _ : afterColon) ->
      let cleanAfterColon = skipNewlines afterColon
      in case cleanAfterColon of
        (LocatedToken TokIndent _ _ : insideBlock) ->
          let cleanBlock = skipNewlines insideBlock
          in case cleanBlock of
            (LocatedToken (TokIdent "case") _ _ : _) ->
              Just $ do
                subjectExpr <- parseExpr fp subjectToks
                (cases, afterCases) <- parseMatchCases fp insideBlock []
                pure ([StmtMatch subjectExpr cases], afterCases)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

-- | Parse the sequence of 'case' branches inside a match block until TokDedent.
parseMatchCases :: FilePath -> [LocatedToken] -> [MatchCase] -> Either ParseError ([MatchCase], [LocatedToken])
parseMatchCases _ (LocatedToken TokDedent _ _ : rest) acc = Right (reverse acc, rest)
parseMatchCases _ (LocatedToken TokEOF _ _ : rest) acc = Right (reverse acc, rest)
parseMatchCases _ [] acc = Right (reverse acc, [])
parseMatchCases fp toks acc =
  let cleanToks = skipNewlines toks
  in case cleanToks of
    (LocatedToken TokDedent _ _ : rest) -> Right (reverse acc, rest)
    (LocatedToken TokEOF _ _ : rest) -> Right (reverse acc, rest)
    [] -> Right (reverse acc, [])
    (LocatedToken (TokIdent "case") _ _ : restCase) -> do
      (patToks, mGuardToks, afterColon) <- parseCaseHead fp restCase
      patExpr <- parseExpr fp patToks
      mGuardExpr <- case mGuardToks of
        Just gToks -> Just <$> parseExpr fp gToks
        Nothing    -> pure Nothing
      (bodyStmts, afterBody) <- parseSuite fp afterColon
      let mc = MatchCase patExpr mGuardExpr bodyStmts
      parseMatchCases fp (skipNewlines afterBody) (mc : acc)
    (tok:_) ->
      parseErrorAt fp tok "Expected 'case' in match statement suite"

-- | Parse the pattern and optional guard expression ('if ...') preceding the colon in a case branch.
parseCaseHead :: FilePath -> [LocatedToken] -> Either ParseError ([LocatedToken], Maybe [LocatedToken], [LocatedToken])
parseCaseHead fp toks =
  let (headToks, atColon) = spanUntilColonDepth toks
  in case atColon of
    (LocatedToken (TokSymbol ":") _ _ : afterColon) ->
      let (patToks, mGuardToks) = splitCaseGuard headToks
      in Right (patToks, mGuardToks, afterColon)
    (tok:_) -> parseErrorAt fp tok "Expected ':' after case pattern"
    []      -> Left (ParseError fp 1 1 "Unexpected end of input in case clause")

-- | Depth-aware span until colon, respecting parentheses, brackets, and braces.
spanUntilColonDepth :: [LocatedToken] -> ([LocatedToken], [LocatedToken])
spanUntilColonDepth = go (0 :: Int) []
  where
    go _ acc [] = (reverse acc, [])
    go depth acc (t@(LocatedToken tok _ _) : rest) = case tok of
      TokSymbol "(" -> go (depth + 1) (t : acc) rest
      TokSymbol "[" -> go (depth + 1) (t : acc) rest
      TokSymbol "{" -> go (depth + 1) (t : acc) rest
      TokSymbol ")" -> go (max 0 (depth - 1)) (t : acc) rest
      TokSymbol "]" -> go (max 0 (depth - 1)) (t : acc) rest
      TokSymbol "}" -> go (max 0 (depth - 1)) (t : acc) rest
      TokSymbol ":" | depth == 0 -> (reverse acc, t : rest)
      TokNewline    | depth == 0 -> (reverse acc, t : rest)
      _ -> go depth (t : acc) rest

-- | Split case pattern tokens and optional 'if' guard tokens at paren depth 0.
splitCaseGuard :: [LocatedToken] -> ([LocatedToken], Maybe [LocatedToken])
splitCaseGuard = go (0 :: Int) []
  where
    go _ acc [] = (reverse acc, Nothing)
    go depth acc (t@(LocatedToken tok _ _) : rest) = case tok of
      TokSymbol "(" -> go (depth + 1) (t : acc) rest
      TokSymbol "[" -> go (depth + 1) (t : acc) rest
      TokSymbol "{" -> go (depth + 1) (t : acc) rest
      TokSymbol ")" -> go (max 0 (depth - 1)) (t : acc) rest
      TokSymbol "]" -> go (max 0 (depth - 1)) (t : acc) rest
      TokSymbol "}" -> go (max 0 (depth - 1)) (t : acc) rest
      TokKw "if" | depth == 0 -> (reverse acc, Just rest)
      _ -> go depth (t : acc) rest

parseAssignOrExprStmt :: FilePath -> [LocatedToken] -> Either ParseError ([Stmt], [LocatedToken])
parseAssignOrExprStmt fp toks =
  let (lineToks, afterLine) = spanUntilStmtEnd toks
      cleanAfter = skipToNewline afterLine
  in case findAugAssignOp lineToks of
      Just (before, op, after) -> do
        lhs <- parseExpr fp before
        rhs <- parseExpr fp after
        pure ([StmtAugAssign lhs op rhs], cleanAfter)
      Nothing ->
        case findAssignOp lineToks of
          Just (targets, valToks) -> do
            valExpr <- parseExpr fp valToks
            targetExprs <- mapM (parseExpr fp) targets
            pure ([StmtAssign targetExprs valExpr], cleanAfter)
          Nothing ->
            case findAnnAssign lineToks of
              Just (targetToks, typeToks, mValToks) -> do
                tgt <- parseExpr fp targetToks
                ty <- parseExpr fp typeToks
                mVal <- case mValToks of
                  Just vt -> fmap Just (parseExpr fp vt)
                  Nothing -> pure Nothing
                pure ([StmtAnnAssign tgt ty mVal], cleanAfter)
              Nothing -> do
                expr <- parseExpr fp lineToks
                pure ([StmtExpr expr], cleanAfter)

findAugAssignOp :: [LocatedToken] -> Maybe ([LocatedToken], Op, [LocatedToken])
findAugAssignOp toks = go toks []
  where
    go [] _ = Nothing
    go (LocatedToken (TokSymbol sym) _ _ : rest) before
      | Just op <- symToAugOp sym = Just (reverse before, op, rest)
    go (t:rest) before = go rest (t:before)

    symToAugOp "+=" = Just OpAdd
    symToAugOp "-=" = Just OpSub
    symToAugOp "*=" = Just OpMul
    symToAugOp "/=" = Just OpDiv
    symToAugOp "//=" = Just OpFloorDiv
    symToAugOp "%=" = Just OpMod
    symToAugOp "**=" = Just OpPow
    symToAugOp "&=" = Just OpBitAnd
    symToAugOp "|=" = Just OpBitOr
    symToAugOp "^=" = Just OpBitXor
    symToAugOp "<<=" = Just OpShiftL
    symToAugOp ">>=" = Just OpShiftR
    symToAugOp "@=" = Just OpMatMult
    symToAugOp _ = Nothing

findAssignOp :: [LocatedToken] -> Maybe ([[LocatedToken]], [LocatedToken])
findAssignOp toks =
  let splitByEq = splitOnTok (TokSymbol "=") toks
  in if length splitByEq >= 2
     then Just (init splitByEq, last splitByEq)
     else Nothing

findAnnAssign :: [LocatedToken] -> Maybe ([LocatedToken], [LocatedToken], Maybe [LocatedToken])
findAnnAssign toks =
  case span (\(LocatedToken t _ _) -> t /= TokSymbol ":") toks of
    (tgt, LocatedToken (TokSymbol ":") _ _ : rest) ->
      case span (\(LocatedToken t _ _) -> t /= TokSymbol "=") rest of
        (ty, LocatedToken (TokSymbol "=") _ _ : val) ->
          Just (tgt, ty, Just val)
        (ty, []) ->
          Just (tgt, ty, Nothing)
        _ -> Nothing
    _ -> Nothing

splitOnTok :: PyToken -> [LocatedToken] -> [[LocatedToken]]
splitOnTok sep toks = go toks (0 :: Int) [] []
  where
    go [] _ curr acc = reverse (reverse curr : acc)
    go (tok@(LocatedToken (TokSymbol s) _ _) : rest) !depth curr acc
      | s `elem` ["(", "[", "{"] = go rest (depth + 1) (tok : curr) acc
      | s `elem` [")", "]", "}"] = go rest (max 0 (depth - 1)) (tok : curr) acc
      | ltToken tok == sep && depth == 0 = go rest 0 [] (reverse curr : acc)
      | otherwise = go rest depth (tok : curr) acc
    go (tok : rest) !depth curr acc
      | ltToken tok == sep && depth == 0 = go rest 0 [] (reverse curr : acc)
      | otherwise = go rest depth (tok : curr) acc

-- | Extract tokens up to matching closing delimiter, tracking nested (), [], and {}.
takeBalancedDelim :: Text -> Text -> [LocatedToken] -> ([LocatedToken], [LocatedToken])
takeBalancedDelim openSym closeSym toks = go toks (0 :: Int) []
  where
    go [] _ acc = (reverse acc, [])
    go (tok@(LocatedToken (TokSymbol s) _ _) : rest) !depth acc
      | s == openSym = go rest (depth + 1) (tok : acc)
      | s == closeSym =
          if depth == 0
            then (reverse acc, tok : rest)
            else go rest (depth - 1) (tok : acc)
      | s `elem` ["(", "[", "{"] = go rest (depth + 1) (tok : acc)
      | s `elem` [")", "]", "}"] =
          if depth > 0
            then go rest (depth - 1) (tok : acc)
            else (reverse acc, tok : rest)
      | otherwise = go rest depth (tok : acc)
    go (tok : rest) !depth acc = go rest depth (tok : acc)

-- ============================================================================
-- Expression Parsing (Operator Precedence)
-- ============================================================================

parseExpr :: FilePath -> [LocatedToken] -> Either ParseError Expr
parseExpr fp toks = do
  (expr, rest) <- parseExprOrTernary fp toks
  if null (skipNewlines rest)
    then Right expr
    else Right expr

parseExprUntilColon :: FilePath -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])
parseExprUntilColon fp toks = do
  let (exprToks, after) = spanUntilColonDepth toks
  expr <- parseExpr fp exprToks
  afterColon <- expectSymbol fp ":" after
  pure (expr, afterColon)

parseExprOrTernary :: FilePath -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])
parseExprOrTernary fp (LocatedToken (TokKw "lambda") _ _ : rest) = do
  let (paramToks, afterParams) = span (\(LocatedToken t _ _) -> t /= TokSymbol ":") rest
  afterColon <- expectSymbol fp ":" afterParams
  params <- parseLambdaParams fp paramToks
  (body, afterBody) <- parseExprOrTernary fp afterColon
  pure (ExprLambda params body, afterBody)
parseExprOrTernary fp toks = do
  (trueVal, afterTrue) <- parseExprOr fp toks
  case afterTrue of
    (LocatedToken (TokKw "if") _ _ : restIf) -> do
      (cond, afterCond) <- parseExprOr fp restIf
      case afterCond of
        (LocatedToken (TokKw "else") _ _ : restElse) -> do
          (falseVal, afterFalse) <- parseExprOrTernary fp restElse
          pure (ExprTernary cond trueVal falseVal, afterFalse)
        _ -> Right (trueVal, afterTrue)
    _ -> Right (trueVal, afterTrue)

parseLambdaParams :: FilePath -> [LocatedToken] -> Either ParseError [Parameter]
parseLambdaParams _ [] = Right []
parseLambdaParams _ toks =
  let idents = [name | LocatedToken (TokIdent name) _ _ <- toks]
  in Right [Parameter n ParamPositional Nothing Nothing | n <- idents]

parseExprOr :: FilePath -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])
parseExprOr fp toks = parseBinaryLeft fp parseExprAnd ["or"] OpOr toks

parseExprAnd :: FilePath -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])
parseExprAnd fp toks = parseBinaryLeft fp parseExprNot ["and"] OpAnd toks

parseExprNot :: FilePath -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])
parseExprNot fp (LocatedToken (TokKw "not") _ _ : rest) = do
  (e, r) <- parseExprNot fp rest
  pure (ExprUnary OpNot e, r)
parseExprNot fp toks = parseExprCompare fp toks

parseExprCompare :: FilePath -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])
parseExprCompare fp toks = do
  (lhs, rest) <- parseExprBitOr fp toks
  go lhs rest
  where
    go left (LocatedToken (TokSymbol "==") _ _ : r) = step left OpEq r
    go left (LocatedToken (TokSymbol "<=") _ _ : r) = step left OpLtE r
    go left (LocatedToken (TokSymbol ">=") _ _ : r) = step left OpGtE r
    go left (LocatedToken (TokSymbol "<") _ _ : r)  = step left OpLt r
    go left (LocatedToken (TokSymbol ">") _ _ : r)  = step left OpGt r
    go left (LocatedToken (TokKw "in") _ _ : r)     = step left OpIn r
    go left (LocatedToken (TokKw "not") _ _ : LocatedToken (TokKw "in") _ _ : r) = step left OpNotIn r
    go left (LocatedToken (TokKw "is") _ _ : LocatedToken (TokKw "not") _ _ : r) = step left OpIsNot r
    go left (LocatedToken (TokKw "is") _ _ : r)     = step left OpIs r
    go left r = Right (left, r)

    step left op r = do
      (rhs, afterRhs) <- parseExprBitOr fp r
      go (ExprBinary op left rhs) afterRhs

parseExprBitOr :: FilePath -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])
parseExprBitOr fp toks = parseBinaryLeft fp parseExprBitXor ["|"] OpBitOr toks

parseExprBitXor :: FilePath -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])
parseExprBitXor fp toks = parseBinaryLeft fp parseExprBitAnd ["^"] OpBitXor toks

parseExprBitAnd :: FilePath -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])
parseExprBitAnd fp toks = parseBinaryLeft fp parseExprShift ["&"] OpBitAnd toks

parseExprShift :: FilePath -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])
parseExprShift fp toks = do
  (lhs, rest) <- parseExprAddSub fp toks
  go lhs rest
  where
    go left (LocatedToken (TokSymbol "<<") _ _ : r) = do
      (rhs, afterRhs) <- parseExprAddSub fp r
      go (ExprBinary OpShiftL left rhs) afterRhs
    go left (LocatedToken (TokSymbol ">>") _ _ : r) = do
      (rhs, afterRhs) <- parseExprAddSub fp r
      go (ExprBinary OpShiftR left rhs) afterRhs
    go left r = Right (left, r)

parseExprAddSub :: FilePath -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])
parseExprAddSub fp toks = do
  (lhs, rest) <- parseExprMulDiv fp toks
  go lhs rest
  where
    go left (LocatedToken (TokSymbol "+") _ _ : r) = do
      (rhs, afterRhs) <- parseExprMulDiv fp r
      go (ExprBinary OpAdd left rhs) afterRhs
    go left (LocatedToken (TokSymbol "-") _ _ : r) = do
      (rhs, afterRhs) <- parseExprMulDiv fp r
      go (ExprBinary OpSub left rhs) afterRhs
    go left r = Right (left, r)

parseExprMulDiv :: FilePath -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])
parseExprMulDiv fp toks = do
  (lhs, rest) <- parseExprUnary fp toks
  go lhs rest
  where
    go left (LocatedToken (TokSymbol "*") _ _ : r) = do
      (rhs, afterRhs) <- parseExprUnary fp r
      go (ExprBinary OpMul left rhs) afterRhs
    go left (LocatedToken (TokSymbol "/") _ _ : r) = do
      (rhs, afterRhs) <- parseExprUnary fp r
      go (ExprBinary OpDiv left rhs) afterRhs
    go left (LocatedToken (TokSymbol "//") _ _ : r) = do
      (rhs, afterRhs) <- parseExprUnary fp r
      go (ExprBinary OpFloorDiv left rhs) afterRhs
    go left (LocatedToken (TokSymbol "%") _ _ : r) = do
      (rhs, afterRhs) <- parseExprUnary fp r
      go (ExprBinary OpMod left rhs) afterRhs
    go left (LocatedToken (TokSymbol "@") _ _ : r) = do
      (rhs, afterRhs) <- parseExprUnary fp r
      go (ExprBinary OpMatMult left rhs) afterRhs
    go left r = Right (left, r)

parseExprUnary :: FilePath -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])
parseExprUnary fp (LocatedToken (TokSymbol "+") _ _ : rest) = do
  (e, r) <- parseExprUnary fp rest
  pure (ExprUnary OpAdd e, r)
parseExprUnary fp (LocatedToken (TokSymbol "-") _ _ : rest) = do
  (e, r) <- parseExprUnary fp rest
  pure (ExprUnary OpSub e, r)
parseExprUnary fp (LocatedToken (TokSymbol "~") _ _ : rest) = do
  (e, r) <- parseExprUnary fp rest
  pure (ExprUnary OpInvert e, r)
parseExprUnary fp (LocatedToken (TokKw "await") _ _ : rest) = do
  (e, r) <- parseExprUnary fp rest
  pure (ExprAwait e, r)
parseExprUnary fp (LocatedToken (TokKw "yield") _ _ : LocatedToken (TokKw "from") _ _ : rest) = do
  (e, r) <- parseExprUnary fp rest
  pure (ExprYieldFrom e, r)
parseExprUnary fp (LocatedToken (TokKw "yield") _ _ : rest) = do
  if null rest || isStmtEnd (head rest)
    then Right (ExprYield Nothing, rest)
    else do
      (e, r) <- parseExprOrTernary fp rest
      pure (ExprYield (Just e), r)
parseExprUnary fp (LocatedToken (TokSymbol "*") _ _ : rest) = do
  (e, r) <- parseExprUnary fp rest
  pure (ExprStarred e, r)
parseExprUnary fp (LocatedToken (TokSymbol "**") _ _ : rest) = do
  (e, r) <- parseExprUnary fp rest
  pure (ExprKwStarred e, r)
parseExprUnary fp toks = parseExprPow fp toks

parseExprPow :: FilePath -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])
parseExprPow fp toks = do
  (lhs, rest) <- parseExprPostfix fp toks
  case rest of
    (LocatedToken (TokSymbol "**") _ _ : r) -> do
      (rhs, afterRhs) <- parseExprUnary fp r
      pure (ExprBinary OpPow lhs rhs, afterRhs)
    _ -> Right (lhs, rest)

parseExprPostfix :: FilePath -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])
parseExprPostfix fp toks = do
  (primary, rest) <- parseExprPrimary fp toks
  go primary rest
  where
    go target (LocatedToken (TokSymbol "(") _ _ : r) = do
      (args, kwArgs, afterArgs) <- parseCallArgs fp r
      go (ExprCall target args kwArgs) afterArgs
    go target (LocatedToken (TokSymbol "[") _ _ : r) = do
      (subExpr, afterSub) <- parseSubscriptOrSlice fp r
      go (ExprSubscript target subExpr) afterSub
    go target (LocatedToken (TokSymbol ".") _ _ : LocatedToken (TokIdent attr) _ _ : r) =
      go (ExprAttr target attr) r
    go target r = Right (target, r)

parseCallArgs :: FilePath -> [LocatedToken] -> Either ParseError ([Expr], [(Text, Expr)], [LocatedToken])
parseCallArgs fp toks = go toks [] []
  where
    go (LocatedToken (TokSymbol ")") _ _ : r) pos kw = Right (reverse pos, reverse kw, r)
    go (LocatedToken (TokSymbol ",") _ _ : r) pos kw = go r pos kw
    go (LocatedToken (TokIdent k) _ _ : LocatedToken (TokSymbol "=") _ _ : r) pos kw = do
      (val, afterVal) <- parseExprOrTernary fp r
      go afterVal pos ((k, val) : kw)
    go ts pos kw = do
      (val, afterVal) <- parseExprOrTernary fp ts
      go afterVal (val : pos) kw

parseSubscriptOrSlice :: FilePath -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])
parseSubscriptOrSlice fp toks = do
  let (sliceToks, afterSlice) = span (\(LocatedToken t _ _) -> t /= TokSymbol "]") toks
  afterClose <- expectSymbol fp "]" afterSlice
  case splitOnTok (TokSymbol ":") sliceToks of
    [single] -> do
      expr <- parseExpr fp single
      pure (expr, afterClose)
    [lowerPart, upperPart] -> do
      mLower <- if null lowerPart then pure Nothing else fmap Just (parseExpr fp lowerPart)
      mUpper <- if null upperPart then pure Nothing else fmap Just (parseExpr fp upperPart)
      pure (ExprSlice mLower mUpper Nothing, afterClose)
    [lowerPart, upperPart, stepPart] -> do
      mLower <- if null lowerPart then pure Nothing else fmap Just (parseExpr fp lowerPart)
      mUpper <- if null upperPart then pure Nothing else fmap Just (parseExpr fp upperPart)
      mStep  <- if null stepPart then pure Nothing else fmap Just (parseExpr fp stepPart)
      pure (ExprSlice mLower mUpper mStep, afterClose)
    _ -> do
      expr <- parseExpr fp sliceToks
      pure (expr, afterClose)

parseExprPrimary :: FilePath -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])
parseExprPrimary fp toks =
  case toks of
    (LocatedToken (TokNum n) _ _ : rest) ->
      Right (ExprLit (LitInt n), rest)
    (LocatedToken (TokFloat d) _ _ : rest) ->
      Right (ExprLit (LitFloat d), rest)
    (LocatedToken (TokStr s) _ _ : rest) ->
      Right (ExprLit (LitString s), rest)
    (LocatedToken (TokBytes b) _ _ : rest) ->
      Right (ExprLit (LitBytes b), rest)
    (LocatedToken (TokFStr parts) _ _ : rest) ->
      Right (ExprFormattedString parts, rest)
    (LocatedToken (TokKw "True") _ _ : rest) ->
      Right (ExprLit (LitBool True), rest)
    (LocatedToken (TokKw "False") _ _ : rest) ->
      Right (ExprLit (LitBool False), rest)
    (LocatedToken (TokKw "None") _ _ : rest) ->
      Right (ExprLit LitNone, rest)
    (LocatedToken (TokSymbol "...") _ _ : rest) ->
      Right (ExprLit LitEllipsis, rest)
    (LocatedToken (TokIdent name) _ _ : LocatedToken (TokSymbol ":=") _ _ : rest) -> do
      (val, afterVal) <- parseExprOrTernary fp rest
      pure (ExprWalrus name val, afterVal)
    (LocatedToken (TokIdent name) _ _ : rest) ->
      Right (ExprId name, rest)
    (LocatedToken (TokSymbol "(") _ _ : rest) ->
      parseParenOrTupleOrGen fp rest
    (LocatedToken (TokSymbol "[") _ _ : rest) ->
      parseListOrListComp fp rest
    (LocatedToken (TokSymbol "{") _ _ : rest) ->
      parseDictOrSetOrComp fp rest
    (tok:_) ->
      parseErrorAt fp tok ("Unexpected token in primary expression: " ++ show (ltToken tok))
    [] ->
      Left (ParseError fp 1 1 "Unexpected end of input in expression")

parseParenOrTupleOrGen :: FilePath -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])
parseParenOrTupleOrGen _ (LocatedToken (TokSymbol ")") _ _ : rest) =
  Right (ExprTuple [], rest)
parseParenOrTupleOrGen fp toks = do
  let (inside, afterParen) = takeBalancedDelim "(" ")" toks
  afterClose <- expectSymbol fp ")" afterParen
  if any (\(LocatedToken t _ _) -> t == TokKw "for") inside
    then do
      (body, compFors) <- parseComprehension fp inside
      pure (ExprGenerator body compFors, afterClose)
    else case splitOnTok (TokSymbol ",") inside of
      [single] -> do
        expr <- parseExpr fp single
        pure (expr, afterClose)
      parts -> do
        exprs <- mapM (parseExpr fp) (filter (not . null) parts)
        pure (ExprTuple exprs, afterClose)

parseListOrListComp :: FilePath -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])
parseListOrListComp _ (LocatedToken (TokSymbol "]") _ _ : rest) =
  Right (ExprList [], rest)
parseListOrListComp fp toks = do
  let (inside, afterList) = takeBalancedDelim "[" "]" toks
  afterClose <- expectSymbol fp "]" afterList
  if any (\(LocatedToken t _ _) -> t == TokKw "for") inside
    then do
      (body, compFors) <- parseComprehension fp inside
      pure (ExprListComp body compFors, afterClose)
    else do
      let parts = filter (not . null) (splitOnTok (TokSymbol ",") inside)
      exprs <- mapM (parseExpr fp) parts
      pure (ExprList exprs, afterClose)

parseDictOrSetOrComp :: FilePath -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])
parseDictOrSetOrComp _ (LocatedToken (TokSymbol "}") _ _ : rest) =
  Right (ExprDict [], rest)
parseDictOrSetOrComp fp toks = do
  let (inside, afterSet) = takeBalancedDelim "{" "}" toks
  afterClose <- expectSymbol fp "}" afterSet
  let isDict = any (\(LocatedToken t _ _) -> t == TokSymbol ":") inside
  let isComp = any (\(LocatedToken t _ _) -> t == TokKw "for") inside
  if isDict && isComp
    then do
      (k, v, compFors) <- parseDictComprehension fp inside
      pure (ExprDictComp k v compFors, afterClose)
    else if isComp
      then do
        (body, compFors) <- parseComprehension fp inside
        pure (ExprSetComp body compFors, afterClose)
      else if isDict
        then do
          let parts = filter (not . null) (splitOnTok (TokSymbol ",") inside)
          pairs <- mapM (parseDictPair fp) parts
          pure (ExprDict pairs, afterClose)
        else do
          let parts = filter (not . null) (splitOnTok (TokSymbol ",") inside)
          exprs <- mapM (parseExpr fp) parts
          pure (ExprSet exprs, afterClose)

parseDictPair :: FilePath -> [LocatedToken] -> Either ParseError (Expr, Expr)
parseDictPair fp toks =
  case span (\(LocatedToken t _ _) -> t /= TokSymbol ":") toks of
    (kToks, LocatedToken (TokSymbol ":") _ _ : vToks) -> do
      k <- parseExpr fp kToks
      v <- parseExpr fp vToks
      pure (k, v)
    _ -> Left (ParseError fp 1 1 "Expected key:value pair in dict literal")

parseComprehension :: FilePath -> [LocatedToken] -> Either ParseError (Expr, [CompFor])
parseComprehension fp toks = do
  let (exprToks, forToks) = span (\(LocatedToken t _ _) -> t /= TokKw "for") toks
  bodyExpr <- parseExpr fp exprToks
  compFors <- parseCompForList fp forToks
  pure (bodyExpr, compFors)

parseDictComprehension :: FilePath -> [LocatedToken] -> Either ParseError (Expr, Expr, [CompFor])
parseDictComprehension fp toks = do
  let (pairToks, forToks) = span (\(LocatedToken t _ _) -> t /= TokKw "for") toks
  (k, v) <- parseDictPair fp pairToks
  compFors <- parseCompForList fp forToks
  pure (k, v, compFors)

parseCompForList :: FilePath -> [LocatedToken] -> Either ParseError [CompFor]
parseCompForList _ [] = Right []
parseCompForList fp (LocatedToken (TokKw "for") _ _ : rest) = do
  let (targetToks, afterTarget) = span (\(LocatedToken t _ _) -> t /= TokKw "in") rest
  targetExpr <- parseExpr fp targetToks
  let afterIn = drop 1 afterTarget
  let (iterToks, afterIter) = span (\(LocatedToken t _ _) -> t /= TokKw "if" && t /= TokKw "for") afterIn
  iterExpr <- parseExpr fp iterToks
  let (ifToks, afterIfs) = span (\(LocatedToken t _ _) -> t /= TokKw "for") afterIter
  ifExprs <- if null ifToks
             then pure []
             else do
               let cleanIfs = drop 1 ifToks
               ifExpr <- parseExpr fp cleanIfs
               pure [ifExpr]
  restFors <- parseCompForList fp afterIfs
  pure (CompFor targetExpr iterExpr ifExprs : restFors)
parseCompForList _ _ = Right []

-- ============================================================================
-- Utility Helpers
-- ============================================================================

parseBinaryLeft :: FilePath -> (FilePath -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])) -> [Text] -> Op -> [LocatedToken] -> Either ParseError (Expr, [LocatedToken])
parseBinaryLeft fp subParser syms op toks = do
  (lhs, rest) <- subParser fp toks
  go lhs rest
  where
    go left (LocatedToken (TokSymbol s) _ _ : r) | s `elem` syms = do
      (rhs, afterRhs) <- subParser fp r
      go (ExprBinary op left rhs) afterRhs
    go left (LocatedToken (TokKw k) _ _ : r) | k `elem` syms = do
      (rhs, afterRhs) <- subParser fp r
      go (ExprBinary op left rhs) afterRhs
    go left r = Right (left, r)

expectSymbol :: FilePath -> Text -> [LocatedToken] -> Either ParseError [LocatedToken]
expectSymbol _ sym (LocatedToken (TokSymbol s) _ _ : rest) | s == sym = Right rest
expectSymbol fp sym (tok:_) = parseErrorAt fp tok ("Expected '" ++ T.unpack sym ++ "'")
expectSymbol fp sym [] = Left (ParseError fp 1 1 (T.pack ("Expected '" ++ T.unpack sym ++ "' but reached EOF")))

expectKw :: FilePath -> Text -> [LocatedToken] -> Either ParseError [LocatedToken]
expectKw _ kw (LocatedToken (TokKw k) _ _ : rest) | k == kw = Right rest
expectKw fp kw (tok:_) = parseErrorAt fp tok ("Expected keyword '" ++ T.unpack kw ++ "'")
expectKw fp kw [] = Left (ParseError fp 1 1 (T.pack ("Expected keyword '" ++ T.unpack kw ++ "' but reached EOF")))

parseErrorAt :: FilePath -> LocatedToken -> String -> Either ParseError a
parseErrorAt fp (LocatedToken _ line col) msg =
  Left (ParseError fp line col (T.pack msg))

tokenToText :: PyToken -> Text
tokenToText (TokIdent t) = t
tokenToText (TokKw t) = t
tokenToText (TokNum n) = T.pack (show n)
tokenToText (TokFloat f) = T.pack (show f)
tokenToText (TokStr s) = "\"" <> s <> "\""
tokenToText (TokBytes b) = "b\"" <> b <> "\""
tokenToText (TokFStr _) = "f\"...\""
tokenToText (TokSymbol s) = s
tokenToText TokNewline = "\n"
tokenToText TokIndent = "  "
tokenToText TokDedent = ""
tokenToText TokEOF = ""

isIdentTok :: LocatedToken -> Bool
isIdentTok (LocatedToken (TokIdent _) _ _) = True
isIdentTok (LocatedToken (TokKw _) _ _) = True
isIdentTok _ = False

spanUntilStmtEnd :: [LocatedToken] -> ([LocatedToken], [LocatedToken])
spanUntilStmtEnd = span (\(LocatedToken t _ _) -> t /= TokNewline && t /= TokEOF && t /= TokDedent)

skipToNewline :: [LocatedToken] -> [LocatedToken]
skipToNewline = dropWhile (\(LocatedToken t _ _) -> t /= TokNewline && t /= TokEOF && t /= TokDedent)

parseIdentList :: [LocatedToken] -> ([Text], [LocatedToken])
parseIdentList toks = go toks []
  where
    go (LocatedToken (TokIdent name) _ _ : LocatedToken (TokSymbol ",") _ _ : r) acc =
      go r (name : acc)
    go (LocatedToken (TokIdent name) _ _ : r) acc =
      (reverse (name : acc), r)
    go r acc = (reverse acc, r)

isStmtEnd :: LocatedToken -> Bool
isStmtEnd (LocatedToken t _ _) = t == TokNewline || t == TokEOF || t == TokDedent
