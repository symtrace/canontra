{- |
Module      : Canontra.Parser.Polyglot
Description : Polyglot source ingestion router across Python, JS, TS, Go, and Rust.

Routes source files to the appropriate zero-span parser based on file extension
or explicit language specification.
-}
module Canontra.Parser.Polyglot
  ( parsePolyglotSource
  , parsePolyglotSourceWithLang
  , detectLanguage
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeExtension)

import Canontra.IR.Program (Program)
import Canontra.Parser.Go (parseGoSource)
import Canontra.Parser.JS (parseJSSource)
import Canontra.Parser.Python (parsePythonSource)
import Canontra.Parser.Rust (parseRustSource)
import Canontra.Types (LanguageTag (..), ParseError (..))

-- | Ingest and parse a polyglot source file by auto-detecting language from extension.
parsePolyglotSource :: FilePath -> Text -> Either ParseError Program
parsePolyglotSource filePath input =
  let lang = detectLanguage filePath
  in parsePolyglotSourceWithLang lang filePath input

-- | Parse source text using an explicitly declared language.
parsePolyglotSourceWithLang :: LanguageTag -> FilePath -> Text -> Either ParseError Program
parsePolyglotSourceWithLang lang filePath input = case lang of
  LangPython     -> parsePythonSource filePath input
  LangJavaScript -> parseJSSource filePath input
  LangTypeScript -> parseJSSource filePath input
  LangGo         -> parseGoSource filePath input
  LangRust       -> parseRustSource filePath input
  LangUnknown _  -> parsePythonSource filePath input

-- | Detect the programming language from a file path extension.
detectLanguage :: FilePath -> LanguageTag
detectLanguage path =
  let ext = T.toLower (T.pack (takeExtension path))
  in case ext of
      ".py"  -> LangPython
      ".pyi" -> LangPython
      ".js"  -> LangJavaScript
      ".jsx" -> LangJavaScript
      ".mjs" -> LangJavaScript
      ".cjs" -> LangJavaScript
      ".ts"  -> LangTypeScript
      ".tsx" -> LangTypeScript
      ".go"  -> LangGo
      ".rs"  -> LangRust
      _      -> LangPython
