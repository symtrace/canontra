{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Canontra.Parser.SymbolTable
Description : High-throughput, zero-allocation symbol interning and canonical symbol table.

Provides bidirectional mapping between variable/type/keyword byte sequences and compact,
unboxed 32-bit 'SymbolId' tokens. Symbol interning eliminates redundant string heap
allocations during AST ingestion and enables 1-cycle CPU register equality checks.
-}
module Canontra.Parser.SymbolTable
  ( SymbolId (..)
  , SymbolTable (..)
  , emptySymbolTable
  , internSymbolBS
  , internSymbolText
  , lookupSymbolBS
  , lookupSymbolText
  , resolveSymbolBS
  , resolveSymbolText
  , internManyBS
  , internManyText
  , fromListBS
  , fromListText
  , symbolTableSize
  , symbolTableEntries
  , preloadPolyglotKeywords
  , pythonKeywords
  , jsKeywords
  , goKeywords
  , rustKeywords
  ) where

import Control.DeepSeq (NFData (..))
import Data.Binary (Binary (..), get, put)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word32)
import GHC.Generics (Generic)

-- | Compact, unboxed 32-bit token representing an interned symbol.
newtype SymbolId = SymbolId { unSymbolId :: Word32 }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving newtype (NFData, Binary, Enum, Bounded)

-- | Immutable, cache-friendly symbol interning table.
data SymbolTable = SymbolTable
  { stNextId  :: {-# UNPACK #-} !Word32
  , stLookup  :: !(Map.Map BS.ByteString SymbolId)
  , stReverse :: !(V.Vector BS.ByteString)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

instance Binary SymbolTable where
  put (SymbolTable nextId lk rev) = do
    put nextId
    put lk
    put (V.toList rev)
  get = do
    nextId <- get
    lk <- get
    revList <- get
    pure (SymbolTable nextId lk (V.fromList revList))

-- | An empty symbol table with zero entries.
emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable
  { stNextId  = 0
  , stLookup  = Map.empty
  , stReverse = V.empty
  }

-- | Intern a raw 'BS.ByteString' symbol, returning its 'SymbolId' and the updated table.
-- If the symbol is already present, returns the existing 'SymbolId' with zero allocations.
internSymbolBS :: BS.ByteString -> SymbolTable -> (SymbolId, SymbolTable)
internSymbolBS !bs !st@(SymbolTable nextId lk rev) =
  case Map.lookup bs lk of
    Just existingId -> (existingId, st)
    Nothing ->
      let !newId = SymbolId nextId
          !newLk = Map.insert bs newId lk
          !newRev = V.snoc rev bs
          !newSt = SymbolTable (nextId + 1) newLk newRev
      in (newId, newSt)

-- | Intern a 'Text' symbol by encoding to UTF-8.
internSymbolText :: Text -> SymbolTable -> (SymbolId, SymbolTable)
internSymbolText !t !st = internSymbolBS (TE.encodeUtf8 t) st

-- | Lookup the 'SymbolId' for a 'BS.ByteString' without mutating the table.
lookupSymbolBS :: BS.ByteString -> SymbolTable -> Maybe SymbolId
lookupSymbolBS !bs (SymbolTable _ lk _) = Map.lookup bs lk

-- | Lookup the 'SymbolId' for a 'Text' without mutating the table.
lookupSymbolText :: Text -> SymbolTable -> Maybe SymbolId
lookupSymbolText !t st = lookupSymbolBS (TE.encodeUtf8 t) st

-- | Resolve a 'SymbolId' back into its original 'BS.ByteString'.
resolveSymbolBS :: SymbolId -> SymbolTable -> Maybe BS.ByteString
resolveSymbolBS (SymbolId idx) (SymbolTable _ _ rev) =
  rev V.!? fromIntegral idx

-- | Resolve a 'SymbolId' back into its original 'Text'.
resolveSymbolText :: SymbolId -> SymbolTable -> Maybe Text
resolveSymbolText !symId !st =
  case resolveSymbolBS symId st of
    Just bs -> Just (TE.decodeUtf8Lenient bs)
    Nothing -> Nothing

-- | Batch-intern a list of 'BS.ByteString's.
internManyBS :: [BS.ByteString] -> SymbolTable -> ([SymbolId], SymbolTable)
internManyBS [] !st = ([], st)
internManyBS (b:bs) !st =
  let (!sid, !st') = internSymbolBS b st
      (!sids, !st'') = internManyBS bs st'
  in (sid : sids, st'')

-- | Batch-intern a list of 'Text' symbols.
internManyText :: [Text] -> SymbolTable -> ([SymbolId], SymbolTable)
internManyText !ts !st = internManyBS (map TE.encodeUtf8 ts) st

-- | Construct a 'SymbolTable' and corresponding 'SymbolId' list from a list of 'BS.ByteString's.
fromListBS :: [BS.ByteString] -> (SymbolTable, [SymbolId])
fromListBS !bs =
  let (!sids, !st) = internManyBS bs emptySymbolTable
  in (st, sids)

-- | Construct a 'SymbolTable' and corresponding 'SymbolId' list from a list of 'Text' symbols.
fromListText :: [Text] -> (SymbolTable, [SymbolId])
fromListText !ts = fromListBS (map TE.encodeUtf8 ts)

-- | Total number of unique interned symbols.
symbolTableSize :: SymbolTable -> Int
symbolTableSize (SymbolTable nextId _ _) = fromIntegral nextId

-- | Extract all interned pairs (SymbolId, ByteString) in indexed order.
symbolTableEntries :: SymbolTable -> [(SymbolId, BS.ByteString)]
symbolTableEntries (SymbolTable _ _ rev) =
  zip (map (SymbolId . fromIntegral) [0 .. V.length rev - 1]) (V.toList rev)

-- ============================================================================
-- Preloaded Polyglot Keywords
-- ============================================================================

-- | Python 3.8+ language keywords.
pythonKeywords :: [BS.ByteString]
pythonKeywords =
  [ "False", "None", "True", "and", "as", "assert", "async", "await"
  , "break", "class", "continue", "def", "del", "elif", "else", "except"
  , "finally", "for", "from", "global", "if", "import", "in", "is"
  , "lambda", "nonlocal", "not", "or", "pass", "raise", "return", "try"
  , "while", "with", "yield"
  ]

-- | JavaScript & TypeScript ES2022+ language keywords.
jsKeywords :: [BS.ByteString]
jsKeywords =
  [ "break", "case", "catch", "class", "const", "continue", "debugger"
  , "default", "delete", "do", "else", "enum", "export", "extends"
  , "false", "finally", "for", "function", "if", "import", "in"
  , "instanceof", "new", "null", "return", "super", "switch", "this"
  , "throw", "true", "try", "typeof", "var", "void", "while", "with"
  , "yield", "let", "static", "yield", "await", "async", "type", "interface"
  , "namespace", "declare", "abstract", "as", "is", "keyof", "readonly"
  ]

-- | Go language keywords.
goKeywords :: [BS.ByteString]
goKeywords =
  [ "break", "case", "chan", "const", "continue", "default", "defer"
  , "else", "fallthrough", "for", "func", "go", "goto", "if", "import"
  , "interface", "map", "package", "range", "return", "select", "struct"
  , "switch", "type", "var"
  ]

-- | Rust language keywords.
rustKeywords :: [BS.ByteString]
rustKeywords =
  [ "as", "async", "await", "break", "const", "continue", "crate", "dyn"
  , "else", "enum", "extern", "false", "fn", "for", "if", "impl", "in"
  , "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return"
  , "self", "Self", "static", "struct", "super", "trait", "true", "type"
  , "unsafe", "use", "where", "while"
  ]

-- | Pre-intern all standard keywords across Python, TS/JS, Go, and Rust.
preloadPolyglotKeywords :: SymbolTable
preloadPolyglotKeywords =
  let allKws = pythonKeywords ++ jsKeywords ++ goKeywords ++ rustKeywords
      (!_, !st) = internManyBS allKws emptySymbolTable
  in st
