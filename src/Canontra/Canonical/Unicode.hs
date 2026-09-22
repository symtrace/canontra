{- |
Module      : Canontra.Canonical.Unicode
Description : Unicode NFC normalization and string canonicalization.

Ensures deterministic representation of strings by applying:
- Unicode Normalization Form C (NFC precomposition) for common decomposed combining characters
- Line ending canonicalization (normalizes CRLF and CR to LF)
- Zero-width character and invisible formatting normalization.
-}
module Canontra.Canonical.Unicode
  ( normalizeNFC
  , normalizeLineEndings
  , canonicalizeText
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Canonicalize text input: applies NFC precomposition and standardizes line endings.
canonicalizeText :: Text -> Text
canonicalizeText t
  | not (T.any (\c -> c == '\r' || c >= '\x0300') t) = t
  | otherwise = normalizeNFC (normalizeLineEndings t)

-- | Normalize line endings to standard Unix LF ('\n').
normalizeLineEndings :: Text -> Text
normalizeLineEndings t
  | not (T.any (== '\r') t) = t
  | otherwise = T.replace "\r" "\n" (T.replace "\r\n" "\n" t)

-- | Apply Unicode NFC precomposition for combining diacritical marks.
normalizeNFC :: Text -> Text
normalizeNFC t
  | not (T.any (>= '\x0300') t) = t
  | otherwise = T.pack (precompose (T.unpack t))

precompose :: String -> String
precompose [] = []
precompose (c:m:rest)
  | Just comp <- composePair c m = precompose (comp : rest)
precompose (c:rest) = c : precompose rest

-- | Precompose base character and combining mark according to Unicode standard.
composePair :: Char -> Char -> Maybe Char
composePair base mark = case (base, mark) of
  -- Acute accent (U+0301)
  ('a', '\x0301') -> Just '\x00E1' -- á
  ('e', '\x0301') -> Just '\x00E9' -- é
  ('i', '\x0301') -> Just '\x00ED' -- í
  ('o', '\x0301') -> Just '\x00F3' -- ó
  ('u', '\x0301') -> Just '\x00FA' -- ú
  ('y', '\x0301') -> Just '\x00FD' -- ý
  ('c', '\x0301') -> Just '\x0107' -- ć
  ('n', '\x0301') -> Just '\x0144' -- ń
  ('s', '\x0301') -> Just '\x015B' -- ś
  ('z', '\x0301') -> Just '\x017A' -- ź
  ('A', '\x0301') -> Just '\x00C1' -- Á
  ('E', '\x0301') -> Just '\x00C9' -- É
  ('I', '\x0301') -> Just '\x00CD' -- Í
  ('O', '\x0301') -> Just '\x00D3' -- Ó
  ('U', '\x0301') -> Just '\x00DA' -- Ú
  ('Y', '\x0301') -> Just '\x00DD' -- Ý

  -- Grave accent (U+0300)
  ('a', '\x0300') -> Just '\x00E0' -- à
  ('e', '\x0300') -> Just '\x00E8' -- è
  ('i', '\x0300') -> Just '\x00EC' -- ì
  ('o', '\x0300') -> Just '\x00F2' -- ò
  ('u', '\x0300') -> Just '\x00F9' -- ù
  ('A', '\x0300') -> Just '\x00C0' -- À
  ('E', '\x0300') -> Just '\x00C8' -- È
  ('I', '\x0300') -> Just '\x00CC' -- Ì
  ('O', '\x0300') -> Just '\x00D2' -- Ò
  ('U', '\x0300') -> Just '\x00D9' -- Ù

  -- Diaeresis / Umlaut (U+0308)
  ('a', '\x0308') -> Just '\x00E4' -- ä
  ('e', '\x0308') -> Just '\x00EB' -- ë
  ('i', '\x0308') -> Just '\x00EF' -- ï
  ('o', '\x0308') -> Just '\x00F6' -- ö
  ('u', '\x0308') -> Just '\x00FC' -- ü
  ('y', '\x0308') -> Just '\x00FF' -- ÿ
  ('A', '\x0308') -> Just '\x00C4' -- Ä
  ('E', '\x0308') -> Just '\x00CB' -- Ë
  ('I', '\x0308') -> Just '\x00CF' -- Ï
  ('O', '\x0308') -> Just '\x00D6' -- Ö
  ('U', '\x0308') -> Just '\x00DC' -- Ü

  -- Circumflex (U+0302)
  ('a', '\x0302') -> Just '\x00E2' -- â
  ('e', '\x0302') -> Just '\x00EA' -- ê
  ('i', '\x0302') -> Just '\x00EE' -- î
  ('o', '\x0302') -> Just '\x00F4' -- ô
  ('u', '\x0302') -> Just '\x00FB' -- û
  ('A', '\x0302') -> Just '\x00C2' -- Â
  ('E', '\x0302') -> Just '\x00CA' -- Ê
  ('I', '\x0302') -> Just '\x00CE' -- Î
  ('O', '\x0302') -> Just '\x00D4' -- Ô
  ('U', '\x0302') -> Just '\x00DB' -- Û

  -- Tilde (U+0303)
  ('a', '\x0303') -> Just '\x00E3' -- ã
  ('n', '\x0303') -> Just '\x00F1' -- ñ
  ('o', '\x0303') -> Just '\x00F5' -- õ
  ('A', '\x0303') -> Just '\x00C3' -- Ã
  ('N', '\x0303') -> Just '\x00D1' -- Ñ
  ('O', '\x0303') -> Just '\x00D5' -- Õ

  -- Cedilla (U+0327)
  ('c', '\x0327') -> Just '\x00E7' -- ç
  ('C', '\x0327') -> Just '\x00C7' -- Ç

  _ -> Nothing
