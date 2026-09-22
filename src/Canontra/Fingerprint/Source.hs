{- |
Module      : Canontra.Fingerprint.Source
Description : F0 baseline source fingerprinting.

The source fingerprint provides the raw, unadorned textual baseline.
Any change in bytes, whitespace, formatting, or comments produces a distinct
hash, representing strict identity before any semantic interpretation occurs.
-}
module Canontra.Fingerprint.Source
  ( computeF0
  , hashBytes
  ) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Text.Printf (printf)

import Canontra.Types (Fingerprint (..))

computeF0 :: BS.ByteString -> Fingerprint -- e.g. computeF0 "print(1)" -> Fingerprint "..."
computeF0 = hashBytes

hashBytes :: BS.ByteString -> Fingerprint -- e.g. SHA-256 hex digest of strict ByteString
hashBytes bs =
  let digest = SHA256.hash bs
      hexStr = concatMap (printf "%02x") (BS.unpack digest)
  in Fingerprint (T.pack hexStr)
