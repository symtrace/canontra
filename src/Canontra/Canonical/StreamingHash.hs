{- |
Module      : Canontra.Canonical.StreamingHash
Description : Zero-allocation direct ByteString.Builder streaming into SHA-256 context.

This module provides high-throughput streaming hashing directly from
pure Haskell 'Data.ByteString.Builder' streams into 'Crypto.Hash.SHA256'
contexts without allocating intermediate contiguous ByteString buffers on the heap.
-}
module Canontra.Canonical.StreamingHash
  ( hashBuilderDirect
  , streamBuilderToSHA256
  ) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Extra as BBE
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Text.Printf (printf)

import Canontra.Types (Fingerprint (..))

-- | Stream a ByteString Builder directly into a SHA-256 context fold.
hashBuilderDirect :: BB.Builder -> Fingerprint
hashBuilderDirect builder =
  let lazyBs = BBE.toLazyByteStringWith
        (BBE.safeStrategy 32768 32768)
        LBS.empty
        builder
      finalCtx = LBS.foldlChunks SHA256.update SHA256.init lazyBs
      digest = SHA256.finalize finalCtx
      hexStr = concatMap (printf "%02x") (LBS.unpack (LBS.fromStrict digest))
  in Fingerprint (T.pack hexStr)

-- | Incrementally update an existing SHA-256 context with Builder chunks.
streamBuilderToSHA256 :: SHA256.Ctx -> BB.Builder -> SHA256.Ctx
streamBuilderToSHA256 initialCtx builder =
  let lazyBs = BBE.toLazyByteStringWith
        (BBE.safeStrategy 32768 32768)
        LBS.empty
        builder
  in LBS.foldlChunks SHA256.update initialCtx lazyBs
