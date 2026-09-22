{- |
Module      : Canontra.Fingerprint.TypeContract
Description : Deterministic structural type contract fingerprint (F_T) computation.

Computes the 9th orthogonal fingerprint tier (F_T) capturing structural interface contracts,
method signatures, and subtyping relationships. Invariant under interface nominal renaming,
method declaration reordering, and union/intersection permutations.
-}
{-# LANGUAGE OverloadedStrings #-}
module Canontra.Fingerprint.TypeContract
  ( computeFT
  , serializeTypeContracts
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Text.Encoding as TE

import Canontra.Analysis.TypeContract
  ( InterfaceContract (..)
  , MethodContract (..)
  , StructuralType (..)
  , extractTypeContracts
  )
import Canontra.Fingerprint.Source (hashBytes)
import Canontra.IR.Program (Program)
import Canontra.Types (Fingerprint)

-- | Compute the structural type contract fingerprint (F_T) for a Program.
computeFT :: Program -> Fingerprint
computeFT prog =
  let contracts = extractTypeContracts prog
      bytes = serializeTypeContracts contracts
  in hashBytes bytes

-- | Deterministically serialize a list of InterfaceContracts into a canonical byte sequence.
serializeTypeContracts :: [InterfaceContract] -> BS.ByteString
serializeTypeContracts rawContracts =
  let sortedContracts = sortBy (comparing (\c -> (icFields c, icMethods c))) rawContracts
      builder = BB.word8 0x0A <> BB.word32BE (fromIntegral (length sortedContracts)) <> foldMap serializeContract sortedContracts
  in BL.toStrict (BB.toLazyByteString builder)
  where
    serializeContract c =
      BB.word32BE (fromIntegral (length (icMethods c)))
        <> foldMap serializeMethod (icMethods c)
        <> BB.word32BE (fromIntegral (length (icFields c)))
        <> foldMap serializeField (icFields c)

    serializeMethod m =
      let nameBytes = TE.encodeUtf8 (mcName m)
      in BB.word32BE (fromIntegral (BS.length nameBytes))
          <> BB.byteString nameBytes
          <> BB.word8 (if mcIsAsync m then 1 else 0)
          <> BB.word32BE (fromIntegral (length (mcParams m)))
          <> foldMap serializeType (mcParams m)
          <> serializeType (mcReturn m)

    serializeField (name, ty) =
      let nameBytes = TE.encodeUtf8 name
      in BB.word32BE (fromIntegral (BS.length nameBytes))
          <> BB.byteString nameBytes
          <> serializeType ty

    serializeType = \case
      TypePrimitive p ->
        let b = TE.encodeUtf8 p
        in BB.word8 0x01 <> BB.word32BE (fromIntegral (BS.length b)) <> BB.byteString b
      TypeRecord fields ->
        BB.word8 0x02 <> BB.word32BE (fromIntegral (length fields)) <> foldMap serializeField fields
      TypeFunction params ret ->
        BB.word8 0x03 <> BB.word32BE (fromIntegral (length params)) <> foldMap serializeType params <> serializeType ret
      TypeArray elemTy ->
        BB.word8 0x04 <> serializeType elemTy
      TypeUnion members ->
        BB.word8 0x05 <> BB.word32BE (fromIntegral (length members)) <> foldMap serializeType members
      TypeIntersection members ->
        BB.word8 0x06 <> BB.word32BE (fromIntegral (length members)) <> foldMap serializeType members
      TypeOptional inner ->
        BB.word8 0x07 <> serializeType inner
      TypeGeneric name args ->
        let b = TE.encodeUtf8 name
        in BB.word8 0x08 <> BB.word32BE (fromIntegral (BS.length b)) <> BB.byteString b <> BB.word32BE (fromIntegral (length args)) <> foldMap serializeType args
