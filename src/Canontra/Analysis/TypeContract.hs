{- |
Module      : Canontra.Analysis.TypeContract
Description : Flow-sensitive structural type system and contract normalizer for F_T.

This module canonicalizes polyglot type declarations (TypeScript interfaces, Go interfaces,
Rust traits, and Python typing.Protocols) into structural normal forms. It enforces:
  1. Method permutation invariance (lexicographical sort by identifier).
  2. Union and intersection commutativity (A | B == B | A, A & B == B & A).
  3. Primitive type cross-language harmonization.
  4. Nominal-independent structural subtyping and equivalence.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Canontra.Analysis.TypeContract
  ( StructuralType (..)
  , MethodContract (..)
  , InterfaceContract (..)
  , makeUnion
  , makeIntersection
  , parseTypeString
  , normalizeMethodContract
  , normalizeInterfaceContract
  , extractTypeContracts
  , areStructurallyEqual
  , isSubtypeOf
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (nub, sort, sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Canontra.IR.Declaration
  ( Class (..)
  , Declaration (..)
  , Function (..)
  , Interface (..)
  , Parameter (..)
  , Struct (..)
  , Trait (..)
  )
import Canontra.IR.Program (Module (..), Program (..))

-- | Language-independent canonical structural type representation.
data StructuralType
  = TypePrimitive !Text
  | TypeRecord ![(Text, StructuralType)]
  | TypeFunction ![StructuralType] !StructuralType
  | TypeArray !StructuralType
  | TypeUnion ![StructuralType]
  | TypeIntersection ![StructuralType]
  | TypeOptional !StructuralType
  | TypeGeneric !Text ![StructuralType]
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Smart constructor for Union types enforcing commutativity and deduplication.
makeUnion :: [StructuralType] -> StructuralType
makeUnion types =
  let flattened = concatMap (\case TypeUnion ts -> ts; other -> [other]) types
      deduped = sort (nub flattened)
  in case deduped of
    []  -> TypePrimitive "never"
    [t] -> t
    ts  -> TypeUnion ts

-- | Smart constructor for Intersection types enforcing commutativity and deduplication.
makeIntersection :: [StructuralType] -> StructuralType
makeIntersection types =
  let flattened = concatMap (\case TypeIntersection ts -> ts; other -> [other]) types
      deduped = sort (nub flattened)
  in case deduped of
    []  -> TypePrimitive "any"
    [t] -> t
    ts  -> TypeIntersection ts

-- | Canonical method contract.
data MethodContract = MethodContract
  { mcName    :: !Text
  , mcParams  :: ![StructuralType]
  , mcReturn  :: !StructuralType
  , mcIsAsync :: !Bool
  } deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Canonical structural interface contract.
data InterfaceContract = InterfaceContract
  { icName    :: !Text
  , icMethods :: ![MethodContract]
  , icFields  :: ![(Text, StructuralType)]
  } deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Normalize a raw type string into canonical StructuralType across polyglot languages.
parseTypeString :: Text -> StructuralType
parseTypeString raw =
  let clean = T.strip raw
  in case () of
    _ | T.null clean -> TypePrimitive "any"
      | "|" `T.isInfixOf` clean && not ("<" `T.isInfixOf` clean) ->
          makeUnion (map parseTypeString (T.splitOn "|" clean))
      | "&" `T.isInfixOf` clean && not ("<" `T.isInfixOf` clean) ->
          makeIntersection (map parseTypeString (T.splitOn "&" clean))
      | clean `elem` ["int", "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "int32", "int64", "number"] ->
          TypePrimitive "number"
      | clean `elem` ["float", "f32", "f64", "float32", "float64", "double"] ->
          TypePrimitive "number"
      | clean `elem` ["str", "string", "String", "Text"] ->
          TypePrimitive "string"
      | clean `elem` ["bool", "boolean", "Boolean"] ->
          TypePrimitive "bool"
      | clean `elem` ["void", "None", "()", "nil", "null", "undefined"] ->
          TypePrimitive "void"
      | clean `elem` ["any", "unknown", "interface{}", "Object", "object"] ->
          TypePrimitive "any"
      | clean `elem` ["bytes", "[]byte", "Uint8Array", "byte[]"] ->
          TypeArray (TypePrimitive "byte")
      | T.isSuffixOf "[]" clean ->
          TypeArray (parseTypeString (T.dropEnd 2 clean))
      | T.isPrefixOf "[]" clean ->
          TypeArray (parseTypeString (T.drop 2 clean))
      | T.isSuffixOf "?" clean ->
          TypeOptional (parseTypeString (T.dropEnd 1 clean))
      | T.isPrefixOf "Optional[" clean && T.isSuffixOf "]" clean ->
          TypeOptional (parseTypeString (T.drop 9 (T.dropEnd 1 clean)))
      | T.isPrefixOf "Promise<" clean && T.isSuffixOf ">" clean ->
          parseTypeString (T.drop 8 (T.dropEnd 1 clean))
      | T.isPrefixOf "Array<" clean && T.isSuffixOf ">" clean ->
          TypeArray (parseTypeString (T.drop 6 (T.dropEnd 1 clean)))
      | T.isPrefixOf "List[" clean && T.isSuffixOf "]" clean ->
          TypeArray (parseTypeString (T.drop 5 (T.dropEnd 1 clean)))
      | otherwise ->
          TypePrimitive clean

-- | Convert an IR Function into a normalized MethodContract.
normalizeMethodContract :: Function -> MethodContract
normalizeMethodContract fn =
  let normParams = map (parseTypeString . maybe "any" id . paramType) (fnParams fn)
      normRet = parseTypeString (maybe "any" id (fnReturnType fn))
  in MethodContract
      { mcName    = fnName fn
      , mcParams  = normParams
      , mcReturn  = normRet
      , mcIsAsync = fnIsAsync fn
      }

-- | Normalize an IR Interface into a canonical InterfaceContract with sorted methods and fields.
normalizeInterfaceContract :: Interface -> InterfaceContract
normalizeInterfaceContract iface =
  let rawMethods = map normalizeMethodContract (ifMethods iface)
      sortedMethods = sortBy (comparing mcName) rawMethods
  in InterfaceContract
      { icName    = ifName iface
      , icMethods = sortedMethods
      , icFields  = []
      }

-- | Extract all structural interface and trait contracts from a Program.
extractTypeContracts :: Program -> [InterfaceContract]
extractTypeContracts (Program modules _) =
  concatMap extractModuleContracts modules
  where
    extractModuleContracts (Module _ _ decls _) =
      concatMap extractDeclContracts decls

    extractDeclContracts = \case
      DeclInterface iface ->
        [normalizeInterfaceContract iface]

      DeclTrait tr ->
        let rawMethods = map normalizeMethodContract (trMethods tr)
            sortedMethods = sortBy (comparing mcName) rawMethods
        in [InterfaceContract (trName tr) sortedMethods []]

      DeclStruct st ->
        let normFields = sortBy (comparing fst)
              [ (fName, parseTypeString (maybe "any" id fTy))
              | (fName, fTy) <- stFields st
              ]
            normMethods = sortBy (comparing mcName)
              (map normalizeMethodContract (stMethods st))
        in [InterfaceContract (stName st) normMethods normFields]

      DeclClass cls ->
        if not (null (clsMethods cls))
          then
            let normMethods = sortBy (comparing mcName)
                  (map normalizeMethodContract (clsMethods cls))
            in [InterfaceContract (clsName cls) normMethods []]
          else []

      _ -> []

-- | Evaluate whether two interface contracts are structurally identical regardless of nominal name.
areStructurallyEqual :: InterfaceContract -> InterfaceContract -> Bool
areStructurallyEqual c1 c2 =
  icMethods c1 == icMethods c2 && icFields c1 == icFields c2

-- | Evaluate whether sub-contract satisfies super-contract (structural subtyping).
isSubtypeOf :: InterfaceContract -> InterfaceContract -> Bool
isSubtypeOf subContract superContract =
  let hasAllMethods = all (`elem` icMethods subContract) (icMethods superContract)
      hasAllFields = all (`elem` icFields subContract) (icFields superContract)
  in hasAllMethods && hasAllFields
