{- |
Module      : Canontra.Analysis.TypeContract
Description : Flow-sensitive structural type system and contract normalizer for F_T.

This module canonicalizes polyglot type declarations (TypeScript interfaces, Go interfaces,
Rust traits, structs, type aliases, and Python typing.Protocols / PEP 695 generics) into
structural normal forms. It enforces:
  1. Method and record field permutation invariance (lexicographical sort by identifier).
  2. Deep recursive Union and Intersection Algebraic Normal Form (A | (B | C) == C | B | A).
  3. De Bruijn generic type parameter alpha-normalization ([T, U] -> [α0, α1]).
  4. Primitive and generic container cross-language harmonization.
  5. Nominal-independent structural subtyping and equivalence.
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
  , makeRecord
  , parseTypeString
  , normalizeMethodContract
  , normalizeInterfaceContract
  , normalizeGenericTypeParams
  , substituteStructuralType
  , extractTypeContracts
  , areStructurallyEqual
  , isSubtypeOf
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAlphaNum)
import Data.List (nub, partition, sort, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
  | TypeRecVar !Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Smart constructor for Union types enforcing deep recursive flattening,
-- commutativity, associativity, and idempotent deduplication.
makeUnion :: [StructuralType] -> StructuralType
makeUnion types =
  let flattened = concatMap flattenU types
      deduped = sort (nub flattened)
  in case deduped of
    []  -> TypePrimitive "never"
    [t] -> t
    ts  -> TypeUnion ts
  where
    flattenU (TypeUnion ts) = concatMap flattenU ts
    flattenU other          = [other]

-- | Smart constructor for Intersection types enforcing deep recursive flattening,
-- commutativity, associativity, and idempotent deduplication.
makeIntersection :: [StructuralType] -> StructuralType
makeIntersection types =
  let flattened = concatMap flattenI types
      deduped = sort (nub flattened)
  in case deduped of
    []  -> TypePrimitive "any"
    [t] -> t
    ts  -> TypeIntersection ts
  where
    flattenI (TypeIntersection ts) = concatMap flattenI ts
    flattenI other                 = [other]

-- | Smart constructor for structural Record types enforcing canonical field sorting.
makeRecord :: [(Text, StructuralType)] -> StructuralType
makeRecord fields =
  TypeRecord (sortBy (comparing fst) (nub fields))

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

-- | Normalize a raw type string into canonical 'StructuralType' across polyglot languages.
parseTypeString :: Text -> StructuralType
parseTypeString raw =
  let clean = stripOuterParens (T.strip (normalizeLifetimes raw))
      unionParts = splitTopLevel '|' clean
      interParts = splitTopLevel '&' clean
  in case () of
    _ | T.null clean -> TypePrimitive "any"
      | length unionParts > 1 ->
          makeUnion (map parseTypeString unionParts)
      | length interParts > 1 ->
          makeIntersection (map parseTypeString interParts)
      | T.isPrefixOf "~" clean ->
          parseTypeString (T.drop 1 clean)
      | T.isPrefixOf "*" clean ->
          parseTypeString (T.drop 1 clean)
      | T.isPrefixOf "&mut " clean ->
          parseTypeString (T.drop 5 clean)
      | T.isPrefixOf "&" clean ->
          parseTypeString (T.drop 1 clean)
      | T.isPrefixOf "{" clean && T.isSuffixOf "}" clean ->
          parseRecordLiteral (T.drop 1 (T.dropEnd 1 clean))
      | T.isPrefixOf "(" clean && T.isSuffixOf ")" clean && "," `T.isInfixOf` clean ->
          let inner = T.drop 1 (T.dropEnd 1 clean)
          in makeUnion (map parseTypeString (splitTopLevel ',' inner))
      | clean `elem` ["int", "i8", "i16", "i32", "i64", "i128", "isize", "u8", "u16", "u32", "u64", "u128", "usize", "int32", "int64", "uint", "uint32", "uint64", "number"] ->
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
      | T.isPrefixOf "map[" clean ->
          case splitGoMapType (T.drop 4 clean) of
            Just (kStr, vStr) ->
              TypeGeneric "Map" [parseTypeString kStr, parseTypeString vStr]
            Nothing ->
              TypePrimitive clean
      | Just (base, args) <- parseGenericApplication clean ->
          normalizeGenericConstruct base (map parseTypeString args)
      | otherwise ->
          TypePrimitive (compactTypeSpacing clean)

-- | Strip redundant outer balanced parentheses wrapping an entire type expression,
-- e.g. @"((string | number))"@ -> @"string | number"@, while preserving
-- @" (A | B) | (C | D) "@ where the first '(' closes before the end.
stripOuterParens :: Text -> Text
stripOuterParens txt
  | T.length txt >= 2 && T.head txt == '(' && T.last txt == ')' && wrapsEntirely (T.init (T.tail txt)) =
      let inner = T.strip (T.init (T.tail txt))
      in if length (splitTopLevel ',' inner) > 1
              && length (splitTopLevel '|' inner) <= 1
              && length (splitTopLevel '&' inner) <= 1
           then txt
           else stripOuterParens inner
  | otherwise = txt
  where
    wrapsEntirely inner = go (0 :: Int) (T.unpack inner)
    go depth [] = depth == 0
    go depth (c:cs)
      | c `elem` ("([<{" :: String) = go (depth + 1) cs
      | c `elem` (")]>}" :: String) =
          if depth == 0 then False else go (depth - 1) cs
      | otherwise = go depth cs

-- | Split a type expression by a delimiter character only at top-level bracket depth 0.
splitTopLevel :: Char -> Text -> [Text]
splitTopLevel delim txt = go (0 :: Int) [] [] (T.unpack txt)
  where
    go _ cur acc [] =
      let finalSeg = T.strip (T.pack (reverse cur))
      in reverse (if T.null finalSeg && null acc then [] else finalSeg : acc)
    go depth cur acc (c:cs)
      | c == delim && depth == 0 =
          let seg = T.strip (T.pack (reverse cur))
          in go 0 [] (seg : acc) cs
      | c `elem` ("([<{" :: String) =
          go (depth + 1) (c : cur) acc cs
      | c `elem` (")]>}" :: String) =
          go (max 0 (depth - 1)) (c : cur) acc cs
      | otherwise =
          go depth (c : cur) acc cs

-- | Parse a structural record type literal @{ a: number; b: string }@ into 'TypeRecord'
-- with canonically sorted fields.
parseRecordLiteral :: Text -> StructuralType
parseRecordLiteral inner =
  let rawEntries = concatMap (splitTopLevel ',') (splitTopLevel ';' inner)
      parsedFields =
        [ (T.strip (T.dropWhileEnd (== '?') fName), parseTypeString fType)
        | entry <- rawEntries
        , let cleanEntry = T.strip entry
        , not (T.null cleanEntry)
        , (fName, rest) <- [T.breakOn ":" cleanEntry]
        , not (T.null rest)
        , let fType = T.drop 1 rest
        ]
  in makeRecord parsedFields

splitGoMapType :: Text -> Maybe (Text, Text)
splitGoMapType txt = go (1 :: Int) [] (T.unpack txt)
  where
    go _ _ [] = Nothing
    go depth kAcc (c:cs)
      | c == '[' = go (depth + 1) (c : kAcc) cs
      | c == ']' =
          if depth == 1
            then Just (T.strip (T.pack (reverse kAcc)), T.strip (T.pack cs))
            else go (depth - 1) (c : kAcc) cs
      | otherwise = go depth (c : kAcc) cs

parseGenericApplication :: Text -> Maybe (Text, [Text])
parseGenericApplication txt
  | T.isSuffixOf ">" txt, Just (base, inner) <- splitOpenDelim '<' (T.dropEnd 1 txt) =
      Just (T.strip base, filter (not . T.null) (splitTopLevel ',' inner))
  | T.isSuffixOf "]" txt, Just (base, inner) <- splitOpenDelim '[' (T.dropEnd 1 txt) =
      if T.null (T.strip base)
        then Nothing
        else Just (T.strip base, filter (not . T.null) (splitTopLevel ',' inner))
  | otherwise = Nothing
  where
    splitOpenDelim openC s =
      let (prefix, rest) = T.break (== openC) s
      in if T.null rest || not (T.all (\c -> isAlphaNum c || c == '_' || c == ':' || c == '.') (T.strip prefix))
           then Nothing
           else Just (prefix, T.drop 1 rest)

normalizeGenericConstruct :: Text -> [StructuralType] -> StructuralType
normalizeGenericConstruct base args
  | base `elem` ["Array", "List", "list", "Vec", "Slice", " ReadonlyArray", "Set", "set"]
  , [elemTy] <- args =
      if base `elem` ["Set", "set"]
        then TypeGeneric "Set" [elemTy]
        else TypeArray elemTy
  | base `elem` ["Optional", "Option"]
  , [innerTy] <- args =
      TypeOptional innerTy
  | base `elem` ["Promise", "Future"]
  , [innerTy] <- args =
      innerTy
  | base `elem` ["Dict", "dict", "Map", "HashMap", "BTreeMap", "Record"] =
      TypeGeneric "Map" args
  | otherwise =
      TypeGeneric base args

compactTypeSpacing :: Text -> Text
compactTypeSpacing = T.concat . T.words

-- ---------------------------------------------------------------------------
-- De Bruijn Generic Type Parameter Canonicalization ([T, U] -> [α0, α1])
-- ---------------------------------------------------------------------------

-- | Parse generic type parameter clauses (e.g. @["[K comparable, V any]"]@ or @["<T, U>"]@)
-- and return:
--   1. A substitution map from source type parameter names to de Bruijn variables (@α0, α1, ...@)
--   2. Canonical constraint fields @("tparam_α0", constraintType)@
normalizeGenericTypeParams :: [Text] -> (Map Text StructuralType, [(Text, StructuralType)])
normalizeGenericTypeParams rawHeaders =
  let parsedParams = concatMap parseHeaderParams rawHeaders
      indexed = zip [0 :: Int ..] parsedParams
      substMap =
        Map.fromList
          [ (pName, TypePrimitive ("α" <> T.pack (show idx)))
          | (idx, (pName, _)) <- indexed
          ]
      constraintFields =
        [ ("tparam_α" <> T.pack (show idx), substituteStructuralType substMap (parseTypeString c))
        | (idx, (_, Just c)) <- indexed
        , let cleanC = T.strip c
        , not (T.null cleanC)
        , cleanC `notElem` ["any", "interface{}", "unknown", "object", "Object"]
        ]
  in (substMap, constraintFields)

parseHeaderParams :: Text -> [(Text, Maybe Text)]
parseHeaderParams header =
  let clean = T.strip header
      inner
        | (T.isPrefixOf "[" clean && T.isSuffixOf "]" clean)
            || (T.isPrefixOf "<" clean && T.isSuffixOf ">" clean) =
            T.strip (T.drop 1 (T.dropEnd 1 clean))
        | otherwise = ""
      rawSegs = filter (not . T.null) (splitTopLevel ',' inner)
      rawPairs = mapMaybeParam (map parseSingleTypeParam rawSegs)
  in propagateGoSharedConstraints rawPairs
  where
    mapMaybeParam [] = []
    mapMaybeParam (Nothing : xs) = mapMaybeParam xs
    mapMaybeParam (Just p : xs)  = p : mapMaybeParam xs

    parseSingleTypeParam :: Text -> Maybe (Text, Maybe Text)
    parseSingleTypeParam seg =
      let s0 = T.strip seg
      in if T.null s0 || T.isPrefixOf "'" s0
           then Nothing
           else
             let s1 = T.dropWhile (\c -> c == '*' || c == ' ') s0
                 s2 = case T.stripPrefix "const " s1 of
                        Just r  -> T.strip r
                        Nothing -> s1
             in if ":" `T.isInfixOf` s2
                  then
                    let (pName, rest) = T.breakOn ":" s2
                        cStr = T.strip (T.drop 1 rest)
                    in Just (T.strip pName, if T.null cStr then Nothing else Just cStr)
                  else if " extends " `T.isInfixOf` s2
                    then
                      let (pName, rest) = T.breakOn " extends " s2
                          cStr = T.strip (T.drop 9 rest)
                      in Just (T.strip pName, if T.null cStr then Nothing else Just cStr)
                    else if "=" `T.isInfixOf` s2
                      then
                        let (pName, _) = T.breakOn "=" s2
                        in Just (T.strip pName, Nothing)
                      else
                        case T.words s2 of
                          []       -> Nothing
                          [pName]  -> Just (pName, Nothing)
                          (pName : cWords) -> Just (pName, Just (T.unwords cWords))

    -- In Go generics, [K, V comparable] shares the trailing constraint across K and V.
    propagateGoSharedConstraints :: [(Text, Maybe Text)] -> [(Text, Maybe Text)]
    propagateGoSharedConstraints [] = []
    propagateGoSharedConstraints [(n, mc)] = [(n, mc)]
    propagateGoSharedConstraints ((n, Nothing) : rest) =
      let rest' = propagateGoSharedConstraints rest
          sharedC = case rest' of
            ((_, Just c) : _) -> Just c
            _                 -> Nothing
      in (n, sharedC) : rest'
    propagateGoSharedConstraints (p : rest) =
      p : propagateGoSharedConstraints rest

-- | Recursively substitute generic type parameter identifiers within a 'StructuralType'
-- and re-normalize Unions, Intersections, and Records after substitution.
substituteStructuralType :: Map Text StructuralType -> StructuralType -> StructuralType
substituteStructuralType substMap = \case
  TypePrimitive p ->
    case Map.lookup p substMap of
      Just replacement -> replacement
      Nothing          -> TypePrimitive p
  TypeRecord fields ->
    makeRecord [(k, substituteStructuralType substMap v) | (k, v) <- fields]
  TypeFunction params ret ->
    TypeFunction
      (map (substituteStructuralType substMap) params)
      (substituteStructuralType substMap ret)
  TypeArray elemTy ->
    TypeArray (substituteStructuralType substMap elemTy)
  TypeUnion members ->
    makeUnion (map (substituteStructuralType substMap) members)
  TypeIntersection members ->
    makeIntersection (map (substituteStructuralType substMap) members)
  TypeOptional inner ->
    TypeOptional (substituteStructuralType substMap inner)
  TypeGeneric base args ->
    let base' = case Map.lookup base substMap of
          Just (TypePrimitive canonBase) -> canonBase
          _                              -> base
    in TypeGeneric base' (map (substituteStructuralType substMap) args)
  TypeRecVar d ->
    TypeRecVar d

applySubstToContract :: Map Text StructuralType -> [(Text, StructuralType)] -> InterfaceContract -> InterfaceContract
applySubstToContract substMap extraFields contract
  | Map.null substMap && null extraFields = contract
  | otherwise =
      let methods' =
            sortBy (comparing mcName)
              [ m
                  { mcParams = map (substituteStructuralType substMap) (mcParams m)
                  , mcReturn = substituteStructuralType substMap (mcReturn m)
                  }
              | m <- icMethods contract
              ]
          fields' =
            sortBy (comparing fst) $
              [ (k, substituteStructuralType substMap v)
              | (k, v) <- icFields contract
              ] ++ extraFields
      in contract { icMethods = methods', icFields = fields' }

-- ---------------------------------------------------------------------------
-- Contract Extraction & Normalization
-- ---------------------------------------------------------------------------

-- | Convert an IR Function into a normalized MethodContract.
normalizeMethodContract :: Function -> MethodContract
normalizeMethodContract fn =
  let (fnSubst, _) = normalizeGenericTypeParams (filter isGenericHeader (fnDecorators fn))
      normParams =
        map
          (substituteStructuralType fnSubst . parseTypeString . maybe "any" id . paramType)
          (fnParams fn)
      normRet =
        substituteStructuralType fnSubst (parseTypeString (maybe "any" id (fnReturnType fn)))
  in MethodContract
      { mcName    = fnName fn
      , mcParams  = normParams
      , mcReturn  = normRet
      , mcIsAsync = fnIsAsync fn
      }

isGenericHeader :: Text -> Bool
isGenericHeader txt =
  let clean = T.strip txt
  in (T.isPrefixOf "[" clean && T.isSuffixOf "]" clean)
     || (T.isPrefixOf "<" clean && T.isSuffixOf ">" clean)

splitNameAndGenericHeader :: Text -> (Text, [Text])
splitNameAndGenericHeader rawName =
  let clean = T.strip rawName
  in case T.break (\c -> c == '[' || c == '<') clean of
       (base, rest)
         | not (T.null rest) && isGenericHeader rest -> (T.strip base, [rest])
         | otherwise -> (clean, [])

-- | Normalize an IR Interface into a canonical InterfaceContract with sorted methods,
-- sorted fields, and de Bruijn normalized generic type parameters.
normalizeInterfaceContract :: Interface -> InterfaceContract
normalizeInterfaceContract iface =
  let (baseName, nameGenHeaders) = splitNameAndGenericHeader (ifName iface)
      (baseGenHeaders, regularBases) = partition isGenericHeader (ifBases iface)
      (substMap, tparamFields) = normalizeGenericTypeParams (nameGenHeaders ++ baseGenHeaders)
      rawMethods = map normalizeMethodContract (ifMethods iface)
      sortedMethods = sortBy (comparing mcName) rawMethods
      normBases = sortBy (comparing fst)
        [ ("constraint_" <> T.pack (show idx), parseTypeString b)
        | (idx, b) <- zip [1 :: Int ..] regularBases
        ]
      baseContract = InterfaceContract
        { icName    = baseName
        , icMethods = sortedMethods
        , icFields  = normBases
        }
  in applySubstToContract substMap tparamFields baseContract

-- | Extract all structural interface, trait, struct, class, and type alias contracts from a Program.
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
        let (baseName, nameGenHeaders) = splitNameAndGenericHeader (trName tr)
            (superGenHeaders, _) = partition isGenericHeader (trSuperTraits tr)
            (substMap, tparamFields) = normalizeGenericTypeParams (nameGenHeaders ++ superGenHeaders)
            rawMethods = map (normalizeMethodContract . normalizeGATMethod) (trMethods tr)
            sortedMethods = sortBy (comparing mcName) rawMethods
            baseContract = InterfaceContract baseName sortedMethods []
        in [applySubstToContract substMap tparamFields baseContract]

      DeclStruct st ->
        let (baseName, nameGenHeaders) = splitNameAndGenericHeader (stName st)
            (genMarkerFns, realMethods) = partition (isGenericHeader . fnName) (stMethods st)
            genHeaders = nameGenHeaders ++ map fnName genMarkerFns
            (substMap, tparamFields) = normalizeGenericTypeParams genHeaders
            normFields = sortBy (comparing fst)
              [ (fName, parseStructField baseName fTy)
              | (fName, fTy) <- stFields st
              ]
            normMethods = sortBy (comparing mcName)
              (map normalizeMethodContract realMethods)
            baseContract = InterfaceContract baseName normMethods normFields
        in [applySubstToContract substMap tparamFields baseContract]

      DeclTypeAlias name (Just def) ->
        let (baseName, nameGenHeaders) = splitNameAndGenericHeader name
            (defGenHeaders, rhsDef) = splitAliasDefGenerics def
            (substMap, tparamFields) = normalizeGenericTypeParams (nameGenHeaders ++ defGenHeaders)
            ty = substituteStructuralType substMap (parseTypeString rhsDef)
            fields = sortBy (comparing fst) (("type", ty) : tparamFields)
        in [InterfaceContract baseName [] fields]

      DeclClass cls ->
        let (baseName, nameGenHeaders) = splitNameAndGenericHeader (clsName cls)
            (decGenHeaders, _) = partition isGenericHeader (clsDecorators cls)
            (substMap, tparamFields) = normalizeGenericTypeParams (nameGenHeaders ++ decGenHeaders)
            normMethods = sortBy (comparing mcName)
              (map normalizeMethodContract (clsMethods cls))
            baseContract = InterfaceContract baseName normMethods []
        in if not (null (clsMethods cls)) || not (null ( nameGenHeaders ++ decGenHeaders ))
             then [applySubstToContract substMap tparamFields baseContract]
             else []

      _ -> []

splitAliasDefGenerics :: Text -> ([Text], Text)
splitAliasDefGenerics defText =
  let clean = T.strip defText
  in if T.isPrefixOf "[" clean || T.isPrefixOf "<" clean
       then case T.breakOn "=" clean of
              (beforeEq, afterEq)
                | not (T.null afterEq) && isGenericHeader (T.strip beforeEq) ->
                    ([T.strip beforeEq], T.strip (T.drop 1 afterEq))
              _ -> ([], clean)
       else ([], clean)

parseStructField :: Text -> Maybe Text -> StructuralType
parseStructField currentStruct mTy = case mTy of
  Nothing -> TypePrimitive "any"
  Just raw ->
    let clean = T.strip raw
        baseName = T.takeWhile (\c -> c /= '[' && c /= '<') (T.dropWhile (== '*') (T.dropWhile (== '&') clean))
    in if baseName == currentStruct
       then TypeRecVar 0
       else parseTypeString clean

normalizeGATMethod :: Function -> Function
normalizeGATMethod fn =
  let normName = normalizeLifetimes (fnName fn)
      normRet = fmap normalizeLifetimes (fnReturnType fn)
      normParams = map (\p -> p { paramType = fmap normalizeLifetimes (paramType p) }) (fnParams fn)
  in fn { fnName = normName, fnReturnType = normRet, fnParams = normParams }

normalizeLifetimes :: Text -> Text
normalizeLifetimes =
  T.replace "'a" "'0" . T.replace "'b" "'1" . T.replace "'c" "'2"

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
