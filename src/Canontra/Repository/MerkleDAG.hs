{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Canontra.Repository.MerkleDAG
Description : Isomorphic Incremental Merkle DAG Repository Engine.

Constructs an explicit hierarchical Merkle Directed Acyclic Graph (DAG) for repository
file trees. Enables O(1) subtree skipping during incremental fingerprinting and
lightning-fast structural repository diffing by evaluating directory-level hash digests.
-}
module Canontra.Repository.MerkleDAG
  ( MerkleDAGNode (..)
  , buildMerkleDAG
  , merkleDAGRootHash
  , diffMerkleDAG
  , flattenMerkleDAG
  , dagNodeCount
  , hotUpdateMerkleDAG
  , removeMerkleDAGLeaf
  , updateMerkleDAGLeaf
  ) where

import Control.DeepSeq (NFData)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import Data.List (groupBy, partition, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Text.Printf (printf)

import Canontra.Types (Fingerprint (..), FingerprintBundle (..))

-- | Node in the hierarchical Merkle DAG.
data MerkleDAGNode
  = MerkleFile !FilePath !FingerprintBundle
  | MerkleDirectory !FilePath !Fingerprint ![MerkleDAGNode]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Total number of nodes in the DAG.
dagNodeCount :: MerkleDAGNode -> Int
dagNodeCount (MerkleFile _ _) = 1
dagNodeCount (MerkleDirectory _ _ children) = 1 + sum (map dagNodeCount children)

-- | Get the root fingerprint of any DAG node.
merkleDAGRootHash :: MerkleDAGNode -> Fingerprint
merkleDAGRootHash (MerkleFile _ bundle) = f4Composite bundle
merkleDAGRootHash (MerkleDirectory _ fp _) = fp

-- | Build a hierarchical Merkle DAG from a list of sorted relative file paths and bundles.
buildMerkleDAG :: [(FilePath, FingerprintBundle)] -> MerkleDAGNode
buildMerkleDAG [] = MerkleDirectory "" (Fingerprint "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855") []
buildMerkleDAG entries =
  let parsedEntries = [(splitPathSegments (normalizePosix p), p, b) | (p, b) <- entries]
  in buildDirDAG "" parsedEntries

-- | Build a directory DAG recursively by segment grouping.
buildDirDAG :: FilePath -> [([String], FilePath, FingerprintBundle)] -> MerkleDAGNode
buildDirDAG dirPath items =
  let directFiles = [MerkleFile origPath b | ([_fileName], origPath, b) <- items]
      nestedItems = [(seg, (rest, origPath, b)) | (seg : rest@(_ : _), origPath, b) <- items]
      groupedNested = groupBy (\(s1, _) (s2, _) -> s1 == s2) (sortOn fst nestedItems)
      subDirs =
        [ let segName = fst (head grp)
              subDirPath = if null dirPath then segName else dirPath ++ "/" ++ segName
              childItems = map snd grp
          in buildDirDAG subDirPath childItems
        | grp <- groupedNested
        ]
      allChildren = sortOn nodePath (directFiles ++ subDirs)
      dirDigest = computeDirDigest allChildren
  in MerkleDirectory dirPath dirDigest allChildren

nodePath :: MerkleDAGNode -> FilePath
nodePath (MerkleFile p _) = p
nodePath (MerkleDirectory p _ _) = p

-- | Compute directory digest by hashing sorted children (name + child digest).
computeDirDigest :: [MerkleDAGNode] -> Fingerprint
computeDirDigest children =
  let childBytes = mconcat
        [ let nameBS = TE.encodeUtf8 (T.pack (nodePath child))
              (Fingerprint digestTxt) = merkleDAGRootHash child
              digestBS = TE.encodeUtf8 digestTxt
          in nameBS <> ":" <> digestBS <> "\n"
        | child <- children
        ]
      digest = SHA256.hash childBytes
      hexStr = concatMap (printf "%02x") (BS.unpack digest)
  in Fingerprint (T.pack hexStr)

-- | O(k) structural diff between two Merkle DAGs, pruning identical subtrees instantly.
diffMerkleDAG :: MerkleDAGNode -> MerkleDAGNode -> [FilePath]
diffMerkleDAG n1 n2
  | merkleDAGRootHash n1 == merkleDAGRootHash n2 = []
  | otherwise = case (n1, n2) of
      (MerkleFile p1 _, MerkleFile p2 _) ->
        if p1 == p2 then [p1] else [p1, p2]
      (MerkleFile p1 _, MerkleDirectory _ _ _) -> [p1]
      (MerkleDirectory _ _ _, MerkleFile p2 _) -> [p2]
      (MerkleDirectory _ _ c1, MerkleDirectory _ _ c2) ->
        let m1 = Map.fromList [(nodePath c, c) | c <- c1]
            m2 = Map.fromList [(nodePath c, c) | c <- c2]
        in concatMap (\k -> case (Map.lookup k m1, Map.lookup k m2) of
            (Just child1, Just child2) -> diffMerkleDAG child1 child2
            (Just child1, Nothing)     -> map fst (flattenMerkleDAG child1)
            (Nothing, Just child2)     -> map fst (flattenMerkleDAG child2)
            (Nothing, Nothing)         -> []
          ) (Map.keys m1 ++ [k | k <- Map.keys m2, not (Map.member k m1)])

-- | Flatten all file entries in a DAG.
flattenMerkleDAG :: MerkleDAGNode -> [(FilePath, FingerprintBundle)]
flattenMerkleDAG (MerkleFile p b) = [(p, b)]
flattenMerkleDAG (MerkleDirectory _ _ children) = concatMap flattenMerkleDAG children

normalizePosix :: FilePath -> FilePath
normalizePosix = map (\c -> if c == '\\' then '/' else c)

splitPathSegments :: FilePath -> [String]
splitPathSegments p = filter (not . null) (splitOnChar '/' p)

splitOnChar :: Char -> String -> [String]
splitOnChar _ "" = []
splitOnChar delim str =
  let (before, rest) = break (== delim) str
  in before : case rest of
       [] -> []
       (_:after) -> splitOnChar delim after

-- | In-place hot mutation of a Merkle DAG leaf node in O(log N) / O(depth) time.
-- Traverses solely along the ancestor path to the root, updating directory digests,
-- leaving all sibling branches untouched. By Theorem 5, the resulting root hash is
-- strictly bit-identical to rebuilding the entire Merkle DAG from scratch.
hotUpdateMerkleDAG :: MerkleDAGNode -> FilePath -> FingerprintBundle -> MerkleDAGNode
hotUpdateMerkleDAG dag path bundle = updateMerkleDAGLeaf dag path (Just bundle)

-- | In-place removal of a Merkle DAG leaf node in O(log N) / O(depth) time.
removeMerkleDAGLeaf :: MerkleDAGNode -> FilePath -> MerkleDAGNode
removeMerkleDAGLeaf dag path = updateMerkleDAGLeaf dag path Nothing

-- | General leaf mutation (insert, update, or delete).
updateMerkleDAGLeaf :: MerkleDAGNode -> FilePath -> Maybe FingerprintBundle -> MerkleDAGNode
updateMerkleDAGLeaf root path mBundle =
  let normPath = normalizePosix path
      segments = splitPathSegments normPath
  in case root of
       MerkleFile p _ ->
         case mBundle of
           Just b  -> MerkleFile p b
           Nothing -> MerkleDirectory "" (Fingerprint "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855") []
       MerkleDirectory dirPath _ children ->
         updateDir segments dirPath children
  where
    updateDir [] curDirPath children =
      let newDigest = computeDirDigest children
      in MerkleDirectory curDirPath newDigest children

    updateDir [fileName] curDirPath children =
      let origPath = if null curDirPath then fileName else curDirPath ++ "/" ++ fileName
          newChildren = case mBundle of
            Just b ->
              let updatedFile = MerkleFile origPath b
                  otherChildren = filter (\c -> nodePath c /= origPath) children
              in sortOn nodePath (updatedFile : otherChildren)
            Nothing ->
              filter (\c -> nodePath c /= origPath) children
          newDigest = computeDirDigest newChildren
      in MerkleDirectory curDirPath newDigest newChildren

    updateDir (seg : restSegs) curDirPath children =
      let subDirPath = if null curDirPath then seg else curDirPath ++ "/" ++ seg
          (existingSubDir, otherChildren) = partition (\c -> nodePath c == subDirPath) children
          updatedSubDir = case existingSubDir of
            (MerkleDirectory _ _ subChildren : _) ->
              updateDir restSegs subDirPath subChildren
            _ ->
              case mBundle of
                Just _  -> updateDir restSegs subDirPath []
                Nothing -> MerkleDirectory subDirPath (Fingerprint "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855") []
          newChildren = case updatedSubDir of
            MerkleDirectory _ _ [] | mBundle == Nothing ->
              otherChildren
            _ ->
              sortOn nodePath (updatedSubDir : otherChildren)
          newDigest = computeDirDigest newChildren
      in MerkleDirectory curDirPath newDigest newChildren
