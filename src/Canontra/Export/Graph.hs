{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Canontra.Export.Graph
Description : Graphviz DOT visualization generators for Call Graphs, CFGs, and DFGs.

Exports semantic graphs into standard Graphviz DOT representations for visual inspection,
compiler tooling, and documentation. Note: Mermaid export is excluded per design mandate.
-}
module Canontra.Export.Graph
  ( exportCallGraphDOT
  , exportCFGDOT
  , exportDFGDOT
  , escapeDOT
  , sanitizeDOTId
  ) where

import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

import Canontra.Analysis.CallGraph
  ( CallEdge (..)
  , CallGraph (..)
  , CalleeTarget (..)
  , CallerNode (..)
  )
import Canontra.Analysis.CFG
  ( BasicBlock (..)
  , BranchCondition (..)
  , CFGEdge (..)
  , ControlFlowGraph (..)
  )
import Canontra.Analysis.DFG
  ( DFGEdge (..)
  , DFGNode (..)
  , DataFlowGraph (..)
  , DefUseKind (..)
  )

-- | Escape characters for valid Graphviz DOT string literals.
escapeDOT :: Text -> Text
escapeDOT = T.concatMap escapeChar
  where
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = ""
    escapeChar c    = T.singleton c

-- | Sanitize a Text string into a valid DOT node identifier.
sanitizeDOTId :: Text -> Text
sanitizeDOTId = T.map cleanChar
  where
    cleanChar c
      | c >= 'a' && c <= 'z' = c
      | c >= 'A' && c <= 'Z' = c
      | c >= '0' && c <= '9' = c
      | otherwise            = '_'

-- ============================================================================
-- Call Graph DOT Export
-- ============================================================================

-- | Export a CallGraph into a standard Graphviz DOT format string.
exportCallGraphDOT :: CallGraph -> Text
exportCallGraphDOT cg =
  T.unlines $
    [ "digraph CallGraph {"
    , "  rankdir=LR;"
    , "  node [shape=box, fontname=\"Helvetica\", style=\"rounded,filled\", fillcolor=\"#f0f4f8\", color=\"#4a5568\"];"
    , "  edge [fontname=\"Helvetica\", color=\"#718096\"];"
    , ""
    ]
    ++ map declareNode allNodes
    ++ [ "" ]
    ++ map declareEdge (cgEdges cg)
    ++ [ "}" ]
  where
    allCallers = cgNodes cg
    allCallees = nub [edgeCallee e | e <- cgEdges cg]
    allNodes = map Left allCallers ++ [Right c | c <- allCallees, not (calleeIsCaller c)]

    calleeIsCaller (TargetLocal n) = CallFunction n `elem` allCallers
    calleeIsCaller _ = False

    nodeId (Left caller) = "caller_" <> sanitizeCaller caller
    nodeId (Right callee) = "callee_" <> sanitizeCallee callee

    sanitizeCaller CallTopLevel = "toplevel"
    sanitizeCaller (CallFunction fn) = "fn_" <> sanitizeDOTId fn
    sanitizeCaller (CallMethod cls m) = "method_" <> sanitizeDOTId cls <> "_" <> sanitizeDOTId m

    sanitizeCallee (TargetLocal n) = "local_" <> sanitizeDOTId n
    sanitizeCallee (TargetMethod cls m) = "meth_" <> sanitizeDOTId cls <> "_" <> sanitizeDOTId m
    sanitizeCallee (TargetImported m s) = "imp_" <> sanitizeDOTId m <> "_" <> sanitizeDOTId s
    sanitizeCallee (TargetDynamic _) = "dyn_" <> sanitizeDOTId "dynamic"

    labelCaller CallTopLevel = "<top-level>"
    labelCaller (CallFunction fn) = "def " <> fn
    labelCaller (CallMethod cls m) = cls <> "." <> m

    labelCallee (TargetLocal n) = n
    labelCallee (TargetMethod cls m) = if T.null cls then m else cls <> "." <> m
    labelCallee (TargetImported m s) = m <> "." <> s
    labelCallee (TargetDynamic _) = "<dynamic>"

    declareNode n@(Left caller) =
      "  \"" <> nodeId n <> "\" [label=\"" <> escapeDOT (labelCaller caller) <> "\"];"
    declareNode n@(Right callee) =
      "  \"" <> nodeId n <> "\" [label=\"" <> escapeDOT (labelCallee callee)
      <> "\", fillcolor=\"#edf2f7\", style=\"dashed,rounded,filled\"];"

    declareEdge (CallEdge caller callee cnt isAsync) =
      let srcId = nodeId (Left caller)
          dstId = if calleeIsCaller callee
                    then nodeId (Left (toCaller callee))
                    else nodeId (Right callee)
          cntLabel = if cnt > 1 then T.pack (show cnt) <> "x" else ""
          asyncLabel = if isAsync then "async" else ""
          edgeLabel = case (T.null cntLabel, T.null asyncLabel) of
            (True, True)   -> ""
            (False, True)  -> cntLabel
            (True, False)  -> asyncLabel
            (False, False) -> cntLabel <> ", " <> asyncLabel
          labelAttr = if T.null edgeLabel then "" else "label=\"" <> escapeDOT edgeLabel <> "\""
          asyncStyle = if isAsync then "style=\"dashed\", color=\"#3182ce\"" else ""
          attrs = filter (not . T.null) [labelAttr, asyncStyle]
          attrStr = if null attrs then "" else " [" <> T.intercalate ", " attrs <> "]"
      in "  \"" <> srcId <> "\" -> \"" <> dstId <> "\"" <> attrStr <> ";"

    toCaller (TargetLocal n) = CallFunction n
    toCaller _ = CallTopLevel

-- ============================================================================
-- Control-Flow Graph (CFG) DOT Export
-- ============================================================================

-- | Export a list of ControlFlowGraphs into a Graphviz DOT representation.
exportCFGDOT :: [ControlFlowGraph] -> Text
exportCFGDOT cfgs =
  T.unlines $
    [ "digraph ControlFlowGraph {"
    , "  rankdir=TB;"
    , "  node [fontname=\"Courier\", shape=box, style=\"filled\"];"
    , "  edge [fontname=\"Helvetica\", color=\"#4a5568\"];"
    , ""
    ]
    ++ concatMap formatSingleCFG (zip [(0 :: Int) ..] cfgs)
    ++ [ "}" ]
  where
    formatSingleCFG (idx, cfg) =
      let clusterName = "cluster_cfg_" <> T.pack (show idx)
          fnName = cfgFunction cfg
      in [ "  subgraph \"" <> clusterName <> "\" {"
         , "    label=\"" <> escapeDOT fnName <> "\";"
         , "    style=\"rounded\";"
         , "    color=\"#cbd5e0\";"
         , "    fillcolor=\"#f7fafc\";"
         , ""
         ]
         ++ map (formatBlock idx) (cfgBlocks cfg)
         ++ map (formatEdge idx) (cfgEdges cfg)
         ++ [ "  }"
            , ""
            ]

    blockId idx bId = "bb_" <> T.pack (show idx) <> "_" <> T.pack (show bId)

    formatBlock idx bb =
      let bIdent = blockId idx (bbId bb)
          bLabel = "bb" <> T.pack (show (bbId bb))
          stmtsCount = length (bbStatements bb)
          stmtInfo = if stmtsCount == 0 then "" else "\\n(" <> T.pack (show stmtsCount) <> " stmts)"
          color = if bbId bb == 0 then "#ebf8ff" else "#ffffff"
      in "    \"" <> bIdent <> "\" [label=\"" <> escapeDOT (bLabel <> stmtInfo)
         <> "\", fillcolor=\"" <> color <> "\"];"

    formatEdge idx edge =
      let src = blockId idx (edgeFrom edge)
          dst = blockId idx (edgeTo edge)
          condText = formatCondition (edgeCondition edge)
          attrStr = if T.null condText then "" else " [label=\"" <> escapeDOT condText <> "\"]"
      in "    \"" <> src <> "\" -> \"" <> dst <> "\"" <> attrStr <> ";"

    formatCondition (CondTrue _) = "True"
    formatCondition (CondFalse _) = "False"
    formatCondition (CondCase _) = "Case"
    formatCondition CondDefault = "Default"
    formatCondition CondUnconditional = ""
    formatCondition (CondException ex) = "catch " <> ex

-- ============================================================================
-- Data-Flow Graph (DFG) DOT Export
-- ============================================================================

-- | Export a list of DataFlowGraphs into a Graphviz DOT representation.
exportDFGDOT :: [DataFlowGraph] -> Text
exportDFGDOT dfgs =
  T.unlines $
    [ "digraph DataFlowGraph {"
    , "  rankdir=LR;"
    , "  node [fontname=\"Courier\", style=\"filled\"];"
    , "  edge [fontname=\"Helvetica\", color=\"#2b6cb0\"];"
    , ""
    ]
    ++ concatMap formatSingleDFG (zip [(0 :: Int) ..] dfgs)
    ++ [ "}" ]
  where
    formatSingleDFG (idx, dfg) =
      let clusterName = "cluster_dfg_" <> T.pack (show idx)
          fnName = dfgFunction dfg
      in [ "  subgraph \"" <> clusterName <> "\" {"
         , "    label=\"" <> escapeDOT fnName <> "\";"
         , "    style=\"rounded\";"
         , "    color=\"#e2e8f0\";"
         , "    fillcolor=\"#faf5ff\";"
         , ""
         ]
         ++ map (formatNode idx) (dfgNodes dfg)
         ++ map (formatEdge idx) (dfgEdges dfg)
         ++ [ "  }"
            , ""
            ]

    nodeIdent idx nId = "dfg_" <> T.pack (show idx) <> "_" <> T.pack (show nId)

    formatNode idx node =
      let nId = nodeIdent idx (dfgNodeId node)
          (nLabel, nShape, nFill, nBorder) = describeKind (dfgKind node)
      in "    \"" <> nId <> "\" [label=\"" <> escapeDOT nLabel
         <> "\", shape=" <> nShape <> ", fillcolor=\"" <> nFill
         <> "\", color=\"" <> nBorder <> "\"];"

    describeKind (DefParam i) =
      ("Param " <> T.pack (show i), "ellipse", "#e6fffa", "#319795")
    describeKind (DefAssignment var) =
      ("Def " <> var, "ellipse", "#e6fffa", "#319795")
    describeKind (DefPhi ids) =
      ("Phi " <> T.pack (show ids), "diamond", "#faf5ff", "#805ad5")
    describeKind (UseRead var) =
      ("Use " <> var, "box", "#ebf8ff", "#3182ce")
    describeKind (UseArgument i) =
      ("Arg " <> T.pack (show i), "box", "#ebf8ff", "#3182ce")
    describeKind UseBranchGuard =
      ("Guard", "hexagon", "#fffaf0", "#dd6b20")

    formatEdge idx edge =
      let src = nodeIdent idx (dfgSource edge)
          dst = nodeIdent idx (dfgTarget edge)
          var = dfgVarName edge
          attrStr = if T.null var then "" else " [label=\"" <> escapeDOT var <> "\"]"
      in "    \"" <> src <> "\" -> \"" <> dst <> "\"" <> attrStr <> ";"
