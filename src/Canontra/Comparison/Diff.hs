{- |
Module      : Canontra.Comparison.Diff
Description : Fine-grained structural diff diagnostics engine for v0.0.3-alpha.

This module analyzes semantic and structural divergences between two programs.
It generates actionable, multi-tier diagnostics explaining differences across
declarations, imported dependencies, computational logic, call graphs, CFGs, and DFGs.
-}
module Canontra.Comparison.Diff
  ( DeclDiff (..)
  , DepDiff (..)
  , StructuralDiff (..)
  , CallGraphDiff (..)
  , CFGDiff (..)
  , DFGDiff (..)
  , DiffResult (..)
  , diffPrograms
  , formatDiffResult
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON (..), ToJSON (..), object, (.=))
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Canontra.Analysis.CallGraph
import Canontra.Analysis.CFG (buildCFGs, cfgBlocks, cfgEdges, cfgFunction)
import Canontra.Analysis.DFG (buildDFGs, dfgEdges, dfgFunction, dfgNodes)
import Canontra.Fingerprint.CallGraph (computeFCG)
import Canontra.Fingerprint.Composite (computeF4)
import Canontra.Fingerprint.ControlFlow (computeFCF)
import Canontra.Fingerprint.DataFlow (computeFDF)
import Canontra.Fingerprint.Declaration (computeF2)
import Canontra.Fingerprint.Dependency (computeF3, extractRichDependencyGraph)
import Canontra.Fingerprint.Structural (computeF1)
import Canontra.Fingerprint.TypeContract (computeFT)
import Canontra.IR.Declaration
import Canontra.IR.Dependency
import Canontra.IR.Program
import Canontra.Normalize.Normalize (normalizeProgram)
import Canontra.Types

data DeclDiff = DeclDiff
  { ddAction  :: Text
  , ddTarget  :: Text
  , ddDetails :: Text
  } deriving stock (Eq, Ord, Show, Generic)

instance ToJSON DeclDiff where
  toJSON dd = object
    [ "action"  .= ddAction dd
    , "target"  .= ddTarget dd
    , "details" .= ddDetails dd
    ]

instance FromJSON DeclDiff where
  parseJSON = Aeson.withObject "DeclDiff" $ \o ->
    DeclDiff <$> o Aeson..: "action" <*> o Aeson..: "target" <*> o Aeson..: "details"

data DepDiff = DepDiff
  { depAction :: Text
  , depModule :: Text
  , depSymbol :: Maybe Text
  , depUsage  :: Text
  } deriving stock (Eq, Ord, Show, Generic)

instance ToJSON DepDiff where
  toJSON dd = object
    [ "action" .= depAction dd
    , "module" .= depModule dd
    , "symbol" .= depSymbol dd
    , "usage"  .= depUsage dd
    ]

instance FromJSON DepDiff where
  parseJSON = Aeson.withObject "DepDiff" $ \o ->
    DepDiff <$> o Aeson..: "action" <*> o Aeson..: "module" <*> o Aeson..:? "symbol" <*> o Aeson..: "usage"

data StructuralDiff = StructuralDiff
  { sdTarget :: Text
  , sdKind   :: Text
  , sdDetail :: Text
  } deriving stock (Eq, Ord, Show, Generic)

instance ToJSON StructuralDiff where
  toJSON sd = object
    [ "target" .= sdTarget sd
    , "kind"   .= sdKind sd
    , "detail" .= sdDetail sd
    ]

instance FromJSON StructuralDiff where
  parseJSON = Aeson.withObject "StructuralDiff" $ \o ->
    StructuralDiff <$> o Aeson..: "target" <*> o Aeson..: "kind" <*> o Aeson..: "detail"

data CallGraphDiff = CallGraphDiff
  { cgdCaller :: Text
  , cgdAction :: Text
  , cgdCallee :: Text
  } deriving stock (Eq, Ord, Show, Generic)

instance ToJSON CallGraphDiff where
  toJSON cgd = object
    [ "caller" .= cgdCaller cgd
    , "action" .= cgdAction cgd
    , "callee" .= cgdCallee cgd
    ]

instance FromJSON CallGraphDiff where
  parseJSON = Aeson.withObject "CallGraphDiff" $ \o ->
    CallGraphDiff <$> o Aeson..: "caller" <*> o Aeson..: "action" <*> o Aeson..: "callee"

data CFGDiff = CFGDiff
  { cfgdFunction :: Text
  , cfgdAction   :: Text
  , cfgdDetail   :: Text
  } deriving stock (Eq, Ord, Show, Generic)

instance ToJSON CFGDiff where
  toJSON cd = object
    [ "function" .= cfgdFunction cd
    , "action"   .= cfgdAction cd
    , "detail"   .= cfgdDetail cd
    ]

instance FromJSON CFGDiff where
  parseJSON = Aeson.withObject "CFGDiff" $ \o ->
    CFGDiff <$> o Aeson..: "function" <*> o Aeson..: "action" <*> o Aeson..: "detail"

data DFGDiff = DFGDiff
  { dfgdFunction :: Text
  , dfgdAction   :: Text
  , dfgdDetail   :: Text
  } deriving stock (Eq, Ord, Show, Generic)

instance ToJSON DFGDiff where
  toJSON dd = object
    [ "function" .= dfgdFunction dd
    , "action"   .= dfgdAction dd
    , "detail"   .= dfgdDetail dd
    ]

instance FromJSON DFGDiff where
  parseJSON = Aeson.withObject "DFGDiff" $ \o ->
    DFGDiff <$> o Aeson..: "function" <*> o Aeson..: "action" <*> o Aeson..: "detail"

data DiffResult = DiffResult
  { drComparison       :: ComparisonResult
  , drDeclarationDiffs :: [DeclDiff]
  , drDependencyDiffs  :: [DepDiff]
  , drStructuralDiffs  :: [StructuralDiff]
  , drCallGraphDiffs   :: [CallGraphDiff]
  , drCFGDiffs         :: [CFGDiff]
  , drDFGDiffs         :: [DFGDiff]
  } deriving stock (Eq, Show, Generic)

instance ToJSON DiffResult where
  toJSON dr = object
    [ "comparison" .= drComparison dr
    , "diagnostics" .= object
        [ "declaration_diffs" .= drDeclarationDiffs dr
        , "dependency_diffs"  .= drDependencyDiffs dr
        , "structural_diffs"  .= drStructuralDiffs dr
        , "call_graph_diffs"  .= drCallGraphDiffs dr
        , "cfg_diffs"         .= drCFGDiffs dr
        , "dfg_diffs"         .= drDFGDiffs dr
        ]
    ]

instance FromJSON DiffResult where
  parseJSON = Aeson.withObject "DiffResult" $ \o -> do
    comp <- o Aeson..: "comparison"
    diag <- o Aeson..: "diagnostics"
    declDiffs   <- diag Aeson..: "declaration_diffs"
    depDiffs    <- diag Aeson..: "dependency_diffs"
    structDiffs <- diag Aeson..: "structural_diffs"
    cgDiffs     <- diag Aeson..: "call_graph_diffs"
    cfgDiffs    <- diag Aeson..:? "cfg_diffs" Aeson..!= []
    dfgDiffs    <- diag Aeson..:? "dfg_diffs" Aeson..!= []
    pure (DiffResult comp declDiffs depDiffs structDiffs cgDiffs cfgDiffs dfgDiffs)

diffPrograms :: Program -> Program -> DiffResult
diffPrograms p1 p2 =
  let np1 = normalizeProgram p1
      np2 = normalizeProgram p2
      f1_1 = computeF1 np1; f1_2 = computeF1 np2
      f2_1 = computeF2 np1; f2_2 = computeF2 np2
      f3_1 = computeF3 np1; f3_2 = computeF3 np2
      fcg_1 = computeFCG np1; fcg_2 = computeFCG np2
      fcf_1 = computeFCF np1; fcf_2 = computeFCF np2
      fdf_1 = computeFDF np1; fdf_2 = computeFDF np2
      ft_1  = computeFT np1;  ft_2  = computeFT np2
      f4_1  = computeF4 f1_1 f2_1 f3_1 fcg_1 fcf_1 fdf_1 ft_1
      f4_2  = computeF4 f1_2 f2_2 f3_2 fcg_2 fcf_2 fdf_2 ft_2

      cr = ComparisonResult
        { crSource       = Different
        , crStructural   = if f1_1 == f1_2 then Identical else Different
        , crDeclaration  = if f2_1 == f2_2 then Identical else Different
        , crDependency   = if f3_1 == f3_2 then Identical else Different
        , crCallGraph    = if fcg_1 == fcg_2 then Identical else Different
        , crControlFlow  = if fcf_1 == fcf_2 then Identical else Different
        , crDataFlow     = if fdf_1 == fdf_2 then Identical else Different
        , crTypeContract = if ft_1 == ft_2 then Identical else Different
        , crComposite    = if f4_1 == f4_2 then Identical else Different
        }

      declDiffs = diffDeclarations np1 np2
      depDiffs = diffDependencies np1 np2
      structDiffs = diffStructures np1 np2
      cgDiffs = diffCallGraphs np1 np2
      cfgDiffs = diffCFGs np1 np2
      dfgDiffs = diffDFGs np1 np2
  in DiffResult cr declDiffs depDiffs structDiffs cgDiffs cfgDiffs dfgDiffs

diffDeclarations :: Program -> Program -> [DeclDiff]
diffDeclarations (Program m1 _) (Program m2 _) =
  let decls1 = concatMap modDeclarations m1
      decls2 = concatMap modDeclarations m2
      map1 = Map.fromList [ (declName d, d) | d <- decls1 ]
      map2 = Map.fromList [ (declName d, d) | d <- decls2 ]
      names1 = Map.keysSet map1
      names2 = Map.keysSet map2
      removed = [ DeclDiff "removed" (declLabel d) "Declaration removed" | name <- Set.toList (Set.difference names1 names2), Just d <- [Map.lookup name map1] ]
      added   = [ DeclDiff "added" (declLabel d) "Declaration added" | name <- Set.toList (Set.difference names2 names1), Just d <- [Map.lookup name map2] ]
      modified = concat [ checkDeclModified d1 d2 | name <- Set.toList (Set.intersection names1 names2), Just d1 <- [Map.lookup name map1], Just d2 <- [Map.lookup name map2] ]
  in sort (removed ++ added ++ modified)
  where
    declName (DeclFunction fn) = "fn:" <> fnName fn
    declName (DeclClass cls)   = "cls:" <> clsName cls
    declName (DeclStruct st)   = "st:" <> stName st
    declName (DeclVariable v _) = "var:" <> v
    declName (DeclInterface i) = "if:" <> ifName i
    declName (DeclTrait t)     = "tr:" <> trName t
    declName (DeclImpl imp)    = "imp:" <> impTarget imp
    declName (DeclReceiver r f) = "rc:" <> rcTypeName r <> "." <> fnName f
    declName (DeclTypeAlias a _) = "alias:" <> a

    declLabel (DeclFunction fn) = "Function: " <> fnName fn
    declLabel (DeclClass cls)   = "Class: " <> clsName cls
    declLabel (DeclStruct st)   = "Struct: " <> stName st
    declLabel (DeclVariable v _) = "Variable: " <> v
    declLabel (DeclInterface i) = "Interface: " <> ifName i
    declLabel (DeclTrait t)     = "Trait: " <> trName t
    declLabel (DeclImpl imp)    = "Impl: " <> impTarget imp
    declLabel (DeclReceiver r f) = "Receiver: " <> rcTypeName r <> "." <> fnName f
    declLabel (DeclTypeAlias a _) = "TypeAlias: " <> a

    checkDeclModified (DeclFunction f1) (DeclFunction f2)
      | fnParams f1 /= fnParams f2 || fnReturnType f1 /= fnReturnType f2 || fnDecorators f1 /= fnDecorators f2 || fnIsAsync f1 /= fnIsAsync f2 =
        [DeclDiff "signature_modified" ("Function: " <> fnName f1) "Function signature altered"]
      | otherwise = []
    checkDeclModified (DeclClass c1) (DeclClass c2)
      | clsBases c1 /= clsBases c2 || clsDecorators c1 /= clsDecorators c2 =
        [DeclDiff "signature_modified" ("Class: " <> clsName c1) "Class bases altered"]
      | otherwise = []
    checkDeclModified _ _ = []

diffDependencies :: Program -> Program -> [DepDiff]
diffDependencies p1 p2 =
  let rdg1 = extractRichDependencyGraph p1
      rdg2 = extractRichDependencyGraph p2
      exts1 = Map.fromList [ (impKey i, i) | i <- rdExternalImports rdg1 ]
      exts2 = Map.fromList [ (impKey i, i) | i <- rdExternalImports rdg2 ]
      k1 = Map.keysSet exts1
      k2 = Map.keysSet exts2
      removed = [ DepDiff "removed" (impModule i) (impSymbol i) (formatUsage (impUsage i)) | k <- Set.toList (Set.difference k1 k2), Just i <- [Map.lookup k exts1] ]
      added   = [ DepDiff "added" (impModule i) (impSymbol i) (formatUsage (impUsage i)) | k <- Set.toList (Set.difference k2 k1), Just i <- [Map.lookup k exts2] ]
      changed = [ DepDiff "usage_changed" (impModule i2) (impSymbol i2) (formatUsage (impUsage i2))
                | k <- Set.toList (Set.intersection k1 k2)
                , Just i1 <- [Map.lookup k exts1]
                , Just i2 <- [Map.lookup k exts2]
                , impUsage i1 /= impUsage i2
                ]
  in sort (removed ++ added ++ changed)
  where
    impKey i = (impModule i, impSymbol i, impAlias i)
    formatUsage = \case
      DepUnused           -> "unused"
      DepDirectCall _     -> "direct_call"
      DepInheritance _    -> "inheritance"
      DepTypeOnly _       -> "type_only"
      DepValueRef _       -> "value_ref"

diffStructures :: Program -> Program -> [StructuralDiff]
diffStructures (Program m1 _) (Program m2 _) =
  let decls1 = concatMap modDeclarations m1
      decls2 = concatMap modDeclarations m2
      map1 = Map.fromList [ (fnName f, f) | DeclFunction f <- decls1 ]
      map2 = Map.fromList [ (fnName f, f) | DeclFunction f <- decls2 ]
      commonFns = Set.intersection (Map.keysSet map1) (Map.keysSet map2)
      fnDiffs = [ StructuralDiff ("Function: " <> name) "body_logic_modified" "Function body logic altered"
                | name <- Set.toList commonFns
                , Just f1 <- [Map.lookup name map1]
                , Just f2 <- [Map.lookup name map2]
                , fnBody f1 /= fnBody f2
                ]
  in fnDiffs

diffCallGraphs :: Program -> Program -> [CallGraphDiff]
diffCallGraphs p1 p2 =
  let cg1 = buildCallGraph p1
      cg2 = buildCallGraph p2
      set1 = Set.fromList [ (formatCaller (edgeCaller e), formatCallee (edgeCallee e)) | e <- cgEdges cg1 ]
      set2 = Set.fromList [ (formatCaller (edgeCaller e), formatCallee (edgeCallee e)) | e <- cgEdges cg2 ]
      removed = [ CallGraphDiff c "edge_removed" t | (c, t) <- Set.toList (Set.difference set1 set2) ]
      added   = [ CallGraphDiff c "edge_added" t   | (c, t) <- Set.toList (Set.difference set2 set1) ]
  in sort (removed ++ added)
  where
    formatCaller CallTopLevel = "<top-level>"
    formatCaller (CallFunction fn) = fn
    formatCaller (CallMethod cls m) = cls <> "." <> m

    formatCallee (TargetLocal name) = name
    formatCallee (TargetMethod cls m) = if T.null cls then m else cls <> "." <> m
    formatCallee (TargetImported m s) = m <> "." <> s
    formatCallee (TargetDynamic _) = "<dynamic>"

diffCFGs :: Program -> Program -> [CFGDiff]
diffCFGs p1 p2 =
  let cfgs1 = Map.fromList [ (cfgFunction c, c) | c <- buildCFGs p1 ]
      cfgs2 = Map.fromList [ (cfgFunction c, c) | c <- buildCFGs p2 ]
      common = Set.intersection (Map.keysSet cfgs1) (Map.keysSet cfgs2)
  in [ CFGDiff fn "cfg_topology_changed" "Control-flow basic blocks or edges modified"
     | fn <- Set.toList common
     , Just c1 <- [Map.lookup fn cfgs1]
     , Just c2 <- [Map.lookup fn cfgs2]
     , (length (cfgBlocks c1), length (cfgEdges c1)) /= (length (cfgBlocks c2), length (cfgEdges c2))
     ]

diffDFGs :: Program -> Program -> [DFGDiff]
diffDFGs p1 p2 =
  let dfgs1 = Map.fromList [ (dfgFunction d, d) | d <- buildDFGs p1 ]
      dfgs2 = Map.fromList [ (dfgFunction d, d) | d <- buildDFGs p2 ]
      common = Set.intersection (Map.keysSet dfgs1) (Map.keysSet dfgs2)
  in [ DFGDiff fn "dfg_flow_changed" "Data-flow reaching definitions modified"
     | fn <- Set.toList common
     , Just d1 <- [Map.lookup fn dfgs1]
     , Just d2 <- [Map.lookup fn dfgs2]
     , (length (dfgNodes d1), length (dfgEdges d1)) /= (length (dfgNodes d2), length (dfgEdges d2))
     ]

formatDiffResult :: DiffResult -> Text
formatDiffResult dr =
  T.unlines $
    [ "canontra semantic diff diagnostics"
    , "==================================="
    , ""
    , "Tier Comparison:"
    , "  Structural:   " <> showStatus (crStructural (drComparison dr))
    , "  Declaration:  " <> showStatus (crDeclaration (drComparison dr))
    , "  Dependency:   " <> showStatus (crDependency (drComparison dr))
    , "  Call Graph:   " <> showStatus (crCallGraph (drComparison dr))
    , "  Control Flow: " <> showStatus (crControlFlow (drComparison dr))
    , "  Data Flow:    " <> showStatus (crDataFlow (drComparison dr))
    , "  Composite:    " <> showStatus (crComposite (drComparison dr))
    , ""
    ] ++
    formatSection "Declaration Changes" (map formatDeclDiff (drDeclarationDiffs dr)) ++
    formatSection "Dependency Changes" (map formatDepDiff (drDependencyDiffs dr)) ++
    formatSection "Structural Logic Changes" (map formatStructDiff (drStructuralDiffs dr)) ++
    formatSection "Call Graph Edge Changes" (map formatCGDiff (drCallGraphDiffs dr)) ++
    formatSection "Control-Flow Graph Changes" (map formatCFGDiff (drCFGDiffs dr)) ++
    formatSection "Data-Flow Graph Changes" (map formatDFGDiff (drDFGDiffs dr))
  where
    showStatus Identical = "IDENTICAL"
    showStatus Different = "DIFFERENT"

    formatSection title items =
      if null items
        then []
        else [title <> ":"] ++ map ("  - " <>) items ++ [""]

    formatDeclDiff dd = "[" <> ddAction dd <> "] " <> ddTarget dd <> " (" <> ddDetails dd <> ")"
    formatDepDiff dd = "[" <> depAction dd <> "] " <> depModule dd <> maybe "" ("." <>) (depSymbol dd) <> " [" <> depUsage dd <> "]"
    formatStructDiff sd = "[" <> sdKind sd <> "] " <> sdTarget sd <> " (" <> sdDetail sd <> ")"
    formatCGDiff cgd = "[" <> cgdAction cgd <> "] " <> cgdCaller cgd <> " --> " <> cgdCallee cgd
    formatCFGDiff cd = "[" <> cfgdAction cd <> "] " <> cfgdFunction cd <> " (" <> cfgdDetail cd <> ")"
    formatDFGDiff dd = "[" <> dfgdAction dd <> "] " <> dfgdFunction dd <> " (" <> dfgdDetail dd <> ")"
