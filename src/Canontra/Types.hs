{- |
Module      : Canontra.Types
Description : Core domain types and result representations for v0.0.4-alpha.

This module defines the essential vocabulary of canontra v0.0.4-alpha:
8-tier fingerprint bundles (F0, F1, F2, F3, F_CG, F_CF, F_DF, F4), polyglot language tags,
structured diagnostics, comparison results, and output manifests.
All types derive NFData to guarantee space-leak-free execution.
-}
{-# LANGUAGE DerivingStrategies #-}
module Canontra.Types
  ( HashAlgorithm (..)
  , LanguageTag (..)
  , languageTagText
  , parseLanguageTag
  , Fingerprint (..)
  , FingerprintBundle (..)
  , ComparisonStatus (..)
  , ComparisonResult (..)
  , VerificationResult (..)
  , ParseError (..)
  , Manifest (..)
  , ManifestMetadata (..)
  , FileEntry (..)
  , RepositoryManifest (..)
  , EvolutionComparison (..)
  , ParamKind (..)
  , Parameter (..)
  , DeclKind (..)
  , GlobalSymbol (..)
  , WholeRepoCallEdge (..)
  , WholeRepoCallGraph (..)
  , InterProceduralDataFlowEdge (..)
  , WholeRepoDataFlowGraph (..)
  , WholeRepoBundle (..)
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), object, withText, (.=))
import Control.DeepSeq (NFData)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data HashAlgorithm = SHA256
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON HashAlgorithm where
  toJSON SHA256 = String "sha256"

instance FromJSON HashAlgorithm where
  parseJSON = withText "HashAlgorithm" $ \t ->
    if T.toLower t == "sha256" then pure SHA256 else fail "Unsupported hash algorithm"

data LanguageTag
  = LangPython
  | LangJavaScript
  | LangTypeScript
  | LangGo
  | LangRust
  | LangUnknown Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

languageTagText :: LanguageTag -> Text
languageTagText = \case
  LangPython     -> "python"
  LangJavaScript -> "javascript"
  LangTypeScript -> "typescript"
  LangGo         -> "go"
  LangRust       -> "rust"
  LangUnknown t  -> t

parseLanguageTag :: Text -> LanguageTag
parseLanguageTag t = case T.toLower t of
  "python"     -> LangPython
  "py"         -> LangPython
  "javascript" -> LangJavaScript
  "js"         -> LangJavaScript
  "typescript" -> LangTypeScript
  "ts"         -> LangTypeScript
  "go"         -> LangGo
  "rust"       -> LangRust
  "rs"         -> LangRust
  other        -> LangUnknown other

instance ToJSON LanguageTag where
  toJSON = String . languageTagText

instance FromJSON LanguageTag where
  parseJSON = withText "LanguageTag" (pure . parseLanguageTag)

newtype Fingerprint = Fingerprint { unFingerprint :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, NFData)

data FingerprintBundle = FingerprintBundle
  { f0Source       :: Fingerprint -- e.g. F0: raw source text hash
  , f1Structural   :: Fingerprint -- e.g. F1: AST identity after normalization
  , f2Declaration  :: Fingerprint -- e.g. F2: declaration hierarchy hash
  , f3Dependency   :: Fingerprint -- e.g. F3: import & dependency graph hash
  , fCGCallGraph   :: Fingerprint -- e.g. F_CG: intra-module call graph topology hash
  , fCFControlFlow :: Fingerprint -- e.g. F_CF: control-flow graph topology hash
  , fDFDataFlow    :: Fingerprint -- e.g. F_DF: data-flow graph Def-Use chain hash
  , fTTypeContract :: Fingerprint -- e.g. F_T: structural type contract hash
  , f4Composite    :: Fingerprint -- e.g. F4: combined 9-tier composite hash
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON FingerprintBundle where
  toJSON fb = object
    [ "source"        .= f0Source fb
    , "structural"    .= f1Structural fb
    , "declaration"   .= f2Declaration fb
    , "dependency"    .= f3Dependency fb
    , "call_graph"    .= fCGCallGraph fb
    , "control_flow"  .= fCFControlFlow fb
    , "data_flow"     .= fDFDataFlow fb
    , "type_contract" .= fTTypeContract fb
    , "composite"     .= f4Composite fb
    ]

instance FromJSON FingerprintBundle where
  parseJSON = Aeson.withObject "FingerprintBundle" $ \o -> do
    s   <- o Aeson..: "source"
    st  <- o Aeson..: "structural"
    dc  <- o Aeson..: "declaration"
    dp  <- o Aeson..: "dependency"
    cg  <- o Aeson..:? "call_graph" Aeson..!= Fingerprint ""
    cf  <- o Aeson..:? "control_flow" Aeson..!= Fingerprint ""
    df  <- o Aeson..:? "data_flow" Aeson..!= Fingerprint ""
    tc  <- o Aeson..:? "type_contract" Aeson..!= Fingerprint ""
    cp  <- o Aeson..: "composite"
    pure (FingerprintBundle s st dc dp cg cf df tc cp)

data ComparisonStatus = Identical | Different
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON ComparisonStatus where
  toJSON Identical = String "identical"
  toJSON Different = String "different"

instance FromJSON ComparisonStatus where
  parseJSON = withText "ComparisonStatus" $ \t -> case T.toLower t of
    "identical" -> pure Identical
    "different" -> pure Different
    _           -> fail "Expected 'identical' or 'different'"

data ComparisonResult = ComparisonResult
  { crSource       :: ComparisonStatus -- e.g. raw text comparison status
  , crStructural   :: ComparisonStatus -- e.g. structural AST comparison status
  , crDeclaration  :: ComparisonStatus -- e.g. declaration signature comparison status
  , crDependency   :: ComparisonStatus -- e.g. dependency graph comparison status
  , crCallGraph    :: ComparisonStatus -- e.g. call graph comparison status
  , crControlFlow  :: ComparisonStatus -- e.g. control flow comparison status
  , crDataFlow     :: ComparisonStatus -- e.g. data flow comparison status
  , crTypeContract :: ComparisonStatus -- e.g. type contract comparison status
  , crComposite    :: ComparisonStatus -- e.g. overall composite comparison status
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON ComparisonResult where
  toJSON cr = object
    [ "source"        .= crSource cr
    , "structural"    .= crStructural cr
    , "declaration"   .= crDeclaration cr
    , "dependency"    .= crDependency cr
    , "call_graph"    .= crCallGraph cr
    , "control_flow"  .= crControlFlow cr
    , "data_flow"     .= crDataFlow cr
    , "type_contract" .= crTypeContract cr
    , "composite"     .= crComposite cr
    ]

instance FromJSON ComparisonResult where
  parseJSON = Aeson.withObject "ComparisonResult" $ \o -> do
    s  <- o Aeson..: "source"
    st <- o Aeson..: "structural"
    dc <- o Aeson..: "declaration"
    dp <- o Aeson..: "dependency"
    cg <- o Aeson..:? "call_graph" Aeson..!= Identical
    cf <- o Aeson..:? "control_flow" Aeson..!= Identical
    df <- o Aeson..:? "data_flow" Aeson..!= Identical
    tc <- o Aeson..:? "type_contract" Aeson..!= Identical
    cp <- o Aeson..: "composite"
    pure (ComparisonResult s st dc dp cg cf df tc cp)

data VerificationResult = VerificationResult
  { vrRuns            :: Int  -- e.g. repeat executions count
  , vrStructuralPass  :: Bool -- e.g. True if all structural runs match
  , vrDeclarationPass :: Bool -- e.g. True if all declaration runs match
  , vrDependencyPass  :: Bool -- e.g. True if all dependency runs match
  , vrCallGraphPass   :: Bool -- e.g. True if all call graph runs match
  , vrControlFlowPass :: Bool -- e.g. True if all control flow runs match
  , vrDataFlowPass    :: Bool -- e.g. True if all data flow runs match
  , vrCompositePass   :: Bool -- e.g. True if all composite runs match
  , vrDeterministic   :: Bool -- e.g. True if every tier passes
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON VerificationResult where
  toJSON vr = object
    [ "runs"              .= vrRuns vr
    , "structural_pass"   .= vrStructuralPass vr
    , "declaration_pass"  .= vrDeclarationPass vr
    , "dependency_pass"   .= vrDependencyPass vr
    , "call_graph_pass"   .= vrCallGraphPass vr
    , "control_flow_pass" .= vrControlFlowPass vr
    , "data_flow_pass"    .= vrDataFlowPass vr
    , "composite_pass"    .= vrCompositePass vr
    , "deterministic"     .= vrDeterministic vr
    ]

instance FromJSON VerificationResult where
  parseJSON = Aeson.withObject "VerificationResult" $ \o -> do
    r   <- o Aeson..: "runs"
    sp  <- o Aeson..: "structural_pass"
    dp  <- o Aeson..: "declaration_pass"
    dpp <- o Aeson..: "dependency_pass"
    cgp <- o Aeson..:? "call_graph_pass" Aeson..!= True
    cfp <- o Aeson..:? "control_flow_pass" Aeson..!= True
    dfp <- o Aeson..:? "data_flow_pass" Aeson..!= True
    cp  <- o Aeson..: "composite_pass"
    dt  <- o Aeson..: "deterministic"
    pure (VerificationResult r sp dp dpp cgp cfp dfp cp dt)

data ParseError = ParseError
  { peFile   :: FilePath -- e.g. "src/main.py"
  , peLine   :: Int      -- e.g. 14
  , peColumn :: Int      -- e.g. 8
  , peReason :: Text     -- e.g. "unexpected token ':'"
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data ManifestMetadata = ManifestMetadata
  { mmFileCount        :: Int -- e.g. 1 for single file
  , mmModuleCount      :: Int -- e.g. 1 module
  , mmDeclarationCount :: Int -- e.g. 5 top-level declarations
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON ManifestMetadata where
  toJSON mm = object
    [ "file_count" .= mmFileCount mm
    , "module_count" .= mmModuleCount mm
    , "declaration_count" .= mmDeclarationCount mm
    ]

instance FromJSON ManifestMetadata where
  parseJSON = Aeson.withObject "ManifestMetadata" $ \o -> do
    fc <- o Aeson..: "file_count"
    mc <- o Aeson..: "module_count"
    dc <- o Aeson..: "declaration_count"
    pure (ManifestMetadata fc mc dc)

data Manifest = Manifest
  { mEngine               :: Text              -- e.g. "canontra"
  , mVersion              :: Text              -- e.g. "0.0.4-alpha"
  , mLanguage             :: Text              -- e.g. "python"
  , mNormalizationVersion :: Text              -- e.g. "0.0.4-alpha"
  , mHashAlgorithm        :: HashAlgorithm     -- e.g. SHA256
  , mFingerprints         :: FingerprintBundle -- e.g. 8-tier bundle
  , mMetadata             :: ManifestMetadata  -- e.g. file and declaration metrics
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON Manifest where
  toJSON m = object
    [ "engine" .= mEngine m
    , "version" .= mVersion m
    , "language" .= mLanguage m
    , "normalization_version" .= mNormalizationVersion m
    , "hash_algorithm" .= mHashAlgorithm m
    , "fingerprints" .= mFingerprints m
    , "metadata" .= mMetadata m
    ]

instance FromJSON Manifest where
  parseJSON = Aeson.withObject "Manifest" $ \o -> do
    eng  <- o Aeson..: "engine"
    ver  <- o Aeson..: "version"
    lang <- o Aeson..: "language"
    nver <- o Aeson..: "normalization_version"
    halg <- o Aeson..: "hash_algorithm"
    fps  <- o Aeson..: "fingerprints"
    meta <- o Aeson..: "metadata"
    pure (Manifest eng ver lang nver halg fps meta)

data FileEntry = FileEntry
  { fePath         :: FilePath          -- e.g. "app/server.py"
  , feFingerprints :: FingerprintBundle -- e.g. computed bundle for this file
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON FileEntry where
  toJSON fe = object
    [ "path" .= fePath fe
    , "fingerprints" .= feFingerprints fe
    ]

instance FromJSON FileEntry where
  parseJSON = Aeson.withObject "FileEntry" $ \o -> do
    p <- o Aeson..: "path"
    fps <- o Aeson..: "fingerprints"
    pure (FileEntry p fps)

data RepositoryManifest = RepositoryManifest
  { rmEngine               :: Text              -- e.g. "canontra"
  , rmVersion              :: Text              -- e.g. "0.0.9-alpha"
  , rmRepositoryFingerprint:: Fingerprint       -- e.g. combined repository hash FR
  , rmWholeRepoCallGraph   :: Maybe Fingerprint -- e.g. F_WCG
  , rmWholeRepoDataFlow    :: Maybe Fingerprint -- e.g. F_WDF
  , rmFiles                :: [FileEntry]       -- e.g. sorted list of file entries
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON RepositoryManifest where
  toJSON rm = object
    [ "engine" .= rmEngine rm
    , "version" .= rmVersion rm
    , "repository_fingerprint" .= rmRepositoryFingerprint rm
    , "whole_repo_call_graph" .= rmWholeRepoCallGraph rm
    , "whole_repo_data_flow" .= rmWholeRepoDataFlow rm
    , "files" .= rmFiles rm
    ]

instance FromJSON RepositoryManifest where
  parseJSON = Aeson.withObject "RepositoryManifest" $ \o -> do
    eng <- o Aeson..: "engine"
    ver <- o Aeson..: "version"
    rfp <- o Aeson..: "repository_fingerprint"
    wcg <- o Aeson..:? "whole_repo_call_graph"
    wdf <- o Aeson..:? "whole_repo_data_flow"
    fs  <- o Aeson..: "files"
    pure (RepositoryManifest eng ver rfp wcg wdf fs)

data EvolutionComparison = EvolutionComparison
  { ecPreviousRev  :: Text             -- e.g. "HEAD~1"
  , ecCurrentRev   :: Text             -- e.g. "HEAD"
  , ecStructural   :: ComparisonStatus -- e.g. Identical or Different
  , ecDeclarations :: ComparisonStatus -- e.g. Identical or Different
  , ecDependencies :: ComparisonStatus -- e.g. Identical or Different
  , ecCallGraph    :: ComparisonStatus -- e.g. Identical or Different
  , ecControlFlow  :: ComparisonStatus -- e.g. Identical or Different
  , ecDataFlow     :: ComparisonStatus -- e.g. Identical or Different
  , ecComposite    :: ComparisonStatus -- e.g. Identical or Different
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON EvolutionComparison where
  toJSON ec = object
    [ "previous_rev" .= ecPreviousRev ec
    , "current_rev"  .= ecCurrentRev ec
    , "structural"   .= ecStructural ec
    , "declarations" .= ecDeclarations ec
    , "dependencies" .= ecDependencies ec
    , "call_graph"   .= ecCallGraph ec
    , "control_flow" .= ecControlFlow ec
    , "data_flow"    .= ecDataFlow ec
    , "composite"    .= ecComposite ec
    ]

instance FromJSON EvolutionComparison where
  parseJSON = Aeson.withObject "EvolutionComparison" $ \o -> do
    pr  <- o Aeson..: "previous_rev"
    cr  <- o Aeson..: "current_rev"
    st  <- o Aeson..: "structural"
    dc  <- o Aeson..: "declarations"
    dp  <- o Aeson..: "dependencies"
    cg  <- o Aeson..:? "call_graph" Aeson..!= Identical
    cf  <- o Aeson..:? "control_flow" Aeson..!= Identical
    df  <- o Aeson..:? "data_flow" Aeson..!= Identical
    cp  <- o Aeson..: "composite"
    pure (EvolutionComparison pr cr st dc dp cg cf df cp)

data ParamKind
  = ParamPositional      -- e.g. standard def f(x)
  | ParamKeywordOnly     -- e.g. def f(*, kw)
  | ParamVarArgs         -- e.g. def f(*args) / ...args
  | ParamKwArgs          -- e.g. def f(**kwargs)
  | ParamPositionalOnly  -- e.g. PEP 570 def f(pos, /)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data Parameter = Parameter
  { paramName    :: Text        -- e.g. "x"
  , paramKind    :: ParamKind   -- e.g. ParamPositional
  , paramDefault :: Maybe Text  -- e.g. Just "0"
  , paramType    :: Maybe Text  -- e.g. Just "int"
  } deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data DeclKind
  = KindFunction
  | KindMethod
  | KindClass
  | KindStruct
  | KindInterface
  | KindTrait
  | KindImpl
  | KindVariable
  | KindTypeAlias
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data GlobalSymbol = GlobalSymbol
  { symFilePath :: !FilePath
  , symModule   :: !Text
  , symDeclName :: !Text
  , symKind     :: !DeclKind
  , symTier2    :: !Fingerprint
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, NFData)

data WholeRepoCallEdge = WholeRepoCallEdge
  { wceCaller     :: !GlobalSymbol
  , wceCallee     :: !GlobalSymbol
  , wceCallCount  :: !Int
  , wceIsAsync    :: !Bool
  , wceIsCrossMod :: !Bool
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, NFData)

data WholeRepoCallGraph = WholeRepoCallGraph
  { wcgNodes :: ![GlobalSymbol]
  , wcgEdges :: ![WholeRepoCallEdge]
  , wcgSCCs  :: ![[GlobalSymbol]]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, NFData)

data InterProceduralDataFlowEdge = InterProceduralDataFlowEdge
  { ipdfSourceSymbol :: !GlobalSymbol
  , ipdfTargetSymbol :: !GlobalSymbol
  , ipdfParamIndex   :: !Int
  , ipdfVarName      :: !Text
  , ipdfIsReturnFlow :: !Bool
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, NFData)

data WholeRepoDataFlowGraph = WholeRepoDataFlowGraph
  { wdfNodes :: ![GlobalSymbol]
  , wdfEdges :: ![InterProceduralDataFlowEdge]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, NFData)

data WholeRepoBundle = WholeRepoBundle
  { wrbRepositoryHash :: !Fingerprint -- F_R
  , wrbCallGraph      :: !Fingerprint -- F_WCG
  , wrbDataFlow       :: !Fingerprint -- F_WDF
  , wrbComposite      :: !Fingerprint -- F_W4
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, NFData)

