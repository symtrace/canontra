{- |
Module      : Canontra.IR.Declaration
Description : Polyglot language-agnostic declaration hierarchy.

Declarations isolate the architectural skeleton of a program across Python,
JavaScript, TypeScript, Go, and Rust. By separating signatures, classes, structs,
interfaces, traits, impls, and receivers from statement-level implementation details,
canontra detects interface changes versus internal implementation evolution.
-}
module Canontra.IR.Declaration
  ( ParamKind (..)
  , Parameter (..)
  , Function (..)
  , Class (..)
  , Struct (..)
  , Interface (..)
  , Receiver (..)
  , Trait (..)
  , Impl (..)
  , Declaration (..)
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Canontra.IR.Expression (Stmt)
import Canontra.Types (ParamKind (..), Parameter (..))

data Function = Function
  { fnName       :: Text        -- e.g. "calculate_total"
  , fnParams     :: [Parameter] -- e.g. [Parameter "price" ParamPositional Nothing (Just "float")]
  , fnReturnType :: Maybe Text  -- e.g. Just "float"
  , fnDecorators :: [Text]      -- e.g. ["@staticmethod", "pub", "@export"]
  , fnBody       :: [Stmt]      -- e.g. [StmtReturn (Just (ExprBinary OpAdd ...))]
  , fnIsAsync    :: Bool        -- e.g. True for async def / async fn
  } deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data Class = Class
  { clsName       :: Text        -- e.g. "CustomerService"
  , clsBases      :: [Text]      -- e.g. ["BaseService"]
  , clsMethods    :: [Function]  -- e.g. [Function "__init__" ...]
  , clsDecorators :: [Text]      -- e.g. ["@dataclass", "export"]
  } deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data Struct = Struct
  { stName       :: Text                  -- e.g. "User"
  , stFields     :: [(Text, Maybe Text)]  -- e.g. [("id", Just "i64"), ("name", Just "String")]
  , stMethods    :: [Function]            -- e.g. member methods
  , stVisibility :: Text                  -- e.g. "pub", "public"
  } deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data Interface = Interface
  { ifName    :: Text       -- e.g. "Reader"
  , ifMethods :: [Function] -- e.g. method signatures
  , ifBases   :: [Text]     -- e.g. extended interfaces
  } deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data Receiver = Receiver
  { rcVarName   :: Text -- e.g. "u"
  , rcTypeName  :: Text -- e.g. "User"
  , rcIsPointer :: Bool -- e.g. True for *User
  } deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data Trait = Trait
  { trName        :: Text       -- e.g. "Display"
  , trMethods     :: [Function] -- e.g. trait method signatures
  , trSuperTraits :: [Text]     -- e.g. ["Clone"]
  } deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data Impl = Impl
  { impTrait   :: Maybe Text -- e.g. Just "Display" or Nothing for inherent impl
  , impTarget  :: Text       -- e.g. "User"
  , impMethods :: [Function] -- e.g. implemented methods
  } deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data Declaration
  = DeclFunction Function            -- e.g. def / func / fn / function
  | DeclClass Class                  -- e.g. class / class with constructor
  | DeclStruct Struct                -- e.g. Go / Rust struct
  | DeclInterface Interface          -- e.g. Go interface / TS interface
  | DeclReceiver Receiver Function   -- e.g. Go receiver method func (r *Recv) Method()
  | DeclTrait Trait                  -- e.g. Rust trait
  | DeclImpl Impl                    -- e.g. Rust impl Trait for Type
  | DeclVariable Text (Maybe Text)   -- e.g. top-level variable or constant
  | DeclTypeAlias Text (Maybe Text)  -- e.g. type alias in TS / Go / Rust
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)
