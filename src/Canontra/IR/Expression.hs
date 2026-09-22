{- |
Module      : Canontra.IR.Expression
Description : Language-independent syntax expressions, operators, and statements.

Expressions and statements capture the operational pulse of computation across
Python, JavaScript, TypeScript, Go, and Rust. This intermediate representation
strips away source-level syntax quirks while faithfully retaining algebraic operator
precedence, control branches, async semantics, concurrency, and invocation contracts.
-}
module Canontra.IR.Expression
  ( Op (..)
  , Lit (..)
  , FStringPart (..)
  , MatchCase (..)
  , SelectCase (..)
  , Expr (..)
  , Stmt (..)
  , CompFor (..)
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Canontra.Types (Parameter)

data Op
  = OpAdd | OpSub | OpMul | OpDiv | OpFloorDiv | OpMod | OpPow
  | OpBitAnd | OpBitOr | OpBitXor | OpShiftL | OpShiftR
  | OpEq | OpNotEq | OpLt | OpLtE | OpGt | OpGtE
  | OpAnd | OpOr | OpNot | OpInvert | OpIn | OpNotIn | OpIs | OpIsNot
  | OpMatMult
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data Lit
  = LitInt Integer   -- e.g. 42
  | LitFloat Double  -- e.g. 3.14159
  | LitString Text   -- e.g. "hello"
  | LitBytes Text    -- e.g. b"binary"
  | LitBool Bool     -- e.g. True
  | LitNone          -- e.g. None / null / nil
  | LitEllipsis      -- e.g. ... / _
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data CompFor = CompFor
  { compTarget :: Expr   -- e.g. x in [x for x in items]
  , compIter   :: Expr   -- e.g. items
  , compIfs    :: [Expr] -- e.g. [x > 0]
  } deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data FStringPart
  = FStringText Text
  | FStringExpr Expr (Maybe Text) (Maybe Text) -- Expr, Conversion (!r, !s), FormatSpec
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data MatchCase = MatchCase
  { mcPattern :: Expr
  , mcGuard   :: Maybe Expr
  , mcBody    :: [Stmt]
  } deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data SelectCase
  = SelectSend Expr Expr        -- e.g. ch <- val
  | SelectRecv (Maybe Text) Expr -- e.g. val := <-ch / <-ch
  | SelectDefault               -- e.g. default:
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data Expr
  = ExprId Text                                      -- e.g. variable name "total"
  | ExprLit Lit                                      -- e.g. integer literal 100
  | ExprBinary Op Expr Expr                          -- e.g. a + b
  | ExprUnary Op Expr                                -- e.g. -x or not flag
  | ExprCall Expr [Expr] [(Text, Expr)]              -- e.g. f(a, key=val)
  | ExprAttr Expr Text                               -- e.g. obj.property
  | ExprSubscript Expr Expr                          -- e.g. arr[idx]
  | ExprSlice (Maybe Expr) (Maybe Expr) (Maybe Expr) -- e.g. a[start:stop:step]
  | ExprList [Expr]                                  -- e.g. [1, 2, 3]
  | ExprTuple [Expr]                                 -- e.g. (1, 2)
  | ExprDict [(Expr, Expr)]                          -- e.g. {"k": v}
  | ExprSet [Expr]                                   -- e.g. {1, 2, 3}
  | ExprLambda [Parameter] Expr                      -- e.g. lambda x: x * 2 / (x) => x * 2
  | ExprTernary Expr Expr Expr                       -- e.g. cond ? trueVal : falseVal
  | ExprListComp Expr [CompFor]                      -- e.g. [x * 2 for x in xs if x > 0]
  | ExprDictComp Expr Expr [CompFor]                 -- e.g. {k: v for k, v in pairs}
  | ExprSetComp Expr [CompFor]                       -- e.g. {x for x in xs}
  | ExprGenerator Expr [CompFor]                     -- e.g. (x for x in xs)
  | ExprWalrus Text Expr                             -- e.g. PEP 572 (x := calc())
  | ExprAwait Expr                                   -- e.g. await coro()
  | ExprYield (Maybe Expr)                           -- e.g. yield item
  | ExprYieldFrom Expr                               -- e.g. yield from generator
  | ExprFormattedString [FStringPart]                -- e.g. f"value: {x}" / `value: ${x}`
  | ExprStarred Expr                                 -- e.g. *args / ...arr
  | ExprKwStarred Expr                               -- e.g. **kwargs
  | ExprOptChain Expr Text                           -- e.g. obj?.field
  | ExprNullish Expr Expr                            -- e.g. a ?? b
  | ExprChanRecv Expr                                -- e.g. <-ch (Go)
  | ExprTryOp Expr                                   -- e.g. expr? (Rust)
  | ExprMacroCall Text [Expr]                        -- e.g. println!(...) / vec![...]
  | ExprJSX Text [(Text, Expr)] [Expr]               -- e.g. <Component attr={v}>children</Component>
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

data Stmt
  = StmtAssign [Expr] Expr                                      -- e.g. x = y = 10
  | StmtAnnAssign Expr Expr (Maybe Expr)                        -- e.g. x: int = 10 / let x: i32 = 10
  | StmtAugAssign Expr Op Expr                                  -- e.g. x += 1
  | StmtExpr Expr                                               -- e.g. print("hi")
  | StmtReturn (Maybe Expr)                                     -- e.g. return result
  | StmtIf Expr [Stmt] [Stmt]                                   -- e.g. if cond: ... else: ...
  | StmtWhile Expr [Stmt] [Stmt]                                -- e.g. while cond: ... else: ...
  | StmtFor Expr Expr [Stmt] [Stmt]                             -- e.g. for item in iter: ... else: ...
  | StmtAsyncFor Expr Expr [Stmt] [Stmt]                        -- e.g. async for item in aiter: ...
  | StmtTry [Stmt] [(Maybe Expr, Maybe Text, [Stmt])] [Stmt] [Stmt] -- e.g. try / except / else / finally
  | StmtWith [(Expr, Maybe Expr)] [Stmt]                        -- e.g. with open(f) as h: ...
  | StmtAsyncWith [(Expr, Maybe Expr)] [Stmt]                   -- e.g. async with lock: ...
  | StmtAssert Expr (Maybe Expr)                                -- e.g. assert x > 0, "must be positive"
  | StmtRaise (Maybe Expr) (Maybe Expr)                         -- e.g. raise ValueError() / panic!(...)
  | StmtBreak                                                   -- e.g. break
  | StmtContinue                                                -- e.g. continue
  | StmtPass                                                    -- e.g. pass
  | StmtDelete [Expr]                                           -- e.g. del obj.field
  | StmtGlobal [Text]                                           -- e.g. global state
  | StmtNonlocal [Text]                                         -- e.g. nonlocal counter
  | StmtMatch Expr [MatchCase]                                  -- e.g. match / switch pattern
  | StmtGo Expr                                                 -- e.g. go worker() (Go)
  | StmtDefer Expr                                              -- e.g. defer file.Close() (Go)
  | StmtChanSend Expr Expr                                      -- e.g. ch <- val (Go)
  | StmtSelect [(SelectCase, [Stmt])]                           -- e.g. select { case ... } (Go)
  | StmtLoop [Stmt]                                             -- e.g. loop { ... } (Rust)
  | StmtSwitch Expr [(Expr, [Stmt])] [Stmt]                     -- e.g. switch(x) { case 1: ... default: ... }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)
