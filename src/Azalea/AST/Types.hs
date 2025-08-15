module Azalea.AST.Types
  ( Ty (..)
  , BOp (..)
  , UOp (..)
  , Function (..)
  , Record (..)
  , Lam (..)
  ) where

import {-# SOURCE #-} Azalea.AST.Expr (Expr)
import Azalea.AST.Span (Span)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Vector qualified as V

data Function = Function
  { funcName :: Text -- Name of the function
  , funcParams :: M.Map Text Ty -- List of parameter names and their types
  , funcReturnType :: Ty
  , funcBody :: Span Expr
  }

-- | A record type - synonymous with a JS object
data Record = Record
  { recordName :: Text
  , recordFields :: M.Map Text (Span Expr)
  }

data Lam = Lam
  { lamParams :: M.Map Text Ty -- List of parameter names
  , lamBody :: Span Expr -- The body of the lambda expression
  }

data Ty
  = TyInt
  | TyFloat
  | TyString
  | TyBool
  | TyUnit
  | TyArray Ty
  | TyFn Function
  | TyCons Text (V.Vector Ty)
  | TyUnknown -- Placeholder for resolving types

-- These are not cylic, so we can derive Show for them
data BOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Eq
  | Neq
  | Gt
  | Lt
  | Gte
  | Lte
  | PlusEq
  | MinusEq
  deriving (Show, Eq)

data UOp
  = Neg -- Negation
  | Not -- Prefix not
  deriving (Show, Eq)
