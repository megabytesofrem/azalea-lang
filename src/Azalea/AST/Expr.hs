{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Azalea.AST.Expr
  ( Literal (..)
  , Expr (..)
  , Record (..)
  , Function (..)
  ) where

import Azalea.AST.Span (Span (..))
import Azalea.AST.Types (BOp (..), Ty (..), UOp (..))
import Data.Map qualified as M
import Data.Text (Text)

data Function body = Function
  { funcName :: Text -- Name of the function
  , funcParams :: M.Map Text Ty -- List of parameter names and their types
  , funcReturnType :: Ty
  , funcBody :: body -- Only used if the function consists of a block
  }
  deriving (Show)

-- | A record type - synonymous with a JS object
data Record = Record
  { recordName :: Text
  , recordFields :: M.Map Text (Span Expr)
  }
  deriving (Show)

data Literal
  = IntLit Integer
  | FloatLit Double
  | StringLit Text
  | ArrayLit [Span Expr] -- Array literal
  | BoolLit Bool
  deriving (Show)

data Expr
  = ELit Literal -- A literal value
  | EIdent Text -- An identifier (variable name)
  | EMember Text (Span Expr) -- Member access, e.g., `obj.member`
  | EBinOp BOp Expr Expr
  | EUnaryOp UOp Expr
  | ERecord Record
  | ECall Expr [Expr]
  | EArrayIndex Expr Expr
  | ELam (M.Map Text Ty) (Span Expr)
  | EIf Expr Expr Expr
  deriving (Show)

-- Use StandaloneDeriving to create Show and Eq instances for types from AST.Types
-- since we have cyclic dependencies with AST.Types
deriving instance Eq Literal
deriving instance Eq Expr
deriving instance Eq Record
instance (Eq a) => Eq (Function a) where
  (Function name params ret body) == (Function name' params' ret' body') =
    name == name' && params == params' && ret == ret' && body == body'