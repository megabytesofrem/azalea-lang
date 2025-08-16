{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Azalea.AST.Expr
  ( Literal (..)
  , Expr (..)
  , Member (..)
  , Record (..)
  , Lam (..)
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

-- | A record type - synonymous with a JS object
data Record = Record
  { recordName :: Text
  , recordFields :: M.Map Text (Span Expr)
  }

data Lam = Lam
  { lamParams :: M.Map Text Ty -- List of parameter names
  , lamBody :: Span Expr -- The body of the lambda expression
  }

data Literal
  = IntLit Integer
  | FloatLit Double
  | StringLit Text
  | ArrayLit [Span Expr] -- Array literal
  | BoolLit Bool

data Expr
  = ELit Literal -- A literal value
  | EIdent Text -- An identifier (variable name)
  | EMember Member
  | EBinOp BOp Expr Expr
  | EUnaryOp UOp Expr
  | ERecord Record
  | ECall Expr [Expr]
  | EArrayIndex Expr Expr
  | ELam Lam
  | EIf Expr Expr Expr

-- | Member access for records/enums
data Member = Member
  { memberName :: Text
  , memberTarget :: Span Expr
  }

-- Use StandaloneDeriving to create Show and Eq instances for types from AST.Types
-- since we have cyclic dependencies with AST.Types
deriving instance Eq Literal
deriving instance Eq Expr
deriving instance Eq Member
deriving instance Eq Record
deriving instance Eq Lam
deriving instance Eq Ty

instance (Eq a) => Eq (Function a) where
  (Function name params ret body) == (Function name' params' ret' body') =
    name == name' && params == params' && ret == ret' && body == body'