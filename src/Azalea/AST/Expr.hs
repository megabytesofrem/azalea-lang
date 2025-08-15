{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Azalea.AST.Expr (Literal (..), Expr (..), Member (..)) where

import Azalea.AST.Span (Span (..))
import Azalea.AST.Types (BOp (..), Function (..), Lam (..), Record (..), Ty (..), UOp (..))
import Data.Text (Text)

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
deriving instance Show Literal
deriving instance Show Expr
deriving instance Show Member
deriving instance Show Function
deriving instance Show Record
deriving instance Show Lam
deriving instance Show Ty

deriving instance Eq Literal
deriving instance Eq Expr
deriving instance Eq Member
deriving instance Eq Function
deriving instance Eq Record
deriving instance Eq Lam
deriving instance Eq Ty