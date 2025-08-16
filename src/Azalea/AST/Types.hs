module Azalea.AST.Types
  ( Ty (..)
  , BOp (..)
  , UOp (..)
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V

data Ty
  = TyInt
  | TyFloat
  | TyString
  | TyBool
  | TyUnit
  | TyArray Ty
  | TyFn [Ty] Ty
  | TyVar Text -- Type variable
  | TyCons Text (V.Vector Ty)
  | TyForAll [Text] Ty -- Forall quantification
  | TyAny
  | TyUnknown -- Placeholder for resolving types
  deriving (Eq)

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
  deriving (Eq)

data UOp
  = Neg -- Negation
  | Not -- Prefix not
  deriving (Eq)

-- Show instances

instance Show Ty where
  show TyInt = "Int"
  show TyFloat = "Float"
  show TyString = "String"
  show TyBool = "Bool"
  show TyUnit = "Unit"
  show (TyArray elemType) = "Array(" ++ show elemType ++ ")"
  show (TyFn params ret) = "(" ++ unwords (map show params) ++ ") -> " ++ show ret
  show (TyCons name args) = T.unpack name ++ "[" ++ unwords (map show (V.toList args)) ++ "]"
  show TyUnknown = "Unknown"
  show TyAny = "Any"
  show (TyVar name) = T.unpack name
  show (TyForAll vars body) = "forall " ++ unwords (map T.unpack vars) ++ ". " ++ show body

instance Show BOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"
  show And = "&&"
  show Or = "||"
  show Eq = "=="
  show Neq = "!="
  show Gt = ">"
  show Lt = "<"
  show Gte = ">="
  show Lte = "<="
  show PlusEq = "+="
  show MinusEq = "-="

instance Show UOp where
  show Neg = "-"
  show Not = "!"