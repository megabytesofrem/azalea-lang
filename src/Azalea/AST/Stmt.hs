module Azalea.AST.Stmt
  ( Block
  , Stmt (..)
  , ToplevelStmt (..)
  )
where

import Azalea.AST.Expr (Expr, Function, Record)
import Azalea.AST.Types (Ty)
import Data.Vector qualified as V

type Block = V.Vector Stmt

data Stmt
  = Let String Ty Expr
  | Mut String Ty Expr
  | Assign String Expr
  | For String Expr Block
  | While Expr Block
  | ExprStmt Expr

data ToplevelStmt
  = AStmt Stmt
  | RecordDecl Record
  | FnDecl (Function Block)