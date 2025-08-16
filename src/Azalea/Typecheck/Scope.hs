module Azalea.Typecheck.Scope
  ( Scope (..)
  , ScopeResult
  , defineVarInScope
  , lookupVarInScope
  , lookupFuncInScope
  , defineFuncInScope
  , mkScope
  ) where

import Azalea.AST.Types (Ty)
import Data.Map qualified as M

data Scope = Scope
  { scopeName :: String
  , scopeParent :: Maybe Scope
  , -- Keep track of variables and functions defined in this scope
    scopeVariables :: M.Map String Ty
  , scopeFunctions :: M.Map String ([Ty], Ty)
  }
  deriving (Show, Eq)

type ScopeResult = Either String Scope

defineVarInScope :: String -> Ty -> Scope -> ScopeResult
defineVarInScope name ty scope =
  Right scope{scopeVariables = M.insert name ty (scopeVariables scope)}

lookupVarInScope :: Scope -> String -> Maybe Ty
lookupVarInScope scope name = M.lookup name (scopeVariables scope)

lookupFuncInScope :: Scope -> String -> Maybe ([Ty], Ty)
lookupFuncInScope scope name = M.lookup name (scopeFunctions scope)

defineFuncInScope :: Scope -> String -> [Ty] -> Ty -> ScopeResult
defineFuncInScope scope name parTys returnType
  | M.member name (scopeVariables scope) =
      Left $ "Function " ++ name ++ " is already defined as a variable in scope " ++ scopeName scope
  | M.member name (scopeFunctions scope) =
      Left $ "Function " ++ name ++ " is already defined in scope " ++ scopeName scope
  | otherwise = Right scope{scopeFunctions = M.insert name (parTys, returnType) (scopeFunctions scope)}

mkScope :: String -> Maybe Scope -> Scope
mkScope name parent =
  Scope
    { scopeName = name
    , scopeParent = parent
    , scopeVariables = M.empty
    , scopeFunctions = M.empty
    }