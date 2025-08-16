{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Azalea.Typecheck.Core
  ( Typechecker
  , TypecheckState (..)
  , TypeEnv
  , lookupVar
  , insertVar
  , freshVar
  , occursCheck
  , findFreeVars
  , collectTypeParams
  , unTypechecker
  ) where

import Azalea.AST.Types (BOp (..), Ty (..))
import Azalea.Typecheck.Scope (Scope, defineFuncInScope, defineVarInScope, lookupFuncInScope, lookupVarInScope, mkScope)
import Control.Applicative (asum)
import Control.Monad.Except
import Control.Monad.State
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V

-- Type environment mapping variable names to types.
type TypeEnv = M.Map Text Ty

data TypecheckState = TypecheckState
  { env :: TypeEnv
  , freshCounter :: Int
  , scopes :: [Scope]
  }

-- Typechecker monad, a stateful monad that can throw errors
newtype Typechecker a = Typechecker
  { unTypechecker :: StateT TypecheckState (Except String) a
  }
  deriving (Functor, Applicative, Monad, MonadState TypecheckState, MonadError String)

lookupVar :: Text -> Typechecker (Maybe Ty)
lookupVar name = gets (M.lookup name . env)

insertVar :: Text -> Ty -> Typechecker ()
insertVar name ty = modify (\s -> s{env = M.insert name ty (env s)})

-- | Occurs check. Check for infinite type expansions.
occursCheck :: Text -> Ty -> Bool
occursCheck name ty = case ty of
  TyVar n -> n == name
  TyArray elemType -> occursCheck name elemType
  TyCons _ params -> any (occursCheck name) (V.toList params)
  _ -> False

-- | Generate a fresh type variable
freshVar :: Typechecker Ty
freshVar = do
  st <- get
  let n = freshCounter st
      name = "t" <> (T.pack $ show n)

  put st{freshCounter = n + 1}
  pure $ TyVar name

findFreeVars :: Ty -> S.Set Text
findFreeVars (TyVar n) = S.singleton n
findFreeVars (TyArray elemType) = findFreeVars elemType
findFreeVars (TyCons _ params) = S.unions $ map findFreeVars (V.toList params)
findFreeVars _ = S.empty

collectTypeParams :: Ty -> Typechecker [Text]
collectTypeParams ty = case ty of
  TyVar name -> pure [name]
  TyCons _ params -> concat <$> traverse collectTypeParams (V.toList params)
  TyArray elemType -> collectTypeParams elemType
  _ -> pure []

-- SCOPE RESOLUTION

pushScope :: Scope -> Typechecker ()
pushScope scope = modify (\s -> s{scopes = scope : scopes s})

popScope :: Typechecker ()
popScope = do
  st <- get
  case scopes st of
    [] -> throwError "Cannot pop the empty scope stack"
    (_ : rest) -> put st{scopes = rest}

lookupVariable :: Text -> Typechecker (Maybe Ty)
lookupVariable name = gets $ \st -> asum $ map (\scope -> lookupVarInScope scope (T.unpack name)) (scopes st)

lookupFunction :: Text -> Typechecker (Maybe ([Ty], Ty))
lookupFunction name = gets $ \st -> asum $ map (\scope -> lookupFuncInScope scope (T.unpack name)) (scopes st)

defineVariable :: Text -> Ty -> Typechecker ()
defineVariable name ty = do
  st <- get
  case scopes st of
    [] -> throwError "No current scope to define variable in"
    (currScope : rest) ->
      case defineVarInScope (T.unpack name) ty currScope of
        Left err -> throwError err
        Right newScope -> put st{scopes = newScope : rest}

defineFunction :: Text -> [Ty] -> Ty -> Typechecker ()
defineFunction name params returnType = do
  st <- get
  case scopes st of
    [] -> throwError "No current scope to define function in"
    (currScope : rest) ->
      case defineFuncInScope currScope (T.unpack name) params returnType of
        Left err -> throwError err
        Right newScope -> put st{scopes = newScope : rest}

clearScopes :: Typechecker ()
clearScopes = modify (\s -> s{scopes = [mkScope "default" Nothing]})

opIsComparison :: BOp -> Bool
opIsComparison Eq = True
opIsComparison Neq = True
opIsComparison Gt = True
opIsComparison Lt = True
opIsComparison Gte = True
opIsComparison Lte = True
opIsComparison _ = False