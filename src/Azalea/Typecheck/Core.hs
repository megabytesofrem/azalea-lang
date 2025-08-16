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

import Azalea.AST.Types (Ty (..))
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