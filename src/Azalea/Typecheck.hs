{-# LANGUAGE TupleSections #-}

module Azalea.Typecheck where

import Azalea.AST.Types (Ty (..))
import Control.Monad (zipWithM)
import Control.Monad.Except
import Control.Monad.State
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V

-- Type environment mapping variable names to types.
type TypeEnv = M.Map Text Ty

data TypecheckState = TCState
  { env :: TypeEnv
  , freshCounter :: Int
  }

type Typechecker a = StateT TypecheckState (Except String) a

lookupVar :: Text -> Typechecker (Maybe Ty)
lookupVar name = gets (M.lookup name . env)

insertVar :: Text -> Ty -> Typechecker ()
insertVar name ty = modify (\s -> s{env = M.insert name ty (env s)})

-- | Occurs check. Check for infinite type expansions.
occursCheck :: Text -> Ty -> Bool
occursCheck name ty = case ty of
  TyVar n -> n == name
  TyArray elemType -> occursCheck name elemType
  TyCons _ args -> any (occursCheck name) (V.toList args)
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
findFreeVars (TyCons _ args) = S.unions $ map findFreeVars (V.toList args)
findFreeVars _ = S.empty

collectTypeParams :: Ty -> Typechecker [Text]
collectTypeParams ty = case ty of
  TyVar name -> pure [name]
  TyCons _ args -> concat <$> traverse collectTypeParams (V.toList args)
  TyArray elemType -> collectTypeParams elemType
  _ -> pure []

-- | Instantiate a type with fresh type variables.
instantiate :: Ty -> Typechecker Ty
instantiate ty = case ty of
  TyForAll vars inner -> do
    subst <- M.fromList <$> mapM (\v -> (v,) <$> freshVar) vars
    pure $ hydrate subst inner
  _ -> pure ty

generalize :: Ty -> Typechecker Ty
generalize ty = do
  -- Collect free type variables in the type
  let freeVars = findFreeVars ty

  -- Find free type variables for each type in the environment
  envVars <- gets (S.unions . map findFreeVars . M.elems . env)
  let generalized = S.difference freeVars envVars
  if S.null generalized
    then pure ty
    else pure $ TyForAll (S.toList generalized) ty

-- | Hydrate a type, by applying substitutions from the type environment.
-- This is used to resolve type variables to their actual types.
hydrate :: TypeEnv -> Ty -> Ty
hydrate subst (TyVar name) = M.findWithDefault (TyVar name) name subst
hydrate subst (TyArray elemType) = TyArray (hydrate subst elemType)
hydrate subst (TyCons name args) =
  if V.null args && M.member name subst
    then M.findWithDefault (TyCons name args) name subst
    else TyCons name (V.map (hydrate subst) args)
-- All other types are concrete, leave them as is
hydrate _ ty = ty

-- | Unify two types, returning a substitution map if they can be unified.
-- Throws an error if they cannot be unified.
unify :: Ty -> Ty -> Typechecker TypeEnv
unify t1 t2
  | t1 == t2 = pure M.empty
unify t1 t2
  | areVoidEquiv t1 t2 = pure M.empty
unify (TyVar v) ty
  | occursCheck v ty = throwError $ "Occurs check failed for variable: " <> show v
  | otherwise = pure (M.singleton v ty)
unify ty (TyVar v)
  | occursCheck v ty = throwError $ "Occurs check failed for variable: " <> show v
  | otherwise = pure (M.singleton v ty)
unify (TyArray a1) (TyArray a2) = unify a1 a2
unify (TyCons name1 args1) (TyCons name2 args2)
  | name1 == name2 && V.length args1 == V.length args2 = do
      subst <- zipWithM unify (V.toList args1) (V.toList args2)
      pure (foldl M.union M.empty subst)
unify (TyFn par1 ret1) (TyFn par2 ret2)
  | length par1 == length par2 = do
      subst <- zipWithM unify par1 par2
      subst' <- unify ret1 ret2
      pure (foldl M.union M.empty (subst ++ [subst']))
unify t1 t2 = throwError $ "Cannot unify types: " <> show t1 <> " and " <> show t2

-- True for types that can be considered equivalent to a void in the type system.
areVoidEquiv :: Ty -> Ty -> Bool
areVoidEquiv TyUnit TyUnknown = True
areVoidEquiv TyUnknown TyUnit = True
areVoidEquiv TyUnit TyAny = True
areVoidEquiv TyAny TyUnit = True
areVoidEquiv _ _ = False

-- | Run the typechecker with an initial state.
runTypechecker :: Typechecker a -> TypecheckState -> Either String (a, TypecheckState)
runTypechecker m s = runExcept (runStateT m s)