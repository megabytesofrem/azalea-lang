{-# LANGUAGE TupleSections #-}

module Azalea.Typecheck.Algorithm
  ( instantiate
  , generalize
  , hydrate
  , unify
  , areVoidEquiv
  )
where

import Azalea.AST (BOp (..), Expr (..), Literal (..), Ty (..), UOp (..))
import Azalea.AST.Span (Span (value))
import Azalea.Typecheck.Core
import Control.Monad (zipWithM)
import Control.Monad.Except (throwError)
import Control.Monad.State (gets)
import Control.Monad.State.Class (modify)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Vector qualified as V

-- | Instantiate a type with fresh type variables.
instantiate :: Ty -> Typechecker Ty
instantiate ty = case ty of
  TyForAll vars inner -> do
    -- `forall A, B. A -> B` becomes `t0 -> t1`, where `t0` and `t1` are fresh type variables.
    subst <- M.fromList <$> mapM (\v -> (v,) <$> freshVar) vars
    pure $ hydrate subst inner
  TyCons name params -> do
    -- For type constructors, instantiate each parameter with a fresh variable
    freshParams <- V.fromList <$> mapM (const freshVar) (V.toList params)
    pure $ TyCons name freshParams
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
hydrate subst (TyCons name params)
  | V.null params && M.member name subst = M.findWithDefault (TyCons name params) name subst
  -- If the type constructor has parameters, hydrate each parameter
  -- by substituting the type variables with their actual types
  | otherwise = TyCons name (V.map (hydrate subst) params)
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
unify (TyCons name1 pars1) (TyCons name2 pars2)
  | name1 == name2 && V.length pars1 == V.length pars2 = do
      subst <- zipWithM unify (V.toList pars1) (V.toList pars2)
      pure (foldl' M.union M.empty subst)
unify (TyFn par1 ret1) (TyFn par2 ret2)
  | length par1 == length par2 = do
      subst <- zipWithM unify par1 par2
      subst' <- unify ret1 ret2
      pure (foldl' M.union M.empty (subst ++ [subst']))
unify t1 t2 = throwError $ "Cannot unify types: " <> show t1 <> " and " <> show t2

-- True for types that can be considered equivalent to a void in the type system.
areVoidEquiv :: Ty -> Ty -> Bool
areVoidEquiv TyUnit TyUnknown = True
areVoidEquiv TyUnknown TyUnit = True
areVoidEquiv TyUnit TyAny = True
areVoidEquiv TyAny TyUnit = True
areVoidEquiv _ _ = False

-- TYPE INFERENCE

inferType :: TypeEnv -> Expr -> Typechecker Ty
inferType env' (ELit lit) = inferLiteral env' lit
inferType env' (EBinOp op l r)
  | op `elem` [Eq, Neq, Gt, Lt, Gte, Lte] = do
      _ <- unify <$> inferType env' l <*> inferType env' r
      pure TyBool
inferType env' (EBinOp op l r)
  | op `notElem` [Eq, Neq, Gt, Lt, Gte, Lte] = do
      lhs <- inferType env' l
      rhs <- inferType env' r
      subst <- unify lhs rhs

      -- Extend the environment with the substitution
      modify (\st -> st{env = M.union subst (env st)})
      pure $ lhs
inferType env' (EUnaryOp op e)
  | op == Neg = do
      ty <- inferType env' e
      case ty of
        TyInt -> pure TyInt
        TyFloat -> pure TyFloat
        _ -> throwError $ "Cannot apply negation to type: " <> show ty
  | op == Not = do
      ty <- inferType env' e
      if ty == TyBool
        then pure TyBool
        else throwError $ "Cannot apply not to type: " <> show ty
inferType _ _ = throwError "Unsupported expression type for type inference"

inferLiteral :: TypeEnv -> Literal -> Typechecker Ty
inferLiteral _ (IntLit _) = pure TyInt
inferLiteral _ (FloatLit _) = pure TyFloat
inferLiteral _ (StringLit _) = pure TyString
inferLiteral _ (BoolLit _) = pure TyBool
inferLiteral env' (ArrayLit elems) = inferArray env' elems

inferArray :: TypeEnv -> [Span Expr] -> Typechecker Ty
inferArray _ [] = pure $ TyArray TyUnknown -- Empty array, type is unknown
inferArray env' elems = do
  elemTypes <- mapM (inferType env' . value) elems
  let firstTy = case elemTypes of
        [] -> TyUnknown
        (x : _) -> x

  mapM_ (unify firstTy) (drop 1 elemTypes)
  pure $ TyArray firstTy