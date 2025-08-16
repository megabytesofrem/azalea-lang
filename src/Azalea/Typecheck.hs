{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Azalea.Typecheck
  ( module Azalea.Typecheck.Core
  , module Azalea.Typecheck.Algorithm
  , runTypechecker
  )
where

import Azalea.Typecheck.Algorithm
import Azalea.Typecheck.Core
import Control.Monad.Except (runExcept)
import Control.Monad.State (runStateT)

-- | Run the typechecker with an initial state.
runTypechecker :: Typechecker a -> TypecheckState -> Either String (a, TypecheckState)
runTypechecker m s = runExcept (runStateT (unTypechecker m) s)