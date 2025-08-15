module Azalea.AST.Span (Span (..), mkSpanned) where

import Text.Megaparsec (SourcePos)

data Span a = Span
  { value :: a
  , pos :: SourcePos
  }
  deriving (Show, Eq)

-- | Create a value wrapped in a `Span` with the given source position.
mkSpanned :: a -> SourcePos -> Span a
mkSpanned val position = Span{value = val, pos = position}

instance Functor Span where
  fmap f (Span val p) = Span (f val) p
