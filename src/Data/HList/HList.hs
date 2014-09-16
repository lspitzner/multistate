{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- | A GADT HList implementation
--
-- Probably exists somewhere else already, but why add a dependency
-- for something so simple.
module Data.HList.HList
  ( HList(..)
) where



import Prelude hiding (reverse)

import Types.Data.List ( Cons, Null )
import Data.Monoid



data HList a where
  TCons :: x -> HList xs -> HList (Cons x xs)
  TNull :: HList Null

instance Show (HList Null) where
  show _ = "()"

instance (Show a, Show (HList b)) => Show (HList (Cons a b)) where
  show (TCons x y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Monoid (HList Null) where
  mempty = TNull
  mappend _ _ = TNull

instance (Monoid x, Monoid (HList xs))
      => Monoid (HList (Cons x xs))
  where
    mempty = TCons mempty mempty
    mappend (TCons x1 xs1) (TCons x2 xs2) =
      TCons (x1 `mappend` x2)
            (xs1 `mappend` xs2)
