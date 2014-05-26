{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Data.HList.HList(
  HList(..)
) where



import Prelude hiding (reverse)

import Types.Data.List ( Cons, Null )



data HList a where
  TCons :: x -> HList xs -> HList (Cons x xs)
  TNull :: HList Null

instance Show (HList Null) where
  show _ = "()"

instance (Show a, Show (HList b)) => Show (HList (Cons a b)) where
  show (TCons x y) = "(" ++ show x ++ "," ++ show y ++ ")"
