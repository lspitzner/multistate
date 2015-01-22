{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | A GADT HList implementation
--
-- Probably exists somewhere else already, but why add a dependency
-- for something so simple.
module Data.HList.HList
  ( HList(..)
  , Append
) where



import Prelude hiding (reverse)

import Data.Monoid



data HList :: [*] -> * where
  HNil :: HList '[]
  (:+:) :: x -> HList xs -> HList (x ': xs)
  -- TCons :: x -> HList xs -> HList (Cons x xs)
  -- TNull :: HList Null

instance Show (HList '[]) where
  show _ = "()"

instance (Show a, Show (HList b)) => Show (HList (a ': b)) where
  show (x :+: y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Monoid (HList '[]) where
  mempty = HNil
  mappend _ _ = HNil

instance (Monoid x, Monoid (HList xs))
      => Monoid (HList (x ': xs))
  where
    mempty = mempty :+: mempty
    mappend (x1 :+: xs1) (x2 :+: xs2) = (x1 `mappend` x2)
                                    :+: (xs1 `mappend` xs2)

instance Eq (HList '[]) where
  HNil == HNil = True
  HNil /= HNil = False

instance (Eq x, Eq (HList xs))
      => Eq (HList (x ': xs))
  where
    x1 :+: xr1 == x2 :+: xr2 = x1==x2 && xr1==xr2
    x1 :+: xr1 /= x2 :+: xr2 = x1/=x2 || xr1/=xr2

type family Append (l1::[*]) (l2::[*]) :: [*]
type instance Append '[] l2 = l2
type instance Append (car1 ': cdr2) l2 = car1 ': Append cdr2 l2
