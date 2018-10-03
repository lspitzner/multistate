{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A GADT HList implementation
--
-- There exist other implementations of HList on hackage, but none seem to
-- be reliably maintained.
module Data.HList.HList
  ( HList(..)
  , Append
  , hAppend
  , HInit(..)
) where



import Prelude hiding (reverse)

import Data.Monoid (Monoid, mappend, mempty)
import Data.Semigroup

import Data.Proxy



data HList :: [*] -> * where
  HNil :: HList '[]
  (:+:) :: x -> HList xs -> HList (x ': xs)
  -- TCons :: x -> HList xs -> HList (Cons x xs)
  -- TNull :: HList Null

infixr 5 :+:

instance Show (HList '[]) where
  show _ = "HNil"

instance (Show a, Show (HList b)) => Show (HList (a ': b)) where
  show (x :+: y) = "(" ++ show x ++ ":+:" ++ show y ++ ")"

instance Semigroup (HList '[]) where
  _ <> _ = HNil
instance (Semigroup x, Semigroup (HList xs))
      => Semigroup (HList (x ': xs))
  where
    (x1 :+: xs1) <> (x2 :+: xs2) = (x1 <> x2) :+: (xs1 <> xs2)

instance Monoid (HList '[]) where
  mempty = HNil
  mappend = (<>)
instance (Semigroup x, Monoid x, Semigroup (HList xs), Monoid (HList xs))
      => Monoid (HList (x ': xs))
  where
    mempty = mempty :+: mempty
    mappend = (<>)

instance Eq (HList '[]) where
  HNil == HNil = True
  HNil /= HNil = False

instance (Eq x, Eq (HList xs))
      => Eq (HList (x ': xs))
  where
    x1 :+: xr1 == x2 :+: xr2 = x1==x2 && xr1==xr2
    x1 :+: xr1 /= x2 :+: xr2 = x1/=x2 || xr1/=xr2

-- cannot use the closed variant because of ghc-7.8.4.
-- (was not investigated more closely; there simply
--  is some syntax error for code which works fine with ghc-7.10.)
type family Append (l1::[*]) (l2::[*]) :: [*]
type instance Append '[] l2 = l2
type instance Append (car1 ': cdr2) l2 = car1 ': Append cdr2 l2

hAppend :: HList ts1 -> HList ts2 -> HList (Append ts1 ts2)
hAppend HNil l = l
hAppend (x:+:xs) l = x :+: hAppend xs l

class HInit (l1 :: [*]) where
  hInit :: forall l2 . Proxy l2 -> HList (Append l1 l2) -> HList l1
  hSplit :: forall l2 . HList (Append l1 l2) -> (HList l1, HList l2)

instance HInit '[] where
  hInit _ _ = HNil
  hSplit l = (HNil, l)
instance HInit l1 => HInit (x ': l1) where
  hInit p (x :+: xs)  = x :+: hInit p xs
#if !MIN_VERSION_base(4,9,0)
  hInit _ _           = error "cannot happen" -- see ghc trac #3927
#endif
  hSplit (x :+: xs) = let (l1, l2) = hSplit xs
                       in (x :+: l1, l2)
#if !MIN_VERSION_base(4,9,0)
  hSplit _          = error "cannot happen"
#endif
