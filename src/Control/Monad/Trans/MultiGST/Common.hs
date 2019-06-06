{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Common definitions for MultiGST.Strict and MultiGST.Lazy
module Control.Monad.Trans.MultiGST.Common
  ( HListM(..)
  , CanReadWrite(..)
  , CanReadWriteFlag(..)
  , HListMContainsImplication
  , HListMContains(..)
  , ContainsReader
  , ContainsState
  , ContainsWriter
  , CanWriteConstraint
  , AppendM
  , HListMReaders
  , AppendMReaders
  , HListMGettableClass(..)
  )
where



import Data.Kind (Type)
import Data.Semigroup
import qualified Data.HList.HList as HList

import Control.Monad.Trans.MultiReader.Class
import Control.Monad.Trans.MultiWriter.Class
import Control.Monad.Trans.MultiState.Class

import GHC.Exts (Constraint)



data CanReadWrite a
  = Gettable a
  | Settable a
  | Tellable a

data CanReadWriteFlag
  = GettableFlag
  | SettableFlag
  | TellableFlag

type family HListMContainsImplication (can :: CanReadWriteFlag) t cts :: Constraint where
  HListMContainsImplication 'GettableFlag t cts = ()
  HListMContainsImplication 'TellableFlag t cts = ()
  HListMContainsImplication 'SettableFlag t cts = HListMContains 'GettableFlag t cts

class HListMContainsImplication can t cts => HListMContains (can :: CanReadWriteFlag) t cts where
  readHListMElem  :: HListM cts -> t
  writeHListMElem :: CanWriteConstraint can => t -> HListM cts -> HListM cts

type ContainsReader = HListMContains 'GettableFlag
type ContainsState  = HListMContains 'SettableFlag
type ContainsWriter = HListMContains 'TellableFlag

instance
#if MIN_VERSION_base(4,8,0)
  {-# OVERLAPPING #-}
#endif
    HListMContains 'GettableFlag x ('Gettable x ': tr) where
  readHListMElem (x :+-: _) = x
  writeHListMElem = error "writeHListMElem CanRead"
  -- ghc is too stupid to acknowledge that the constraint cannot be fulfilled..

instance
#if MIN_VERSION_base(4,8,0)
  {-# OVERLAPPING #-}
#endif
    HListMContains 'GettableFlag x ('Settable x ': tr) where
  readHListMElem (x :++: _) = x
  writeHListMElem = error "writeHListMElem CanRead"
  -- ghc is too stupid to acknowledge that the constraint cannot be fulfilled..

instance HListMContains 'GettableFlag x ts => HListMContains 'GettableFlag x (t ': ts) where
  readHListMElem (_ :+-: xr) = readHListMElem @'GettableFlag xr
  readHListMElem (_ :-+: xr) = readHListMElem @'GettableFlag xr
  readHListMElem (_ :++: xr) = readHListMElem @'GettableFlag xr
  writeHListMElem = error "writeHListMElem CanRead"

instance
#if MIN_VERSION_base(4,8,0)
  {-# OVERLAPPING #-}
#endif
    HListMContains 'TellableFlag x ('Tellable x ': tr) where
  readHListMElem (x :-+: _) = x
  writeHListMElem x ts = case ts of (_ :-+: tr) -> x :-+: tr

instance HListMContains 'TellableFlag x ts => HListMContains 'TellableFlag x (t ': ts) where
  readHListMElem (_ :+-: xr) = readHListMElem @'TellableFlag xr
  readHListMElem (_ :-+: xr) = readHListMElem @'TellableFlag xr
  readHListMElem (_ :++: xr) = readHListMElem @'TellableFlag xr
  writeHListMElem x (t :+-: tr) = t :+-: writeHListMElem @'TellableFlag x tr
  writeHListMElem x (t :-+: tr) = t :-+: writeHListMElem @'TellableFlag x tr
  writeHListMElem x (t :++: tr) = t :++: writeHListMElem @'TellableFlag x tr

instance
#if MIN_VERSION_base(4,8,0)
  {-# OVERLAPPING #-}
#endif
    HListMContains 'GettableFlag x ('Settable x ': tr)
    => HListMContains 'SettableFlag x ('Settable x ': tr) where
  readHListMElem (x :++: _) = x
  writeHListMElem x ts = case ts of (_ :++: tr) -> x :++: tr

instance HListMContains 'SettableFlag x ts => HListMContains 'SettableFlag x (t ': ts) where
  readHListMElem (_ :+-: xr) = readHListMElem @'SettableFlag xr
  readHListMElem (_ :-+: xr) = readHListMElem @'SettableFlag xr
  readHListMElem (_ :++: xr) = readHListMElem @'SettableFlag xr
  writeHListMElem x (t :+-: tr) = t :+-: writeHListMElem @'SettableFlag x tr
  writeHListMElem x (t :-+: tr) = t :-+: writeHListMElem @'SettableFlag x tr
  writeHListMElem x (t :++: tr) = t :++: writeHListMElem @'SettableFlag x tr


type family CanWriteConstraint (f :: CanReadWriteFlag) :: Constraint where
  CanWriteConstraint 'TellableFlag = ()
  CanWriteConstraint 'SettableFlag = ()

data HListM :: [CanReadWrite Type] -> Type where
  HNilM :: HListM '[]
  (:+-:) :: x -> HListM xr -> HListM ('Gettable x ': xr)
  (:++:) :: x -> HListM xr -> HListM ('Settable x ': xr)
  (:-+:) :: x -> HListM xr -> HListM ('Tellable x ': xr)

instance Semigroup (HListM '[]) where
  _ <> _ = HNilM

instance Monoid (HListM '[]) where
  mempty = HNilM
  mappend = (<>)

instance Eq (HListM '[]) where
  HNilM == HNilM = True
  HNilM /= HNilM = False

instance (Eq x, Eq (HListM xs))
      => Eq (HListM ('Gettable x ': xs))
  where
    x1 :+-: xr1 == x2 :+-: xr2 = x1==x2 && xr1==xr2
    x1 :+-: xr1 /= x2 :+-: xr2 = x1/=x2 || xr1/=xr2
instance (Eq x, Eq (HListM xs))
      => Eq (HListM ('Tellable x ': xs))
  where
    x1 :-+: xr1 == x2 :-+: xr2 = x1==x2 && xr1==xr2
    x1 :-+: xr1 /= x2 :-+: xr2 = x1/=x2 || xr1/=xr2
instance (Eq x, Eq (HListM xs))
      => Eq (HListM ('Settable x ': xs))
  where
    x1 :++: xr1 == x2 :++: xr2 = x1==x2 && xr1==xr2
    x1 :++: xr1 /= x2 :++: xr2 = x1/=x2 || xr1/=xr2

type family AppendM (l1 :: [CanReadWrite Type]) (l2 :: [CanReadWrite Type]) :: [CanReadWrite Type] where
  AppendM '[] l2 = l2
  AppendM (car1 ': cdr2) l2 = car1 ': AppendM cdr2 l2

type family HListMReaders (l :: [Type]) :: [CanReadWrite Type] where
  HListMReaders '[] = '[]
  HListMReaders (t ': tr) = 'Gettable t ': HListMReaders tr

type family AppendMReaders (l1 :: [Type]) (l2 :: [CanReadWrite Type]) :: [CanReadWrite Type] where
  AppendMReaders '[] l2 = l2
  AppendMReaders (t ': tr) l2 = 'Gettable t ': AppendMReaders tr l2

class HListMGettableClass ts where
  type HListMGettableOnly ts :: [Type]
  hListMGettableOnly :: HListM ts -> HList.HList (HListMGettableOnly ts)

instance HListMGettableClass '[] where
  type HListMGettableOnly '[] = '[]
  hListMGettableOnly HNilM = HList.HNil

instance HListMGettableClass tr => HListMGettableClass ('Gettable t ': tr) where
  type HListMGettableOnly ('Gettable t ': tr) = (t ': HListMGettableOnly tr)
  hListMGettableOnly (t :+-: tr) = t HList.:+: hListMGettableOnly tr
instance HListMGettableClass tr => HListMGettableClass ('Settable t ': tr) where
  type HListMGettableOnly ('Settable t ': tr) = HListMGettableOnly tr
  hListMGettableOnly (_ :++: tr) = hListMGettableOnly tr
instance HListMGettableClass tr => HListMGettableClass ('Tellable t ': tr) where
  type HListMGettableOnly ('Tellable t ': tr) = HListMGettableOnly tr
  hListMGettableOnly (_ :-+: tr) = hListMGettableOnly tr
