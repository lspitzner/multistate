{-# LANGUAGE CPP #-}

-- | Class to provide type-driven access to elements of a HList
module Data.HList.ContainsType
  ( ContainsType(..)
  )
where



import Data.HList.HList



----------------------------------------
-- class ContainsType
-- | for get/put of a value in a HList, with type-directed lookup.
class ContainsType a c where
  setHListElem :: a -> HList c -> HList c
  getHListElem :: HList c -> a

#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPPING #-} ContainsType a (a ': xs) where
#else
instance ContainsType a (a ': xs) where
#endif
  setHListElem a xs = a :+: case xs of (_ :+: xr) -> xr
  getHListElem (x :+: _) = x

instance (ContainsType a xs) => ContainsType a (x ': xs) where
  setHListElem a (x :+: xr) = x :+: setHListElem a xr
  getHListElem (_ :+: xr) = getHListElem xr

