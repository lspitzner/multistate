-- | Class to provide type-driven access to elements of a HList
module Data.HList.ContainsType
  ( ContainsType(..)
  )
where



import Data.HList.HList



class ContainsType a c where
  setHListElem :: a -> HList c -> HList c
  getHListElem :: HList c -> a

instance ContainsType a (a ': xs) where
  setHListElem a xs = a :+: case xs of (_ :+: xr) -> xr
  getHListElem (x :+: _) = x

instance (ContainsType a xs) => ContainsType a (x ': xs) where
  setHListElem a (x :+: xr) = x :+: setHListElem a xr
  getHListElem (_ :+: xr) = getHListElem xr
