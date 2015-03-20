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
  setHListElem a (_ :+: xs) = a :+: xs
  getHListElem (x :+: _) = x

instance (ContainsType a xs) => ContainsType a (x ': xs) where
  setHListElem a (x :+: xs) = x :+: setHListElem a xs
  getHListElem (_ :+: xs) = getHListElem xs
