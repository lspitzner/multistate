-- | The multi-valued version of mtl's MonadReader
module Control.Monad.Trans.MultiReader.Class
  (
  -- * MonadMultiReader class
    MonadMultiReader(..)
  )
where



import Control.Monad.Trans.Class  ( MonadTrans
                                  , lift )



-- | All methods must be defined.
--
-- The idea is: Any monad stack is instance of @MonadMultiReader a@, iff
-- the stack contains a @MultiReaderT x@ with /a/ element of /x/.
class (Monad m) => MonadMultiReader a m where
  mAsk :: m a -- ^ Access to a specific type in the environment.

instance (MonadTrans t, Monad (t m), MonadMultiReader a m)
      => MonadMultiReader a (t m) where
  mAsk = lift $ mAsk

{-
it might make seem straightforward to define the following class that
corresponds to other transformer classes. But while we can define the the
class and its instances, there is a problem we try to use it, assuming that we
do not want to annotate the full type signature of the config:
  the type of the config can not be inferred properly. we would need a feature
  like "infer, as return type for this function, the only type for
  which there exists a valid chain of instance definitions that is needed to
  by this function".
  In other words, it is impossible to use the mAskRaw function without
  binding a concrete type for c, because otherwise the inference runs into
  some overlapping instances.
For this reason, I removed this type class and created a non-class function
mAskRaw, for which the type inference works because it involves no
type classes.
  lennart spitzner
-}

--class (Monad m) => MonadMultiReaderRaw c m where
--  mAskRaw :: m (HList c)

--instance (MonadTrans t, Monad (t m), MonadMultiReaderRaw c m)
--      => MonadMultiReaderRaw c (t m) where
--  mAskRaw = lift $ mAskRaw

--instance (Monad m) => MonadMultiReaderRaw a (MultiReaderT a m) where
--  mAskRaw = MultiReaderT $ get