-- | The MonadMultiReader type-class
module Control.Monad.Trans.MultiGet.Class
  (
  -- * MonadMultiReader class
    MonadMultiGet(..)
  )
where



import Control.Monad.Trans.Class  ( MonadTrans
                                  , lift )



-- | In contrast to MonadMultiReader, MonadMultiGet is defined for State too,
-- so it corresponds to read-access of any kind.
--
-- Note however that for MultiRWS, only the values from the @state@ part can
-- be accessed via @MonadMultiGet@, due to limitations of the design of
-- @MultiRWS@ and of the type system. This is issue is resolved in the
-- @MultiGST@ type.
class (Monad m) => MonadMultiGet a m where
  mGet :: m a -- ^ Access to a specific type in the environment.

instance (MonadTrans t, Monad (t m), MonadMultiGet a m)
      => MonadMultiGet a (t m) where
  mGet = lift $ mGet

