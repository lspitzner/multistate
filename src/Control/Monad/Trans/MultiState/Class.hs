-- | The multi-valued version of mtl's MonadState
module Control.Monad.Trans.MultiState.Class
  (
  -- * MonadMultiState class
    MonadMultiState(..)
  )
where



import Control.Monad.Trans.Class  ( MonadTrans
                                  , lift )



-- | All methods must be defined.
--
-- The idea is: Any monad stack is instance of @MonadMultiState a@, iff
-- the stack contains a @MultiStateT x@ with /a/ element of /x/.
class (Monad m) => MonadMultiState a m where
  -- | state set function for values of type @a@.
  mSet :: a -> m ()
  -- | state get function for values of type @a@.
  mGet :: m a

instance (MonadTrans t, Monad (t m), MonadMultiState a m)
      => MonadMultiState a (t m) where
  mSet = lift . mSet
  mGet = lift $ mGet
