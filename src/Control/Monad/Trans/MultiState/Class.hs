-- | The multi-valued version of mtl's MonadState
module Control.Monad.Trans.MultiState.Class
  (
  -- * MonadMultiState class
    MonadMultiGet(..)
  , MonadMultiState(..)
  )
where



import Control.Monad.Trans.MultiGet.Class

import Control.Monad.Trans.Class  ( MonadTrans
                                  , lift )



-- The idea is: Any monad stack is instance of @MonadMultiState a@, iff
-- the stack contains a @MultiStateT s m@ with /a/ element of /s/,
-- or a @MultiRWST r w s m@ with /a/ element of /s/.
class (MonadMultiGet a m) => MonadMultiState a m where
  mSet :: a -> m ()

instance (MonadTrans t, Monad (t m), MonadMultiState a m)
      => MonadMultiState a (t m) where
  mSet = lift . mSet

