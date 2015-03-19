-- | The multi-valued version of mtl's MonadWriter
module Control.Monad.Trans.MultiWriter.Class
  (
  -- * MonadMultiWriter class
    MonadMultiWriter(..)
  )
where



import Control.Monad.Trans.Class  ( MonadTrans
                                  , lift )

import Data.Monoid



-- TODO: some haddock
class (Monad m, Monoid a) => MonadMultiWriter a m where
  mTell :: a -> m ()  

instance (MonadTrans t, Monad (t m), MonadMultiWriter a m)
      => MonadMultiWriter a (t m) where
  mTell = lift . mTell
