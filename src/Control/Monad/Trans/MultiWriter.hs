{-# LANGUAGE FlexibleContexts #-}

-- | The multi-valued version of mtl's Writer / WriterT
-- / MonadWriter
module Control.Monad.Trans.MultiWriter
  ( -- * MultiWriterT
    MultiWriterT(..)
  , MultiWriterTNull
  , MultiWriter
  -- * MonadMultiWriter class
  , MonadMultiWriter(..)
  -- * functions
  , mGetRaw
  , withMultiWriter
  , withMultiWriters
  , runMultiWriterT
  , execMultiWriterT
  , mapMultiWriterT
  )
where



-- just re-exports
import Control.Monad.Trans.MultiWriter.Class
import Control.Monad.Trans.MultiWriter.Lazy
