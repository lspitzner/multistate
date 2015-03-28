{-# LANGUAGE FlexibleContexts #-}

-- | The multi-valued version of mtl's Writer / WriterT
-- / MonadWriter
module Control.Monad.MultiWriter {-# DEPRECATED "Use Control.Monad.Trans.MultiWriter instead" #-}
  ( -- * MultiWriterT
    MultiWriterT(..)
  , MultiWriterTNull
  , MultiWriter
  -- * MonadMultiWriter class
  , MonadMultiWriter(..)
  -- * run-functions
  , runMultiWriterT
  , runMultiWriterTAW
  , runMultiWriterTWA
  -- , runMultiWriterTA
  , runMultiWriterTW
  -- , runMultiWriterT_
  , runMultiWriterTNil
  , runMultiWriterTNil_
  -- * with-functions (single Writer)
  , withMultiWriter
  , withMultiWriterAW
  , withMultiWriterWA
  , withMultiWriterW
  -- * with-functions (multiple Writers)
  , withMultiWriters
  , withMultiWritersAW
  , withMultiWritersWA
  , withMultiWritersW
  -- * other functions
  , mapMultiWriterT
  , mGetRaw
  , mPutRaw
  )
where



-- just re-exports
import Control.Monad.Trans.MultiWriter.Class
import Control.Monad.Trans.MultiWriter.Lazy
