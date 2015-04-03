-- | The multi-valued version of mtl's Reader / ReaderT
-- / MonadReader
module Control.Monad.MultiReader {-# DEPRECATED "Use Control.Monad.Trans.MultiReader instead" #-}
  (
  -- * MultiReaderT
    MultiReaderT(..)
  , MultiReaderTNull
  , MultiReader
  -- * MonadMultiReader class
  , MonadMultiReader(..)
  -- * run-functions
  , runMultiReaderT
  , runMultiReaderT_
  , runMultiReaderTNil
  , runMultiReaderTNil_
  -- * with-functions (single Reader)
  , withMultiReader
  , withMultiReader_
  -- * with-functions (multiple Readers)
  , withMultiReaders
  , withMultiReaders_
  -- * inflate-function (run ReaderT in MultiReaderT)
  , inflateReader
  -- * other functions
  , mapMultiReaderT
  , mGetRaw
  , mPutRaw
) where



-- just re-export
import Control.Monad.Trans.MultiReader.Class
import Control.Monad.Trans.MultiReader.Lazy
