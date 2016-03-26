-- | The multi-valued version of mtl's Reader / ReaderT
-- / MonadReader
module Control.Monad.Trans.MultiReader
  ( -- * MultiReaderT
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
  -- * with-functions (single reader)
  , withMultiReader
  , withMultiReader_
  -- * with-functions (multiple readers)
  , withMultiReaders
  , withMultiReaders_
  -- * without-function (single reader)
  , withoutMultiReader
  -- * inflate-function (run ReaderT in MultiReaderT)
  , inflateReader
  -- * other functions
  , mapMultiReaderT
  , mGetRaw
  , mPutRaw
) where



-- just re-export
import Control.Monad.Trans.MultiReader.Lazy
