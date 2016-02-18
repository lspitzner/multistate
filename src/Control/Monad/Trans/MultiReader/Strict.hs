-- | The multi-valued version of mtl's Reader / ReaderT
module Control.Monad.Trans.MultiReader.Strict
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



import Data.HList.HList
import Data.HList.ContainsType

import Control.Monad.Trans.MultiReader.Class ( MonadMultiReader(..) )

import Control.Monad.State.Strict ( StateT(..)
                                  , MonadState(..)
                                  , evalStateT
                                  , mapStateT )
import Control.Monad.Reader       ( ReaderT(..) )
import Control.Monad.Trans.Class  ( MonadTrans
                                  , lift )
import Control.Monad.Writer.Class ( MonadWriter
                                  , listen
                                  , tell
                                  , writer
                                  , pass )

import Data.Functor.Identity      ( Identity )

import Control.Applicative        ( Applicative(..) )
import Control.Monad              ( liftM
                                  , ap
                                  , void )
import Control.Monad.Fix          ( MonadFix(..) )
import Control.Monad.IO.Class     ( MonadIO(..) )



-- | A Reader transformer monad patameterized by:
--   
-- * x - The list of types constituting the environment / input (to be read),
-- * m - The inner monad.
-- 
-- 'MultiReaderT' corresponds to mtl's 'ReaderT', but can contain
-- a heterogenous list of types.
-- 
-- This heterogenous list is represented using Types.Data.List, i.e:
-- 
--   * @'[]@ - The empty list,
--   * @a ': b@ - A list where @/a/@ is an arbitrary type
--     and @/b/@ is the rest list.
-- 
-- For example,
-- 
-- > MultiReaderT '[Int, Bool] :: (* -> *) -> (* -> *)
-- 
-- is a Reader transformer containing the types [Int, Bool].
newtype MultiReaderT x m a = MultiReaderT {
  runMultiReaderTRaw :: StateT (HList x) m a
}

-- | A MultiReader transformer carrying an empty state.
type MultiReaderTNull = MultiReaderT '[]

-- | A reader monad parameterized by the list of types x of the environment
-- / input to carry.
--
-- Similar to @Reader r = ReaderT r Identity@
type MultiReader x = MultiReaderT x Identity

instance (Functor f) => Functor (MultiReaderT x f) where
  fmap f = MultiReaderT . fmap f . runMultiReaderTRaw

instance (Applicative m, Monad m) => Applicative (MultiReaderT x m) where
  pure = MultiReaderT . pure
  (<*>) = ap

instance Monad m => Monad (MultiReaderT x m) where
  return = MultiReaderT . return
  k >>= f = MultiReaderT $ runMultiReaderTRaw k >>= (runMultiReaderTRaw . f)

instance MonadTrans (MultiReaderT x) where
  lift = MultiReaderT . lift

instance (Monad m, ContainsType a c)
      => MonadMultiReader a (MultiReaderT c m) where
  mAsk = MultiReaderT $ liftM getHListElem get

instance MonadFix m => MonadFix (MultiReaderT r m) where
  mfix f = MultiReaderT $ mfix (runMultiReaderTRaw . f)

-- methods

-- | A raw extractor of the contained HList (i.e. the complete Reader).
mGetRaw :: Monad m => MultiReaderT a m (HList a)
mGetRaw = MultiReaderT get

mPutRaw :: Monad m => HList s -> MultiReaderT s m ()
mPutRaw = MultiReaderT . put

-- | Map both the return value and the environment of a computation
-- using the given function.
--
-- Note that there is a difference to mtl's ReaderT,
-- where it is /not/ possible to modify the environment.
mapMultiReaderT :: (m (a, HList w) -> m' (a', HList w))
               -> MultiReaderT w m  a
               -> MultiReaderT w m' a'
mapMultiReaderT f = MultiReaderT . mapStateT f . runMultiReaderTRaw

runMultiReaderT   ::   Monad m => HList r -> MultiReaderT r m a -> m a
runMultiReaderT_  :: Functor m => HList r -> MultiReaderT r m a -> m ()
-- ghc too dumb for this shortcut, unfortunately
-- runMultiReaderT   s k = runMultiReaderTNil $ withMultiReaders s k
-- runMultiReaderT_  s k = runMultiReaderTNil $ withMultiReaders_ s k
runMultiReaderT  s k = evalStateT (runMultiReaderTRaw k) s
runMultiReaderT_ s k = void $ runStateT (runMultiReaderTRaw k) s

runMultiReaderTNil  ::   Monad m => MultiReaderT '[] m a -> m a
runMultiReaderTNil_ :: Functor m => MultiReaderT '[] m a -> m ()
runMultiReaderTNil  k = evalStateT (runMultiReaderTRaw k) HNil
runMultiReaderTNil_ k = void $ runStateT (runMultiReaderTRaw k) HNil

withMultiReader   :: Monad m => r -> MultiReaderT (r ': rs) m a -> MultiReaderT rs m a
withMultiReader_  :: (Functor m, Monad m) => r -> MultiReaderT (r ': rs) m a -> MultiReaderT rs m ()
withMultiReader  x k = MultiReaderT $
  get >>= lift . evalStateT (runMultiReaderTRaw k) . (x :+:)
withMultiReader_ x k = void $ withMultiReader x k

withMultiReaders  :: Monad m => HList r1 -> MultiReaderT (Append r1 r2) m a -> MultiReaderT r2 m a
withMultiReaders_ :: (Functor m, Monad m) => HList r1 -> MultiReaderT (Append r1 r2) m a -> MultiReaderT r2 m ()
withMultiReaders  HNil       = id
withMultiReaders  (x :+: xs) = withMultiReaders xs . withMultiReader x
withMultiReaders_ HNil       = liftM (const ())
withMultiReaders_ (x :+: xs) = withMultiReaders_ xs . withMultiReader_ x

inflateReader :: (Monad m, ContainsType r rs)
              => ReaderT r m a
              -> MultiReaderT rs m a
inflateReader k = mAsk >>= lift . runReaderT k

-- foreign lifting instances

instance (MonadState s m) => MonadState s (MultiReaderT c m) where
  put   = lift . put
  get   = lift $ get
  state = lift . state

instance (MonadWriter w m) => MonadWriter w (MultiReaderT c m) where
  writer = lift . writer
  tell   = lift . tell
  listen = MultiReaderT .
    mapStateT (liftM (\((a,w), w') -> ((a, w'), w)) . listen) .
    runMultiReaderTRaw
  pass = MultiReaderT .
    mapStateT (pass . liftM (\((a, f), w) -> ((a, w), f))) .
    runMultiReaderTRaw  

instance MonadIO m => MonadIO (MultiReaderT c m) where
  liftIO = lift . liftIO
