{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The multi-valued version of mtl's Writer / WriterT
module Control.Monad.Trans.MultiWriter.Strict
  (
  -- * MultiWriterT
    MultiWriterT(..)
  , MultiWriterTNull
  , MultiWriter
  -- * MonadMultiWriter class
  , MonadMultiWriter(..)
  -- * run-functions
  , runMultiWriterT
  , runMultiWriterTAW
  , runMultiWriterTWA
  , runMultiWriterTW
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
  -- * inflate-function (run WriterT in MultiWriterT)
  , inflateWriter
  -- * other functions
  , mapMultiWriterT
  , mGetRaw
  , mPutRaw
  )
where



import Data.HList.HList
import Data.HList.ContainsType

import Control.Monad.Trans.MultiWriter.Class ( MonadMultiWriter(..) )

import Control.Monad.State.Strict  ( StateT(..)
                                   , MonadState(..)
                                   , execStateT
                                   , evalStateT
                                   , mapStateT )
import Control.Monad.Writer.Strict ( WriterT(..) )
import Control.Monad.Trans.Class   ( MonadTrans
                                   , lift )
import Control.Monad.Writer.Class  ( MonadWriter
                                   , listen
                                   , tell
                                   , writer
                                   , pass )

import Data.Functor.Identity       ( Identity )

import Control.Applicative         ( Applicative(..) )
import Control.Monad               ( liftM
                                   , ap
                                   , void )
import Control.Monad.Fix           ( MonadFix(..) )
import Control.Monad.IO.Class      ( MonadIO(..) )

import Data.Monoid



-- | A Writer transformer monad patameterized by:
--   
-- * x - The list of types that can be written (Monoid instances).
-- * m - The inner monad.
-- 
-- 'MultiWriterT' corresponds to mtl's 'WriterT', but can contain
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
-- > MultiWriterT '[Int, Bool] :: (* -> *) -> (* -> *)
-- 
-- is a Writer transformer containing the types [Int, Bool].
newtype MultiWriterT x m a = MultiWriterT {
  runMultiWriterTRaw :: StateT (HList x) m a
}

-- | A MultiWriter transformer carrying an empty state.
type MultiWriterTNull = MultiWriterT '[]

type MultiWriter x a = MultiWriterT x Identity a

instance (Functor f) => Functor (MultiWriterT x f) where
  fmap f = MultiWriterT . fmap f . runMultiWriterTRaw

instance (Applicative m, Monad m) => Applicative (MultiWriterT x m) where
  pure = MultiWriterT . pure
  (<*>) = ap

instance Monad m => Monad (MultiWriterT x m) where
  return = MultiWriterT . return
  k >>= f = MultiWriterT $ runMultiWriterTRaw k >>= (runMultiWriterTRaw.f)

instance MonadTrans (MultiWriterT x) where
  lift = MultiWriterT . lift

instance (Monad m, ContainsType a c, Monoid a)
      => MonadMultiWriter a (MultiWriterT c m) where
  mTell v = MultiWriterT $ do
    x <- get
    put $ setHListElem (getHListElem x `mappend` v) x

instance MonadFix m => MonadFix (MultiWriterT w m) where
  mfix f = MultiWriterT $ mfix (runMultiWriterTRaw . f)

-- methods

-- | A raw extractor of the contained HList (i.e. the complete state).
mGetRaw :: Monad m => MultiWriterT a m (HList a)
mGetRaw = MultiWriterT get

mPutRaw :: Monad m => HList s -> MultiWriterT s m ()
mPutRaw = MultiWriterT . put

-- | Map both the return value and the state of a computation
-- using the given function.
mapMultiWriterT :: (m (a, HList w) -> m' (a', HList w))
               -> MultiWriterT w m  a
               -> MultiWriterT w m' a'
mapMultiWriterT f = MultiWriterT . mapStateT f . runMultiWriterTRaw

runMultiWriterT   :: (Monoid (HList w), Functor m) => MultiWriterT w m a -> m (a, HList w)
runMultiWriterTAW :: (Monoid (HList w), Functor m) => MultiWriterT w m a -> m (a, HList w)
runMultiWriterTWA :: (Monoid (HList w),   Monad m) => MultiWriterT w m a -> m (HList w, a)
runMultiWriterTW  :: (Monoid (HList w),   Monad m) => MultiWriterT w m a -> m (HList w)
runMultiWriterT     = runMultiWriterTAW
runMultiWriterTAW k = runStateT (runMultiWriterTRaw k) mempty
runMultiWriterTWA k = (\(a,b) -> (b,a)) `liftM` runStateT (runMultiWriterTRaw k) mempty
runMultiWriterTW  k = execStateT (runMultiWriterTRaw k) mempty

runMultiWriterTNil  ::   Monad m => MultiWriterT '[] m a -> m a
runMultiWriterTNil_ :: Functor m => MultiWriterT '[] m a -> m ()
runMultiWriterTNil  k = evalStateT (runMultiWriterTRaw k) HNil
runMultiWriterTNil_ k = void $ runStateT (runMultiWriterTRaw k) HNil

withMultiWriter   :: (Monoid w, Monad m) => MultiWriterT (w ': ws) m a -> MultiWriterT ws m (a, w)
withMultiWriterAW :: (Monoid w, Monad m) => MultiWriterT (w ': ws) m a -> MultiWriterT ws m (a, w)
withMultiWriterWA :: (Monoid w, Monad m) => MultiWriterT (w ': ws) m a -> MultiWriterT ws m (w, a)
withMultiWriterW  :: (Monoid w, Monad m) => MultiWriterT (w ': ws) m a -> MultiWriterT ws m w
withMultiWriter = withMultiWriterAW
withMultiWriterAW k = MultiWriterT $ do
  w <- get
  (a, w') <- lift $ runStateT (runMultiWriterTRaw k) (mempty :+: w)
  case w' of x' :+: wr' -> do put wr'; return (a, x')
withMultiWriterWA k = (\(a,b) -> (b,a)) `liftM` withMultiWriterAW k
withMultiWriterW  k = snd `liftM` withMultiWriterAW k

withMultiWriters   :: forall w1 w2 m a
               . (Monoid (HList w1), Monad m, HInit w1)
              => MultiWriterT (Append w1 w2) m a
              -> MultiWriterT w2 m (a, HList w1)
withMultiWritersAW :: forall w1 w2 m a
               . (Monoid (HList w1), Monad m, HInit w1)
              => MultiWriterT (Append w1 w2) m a
              -> MultiWriterT w2 m (a, HList w1)
withMultiWritersWA :: forall w1 w2 m a
               . (Monoid (HList w1), Monad m, HInit w1)
              => MultiWriterT (Append w1 w2) m a
              -> MultiWriterT w2 m (HList w1, a)
-- withMultiWritersA would have too much ambiguity for what the ws are
-- (one could use a Proxy, but that does not seem to be worth the effort)
-- same reasoning for withMultiWriters_
withMultiWritersW  :: forall w1 w2 m a
               . (Monoid (HList w1), Monad m, HInit w1)
              => MultiWriterT (Append w1 w2) m a
              -> MultiWriterT w2 m (HList w1)
withMultiWriters = withMultiWritersAW
withMultiWritersAW k = MultiWriterT $ do
  w <- get
  (a, ws') <- lift $ runStateT (runMultiWriterTRaw k) (hAppend (mempty :: HList w1) w)
  let (o, w') = hSplit ws'
  put w'
  return $ (a, o)
withMultiWritersWA k = MultiWriterT $ do
  w <- get
  (a, ws') <- lift $ runStateT (runMultiWriterTRaw k) (hAppend (mempty :: HList w1) w)
  let (o, w') = hSplit ws'
  put w'
  return $ (o, a)
withMultiWritersW k  = MultiWriterT $ do
  w <- get
  ws' <- lift $ execStateT (runMultiWriterTRaw k) (hAppend (mempty :: HList w1) w)
  let (o, w') = hSplit ws'
  put w'
  return $ o

inflateWriter :: (Monad m, Monoid w, ContainsType w ws)
              => WriterT w m a
              -> MultiWriterT ws m a
inflateWriter k = do
  (x, w) <- lift $ runWriterT k
  mTell w
  return x

-- foreign lifting instances

instance (MonadState s m) => MonadState s (MultiWriterT c m) where
  put   = lift . put
  get   = lift $ get
  state = lift . state

instance (MonadWriter w m) => MonadWriter w (MultiWriterT c m) where
  writer = lift . writer
  tell   = lift . tell
  listen = MultiWriterT .
    mapStateT (liftM (\((a,w), w') -> ((a, w'), w)) . listen) .
    runMultiWriterTRaw
  pass = MultiWriterT .
    mapStateT (pass . liftM (\((a, f), w) -> ((a, w), f))) .
    runMultiWriterTRaw

instance MonadIO m => MonadIO (MultiWriterT c m) where
  liftIO = lift . liftIO
