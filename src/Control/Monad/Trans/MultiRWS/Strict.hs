{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The multi-valued version of mtl's RWS / RWST
module Control.Monad.Trans.MultiRWS.Strict
  (
  -- * MultiRWST
    MultiRWST(..)
  , MultiRWSTNull
  , MultiRWS
  -- * MonadMulti classes
  , MonadMultiReader(..)
  , MonadMultiWriter(..)
  , MonadMultiState(..)
  -- * run-functions (extracting from RWST)
  , runMultiRWST
  , runMultiRWSTASW
  , runMultiRWSTW
  , runMultiRWSTAW
  , runMultiRWSTSW
  , runMultiRWSTNil
  , runMultiRWSTNil_
  -- * with-functions (extending an RWST)
  , withMultiReader
  , withMultiReader_
  , withMultiReaders
  , withMultiReaders_
  , withMultiWriter
  , withMultiWriterAW
  , withMultiWriterWA
  , withMultiWriterW
  , withMultiWriters
  , withMultiWritersAW
  , withMultiWritersWA
  , withMultiWritersW
  , withMultiState
  , withMultiStateAS
  , withMultiStateSA
  , withMultiStateA
  , withMultiStateS
  , withMultiState_
  , withMultiStates
  , withMultiStatesAS
  , withMultiStatesSA
  , withMultiStatesA
  , withMultiStatesS
  , withMultiStates_
  -- * without-functions (reducing an RWST; inverse of with)
  , withoutMultiReader
  , withoutMultiState
  -- * inflate-functions (run simple transformer in MultiRWST)
  , inflateReader
  , inflateMultiReader
  , inflateWriter
  , inflateMultiWriter
  , inflateState
  , inflateMultiState
  -- * other functions
  , mapMultiRWST
  , mGetRawR
  , mGetRawW
  , mGetRawS
  , mPutRawR
  , mPutRawW
  , mPutRawS
  )
where



import Data.HList.HList
import Data.HList.ContainsType

import Control.Monad.Trans.MultiReader.Class  ( MonadMultiReader(..) )
import Control.Monad.Trans.MultiWriter.Class  ( MonadMultiWriter(..) )
import Control.Monad.Trans.MultiState.Class   ( MonadMultiState(..) )
import Control.Monad.Trans.MultiReader.Strict ( MultiReaderT(..)
                                              , runMultiReaderT )
import Control.Monad.Trans.MultiWriter.Strict ( MultiWriterT(..)
                                              , runMultiWriterT )
import Control.Monad.Trans.MultiState.Strict  ( MultiStateT(..)
                                              , runMultiStateT )

import Control.Monad.State.Strict      ( StateT(..)
                                       , MonadState(..)
                                       , execStateT
                                       , evalStateT
                                       , mapStateT )
import Control.Monad.Reader            ( ReaderT(..) )
import Control.Monad.Writer.Strict     ( WriterT(..) )
import Control.Monad.Trans.Class       ( MonadTrans
                                       , lift )

import Data.Functor.Identity           ( Identity )

import Control.Applicative             ( Applicative(..)
                                       , Alternative(..)
                                       )
import Control.Monad                   ( MonadPlus(..)
                                       , liftM
                                       , ap
                                       , void )
import Control.Monad.Base              ( MonadBase(..)
                                       , liftBaseDefault
                                       )
import Control.Monad.Trans.Control     ( MonadTransControl(..)
                                       , MonadBaseControl(..)
                                       , ComposeSt
                                       , defaultLiftBaseWith
                                       , defaultRestoreM
                                       )
import Control.Monad.Fix               ( MonadFix(..) )
import Control.Monad.IO.Class          ( MonadIO(..) )

import Data.Monoid



newtype MultiRWST r w s m a = MultiRWST {
  runMultiRWSTRaw :: StateT (HList r, HList w, HList s) m a
}

type MultiRWSTNull = MultiRWST '[] '[] '[]

type MultiRWS r w s = MultiRWST r w s Identity

instance (Functor f) => Functor (MultiRWST r w s f) where
  fmap f = MultiRWST . fmap f . runMultiRWSTRaw

instance (Applicative m, Monad m) => Applicative (MultiRWST r w s m) where
  pure = MultiRWST . pure
  (<*>) = ap

instance (Monad m) => Monad (MultiRWST r w s m) where
  return = MultiRWST . return
  k >>= f = MultiRWST $ runMultiRWSTRaw k >>= runMultiRWSTRaw . f

instance MonadTrans (MultiRWST r w s) where
  lift = MultiRWST . lift

#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPPING #-} (Monad m, ContainsType a r)
#else
instance (Monad m, ContainsType a r)
#endif
      => MonadMultiReader a (MultiRWST r w s m) where
  mAsk = MultiRWST $ liftM (\(r,_,_) -> getHListElem r) get

#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPPING #-} (Monad m, ContainsType a w, Monoid a)
#else
instance (Monad m, ContainsType a w, Monoid a)
#endif
      => MonadMultiWriter a (MultiRWST r w s m) where
  mTell v = MultiRWST $ do
    (r,w,s) <- get
    put $ (r, setHListElem (getHListElem w `mappend` v) w, s)

#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPPING #-} (Monad m, ContainsType a s)
#else
instance (Monad m, ContainsType a s)
#endif
      => MonadMultiState a (MultiRWST r w s m) where
  mSet v = MultiRWST $ do
    (r,w,s) <- get
    put (r, w, setHListElem v s)
  mGet = MultiRWST $ do
    (_,_,s) <- get
    return $ getHListElem s

instance MonadFix m => MonadFix (MultiRWST r w s m) where
  mfix f = MultiRWST $ mfix (runMultiRWSTRaw . f)

-- methods

runMultiRWST    :: ( Monad m
                   , Monoid (HList w)
                   )
                => HList r
                -> HList s
                -> MultiRWST r w s m a
                -> m (a, HList s, HList w)
runMultiRWSTASW :: ( Monad m
                   , Monoid (HList w)
                   )
                => HList r
                -> HList s
                -> MultiRWST r w s m a
                -> m (a, HList s, HList w)
runMultiRWSTW   :: ( Monad m
                   , Monoid (HList w)
                   )
                => HList r
                -> HList s
                -> MultiRWST r w s m a
                -> m (HList w)
runMultiRWSTAW  :: ( Monad m
                    , Monoid (HList w)
                    )
                 => HList r
                 -> HList s
                 -> MultiRWST r w s m a
                 -> m (a, HList w)
runMultiRWSTSW  :: ( Monad m
                    , Monoid (HList w)
                    )
                 => HList r
                 -> HList s
                 -> MultiRWST r w s m a
                 -> m (HList s, HList w)

runMultiRWSTNil :: ( Monad m )
                => MultiRWST '[] '[] '[] m a
                -> m a
runMultiRWSTNil_ :: ( Monad m, Functor m )
                 => MultiRWST '[] '[] '[] m a
                 -> m ()
runMultiRWST = runMultiRWSTASW
runMultiRWSTASW r s k = do
  (x, (_, w, s')) <- runStateT (runMultiRWSTRaw k) (r, mempty, s)
  return $ (x, s', w)
runMultiRWSTW r s k = do
  (_, w, _) <- execStateT (runMultiRWSTRaw k) (r, mempty, s)
  return $ w
runMultiRWSTAW r s k = do
  (x, (_, w, _)) <- runStateT (runMultiRWSTRaw k) (r, mempty, s)
  return $ (x, w)
runMultiRWSTSW r s k = do
  (_, w, s') <- execStateT (runMultiRWSTRaw k) (r, mempty, s)
  return $ (s', w)
runMultiRWSTNil  k = evalStateT (runMultiRWSTRaw k) (HNil, HNil, HNil)
runMultiRWSTNil_ k = void $ runStateT (runMultiRWSTRaw k) (HNil, HNil, HNil)

withMultiReader  :: Monad m => r -> MultiRWST (r ': rs) w s m a -> MultiRWST rs w s m a
withMultiReader_ :: (Functor m, Monad m) => r -> MultiRWST (r ': rs) w s m a -> MultiRWST rs w s m ()
withMultiReaders  :: Monad m => HList r1 -> MultiRWST (Append r1 r2) w s m a -> MultiRWST r2 w s m a
withMultiReaders_ :: (Functor m, Monad m) => HList r1 -> MultiRWST (Append r1 r2) w s m a -> MultiRWST r2 w s m ()
withMultiReader x k = MultiRWST $ do
  (r, w, s) <- get
  (a, (_, w', s')) <- lift $ runStateT (runMultiRWSTRaw k) (x :+: r, w, s)
  put (r, w', s')
  return a
withMultiReader_ x k = MultiRWST $ do
  (r, w, s) <- get
  (_, w', s') <- lift $ execStateT (runMultiRWSTRaw k) (x :+: r, w, s)
  put (r, w', s')
withMultiReaders HNil       = id
withMultiReaders (x :+: xs) = withMultiReaders xs . withMultiReader x
withMultiReaders_ HNil       = void
withMultiReaders_ (x :+: xs) = withMultiReaders_ xs . withMultiReader x

withMultiWriter   :: (Monoid w, Monad m) => MultiRWST r (w ': ws) s m a -> MultiRWST r ws s m (a, w)
withMultiWriterAW :: (Monoid w, Monad m) => MultiRWST r (w ': ws) s m a -> MultiRWST r ws s m (a, w)
withMultiWriterWA :: (Monoid w, Monad m) => MultiRWST r (w ': ws) s m a -> MultiRWST r ws s m (w, a)
withMultiWriterW  :: (Monoid w, Monad m) => MultiRWST r (w ': ws) s m a -> MultiRWST r ws s m w
withMultiWriters   :: forall r w1 w2 s m a
                    . (Monoid (HList w1), Monad m, HInit w1)
                   => MultiRWST r (Append w1 w2) s m a
                   -> MultiRWST r w2 s m (a, HList w1)
withMultiWritersAW :: forall r w1 w2 s m a
                    . (Monoid (HList w1), Monad m, HInit w1)
                   => MultiRWST r (Append w1 w2) s m a
                   -> MultiRWST r w2 s m (a, HList w1)
withMultiWritersWA :: forall r w1 w2 s m a
                    . (Monoid (HList w1), Monad m, HInit w1)
                   => MultiRWST r (Append w1 w2) s m a
                   -> MultiRWST r w2 s m (HList w1, a)
withMultiWritersW  :: forall r w1 w2 s m a
                    . (Monoid (HList w1), Monad m, HInit w1)
                   => MultiRWST r (Append w1 w2) s m a
                   -> MultiRWST r w2 s m (HList w1)
withMultiWriter = withMultiWriterAW
withMultiWriterAW k = MultiRWST $ do
  (r, w, s) <- get
  (a, (_, w', s')) <- lift $ runStateT (runMultiRWSTRaw k) (r, mempty :+: w, s)
  case w' of
    x' :+: wr' -> do
      put (r, wr', s')
      return (a, x')
withMultiWriterWA k = MultiRWST $ do
  (r, w, s) <- get
  (a, (_, w', s')) <- lift $ runStateT (runMultiRWSTRaw k) (r, mempty :+: w, s)
  case w' of
    x' :+: wr' -> do
      put (r, wr', s')
      return (x', a)
withMultiWriterW k = MultiRWST $ do
  (r, w, s) <- get
  (_, w', s') <- lift $ execStateT (runMultiRWSTRaw k) (r, mempty :+: w, s)
  case w' of
    x' :+: wr' -> do
      put (r, wr', s')
      return x'
withMultiWriters = withMultiWritersAW
withMultiWritersAW k = MultiRWST $ do
  (r, w, s) <- get
  (a, (_, w', s')) <- lift $ runStateT (runMultiRWSTRaw k) (r, hAppend (mempty :: HList w1) w, s)
  let (o, wr') = hSplit w'
  put (r, wr', s')
  return (a, o)
withMultiWritersWA k = MultiRWST $ do
  (r, w, s) <- get
  (a, (_, w', s')) <- lift $ runStateT (runMultiRWSTRaw k) (r, hAppend (mempty :: HList w1) w, s)
  let (o, wr') = hSplit w'
  put (r, wr', s')
  return (o, a)
withMultiWritersW k = MultiRWST $ do
  (r, w, s) <- get
  (_, w', s') <- lift $ execStateT (runMultiRWSTRaw k) (r, hAppend (mempty :: HList w1) w, s)
  let (o, wr') = hSplit w'
  put (r, wr', s')
  return o

withMultiState   :: Monad m => s -> MultiRWST r w (s ': ss) m a -> MultiRWST r w ss m (a, s)
withMultiStateAS :: Monad m => s -> MultiRWST r w (s ': ss) m a -> MultiRWST r w ss m (a, s)
withMultiStateSA :: Monad m => s -> MultiRWST r w (s ': ss) m a -> MultiRWST r w ss m (s, a)
withMultiStateA  :: Monad m => s -> MultiRWST r w (s ': ss) m a -> MultiRWST r w ss m a
withMultiStateS  :: Monad m => s -> MultiRWST r w (s ': ss) m a -> MultiRWST r w ss m s
withMultiState_  :: (Functor m, Monad m) => s -> MultiRWST r w (s ': ss) m a -> MultiRWST r w ss m ()
withMultiStates   :: Monad m => HList s1 -> MultiRWST r w (Append s1 s2) m a -> MultiRWST r w s2 m (a, HList s1)
withMultiStatesAS :: Monad m => HList s1 -> MultiRWST r w (Append s1 s2) m a -> MultiRWST r w s2 m (a, HList s1)
withMultiStatesSA :: Monad m => HList s1 -> MultiRWST r w (Append s1 s2) m a -> MultiRWST r w s2 m (HList s1, a)
withMultiStatesA  :: Monad m => HList s1 -> MultiRWST r w (Append s1 s2) m a -> MultiRWST r w s2 m a
withMultiStatesS  :: Monad m => HList s1 -> MultiRWST r w (Append s1 s2) m a -> MultiRWST r w s2 m (HList s1)
withMultiStates_  :: (Functor m, Monad m) => HList s1 -> MultiRWST r w (Append s1 s2) m a -> MultiRWST r w s2 m ()
withMultiState = withMultiStateAS
withMultiStateAS x k = MultiRWST $ do
  (r, w, s) <- get
  (a, (_, w', s')) <- lift $ runStateT (runMultiRWSTRaw k) (r, w, (x :+: s))
  case s' of
    x' :+: sr' -> do
      put (r, w', sr')
      return (a, x')
withMultiStateSA x k = MultiRWST $ do
  (r, w, s) <- get
  (a, (_, w', s')) <- lift $ runStateT (runMultiRWSTRaw k) (r, w, (x :+: s))
  case s' of
    x' :+: sr' -> do
      put (r, w', sr')
      return (x', a)
withMultiStateA x k = MultiRWST $ do
  (r, w, s) <- get
  (a, (_, w', s')) <- lift $ runStateT (runMultiRWSTRaw k) (r, w, (x :+: s))
  case s' of
    _ :+: sr' -> do
      put (r, w', sr')
      return a
withMultiStateS x k = MultiRWST $ do
  (r, w, s) <- get
  (_, w', s') <- lift $ execStateT (runMultiRWSTRaw k) (r, w, (x :+: s))
  case s' of
    x' :+: sr' -> do
      put (r, w', sr')
      return x'
withMultiState_ x k = MultiRWST $ do
  (r, w, s) <- get
  (_, w', s') <- lift $ execStateT (runMultiRWSTRaw k) (r, w, (x :+: s))
  case s' of _ :+: sr' -> put (r, w', sr')
withMultiStates                = withMultiStatesAS
withMultiStatesAS HNil       k = do a <- k; return (a, HNil)
withMultiStatesAS (x :+: xs) k = do
  ((a, x'), xs') <- withMultiStates xs $ withMultiState x k
  return (a, x' :+: xs')
withMultiStatesSA HNil       k = do a <- k; return (HNil, a)
withMultiStatesSA (x :+: xs) k = do
  ((a, x'), xs') <- withMultiStates xs $ withMultiState x k
  return (x' :+: xs', a)
withMultiStatesA HNil       = id
withMultiStatesA (x :+: xs) = withMultiStatesA xs . withMultiStateA x
withMultiStatesS HNil       k = k >> return HNil
withMultiStatesS (x :+: xs) k = do
  (x', xs') <- withMultiStates xs $ withMultiStateS x k
  return (x' :+: xs')
withMultiStates_ HNil       = void
withMultiStates_ (x :+: xs) = withMultiStates_ xs . withMultiState_ x

withoutMultiReader :: Monad m => MultiRWST rs w s m a -> MultiRWST (r ': rs) w s m a
withoutMultiReader k = MultiRWST $ do
  (rs@(_ :+: rr), w, s) <- get
  (a, (_, w', s')) <- lift $ runStateT (runMultiRWSTRaw k) (rr, w, s)
  put (rs, w', s')
  return a

withoutMultiState :: Monad m => MultiRWST r w ss m a -> MultiRWST r w (s ': ss) m a
withoutMultiState k = MultiRWST $ do
  (r, w, s :+: sr) <- get
  (a, (_, w', s')) <- lift $ runStateT (runMultiRWSTRaw k) (r, w, sr)
  put (r, w', s :+: s')
  return a

inflateReader :: (Monad m, ContainsType r rs)
              => ReaderT r m a
              -> MultiRWST rs w s m a
inflateReader k = mAsk >>= lift . runReaderT k
inflateMultiReader :: Monad m => MultiReaderT r m a -> MultiRWST r w s m a
inflateMultiReader k = do
  r <- mGetRawR
  lift $ runMultiReaderT r k
inflateWriter :: (Monad m, ContainsType w ws, Monoid w)
              => WriterT w m a
              -> MultiRWST r ws s m a
inflateWriter k = do
  (x, w) <- lift $ runWriterT k
  mTell w
  return x
inflateMultiWriter :: (Functor m, Monad m, Monoid (HList w))
                   => MultiWriterT w m a
                   -> MultiRWST r w s m a
inflateMultiWriter k = do
  (x, w) <- lift $ runMultiWriterT k
  mPutRawW w
  return x
inflateState :: (Monad m, ContainsType s ss)
             => StateT s m a
             -> MultiRWST r w ss m a
inflateState k = do
  s <- mGet
  (x, s') <- lift $ runStateT k s
  mSet s'
  return x
inflateMultiState :: (Functor m, Monad m)
                  => MultiStateT s m a
                  -> MultiRWST r w s m a
inflateMultiState k = do
  s <- mGetRawS
  (x, s') <- lift $ runMultiStateT s k
  mPutRawS s'
  return x

mGetRawR :: Monad m => MultiRWST r w s m (HList r)
mPutRawR :: Monad m => HList r -> MultiRWST r w s m ()
mGetRawW :: Monad m => MultiRWST r w s m (HList w)
mPutRawW :: Monad m => HList w -> MultiRWST r w s m ()
mGetRawS :: Monad m => MultiRWST r w s m (HList s)
mPutRawS :: Monad m => HList s -> MultiRWST r w s m ()
mGetRawR = (\(r, _, _) -> r) `liftM` MultiRWST get
mPutRawR r = MultiRWST $ do
  ~(_, w, s) <- get
  put (r, w, s)
mGetRawW = (\(_, w, _) -> w) `liftM` MultiRWST get
mPutRawW w = MultiRWST $ do
  ~(r, _, s) <- get
  put (r, w, s)
mGetRawS = (\(_, _, s) -> s) `liftM` MultiRWST get
mPutRawS s = MultiRWST $ do
  ~(r, w, _) <- get
  put (r, w, s)

mapMultiRWST :: (ss ~ (HList r, HList w, HList s))
             => (m (a, ss) -> m' (a', ss))
             -> MultiRWST r w s m a
             -> MultiRWST r w s m' a'
mapMultiRWST f = MultiRWST . mapStateT f . runMultiRWSTRaw

-- foreign lifting instances

instance MonadIO m => MonadIO (MultiRWST r w s m) where
  liftIO = lift . liftIO

instance (Functor m, Applicative m, MonadPlus m) => Alternative (MultiRWST r w s m) where
  empty = lift mzero
  MultiRWST m <|> MultiRWST n = MultiRWST $ m <|> n

instance MonadPlus m => MonadPlus (MultiRWST r w s m) where
  mzero = MultiRWST $ mzero
  MultiRWST m `mplus` MultiRWST n = MultiRWST $ m `mplus` n

instance MonadBase b m => MonadBase b (MultiRWST r w s m) where
  liftBase = liftBaseDefault

instance MonadTransControl (MultiRWST r w s) where
  type StT (MultiRWST r w s) a = (a, (HList r, HList w, HList s))
  liftWith f = MultiRWST $ liftWith $ \s -> f $ \r -> s $ runMultiRWSTRaw r
  restoreT = MultiRWST . restoreT

instance MonadBaseControl b m => MonadBaseControl b (MultiRWST r w s m) where
  type StM (MultiRWST r w s m) a = ComposeSt (MultiRWST r w s) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM
