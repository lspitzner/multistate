{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Alternative multi-valued version of mtl's RWS / RWST. In contrast to
-- @'MultiRWS'(T)@ this version only takes a single list of types as
-- parameter, but with additional encoding of the allowed access for each
-- element. This supports the @'MonadMultiGet'@ notion more succinctly, i.e.
-- to pass a "state" element to a function that only requires/expects read/get
-- access. This is not possible with 'MultiRWS'.
module Control.Monad.Trans.MultiGST.Lazy
  ( MultiGSTT(..)
  , MultiGSTTNull
  , MultiGST
  -- * MonadMulti classes
  , ContainsReader
  , ContainsState
  , ContainsWriter
  , MonadMultiReader(..)
  , MonadMultiWriter(..)
  , MonadMultiGet(..)
  , MonadMultiState(..)
  , CanReadWrite(..)
  -- * run-functions
  , runMultiGSTTNil
  , runMultiGSTTNil_
  -- * with-functions
  , withReader
  , withReader_
  , withReaders
  , withWriter
  , withWriterAW
  , withWriterWA
  , withWriterW
  , withState
  , withStateAS
  , withStateSA
  , withStateA
  , withStateS
  , withState_
  -- * without-functions
  , without
  -- * other functions
  , mGetRaw
  , mSetRaw
  , mGetRawR
  , mapMultiGSTT
  )
where



import Control.Monad.State.Lazy        ( StateT(..)
                                       , MonadState(..)
                                       , execStateT
                                       , evalStateT
                                       , mapStateT )

import Data.Functor.Identity           ( Identity )

import Control.Monad.Trans.Class       ( MonadTrans
                                       , lift
                                       )
import Control.Monad                   ( MonadPlus(..)
                                       , liftM
                                       , ap
                                       , void
                                       )
import Control.Applicative             ( Applicative(..)
                                       , Alternative(..)
                                       )
import Control.Monad.Fix               ( MonadFix(..) )
import Control.Monad.IO.Class          ( MonadIO(..) )

import Data.Monoid                     ( Monoid
                                       , (<>)
                                       )


import GHC.Exts (Constraint)

import Control.Monad.Trans.MultiReader.Class
import Control.Monad.Trans.MultiWriter.Class
import Control.Monad.Trans.MultiState.Class

import qualified Data.HList.HList as HList

import Control.Monad.Trans.MultiGST.Common



newtype MultiGSTT ts m a = MultiGSTT {
  runMultiGSTTRaw :: StateT (HListM ts) m a
}
  deriving(Functor, Applicative, Monad, MonadTrans, MonadIO, Alternative, MonadPlus)

type MultiGSTTNull = MultiGSTT '[]

type MultiGST r = MultiGSTT r Identity


instance
#if MIN_VERSION_base(4,8,0)
  {-# OVERLAPPING #-}
#endif
    (Monad m, HListMContains 'GettableFlag a cts)
      => MonadMultiGet a (MultiGSTT cts m) where
  mGet = MultiGSTT $ liftM (\ts -> readHListMElem @'GettableFlag ts) get

instance
#if MIN_VERSION_base(4,8,0)
  {-# OVERLAPPING #-}
#endif
    (Monad m, HListMContains 'SettableFlag a cts)
      => MonadMultiState a (MultiGSTT cts m) where
  mSet x = MultiGSTT $ do
    ts <- get
    put $ writeHListMElem @'SettableFlag x ts

instance
#if MIN_VERSION_base(4,8,0)
  {-# OVERLAPPING #-}
#endif
    ( Monad m
    , Monoid a
    , HListMContains 'TellableFlag a cts
    ) => MonadMultiWriter a (MultiGSTT cts m) where
  mTell x = MultiGSTT $ do
    ts <- get
    let x' = readHListMElem @'TellableFlag ts
    put $ writeHListMElem @'TellableFlag (x' <> x) ts

runMultiGSTTNil :: Monad m => MultiGSTT '[] m a -> m a
runMultiGSTTNil_ :: Monad m => MultiGSTT '[] m a -> m ()

runMultiGSTTNil k = evalStateT (runMultiGSTTRaw k) (HNilM)
runMultiGSTTNil_ k = liftM (const ()) (evalStateT (runMultiGSTTRaw k) (HNilM))

withReader :: Monad m => t -> MultiGSTT ('Gettable t ': tr) m a -> MultiGSTT tr m a
withReader x k = MultiGSTT $ do
  tr <- get
  ~(a, ts') <- lift $ runStateT (runMultiGSTTRaw k) (x :+-: tr)
  put $ case ts' of _ :+-: tr' -> tr'
  return a

withReader_ :: Monad m => t -> MultiGSTT ('Gettable t ': tr) m a -> MultiGSTT tr m ()
withReader_ x k = MultiGSTT $ do
  tr <- get
  ~(_, ts') <- lift $ runStateT (runMultiGSTTRaw k) (x :+-: tr)
  put $ case ts' of _ :+-: tr' -> tr'

withReaders  :: Monad m => HList.HList rs -> MultiGSTT (AppendM (HListMReaders rs) ts) m a -> MultiGSTT ts m a
withReaders HList.HNil       = id
withReaders (t HList.:+: ts) = withReaders ts . withReader t

withWriter   :: (Monoid t, Monad m) => MultiGSTT ('Tellable t ': tr) m a -> MultiGSTT tr m (a, t)
withWriterAW :: (Monoid t, Monad m) => MultiGSTT ('Tellable t ': tr) m a -> MultiGSTT tr m (a, t)
withWriterWA :: (Monoid t, Monad m) => MultiGSTT ('Tellable t ': tr) m a -> MultiGSTT tr m (t, a)
withWriterW  :: (Monoid t, Monad m) => MultiGSTT ('Tellable t ': tr) m a -> MultiGSTT tr m t
withWriter = withWriterAW
withWriterAW k = MultiGSTT $ do
  tr <- get
  ~(a, ts') <- lift $ runStateT (runMultiGSTTRaw k) (mempty :-+: tr)
  case ts' of
    t :-+: tr' -> do
      put tr'
      return (a, t)
withWriterWA k = MultiGSTT $ do
  tr <- get
  ~(a, ts') <- lift $ runStateT (runMultiGSTTRaw k) (mempty :-+: tr)
  case ts' of
    t :-+: tr' -> do
      put tr'
      return (t, a)
withWriterW k = MultiGSTT $ do
  tr <- get
  ~(_, ts') <- lift $ runStateT (runMultiGSTTRaw k) (mempty :-+: tr)
  case ts' of
    t :-+: tr' -> do
      put tr'
      return t

withState   :: Monad m => t -> MultiGSTT ('Settable t ': tr) m a -> MultiGSTT tr m (a, t)
withStateAS :: Monad m => t -> MultiGSTT ('Settable t ': tr) m a -> MultiGSTT tr m (a, t)
withStateSA :: Monad m => t -> MultiGSTT ('Settable t ': tr) m a -> MultiGSTT tr m (t, a)
withStateA  :: Monad m => t -> MultiGSTT ('Settable t ': tr) m a -> MultiGSTT tr m a
withStateS  :: Monad m => t -> MultiGSTT ('Settable t ': tr) m a -> MultiGSTT tr m t
withState_  :: Monad m => t -> MultiGSTT ('Settable t ': tr) m a -> MultiGSTT tr m ()
withState = withStateAS
withStateAS t k = MultiGSTT $ do
  tr <- get
  ~(a, ts') <- lift $ runStateT (runMultiGSTTRaw k) (t :++: tr)
  case ts' of
    t' :++: tr' -> do
      put tr'
      return (a, t')
withStateSA t k = MultiGSTT $ do
  tr <- get
  ~(a, ts') <- lift $ runStateT (runMultiGSTTRaw k) (t :++: tr)
  case ts' of
    t' :++: tr' -> do
      put tr'
      return (t', a)
withStateA t k = MultiGSTT $ do
  tr <- get
  ~(a, ts') <- lift $ runStateT (runMultiGSTTRaw k) (t :++: tr)
  case ts' of
    _ :++: tr' -> do
      put tr'
      return a
withStateS t k = MultiGSTT $ do
  tr <- get
  ~(_, ts') <- lift $ runStateT (runMultiGSTTRaw k) (t :++: tr)
  case ts' of
    t' :++: tr' -> do
      put tr'
      return t'
withState_ t k = MultiGSTT $ do
  tr <- get
  ~(_, ts') <- lift $ runStateT (runMultiGSTTRaw k) (t :++: tr)
  case ts' of
    _ :++: tr' -> do
      put tr'

without :: Monad m => MultiGSTT tr m a -> MultiGSTT (ct ': tr) m a
without k = MultiGSTT $ do
  ts <- get
  case ts of
    (t :+-: tr) -> do
      ~(a, tr') <- lift $ runStateT (runMultiGSTTRaw k) tr
      put (t :+-: tr')
      return a
    (t :-+: tr) -> do
      ~(a, tr') <- lift $ runStateT (runMultiGSTTRaw k) tr
      put (t :-+: tr')
      return a
    (t :++: tr) -> do
      ~(a, tr') <- lift $ runStateT (runMultiGSTTRaw k) tr
      put (t :++: tr')
      return a

mGetRaw :: Monad m => MultiGSTT ts m (HListM ts)
mGetRaw = MultiGSTT get

mGetRawR :: (Monad m, HListMGettableClass ts) => MultiGSTT ts m (HList.HList (HListMGettableOnly ts))
mGetRawR = MultiGSTT $ liftM hListMGettableOnly get

mSetRaw :: Monad m => HListM ts -> MultiGSTT ts m ()
mSetRaw = MultiGSTT . put

mapMultiGSTT
  :: (ts ~ HListM cts)
  => (m (a, ts) -> m' (a', ts))
  -> MultiGSTT cts m a
  -> MultiGSTT cts m' a'
mapMultiGSTT f = MultiGSTT . mapStateT f . runMultiGSTTRaw

