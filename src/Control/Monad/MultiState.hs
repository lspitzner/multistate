{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.MultiState
  ( MultiStateT(..)
  , MultiStateTNull
  , MultiState
  , MonadMultiState(..)
  , mGetRaw
  , withMultiState
  , withMultiStates
  , evalMultiStateT
  , evalMultiStateTWithInitial
  , mapMultiStateT
  , Cons -- re-export that stuff to allow writing type signatures.
  , Null
) where



import Data.HList.HList

import Control.Monad.State.Strict ( StateT(..)
                                  , MonadState(..)
                                  , evalStateT
                                  , mapStateT )
import Control.Monad.Trans.Class  ( MonadTrans
                                  , lift )
import Control.Monad.Writer.Class ( MonadWriter
                                  , listen
                                  , tell
                                  , writer
                                  , pass )

import Types.Data.List            ( Cons
                                  , Null
                                  , Append )
import Data.Functor.Identity      ( Identity )

import Control.Applicative        ( Applicative(..) )
import Control.Monad              ( liftM
                                  , ap )



newtype MultiStateT x m a = MultiStateT {
  runMultiStateTRaw :: StateT (HList x) m a
}

type MultiStateTNull = MultiStateT Null

type MultiState x a = MultiStateT x Identity a

class ContainsType a c where
  setHListElem :: a -> HList c -> HList c
  getHListElem :: HList c -> a

class (Monad m) => MonadMultiState a m where
  mSet :: a -> m ()
  mGet :: m a

instance ContainsType a (Cons a xs) where
  setHListElem a (TCons _ xs) = TCons a xs
  getHListElem (TCons x _) = x

instance (ContainsType a xs) => ContainsType a (Cons x xs) where
  setHListElem a (TCons x xs) = TCons x $ setHListElem a xs
  getHListElem (TCons _ xs) = getHListElem xs

instance (Functor f) => Functor (MultiStateT x f) where
  fmap f = MultiStateT . fmap f . runMultiStateTRaw

instance (Applicative m, Monad m) => Applicative (MultiStateT x m) where
  pure = MultiStateT . pure
  (<*>) = ap

instance Monad m => Monad (MultiStateT x m) where
  return = MultiStateT . return
  k >>= f = MultiStateT $ runMultiStateTRaw k >>= (runMultiStateTRaw.f)

instance MonadTrans (MultiStateT x) where
  lift = MultiStateT . lift

withMultiState :: Monad m
               => x
               -> MultiStateT (Cons x xs) m a
               -> MultiStateT xs m a
withMultiState x k = MultiStateT $ do
  s <- get
  (a, TCons _ s') <- lift $ runStateT (runMultiStateTRaw k) (TCons x s)
  put s'
  return a

withMultiStates :: Monad m
                => HList xs
                -> MultiStateT (Append xs ys) m a
                -> MultiStateT ys m a
withMultiStates TNull = id
withMultiStates (TCons x xs) = withMultiStates xs . withMultiState x

instance (Monad m, ContainsType a c)
      => MonadMultiState a (MultiStateT c m) where
  mSet v = MultiStateT $ get >>= (put . setHListElem v)
  mGet = MultiStateT $ liftM getHListElem get

instance (MonadTrans t, Monad (t m), MonadMultiState a m)
      => MonadMultiState a (t m) where
  mSet = lift . mSet
  mGet = lift $ mGet

evalMultiStateT :: Monad m => MultiStateT Null m a -> m a
evalMultiStateT k = evalStateT (runMultiStateTRaw k) TNull

evalMultiStateTWithInitial :: Monad m
                           => HList a
                           -> MultiStateT a m b
                           -> m b
evalMultiStateTWithInitial c k = evalStateT (runMultiStateTRaw k) c

mGetRaw :: Monad m => MultiStateT a m (HList a)
mGetRaw = MultiStateT get

mapMultiStateT :: (m (a, HList w) -> m' (a', HList w))
               -> MultiStateT w m  a
               -> MultiStateT w m' a'
mapMultiStateT f = MultiStateT . mapStateT f . runMultiStateTRaw

-- foreign lifting instances

instance (MonadState s m) => MonadState s (MultiStateT c m) where
  put   = lift . put
  get   = lift $ get
  state = lift . state

instance (MonadWriter w m) => MonadWriter w (MultiStateT c m) where
  writer = lift . writer
  tell   = lift . tell
  listen = MultiStateT .
    mapStateT (liftM (\((a,w), w') -> ((a, w'), w)) . listen) .
    runMultiStateTRaw
  pass = MultiStateT .
    mapStateT (pass . liftM (\((a, f), w) -> ((a, w), f))) .
    runMultiStateTRaw  
