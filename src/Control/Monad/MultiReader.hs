{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.MultiReader
  ( MultiReaderT(..)
  , MultiReaderTNull
  , MultiReader
  , MonadMultiReader(..)
  , mAskRaw
  , withMultiReader
  , withMultiReaders
  , evalMultiReaderT
  , evalMultiReaderTWithInitial
  , mapMultiReaderT
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



newtype MultiReaderT x m a = MultiReaderT {
  runMultiReaderTRaw :: StateT (HList x) m a
}

type MultiReaderTNull = MultiReaderT Null

type MultiReader x a = MultiReaderT x Identity a

class ContainsType a c where
  setHListElem :: a -> HList c -> HList c
  getHListElem :: HList c -> a

class (Monad m) => MonadMultiReader a m where
  mAsk :: m a

{-
it might make seem straightforward to define the following class that
corresponds to other transformer classes. But while we can define the the
class and its instances, there is a problem we try to use it, assuming that we
do not want to annotate the full type signature of the config:
  the type of the config can not be inferred properly. we would need a feature
  like "infer, as return type for this function, the only type for
  which there exists a valid chain of instance definitions that is needed to
  by this function".
  In other words, it is impossible to use the mAskRaw function without
  binding a concrete type for c, because otherwise the inference runs into
  some overlapping instances.
For this reason, I removed this type class and created a non-class function
mAskRaw, for which the type inference works because it involves no
type classes.
  lennart spitzner
-}

--class (Monad m) => MonadMultiReaderRaw c m where
--  mAskRaw :: m (HList c)

--instance (MonadTrans t, Monad (t m), MonadMultiReaderRaw c m)
--      => MonadMultiReaderRaw c (t m) where
--  mAskRaw = lift $ mAskRaw

--instance (Monad m) => MonadMultiReaderRaw a (MultiReaderT a m) where
--  mAskRaw = MultiReaderT $ get

instance ContainsType a (Cons a xs) where
  setHListElem a (TCons _ xs) = TCons a xs
  getHListElem (TCons x _) = x

instance (ContainsType a xs) => ContainsType a (Cons x xs) where
  setHListElem a (TCons x xs) = TCons x $ setHListElem a xs
  getHListElem (TCons _ xs) = getHListElem xs

instance (Functor f) => Functor (MultiReaderT x f) where
  fmap f = MultiReaderT . fmap f . runMultiReaderTRaw

instance (Applicative m, Monad m) => Applicative (MultiReaderT x m) where
  pure = MultiReaderT . pure
  (<*>) = ap

instance Monad m => Monad (MultiReaderT x m) where
  return = MultiReaderT . return
  k >>= f = MultiReaderT $ runMultiReaderTRaw k >>= (runMultiReaderTRaw.f)

instance MonadTrans (MultiReaderT x) where
  lift = MultiReaderT . lift

withMultiReader :: Monad m
                => x
                -> MultiReaderT (Cons x xs) m a
                -> MultiReaderT xs m a
withMultiReader x k = MultiReaderT $ do
  s <- get
  (a, TCons _ s') <- lift $ runStateT (runMultiReaderTRaw k) (TCons x s)
  put s'
  return a

withMultiReaders :: Monad m
                 => HList xs
                 -> MultiReaderT (Append xs ys) m a
                 -> MultiReaderT ys m a
withMultiReaders TNull = id
withMultiReaders (TCons x xs) = withMultiReaders xs . withMultiReader x

instance (Monad m, ContainsType a c)
      => MonadMultiReader a (MultiReaderT c m) where
  mAsk = MultiReaderT $ liftM getHListElem get

instance (MonadTrans t, Monad (t m), MonadMultiReader a m)
      => MonadMultiReader a (t m) where
  mAsk = lift $ mAsk

mAskRaw :: Monad m => MultiReaderT a m (HList a)
mAskRaw = MultiReaderT get

evalMultiReaderT :: Monad m => MultiReaderT Null m a -> m a
evalMultiReaderT k = evalStateT (runMultiReaderTRaw k) TNull

evalMultiReaderTWithInitial :: Monad m
                            => HList a
                            -> MultiReaderT a m b
                            -> m b
evalMultiReaderTWithInitial c k = evalStateT (runMultiReaderTRaw k) c

mapMultiReaderT :: (m (a, HList w)
                -> m' (a', HList w))
                -> MultiReaderT w m a
                -> MultiReaderT w m' a'
mapMultiReaderT f = MultiReaderT . mapStateT f . runMultiReaderTRaw

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
