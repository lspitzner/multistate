{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.MultiReader(
  MultiReaderT(..),
  MultiReader,
  MonadMultiReader(..),
  withConfig,
  withConfigs,
  evalMultiReaderT
) where

import Control.Monad.State.Strict ( StateT(..), MonadState(..), evalStateT )
import Control.Monad.Trans.Class ( MonadTrans, lift )

import Types.Data.List ( Cons, Null, Append )
import Data.Functor.Identity(Identity)

import Control.Monad(liftM)

data TList a where
  TCons :: x -> TList xs -> TList (Cons x xs)
  TNull :: TList Null

newtype MultiReaderT x m a = MultiReaderT {
  runMultiReaderTRaw :: (StateT (TList x) m a)
}

type MultiReader x a = MultiReaderT x Identity a

class HasConfig a c where
  setConfigValue :: a -> TList c -> TList c
  getConfigValue :: TList c -> a

class (Monad m) => MonadMultiReader a m where
  getConfig :: m a

instance HasConfig a (Cons a xs) where
  setConfigValue a (TCons _ xs) = TCons a xs
  getConfigValue (TCons x _) = x

instance (HasConfig a xs) => HasConfig a (Cons x xs) where
  setConfigValue a (TCons x xs) = TCons x $ setConfigValue a xs
  getConfigValue (TCons _ xs) = getConfigValue xs

instance Monad m => Monad (MultiReaderT x m) where
  return = MultiReaderT . return
  k >>= f = MultiReaderT $ (runMultiReaderTRaw k)>>=(runMultiReaderTRaw.f)

instance MonadTrans (MultiReaderT x) where
  lift = MultiReaderT . lift

withConfig :: Monad m => x -> MultiReaderT (Cons x xs) m a -> MultiReaderT xs m a
withConfig x k = MultiReaderT $ do
  s <- get
  (a, TCons _ s') <- lift $ runStateT (runMultiReaderTRaw k) (TCons x s)
  put s'
  return a

withConfigs :: Monad m => TList xs -> MultiReaderT (Append xs ys) m a -> MultiReaderT ys m a
withConfigs TNull = id
withConfigs (TCons x xs) = withConfigs xs . withConfig x

instance (Monad m, HasConfig a c) => MonadMultiReader a (MultiReaderT c m) where
  getConfig = MultiReaderT $ liftM getConfigValue $ get

instance (MonadTrans t, Monad (t m), MonadMultiReader a m) => MonadMultiReader a (t m) where
  getConfig = lift $ getConfig

evalMultiReaderT :: Monad m => MultiReaderT Null m a -> m a
evalMultiReaderT k = evalStateT (runMultiReaderTRaw k) TNull

instance (MonadState s m) => MonadState s (MultiReaderT c m) where
  put = lift . put
  get = lift $ get
  state = lift . state
