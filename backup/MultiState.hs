{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.MultiState(
  MultiStateT(..),
  MultiState,
  MonadMultiState(..),
  withConfig,
  withConfigs,
  evalMultiStateT
) where

import Control.Monad.State.Strict ( StateT(..), MonadState(..), evalStateT )
import Control.Monad.Trans.Class ( MonadTrans, lift )

import Types.Data.List ( Cons, Null, Append )
import Data.Functor.Identity(Identity)

import Control.Monad(liftM)

data TList a where
  TCons :: x -> TList xs -> TList (Cons x xs)
  TNull :: TList Null

newtype MultiStateT x m a = MultiStateT {
  runMultiStateTRaw :: (StateT (TList x) m a)
}

type MultiState x a = MultiStateT x Identity a

class HasConfig a c where
  setConfigValue :: a -> TList c -> TList c
  getConfigValue :: TList c -> a

class (Monad m) => MonadMultiState a m where
  setConfig :: a -> m ()
  getConfig :: m a

instance HasConfig a (Cons a xs) where
  setConfigValue a (TCons _ xs) = TCons a xs
  getConfigValue (TCons x _) = x

instance (HasConfig a xs) => HasConfig a (Cons x xs) where
  setConfigValue a (TCons x xs) = TCons x $ setConfigValue a xs
  getConfigValue (TCons _ xs) = getConfigValue xs

instance Monad m => Monad (MultiStateT x m) where
  return = MultiStateT . return
  k >>= f = MultiStateT $ (runMultiStateTRaw k)>>=(runMultiStateTRaw.f)

instance MonadTrans (MultiStateT x) where
  lift = MultiStateT . lift

withConfig :: Monad m => x -> MultiStateT (Cons x xs) m a -> MultiStateT xs m a
withConfig x k = MultiStateT $ do
  s <- get
  (a, TCons _ s') <- lift $ runStateT (runMultiStateTRaw k) (TCons x s)
  put s'
  return a

withConfigs :: Monad m => TList xs -> MultiStateT (Append xs ys) m a -> MultiStateT ys m a
withConfigs TNull = id
withConfigs (TCons x xs) = withConfigs xs . withConfig x

instance (Monad m, HasConfig a c) => MonadMultiState a (MultiStateT c m) where
  setConfig v = MultiStateT $ get >>= (put . setConfigValue v)
  getConfig = MultiStateT $ liftM getConfigValue $ get

instance (MonadTrans t, Monad (t m), MonadMultiState a m) => MonadMultiState a (t m) where
  setConfig = lift . setConfig
  getConfig = lift $ getConfig

evalMultiStateT :: Monad m => MultiStateT Null m a -> m a
evalMultiStateT k = evalStateT (runMultiStateTRaw k) TNull

instance (MonadState s m) => MonadState s (MultiStateT c m) where
  put = lift . put
  get = lift $ get
  state = lift . state

{-
toVals2 :: a -> b -> TList (Cons a (Cons b Null))
toVals2 a b = TCons a $ TCons b TNull

toVals3 :: a -> b -> c -> TList (Cons a (Cons b (Cons c Null)))
toVals3 a b c = TCons a $ toVals2 b c

main :: IO ()
main = do
  _ <- runConfList $ do
    --x <- getVal -- Fail
    --lift $ putStrLn $ show (x :: Char) -- Fail
    withVal 'a' $ do
      x <- getVal
      lift $ putStrLn [x] -- Works
      withVals (toVals3 ('c') (2 :: Int) ("Bla" :: String)) $ do
        y <- getVal
        z <- getVal
        a <- getVal
        -- By type we load the right config value
        -- independent of their oder in the config.
        lift $ putStrLn [y :: Char]
        lift $ putStrLn (z :: String)
        lift $ print (a :: Int)
      --y <- getVal
      --lift $ print (y :: Int) -- Will not work, because int it not in environment.
    return ()
  return ()
-}
