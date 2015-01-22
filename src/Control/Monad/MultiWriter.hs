{-# LANGUAGE FlexibleContexts #-}

-- | The multi-valued version of mtl's Writer / WriterT
-- / MonadWriter
module Control.Monad.MultiWriter
  ( -- * MultiWriterT
    MultiWriterT(..)
  , MultiWriterTNull
  , MultiWriter
  -- * MonadMultiWriter class
  , MonadMultiWriter(..)
  -- * functions
  , mGetRaw
  , withMultiWriter
  , withMultiWriters
  , runMultiWriterT
  , execMultiWriterT
  , mapMultiWriterT
) where

import Data.HList.HList

import Control.Monad.State.Strict ( StateT(..)
                                  , MonadState(..)
                                  , execStateT
                                  , mapStateT )
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
                                  , ap )

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

class ContainsType a c where
  setHListElem :: a -> HList c -> HList c
  getHListElem :: HList c -> a

class (Monad m, Monoid a) => MonadMultiWriter a m where
  mTell :: a -> m ()  

instance ContainsType a (a ': xs) where
  setHListElem a (_ :+: xs) = a :+: xs
  getHListElem (x :+: _) = x

instance (ContainsType a xs) => ContainsType a (x ': xs) where
  setHListElem a (x :+: xs) = x :+: setHListElem a xs
  getHListElem (_ :+: xs) = getHListElem xs

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

withMultiWriter :: Monad m
               => x
               -> MultiWriterT (x ': xs) m a
               -> MultiWriterT xs m a
withMultiWriter x k = MultiWriterT $ do
  s <- get
  (a, _ :+: s') <- lift $ runStateT (runMultiWriterTRaw k) (x :+: s)
  put s'
  return a
withMultiWriters
 :: Monad m
                => HList xs
                -> MultiWriterT (Append xs ys) m a
                -> MultiWriterT ys m a
withMultiWriters HNil = id
withMultiWriters (x :+: xs) = withMultiWriters xs . withMultiWriter x

instance (Monad m, ContainsType a c, Monoid a)
      => MonadMultiWriter a (MultiWriterT c m) where
  mTell v = MultiWriterT $ do
    x <- get
    put $ setHListElem (getHListElem x `mappend` v) x

instance (MonadTrans t, Monad (t m), MonadMultiWriter a m)
      => MonadMultiWriter a (t m) where
  mTell = lift . mTell

runMultiWriterT :: (Monad m, Monoid (HList l))
                => MultiWriterT l m a -> m (a, HList l)
runMultiWriterT k = runStateT (runMultiWriterTRaw k) mempty

execMultiWriterT :: (Monad m, Monoid (HList l))
                 => MultiWriterT l m a -> m (HList l)
execMultiWriterT k = execStateT (runMultiWriterTRaw k) mempty

mGetRaw :: Monad m => MultiWriterT a m (HList a)
mGetRaw = MultiWriterT get

mapMultiWriterT :: (m (a, HList w) -> m' (a', HList w))
               -> MultiWriterT w m  a
               -> MultiWriterT w m' a'
mapMultiWriterT f = MultiWriterT . mapStateT f . runMultiWriterTRaw

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
