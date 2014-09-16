{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The multi-valued version of mtl's State / StateT
-- / MonadState
module Control.Monad.MultiState
  (
  -- * MultiStateT
    MultiStateT(..)
  , MultiStateTNull
  , MultiState
  -- * MonadMultiState class
  , MonadMultiState(..)
  -- * functions
  , mGetRaw
  , withMultiState
  , withMultiStates
  , evalMultiStateT
  , evalMultiStateTWithInitial
  , mapMultiStateT
  -- * re-exports
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



-- | A State transformer monad patameterized by:
--   
-- * x - The list of types constituting the state,
-- * m - The inner monad.
-- 
-- 'MultiStateT' corresponds to mtl's 'StateT', but can contain
-- a heterogenous list of types.
-- 
-- This heterogenous list is represented using Types.Data.List, i.e:
-- 
--   * @'Null'@ - The empty list,
--   * @'Cons' a b@ - A list where @/a/@ is an arbitrary type
--     and @/b/@ is the rest list.
-- 
-- For example,
-- 
-- > MultiStateT (Cons Int (Cons Bool Null)) :: (* -> *) -> (* -> *)
-- 
-- is a State wrapper containing the types [Int,Bool].
newtype MultiStateT x m a = MultiStateT {
  runMultiStateTRaw :: StateT (HList x) m a
}

-- | A MultiState transformer carrying an empty state.
type MultiStateTNull = MultiStateT Null

-- | A state monad parameterized by the list of types x of the state to carry.
--
-- Similar to @State s = StateT s Identity@
type MultiState x = MultiStateT x Identity

class ContainsType a c where
  setHListElem :: a -> HList c -> HList c
  getHListElem :: HList c -> a

-- | All methods must be defined.
--
-- The idea is: Any monad stack is instance of @MonadMultiState a@, iff
-- the stack contains a @MultiStateT x@ with /a/ element of /x/.
class (Monad m) => MonadMultiState a m where
  -- | state set function for values of type @a@.
  mSet :: a -> m ()
  -- | state get function for values of type @a@.
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

-- | Adds an element to the state, thereby transforming a MultiStateT over
-- values with types /(x:xs)/ to a MultiStateT over /xs/.
--
-- Think "Execute this computation with this additional value as state".
withMultiState :: Monad m
               => x                           -- ^ The value to add
               -> MultiStateT (Cons x xs) m a -- ^ The computation using the
                                              -- enlarged state
               -> MultiStateT xs m a          -- ^ An computation using the
                                              -- smaller state
withMultiState x k = MultiStateT $ do
  s <- get
  (a, TCons _ s') <- lift $ runStateT (runMultiStateTRaw k) (TCons x s)
  put s'
  return a

-- | Adds a heterogenous list of elements to the state, thereby
-- transforming a MultiStateT over values with types /xs++ys/ to a MultiStateT
-- over /ys/.
--
-- Similar to recursively adding single values with 'withMultiState'.
--
-- Note that /ys/ can be Null; in that case the return value can be
-- evaluated further using 'evalMultiStateT'.
withMultiStates :: Monad m
                => HList xs                       -- ^ The list of values to add
                -> MultiStateT (Append xs ys) m a -- ^ The computation using the
                                                  --   enlarged state
                -> MultiStateT ys m a             -- ^ A computation using the
                                                  -- smaller state
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

-- | Evaluate an empty state computation.
--
-- Because the state is empty, no initial state must be provided.
--
-- Currently it is not directly possible to extract the final state of a
-- computation (similar to @execStateT@ and @runStateT@ for mtl's StateT),
-- but you can use 'mGetRaw' if you need such functionality.
--
-- If you want to evaluate a computation over any non-Null state, either
-- use
-- 
-- * 'evalMultiStateTWithInitial'
-- * simplify the computation using 'withMultiState' / 'withMultiStates',
--   then use 'evalMultiStateT' on the result.
evalMultiStateT :: Monad m => MultiStateT Null m a -> m a
evalMultiStateT k = evalStateT (runMultiStateTRaw k) TNull

-- | Evaluate a state computation with the given initial state.
evalMultiStateTWithInitial :: Monad m
                           => HList a           -- ^ The initial state
                           -> MultiStateT a m b -- ^ The computation to evaluate
                           -> m b
evalMultiStateTWithInitial c k = evalStateT (runMultiStateTRaw k) c

-- | A raw extractor of the contained HList (i.e. the complete state).
--
-- For a possible usecase, see 'withMultiStates'.
mGetRaw :: Monad m => MultiStateT a m (HList a)
mGetRaw = MultiStateT get

-- | Map both the return value and the state of a computation
-- using the given function.
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
