-- | The multi-valued version of mtl's State / StateT
module Control.Monad.Trans.MultiState.Strict
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
) where



import Data.HList.HList
import Data.HList.ContainsType

import Control.Monad.Trans.MultiState.Class

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
--   * @'[]@ - The empty list,
--   * @a ': b@ - A list where @/a/@ is an arbitrary type
--     and @/b/@ is the rest list.
-- 
-- For example,
-- 
-- > MultiStateT '[Int, Bool] :: (* -> *) -> (* -> *)
-- 
-- is a State wrapper containing the types [Int, Bool].
newtype MultiStateT x m a = MultiStateT {
  runMultiStateTRaw :: StateT (HList x) m a
}

-- | A MultiState transformer carrying an empty state.
type MultiStateTNull = MultiStateT '[]

-- | A state monad parameterized by the list of types x of the state to carry.
--
-- Similar to @State s = StateT s Identity@
type MultiState x = MultiStateT x Identity

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
               -> MultiStateT (x ': xs) m a -- ^ The computation using the
                                              -- enlarged state
               -> MultiStateT xs m a          -- ^ An computation using the
                                              -- smaller state
withMultiState x k = MultiStateT $ do
  s <- get
  (a, _ :+: s') <- lift $ runStateT (runMultiStateTRaw k) (x :+: s)
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
withMultiStates HNil = id
withMultiStates (x :+: xs) = withMultiStates xs . withMultiState x

instance (Monad m, ContainsType a c)
      => MonadMultiState a (MultiStateT c m) where
  mSet v = MultiStateT $ get >>= put . setHListElem v
  mGet = MultiStateT $ liftM getHListElem get

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
evalMultiStateT :: Monad m => MultiStateT '[] m a -> m a
evalMultiStateT k = evalStateT (runMultiStateTRaw k) HNil

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
