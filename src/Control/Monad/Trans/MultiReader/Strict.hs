-- | The multi-valued version of mtl's Reader / ReaderT
module Control.Monad.Trans.MultiReader.Strict
  (
  -- * MultiReaderT
    MultiReaderT(..)
  , MultiReaderTNull
  , MultiReader
  -- * functions
  , mAskRaw
  , withMultiReader
  , withMultiReaders
  , evalMultiReaderT
  , evalMultiReaderTWithInitial
  , mapMultiReaderT
) where



import Data.HList.HList
import Data.HList.ContainsType

import Control.Monad.Trans.MultiReader.Class ( MonadMultiReader(..) )

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



-- | A Reader transformer monad patameterized by:
--   
-- * x - The list of types constituting the environment / input (to be read),
-- * m - The inner monad.
-- 
-- 'MultiReaderT' corresponds to mtl's 'ReaderT', but can contain
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
-- > MultiReaderT '[Int, Bool] :: (* -> *) -> (* -> *)
-- 
-- is a Reader transformer containing the types [Int, Bool].
newtype MultiReaderT x m a = MultiReaderT {
  runMultiReaderTRaw :: StateT (HList x) m a
}

-- | A MultiReader transformer carrying an empty state.
type MultiReaderTNull = MultiReaderT '[]

-- | A reader monad parameterized by the list of types x of the environment
-- / input to carry.
--
-- Similar to @Reader r = ReaderT r Identity@
type MultiReader x = MultiReaderT x Identity

instance (Functor f) => Functor (MultiReaderT x f) where
  fmap f = MultiReaderT . fmap f . runMultiReaderTRaw

instance (Applicative m, Monad m) => Applicative (MultiReaderT x m) where
  pure = MultiReaderT . pure
  (<*>) = ap

instance Monad m => Monad (MultiReaderT x m) where
  return = MultiReaderT . return
  k >>= f = MultiReaderT $ runMultiReaderTRaw k >>= (runMultiReaderTRaw . f)

instance MonadTrans (MultiReaderT x) where
  lift = MultiReaderT . lift

-- | Adds an element to the environment, thereby transforming a MultiReaderT
-- carrying an environment with types /(x:xs)/ to a a MultiReaderT with /xs/.
--
-- Think "Execute this computation with this additional value as environment".
withMultiReader :: Monad m
                => x
                -> MultiReaderT (x ': xs) m a
                -> MultiReaderT xs m a
withMultiReader x k = MultiReaderT $ do
  s <- get
  (a, _  :+: s') <- lift $ runStateT (runMultiReaderTRaw k) (x :+: s)
  put s'
  return a

-- | Adds a heterogenous list of elements to the environment, thereby
-- transforming a MultiReaderT carrying an environment with values
-- over types /xs++ys/ to a MultiReaderT over /ys/.
--
-- Similar to recursively adding single values with 'withMultiReader'.
--
-- Note that /ys/ can be Null; in that case the return value can be
-- evaluated further using 'evalMultiReaderT'.
withMultiReaders :: Monad m
                 => HList xs
                 -> MultiReaderT (Append xs ys) m a
                 -> MultiReaderT ys m a
withMultiReaders HNil = id
withMultiReaders (x :+: xs) = withMultiReaders xs . withMultiReader x

instance (Monad m, ContainsType a c)
      => MonadMultiReader a (MultiReaderT c m) where
  mAsk = MultiReaderT $ liftM getHListElem get

-- | A raw extractor of the contained HList (i.e. the complete environment).
--
-- For a possible usecase, see 'withMultiReaders'.
mAskRaw :: Monad m => MultiReaderT a m (HList a)
mAskRaw = MultiReaderT get

-- | Evaluate a computation over an empty environment.
--
-- Because the environment is empty, it does not need to be provided.
--
-- If you want to evaluate a computation over any non-Null environment, either
-- use
-- 
-- * 'evalMultiReaderTWithInitial'
-- * simplify the computation using 'withMultiReader' / 'withMultiReaders',
--   then use 'evalMultiReaderT' on the result.
evalMultiReaderT :: Monad m => MultiReaderT '[] m a -> m a
evalMultiReaderT k = evalStateT (runMultiReaderTRaw k) HNil

-- | Evaluate a reader computation with the given environment.
evalMultiReaderTWithInitial :: Monad m
                            => HList a            -- ^ The initial state
                            -> MultiReaderT a m b -- ^ The computation to evaluate
                            -> m b
evalMultiReaderTWithInitial c k = evalStateT (runMultiReaderTRaw k) c

-- | Map both the return value and the environment of a computation
-- using the given function.
--
-- Note that there is a difference to mtl's ReaderT,
-- where it is /not/ possible to modify the environment.
mapMultiReaderT :: (m (a, HList w) -> m' (a', HList w))
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




