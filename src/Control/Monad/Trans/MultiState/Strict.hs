-- | The multi-valued version of mtl's State / StateT
module Control.Monad.Trans.MultiState.Strict
  (
  -- * MultiStateT
    MultiStateT(..)
  , MultiStateTNull
  , MultiState
  -- * MonadMultiState class
  , MonadMultiState(..)
  -- * run-functions
  , runMultiStateT
  , runMultiStateTAS
  , runMultiStateTSA
  , runMultiStateTA
  , runMultiStateTS
  , runMultiStateT_
  , runMultiStateTNil
  , runMultiStateTNil_
  -- * with-functions (single state)
  , withMultiState
  , withMultiStateAS
  , withMultiStateSA
  , withMultiStateA
  , withMultiStateS
  , withMultiState_
  -- * with-functions (multiple states)
  , withMultiStates
  , withMultiStatesAS
  , withMultiStatesSA
  , withMultiStatesA
  , withMultiStatesS
  , withMultiStates_
  -- * inflate-functions (run single state in multiple states)
  , inflateState
  , inflateReader
  , inflateWriter
  -- * other functions
  , mapMultiStateT
  , mGetRaw
  , mPutRaw
) where



import Data.HList.HList
import Data.HList.ContainsType

import Control.Monad.Trans.MultiState.Class

import Control.Monad.State.Strict      ( StateT(..)
                                       , MonadState(..)
                                       , evalStateT
                                       , execStateT
                                       , mapStateT )
import Control.Monad.Reader            ( ReaderT(..) )
import Control.Monad.Writer.Strict     ( WriterT(..) )
import Control.Monad.Trans.Class       ( MonadTrans
                                       , lift )
import Control.Monad.Writer.Class      ( MonadWriter
                                       , listen
                                       , tell
                                       , writer
                                       , pass )

import Data.Functor.Identity           ( Identity )

import Control.Applicative             ( Applicative(..) )
import Control.Monad                   ( liftM
                                       , ap
                                       , void )
import Data.Monoid                     ( Monoid )
import Control.Monad.Fix               ( MonadFix(..) )
import Control.Monad.IO.Class          ( MonadIO(..) )



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

instance (Monad m, ContainsType a c)
      => MonadMultiState a (MultiStateT c m) where
  mSet v = MultiStateT $ get >>= put . setHListElem v
  mGet = MultiStateT $ liftM getHListElem get

instance MonadFix m => MonadFix (MultiStateT s m) where
  mfix f = MultiStateT $ mfix (runMultiStateTRaw . f)

-- methods

-- | A raw extractor of the contained HList (i.e. the complete state).
mGetRaw :: Monad m => MultiStateT a m (HList a)
mGetRaw = MultiStateT get

mPutRaw :: Monad m => HList s -> MultiStateT s m ()
mPutRaw = MultiStateT . put

-- | Map both the return value and the state of a computation
-- using the given function.
mapMultiStateT :: (m (a, HList w) -> m' (a', HList w))
               -> MultiStateT w m  a
               -> MultiStateT w m' a'
mapMultiStateT f = MultiStateT . mapStateT f . runMultiStateTRaw

runMultiStateT   :: Functor m => HList s -> MultiStateT s m a -> m (a, HList s)
runMultiStateTAS :: Functor m => HList s -> MultiStateT s m a -> m (a, HList s)
runMultiStateTSA :: Monad m   => HList s -> MultiStateT s m a -> m (HList s, a)
runMultiStateTA  :: Monad m   => HList s -> MultiStateT s m a -> m a
runMultiStateTS  :: Monad m   => HList s -> MultiStateT s m a -> m (HList s)
runMultiStateT_  :: Functor m => HList s -> MultiStateT s m a -> m ()
-- ghc too dumb for this shortcut, unfortunately
-- runMultiStateT   s k = runMultiStateTNil $ withMultiStates s k
-- runMultiStateTAS s k = runMultiStateTNil $ withMultiStatesAS s k
-- runMultiStateTSA s k = runMultiStateTNil $ withMultiStatesSA s k
-- runMultiStateTA  s k = runMultiStateTNil $ withMultiStatesA s k
-- runMultiStateTS  s k = runMultiStateTNil $ withMultiStatesS s k
-- runMultiStateT_  s k = runMultiStateTNil $ withMultiStates_ s k
runMultiStateT   s k = runMultiStateTAS s k
runMultiStateTAS s k = runStateT (runMultiStateTRaw k) s
runMultiStateTSA s k = (\(a,b) -> (b,a)) `liftM` runStateT (runMultiStateTRaw k) s
runMultiStateTA  s k = evalStateT (runMultiStateTRaw k) s
runMultiStateTS  s k = execStateT (runMultiStateTRaw k) s
runMultiStateT_  s k = void $ runStateT (runMultiStateTRaw k) s

runMultiStateTNil  ::   Monad m => MultiStateT '[] m a -> m a
runMultiStateTNil_ :: Functor m => MultiStateT '[] m a -> m ()
runMultiStateTNil  k = evalStateT (runMultiStateTRaw k) HNil
runMultiStateTNil_ k = void $ runStateT (runMultiStateTRaw k) HNil

withMultiState   :: Monad m => s -> MultiStateT (s ': ss) m a -> MultiStateT ss m (a, s)
withMultiStateAS :: Monad m => s -> MultiStateT (s ': ss) m a -> MultiStateT ss m (a, s)
withMultiStateSA :: Monad m => s -> MultiStateT (s ': ss) m a -> MultiStateT ss m (s, a)
withMultiStateA  :: Monad m => s -> MultiStateT (s ': ss) m a -> MultiStateT ss m a
withMultiStateS  :: Monad m => s -> MultiStateT (s ': ss) m a -> MultiStateT ss m s
withMultiState_  :: (Functor m, Monad m) => s -> MultiStateT (s ': ss) m a -> MultiStateT ss m ()
withMultiState = withMultiStateAS
withMultiStateAS x k = MultiStateT $ do
  s <- get
  ~(a, s') <- lift $ runStateT (runMultiStateTRaw k) (x :+: s)
  case s' of x' :+: sr' -> do put sr'; return (a, x')
withMultiStateSA s k = (\(a,b) -> (b,a)) `liftM` withMultiStateAS s k
withMultiStateA  s k = fst `liftM` withMultiStateAS s k
withMultiStateS  s k = snd `liftM` withMultiStateAS s k
withMultiState_  s k = void $ withMultiStateAS s k

withMultiStates   :: Monad m => HList s1 -> MultiStateT (Append s1 s2) m a -> MultiStateT s2 m (a, HList s1)
withMultiStatesAS :: Monad m => HList s1 -> MultiStateT (Append s1 s2) m a -> MultiStateT s2 m (a, HList s1)
withMultiStatesSA :: Monad m => HList s1 -> MultiStateT (Append s1 s2) m a -> MultiStateT s2 m (HList s1, a)
withMultiStatesA  :: Monad m => HList s1 -> MultiStateT (Append s1 s2) m a -> MultiStateT s2 m a
withMultiStatesS  :: Monad m => HList s1 -> MultiStateT (Append s1 s2) m a -> MultiStateT s2 m (HList s1)
withMultiStates_  :: (Functor m, Monad m) => HList s1 -> MultiStateT (Append s1 s2) m a -> MultiStateT s2 m ()
withMultiStates = withMultiStatesAS
withMultiStatesAS HNil       = liftM (\r -> (r, HNil))
withMultiStatesAS (x :+: xs) = liftM (\((a, x'), xs') -> (a, x' :+: xs'))
                        . withMultiStatesAS xs 
                        . withMultiStateAS x
withMultiStatesSA HNil       = liftM (\r -> (HNil, r))
withMultiStatesSA (x :+: xs) = liftM (\((a, x'), xs') -> (x' :+: xs', a))
                        . withMultiStatesAS xs 
                        . withMultiStateAS x
withMultiStatesA  HNil       = id
withMultiStatesA  (x :+: xs) = withMultiStatesA xs . withMultiStateA x
withMultiStatesS  HNil       = liftM (const HNil)
withMultiStatesS (x :+: xs)  = liftM (\(x', xs') -> x' :+: xs')
                        . withMultiStatesAS xs
                        . withMultiStateS x
withMultiStates_  HNil       = liftM (const ())
withMultiStates_ (x :+: xs)  = withMultiStates_ xs . withMultiState_ x

inflateState :: (Monad m, ContainsType s ss)
             => StateT s m a
             -> MultiStateT ss m a
inflateState k = do
  s <- mGet
  (x, s') <- lift $ runStateT k s
  mSet s'
  return x

inflateReader :: (Monad m, ContainsType r ss)
              => ReaderT r m a
              -> MultiStateT ss m a
inflateReader k = mGet >>= lift . runReaderT k

inflateWriter :: (Monad m, ContainsType w ss, Monoid w)
              => WriterT w m a
              -> MultiStateT ss m a
inflateWriter k = do
  (x, w) <- lift $ runWriterT k
  mSet w
  return x

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

instance MonadIO m => MonadIO (MultiStateT c m) where
  liftIO = lift . liftIO
