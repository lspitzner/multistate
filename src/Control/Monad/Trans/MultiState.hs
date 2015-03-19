-- | The multi-valued version of mtl's State / StateT
-- / MonadState
module Control.Monad.Trans.MultiState
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



-- just re-export
import Control.Monad.Trans.MultiState.Class
import Control.Monad.Trans.MultiState.Lazy
