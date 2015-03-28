-- | The multi-valued version of mtl's State / StateT
-- / MonadState
module Control.Monad.MultiState {-# DEPRECATED "Use Control.Monad.Trans.MultiState instead" #-}
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
  -- * other functions
  , mapMultiStateT
  , mGetRaw
  , mPutRaw
) where



-- just re-export
import Control.Monad.Trans.MultiState.Class
import Control.Monad.Trans.MultiState.Lazy
