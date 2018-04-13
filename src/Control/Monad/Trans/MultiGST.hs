-- | The multi-valued version of mtl's RWS / RWST
module Control.Monad.Trans.MultiGST
  (
  -- * MultiRWST
    MultiGSTT(..)
  , MultiGSTTNull
  , MultiGST
  -- * MonadMulti classes
  , ContainsReader
  , ContainsState
  , ContainsWriter
  , MonadMultiReader(..)
  , MonadMultiWriter(..)
  , MonadMultiGet(..)
  , CanReadWrite(..)
  -- * run-functions (extracting from RWST)
  , runMultiGSTTNil
  , runMultiGSTTNil_
  -- * with-functions (extending an RWST)
  , withReader
  , withReader_
  , withReaders
  , withWriter
  , withWriterAW
  , withWriterWA
  , withWriterW
  , withState
  , withStateAS
  , withStateSA
  , withStateA
  , withStateS
  , withState_
  -- * without-functions (reducing an RWST; inverse of with)
  , without
  -- * other functions
  , mapMultiGSTT
  , mGetRawR
  , mSetRaw
  , mGetRaw
) where



-- just re-export
import Control.Monad.Trans.MultiGST.Lazy
