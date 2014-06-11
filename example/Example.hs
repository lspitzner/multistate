{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where



import Control.Monad.MultiState

import Control.Applicative ( (<$>), (<*>) )

import Control.Monad.Trans ( lift )

{-
Small example showing
  1) a MultiState containing a Char and a [Char],
  2) the polymorphic mGet,
  3) how to initially put values into the MultiState using withMultiState,
  4) the type inference at work - note that we omitted all type signatures.
-}

--examplePrint :: MultiStateT (Cons [Char] (Cons Char Null)) IO ()
-- or more general:
--examplePrint :: ( MonadMultiState [Char] m
--                , MonadMultiState Char m
--                , m~MultiStateT x IO)
--             => m ()
examplePrint = do
  c  <- mGet
  cs <- mGet
  lift $ putStrLn (c:cs)

exampleAction = do
  examplePrint
  mSet 'J'
  examplePrint

main = evalMultiStateT
     $ withMultiState 'H'
     $ withMultiState "ello, World!"
     $ exampleAction

-- output of main:
--  "Hello, World!
--   Jello, World!
--  "

--whatIsNotPossible :: MultiStateT (Cons [Char] Null) IO ()
--whatIsNotPossible = mGet >>= (lift . print) -- type ambiguous

-- another thing that is not directly possible is the restriction to
-- specific values, i.e. a function
--  restrict :: MultiStateT xvalues m a -> MultiStateT yvalues m a
-- where yvalues is a "superset" of xvalues.

--TODO: example with mGetRaw and withMultiStates
