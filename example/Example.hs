{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where



import Control.Monad.MultiState

import Control.Applicative ( (<$>), (<*>) )

import Control.Monad.Trans ( lift )
import Control.Monad.Writer

{-
Small example showing
  1) a MultiState containing a Char and a String,
  2) the polymorphic mGet,
  3) how to initially put values into the MultiState using withMultiState,
  4) the type inference at work - note that there was no need to annotate
     combinedPrint
-}

simpleExample :: IO ()
simpleExample = evalMultiStateT
              $ withMultiState 'H'              -- add a Char to the state
              $ withMultiState "ello, World!" -- add a String to the state
              $ do
  -- the monad here is MultiStateT '[String, Char] IO
  let combinedPrint = do
        c  <- mGet
        cs <- mGet
        -- i <- mGet -- No instance for (Control.Monad.MultiState.ContainsType Int '[])
        -- lift $ print $ (i :: Int)
        lift $ putStrLn (c:cs)
  combinedPrint
  mSet 'J' -- we set the Char in the state to 'J'
  combinedPrint

-- output:
--  "Hello, World!
--   Jello, World!
--  "

-- and a more complex example:

newtype Account = Account Float
newtype Interest = Interest Float

setAccount :: MonadMultiState Account m => Float -> m ()
setAccount x = mSet (Account x)
getAccount :: MonadMultiState Account m => m Float
getAccount = do
  (Account x) <- mGet
  return x
modAccount :: MonadMultiState Account m => (Float -> Float) -> m ()
modAccount f = do
  (Account x) <- mGet
  mSet (Account (f x))

-- wait for a specific time, changing the account according to interest
wait :: ( MonadMultiState Account m
        , MonadMultiState Interest m )
     => Float
     -> m ()
wait t = do
  (Interest i) <- mGet
  (Account x) <- mGet
  mSet (Account (x*(1+i)**t))

logAccount :: ( MonadWriter [String] m
              , MonadMultiState Account m)
           => m ()
logAccount = do
  (Account x) <- mGet
  tell $ ["account balance = " ++ show x]

accountCalculation :: Writer [String] ()
accountCalculation = evalMultiStateT $ do
  tell ["account calculation start"]
  -- we cannot use any of the account methods here, because state is empty
  -- logAccount
  --   -->
  --   No instance for (Control.Monad.MultiState.ContainsType Account '[])
  withMultiState (Account 0.0) $ do -- state contains an Account.
    logAccount
    modAccount (+10.0)
    logAccount
    -- trying to use "wait" here would give type error, like above.
    withMultiState (Interest 0.03) $ do -- state now also contains Interest.
      wait 10.0 -- we can use wait, because state contains all
                -- necessary stuff.
      logAccount
      modAccount (\x -> x - 10.0)
      wait 10.0
      logAccount
      mSet (Interest 0.00)
      wait 10.0
    -- we can return back to the environment without interest
    -- but the changes to the account are still present
    logAccount
  -- and we can return to an empty state
  tell ["account calculation end"]

main = do
  simpleExample
  mapM_ putStrLn $ execWriter accountCalculation


-- whatIsNotPossible :: MultiStateT '[String] IO ()
-- whatIsNotPossible = mGet >>= (lift . print) -- type ambiguous

-- another thing that is not directly possible is the restriction to
-- specific values, i.e. a function
--  restrict :: MultiStateT xvalues m a -> MultiStateT yvalues m a
-- where yvalues is a "superset" of xvalues.

--TODO: example with mGetRaw and withMultiStates
