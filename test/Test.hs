{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where



import Data.Functor.Identity
import qualified Control.Monad.MultiState as MS
import qualified Control.Monad.MultiReader as MR

import Control.Applicative ( Applicative, (<$>), (<*>) )



type Tests = [(Bool, String)]

runEvalMS :: MS.MultiStateT '[] Identity a -> a
runEvalMS = runIdentity . MS.evalMultiStateT
runEvalMR :: MR.MultiReaderT '[] Identity a -> a
runEvalMR = runIdentity . MR.evalMultiReaderT

runnerMS :: a -> MS.MultiStateT '[a] Identity a -> a
runnerMS x m = runIdentity $ MS.evalMultiStateT $ MS.withMultiState x m
runnerMR :: a -> MR.MultiReaderT '[a] Identity a -> a
runnerMR x m = runIdentity $ MR.evalMultiReaderT $ MR.withMultiReader x m

runnerMS_ :: a -> MS.MultiStateT '[a] Identity b -> a
runnerMS_ x m = runIdentity
              $ MS.evalMultiStateT
              $ MS.withMultiState x (m >> MS.mGet)
runnerMR_ :: a -> MR.MultiReaderT '[a] Identity b -> a
runnerMR_ x m = runIdentity
              $ MR.evalMultiReaderT
              $ MR.withMultiReader x (m >> MR.mAsk)

intRunnerMS :: Int -> MS.MultiStateT '[Int] Identity Int -> Int
intRunnerMS = runnerMS
intRunnerMS_ :: Int -> MS.MultiStateT '[Int] Identity b -> Int
intRunnerMS_ = runnerMS_
intRunnerMR :: Int -> MR.MultiReaderT '[Int] Identity Int -> Int
intRunnerMR = runnerMR
intRunnerMR_ :: Int -> MR.MultiReaderT '[Int] Identity b -> Int
intRunnerMR_ = runnerMR_

mrAskTuple :: ( Applicative m
              , MR.MonadMultiReader a m
              , MR.MonadMultiReader b m)
           => m (a,b)
mrAskTuple = (,) <$> MR.mAsk <*> MR.mAsk
msGetTuple :: ( Applicative m
              , MS.MonadMultiState a m
              , MS.MonadMultiState b m)
           => m (a,b)
msGetTuple = (,) <$> MS.mGet <*> MS.mGet

testsMultiState :: Tests
testsMultiState =
  [
    (1 == runIdentity (Identity (1::Int))
    , "identity"),
    (2 == intRunnerMS_ 2 (return ())
    , "multistate getConfig"),
    (3 == intRunnerMS_ 100 (MS.mSet (3::Int))
    , "multistate setConfig"),
    (4 == intRunnerMS_ 4 (MS.mGet >>= \x -> MS.mSet (x::Int))
    , "multistate setConfig"),
    (5 == intRunnerMS (4::Int) (MS.withMultiState (5::Int) MS.mGet)
    , "multistate nesting"),
    (6 == intRunnerMS (4::Int) (   MS.mSet (100::Int)
                                >> MS.withMultiState (6::Int) MS.mGet)
    , "multistate nesting"),
    (7 == intRunnerMS (4::Int) (   MS.withMultiState (100::Int)
                                     $ MS.mSet (7::Int)
                                >> MS.mGet)
    , "multistate nesting"),
    ((True, 'a') == ( runEvalMS
                    $ MS.withMultiState True
                    $ MS.withMultiState 'a'
                    $ msGetTuple )
    , "multistate multiple types"),
    ((True, 'b') == ( runEvalMS
                    $ MS.withMultiState True
                    $ MS.withMultiState 'a'
                    $ MS.withMultiState 'b'
                    $ msGetTuple )
    , "multistate multiple types"),
    ((False, 'a') == ( runEvalMS
                     $ MS.withMultiState True
                     $ MS.withMultiState 'a'
                     $ MS.withMultiState False
                     $ msGetTuple )
    , "multistate multiple types"),
    (test13MS
    , "askRaw")
  ]

testsMultiReader :: Tests
testsMultiReader =
  [
    (1 == runIdentity (Identity (1::Int))
    , "identity"),
    (2 == intRunnerMR_ 2 (return ())
    , "multistate getConfig"),
    (5 == intRunnerMR (4::Int) (MR.withMultiReader (5::Int) MR.mAsk)
    , "multistate nesting"),
    ((True, 'a') == ( runEvalMR
                    $ MR.withMultiReader True
                    $ MR.withMultiReader 'a'
                    $ mrAskTuple )
    , "multistate multiple types"),
    ((True, 'b') == ( runEvalMR
                    $ MR.withMultiReader True
                    $ MR.withMultiReader 'a'
                    $ MR.withMultiReader 'b'
                    $ mrAskTuple )
    , "multistate multiple types"),
    ((False, 'a') == ( runEvalMR
                     $ MR.withMultiReader True
                     $ MR.withMultiReader 'a'
                     $ MR.withMultiReader False
                     $ mrAskTuple )
    , "multistate multiple types"),
    (test13MR
    , "getRaw")
  ]

tests :: Tests
tests = testsMultiState ++ testsMultiReader

test13MR :: Bool
test13MR = runIdentity
         $ MR.evalMultiReaderT
         $ MR.withMultiReader True
         $ MR.withMultiReader 'a'
         $ do
  c <- MR.mAskRaw
  return $ runIdentity
         $ MR.evalMultiReaderT
         $ MR.withMultiReaders c
         $ do
    b <- MR.mAsk
    return (b::Bool)

test13MS :: Bool
test13MS = runIdentity
         $ MS.evalMultiStateT
         $ MS.withMultiState True
         $ MS.withMultiState 'a'
         $ do
  c <- MS.mGetRaw
  return $ runIdentity
         $ MS.evalMultiStateT
         $ MS.withMultiStates c
         $ do
    b <- MS.mGet
    return (b::Bool)

main :: IO ()
main = do
  mapM_ (putStrLn . ("error: "++) . snd) $ filter (\(b, _) -> not b) tests
  putStrLn $    "ran "
             ++ show (length tests)
             ++ " tests (no further output = good)"
  return ()

{-

main = do
  evalStateT
    (evalMultiReaderT $ withConfig 'a' $ do
        x <- withConfig 'b' getConfig
        lift $ lift $ print (x::Char)
        y <- get
        lift $ lift $ print (y::Int)
        return ()
    )
    (1::Int)
  evalMultiReaderT $ withConfig 'a' $ evalStateT
    ( do
        x <- getConfig
        lift $ lift $ print (x::Char)
        y <- get
        lift $ lift $ print (y::Int)
        return ()
    )
    (1::Int)

main = do
  evalStateT
    (evalMultiStateT $ withConfig 'a' $ do
        x <- withConfig 'b' getConfig
        lift $ lift $ print (x::Char)
        y <- get
        lift $ lift $ print (y::Int)
        return ()
    )
    (1::Int)
  evalMultiStateT $ withConfig 'a' $ evalStateT
    ( do
        x <- getConfig
        lift $ lift $ print (x::Char)
        y <- get
        lift $ lift $ print (y::Int)
        return ()
    )
    (1::Int)

-}