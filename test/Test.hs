{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where



import Data.Functor.Identity
import Data.HList.HList
import Data.Monoid
import Data.Semigroup

import qualified Control.Monad.Trans.MultiState as MS
import qualified Control.Monad.Trans.MultiReader as MR
import qualified Control.Monad.Trans.MultiWriter as MW

import Control.Applicative ( Applicative, (<$>), (<*>) )

import Test.Hspec



type Tests = [(Bool, String)]

runEvalMS :: MS.MultiStateT '[] Identity a -> a
runEvalMS = runIdentity . MS.runMultiStateTNil
runEvalMR :: MR.MultiReaderT '[] Identity a -> a
runEvalMR = runIdentity . MR.runMultiReaderTNil
runExecMW :: Monoid (HList x) => MW.MultiWriterT x Identity a -> HList x
runExecMW = runIdentity . MW.runMultiWriterTW

runnerMS :: a -> MS.MultiStateT '[a] Identity a -> a
runnerMS x m = runEvalMS $ MS.withMultiStateA x m
runnerMR :: a -> MR.MultiReaderT '[a] Identity a -> a
runnerMR x m = runEvalMR $ MR.withMultiReader x m
runnerMW :: (Semigroup a, Monoid a) => MW.MultiWriterT '[a] Identity b -> a
runnerMW m = case runExecMW m of (x :+: _) -> x
-- TODO: ghc bug?: warning on:
-- runnerMW m = case runExecMW m of (x :+: HNil) -> x

runnerMS_ :: a -> MS.MultiStateT '[a] Identity b -> a
runnerMS_ x m = runIdentity
              $ MS.runMultiStateTNil
              $ MS.withMultiStateA x (m >> MS.mGet)
runnerMR_ :: a -> MR.MultiReaderT '[a] Identity b -> a
runnerMR_ x m = runIdentity
              $ MR.runMultiReaderTNil
              $ MR.withMultiReader x (m >> MR.mAsk)

intRunnerMS :: Int -> MS.MultiStateT '[Int] Identity Int -> Int
intRunnerMS = runnerMS
intRunnerMS_ :: Int -> MS.MultiStateT '[Int] Identity b -> Int
intRunnerMS_ = runnerMS_
intRunnerMR :: Int -> MR.MultiReaderT '[Int] Identity Int -> Int
intRunnerMR = runnerMR
intRunnerMR_ :: Int -> MR.MultiReaderT '[Int] Identity b -> Int
intRunnerMR_ = runnerMR_
stringRunnerMW :: MW.MultiWriterT '[String] Identity b -> String
stringRunnerMW = runnerMW

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

testsMultiState :: Spec
testsMultiState = do
  it "identity" $ 1 `shouldBe` runIdentity (Identity (1::Int))
  it "getConfig"
    $ intRunnerMS_ 2 (return ())
    `shouldBe` 2
  it "setConfig"
    $ intRunnerMS_ 100 (MS.mSet (3::Int))
    `shouldBe` 3
  it "setConfig"
    $ intRunnerMS_ 4 (MS.mGet >>= \x -> MS.mSet (x::Int))
    `shouldBe` 4
  it "nesting 1"
    $ intRunnerMS (4::Int) (MS.withMultiStateA (5::Int) MS.mGet)
    `shouldBe` 5
  it "nesting 2"
    $ intRunnerMS (4::Int) (   MS.mSet (100::Int)
                                >> MS.withMultiStateA (6::Int) MS.mGet)
    `shouldBe` 6
  it "nesting 3"
    $ intRunnerMS (4::Int) (MS.withMultiStateA (100::Int)
                                        $ MS.mSet (7::Int) >> MS.mGet)
    `shouldBe` 7
  it "multiple types 1"
    $ ( runEvalMS
      $ MS.withMultiStateA True
      $ MS.withMultiStateA 'a'
      $ msGetTuple )
    `shouldBe` (True, 'a')
  it "multiple types 2"
    $ ( runEvalMS
      $ MS.withMultiStateA True
      $ MS.withMultiStateA 'a'
      $ MS.withMultiStateA 'b'
      $ msGetTuple )
    `shouldBe` (True, 'b')
  it "askRaw" test13MS

testsMultiReader :: Spec
testsMultiReader = do
  it "identity"
    $ runIdentity (Identity (1::Int))
    `shouldBe` 1
  it "getConfig"
    $ intRunnerMR_ 2 (return ())
    `shouldBe` 2
  it "nesting"
    $ intRunnerMR (4::Int) (MR.withMultiReader (5::Int) MR.mAsk)
    `shouldBe` 5
  it "multiple types 1"
    $ ( runEvalMR
      $ MR.withMultiReader True
      $ MR.withMultiReader 'a'
      $ mrAskTuple )
    `shouldBe` (True, 'a')
  it "multiple types 2"
    $ ( runEvalMR
      $ MR.withMultiReader True
      $ MR.withMultiReader 'a'
      $ MR.withMultiReader 'b'
      $ mrAskTuple )
    `shouldBe` (True, 'b')
  it "multiple types 3"
    $ ( runEvalMR
      $ MR.withMultiReader True
      $ MR.withMultiReader 'a'
      $ MR.withMultiReader False
      $ mrAskTuple )
    `shouldBe` (False, 'a')
  it "getRaw" test13MR

testsMultiWriter :: Spec
testsMultiWriter = do
  it "1-0"
    $ stringRunnerMW (return ())
    `shouldBe` ""
  it "1-1"
    $ stringRunnerMW (MW.mTell "a")
    `shouldBe` "a"
  it "1-2"
    $ stringRunnerMW (MW.mTell "a" >> MW.mTell "b")
    `shouldBe` "ab"
  it "2"
    $ runExecMW (MW.mTell "a" >> MW.mTell [True] >> MW.mTell "b")
    `shouldBe` ("ab" :+: [True] :+: HNil)

tests :: Spec
tests = do
  describe "MultiState" $ testsMultiState
  describe "MultiReader" $ testsMultiReader
  describe "MultiWriter" $ testsMultiWriter
  lazyStateTest

test13MR :: Bool
test13MR = runIdentity
         $ MR.runMultiReaderTNil
         $ MR.withMultiReader True
         $ MR.withMultiReader 'a'
         $ do
  c <- MR.mGetRaw
  return $ runIdentity
         $ MR.runMultiReaderTNil
         $ MR.withMultiReaders c
         $ do
    b <- MR.mAsk
    return (b::Bool)

test13MS :: Bool
test13MS = runIdentity
         $ MS.runMultiStateTNil
         $ MS.withMultiStateA True
         $ MS.withMultiStateA 'a'
         $ do
  c <- MS.mGetRaw
  return $ runIdentity
         $ MS.runMultiStateTNil
         $ MS.withMultiStatesA c
         $ do
    b <- MS.mGet
    return (b::Bool)

lazyStateTest :: Spec
lazyStateTest = it "lazyStateTest" $ (33, True) `shouldBe` l
  where
    l :: (Int, Bool)
    l = case runIdentity $ MS.runMultiStateTS ([] :+: [] :+: HNil) action of
      (x :+: y :+: _) -> (head x, head y)
#if !MIN_VERSION_base(4,9,0)
      _ -> error "some ghc versions think that above is not exhaustive."
#endif
    action :: MS.MultiStateT '[[Int], [Bool]] Identity ()
    action = do
      action
      x <- MS.mGet
      MS.mSet $ (33::Int):x
      y <- MS.mGet
      MS.mSet $ True:y


main :: IO ()
main = hspec $ tests
  -- mapM_ (putStrLn . ("error: "++) . snd) $ filter (\(b, _) -> not b) tests
  -- putStrLn $    "ran "
  --            ++ show (length tests)
  --            ++ " tests (no further output = good)"
  -- return ()

{-

main = do
  evalStateT
    (runMultiReaderT $ withConfig 'a' $ do
        x <- withConfig 'b' getConfig
        lift $ lift $ print (x::Char)
        y <- get
        lift $ lift $ print (y::Int)
        return ()
    )
    (1::Int)
  runMultiReaderT $ withConfig 'a' $ evalStateT
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