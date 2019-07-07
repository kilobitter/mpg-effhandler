{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TestEff where

import Control.Monad (join)
import Data.Function ((&))

import Control.Effects.Eff
import Control.Effects.IO
import Control.Effects.State

main :: IO ()
main = do
  putStrLn "hello world"

-- effect-handlers

-- definition API 1 (e.g. MusicAPI)

data Eff1 a
  = Eff1 String (Int -> a)
  deriving (Functor, Typeable)

eff1 :: (Member Eff1 r) => String -> Eff r Int
eff1 str = effect $ \k -> inj $ Eff1 str k

-- definition IO handler for API 1

eff1Handler :: (Member LiftIO r) => Handler Eff1 r a a
eff1Handler (Value a) = return a
eff1Handler (Comp (Eff1 str k)) = do
  finish $ liftIO $ print str
  x <- finish $ liftIO $ readLn
  k x

-- definition API 2 (e.g. PlaylistAPI)

data Eff2 a
  = Eff2 String (Int -> a)
  deriving (Functor, Typeable)

eff2 :: (Member Eff2 r) => String -> Eff r Int
eff2 str = effect $ \k -> inj $ Eff2 str k

-- definition IO handler for API 2

eff2Handler :: (Member LiftIO r) => Handler Eff2 r a a
eff2Handler (Value a) = return a
eff2Handler (Comp (Eff2 str k)) = do
  finish $ liftIO $ print str
  x <- finish $ liftIO $ readLn
  k x

-- definition state handler for API 2

eff2StHandler :: (Member (State Int) r) => Handler Eff2 r a a
eff2StHandler (Value a) = return a
eff2StHandler (Comp (Eff2 _ k)) = do
  x <- finish get
  k x

-- test computation combining both apis

test :: (Member Eff1 r, Member Eff2 r) =>
  Eff r Int
test = do
  x <- eff1 "a"
  y <- eff2 "b"
  return $ x + y

-- run mixing state and IO handler

testH :: (Member LiftIO r, Member (State Int) r) => Eff r Int
testH = test
  & handle eff1Handler
  & handle eff2StHandler

testH2 :: (Member LiftIO r) => Eff r (Int -> Eff r Int)
testH2 = testH
  & handle stateHandler

testH3 :: IO Int
testH3 = testH2
  & handle ioHandler
  & runPure
  & fmap (\x -> (x 5) & handle ioHandler & runPure)
  & join

-- run using both IO handlers

testH4 :: IO Int
testH4 = test
  & handle eff1Handler
  & handle eff2Handler
  & handle ioHandler
  & runPure

