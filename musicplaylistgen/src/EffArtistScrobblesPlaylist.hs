{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module EffArtistScrobblesPlaylist where

import API.LastAPIHandler
import API.YTAPIHandler
import API.APITypes
import API.SpotifyAPIHandler
import API.MiscIO
import Mocking.LastMock
import Mocking.MiscMock
import GUI.WebForm


import           Data.Functor.Identity
import           Data.Maybe
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans hiding (liftIO)
import           Control.Monad (join)
import           Data.Function ((&))
import           Control.Concurrent.MVar

import           Control.Effects.Eff
import           Control.Effects.IO
import           Control.Effects.State

main :: IO ()
main = do
  sth <- testH
  putStrLn sth


runProgram :: (Member EffAPI r, Member EffPlaylist r, Member EffIO r) =>
  Eff r String
runProgram = do
  (artist,limit) <- getArtLim
  playlist <- requestapi artist limit
  genPlaylist artist playlist


data EffIO a 
  = ReqArtLim ((Artist, Limit) -> a) 
  deriving (Functor, Typeable) 

getArtLim :: (Member EffIO r) => Eff r (Artist, Limit)
getArtLim = effect $ \k -> inj $ ReqArtLim k

cmdHandler :: (Member LiftIO r) => Handler EffIO r a a
cmdHandler (Value a) = return a
cmdHandler (Comp (ReqArtLim k)) = do
    x <- finish $ liftIO getArtLimIO
    k x

webHandler :: (Member LiftIO r) => Handler EffIO r a a
webHandler (Value a) = return a
webHandler (Comp (ReqArtLim k)) = do
    x <- finish $ liftIO getArtLimWeb
    k x

noIOHandler :: (String, String) -> Handler EffIO r a a 
noIOHandler _ (Value a) = return a
noIOHandler (a,l) (Comp (ReqArtLim k)) = do
    x <- finish $ return $ runIdentity $ getArtLimMock a l
    k x


data EffAPI a
  = ReqAPI Artist Limit (SongList -> a)
  deriving (Functor, Typeable)

requestapi :: (Member EffAPI r) => Artist -> Limit -> Eff r SongList
requestapi a l = effect $ \k -> inj $ ReqAPI a l k

-- definition IO handler for API 1

-- to implement parallel: want to use handler inside handler
-- alternative method: create entirely separate handler

multirequestHandler :: (Member LiftIO r) => Handler EffAPI r a a
multirequestHandler (Value a) = return a
multirequestHandler (Comp (ReqAPI a l k)) = do
  x <- finish $ liftIO $ runMaybeT $ multFMArtistList (splitArtists a) l
  k $ fromJust x

requestHandler :: (Member LiftIO r) => Handler EffAPI r a a
requestHandler (Value a) = return a
requestHandler (Comp (ReqAPI a l k)) = do
  x <- finish $ liftIO $ runMaybeT $ lastFmApiTopRequest a l
  k (fromJust x)

mockHandler :: Handler EffAPI r a a
mockHandler (Value a) = return a
mockHandler (Comp (ReqAPI a l k)) = do
  x <- finish $  return $ runIdentity $ requestMockS a l
  k x

      --main = putStr (runIdentity (artistRequest getArtistMock getLimitMock requestMockS playlistToString))
  --main = fmap (const ()) (runMaybeT (artistRequest (lift getArtistIO) (lift getLimitIO) lastFmApiTopRequest (\a b -> lift (ytURLGen a b))))

data EffPlaylist a
  = EffPlaylist Artist SongList (String -> a)
  deriving (Functor, Typeable)

genPlaylist :: (Member EffPlaylist r) => Artist -> SongList -> Eff r String
genPlaylist a sl = effect $ \k -> inj $ EffPlaylist a sl k

ytPlayHandler :: (Member LiftIO r) => Handler EffPlaylist r a a
ytPlayHandler (Value a) = return a
ytPlayHandler (Comp (EffPlaylist a sl k)) = do
  x <- finish $ liftIO $ ytURLGen a sl
  k x 

spPlayHandler :: (Member LiftIO r) => Handler EffPlaylist r a a
spPlayHandler (Value a) = return a
spPlayHandler (Comp (EffPlaylist a sl k)) = do
  x <- finish $ liftIO $ spURLGen a sl
  k x 

mockPlayHandler :: Handler EffPlaylist r a a
mockPlayHandler (Value a) = return a
mockPlayHandler (Comp (EffPlaylist a sl k)) = do
  x <- finish $ return $ runIdentity $ playlistToString a sl
  k x

testH :: IO String
testH = runProgram
  & handle cmdHandler
  & handle requestHandler
  & handle ytPlayHandler
  & handle ioHandler
  & runPure

testH2 :: IO String
testH2 = runProgram
  & handle webHandler
  & handle requestHandler
  & handle spPlayHandler
  & handle ioHandler
  & runPure

testMock :: (String, String) -> String
testMock al = runProgram
  & handle (noIOHandler al)
  & handle mockHandler
  & handle mockPlayHandler
  & runPure