{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module EffArtistScrobblesPlaylist where

import API.LastAPIHandler
import API.YTAPIHandler
import API.APITypes
import API.MiscIO
import Mocking.LastMock
import Mocking.MiscMock

import           Data.Functor.Identity
import           Data.Maybe
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans hiding (liftIO)
import           Control.Monad (join)
import           Data.Function ((&))

import           Control.Effects.Eff
import           Control.Effects.IO
import           Control.Effects.State



artistRequest :: (Monad f) => f Artist -> f Limit -> (Artist -> Limit -> f SongList) -> (Artist -> SongList -> f a) -> f a
artistRequest getArtist getLimit getPopSongs printPL = do
    artist <- getArtist
    limit <- getLimit
    result <- getPopSongs artist limit
    printPL artist result

main :: IO ()
main = putStrLn "hello world"


runProgram :: (Member EffAPI r, Member EffPlaylist r) =>
  Eff r String
runProgram = do
  artist <- getArtist
  limit <- getLimit
  playlist <- requestapi artist limit
  genPlaylist artist playlist



data EffAPI a
  = ReqAPI Artist Limit (SongList -> a)
  | ReqArt (Artist -> a)
  | ReqLim (Limit -> a)
  deriving (Functor, Typeable)

requestapi :: (Member EffAPI r) => Artist -> Limit -> Eff r SongList
requestapi a l = effect $ \k -> inj $ ReqAPI a l k

getArtist :: (Member EffAPI r) => Eff r Artist
getArtist = effect $ \k -> inj $ ReqArt k

getLimit :: (Member EffAPI r) => Eff r Limit
getLimit = effect $ \k -> inj $ ReqLim k

-- definition IO handler for API 1

requestHandler :: (Member LiftIO r) => Handler EffAPI r a a
requestHandler (Value a) = return a
requestHandler (Comp (ReqAPI a l k)) = do
  x <- finish $ liftIO $ runMaybeT $ lastFmApiTopRequest a l
  k (fromJust x)
requestHandler (Comp (ReqArt k)) = do
    x <- finish $ liftIO getArtistIO
    k x
requestHandler (Comp (ReqLim k)) = do
    x <- finish $ liftIO getLimitIO
    k x

mockHandler :: Handler EffAPI r a a
mockHandler (Value a) = return a
mockHandler (Comp (ReqAPI a l k)) = do
  x <- finish $  return $ runIdentity $ requestMockS a l
  k (fromJust x)
mockHandler (Comp (ReqArt k)) = do
    x <- finish $ return $ runIdentity getArtistMock
    k x
mockHandler (Comp (ReqLim k)) = do
    x <- finish $ return $ runIdentity getLimitMock
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

textPlayHandler :: Handler EffPlaylist r a a
textPlayHandler (Value a) = return a
textPlayHandler (Comp (EffPlaylist a sl k)) = do
  x <- finish $ return $ runIdentity $ playlistToString a sl
  k x

testH :: IO String
testH = runProgram
  & handle requestHandler
  & handle ytPlayHandler
  & handle ioHandler
  & runPure

testMock :: String
testMock = runProgram
  & handle mockHandler
  & handle textPlayHandler
  & runPure