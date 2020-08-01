{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module EffArtistScrobblesPlaylist 
 (testH, testH2, testMock) where

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
import           Web.Browser

import           Control.Effects.Eff
import           Control.Effects.IO
import           Control.Effects.State

main :: IO ()
main = testH

-- Runs the program, contains the abstract operations
runProgram :: (Member EffAPI r, Member EffPlaylist r, Member EffIO r) =>
  Eff r ()
runProgram = do
  (artist,limit) <- getArtLim
  playlist <- requestapi artist limit
  pllink <- genPlaylist artist playlist
  resultPL pllink


--Effect class containing user-interaction related effects
data EffIO a 
  = ReqArtLim ((Artist, Limit) -> a) 
  | PLResult String ( () -> a)
  deriving (Functor, Typeable) 

--Getting user input
getArtLim :: (Member EffIO r) => Eff r (Artist, Limit)
getArtLim = effect $ \k -> inj $ ReqArtLim k

--Returning the end result to the user
resultPL :: (Member EffIO r) =>  String -> Eff r ()
resultPL s = effect $ \k -> inj $ PLResult s k

--Through the command prompt
cmdHandler :: (Member LiftIO r) => Handler EffIO r a a
cmdHandler (Value a) = return a
cmdHandler (Comp (ReqArtLim k)) = do
    x <- finish $ liftIO getArtLimIO
    k x
cmdHandler (Comp (PLResult s k)) = do
    x <- finish $ liftIO $ putStrLn s
    k x

--Through the threepenny-GUI browser interface
webHandler :: (Member LiftIO r) => Handler EffIO r a a
webHandler (Value a) = return a
webHandler (Comp (ReqArtLim k)) = do
    x <- finish $ liftIO getArtLimWeb
    k x
webHandler (Comp (PLResult s k)) = do
    x <- finish $ liftIO $ openBrowser s >>= (\b -> if b then putStrLn "success" else putStrLn "failure")
    k x

--Have no user input, return to commandline
noIOHandler :: (Member LiftIO r) => (String, String) -> Handler EffIO r a a 
noIOHandler _ (Value a) = return a
noIOHandler (a,l) (Comp (ReqArtLim k)) = do
    x <- finish $ return $ runIdentity $ getArtLimMock a l
    k x
noIOHandler _ (Comp (PLResult s k)) = do
    x <- finish $ liftIO $ putStrLn s
    k x


--Define the request method
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

testH :: IO ()
testH = runProgram
  & handle cmdHandler
  & handle requestHandler
  & handle ytPlayHandler
  & handle ioHandler
  & runPure

testH2 :: IO ()
testH2 = runProgram
  & handle webHandler
  & handle requestHandler
  & handle spPlayHandler
  & handle ioHandler
  & runPure

testMock :: (String, String) -> IO ()
testMock al = runProgram
  & handle (noIOHandler al)
  & handle mockHandler
  & handle spPlayHandler
  & handle ioHandler
  & runPure