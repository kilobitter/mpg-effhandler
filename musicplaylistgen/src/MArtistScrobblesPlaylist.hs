{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module MArtistScrobblesPlaylist where

import API.LastAPIHandler
import API.YTAPIHandler
import API.APITypes
import API.MiscIO
import Mocking.LastMock
import Mocking.MiscMock
import GUI.WebForm

import           Data.Functor.Identity
import           Data.Maybe
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans

-- artistRequest :: (Monad f) => f Artist -> f Limit -> (Artist -> Limit -> f SongList) -> (Artist -> SongList -> f a) -> f a
-- artistRequest :: (Proxy MusicAPI m) => m -> f a
artistRequest mP pP iP = do
   (artist,limit) <- getArtLim iP
   list <- getPopSongs mP artist limit
   genPlaylist pP artist list


main :: IO ()
main = do 
  print "hello"
-- artistRequest (Proxy @Mock)
--main = putStr (runIdentity (artistRequest getArtistMock getLimitMock requestMockS playlistToString))
--main = fmap (const ()) (runMaybeT (artistRequest (lift getArtistIO) (lift getLimitIO) lastFmApiTopRequest (\a b -> lift (ytURLGen a b))))
-- main = fmap (const ()) (runMaybeT (artistRequest (lift getArtistIO) (lift getLimitIO) lastFmApiTopRequest (\a b -> lift (printPlaylist a b))))

data Proxy s = Proxy

data InputMethod
  = Cmd
  | WebForm
  | MockInput

class MusicInput (a :: InputMethod) f where
  getArtLim :: Proxy a -> f (Artist, Limit)

instance MusicInput Cmd IO where
  getArtLim _ = getArtLimIO

instance MusicInput WebForm IO where
  getArtLim _ = getArtLimWeb

-- moet hier de argumenten geven want gaat niet in de aanroep, er is geen "handler" die argumenten kan pakken, jammer
instance MusicInput MockInput Identity where
  getArtLim _ = getArtLimMock "Roedel" "7"

data MusicAPIs
  = LastFM
  | MultiLastFM
  | MockAPI

class MusicAPI (a :: MusicAPIs) f where
  getPopSongs :: Proxy a -> Artist -> Limit -> MaybeT f SongList

instance MusicAPI LastFM IO where
  getPopSongs _ = lastFmApiTopRequest

instance MusicAPI MultiLastFM IO where
  getPopSongs _ a = multFMArtistList (splitArtists a)

instance MusicAPI MockAPI Identity where
  getPopSongs _ a l = lift $ requestMockS a l

data PLAPIs
 = YouTube
 | Spotify
 | MockPL

class PlaylistAPI (a :: PLAPIs) f where
  genPlaylist :: Proxy a -> Artist -> SongList -> f String

instance PlaylistAPI YouTube IO where
  genPlaylist _ = ytURLGen

instance PlaylistAPI Spotify IO where
  genPlaylist _ = spURLGen

instance PlaylistAPI MockPL Identity where
  genPlaylist _ =  playlistToString