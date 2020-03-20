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

import           Data.Functor.Identity
import           Data.Maybe
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans

-- artistRequest :: (Monad f) => f Artist -> f Limit -> (Artist -> Limit -> f SongList) -> (Artist -> SongList -> f a) -> f a
-- artistRequest :: (Proxy MusicAPI m) => m -> f a
artistRequest mP pP = do
   artist <- getArtist mP
   limit <- getLimit mP
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

data MusicAPIs
  = LastFM
  | MultiLastFM
  | Mock

class MusicAPI (a :: MusicAPIs) f where
  getArtist :: Proxy a -> f Artist
  getLimit :: Proxy a -> f Limit
  getPopSongs :: Proxy a -> Artist -> Limit -> MaybeT f SongList

instance MusicAPI LastFM IO where
  getArtist _ = getArtistIO
  getLimit _ = getLimitIO
  getPopSongs _ = lastFmApiTopRequest

instance MusicAPI MultiLastFM IO where
  getArtist _ = getArtistIO
  getLimit _ = getLimitIO
  getPopSongs _ a = multFMArtistList (splitArtists a)
  

instance MusicAPI Mock Identity where
  getArtist _ = getArtistMock
  getLimit _ = getLimitMock
  getPopSongs _ = requestMockS

data PLAPIs
 = YouTube
 | Spotify
 | MockPL

class PlaylistAPI (a :: PLAPIs) f where
  genPlaylist :: Proxy a -> Artist -> SongList -> f String

instance PlaylistAPI YouTube IO where
  genPlaylist _ = ytURLGen

instance PlaylistAPI MockPL Identity where
  genPlaylist _ =  playlistToString


  
-- class MusicAPI f where
--   getArtist :: f Artist
--   getLimit :: f Limit
--   getPopSongs :: Artist -> Limit -> MaybeT f SongList

-- instance MusicAPI IO where
--   getArtist = getArtistIO
--   getLimit = getLimitIO
--   getPopSongs = lastFmApiTopRequest

-- instance MusicAPI Identity where
--   getArtist = getArtistMock
--   getLimit = getLimitMock
--   getPopSongs = requestMockS