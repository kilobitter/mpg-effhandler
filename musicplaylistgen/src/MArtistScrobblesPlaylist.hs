{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}

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



artistRequest :: (Monad f) => f Artist -> f Limit -> (Artist -> Limit -> f SongList) -> (Artist -> SongList -> f a) -> f a
artistRequest getArtist getLimit getPopSongs printPL = do
    artist <- getArtist
    limit <- getLimit
    result <- getPopSongs artist limit
    printPL artist result

main :: IO ()
--main = putStr (runIdentity (artistRequest getArtistMock getLimitMock requestMockS playlistToString))
--main = fmap (const ()) (runMaybeT (artistRequest (lift getArtistIO) (lift getLimitIO) lastFmApiTopRequest (\a b -> lift (ytURLGen a b))))
-- main = fmap (const ()) (runMaybeT (artistRequest (lift getArtistIO) (lift getLimitIO) lastFmApiTopRequest (\a b -> lift (printPlaylist a b))))


data Proxy s = Proxy

data MusicAPIs
  = LastFM
  | Mock

class MusicAPI (a :: MusicAPIs) f where
  getArtist :: Proxy a -> f Artist
  getLimit :: Proxy a -> f Limit
  getPopSongs :: Proxy a -> Artist -> Limit -> MaybeT f SongList

instance MusicAPI LastFM IO where
  getArtist _ = getArtistIO
  getLimit _ = getLimitIO
  getPopSongs _ = lastFmApiTopRequest

instance MusicAPI Mock Identity where
  getArtist _ = getArtistMock
  getLimit _ = getLimitMock
  getPopSongs _ = RequestMockS

main = do
  getArtist (Proxy @LastFM)
  getArtist (Proxy @Other)