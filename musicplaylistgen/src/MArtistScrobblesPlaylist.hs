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
artistRequest p = do
   artist <- getArtist p
   limit <- getLimit p
   getPopSongs p artist limit

main :: IO ()
main = artistRequest (Proxy @LastFM)
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
  getPopSongs _ = multFMArtistList
  

instance MusicAPI Mock Identity where
  getArtist _ = getArtistMock
  getLimit _ = getLimitMock
  getPopSongs _ = requestMockS
