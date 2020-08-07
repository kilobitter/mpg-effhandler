{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MArtistScrobblesPlaylist where

import API.LastAPIHandler
import API.YTAPIHandler
import API.SpotifyAPIHandler
import API.APITypes
import API.MiscIO
import Mocking.LastMock
import Mocking.MiscMock
import GUI.WebForm

import           Data.Functor.Identity
import           Data.Maybe
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans
import           Web.Browser
import           Control.Lens
import           Control.Monad.State.Lazy

-- artistRequest :: (Monad f) => f Artist -> f Limit -> (Artist -> Limit -> f SongList) -> (Artist -> SongList -> f a) -> f a
-- -- artistRequest :: (Proxy MusicAPI m) => m -> f a
-- artistRequest :: InputMethod a -> MusicAPI a -> PlaylistAPI a -> a ()
-- artistRequest :: (Monad f) => Proxy a -> Proxy b -> Proxy c -> f ()
-- artistRequest iP mP pP = do
--    (artist,limit) <- getArtLim iP
--    list <- runMaybeT $ getPopSongs mP artist limit
--    link <- genPlaylist pP artist (fromJust list)
--    showLink iP link


-- main :: IO ()
-- main = artistRequest (Proxy @MockInput) (Proxy @MockAPI) (Proxy @MockPL)
-- artistRequest (Proxy @Mock)
--main = putStr (runIdentity (artistRequest getArtistMock getLimitMock requestMockS playlistToString))
--main = fmap (const ()) (runMaybeT (artistRequest (lift getArtistIO) (lift getLimitIO) lastFmApiTopRequest (\a b -> lift (ytURLGen a b))))
-- main = fmap (const ()) (runMaybeT (artistRequest (lift getArtistIO) (lift getLimitIO) lastFmApiTopRequest (\a b -> lift (printPlaylist a b))))

data TestState = TestState 
    { _fakeInput :: (String, String)
    , _fakeAPI :: SongList
    , _fakeOut :: String
    }

makeLenses ''TestState

newtype TestM (inout :: InputMethod) (requestType :: RequestMethod) a =  
  TestM {runTestM :: (State TestState) a}
   deriving ( Functor
             , Applicative
             , Monad
             , MonadState TestState)

newtype AppM (inout :: InputMethod) (requestType :: RequestMethod) (playlistType :: PLMethod) a = AppM
  { runAppM :: MaybeT IO a
  } deriving (Functor, Applicative, Monad, MonadIO)


newtype ExtraAppM (inout :: InputMethod) (requestType :: RequestMethod) (playlistType :: PLMethod) a = ExtraAppM
  { runExtraAppM :: StateT TestState (MaybeT IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState TestState)

data InputMethod
  = Cmd
  | WebForm
  | MockInputStatic
  | MockInputState

class MusicInput f where
  getArtLim :: f (Artist, Limit)
  showLink :: String -> f ()

instance MusicInput (ExtraAppM Cmd reqt plt) where
  getArtLim = liftIO getArtLimIO
  showLink = liftIO . putStrLn

instance MusicInput (ExtraAppM WebForm reqt plt) where
  getArtLim = liftIO getArtLimWeb
  showLink l = liftIO $ openBrowser l >>= (\b -> if b then putStrLn "success" else putStrLn "failure")

-- moet hier de argumenten geven want gaat niet in de aanroep, er is geen "handler" die argumenten kan pakken, jammer
instance MusicInput (ExtraAppM MockInputStatic reqt plt) where
  getArtLim = return $ runIdentity $ getArtLimMock "Roedel" "7"
  showLink _ = return ()

instance MusicInput (ExtraAppM MockInputState reqt plt) where
  getArtLim = use fakeInput
  showLink l = fakeOut .= l


data RequestMethod
  = LastFM
  | ParLastFM
  | MockAPIState
  | MockAPIStatic

class MusicAPI f where
  getPopSongs :: Artist -> Limit -> f SongList

instance MusicAPI (ExtraAppM inoutt LastFM plt) where
  getPopSongs a l = ExtraAppM $ lift $ lastFmApiTopRequest a l

instance MusicAPI (ExtraAppM inoutt ParLastFM plt) where
  getPopSongs a l = ExtraAppM $ lift $ multFMArtistList (splitArtists a) l

instance MusicAPI (ExtraAppM inoutt MockAPIStatic plt) where
  getPopSongs a l = return $ runIdentity $ requestMockS a l

instance MusicAPI (ExtraAppM inoutt MockAPIState plt) where
  getPopSongs _ _ = use fakeAPI

data PLMethod
 = YouTube
 | Spotify
 | MockPL

class PlaylistAPI f where
  genPlaylist :: Artist -> SongList -> f String

instance PlaylistAPI (ExtraAppM inoutt reqt YouTube) where
  genPlaylist a sl = liftIO (ytURLGen a sl)

instance PlaylistAPI (ExtraAppM inoutt reqt Spotify) where
  genPlaylist a sl = liftIO (spURLGen a sl)

instance PlaylistAPI (ExtraAppM inoutt reqt MockPL) where
  genPlaylist a sl =  return $ runIdentity $ playlistToString a sl