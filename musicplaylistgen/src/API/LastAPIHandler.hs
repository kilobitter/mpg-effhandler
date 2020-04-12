{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module API.LastAPIHandler where

import           API.APIKeys
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Functor.Identity
import qualified Data.Text as T
import           GHC.Generics
import           Data.ByteString.Char8 (pack)
import Control.Concurrent.Async
import           Data.List as L
import           Data.List.Split
import           Data.Maybe

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Base
import           API.APITypes
import           Control.Concurrent
import           Control.Concurrent.MSem
import qualified Data.Traversable as T


settings :: ManagerSettings
settings =  managerSetProxy
                (proxyEnvironment Nothing)
                tlsManagerSettings

lastFmApiTopRequest :: Artist -> Limit -> MaybeT IO SongList
lastFmApiTopRequest artist limit = do
  man <- lift (newManager settings)
  reqURL <- parseRequest ("http://ws.audioscrobbler.com/2.0/?method=artist.gettoptracks" ++ 
                           "&artist=" ++ artist ++
                           "&api_key=" ++ lastAPIKey ++ 
                           "&limit=" ++ limit ++
                           "&format=json")
  let req = reqURL
            { proxy = Just $ Proxy "localhost" 1234
            }
  response <- lift (httpLbs req man)
  MaybeT (return (decode (responseBody response) :: Maybe SongList))

lastFmApiGeoRequest :: Country -> Limit -> MaybeT IO CSongList
lastFmApiGeoRequest country limit = do
  let settings = managerSetProxy
            (proxyEnvironment Nothing)
            defaultManagerSettings
  man <- lift (newManager settings)
  reqURL <- parseRequest ("http://ws.audioscrobbler.com/2.0/?method=geo.gettoptracks" ++ 
                           "&country=" ++ country ++
                           "&api_key=" ++ lastAPIKey ++ 
                           "&limit=" ++ limit ++
                         "&format=json")
  let req = reqURL
            -- Note that the following settings will be completely ignored.
            { proxy = Just $ Proxy "localhost" 1234
            }
  response <- lift (httpLbs req man)
  MaybeT (return (decode (responseBody response) :: Maybe CSongList))

  --HIER zou decode dus abstract moeten zijn, vervangen door "decoder" ofzo
  --die zou dan ofwel gewoon "decode" of "parseEither verboseparse" moeten zijn,
  -- en da zou op zich al genoeg moeten zijn
  -- oftewel de hele rambam abstract maken??

-------------------------
--concurrency functions
--------------------------

--return a list of songlists based on multiple artists, separated by commas

splitArtists :: Artist -> [Artist]
splitArtists = splitOn ","

mapPool :: T.Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
mapPool max f xs = do
    sem <- new max
    mapConcurrently (with sem . f) xs

multFMArtistList :: [Artist] -> Limit -> MaybeT IO SongList
multFMArtistList artists limit = do 
  lizt <- lift $ mapPool 2 (\x -> runMaybeT $ lastFmApiTopRequest x limit) artists
  MaybeT $ return (concatSongLists $ removeNothings lizt)
  

interleaveSongList :: [Maybe SongList] -> SongList
interleaveSongList msl = SongList $ concat $ L.transpose (map list (removeNothings msl))

removeNothings :: [Maybe a] -> [a]
removeNothings [] = []
removeNothings (Just h:t) =  h:removeNothings t
removeNothings (Nothing:t) =  removeNothings t

concatSongLists :: [SongList] -> Maybe SongList
concatSongLists lsl = Just $ SongList $ concatMap list lsl