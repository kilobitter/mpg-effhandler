{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module API.LastAPIHandler where

-- 835789484db9bf9715aa1fd908be19f9 - API key
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
import           Control.Concurrent.Async

apiKey :: String
apiKey = "835789484db9bf9715aa1fd908be19f9"

ytAPIkey :: String
ytAPIkey = "AIzaSyBjXUQoe09edP1nkUC55xQLoJrFM3SoAx4"

settings :: ManagerSettings
settings =  managerSetProxy
                (proxyEnvironment Nothing)
                tlsManagerSettings

lastFmApiTopRequest :: Artist -> Limit -> MaybeT IO SongList
lastFmApiTopRequest artist limit = do
  man <- lift (newManager settings)
  reqURL <- parseRequest ("http://ws.audioscrobbler.com/2.0/?method=artist.gettoptracks" ++ 
                           "&artist=" ++ artist ++
                           "&api_key=" ++ apiKey ++ 
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
                           "&api_key=" ++ apiKey ++ 
                           "&limit=" ++ limit ++
                         "&format=json")
  let req = reqURL
            -- Note that the following settings will be completely ignored.
            { proxy = Just $ Proxy "localhost" 1234
            }
  response <- lift (httpLbs req man)
  MaybeT (return (decode (responseBody response) :: Maybe CSongList))

-------------------------
--concurrency functions
--------------------------

--return a list of songlists based on multiple artists, separated by commas

splitArtists :: Artist -> [Artist]
splitArtists = splitOn ","

multFMArtistList :: [Artist] -> Limit -> IO SongList
multFMArtistList artists limit = do 
  lizt <- mapConcurrently (\x -> runMaybeT $ lastFmApiTopRequest x limit) artists
  return (concatSongLists $ removeNothings lizt)
  

interleaveSongList :: [Maybe SongList] -> SongList
interleaveSongList msl = SongList $ concat $ L.transpose (map list (removeNothings msl))

removeNothings :: [Maybe a] -> [a]
removeNothings [] = []
removeNothings (Just h:t) =  h:removeNothings t
removeNothings (Nothing:t) =  removeNothings t

concatSongLists :: [SongList] -> SongList
concatSongLists lsl = SongList $ concatMap list lsl