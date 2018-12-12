{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module ArtistConcertPlaylist where

-- 58eb085d-e1b6-4d6f-a070-f35e860dd4fe - API key
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.Dates
import           Data.Functor.Identity
import qualified Data.Text as T
import           Data.Time
import           Data.Time.Format
import           GHC.Generics

import           Network.HTTP.Client
import           Network.HTTP.Types.Header

apiKey :: ByteString
apiKey = "58eb085d-e1b6-4d6f-a070-f35e860dd4fe"
apiHeader :: HeaderName
apiHeader = "x-api-key"

newtype SetListList = SetListList {slist :: [SetList] }
data SetList = SetList {setartist :: String, setDate :: Day, setlist :: [Song]}
data MBArtist = MBArtist {artname :: String, artId :: String}
type SongList = [Song]

type Artist = String
type Limit = String
type Song = String

instance FromJSON MBArtist where 
  parseJSON = withObject "MBArtist" $ \o -> do
          [artistO] <- o .: "artists"
          idA     <- artistO .: "id"
          name <- artistO .: "name"
          return $ MBArtist name idA

instance FromJSON SetList where
  parseJSON = withObject "SetList" $ \o -> do
          slists <- o .: "setlist"
          idA     <- artistO .: "id"
          name <- artistO .: "name"
          return $ SetList artist (parseTimeM True defaultTimeLocale "%m-%d-%Y" sDate :: Maybe Day) slist

-- instance FromJSON SongList where
--   parseJSON = withObject "SongList" $ \o -> do
--           topSongsO <- o .: "toptracks"
--           songList     <- topSongsO .: "track"
--           return $ SongList songList

-- instance FromJSON SongPlays where
--         parseJSON = withObject "SongPlays" $ \o -> do
--           nname <- o .: "name"
--           nplaycount  <- o .: "playcount"
--           return $ SongPlays nname (read nplaycount :: Int)

-- Mock/Pure functions

getArtistMock :: Identity String
getArtistMock = "Bones"

getLimitMock :: Identity String
getLimitMock = "3"

requestMock :: Artist -> Limit -> Identity SongList
requestMock art lim
    | art == "Bones" && lim == "3" = Identity (SongList [SongPlays "Dirt" 25, SongPlays "hdmi"  18, SongPlays "Corduroy" 7])
    | otherwise = error "Called with wrong arguments"


playlistToString :: Artist -> SongList -> Identity String
-- playlistToString artist Nothing = "api request/decoding failed for artist: "
playlistToString _ [] = ""
playlistToString artist (sph:spt) = Identity
        (artist ++ " - " ++ sph ++
        runIdentity (playlistToString artist (SongList spt)))


-- API/IO functions

getArtistIO :: IO Artist
getArtistIO = do
  putStrLn "Input desired artist"
  getLine

getLimitIO :: IO Artist
getLimitIO = do
  putStrLn "Input desired limit"
  getLine

setListApiRequest :: MBArtist -> Limit -> MaybeT IO SongList
setListApiRequest mbartist limit = do
  let settings = managerSetProxy
            (proxyEnvironment Nothing)
            defaultManagerSettings
  man <- lift (newManager settings)
  reqURL <- parseRequest ("https://api.setlist.fm/rest/1.0/artist/" ++
                            artId mbartist ++ "/setlists")
  let req = reqURL
            -- Note that the following settings will be completely ignored.
            { proxy = Just $ Proxy "localhost" 1234,
              requestHeaders = [(apiHeader, apiKey),(hAccept,"application/json")]
            }
  response <- lift (httpLbs req man)
  MaybeT (return (decode (responseBody response) :: Maybe SongList))

mbArtistRequest :: Artist -> MaybeT IO MBArtist
mbArtistRequest artist = do
  let settings = managerSetProxy
            (proxyEnvironment Nothing)
            defaultManagerSettings
  man <- lift (newManager settings)
  reqURL <- parseRequest ("http://musicbrainz.org/ws/2/artist/" ++ 
                           "?query=" ++ artist ++
                           "&fmt=json&limit=1")
  let req = reqURL
            { proxy = Just $ Proxy "localhost" 1234
            }
  response <- lift (httpLbs req man)
  MaybeT (return (decode (responseBody response) :: Maybe MBArtist))

printPlaylist :: Artist -> SongList -> IO ()
-- printPlaylist artist Nothing = putStrLn ("api request/decoding failed for artist: " ++ artist)
printPlaylist _ [] = putStr ""
printPlaylist artist (sph:spt) = do
        putStrLn (artist ++ " - " ++ sname ++ " - " ++ "Plays:" ++ show splays) 
        printPlaylist artist spt

-- Common code

artistRequest :: (Monad f) => f Artist -> f Limit -> (Artist -> f MBArtist) -> (MBArtist -> Limit -> f SongList) -> (MBArtist -> SongList -> f a) -> f a
artistRequest getArtist getLimit idRequest getPopSongs printPL = do
    artist <- getArtist
    artistID <- idRequest artist
    limit <- getLimit
    result <- getPopSongs artistID limit
    printPL artistID result

--main :: IO ()
-- main = putStr (runIdentity (artistRequest getArtistMock getLimitMock requestMock playlistToString))
--main = fmap (const ()) (runMaybeT (artistRequest (lift getArtistIO) (lift getLimitIO) lastFmApiRequest (\a b -> lift (printPlaylist a b))))

--     print ("http://ws.audioscrobbler.com/2.0/?method=artist.gettoptracks" ++ 
--         "&artist=" ++ artist ++
--         "&api_key=" ++ apiKey ++ 
--         "&limit=" ++ limit ++
--         "&format=json")