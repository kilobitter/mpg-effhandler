{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module ArtistScrobblesPlaylist where

-- 835789484db9bf9715aa1fd908be19f9 - API key
-- AIzaSyBjXUQoe09edP1nkUC55xQLoJrFM3SoAx4 - yt API key
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Functor.Identity
import qualified Data.Text as T
import           GHC.Generics

import           Network.HTTP.Client
import           Network.HTTP.Base

apiKey :: String
apiKey = "835789484db9bf9715aa1fd908be19f9"

ytAPIkey :: String
ytAPIkey = "AIzaSyBjXUQoe09edP1nkUC55xQLoJrFM3SoAx4"

settings :: ManagerSettings
settings =  managerSetProxy
                (proxyEnvironment Nothing)
                defaultManagerSettings


data SongPlays =  SongPlays {name :: String, playcount :: Int} deriving Show
newtype SongList = SongList {list :: [SongPlays] } deriving Show
data YTLink = YTLink {vname :: String, linklist :: String}

instance FromJSON SongList where
    parseJSON = withObject "SongList" $ \o -> do
            topSongsO <- o .: "toptracks"
            songList     <- topSongsO .: "track"
            return $ SongList songList

instance FromJSON SongPlays where
        parseJSON = withObject "SongPlays" $ \o -> do
          nname <- o .: "name"
          nplaycount  <- o .: "playcount"
          return $ SongPlays nname (read nplaycount :: Int)

instance FromJSON YTLink where
    parseJSON = withObject "YTLink" $ \o -> do
      [item] <- o .: "items"
      idO <- item .: "id"
      vidId <- idO .: "videoID"
      snippetO <- o .: "snippet"
      vidName <- snippetO .: "title"
      return (YTLink vidName vidId)

-- printPlaylist :: Artist -> SongList -> IO ()
-- -- printPlaylist artist Nothing = putStrLn ("api request/decoding failed for artist: " ++ artist)
-- printPlaylist _ (SongList []) = putStr ""
-- printPlaylist artist (SongList (sph:spt)) = do
--         putStrLn (artist ++ " - " ++ sname ++ " - " ++ "Plays:" ++ show splays) 
--         printPlaylist artist (SongList spt)
--                 where SongPlays sname splays = sph

playlistToString :: Artist -> SongList -> Identity String
-- playlistToString artist Nothing = "api request/decoding failed for artist: "
playlistToString _ (SongList []) = ""
playlistToString artist (SongList (sph:spt)) = Identity
        (artist ++ " - " ++ sname ++ " - " ++ "Plays:" ++ show splays ++ "\n" ++
        runIdentity (playlistToString artist (SongList spt)))
                where SongPlays sname splays = sph

generateYTPlaylist :: Artist -> SongList -> MaybeT IO [YTLink]
generateYTPlaylist artist (SongList []) = ""
generateYTPlaylist artist (SongList (slh:slt)) = do
  man <- lift (newManager settings)
  reqURL <- parseRequest ("https://www.googleapis.com/youtube/v3/search" ++ 
                           "&q=" ++ urlEncode (artist ++ " " ++ name slh) ++ 
                           "&maxResults=1" ++
                           "&part=snippet"
                           "&key={" ++ ytAPIkey ++ "}")
  let req = reqURL
            { proxy = Just $ Proxy "localhost" 1234
            }
  response <- lift (httpLbs req man)
  MaybeT $ return $ (decode (responseBody response) :: Maybe YTLink) : lift (generateYTPlaylist artist slt)
        
        
--         return value

type Artist = String
type Limit = String

getArtistMock :: Identity String
getArtistMock = "Bones"

getLimitMock :: Identity String
getLimitMock = "3"

requestMock :: Artist -> Limit -> Identity SongList
requestMock art lim
    | art == "Bones" && lim == "3" = Identity (SongList [SongPlays "Dirt" 25, SongPlays "hdmi"  18, SongPlays "Corduroy" 7])
    | otherwise = error "Called with wrong arguments"

lastFmApiRequest :: Artist -> Limit -> MaybeT IO SongList
lastFmApiRequest artist limit = do
  man <- lift (newManager settings)
  reqURL <- parseRequest ("http://ws.audioscrobbler.com/2.0/?method=artist.gettoptracks" ++ 
                           "&artist=" ++ artist ++
                           "&api_key=" ++ apiKey ++ 
                           "&limit=" ++ limit ++
                           "&format=json")
  let req = reqURL
            -- Note that the following settings will be completely ignored.
            { proxy = Just $ Proxy "localhost" 1234
            }
  response <- lift (httpLbs req man)
  MaybeT (return (decode (responseBody response) :: Maybe SongList))

getArtistIO :: IO Artist
getArtistIO = do
  putStrLn "Input desired artist"
  getLine

getLimitIO :: IO Artist
getLimitIO = do
  putStrLn "Input desired limit"
  getLine

  -- 
artistRequest :: (Monad f) => f Artist -> f Limit -> (Artist -> Limit -> f SongList) -> (Artist -> SongList -> f a) -> f a
artistRequest getArtist getLimit getPopSongs printPL = do
    artist <- getArtist
    limit <- getLimit
    result <- getPopSongs artist limit
    printPL artist result

main :: IO ()
main = putStr (runIdentity (artistRequest getArtistMock getLimitMock requestMock playlistToString))
--main = fmap (const ()) (runMaybeT (artistRequest (lift getArtistIO) (lift getLimitIO) lastFmApiRequest (\a b -> lift (printPlaylist a b))))

--     print ("http://ws.audioscrobbler.com/2.0/?method=artist.gettoptracks" ++ 
--         "&artist=" ++ artist ++
--         "&api_key=" ++ apiKey ++ 
--         "&limit=" ++ limit ++
--         "&format=json")