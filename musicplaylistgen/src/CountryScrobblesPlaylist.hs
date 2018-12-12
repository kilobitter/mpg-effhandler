{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module CountryScrobblesPlaylist where

-- 835789484db9bf9715aa1fd908be19f9 - API key
import           Control.Monad.Trans.Maybe
import Network.HTTP.Client
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import Control.Monad.Trans

import Data.Functor.Identity

apiKey :: String
apiKey = "835789484db9bf9715aa1fd908be19f9"

type Country = String
type Limit = String
data SongPlays =  SongPlays {artistName :: String, name :: String, playcount :: Int} deriving Show
newtype SongList = SongList {list :: [SongPlays] } deriving Show

instance FromJSON SongList where
    parseJSON = withObject "SongList" $ \o -> do
            topSongsO <- o .: "tracks"
            songList     <- topSongsO .: "track"
            return $ SongList songList

instance FromJSON SongPlays where
        parseJSON = withObject "SongPlays" $ \o -> do
          artistO <- o .: "artist"
          aname <- artistO .: "name"
          sname <- o .: "name"
          nplaycount  <- o .: "listeners"
          return $ SongPlays aname sname (read nplaycount :: Int)

printPlaylist :: Country -> SongList -> IO ()
-- printPlaylist artist Nothing = putStrLn ("api request/decoding failed for artist: " ++ artist)
printPlaylist country (SongList []) = putStr ("--- data for " ++ country ++ " ---")
printPlaylist country (SongList (sph:spt)) = do
        putStrLn (aname ++ " - " ++ sname ++ " - " ++ "Plays:" ++ show splays) 
        printPlaylist country (SongList spt)
                where SongPlays aname sname splays = sph

playlistToString :: Country -> SongList -> Identity String
-- playlistToString artist Nothing = "api request/decoding failed for artist: "
playlistToString _ (SongList []) = ""
playlistToString artist (SongList (sph:spt)) = Identity
        (artist ++ " - " ++ sname ++ " - " ++ "Plays:" ++ show splays ++ "\n" ++
        runIdentity (playlistToString artist (SongList spt)))
                where SongPlays _ sname splays = sph


getArtistMock :: Identity String
getArtistMock = "Bones"

getLimitMock :: Identity String
getLimitMock = "3"

requestMock :: Country -> Limit -> Identity SongList
requestMock country lim
    | country == "Belgium" && lim == "3" = Identity (SongList [SongPlays "Bones" "Dirt" 25, SongPlays "Bones" "hdmi"  18, SongPlays "Bones" "Corduroy" 7])
    | otherwise = error "Called with wrong arguments"

lastFmApiRequest :: Country -> Limit -> MaybeT IO SongList
lastFmApiRequest country limit = do
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
  MaybeT (return (decode (responseBody response) :: Maybe SongList))

getCountryIO :: IO Country
getCountryIO = do
  putStrLn "Input desired country"
  getLine

getLimitIO :: IO Limit
getLimitIO = do
  putStrLn "Input desired limit"
  getLine

  -- 
countryRequest :: (Monad f) => f Country -> f Limit -> (Country -> Limit -> f SongList) -> (Country -> SongList -> f a) -> f a
countryRequest getCountry getLimit getPopSongs printPL = do
    country <- getCountry
    limit <- getLimit
    result <- getPopSongs country limit
    printPL country result

main :: IO ()
--main = putStr (runIdentity (countryRequest getArtistMock getLimitMock requestMock playlistToString))
main = fmap (const ()) (runMaybeT (countryRequest (lift getCountryIO) (lift getLimitIO) lastFmApiRequest (\a b -> lift (printPlaylist a b))))

--     print ("http://ws.audioscrobbler.com/2.0/?method=artist.gettoptracks" ++ 
--         "&artist=" ++ artist ++
--         "&api_key=" ++ apiKey ++ 
--         "&limit=" ++ limit ++
--         "&format=json")