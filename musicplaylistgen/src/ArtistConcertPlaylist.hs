{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module ArtistConcertPlaylist where

-- 58eb085d-e1b6-4d6f-a070-f35e860dd4fe - API key
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.Functor.Identity
import qualified Data.Text as T
import           Data.Time
import           Data.Time.Format
import           Data.Maybe
import           GHC.Generics
import           API.SetlistAPIHandler
import           API.MBAPIHandler
import           API.APITypes
import           API.MiscIO

import           Network.HTTP.Client
import           Network.HTTP.Types.Header



-- Mock/Pure functions

-- requestMock :: Artist -> Limit -> Identity SLSongList
-- requestMock art lim
--     | art == "Bones" && lim == "3" = Identity (SLSongList [SongPlays "Dirt" 25, SongPlays "hdmi"  18, SongPlays "Corduroy" 7])
--     | otherwise = error "Called with wrong arguments"



-- API/IO functions


-- Common code

artistRequest :: (Monad f) => f Artist -> f Limit -> (Artist -> f MBArtist) -> (MBArtist -> Limit -> f SLSongList) -> (Artist -> SLSongList -> f a) -> f a
artistRequest getArtist getLimit idRequest getPopSongs printPL = do
    artist <- getArtist
    artistID <- idRequest artist
    limit <- getLimit
    result <- getPopSongs artistID limit
    printPL artist result

main :: IO ()
--main = putStr (runIdentity (artistRequest getArtistMock getLimitMock requestMock playlistToString))
main = fmap (const ()) (runMaybeT (artistRequest (lift getArtistIO) (lift getLimitIO) mbArtistRequest setListApiRequest (\a b -> lift (printPlaylistSL a b))))

--     print ("http://ws.audioscrobbler.com/2.0/?method=artist.gettoptracks" ++ 
--         "&artist=" ++ artist ++
--         "&api_key=" ++ apiKey ++ 
--         "&limit=" ++ limit ++
--         "&format=json")