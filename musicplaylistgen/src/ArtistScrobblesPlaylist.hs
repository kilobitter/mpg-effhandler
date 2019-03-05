{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module ArtistScrobblesPlaylist where

import LastAPIHandler
import YTAPIHandler
import APITypes
import MiscIO

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
--main = fmap (const ()) (runMaybeT (artistRequest (lift getArtistIO) (lift getLimitIO) lastFmApiRequest (\a b -> lift (ytURLGen a b))))
main = fmap (const ()) (runMaybeT (artistRequest (lift getArtistIO) (lift getLimitIO) lastFmApiTopRequest (\a b -> lift (printPlaylist a b))))



-- testReq :: Artist -> SongList -> IO ()
-- testReq artist (SongList []) = putStrLn ""
-- testReq artist (SongList (slh:slt)) = do
--   man <- newManager settings
--   reqURL <- parseUrlThrow ("https://www.googleapis.com/youtube/v3/search" ++
--                               "?q=" ++ urlEncode (artist ++ " " ++ name slh) ++ 
--                               "&maxResults=1" ++
--                               "&part=snippet" ++
--                               "&key=" ++ ytAPIkey)
--   let req = reqURL
--             { proxy = Just $ Proxy "localhost" 1234
--             }
--   response <- httpLbs req man
--   testReq artist (SongList slt)
--   print (eitherDecode (responseBody response) :: Either String YTLink)

-- requestArtistMetadata :: (Artist -> Limit -> f ByteString)
-- parseResponse :: (ByteString -> f SongList)