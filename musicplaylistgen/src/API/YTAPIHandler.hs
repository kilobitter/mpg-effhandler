{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module API.YTAPIHandler where

-- AIzaSyBjXUQoe09edP1nkUC55xQLoJrFM3SoAx4 - yt API key
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import qualified Data.Text as T
import           GHC.Generics
import           Data.ByteString.Char8 (pack)

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Base
import           API.APITypes

ytAPIkey :: String
ytAPIkey = "AIzaSyBjXUQoe09edP1nkUC55xQLoJrFM3SoAx4"


settings :: ManagerSettings
settings =  managerSetProxy
                (proxyEnvironment Nothing)
                tlsManagerSettings

generateYTPlaylist :: Artist -> SongList -> IO [Maybe YTLink]
generateYTPlaylist artist (SongList []) = return []
generateYTPlaylist artist (SongList (slh:slt)) = do
  man <- newManager settings
  reqURL <- parseUrlThrow ("https://www.googleapis.com/youtube/v3/search" ++
                              "?q=" ++ urlEncode (artist ++ " " ++ name slh) ++ 
                              "&maxResults=1" ++
                              "&part=snippet" ++
                              "&key=" ++ ytAPIkey)
  let req = reqURL
            { proxy = Just $ Proxy "localhost" 1234
            }
  response <- httpLbs req man
  helper <- generateYTPlaylist artist (SongList slt)
  return ((decode (responseBody response) :: Maybe YTLink) : helper)

removeNothings :: [Maybe a] -> [a]
removeNothings [] = []
removeNothings (Just h:t) =  h:removeNothings t
removeNothings (Nothing:t) =  removeNothings t

playlistLink :: [YTLink] -> String
playlistLink [] = ""
playlistLink lst = vid (head lst) ++ "," ++ playlistLink (tail lst) 

ytURLGen :: Artist -> SongList -> IO ()
ytURLGen a sl = do
  ytpl <- generateYTPlaylist a sl
  putStr "http://www.youtube.com/watch_videos?video_ids="
  putStrLn $ playlistLink $ removeNothings ytpl