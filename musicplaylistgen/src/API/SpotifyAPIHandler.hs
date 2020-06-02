{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module API.SpotifyAPIHandler where


import           API.APIKeys
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import qualified Data.Text as T
import           GHC.Generics
import qualified Data.ByteString.Lazy as B
import           Data.ByteString.Char8 (pack)
import           Control.Exception
import           Network.OAuth.OAuth2
import           Network.OAuth.OAuth2.TokenRequest

import           Web.Browser
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Base
import           Network.HTTP.Listen
import           URI.Encode as E
import           Network.Socket
import           API.APITypes
import           API.MiscIO

data SpotifyID = String

listener :: Listener B.ByteString IO
listener request = print request >> return Nothing

settings :: ManagerSettings
settings =  managerSetProxy
                (proxyEnvironment Nothing)
                tlsManagerSettings

spURLGen :: Artist -> SongList -> IO String
spURLGen a sl = do
    man <- newManager settings
    openBrowser ( "https://accounts.spotify.com/authorize" ++
                "?response_type=code" ++
                "&client_id=" ++ spotifyCID ++
                "&scope=playlist-modify-public" ++
                "&redirect_uri=localhost:8999%2Fcallback")
    
    reqURI <- bracket (prepareSocket 8999) close
            (\sock -> do 
                conn <- acceptConnection sock
                stream <- openStream conn
                result <- receiveRequest stream
                closeStream stream
                return result)
    queryList <- parseSpotifyRequest reqURI
    authcode <- take 1 [b | (a,b) <- queryList, a == "code"]
    spotifyRes <- fetchAccessToken man spotifyOAuth authcode
    (spotifyAT, spotifyRT) <- takeTokens spotifyRes
    generatePlaylist spotifyAT spotifyRT a sl

    
takeTokens :: OAuth2Result Errors OAuth2Token -> (AccessToken, RefreshToken)
takeTokens Left err = do
                        print err
                        return ("NaN", "NaN")
takeTokens Right token = (accessToken token, refreshToken token)

parseSpotifyRequest :: Result -> String
parseSpotifyRequest (Left a) = "Error occurred"
parseSpotifyRequest (Right b) = parseQuery $ uriQuery $ rqURI b

generatePlaylist :: AccessToken -> RefreshToken -> Artist -> SongList -> IO String
generatePlayList spAT art sl = do
  sIDs <- spotifySearch spAT art sl
  userId <- spotifyGetUser spAT
  playlistID <- spotifyCreatePlaylist spAT
  let tracksURI = toSpotURIs sIDs
  statusResponse <- addSongsToPL spAT playlistID tracksURI
  "http://open.spotify.com/user/" ++ userId ++ "/playlist/" ++ playlistId


spotifySearch :: AccessToken -> Artist -> SongList -> IO [Maybe SPTrackId]
spotifySearch spAT art (SongList []) = return []
spotifySearch spAT art (SongList (slh:slt)) = do
  man <- newManager settings
  urlVars <- urlEncodeVars [("q","artist:" ++ art ++ " track:" ++ name slh),
                            ("type","track"),
                            ("limit","1")]
  reqURL <- parseUrlThrow ("https://api.spotify.com/v1/search?" ++ 
                           urlVars)
  let req = reqURL
            { proxy = Just $ Proxy "localhost" 1234,
              requestHeaders = [(hAuthorization,"Bearer " ++ spAT)]
            }
  response <- httpLbs req man
  helper <- spotifySearch artist (SongList slt)
  return ((decode (responseBody response) :: Maybe SPTrackId) : helper)

spotifyCreatePlaylist :: AccessToken -> SPUserId -> Artist -> IO Maybe SPPlaylistId
spotifyCreatePlaylist spAT user art = do
  man <- newManager settings
  let requestObject = object [ "name" .= (("MPG Playlist - " ++ art) :: Text)
        , "description"  .= ("An MPG-generated playlist"  :: Int)]
  reqURL <- parseUrlThrow ("https://api.spotify.com/v1/users/" ++ user ++ "/playlists")
  let req = reqURL
            { proxy = Just $ Proxy "localhost" 1234,
              method = "POST",
              requestHeaders = [(hAuthorization,"Bearer " ++ spAT),(hContentType,"application/json")],
              requestBody = RequestBodyLBS $ encode requestObject
            }
  response <- httpLbs req man
  return ((decode (responseBody response) :: Maybe SPPlaylistId) : helper)

spotifyGetUser :: AccessToken -> IO Maybe SPUserId
spotifyGetUser spAT = do
  man <- newManager settings
  reqURL <- parseUrlThrow "https://api.spotify.com/v1/me"
  let req = reqURL
            { proxy = Just $ Proxy "localhost" 1234,
              requestHeaders = [(hAuthorization,"Bearer " ++ spAT)]
            }
  response <- httpLbs req man
  return (decode (responseBody response) :: Maybe SPUserId)

toSpotURIs :: [SPTrackId] -> String
toSpotURIs [item] = ["spotify:track:" ++ item]
toSpotURIs (h:t) = foldl (\x y -> (x ++ "," ++ y)) ("spotify:track:" ++ h) (map ("spotify:track:" ++ t)) 

addSongsToPL :: AccessToken -> SPPlaylistId -> String -> IO Int
addSongsToPL spAT plId trackString = do
  man <- newManager settings
  reqURL <- parseUrlThrow ("https://api.spotify.com/v1/playlists/" ++ plId ++ "/tracks?uris=" ++ E.encode trackString)
  let req = reqURL
            { proxy = Just $ Proxy "localhost" 1234,
              requestHeaders = [(hAuthorization,"Bearer " ++ spAT)]
            }
  response <- httpLbs req man
  return $ statusCode $ responseStatus response