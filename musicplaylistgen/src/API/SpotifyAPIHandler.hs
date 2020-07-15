{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module API.SpotifyAPIHandler where


import           API.APIKeys
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson as AE
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Maybe
import           GHC.Generics
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Internal as IB
import           Control.Exception
import           Network.OAuth.OAuth2
import           Network.OAuth.OAuth2.TokenRequest

import           Web.Browser
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Base as NB
import           Network.HTTP.Listen
import           Network.HTTP.Types.URI
import           Network.URI.Encode as E
import           Network.URI
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Socket
import qualified Network.Stream as NS
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
                "&redirect_uri=http://localhost:8999/callback")
    
    reqURI <- bracket (prepareSocket 8999) close
            (\sock -> do 
                conn <- acceptConnection sock
                stream <- openStream conn
                result <- receiveRequest (stream :: Stream String)
                closeStream stream
                return result)
    let queryList = parseSpotifyRequest reqURI
    let authcode = head [b | (a,b) <- queryList, a == "code"]
    spotifyRes <- fetchAccessToken man spotifyOAuth (ExchangeToken $ decodeUtf8 authcode)
    let (spotifyAT, spotifyRT) = takeTokens spotifyRes
    generatePlaylist spotifyAT a sl
    
packStrBS :: String -> IB.ByteString
packStrBS = encodeUtf8 . T.pack

takeTokens :: OAuth2Result Errors OAuth2Token -> (AccessToken, Maybe RefreshToken)
takeTokens (Left err) = (AccessToken (T.pack $ show err), Just $ RefreshToken "NaN")
takeTokens (Right token) = (accessToken token, refreshToken token)

parseSpotifyRequest :: NS.Result (NB.Request a) -> SimpleQuery
parseSpotifyRequest (Left a) = [("ERROR","ERROR")]
parseSpotifyRequest (Right b) = parseSimpleQuery $ packStrBS $ uriQuery $ rqURI b

generatePlaylist :: AccessToken -> Artist -> SongList -> IO String
generatePlaylist spAT art sl = do
  sIDs <- spotifySearch spAT art sl
  userId <- spotifyGetUser spAT
  playlistID <- spotifyCreatePlaylist spAT (fromJust userId) art
  let tracksURI = toSpotURIs sIDs
  print tracksURI
  statusResponse <- addSongsToPL spAT (fromJust playlistID) tracksURI
  return $ "http://open.spotify.com/user/" ++ usid (fromJust userId) ++ "/playlist/" ++ pl (fromJust playlistID)


spotifySearch :: AccessToken -> Artist -> SongList -> IO [Maybe SPTrackId]
spotifySearch spAT art (SongList []) = return []
spotifySearch spAT art (SongList (slh:slt)) = do
  man <- newManager settings
  let urlVars = urlEncodeVars [("q","artist:" ++ art ++ " track:" ++ name slh),
                            ("type","track"),
                            ("limit","1")]
  reqURL <- parseUrlThrow ("https://api.spotify.com/v1/search?" ++ 
                           urlVars)
  let req = reqURL
            { proxy = Just $ Proxy "localhost" 1234,
              requestHeaders = [(hAuthorization, bearerHeader spAT)]
            }
  response <- httpLbs req man
  helper <- spotifySearch spAT art (SongList slt)
  return ((AE.decode (responseBody response) :: Maybe SPTrackId) : helper)

spotifyCreatePlaylist :: AccessToken -> SPUserId -> Artist -> IO (Maybe SPPlaylistId)
spotifyCreatePlaylist spAT user art = do
  man <- newManager settings
  let requestObject = object [ "name" .= T.append "MPG Playlist - " (T.pack art)
        , "description" .= ("An MPG-generated playlist" :: T.Text)]
  reqURL <- parseUrlThrow ("https://api.spotify.com/v1/users/" ++ usid user ++ "/playlists")
  let req = reqURL
            { proxy = Just $ Proxy "localhost" 1234,
              method = "POST",
              requestHeaders = [(hAuthorization, bearerHeader spAT),(hContentType,"application/json")],
              requestBody = RequestBodyLBS $ AE.encode requestObject
            }
  response <- httpLbs req man
  return (AE.decode (responseBody response) :: Maybe SPPlaylistId)

spotifyGetUser :: AccessToken -> IO (Maybe SPUserId)
spotifyGetUser spAT = do
  man <- newManager settings
  reqURL <- parseUrlThrow "https://api.spotify.com/v1/me"
  let req = reqURL
            { proxy = Just $ Proxy "localhost" 1234,
              requestHeaders = [(hAuthorization, bearerHeader spAT),(hContentType,"application/json")]
            }
  response <- httpLbs req man
  return (AE.decode (responseBody response) :: Maybe SPUserId)

toSpotURIs :: [Maybe SPTrackId] -> String
toSpotURIs [] = "emptylist"
toSpotURIs [Nothing] = "somethingswrong"
toSpotURIs (Nothing:t) = "somethingisverywrong"
toSpotURIs [Just item] = "spotify:track:" ++ songId item
toSpotURIs (Just h:t) = foldl (\x y -> (x ++ "," ++ y)) ("spotify:track:" ++ songId h) (map (("spotify:track:" ++) . songId . fromJust) t) 

addSongsToPL :: AccessToken -> SPPlaylistId -> String -> IO Int
addSongsToPL spAT plId trackString = do
  man <- newManager settings
  print ("https://api.spotify.com/v1/playlists/" ++ pl plId ++ "/tracks?uris=" ++ E.encode trackString)
  reqURL <- parseUrlThrow ("https://api.spotify.com/v1/playlists/" ++ pl plId ++ "/tracks?uris=" ++ E.encode trackString)
  let req = reqURL
            { proxy = Just $ Proxy "localhost" 1234,
              method = "POST",
              requestHeaders = [(hAuthorization, bearerHeader spAT),(hContentType,"application/json"),(hAccept,"application/json")]
            }
  response <- httpLbs req man
  print response
  return $ statusCode $ responseStatus response

bearerHeader :: AccessToken -> IB.ByteString
bearerHeader at = encodeUtf8 $ T.append "Bearer " (atoken at)