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

import           Web.Browser
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Base
import           Network.HTTP.Listen
import           Network.Socket
import           API.APITypes
import           API.MiscIO


listener :: Listener B.ByteString IO
listener request = print request >> return Nothing

settings :: ManagerSettings
settings =  managerSetProxy
                (proxyEnvironment Nothing)
                tlsManagerSettings

spURLGen :: Artist -> SongList -> IO String
spURLGen a sl = do
    openBrowser ( "https://accounts.spotify.com/authorize" ++
                "?response_type=code" ++
                "&client_id=" ++ spotifyCID ++
                "&scope=playlist-modify-public" ++
                "&redirect_uri=localhost:8999%2Fcallback")
    
    bracket (prepareSocket 8999) close
            (\sock -> do 
                conn <- acceptConnection sock
                stream <- openStream conn
                result <- receiveRequest stream -- moet deze ergens bijhouden
                closeStream stream)
    
    

