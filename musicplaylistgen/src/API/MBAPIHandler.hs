{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module API.MBAPIHandler where

import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Functor.Identity
import qualified Data.Text as T
import           GHC.Generics
import           Data.ByteString.Char8 (pack)
import           Network.HTTP.Types.Header

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Base
import           API.APITypes

settings = managerSetProxy
  (proxyEnvironment Nothing)
  defaultManagerSettings


mbArtistRequest :: Artist -> MaybeT IO MBArtist
mbArtistRequest artist = do
  man <- lift (newManager settings)
  reqURL <- parseRequest ("http://musicbrainz.org/ws/2/artist/" ++ 
                           "?query=artist:" ++ artist ++
                           "&fmt=json&limit=1")
  let req = reqURL
            { proxy = Just $ Proxy "localhost" 1234 ,
              requestHeaders = [(hUserAgent,"MusicPlaylistGenerator/0.0.1 ( ghijs.kilani@student.kuleuven.be )")]
            }
  response <- lift (httpLbs req man)
  MaybeT (return (decode (responseBody response) :: Maybe MBArtist))