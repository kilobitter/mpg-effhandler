{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module SetlistAPIHandler where

import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Functor.Identity
import qualified Data.Text as T
import           GHC.Generics
import           Data.ByteString.Char8 (pack)

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Base
import           Data.ByteString (ByteString)
import           Network.HTTP.Types.Header
import           APITypes

apiKey :: ByteString
apiKey = "58eb085d-e1b6-4d6f-a070-f35e860dd4fe"
apiHeader :: HeaderName
apiHeader = "x-api-key"


setListApiRequest :: MBArtist -> Limit -> MaybeT IO SLSongList
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
  MaybeT (return (decode (responseBody response) :: Maybe SLSongList))
