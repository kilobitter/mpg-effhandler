{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module API.SetlistAPIHandler where

import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Functor.Identity
import qualified Data.Text as T
import           GHC.Generics
import           Data.ByteString.Char8 (pack)

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Base
import           Data.ByteString (ByteString)
import           Network.HTTP.Types.Header
import           API.APITypes

apiKey :: ByteString
apiKey = "58eb085d-e1b6-4d6f-a070-f35e860dd4fe"
apiHeader :: HeaderName
apiHeader = "x-api-key"


settings :: ManagerSettings
settings =  managerSetProxy
                (proxyEnvironment Nothing)
                tlsManagerSettings

setListApiRequest :: MBArtist -> Limit -> MaybeT IO SLSongList
setListApiRequest mbartist limit = do
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



setListApiRequestTest :: IO (Either String SetListList)
setListApiRequestTest mbartist limit = do
  man <- newManager settings
  reqURL <- parseRequest ("https://api.setlist.fm/rest/1.0/artist/" ++
                            artId mbartist ++ "/setlists")
  let req = reqURL
            { proxy = Just $ Proxy "localhost" 1234,
              requestHeaders = [(apiHeader, apiKey),(hAccept,"application/json")]
            }
  response <- httpLbs req man
  return (eitherDecode (responseBody response) :: Either String SetListList)

verboseParser :: Value -> Parser (Either String SetListList)
verboseParser v = 
  case parseEither parseJSON v of
    Left err -> return . Left $ err ++ " -- Invalid object is: " ++ show v
    Right parsed -> return $ Right parsed