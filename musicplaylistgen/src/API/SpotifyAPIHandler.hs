{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module API.SpotifyAPIHandler where

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
import           API.MiscIO

settings :: ManagerSettings
settings =  managerSetProxy
                (proxyEnvironment Nothing)
                tlsManagerSettings

    