{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module PerfTest where

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
import           Criterion.Main.Options
import           Criterion.Main

import           Network.HTTP.Client
import           Network.HTTP.Types.Header

import           ArtistScrobblesPlaylist as M
import           EffArtistScrobblesPlaylist as E
import           MArtistScrobblesPlaylist as T

main = defaultMain [
       bgroup "eff" [ bench "10" $ whnf M.mockRun
                    , bench "35" $ whnf testMock
                    , bench "37" $ whnf fib 37
                    ]
                   ]