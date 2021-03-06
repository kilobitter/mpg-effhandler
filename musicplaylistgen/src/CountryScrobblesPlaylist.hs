{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module CountryScrobblesPlaylist where

-- 835789484db9bf9715aa1fd908be19f9 - API key
import Control.Monad.Trans.Maybe
import Network.HTTP.Client
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import Control.Monad.Trans

import Data.Functor.Identity
import API.LastAPIHandler
import API.YTAPIHandler
import API.APITypes
import API.MiscIO

countryRequest :: (Monad f) => f Country -> f Limit -> (Country -> Limit -> f CSongList) -> (Country -> CSongList -> f a) -> f a
countryRequest getCountry getLimit getPopSongs printPL = do
    country <- getCountry
    limit <- getLimit
    result <- getPopSongs country limit
    printPL country result

main :: IO ()
--main = putStr (runIdentity (countryRequest getArtistMock getLimitMock requestMockC playlistToStringC))
main = fmap (const ()) (runMaybeT (countryRequest (lift getCountryIO) (lift getLimitIO) lastFmApiGeoRequest (\a b -> lift (printPlaylistC a b))))

--     print ("http://ws.audioscrobbler.com/2.0/?method=artist.gettoptracks" ++ 
--         "&artist=" ++ artist ++
--         "&api_key=" ++ apiKey ++ 
--         "&limit=" ++ limit ++
--         "&format=json")