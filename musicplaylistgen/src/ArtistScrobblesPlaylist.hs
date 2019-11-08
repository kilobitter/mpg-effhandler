{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module ArtistScrobblesPlaylist where

import API.LastAPIHandler
import API.YTAPIHandler
import API.APITypes
import API.MiscIO
import Mocking.LastMock
import Mocking.MiscMock

import           Data.Functor.Identity
import           Data.Maybe
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans



artistRequest :: (Monad f) => f Artist -> f Limit -> (Artist -> Limit -> f SongList) -> (Artist -> SongList -> f a) -> f a
artistRequest getArtist getLimit getPopSongs printPL = do
    artist <- getArtist
    limit <- getLimit
    result <- getPopSongs artist limit
    printPL artist result

main :: IO ()
--main = putStr (runIdentity (artistRequest getArtistMock getLimitMock requestMockS playlistToString))
--main = fmap (const ()) (runMaybeT (artistRequest (lift getArtistIO) (lift getLimitIO) lastFmApiTopRequest (\a b -> lift (ytURLGen a b))))
main = fmap (const ()) (runMaybeT (artistRequest (lift getArtistIO) (lift getLimitIO) lastFmApiTopRequest (\a b -> lift (printPlaylist a b))))

mockRun :: IO ()
mockRun = putStr (runIdentity 
                    (artistRequest 
                        getArtistMock
                        getLimitMock 
                        requestMockS 
                        playlistToString))

ytIORun :: IO ()
ytIORun = fmap (const ()) (runMaybeT 
                            (artistRequest 
                                (lift getArtistIO) 
                                (lift getLimitIO) 
                                lastFmApiTopRequest 
                                (\a b -> lift (ytURLGen a b))))

printIORun :: IO ()
printIORun = fmap (const ()) (runMaybeT 
                                (artistRequest 
                                    (lift getArtistIO) 
                                    (lift getLimitIO) 
                                    lastFmApiTopRequest 
                                    (\a b -> lift (printPlaylist a b))))

multiYtIORun :: IO ()
multiYtIORun = fmap (const ()) (runMaybeT 
                                (artistRequest 
                                    (lift getArtistIO) 
                                    (lift getLimitIO) 
                                    multiFmApiTopRequest 
                                    (\a b -> lift (ytURLGen a b))))