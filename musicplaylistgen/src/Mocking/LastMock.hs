{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Mocking.LastMock where

import API.APITypes
import Data.Functor.Identity
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe

requestMockS :: Artist -> Limit -> Identity SongList
requestMockS art lim
    = Identity $ SongList (generateSongList art (read lim))

requestMockC :: Country -> Limit -> Identity CSongList
requestMockC country lim
    | country == "Belgium" && lim == "3" = Identity (CSongList [CSongPlays "Bones" "Dirt" 25, CSongPlays "Bones" "hdmi"  18, CSongPlays "Bones" "Corduroy" 7])
    | otherwise = error "Called with wrong arguments"

generateSongList :: Artist -> Int -> [SongPlays]
generateSongList a 0 = [SongPlays "last one" 0]
generateSongList a l = SongPlays (replicate l 'a') l : generateSongList a (l - 1)