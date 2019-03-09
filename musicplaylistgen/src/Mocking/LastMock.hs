{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module LastMock where

import APITypes
import Data.Functor.Identity

requestMockS :: Artist -> Limit -> Identity SongList
requestMockS art lim
    | art == "Bones" && lim == "3" = Identity (SongList [SongPlays "Dirt" 25, SongPlays "hdmi"  18, SongPlays "Corduroy" 7])
    | otherwise = error "Called with wrong arguments"

requestMockC :: Country -> Limit -> Identity CSongList
requestMockC country lim
    | country == "Belgium" && lim == "3" = Identity (CSongList [CSongPlays "Bones" "Dirt" 25, CSongPlays "Bones" "hdmi"  18, CSongPlays "Bones" "Corduroy" 7])
    | otherwise = error "Called with wrong arguments"