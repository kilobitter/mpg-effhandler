{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module LastMock where

import APITypes
import Data.Functor.Identity

requestMock :: Artist -> Limit -> Identity SongList
requestMock art lim
    | art == "Bones" && lim == "3" = Identity (SongList [SongPlays "Dirt" 25, SongPlays "hdmi"  18, SongPlays "Corduroy" 7])
    | otherwise = error "Called with wrong arguments"