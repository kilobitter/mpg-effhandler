{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Mocking.MiscMock where

import API.APITypes
import Data.Functor.Identity

getArtLimMock :: String -> String -> Identity (Artist, Limit)
getArtLimMock sa sl = Identity (sa, sl)

getArtistMock :: String -> Identity Artist
getArtistMock = Identity

getLimitMock :: String -> Identity Limit
getLimitMock = Identity

getCountryMock :: String -> Identity String
getCountryMock = Identity

-- setlist-playlist
playlistToStringSL :: Artist -> SLSongList -> Identity String
-- playlistToStringSL artist Nothing = "api request/decoding failed for artist: "
playlistToStringSL _ [] = ""
playlistToStringSL artist (sph:spt) = Identity
        (artist ++ " - " ++ show sph ++
        runIdentity (playlistToStringSL artist spt))

-- topscrobble-playlist
playlistToString :: Artist -> SongList -> Identity String
-- playlistToString artist Nothing = "api request/decoding failed for artist: "
playlistToString _ (SongList []) = ""
playlistToString artist (SongList (sph:spt)) = Identity
        (artist ++ " - " ++ sname ++ " - " ++ "Plays:" ++ show splays ++ "\n" ++
        runIdentity (playlistToString artist (SongList spt)))
                where SongPlays sname splays artist = sph


playlistToStringC :: Country -> CSongList -> Identity String
-- playlistToStringC artist Nothing = "api request/decoding failed for artist: "
playlistToStringC _ (CSongList []) = ""
playlistToStringC artist (CSongList (sph:spt)) = Identity
        (artist ++ " - " ++ sname ++ " - " ++ "Plays:" ++ show splays ++ "\n" ++
        runIdentity (playlistToStringC artist (CSongList spt)))
                where CSongPlays _ sname splays = sph