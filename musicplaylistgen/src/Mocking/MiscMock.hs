{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module MiscMock where

import API.APITypes
import Data.Functor.Identity



getArtistMock :: Identity String
getArtistMock = "Bones"

getLimitMock :: Identity String
getLimitMock = "3"

getCountryMock :: Identity String
getCountryMock = "Belgium"

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
                where SongPlays sname splays = sph


playlistToStringC :: Country -> CSongList -> Identity String
-- playlistToStringC artist Nothing = "api request/decoding failed for artist: "
playlistToStringC _ (CSongList []) = ""
playlistToStringC artist (CSongList (sph:spt)) = Identity
        (artist ++ " - " ++ sname ++ " - " ++ "Plays:" ++ show splays ++ "\n" ++
        runIdentity (playlistToStringC artist (CSongList spt)))
                where CSongPlays _ sname splays = sph