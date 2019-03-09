{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module API.MiscIO where

import API.APITypes
    
getArtistIO :: IO Artist
getArtistIO = do
  putStrLn "Input desired artist"
  getLine

getLimitIO :: IO Limit
getLimitIO = do
  putStrLn "Input desired limit"
  getLine

getCountryIO :: IO Country
getCountryIO = do
  putStrLn "Input desired country"
  getLine




--topScrobble-playlist
printPlaylist :: Artist -> SongList -> IO ()
-- printPlaylist artist Nothing = putStrLn ("api request/decoding failed for artist: " ++ artist)
printPlaylist _ (SongList []) = putStr ""
printPlaylist artist (SongList (sph:spt)) = do
        putStrLn (artist ++ " - " ++ sname ++ " - " ++ "Plays:" ++ show splays) 
        printPlaylist artist (SongList spt)
                where SongPlays sname splays = sph


printPlaylistC :: Country -> CSongList -> IO ()
-- printPlaylistC artist Nothing = putStrLn ("api request/decoding failed for artist: " ++ artist)
printPlaylistC country (CSongList []) = putStr ("--- data for " ++ country ++ " ---")
printPlaylistC country (CSongList (sph:spt)) = do
        putStrLn (aname ++ " - " ++ sname ++ " - " ++ "Plays:" ++ show splays) 
        printPlaylistC country (CSongList spt)
                where CSongPlays aname sname splays = sph

  
printPlaylistSL :: Artist -> SLSongList -> IO ()
-- printPlaylistSL artist Nothing = putStrLn ("api request/decoding failed for artist: " ++ artist)
printPlaylistSL _ [] = putStr ""
printPlaylistSL artist (sph:spt) = do
        putStrLn (artist ++ " - " ++ show sph) 
        printPlaylistSL artist spt