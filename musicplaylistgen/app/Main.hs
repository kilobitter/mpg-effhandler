module Main where

import Lib
import EffArtistScrobblesPlaylist

main :: IO ()
main = testH2 >>= putStrLn
