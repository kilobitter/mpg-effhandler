{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module API.APITypes where


import           Data.Aeson
import           Data.Aeson.Types
import           Data.Functor.Identity
import qualified Data.Text as T
import           GHC.Generics
import           Data.ByteString.Char8 (pack)
import           Data.Time
import           Data.Time.Format
import           Data.Maybe

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Base


-- LastFM-specific
-- TopScrobbles
data SongPlays =  SongPlays {name :: String, playcount :: Int} deriving Show
newtype SongList = SongList {list :: [SongPlays] } deriving Show

type Artist = String
type Limit = String
type Country = String

instance FromJSON SongList where
    parseJSON = withObject "SongList" $ \o -> do
            topSongsO <- o .: "toptracks"
            songList  <- topSongsO .: "track"
            return $ SongList songList

instance FromJSON SongPlays where
        parseJSON = withObject "SongPlays" $ \o -> do
          nname <- o .: "name"
          nplaycount  <- o .: "playcount"
          return $ SongPlays nname (read nplaycount :: Int)

-- TopGeo

data CSongPlays =  CSongPlays {cartistName :: String, cname :: String, cplaycount :: Int} deriving Show
newtype CSongList = CSongList {clist :: [CSongPlays] } deriving Show

instance FromJSON CSongList where
    parseJSON = withObject "CSongList" $ \o -> do
            topSongsO <- o .: "tracks"
            songList <- topSongsO .: "track"
            return $ CSongList songList

instance FromJSON CSongPlays where
        parseJSON = withObject "CSongPlays" $ \o -> do
          artistO <- o .: "artist"
          aname <- artistO .: "name"
          sname <- o .: "name"
          nplaycount  <- o .: "listeners"
          return $ CSongPlays aname sname (read nplaycount :: Int)

    
-- Setlist-specific
newtype SetListList = SetListList {sllist :: [SetList] } deriving Show
data SetList = SetList {setartist :: String, setDate :: Day, setlist :: [Song]} deriving Show
newtype Song = Song {songname :: String} deriving Show
type SLSongList = [Song]

instance FromJSON SetListList where
  parseJSON = withObject "SetListList" $ \o -> do
          slists <- o .: "setlist"
          return $ SetListList slists

instance FromJSON SetList where
  parseJSON = withObject "SetList" $ \o -> do
          sDate <- o .: "eventDate"
          artistO     <- o .: "artist"
          saname <- artistO .: "name"
          setO <- o .: "sets"
          setlist <- setO .: "set"
          let parsedDate = parseTimeM True defaultTimeLocale "%d-%m-%Y" sDate :: Maybe Day
          return $ SetList saname (fromJust parsedDate) setlist

instance FromJSON Song where
  parseJSON = withObject "Song" $ \o -> do
          [song] <- o .:  "song"
          sname <- song .:? "name" .!= "--empty--"
          return $ Song sname

-- YT-specific
data YTLink = YTLink {vname :: String, vid :: String} deriving Show

instance FromJSON YTLink where
    parseJSON = withObject "YTLink" $ \o -> do
      [item] <- o .: "items"
      idO <- item .: "id"
      vidId <- idO .: "videoId"
      snippetO <- item .: "snippet"
      vidName <- snippetO .: "title"
      return (YTLink vidName vidId)

-- MB-specific
data MBArtist = MBArtist {artname :: String, artId :: String} deriving Show

instance FromJSON MBArtist where 
    parseJSON = withObject "MBArtist" $ \o -> do
            [artistO] <- o .: "artists"
            idA     <- artistO .: "id"
            name <- artistO .: "name"
            return $ MBArtist name idA

-- Spotify-specific

data SPTrackId = SPTrackId {songName :: String, songId :: String} deriving Show
newtype SPUserId = SPUserId {usid :: String}
newtype SPPlaylistId = SPPlaylistId {pl :: String}

instance FromJSON SPTrackId where
    parseJSON = withObject "SPTrackId" $ \o -> do
      tracks <- o .: "tracks"
      [item] <- tracks .: "items"
      sid <- item .: "id"
      sname <- item .: "name"
      return (SPTrackId sname sid)

instance FromJSON SPUserId where
    parseJSON = withObject "SPUserId" $ \o -> do
      sid <- o .: "id"
      return (SPUserId sid)

instance FromJSON SPPlaylistId where
    parseJSON = withObject "SPPlaylistId" $ \o -> do
      sid <- o .: "id"
      return (SPPlaylistId sid)


-- Parsing-specific

verboseParser :: Value -> Parser (Either String SongList)
verboseParser v =
  case parseEither parseJSON v of
    Left err -> return . Left $ err ++ " -- Invalid object is: " ++ show v
    Right parsed -> return $ Right parsed