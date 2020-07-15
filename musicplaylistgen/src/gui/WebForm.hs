module GUI.WebForm where

import Control.Monad
import Data.IORef

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Control.Concurrent.MVar
import Control.Concurrent
import Web.Browser
import API.APITypes

-- web helper function
getArtLimWeb :: IO (Artist, Limit)
getArtLimWeb = do
  alpair <- newEmptyMVar
  forkIO $ callGUI alpair
  openBrowser "http://localhost:8023"
  takeMVar alpair

callGUI :: MVar (Artist, Limit) -> IO ()
callGUI md = 
    startGUI defaultConfig { jsStatic = Just "static" } (setup md)

setup :: MVar (Artist, Limit) -> Window -> UI ()
setup md w = void $ do
    return w # set title "MPG-FORM"
--  UI.addStyleSheet w "form.css"

    elements <- mkWindow md
    getBody w #+
        [UI.div #. "wrap" #+ (greet ++ map element elements)]
    return ()

greet :: [UI Element]
greet =
    [ UI.h1  #+ [string "Music Playlist Generator"]
    , UI.div #+ [string "Please enter your desired artist"]
    ]


mkForm :: String -> UI (Element, Element, Element, Element)
mkForm title = do
    button <- UI.button #. "button" #+ [string title]
    artistI <- UI.input
    limitPrompt <- UI.div #+ [string "Please enter the playlist length"]
    limitI <- UI.input
    view   <- UI.p #+ [element artistI, element limitPrompt, element limitI, element button]
    return (button, artistI, limitI, view)

mkWindow :: MVar (Artist, Limit) -> UI [Element]
mkWindow md = do
    list    <- UI.ul #. "artistForm"
    
    (button1, artist1, limit1, view1) <- mkForm button1Title
    on UI.click button1 $ \_ -> do
        artistName <- get value artist1
        limitNum <- get value limit1
        liftIO $ putMVar md (artistName, limitNum)
    return [list, view1]

  where button1Title = "Submit Query"