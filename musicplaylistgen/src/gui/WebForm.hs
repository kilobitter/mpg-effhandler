module WebForm where

import Control.Monad
import Data.IORef

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

-- main is natuurlijk onnodig in het programma
main :: IO ()
main = 
    startGUI defaultConfig { jsStatic = Just "static" } setup

setup :: Window -> UI (IORef String, IORef String)
setup w = void $ do
    return w # set title "MPG-FORM"
--  UI.addStyleSheet w "form.css"

    (artist, limit, elements) <- mkWindow
    getBody w #+
        [UI.div #. "wrap" #+ (greet ++ map element elements)]
    return (artist, limit)

greet :: [UI Element]
greet =
    [ UI.h1  #+ [string "Music Playlist Generator"]
    , UI.div #+ [string "Please enter your desired artist"]
    ]


mkForm :: String -> UI (Element, Element, Element, Element)
mkForm title = do
    button <- UI.button #. "button" #+ [string title]
    artistI <- UI.input
    limitI <- UI.input
    view   <- UI.p #+ [element artistI, element limitI, element button]
    return (button, artistI, limitI, view)

mkWindow :: UI (IORef String, IORef String, [Element])
mkWindow = do
    list    <- UI.ul #. "artistForm"
    
    (button1, artist1, limit1, view1) <- mkForm button1Title
    
    artist <- liftIO $ newIORef ""
    limit <- liftIO $ newIORef ""
    on UI.click button1 $ \_ -> do
        artistName <- get value artist1
        limitNum <- get value limit1
        liftIO $ writeIORef artist artistName
        liftIO $ writeIORef limit limitNum
--      Hier zou dus de aanroep moeten zijn naar de query, maar dan zou ik de GUI moeten sluiten ofzo
--      anders ga ik niet terug naar de control flow met de handlers, en dat vernietigt het nut best wel
    return (artist, limit, [list, view1])

  where button1Title = "Submit Query"