module Forms where
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS

ui :: IO ()
ui = do
 window <- windowNew
           (Size (Width 200) (Height 200))
           Nothing
           Nothing
 begin window
 t1' <- textEditorNew
        (Rectangle (Position (X 10) (Y 10)) (Size (Width 100) (Height 15)))
        (Just "please enter your artist")
 t2' <- textEditorNew
        (Rectangle (Position (X 10) (Y 10)) (Size (Width 100) (Height 15)))
        (Just "please enter the playlist length")
 r1' <- radioLightButtonNew
        (Rectangle (Position (X 10) (Y 30)) (Size (Width 100) (Height 15)))
        (Just "Scrobbles")
 r2' <- radioLightButtonNew
        (Rectangle (Position (X 10) (Y 50)) (Size (Width 100) (Height 15)))
        (Just "Concerts")
 b' <- buttonNew
        (Rectangle (Position (X 10) (Y 50)) (Size (Width 100) (Height 15)))
        (Just "Generate!")
 setLabelsize b' (FontSize 10)
 setLabelsize t1' (FontSize 10)
 setLabelsize t2' (FontSize 10)
 setLabelsize r1' (FontSize 10)
 setLabelsize r2' (FontSize 10)
 setCallback b' buttonCb
 end window
 showWidget window