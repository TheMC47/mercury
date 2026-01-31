module Mercury.Window (Window (..)) where

import Data.Default
import Data.Text (Text)
import Mercury.Widget (Widget)
import Mercury.Window.Geometry (Geometry)

data Window = Window
    { rootWidget :: !Widget
    , geometry :: !Geometry
    , title :: !Text
    }
