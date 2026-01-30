module Mercury.Window where

import Data.Default (Default (def))
import Data.Text (Text)
import Mercury.Widget (Widget)

data Window m = Window
    { rootWidget :: !(Widget m)
    , geometry :: !Geometry
    , title :: !Text
    }

type Position = (Int, Int)

data Geometry = Geometry
    { width :: !(Maybe Int)
    , height :: !(Maybe Int)
    , position :: !Position
    , anchor :: !Anchor
    , screen :: !Int
    }

instance Default Geometry where
    def =
        Geometry
            { width = Nothing
            , height = Nothing
            , position = (0, 0)
            , anchor = TopCenter
            , screen = 0
            }

data Anchor
    = TopLeft
    | TopCenter
    | TopRight
    | CenterLeft
    | Center
    | CenterRight
    | BottomLeft
    | BottomCenter
    | BottomRight
    deriving (Eq, Show)

data Stacking = Above | Below
    deriving (Eq, Show)
