module Mercury.Window where

import Data.Default (Default (def))
import Data.Text (Text)
import Mercury.Widget (Widget)

data Window = Window
    { rootWidget :: !Widget
    , geometry :: !Geometry
    , title :: !Text
    }

data Geometry = Geometry
    { width :: !(Maybe Int)
    , height :: !(Maybe Int)
    , x :: !(Maybe Int)
    , y :: !(Maybe Int)
    , anchor :: !Anchor
    }

instance Default Geometry where
    def =
        Geometry
            { width = Nothing
            , height = Nothing
            , x = Nothing
            , y = Nothing
            , anchor = TopCenter
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
