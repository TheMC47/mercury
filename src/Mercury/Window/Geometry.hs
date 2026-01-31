module Mercury.Window.Geometry (
    Position,
    Geometry (..),
    Anchor (..),
    Stacking (..),
) where

import Data.Default (Default (..))

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
