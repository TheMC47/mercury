module Mercury.Window.Geometry (
    Position,
    Geometry (..),
    Anchor (..),
    Stacking (..),
    Dimension (..),
    concretizeDimension,
    Strut (..),
    Direction (..),
    Orientation (..),
) where

import Data.Default (Default (..))

data Dimension
    = Absolute !Int
    | Percentage !Int
    deriving (Eq, Show)

concretizeDimension :: (Integral a) => a -> Dimension -> a
concretizeDimension _ (Absolute x) = fromIntegral x
concretizeDimension l (Percentage x) = (l * fromIntegral x) `div` 100

data Orientation = H | V

data Direction = L | R | T | B

data Strut = Strut !Direction !Dimension

-- type Position = (Dimension, Dimension)
type Position = (Int, Int)

data Geometry = Geometry
    { width :: !(Maybe Dimension)
    , height :: !(Maybe Dimension)
    , position :: !Position
    , anchor :: !Anchor
    , screen :: !Int
    , strut :: !(Maybe Strut)
    }

instance Default Geometry where
    def =
        Geometry
            { width = Nothing
            , height = Nothing
            , position = (0, 0)
            , anchor = TopCenter
            , screen = 0
            , strut = Nothing
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
