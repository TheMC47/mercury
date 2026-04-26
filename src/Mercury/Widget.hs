{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Mercury.Widget (
    Action (..),
    module Mercury.Expression,
    Widget (..),
    Box (..),
    Label (..),
    Button (..),
    CenterBox (..),
    ToWidget (..),
    (=:),
    box,
    label,
    button,
    centerBox,
    classes,
    child,
    onClick,
    children,
    childLeft,
    childRight,
    childCenter,
    text,
    spaceEvenly,
    orientation,
    halign,
    valign,
    spacing,
    getAllVariables,
) where

import Data.Kind (Type)
import Data.Set qualified as S
import Data.Text (Text)
import GHC.Generics
import Mercury.Expression
import Mercury.Runtime (MercuryRuntime)
import Mercury.Runtime.Rendering.Backend (RenderingBackend)
import Mercury.Variable (Variable)
import Mercury.Window.Geometry (Alignment, Orientation)

data Action = Action {runAction :: forall (b :: Type). (RenderingBackend b) => MercuryRuntime b ()}

class HasVariables a where
    getAllVariables :: a -> S.Set Variable
    default getAllVariables :: (Generic a, GHasVariables (Rep a)) => a -> S.Set Variable
    getAllVariables x = ggetVariables (from x)

instance HasVariables (Expression a) where
    getAllVariables = dependencies

instance HasVariables Text where getAllVariables _ = mempty
instance HasVariables Action where getAllVariables _ = mempty
instance (HasVariables a) => HasVariables [a] where getAllVariables = foldMap getAllVariables

instance (HasVariables a) => HasVariables (Maybe a) where
    getAllVariables Nothing = mempty
    getAllVariables (Just a) = getAllVariables a

class GHasVariables f where
    ggetVariables :: f p -> S.Set Variable

instance GHasVariables U1 where
    ggetVariables U1 = mempty

instance (GHasVariables a, GHasVariables b) => GHasVariables (a :*: b) where
    ggetVariables (a :*: b) = ggetVariables a <> ggetVariables b
instance (GHasVariables a, GHasVariables b) => GHasVariables (a :+: b) where
    ggetVariables (L1 x) = ggetVariables x
    ggetVariables (R1 x) = ggetVariables x

instance (GHasVariables a) => GHasVariables (M1 i c a) where
    ggetVariables (M1 x) = ggetVariables x

instance (HasVariables a) => GHasVariables (K1 i a) where
    ggetVariables (K1 x) = getAllVariables x

data Box = WBox
    { box_spaceEvenly :: !(Maybe (Expression Bool))
    , box_children :: ![Widget]
    , box_class :: !(Maybe (Expression [Text]))
    , box_orientation :: !(Maybe (Expression Orientation))
    , box_halign :: !(Maybe (Expression Alignment))
    , box_valign :: !(Maybe (Expression Alignment))
    , box_spacing :: !(Maybe (Expression Int))
    }
    deriving (Generic)

instance HasVariables Box

data Label = WLabel
    { label_text :: !(Expression Text)
    , label_class :: !(Maybe (Expression [Text]))
    }
    deriving (Generic)
instance HasVariables Label

data Button = WButton
    { button_child :: !(Maybe Widget)
    , button_onClick :: !(Maybe Action)
    , button_class :: !(Maybe (Expression [Text]))
    }
    deriving (Generic)
instance HasVariables Button

data CenterBox = WBCenterBox
    { centerbox_childLeft :: !(Maybe Widget)
    , centerbox_childRight :: !(Maybe Widget)
    , centerbox_childCenter :: !(Maybe Widget)
    , centerbox_class :: !(Maybe (Expression [Text]))
    , centerbox_orientation :: !(Maybe (Expression Orientation))
    }
    deriving (Generic)
instance HasVariables CenterBox

data Widget
    = AnyBox !Box
    | AnyLabel !Label
    | AnyButton !Button
    | AnyCenterBox !CenterBox
    deriving (Generic)
instance HasVariables Widget

button :: Button
button = WButton Nothing Nothing Nothing

label :: Label
label = WLabel (pure "") Nothing

box :: Box
box =
    WBox
        { box_spaceEvenly = Nothing
        , box_children = []
        , box_class = Nothing
        , box_orientation = Nothing
        , box_halign = Nothing
        , box_valign = Nothing
        , box_spacing = Nothing
        }

centerBox :: CenterBox
centerBox =
    WBCenterBox
        { centerbox_childLeft = Nothing
        , centerbox_childRight = Nothing
        , centerbox_childCenter = Nothing
        , centerbox_class = Nothing
        , centerbox_orientation = Nothing
        }

class Into a b where
    into :: a -> b

instance Into a a where into = id
instance Into a (Expression a) where into = pure

instance Into (Expression a) (Maybe (Expression a)) where into = Just
instance Into a (Maybe (Expression a)) where into = Just . into
instance Into Action (Maybe Action) where into = Just
instance Into a [a] where into a = [a]

class ToWidget a where
    w :: a -> Widget

instance ToWidget Box where w = AnyBox
instance ToWidget Label where w = AnyLabel
instance ToWidget Button where w = AnyButton
instance ToWidget CenterBox where w = AnyCenterBox
instance ToWidget Widget where w = id

instance (ToWidget a) => Into a Widget where into = w
instance (ToWidget a) => Into a (Maybe Widget) where into = Just . w

type Setter s a = a -> s -> s

-- Classes
class HasClasses s where classes :: Setter s (Maybe (Expression [Text]))
instance HasClasses Box where classes c b = b{box_class = c}
instance HasClasses Button where classes c b = b{button_class = c}
instance HasClasses Label where classes c b = b{label_class = c}
instance HasClasses CenterBox where classes c b = b{centerbox_class = c}

-- Space evenly
class HasSpaceEvenly s where spaceEvenly :: Setter s (Maybe (Expression Bool))
instance HasSpaceEvenly Box where spaceEvenly c b = b{box_spaceEvenly = c}

-- Text
class HasText s where text :: Setter s (Expression Text)
instance HasText Label where text t l = l{label_text = t}

-- Child
class HasChild s where child :: Setter s (Maybe Widget)
instance HasChild Button where child c b = b{button_child = c}

-- OnClick
class HasOnClick s where onClick :: Setter s (Maybe Action)
instance HasOnClick Button where onClick c b = b{button_onClick = c}

-- Children
class HasChildren s where children :: Setter s [Widget]
instance HasChildren Box where children c b = b{box_children = c}

-- Children for center box
class HasCenterBoxChildren s where
    childLeft :: Setter s (Maybe Widget)
    childRight :: Setter s (Maybe Widget)
    childCenter :: Setter s (Maybe Widget)

instance HasCenterBoxChildren CenterBox where
    childLeft c b = b{centerbox_childLeft = c}
    childRight c b = b{centerbox_childRight = c}
    childCenter c b = b{centerbox_childCenter = c}

-- Orientation
class HasOrientation s where orientation :: Setter s (Maybe (Expression Orientation))
instance HasOrientation Box where orientation o b = b{box_orientation = o}
instance HasOrientation CenterBox where orientation o b = b{centerbox_orientation = o}

-- Alignment
class HasHAlign s where halign :: Setter s (Maybe (Expression Alignment))
instance HasHAlign Box where halign a b = b{box_halign = a}

class HasVAlign s where valign :: Setter s (Maybe (Expression Alignment))
instance HasVAlign Box where valign a b = b{box_valign = a}

-- Spacing
class HasSpacing s where spacing :: Setter s (Maybe (Expression Int))
instance HasSpacing Box where spacing s b = b{box_spacing = s}

(=:) :: (Into v a) => Setter s a -> v -> s -> s
s =: v = s (into v)
infixl 0 =:
