{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
    getAllVariables,
    HasClasses (..),
    HasSpaceEvenly (..),
    HasText (..),
    HasChild (..),
    HasOnClick (..),
    HasChildren (..),
    HasChildLeft (..),
    HasChildRight (..),
    HasChildCenter (..),
    HasOrientation (..),
    HasHalign (..),
    HasValign (..),
    HasSpacing (..),
) where

import Control.Lens (ASetter, makeFields, (.~))
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
    { _boxSpaceEvenly :: !(Maybe (Expression Bool))
    , _boxChildren :: ![Widget]
    , _boxClasses :: !(Maybe (Expression [Text]))
    , _boxOrientation :: !(Maybe (Expression Orientation))
    , _boxHalign :: !(Maybe (Expression Alignment))
    , _boxValign :: !(Maybe (Expression Alignment))
    , _boxSpacing :: !(Maybe (Expression Int))
    }
    deriving (Generic)
instance HasVariables Box

data Label = WLabel
    { _labelText :: !(Expression Text)
    , _labelClasses :: !(Maybe (Expression [Text]))
    }
    deriving (Generic)
instance HasVariables Label

data Button = WButton
    { _buttonChild :: !(Maybe Widget)
    , _buttonOnClick :: !(Maybe Action)
    , _buttonClasses :: !(Maybe (Expression [Text]))
    }
    deriving (Generic)
instance HasVariables Button

data CenterBox = WBCenterBox
    { _centerBoxChildLeft :: !(Maybe Widget)
    , _centerBoxChildRight :: !(Maybe Widget)
    , _centerBoxChildCenter :: !(Maybe Widget)
    , _centerBoxClasses :: !(Maybe (Expression [Text]))
    , _centerBoxOrientation :: !(Maybe (Expression Orientation))
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

makeFields ''Box
makeFields ''Label
makeFields ''Button
makeFields ''CenterBox

button :: Button
button = WButton Nothing Nothing Nothing

label :: Label
label = WLabel (pure "") Nothing

box :: Box
box =
    WBox
        { _boxSpaceEvenly = Nothing
        , _boxChildren = []
        , _boxClasses = Nothing
        , _boxOrientation = Nothing
        , _boxHalign = Nothing
        , _boxValign = Nothing
        , _boxSpacing = Nothing
        }

centerBox :: CenterBox
centerBox =
    WBCenterBox
        { _centerBoxChildLeft = Nothing
        , _centerBoxChildRight = Nothing
        , _centerBoxChildCenter = Nothing
        , _centerBoxClasses = Nothing
        , _centerBoxOrientation = Nothing
        }

class Into a b where
    into :: a -> b

instance {-# INCOHERENT #-} Into (Expression a) (Expression a) where into = id
instance Into a (Expression a) where into = pure

instance Into (Expression a) (Maybe (Expression a)) where into = Just
instance Into a (Maybe (Expression a)) where into = Just . into
instance Into Action (Maybe Action) where into = Just
instance Into a [a] where into a = [a]
instance Into [a] [a] where into = id

class ToWidget a where
    w :: a -> Widget

instance ToWidget Box where w = AnyBox
instance ToWidget Label where w = AnyLabel
instance ToWidget Button where w = AnyButton
instance ToWidget CenterBox where w = AnyCenterBox
instance ToWidget Widget where w = id

instance (ToWidget a) => Into a Widget where into = w
instance (ToWidget a) => Into a (Maybe Widget) where into = Just . w

(=:) :: (Into v a) => ASetter s s a a -> v -> s -> s
l =: v = l .~ into v
infixl 0 =:
