{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Mercury.Widget (
    Widget' (..),
    Action (..),
    module Mercury.Expression,
    Widget (..),
    (=:),
    Box,
    box,
    Label,
    label,
    Button,
    button,
    classes,
    child,
    onClick,
    children,
    text,
    spaceEvenly,
    w,
    getAllVariables,
) where

import Data.Kind (Type)
import Data.Set qualified as S
import Data.Text (Text)
import Mercury.Expression
import Mercury.Runtime (MercuryRuntime)
import Mercury.Runtime.Rendering.Backend (RenderingBackend)
import Mercury.Variable (Variable)

data Action = Action {runAction :: forall (b :: Type). (RenderingBackend b) => MercuryRuntime b ()}

data WidgetType = KBox | KLabel | KButton

data Widget' (k :: WidgetType) where
    WBox ::
        { box_spaceEvenly :: !(Maybe (Expression Bool))
        , box_children :: ![Widget]
        , box_class :: !(Maybe (Expression [Text]))
        } ->
        Widget' 'KBox
    WLabel ::
        { label_text :: !(Expression Text)
        , label_class :: !(Maybe (Expression [Text]))
        } ->
        Widget' 'KLabel
    WButton ::
        { button_child :: !(Maybe Widget)
        , button_onClick :: !(Maybe Action)
        , button_class :: !(Maybe (Expression [Text]))
        } ->
        Widget' 'KButton

data Widget where
    AnyWidget :: Widget' k -> Widget

getAllVariables :: Widget -> S.Set Variable
getAllVariables (AnyWidget widget) = case widget of
    WBox{..} ->
        maybe mempty dependencies box_spaceEvenly
            <> maybe mempty dependencies box_class
            <> foldMap getAllVariables box_children
    WLabel{..} ->
        maybe mempty dependencies label_class
            <> dependencies label_text
    WButton{..} ->
        maybe mempty dependencies button_class
            <> maybe mempty getAllVariables button_child

type Box = Widget' 'KBox
type Label = Widget' 'KLabel
type Button = Widget' 'KButton

button :: Button
button = WButton Nothing Nothing Nothing

label :: Label
label = WLabel (pure "") Nothing

box :: Box
box = WBox Nothing [] Nothing

w :: Widget' k -> Widget
w = AnyWidget

class Into a b where
    into :: a -> b

instance Into a a where into = id
instance Into a (Expression a) where into = pure

instance Into (Expression a) (Maybe (Expression a)) where into = Just
instance Into a (Maybe (Expression a)) where into = Just . into
instance Into (Widget' k) Widget where into = AnyWidget
instance Into Widget (Maybe Widget) where into = Just
instance Into (Widget' k) (Maybe Widget) where into = Just . AnyWidget
instance Into Action (Maybe Action) where into = Just
instance Into a [a] where into a = [a]

type Setter s a = a -> s -> s

-- Classes
class HasClasses s where classes :: Setter s (Maybe (Expression [Text]))
instance HasClasses Box where classes c b = b{box_class = c}
instance HasClasses Button where classes c b = b{button_class = c}
instance HasClasses Label where classes c b = b{label_class = c}

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

(=:) :: (Into v a) => Setter s a -> v -> s -> s
s =: v = s (into v)
infixl 0 =:
