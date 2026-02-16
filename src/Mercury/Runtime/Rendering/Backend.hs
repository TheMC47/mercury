{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Mercury.Runtime.Rendering.Backend (
    RenderingBackend (..),
    RenderedLabel (..),
    RenderedButton (..),
    RenderedBox (..),
    RenderedCenterBox (..),
    RenderBoxProps (..),
    RenderLabelProps (..),
    RenderButtonProps (..),
    RenderCenterBoxProps (..),
) where

import Control.Monad.IO.Class
import Data.Kind (Type)
import Data.Text
import Mercury.Window.Geometry (Alignment, Geometry, Orientation)
import UnliftIO

class RenderingBackend (b :: Type) where
    data Widget b
    data Window b
    data BackendHandle b
    data ExtraBackendData b

    withBackend :: (MonadUnliftIO m) => ExtraBackendData b -> (BackendHandle b -> m ()) -> m ()
    shutdown :: (MonadIO m) => BackendHandle b -> m ()
    withHotReload :: (MonadUnliftIO m) => ExtraBackendData b -> (BackendHandle b -> m ()) -> m ()

    renderBox :: (MonadIO m) => RenderBoxProps b -> m (RenderedBox b)
    renderLabel :: (MonadIO m) => RenderLabelProps -> m (RenderedLabel b)
    renderButton :: (MonadUnliftIO m) => RenderButtonProps m b -> m (RenderedButton b)
    renderCenterBox :: (MonadIO m) => RenderCenterBoxProps b -> m (RenderedCenterBox b)

    createWindow :: (MonadUnliftIO m) => BackendHandle b -> Widget b -> Geometry -> Text -> m (Window b)

data RenderBoxProps b = RenderBoxProps
    { renderBox_spaceEvenly :: !(Maybe Bool)
    , renderBox_children :: ![Widget b]
    , renderBox_class :: !(Maybe [Text])
    , renderBox_orientation :: !(Maybe Orientation)
    , renderBox_halign :: !(Maybe Alignment)
    , renderBox_valign :: !(Maybe Alignment)
    }

data RenderLabelProps = RenderLabelProps
    { renderLabel_text :: !Text
    , renderLabel_class :: !(Maybe [Text])
    }

data RenderButtonProps (m :: Type -> Type) b = RenderButtonProps
    { renderButton_child :: !(Maybe (Widget b))
    , renderButton_onClick :: !(Maybe (m ()))
    , renderButton_class :: !(Maybe [Text])
    }

data RenderCenterBoxProps b = RenderCenterBoxProps
    { renderCenterBox_childLeft :: !(Maybe (Widget b))
    , renderCenterBox_childCenter :: !(Maybe (Widget b))
    , renderCenterBox_childRight :: !(Maybe (Widget b))
    , renderCenterBox_class :: !(Maybe [Text])
    , renderCenterBox_orientation :: !(Maybe Orientation)
    }

data RenderedLabel b = RenderedLabel
    { label_widget :: !(Widget b)
    , label_setText :: forall m. (MonadUnliftIO m) => Text -> m ()
    , label_setClass :: forall m. (MonadUnliftIO m) => [Text] -> m ()
    }

data RenderedButton b = RenderedButton
    { button_widget :: !(Widget b)
    , button_setClass :: forall m. (MonadUnliftIO m) => [Text] -> m ()
    , button_setOnClick :: forall m. (MonadUnliftIO m) => m () -> m ()
    }

data RenderedBox b = RenderedBox
    { box_widget :: !(Widget b)
    , box_setSpaceEvenly :: forall m. (MonadUnliftIO m) => Bool -> m ()
    , box_setClass :: forall m. (MonadUnliftIO m) => [Text] -> m ()
    , box_setOrientation :: forall m. (MonadUnliftIO m) => Orientation -> m ()
    , box_halign :: forall m. (MonadUnliftIO m) => Alignment -> m ()
    , box_valign :: forall m. (MonadUnliftIO m) => Alignment -> m ()
    }

data RenderedCenterBox b = RenderedCenterBox
    { centerbox_widget :: !(Widget b)
    , centerbox_setClass :: forall m. (MonadUnliftIO m) => [Text] -> m ()
    , centerbox_setOrientation :: forall m. (MonadUnliftIO m) => Orientation -> m ()
    }
