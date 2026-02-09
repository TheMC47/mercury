{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Mercury.Runtime.Rendering.Backend (
    RenderingBackend (..),
    RenderedLabel (..),
    RenderedButton (..),
    RenderedBox (..),
    RenderBoxProps (..),
    RenderLabelProps (..),
    RenderButtonProps (..),
) where

import Control.Monad.IO.Class
import Data.Kind (Type)
import Data.Text
import Mercury.Window.Geometry (Geometry)
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

    createWindow :: (MonadUnliftIO m) => BackendHandle b -> Widget b -> Geometry -> Text -> m (Window b)

data RenderBoxProps b = RenderBoxProps
    { renderBox_spaceEvenly :: !(Maybe Bool)
    , renderBox_children :: ![Widget b]
    , renderBox_class :: !(Maybe [Text])
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
    }
