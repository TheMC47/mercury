{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Mercury.Runtime.Rendering.Backend (
    RenderingBackend (..),
    RenderedLabel (..),
    RenderedButton (..),
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

    renderBox :: (MonadIO m) => Bool -> [Widget b] -> m (Widget b)
    renderLabel :: (MonadIO m) => Text -> m (RenderedLabel b)
    renderButton :: (MonadUnliftIO m) => Widget b -> m () -> m (RenderedButton b)

    createWindow :: (MonadUnliftIO m) => BackendHandle b -> Widget b -> Geometry -> Text -> m (Window b)

data RenderedLabel b = RenderedLabel
    { labelWidget :: !(Widget b)
    , setText :: forall m. (MonadUnliftIO m) => Text -> m ()
    }

data RenderedButton b = RenderedButton
    { buttonWidget :: !(Widget b)
    , setClass :: forall m. (MonadUnliftIO m) => [Text] -> m ()
    }
