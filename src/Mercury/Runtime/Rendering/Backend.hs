{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Mercury.Runtime.Rendering.Backend (RenderingBackend (..)) where

import Control.Monad.IO.Class
import Data.Text
import Mercury.Window (Geometry)
import UnliftIO

class RenderingBackend b where
    type Widget b
    type Box b
    type Label b
    type Button b
    type Window b
    type Application b

    renderBox :: (MonadIO m) => Bool -> [Widget b] -> m (Box b)
    renderLabel :: (MonadIO m) => Text -> m (Label b)
    renderButton :: (MonadIO m) => Widget b -> m (Button b)
    renderWindow :: (MonadIO m) => Application b -> Widget b -> Geometry -> Text -> m (Window b)

    setLabelText :: (MonadIO m) => Label b -> Text -> m ()

    createApplication :: (MonadIO m) => Text -> m (Application b)
    onApplicationActivate :: (MonadUnliftIO m) => Application b -> m () -> m ()
    runApplication :: (MonadIO m) => Application b -> m ()

    labelToWidget :: (MonadIO m) => Label b -> m (Widget b)
    boxToWidget :: (MonadIO m) => Box b -> m (Widget b)
    buttonToWidget :: (MonadIO m) => Button b -> m (Widget b)
