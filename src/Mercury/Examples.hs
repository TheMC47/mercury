{-# LANGUAGE OverloadedStrings #-}

module Mercury.Examples where

import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Mercury.Prelude
import Mercury.Variable.Typed
import Mercury.Widget
import Mercury.Window.Geometry
import System.Process
import UnliftIO.Concurrent

workspacesWidget :: Widget
workspacesWidget =
    let
        workspace :: Int -> Widget
        workspace n =
            w $
                button
                    & (child =: w (label & (text =: tshow (n + 1))))
                    & (onClick =: Action (liftIO $ void $ forkIO $ callCommand ("wmctrl -s " <> show n)))
     in
        w $
            box
                & (orientation =: H)
                & (spaceEvenly =: True)
                & (children =: map workspace [0 .. 9])

musicWidget :: Widget
musicWidget = w $ label & (text =: ("Music" :: Text))

ewwBar :: Widget
ewwBar =
    w $
        centerBox
            & (orientation =: H)
            & (childLeft =: workspacesWidget)
            & (childCenter =: musicWidget)
            & (childRight =: sidestuffWidget)

sidestuffWidget :: Widget
sidestuffWidget = w $ label & (text =: (#) timestampVar)

(.:) = (.) . (.)

timestampVar :: TypedVariable Text
timestampVar =
    pollingVar
        "current_time"
        1000
        (((T.pack . formatTime defaultTimeLocale "%H:%M %b %d, %Y") .: utcToLocalTime) <$> getCurrentTimeZone <*> getCurrentTime)
