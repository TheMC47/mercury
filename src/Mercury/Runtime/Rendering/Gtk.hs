{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Mercury.Runtime.Rendering.Gtk (GtkBackend (..)) where

import Control.Monad
import Data.Foldable (traverse_)
import Data.GI.Base
import Data.GI.Base.Overloading (IsDescendantOf)
import qualified GI.Gtk as Gtk
import Mercury.Runtime
import Mercury.Runtime.Rendering.Backend
import Mercury.Window
import UnliftIO

data GtkBackend = GtkBackend

instance RenderingBackend GtkBackend where
    type Widget GtkBackend = Gtk.Widget
    type Box GtkBackend = Gtk.Box
    type Label GtkBackend = Gtk.Label
    type Button GtkBackend = Gtk.Button
    type Window GtkBackend = Gtk.Window
    type Application GtkBackend = Gtk.Application

    renderBox homogeneous ws = do
        box <- new Gtk.Box [#homogeneous := homogeneous]
        traverse_ (#append box) ws
        return box
    renderLabel str = new Gtk.Label [#label := str]
    renderButton w = new Gtk.Button [#child := w]
    setLabelText = #setLabel
    labelToWidget = Gtk.toWidget
    boxToWidget = Gtk.toWidget
    buttonToWidget = Gtk.toWidget

    createApplication appId = new Gtk.Application [#applicationId := appId]
    onApplicationActivate app action =
        toIO action >>= void . Gtk.on app #activate

    runApplication app = void $ #run app Nothing

    renderWindow app w geom t = do
        win <- new Gtk.Window [#child := w, #title := t, #application := app]
        #setDefaultSize win (maybe (-1) fromIntegral (width geom)) (maybe (-1) fromIntegral (height geom))
        #present win
        return win
