{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Mercury.Runtime.Rendering.Gtk (GtkBackend (..)) where

import Data.Foldable (traverse_)
import Data.GI.Base
import Data.GI.Base.Overloading (IsDescendantOf)
import qualified GI.Gtk as Gtk
import Mercury.Runtime
import Mercury.Runtime.Rendering.Backend

data GtkBackend = GtkBackend

instance RenderingBackend GtkBackend where
    type Widget GtkBackend = Gtk.Widget
    type Box GtkBackend = Gtk.Box
    type Label GtkBackend = Gtk.Label
    type Button GtkBackend = Gtk.Button

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
