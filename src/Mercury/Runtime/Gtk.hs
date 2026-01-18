{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}

module Mercury.Runtime.Gtk (handle) where

import Data.Foldable (traverse_)
import Data.GI.Base
import Data.GI.Base.Overloading (IsDescendantOf)
import qualified GI.Gtk as Gtk
import Mercury.Runtime
import Mercury.Runtime.Rendering.Handle

handle :: Handle MercuryRuntime Gtk.Widget Gtk.Box Gtk.Label Gtk.Button
handle =
    Handle
        { boxToWidget = Gtk.toWidget
        , labelToWidget = Gtk.toWidget
        , buttonToWidget = Gtk.toWidget
        , renderBox = gtkRenderBox
        , renderLabel = \str -> new Gtk.Label [#label := str]
        , renderButton = \w -> new Gtk.Button [#child := w]
        , setLabelText = #setLabel
        }

gtkRenderBox :: Bool -> [Gtk.Widget] -> MercuryRuntime Gtk.Box
gtkRenderBox homogeneous ws = do
    box <- new Gtk.Box [#homogeneous := homogeneous]
    traverse_ (#append box) ws
    return box
