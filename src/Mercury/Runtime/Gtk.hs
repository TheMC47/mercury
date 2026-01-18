{-# LANGUAGE GADTs #-}

module Mercury.Runtime.Gtk () where

import Data.Text
import Mercury.Runtime
import Mercury.Widget

-- Option1: GADT
-- Option2: they get a callback to register for updates

-- Represents a live property of a widget: a handle to the backend widget and an expression it's bound to
