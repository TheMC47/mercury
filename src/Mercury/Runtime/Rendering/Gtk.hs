{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Mercury.Runtime.Rendering.Gtk (GtkBackend (..)) where

import Control.Monad
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Foldable (traverse_)
import Data.Functor
import Data.GI.Base
import Data.GI.Base.Overloading (IsDescendantOf)
import Debug.Trace
import Foreign (castPtr)
import GI.GLib (ioAddWatch)
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import GI.Gdk.Objects.Display
import GI.Gdk.Objects.Surface
import qualified GI.GdkX11 as GdkX11
import qualified GI.Gtk as Gtk
import qualified Graphics.X11 as X11
import Graphics.X11.Xlib.Extras as X11
import Mercury.Runtime
import Mercury.Runtime.Rendering.Backend
import Mercury.Window
import System.Exit (exitSuccess)
import UnliftIO
import UnliftIO.Concurrent

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
    renderButton w onClick = do
        onClickIO <- toIO onClick
        new Gtk.Button [#child := w, On #clicked onClickIO]
    setLabelText = #setLabel
    labelToWidget = Gtk.toWidget
    boxToWidget = Gtk.toWidget
    buttonToWidget = Gtk.toWidget

    createApplication appId = new Gtk.Application [#applicationId := appId]
    onApplicationActivate app action =
        toIO action >>= void . Gtk.on app #activate

    runApplication app = void $ #run app Nothing
    killAllWindows app = do
        windows <- #getWindows app
        traverse_ (\w -> #hide w >> #destroy w) windows

    renderWindow app w geom t = do
        win <-
            new
                Gtk.Window
                [ #child := w
                , #title := t
                , #application := app
                , #decorated := False
                , #resizable := False
                , #defaultWidth := maybe (-1) fromIntegral (width geom)
                , #defaultHeight := maybe (-1) fromIntegral (height geom)
                ]
        maybe (return ()) (setMoveWindow win) (position geom)
        #present win
        return win

    idleAdd action = do
        ioAction <- toIO action
        void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE (ioAction $> False)

setMoveWindow :: (MonadIO m) => Gtk.Window -> Position -> m ()
setMoveWindow win (x, y) = void $ on win #realize $ do
    result <- runExceptT (setMoveWindow' win (x, y))
    case result of
        Left err -> error ("setMoveWindow error: " ++ err)
        Right () -> return ()

setMoveWindow' :: (MonadIO m) => Gtk.Window -> Position -> ExceptT String m ()
setMoveWindow' win (x, y) = do
    maybeSurface <- #getSurface win
    maybeX11Surface <- maybe (throwE "No surface available") (liftIO . castTo GdkX11.X11Surface) maybeSurface
    x11Surface <- maybe (throwE "Not an X11 surface") return maybeX11Surface
    gdkDisplay <- #getDisplay x11Surface
    maybeX11Display <- liftIO $ castTo GdkX11.X11Display gdkDisplay
    display <- maybe (throwE "Not an X11 display") return maybeX11Display
    x11Display <- GdkX11.x11DisplayGetXdisplay display
    liftIO $
        withManagedPtr x11Display $ \x11DisplayPtr -> do
            let dpy = X11.Display (castPtr x11DisplayPtr)
            xid <- GdkX11.x11SurfaceGetXid x11Surface
            let winID = fromIntegral xid :: X11.Window
            -- TODO add window type
            wmWindowType <- X11.internAtom dpy "_NET_WM_WINDOW_TYPE" False
            typeDock <- X11.internAtom dpy "_NET_WM_WINDOW_TYPE_DOCK" False
            X11.changeProperty32 dpy winID wmWindowType X11.aTOM X11.propModeReplace [fromIntegral typeDock]
            X11.moveWindow dpy winID (fromIntegral x) (fromIntegral y)
            X11.flush dpy
    return ()
