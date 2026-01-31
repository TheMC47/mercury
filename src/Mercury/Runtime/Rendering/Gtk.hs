{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.Maybe (fromMaybe, listToMaybe)
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
import Graphics.X11.Xinerama (getScreenInfo)
import Graphics.X11.Xlib.Extras as X11
import Mercury.Runtime
import Mercury.Runtime.Rendering.Backend
import Mercury.Window
import Mercury.Window.Geometry
import System.Exit (exitSuccess)
import UnliftIO
import UnliftIO.Concurrent

data GtkBackend = GtkBackend

instance RenderingBackend GtkBackend where
    newtype Widget GtkBackend = MkWidget Gtk.Widget
    newtype Box GtkBackend = MkBox Gtk.Box
    newtype Label GtkBackend = MkLabel Gtk.Label
    newtype Button GtkBackend = MkButton Gtk.Button
    newtype Window GtkBackend = MkWindow Gtk.Window
    newtype Application GtkBackend = MkApplication Gtk.Application

    renderBox homogeneous ws = do
        box <- new Gtk.Box [#homogeneous := homogeneous]
        traverse_ (#append box) [w | MkWidget w <- ws]
        return (MkBox box)
    renderLabel str = MkLabel <$> new Gtk.Label [#label := str]
    renderButton (MkWidget w) onClick = do
        onClickIO <- toIO onClick
        MkButton <$> new Gtk.Button [#child := w, On #clicked onClickIO]
    setLabelText (MkLabel l) = #setLabel l
    labelToWidget (MkLabel l) = MkWidget <$> Gtk.toWidget l
    boxToWidget (MkBox b) = MkWidget <$> Gtk.toWidget b
    buttonToWidget (MkButton b) = MkWidget <$> Gtk.toWidget b

    createApplication appId = MkApplication <$> new Gtk.Application [#applicationId := appId]
    onApplicationActivate (MkApplication app) action =
        toIO action >>= void . Gtk.on app #activate

    runApplication (MkApplication app) = void $ #run app Nothing
    killAllWindows (MkApplication app) = do
        windows <- #getWindows app
        traverse_ (\w -> #hide w >> #destroy w) windows

    renderWindow (MkApplication app) (MkWidget w) geom t = do
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
        setMoveWindow win (screen geom) (position geom)
        #present win
        return (MkWindow win)

    idleAdd action = do
        ioAction <- toIO action
        void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE (ioAction $> False)

setMoveWindow :: (MonadUnliftIO m) => Gtk.Window -> Int -> Position -> m ()
setMoveWindow win screenIdx (x, y) = void $ on win #realize $ do
    withX11Window win $ \winID dpy -> do
        -- TODO add window type
        wmWindowType <- X11.internAtom dpy "_NET_WM_WINDOW_TYPE" False
        typeDock <- X11.internAtom dpy "_NET_WM_WINDOW_TYPE_DOCK" False
        X11.changeProperty32 dpy winID wmWindowType X11.aTOM X11.propModeReplace [fromIntegral typeDock]
        screenInfos <- liftIO $ getScreenInfo dpy
        let scrrenInfo = listToMaybe [screenInfos !! screenIdx | screenIdx >= 0, screenIdx < length screenInfos]
        let move = X11.moveWindow dpy winID
        flip (maybe (move (fromIntegral x) (fromIntegral y))) scrrenInfo $ \X11.Rectangle{..} -> do
            move (rect_x + fromIntegral x) (rect_y + fromIntegral y)
        X11.flush dpy

withX11Window :: (MonadUnliftIO m) => Gtk.Window -> (X11.Window -> X11.Display -> m a) -> m a
withX11Window win action =
    withXID win $ withX11Display . action

withXID :: (MonadUnliftIO m) => Gtk.Window -> (X11.Window -> m a) -> m a
withXID win action = do
    maybeSurface <- #getSurface win
    maybeX11Surface <- maybe (error "No surface available") (liftIO . castTo GdkX11.X11Surface) maybeSurface
    x11Surface <- maybe (error "Not an X11 surface") return maybeX11Surface
    xid <- GdkX11.x11SurfaceGetXid x11Surface
    let winID = fromIntegral xid :: X11.Window
    action winID

withX11Display :: (MonadUnliftIO m) => (X11.Display -> m a) -> m a
withX11Display action = do
    uio <- askRunInIO
    maybeDisplay <- Gdk.displayGetDefault
    display <- maybe (error "No default GDK display") return maybeDisplay
    maybeX11Display <- liftIO $ castTo GdkX11.X11Display display
    x11Display <- maybe (error "Not an X11 display") return maybeX11Display
    x11Dpy <- GdkX11.x11DisplayGetXdisplay x11Display
    liftIO $
        withManagedPtr x11Dpy $ \x11DisplayPtr ->
            uio $ action (X11.Display (castPtr x11DisplayPtr))
