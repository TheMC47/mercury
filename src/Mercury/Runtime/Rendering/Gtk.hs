{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Mercury.Runtime.Rendering.Gtk (
    GtkBackend (..),
    concretizeStruts,
    ExtraBackendData (..),
) where

import Control.Monad
import Data.Default (Default (def))
import Data.Foldable (traverse_)
import Data.GI.Base
import Data.GI.Base.Utils
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Foreign (castPtr)
import Foreign.C (CInt, CLong)
import Foreign.Store
import GI.GLib qualified as GLib
import GI.Gdk qualified as Gdk
import GI.Gdk.Objects.Surface
import GI.GdkX11 qualified as GdkX11
import GI.Gtk qualified as Gtk
import GI.Pango
import Graphics.X11 (rect_height, rect_width, rect_x, rect_y)
import Graphics.X11 qualified as X11
import Graphics.X11.Xinerama (getScreenInfo)
import Graphics.X11.Xlib.Extras as X11
import Mercury.Runtime.Rendering.Backend
import Mercury.Window.Geometry
import System.Posix
import UnliftIO

data GtkBackend = GtkBackend

instance RenderingBackend GtkBackend where
    newtype Widget GtkBackend = MkWidget Gtk.Widget
    newtype Window GtkBackend = MkWindow Gtk.Window
    newtype BackendHandle GtkBackend = MkBackendHandle Gtk.Application
    data ExtraBackendData GtkBackend = ExtraGTKData
        { cssFilePath :: !(Maybe FilePath)
        , applicationId :: !Text
        }

    withBackend ExtraGTKData{..} callback = do
        app <- new Gtk.Application [#applicationId := applicationId]

        shutdownIO <- toIO (shutdown (MkBackendHandle app))
        liftIO $ void $ installHandler sigTERM (Catch shutdownIO) Nothing

        callbackIO <- toIO (callback (MkBackendHandle app))
        void $ Gtk.on app #activate $ do
            whenJust cssFilePath setupCSS
            callbackIO
        void $ #run app Nothing
      where
        setupCSS :: FilePath -> IO ()
        setupCSS path = do
            cssProvider <- Gtk.cssProviderNew
            Gtk.cssProviderLoadFromPath cssProvider path
            maybeDisplay <- Gdk.displayGetDefault
            case maybeDisplay of
                Just display ->
                    Gtk.styleContextAddProviderForDisplay
                        display
                        cssProvider
                        (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)
                Nothing -> putStrLn "Warning: No display found, CSS not loaded."
    withHotReload ExtraGTKData{..} callback = do
        mbOldPid <- liftIO $ lookupStore 0
        whenJust mbOldPid $ \store -> liftIO $ do
            oldPid <- readStore store :: IO ProcessID
            catch
                (signalProcess sigTERM oldPid)
                ( \(e :: SomeException) -> putStrLn $ "SIGTERM failed: " ++ displayException e
                )
            void $ getProcessStatus True False oldPid
        callbackIO <- toIO (withBackend ExtraGTKData{..} callback)
        pid <- liftIO $ forkProcess callbackIO
        liftIO $ writeStore (Store 0) pid

    shutdown (MkBackendHandle app) = void $ GLib.idleAdd (-100) $ do
        windows <- #getWindows app
        traverse_ (\w -> #hide w >> #destroy w) windows
        #quit app
        return False

    renderBox RenderBoxProps{..} = do
        box <- new Gtk.Box [#hexpand := False, #vexpand := False]
        whenJust renderBox_spaceEvenly (#setHomogeneous box)
        whenJust renderBox_class (#setCssClasses box)
        traverse_ (#append box) [w | MkWidget w <- renderBox_children]
        widget <- Gtk.toWidget box
        pure $
            RenderedBox
                { box_widget = MkWidget widget
                , box_setClass = #setCssClasses box
                , box_setSpaceEvenly = #setHomogeneous box
                }

    renderLabel RenderLabelProps{..} = do
        label <-
            new
                Gtk.Label
                [ #label := renderLabel_text
                , #hexpand := False
                , #vexpand := False
                , #ellipsize := EllipsizeModeEnd
                ]
        widget <- Gtk.toWidget label
        whenJust renderLabel_class (#setCssClasses label)
        pure $
            RenderedLabel
                { label_widget = MkWidget widget
                , label_setText = #setLabel label
                , label_setClass = #setCssClasses label
                }

    renderButton RenderButtonProps{..} = do
        btn <- new Gtk.Button [#hexpand := False, #vexpand := False]
        whenJust renderButton_class (#setCssClasses btn)
        whenJust renderButton_child (#setChild btn . (\(MkWidget w) -> Just w))
        whenJust renderButton_onClick $ \action -> do
            onClickIO <- toIO action
            void $ Gtk.on btn #clicked onClickIO
        widget <- Gtk.toWidget btn
        pure $
            RenderedButton
                { button_widget = MkWidget widget
                , button_setClass = #setCssClasses btn
                , button_setOnClick = \action -> do
                    onClickIO <- toIO action
                    void $ Gtk.on btn #clicked onClickIO
                }

    createWindow (MkBackendHandle app) (MkWidget w) geom t = do
        win <-
            new
                Gtk.Window
                [ #child := w
                , #title := t
                , #application := app
                , #decorated := False
                , #resizable := False
                ]
        setWindowGeometry win geom
        #present win
        return (MkWindow win)

instance Default (ExtraBackendData GtkBackend) where
    def =
        ExtraGTKData
            { cssFilePath = Nothing
            , applicationId = "com.example.MercuryApp"
            }

setWindowGeometry :: (MonadUnliftIO m) => Gtk.Window -> Geometry -> m ()
setWindowGeometry win Geometry{position = (x, y), ..} = void $ on win #realize $ do
    withX11Window win $ \winID dpy -> do
        screenInfos <- liftIO $ getScreenInfo dpy
        rootWin <- X11.rootWindow dpy (X11.defaultScreen dpy)
        WindowAttributes{wa_width, wa_height} <- X11.getWindowAttributes dpy rootWin
        screenInfo <- maybe (error "No screen info available") return $ listToMaybe [screenInfos !! screen | screen >= 0, screen < length screenInfos]

        wmWindowType <- X11.internAtom dpy "_NET_WM_WINDOW_TYPE" False
        typeDock <- X11.internAtom dpy "_NET_WM_WINDOW_TYPE_DOCK" False
        X11.changeProperty32 dpy winID wmWindowType X11.aTOM X11.propModeReplace [fi typeDock]

        X11.moveWindow dpy winID (rect_x screenInfo + fi x) (rect_y screenInfo + fi y)
        whenJust strut $ \strut' -> do
            let strutValues = concretizeStruts wa_width wa_height screenInfo strut'
            wmStrutPartial <- X11.internAtom dpy "_NET_WM_STRUT_PARTIAL" False
            X11.changeProperty32 dpy winID wmStrutPartial X11.cARDINAL X11.propModeReplace strutValues

        let w = concretizeDimension (rect_width screenInfo) <$> width
            h = concretizeDimension (rect_height screenInfo) <$> height
        #setDefaultSize win (maybe (-1) fi w) (maybe (-1) fi h)
        #setSizeRequest win (maybe (-1) fi w) (maybe (-1) fi h)

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
    let winID = fi xid :: X11.Window
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

fi :: forall a b. (Integral a, Num b) => a -> b
fi = fromIntegral

{- ORMOLU_DISABLE -}
concretizeStruts :: CInt -> CInt -> X11.Rectangle -> Strut -> [CLong]
concretizeStruts
    (fi -> rootW)
    (fi -> rootH)
    X11.Rectangle
        { rect_x = fi -> monX
        , rect_y = fi -> monY
        , rect_width = fi -> monW
        , rect_height = fi -> monH
        }
    (Strut dir len) =
        let
            monEndX = monX + monW
            monEndY = monY + monH
            l = (`concretizeDimension` len) $ case dir of
                L -> monH
                R -> monH
                T -> monW
                B -> monW
         in
            case dir of
                --   [left    , right              , top     , bottom             , left_start_y, left_end_y , right_start_y, right_end_y    , top_start_x, top_end_x  , bottom_start_x, bottom_end_x   ]
                L -> [monX + l, 0                  , 0       , 0                  , monY        , monEndY - 1, 0            , 0              , 0          , 0          , 0             , 0              ]
                R -> [0       , rootW - monEndX + l, 0       , 0                  , 0           , 0          , monY         , monY + monH - 1, 0          , 0          , 0             , 0              ]
                T -> [0       , 0                  , monY + l, 0                  , 0           , 0          , 0            , 0              , monX       , monEndX - 1, 0             , 0              ]
                B -> [0       , 0                  , 0       , rootH - monEndY + l, 0           , 0          , 0            , 0              , 0          , 0          , monX          , monX + monW - 1]
{- ORMOLU_ENABLE -}
