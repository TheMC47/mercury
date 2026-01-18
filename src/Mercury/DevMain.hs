-- Current state: working PoC
-- What happens: A GTK application with live-updating labels based on variable expressions.
-- Next:
-- - Add more variable types: subscriptions, commands, etc.
-- - Make a configuration monad to define and build widgets. Handling variables
-- and building indexes automatically. I could also make the variable type more
-- opaque and hide the reliance on Text to identify variables.
-- - Add a logging system
-- - Handle errors better, especially decoding variable values
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Mercury.DevMain (update) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, ask, asks)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Data.Foldable
import Data.GI.Base (AttrOp (On, (:=)), new, set)
import Data.List (nubBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid (Ap)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import GHC.IO.FD (FD (fdFD))
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import Mercury.Runtime
import Mercury.Runtime.Identified (newStoreIO)
import Mercury.Variable
import Mercury.Widget
import qualified StmContainers.Map as SM
import System.IO
import System.Process
import Text.Read (readMaybe)
import UnliftIO (askRunInIO)
import UnliftIO.Concurrent

tshow :: (Show a) => a -> Text
tshow = T.pack . show

readText :: (Read a) => Text -> Maybe a
readText = readMaybe . T.unpack

myWidget :: Widget
myWidget =
    Box
        { spaceEvenly = True
        , children =
            [ Label{text = (#) timestampVar}
            , Button{child = Box{spaceEvenly = False, children = [Label{text = tshow <$> cpuAnd100}]}}
            , Label{text = (#) xpropSpy}
            ]
        }

updateVariableValue :: Variable -> Text -> MercuryRuntime ()
updateVariableValue = updateValue

setupVariable :: Variable -> MercuryRuntime ()
setupVariable v@Variable{..} = do
    case runtimeBehavior of
        Polling{..} -> do
            void $ forkIO $ forever $ do
                val <- runPollingAction action
                updateVariableValue v val
                threadDelay (intervalMs * 1000)
        Subscription{..} -> do
            let SubscriptionScriptAction (Script path args) = script
            (_, Just hout, _, _) <- liftIO $ createProcess (proc path args){std_out = CreatePipe}
            liftIO $ hSetBuffering hout LineBuffering
            void $ forkIO $ forever $ do
                line <- liftIO $ hGetLine hout
                updateVariableValue v (T.pack line)

updateComponent :: LiveComponent -> MercuryRuntime ()
updateComponent LiveLabel{..} = evalExpression labelExpr >>= #setLabel labelWidget

activate :: Gtk.Application -> IO ()
activate app = do
    runtimeVariables <- SM.newIO
    uidStore <- newStoreIO
    runMercuryRuntime (activate' app) (RuntimeEnvironment{..})

activate' :: Gtk.Application -> MercuryRuntime ()
activate' app = do
    -- Step 1: Render the widget, collecting live components
    (widget, liveComponents) <- runWriterT $ render myWidget
    let allVars = getAllVars myWidget
    traverse_ addVariable (S.toList allVars)
    traverse_ setupVariable (S.toList allVars)
    traverse_ setupLiveComponent liveComponents

    window <-
        new
            Gtk.ApplicationWindow
            [ #application := app
            , #title := "Mercury Test"
            , #child := widget
            , #defaultWidth := 800
            , #defaultHeight := 600
            ]
    #present window

runPollingAction :: PollingAction -> MercuryRuntime Text
runPollingAction (PollingCustomIO io) = liftIO io
runPollingAction (PollingScriptAction (Script path args)) = do
    output <- liftIO $ readProcess path args ""
    return $ T.pack (init output)

timestampVar :: Variable
timestampVar =
    Variable
        { name = "current_time"
        , runtimeBehavior = Polling 1000 (PollingCustomIO (tshow <$> getCurrentTime))
        }

cpuUsage :: Variable
cpuUsage =
    Variable
        { name = "cpu_usage"
        , runtimeBehavior = Polling 2000 (PollingCustomIO (return "42"))
        }

xpropSpy :: Variable
xpropSpy =
    Variable
        { name = "xprop_spy"
        , runtimeBehavior = Subscription (SubscriptionScriptAction (Script "xprop" ["-root", "-spy", "_NET_ACTIVE_WINDOW"]))
        }

cpuAnd100 :: Expression Int
cpuAnd100 =
    do
        uText <- (#) cpuUsage
        pure $ fromMaybe 0 (readText @Int uText)

data LiveComponent = LiveLabel
    { labelWidget :: !Gtk.Label
    , labelExpr :: !(Expression Text)
    }

render :: Widget -> WriterT [LiveComponent] MercuryRuntime Gtk.Widget
render (Label{..}) = do
    value <- lift $ evalExpression text
    l <- new Gtk.Label [#label := value]
    unless (isStatic text) $ tell [LiveLabel l text]
    Gtk.toWidget l
render (Box{..}) = do
    box <- new Gtk.Box [#homogeneous := spaceEvenly]
    traverse_ (#append box <=< render) children
    Gtk.toWidget box
render (Button{..}) = do
    button <- new Gtk.Button []
    childWidget <- render child
    button `set` [#child := childWidget]
    Gtk.toWidget button

setupLiveComponent :: LiveComponent -> MercuryRuntime ()
setupLiveComponent comp@LiveLabel{..} =
    traverse_ (`subscribeToVariable` updateComponent comp) (S.toList (dependencies labelExpr))

update :: IO ()
update = do
    app <-
        new
            Gtk.Application
            [ #applicationId := "haskell-gi.example"
            , On #activate (activate ?self)
            ]
    void $ #run app Nothing
