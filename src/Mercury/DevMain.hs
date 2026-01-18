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
{-# LANGUAGE LambdaCase #-}
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
import qualified Mercury.Runtime.Gtk as MGtk
import Mercury.Runtime.Identified (Identified, newStoreIO)
import qualified Mercury.Runtime.Rendering.Handle as Render
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

activate :: Gtk.Application -> IO ()
activate app = do
    runtimeVariables <- SM.newIO
    uidStore <- newStoreIO
    runMercuryRuntime (activate' app) (RuntimeEnvironment{..})

activate' :: Gtk.Application -> MercuryRuntime ()
activate' app = do
    let allVars = getAllVars myWidget
    traverse_ addVariable (S.toList allVars)
    traverse_ setupVariable (S.toList allVars)

    widget <- render MGtk.handle myWidget

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

-- TODO collapse [Identified (MercuryRuntime ())] into Identified (MercuryRuntime ())
mountExpression :: Expression a -> (a -> MercuryRuntime ()) -> MercuryRuntime [Identified (MercuryRuntime ())]
mountExpression expr onChange =
    traverse
        ( \var ->
            subscribeToVariable var $ evalExpression expr >>= onChange
        )
        (S.toList (dependencies expr))

render ::
    Render.Handle MercuryRuntime widget box label button ->
    Widget ->
    MercuryRuntime widget
render h@Render.Handle{..} = \case
    Label{..} -> do
        textValue <- evalExpression text
        labelWidget <- renderLabel textValue
        unless (isStatic text) (void $ mountExpression text (setLabelText labelWidget))
        labelToWidget labelWidget
    Box{..} -> do
        renderedChildren <- traverse (render h) children
        boxWidget <- renderBox spaceEvenly renderedChildren
        boxToWidget boxWidget
    Button{..} -> do
        renderedChild <- render h child
        buttonWidget <- renderButton renderedChild
        buttonToWidget buttonWidget

update :: IO ()
update = do
    app <-
        new
            Gtk.Application
            [ #applicationId := "haskell-gi.example"
            , On #activate (activate ?self)
            ]
    void $ #run app Nothing
