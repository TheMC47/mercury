{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Mercury.DevMain (update) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Mercury.Runtime
import Mercury.Runtime.Identified (newStoreIO)
import Mercury.Runtime.Rendering
import Mercury.Runtime.Rendering.Backend hiding (Widget, Window)
import Mercury.Runtime.Rendering.Backend qualified as R
import Mercury.Runtime.Rendering.Gtk
import Mercury.Variable
import Mercury.Variable.Typed
import Mercury.Widget
import Mercury.Window
import Mercury.Window.Geometry
import StmContainers.Map qualified as SM
import System.IO
import System.Process
import UnliftIO.Concurrent

-- Config-----------------------------------------------------------------------
myWindow :: Window
myWindow =
    Window
        { rootWidget = myWidget
        , geometry =
            def
                { width = Just (Percentage 50)
                , height = Just (Percentage 2)
                , position = (50, 1400)
                , strut = Just (Strut B (Absolute 30))
                , screen = 0
                }
        , title = "Mercury GTK Example"
        }

timestampVar :: TypedVariable Text
timestampVar = pollingVar "current_time" 1000 (tshow <$> getCurrentTime)

cpuUsage :: TypedVariable Int
cpuUsage = pureVar "cpu_usage" 99

xpropSpy :: TypedVariable Text
xpropSpy = subscriptionVar "xprop_spy" (SubscriptionScriptAction (Script "xprop" ["-root", "-spy", "_NET_ACTIVE_WINDOW"]))

cpuAnd100 :: Expression Int
cpuAnd100 = useOr 0 cpuUsage

-- Plumbing --------------------------------------------------------------------

update :: IO ()
update = activate

activate' :: (R.RenderingBackend b) => R.BackendHandle b -> MercuryRuntime b ()
activate' handle = do
    let allVars = getAllVariables myWidget
    traverse_ addVariable (S.toList allVars)
    traverse_ setupVariable (S.toList allVars)

    void $ renderWindow handle myWindow

runPollingAction :: PollingAction -> MercuryRuntime b Text
runPollingAction (PollingCustomIO io) = liftIO io
runPollingAction (PollingScriptAction (Script path args)) = do
    output <- liftIO $ readProcess path args ""
    return $ T.pack (init output)

tshow :: (Show a) => a -> Text
tshow = T.pack . show

myWidget :: Widget
myWidget =
    w $
        box
            & (spaceEvenly =: False)
            & ( children
                    =: [ w $ label & (text =: (#) timestampVar)
                       , w $
                            button
                                & (child =: w (label & (text =: tshow <$> cpuAnd100)))
                                & (onClick =: Action closeWindows)
                                & (classes =: (cpuAnd100 <&> \v -> ["close-button" :: Text | v >= 100]))
                       , w $
                            button
                                & (child =: Just (w $ label & (text =: ("Increment" :: Text))))
                                & (onClick =: Action (void $ withTypedValue cpuUsage $ updateTypedValue cpuUsage . (+ 1)))
                       , w $ label & (text =: (#) xpropSpy)
                       ]
              )

updateVariableValue :: Variable -> Text -> MercuryRuntime b ()
updateVariableValue = updateValue

setupVariable :: Variable -> MercuryRuntime b ()
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
        Pure{} -> pure ()

activate :: IO ()
activate = do
    runtimeVariables <- SM.newIO
    uidStore <- newStoreIO
    withBackend @GtkBackend (def{applicationId = "com.example.mercuryapp", cssFilePath = Just "style.css"}) $ \handle -> do
        let backendHandle = handle
            env = RuntimeEnvironment{..}
        runMercuryRuntime (activate' handle) env
