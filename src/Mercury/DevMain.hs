-- Current state: working PoC
-- What happens: A GTK application with live-updating labels based on variable expressions.
-- Next:
-- - Add more variable types: subscriptions, commands, etc.
-- - Make a configuration monad to define and build widgets. Handling variables
-- and building indexes automatically. I could also make the variable type more
-- opaque and hide the reliance on Text to identify variables.
-- - Add a logging system
-- - Handle errors better, especially decoding variable values
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Mercury.DevMain (update) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable
import Data.Functor
import Data.GI.Base.Utils (whenJust)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.Time.Clock.POSIX
import Mercury.Runtime
import Mercury.Runtime.Identified (Identified, newStoreIO)
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
                , strut = Nothing
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

-- TODO collapse [Identified (MercuryRuntime b ())] into Identified (MercuryRuntime b ())
mountExpression :: Expression a -> (a -> MercuryRuntime b ()) -> MercuryRuntime b [Identified (MercuryRuntime b ())]
mountExpression expr onChange =
    traverse
        ( \var ->
            subscribeToVariable var $ evalExpression expr >>= onChange
        )
        (S.toList (dependencies expr))

render :: (RenderingBackend b) => Widget -> MercuryRuntime b (R.Widget b)
render Label{..} = do
    textValue <- evalExpression text
    label <- renderLabel textValue
    unless (isStatic text) (void $ mountExpression text (setText label))
    return (labelWidget label)
render Box{..} = do
    renderedChildren <- traverse render children
    renderBox spaceEvenly renderedChildren
render Button{onClick = Action a, ..} = do
    renderedChild <- render child
    button <- renderButton renderedChild a
    whenJust buttonClass $ \expr -> do
        classValue <- evalExpression expr
        setClass button classValue
        unless (isStatic expr) $
            void $
                mountExpression expr (setClass button)
    return (buttonWidget button)

renderWindow :: (RenderingBackend b) => R.BackendHandle b -> Window -> MercuryRuntime b (R.Window b)
renderWindow handle Window{..} = do
    widget <- render rootWidget
    createWindow handle widget geometry title

update :: IO ()
update = activate

activate' :: (R.RenderingBackend b) => R.BackendHandle b -> MercuryRuntime b ()
activate' handle = do
    time <- liftIO getPOSIXTime
    liftIO $ putStrLn $ "Application started at POSIX time: " ++ show time

    let allVars = getAllVars myWidget
    traverse_ addVariable (S.toList allVars)
    traverse_ setupVariable (S.toList allVars)

    void $ renderWindow handle myWindow
    liftIO $ putStrLn $ "Render finished" ++ show time

runPollingAction :: PollingAction -> MercuryRuntime b Text
runPollingAction (PollingCustomIO io) = liftIO io
runPollingAction (PollingScriptAction (Script path args)) = do
    output <- liftIO $ readProcess path args ""
    return $ T.pack (init output)

tshow :: (Show a) => a -> Text
tshow = T.pack . show

myWidget :: Widget
myWidget =
    Box
        { spaceEvenly = True
        , children =
            [ Label{text = (#) timestampVar}
            , Button
                { child = Box{spaceEvenly = False, children = [Label{text = tshow <$> cpuAnd100}]}
                , onClick = Action closeWindows
                , buttonClass =
                    Just
                        (cpuAnd100 <&> \v -> ["close-button" | v >= 100])
                }
            , Button
                { child = Label{text = pure "Increment"}
                , onClick =
                    Action
                        ( void $
                            withTypedValue cpuUsage $
                                updateTypedValue cpuUsage . (+ 1)
                        )
                , buttonClass = Nothing
                }
            , Label{text = (#) xpropSpy}
            ]
        }

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
