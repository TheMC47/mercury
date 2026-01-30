-- Current state: working PoC
-- What happens: A GTK application with live-updating labels based on variable expressions.
-- Next:
-- - Add more variable types: subscriptions, commands, etc.
-- - Make a configuration monad to define and build widgets. Handling variables
-- and building indexes automatically. I could also make the variable type more
-- opaque and hide the reliance on Text to identify variables.
-- - Add a logging system
-- - Handle errors better, especially decoding variable values
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Mercury.DevMain (update) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, ask, asks)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Data.Default
import Data.Foldable
import Data.IORef
import Data.List (nubBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid (Ap)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import Debug.Trace
import Mercury.Runtime
import Mercury.Runtime.Identified (Identified, newStoreIO)
import Mercury.Runtime.Rendering.Backend hiding (Widget, Window, renderWindow)
import qualified Mercury.Runtime.Rendering.Backend as R
import Mercury.Runtime.Rendering.Gtk
import Mercury.Variable
import Mercury.Widget
import Mercury.Window
import qualified StmContainers.Map as SM
import System.IO
import System.Process
import Text.Read (readMaybe)
import UnliftIO (IORef, MonadUnliftIO, askRunInIO)
import UnliftIO.Concurrent

-- Config-----------------------------------------------------------------------
myWindow :: (R.RenderingBackend b) => Window (MercuryRuntime b)
myWindow =
    Window
        { rootWidget = myWidget
        , geometry =
            def
                { width = Just 100
                , height = Just 10
                , position = (26, 26)
                , screen = 1
                }
        , title = "Mercury GTK Example"
        }

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
        , runtimeBehavior = Polling 2000 (PollingCustomIO (return "72"))
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

-- Plumbing --------------------------------------------------------------------

-- TODO collapse [Identified (MercuryRuntime b ())] into Identified (MercuryRuntime b ())
mountExpression :: Expression a -> (a -> MercuryRuntime b ()) -> MercuryRuntime b [Identified (MercuryRuntime b ())]
mountExpression expr onChange =
    traverse
        ( \var ->
            subscribeToVariable var $ evalExpression expr >>= onChange
        )
        (S.toList (dependencies expr))

render :: forall b. (RenderingBackend b) => Widget (MercuryRuntime b) -> MercuryRuntime b (R.Widget b)
render Label{..} = do
    textValue <- evalExpression text
    labelWidget <- renderLabel @b textValue
    unless (isStatic text) (void $ mountExpression text (setLabelText @b labelWidget))
    labelToWidget @b labelWidget
render Box{..} = do
    renderedChildren <- traverse (render @b) children
    boxWidget <- renderBox @b spaceEvenly renderedChildren
    boxToWidget @b boxWidget
render Button{..} = do
    renderedChild <- render @b child
    buttonWidget <- renderButton @b renderedChild onClick
    buttonToWidget @b buttonWidget

renderWindow :: forall b. (RenderingBackend b) => R.Application b -> Window (MercuryRuntime b) -> MercuryRuntime b (R.Window b)
renderWindow app Window{..} = do
    widget <- render @b rootWidget
    R.renderWindow @b app widget geometry title

update :: IO ()
update = activate

activate' :: forall b. (RenderingBackend b) => MercuryRuntime b ()
activate' = do
    time <- liftIO getPOSIXTime
    liftIO $ putStrLn $ "Application started at POSIX time: " ++ show time
    app <- startApplication @b

    let allVars = getAllVars (myWidget @b)
    traverse_ addVariable (S.toList allVars)
    traverse_ setupVariable (S.toList allVars)

    onApplicationActivate @b app $ do
        void $ renderWindow @b app myWindow
        liftIO $ putStrLn $ "Render finished" ++ show time
    runApplication @b app

runPollingAction :: PollingAction -> MercuryRuntime b Text
runPollingAction (PollingCustomIO io) = liftIO io
runPollingAction (PollingScriptAction (Script path args)) = do
    output <- liftIO $ readProcess path args ""
    return $ T.pack (init output)

tshow :: (Show a) => a -> Text
tshow = T.pack . show

readText :: (Read a) => Text -> Maybe a
readText = readMaybe . T.unpack

myWidget :: (R.RenderingBackend b) => Widget (MercuryRuntime b)
myWidget =
    Box
        { spaceEvenly = True
        , children =
            [ Label{text = (#) timestampVar}
            , Button
                { child = Box{spaceEvenly = False, children = [Label{text = tshow <$> cpuAnd100}]}
                , onClick = closeWindows
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

activate :: IO ()
activate = do
    runtimeVariables <- SM.newIO
    uidStore <- newStoreIO
    let renderingBackend = GtkBackend
    applicationInstance <- newIORef Nothing
    runMercuryRuntime activate' (RuntimeEnvironment{..})
