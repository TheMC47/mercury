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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Mercury.DevMain (update) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, ask, asks)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.Function
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
import System.IO
import System.Process
import Text.Read

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

data RuntimeEnvironment = RuntimeEnvironment
    { varValues :: !(TVar VariableEnv)
    , subscribers :: !(M.Map Text (MercuryRuntime ()))
    }

newtype MercuryRuntime a = MercuryRuntime (ReaderT RuntimeEnvironment IO a)
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadReader RuntimeEnvironment)

instance (Semigroup a) => Semigroup (MercuryRuntime a) where
    (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (MercuryRuntime a) where
    mempty = pure mempty

runMercuryRuntime :: MercuryRuntime a -> RuntimeEnvironment -> IO a
runMercuryRuntime (MercuryRuntime m) = runReaderT m

updateVariableValue :: Variable -> Text -> MercuryRuntime ()
updateVariableValue Variable{..} newVal = do
    RuntimeEnvironment{..} <- ask
    didChange <- liftIO $ atomically $ do
        previousValue <- M.lookup name <$> readTVar varValues
        let changed = previousValue /= Just newVal
        when changed $ modifyTVar varValues (M.insert name newVal)
        return changed
    when didChange $
        M.findWithDefault mempty name subscribers

setupVariable :: Variable -> MercuryRuntime ()
setupVariable v@Variable{..} = do
    env <- ask
    let updateVariableValue' val = runMercuryRuntime (updateVariableValue v val) env
    case runtimeBehavior of
        Polling{..} -> do
            liftIO $ void $ forkIO $ forever $ do
                val <- runPollingAction action
                updateVariableValue' val
                threadDelay (intervalMs * 1000)
        Subscription{..} -> liftIO $ do
            let SubscriptionScriptAction (Script path args) = script
            (_, Just hout, _, _) <- createProcess (proc path args){std_out = CreatePipe}
            hSetBuffering hout LineBuffering
            liftIO $ void $ forkIO $ forever $ do
                line <- hGetLine hout
                updateVariableValue' (T.pack line)

updateComponent :: LiveComponent -> MercuryRuntime ()
updateComponent LiveLabel{..} = do
    env <- asks varValues
    vals <- liftIO $ readTVarIO env
    liftIO $ #setLabel labelWidget (eval labelExpr vals)

activate :: Gtk.Application -> IO ()
activate app = do
    -- Step 1: Render the widget, collecting live components
    (widget, liveComponents) <- runWriterT $ render mempty myWidget
    -- Step2: Setup rendering updates for all variables
    let liveComponentIndex = buildLiveComponentIndex liveComponents
    let subscriberMap = M.map (mconcat . map updateComponent) liveComponentIndex

    -- Step 3: Set up runtime environment
    runtimeEnv <- (`RuntimeEnvironment` subscriberMap) <$> newTVarIO M.empty

    -- Step 4: initialize variables
    let allVars = getAllVars myWidget
    runMercuryRuntime (traverse_ setupVariable allVars) runtimeEnv

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

data ScriptAction = Script !FilePath ![String]

data PollingAction
    = PollingCustomIO !(IO Text)
    | PollingScriptAction !ScriptAction

data SubscriptionAction
    = SubscriptionScriptAction !ScriptAction

data RuntimeBehavior
    = Polling {intervalMs :: !Int, action :: !PollingAction}
    | Subscription {script :: !SubscriptionAction}

runPollingAction :: PollingAction -> IO Text
runPollingAction (PollingCustomIO io) = io
runPollingAction (PollingScriptAction (Script path args)) = do
    output <- readProcess path args ""
    return $ T.pack (init output)

data Variable = Variable
    { name :: !Text
    , runtimeBehavior :: !RuntimeBehavior
    }

timestampVar :: Variable
timestampVar =
    Variable
        { name = "current_time"
        , runtimeBehavior = Polling 1000 (PollingCustomIO (tshow <$> getCurrentTime))
        }

type VariableEnv = M.Map Text Text

data Expression a = Expression
    { dependencies :: !(S.Set Text)
    , vars :: ![Variable]
    , eval :: !(VariableEnv -> a)
    }

isStatic :: Expression a -> Bool
isStatic e = S.null (dependencies e)

use :: Variable -> Expression Text
use v@(Variable{..}) =
    Expression
        { dependencies = S.singleton name
        , eval = fromMaybe "" . M.lookup name
        , vars = [v]
        }

(#) :: Variable -> Expression Text
(#) = use
infixl 9 #

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

instance Functor Expression where
    fmap f e = e{eval = f . eval e}
instance Applicative Expression where
    pure x =
        Expression
            { dependencies = S.empty
            , eval = const x
            , vars = []
            }
    (Expression depsF varsF evalF) <*> (Expression depsX varsX evalX) =
        Expression
            { dependencies = depsF `S.union` depsX
            , eval = evalF <*> evalX
            , vars = nubBy ((==) `on` name) (varsF <> varsX)
            }

cpuAnd100 :: Expression Int
cpuAnd100 =
    do
        uText <- (#) cpuUsage
        pure $ fromMaybe 0 (readText @Int uText)

data Widget
    = Box
        { spaceEvenly :: !Bool
        , children :: ![Widget]
        }
    | Label
        { text :: !(Expression Text)
        }
    | Button
        { child :: !Widget
        }

getAllVars :: Widget -> [Variable]
getAllVars (Label{..}) = vars text
getAllVars (Box{..}) =
    nubBy ((==) `on` name) $ concatMap getAllVars children
getAllVars (Button{..}) = getAllVars child

data LiveComponent = LiveLabel
    { labelWidget :: !Gtk.Label
    , labelExpr :: !(Expression Text)
    }

render :: VariableEnv -> Widget -> WriterT [LiveComponent] IO Gtk.Widget
render env (Label{..}) = do
    l <- new Gtk.Label [#label := eval text env]
    unless (isStatic text) $ tell [LiveLabel l text]
    Gtk.toWidget l
render env (Box{..}) = do
    box <- new Gtk.Box [#homogeneous := spaceEvenly]
    traverse_ (#append box <=< render env) children
    Gtk.toWidget box
render env (Button{..}) = do
    button <- new Gtk.Button []
    childWidget <- render env child
    button `set` [#child := childWidget]
    Gtk.toWidget button

buildLiveComponentIndex :: [LiveComponent] -> M.Map Text [LiveComponent] -- Variable name to components depending on it
buildLiveComponentIndex components =
    M.fromListWith
        (++)
        [(var, [comp]) | comp <- components, var <- S.toList (dependencies (getExpr comp))]
  where
    getExpr :: LiveComponent -> Expression Text
    getExpr (LiveLabel{..}) = labelExpr

update :: IO ()
update = do
    app <-
        new
            Gtk.Application
            [ #applicationId := "haskell-gi.example"
            , On #activate (activate ?self)
            ]
    void $ #run app Nothing
