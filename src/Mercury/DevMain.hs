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
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Mercury.DevMain (update) where

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.Function
import Data.Functor
import Data.GI.Base (AttrOp (On, (:=)), new, set)
import Data.IORef
import Data.List (nubBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import GHC.IO.FD (FD (fdFD))
import GHC.IO.Handle.FD
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

activate :: Gtk.Application -> IO ()
activate app = do
    -- Step 1: Render the widget, collecting live components
    (widget, liveComponents) <- runWriterT $ render (M.fromList [("cpu_usage", "14")]) myWidget

    -- Step 2: Set up variable polling and live updates
    variables <- newIORef M.empty
    let liveComponentIndex = buildLiveComponentIndex liveComponents
    let allVars = getAllVars myWidget
    let updateValue :: Text -> Text -> IO ()
        updateValue varName newVal = do
            updated <- atomicModifyIORef' variables (\env -> let updated = M.insert varName newVal env in (updated, updated))
            traverse_ (updateComponent updated) (M.findWithDefault [] varName liveComponentIndex)
    let updateVariable :: Variable -> IO ()
        updateVariable Variable{..} = case runtimeBehavior of
            Polling _ -> do
                runAction action >>= updateValue name
            Subscription -> mempty
    let intializeVariable :: Variable -> IO ()
        intializeVariable v@Variable{..} = case runtimeBehavior of
            Polling{..} -> updateVariable v >> gPoll intervalMs (updateVariable v)
            Subscription -> case action of
                Script scriptPath args -> subscribeToScript (updateValue name) scriptPath args
                CustomIO _ -> fail "Subscriptions cannot use CustomIO actions"
    traverse_ intializeVariable allVars

    window <-
        new
            Gtk.ApplicationWindow
            [ #application := app
            , #title := "Mercury Test"
            , #child := widget
            , #defaultWidth := 800
            , #defaultHeight := 600
            ]
    #show window

data RuntimeBehavior
    = Polling {intervalMs :: !Int}
    | Subscription

data Action = CustomIO (IO Text) | Script FilePath [String]

runAction :: Action -> IO Text
runAction (CustomIO io) = io
runAction (Script path args) = do
    output <- readProcess path args ""
    return $ T.pack (init output)

data Variable = Variable
    { name :: !Text
    , runtimeBehavior :: !RuntimeBehavior
    , action :: Action
    }

timestampVar :: Variable
timestampVar =
    Variable
        { name = "current_time"
        , runtimeBehavior = Polling 1000
        , action = CustomIO (tshow <$> getCurrentTime)
        }

updateComponent :: VariableEnv -> LiveComponent -> IO ()
updateComponent env LiveLabel{..} = #setLabel labelWidget (eval labelExpr env)

type VariableEnv = M.Map Text Text

data Expression a = Expression
    { dependencies :: !(S.Set Text)
    , vars :: ![Variable]
    , eval :: VariableEnv -> a
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

gPoll :: Int -> IO () -> IO ()
gPoll timeout action =
    void $ GLib.timeoutAdd GLib.PRIORITY_DEFAULT (fromIntegral timeout) (action $> True)

(#) :: Variable -> Expression Text
(#) = use
infixl 9 #

cpuUsage :: Variable
cpuUsage =
    Variable
        { name = "cpu_usage"
        , runtimeBehavior = Polling 2000
        , action = CustomIO (return "42")
        }

xpropSpy :: Variable
xpropSpy =
    Variable
        { name = "xprop_spy"
        , runtimeBehavior = Subscription
        , action = Script "xprop" ["-root", "-spy", "_NET_ACTIVE_WINDOW"]
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

subscribeToScript :: (Text -> IO ()) -> FilePath -> [String] -> IO ()
subscribeToScript action scriptPath args = do
    (_, _, stdoutFd, _) <-
        GLib.spawnAsyncWithPipes
            Nothing
            (scriptPath : args)
            Nothing
            [GLib.SpawnFlagsSearchPath]
            Nothing
    channel <- GLib.iOChannelUnixNew stdoutFd
    GLib.iOChannelSetEncoding channel (Just "UTF-8")
    void
        $ GLib.ioAddWatch
            channel
            GLib.PRIORITY_DEFAULT
            [GLib.IOConditionIn, GLib.IOConditionHup]
        $ \_ conditions _ -> do
            if GLib.IOConditionHup `elem` conditions
                then pure False
                else do
                    (status, line, _, _) <- GLib.iOChannelReadLine channel
                    if status == GLib.IOStatusNormal
                        then action line $> True
                        else pure False

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
