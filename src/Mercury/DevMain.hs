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
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
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
            ]
        }

activate :: Gtk.Application -> IO ()
activate app = do
    variables <- newIORef M.empty

    (widget, liveComponents) <- runWriterT $ render (M.fromList [("cpu_usage", "14")]) myWidget

    let liveComponentIndex = buildLiveComponentIndex liveComponents
    let allVars = getAllVars myWidget
    let updateVariable :: Variable -> IO ()
        updateVariable Variable{..} = do
            val <- getValue
            updated <- atomicModifyIORef' variables (\env -> let updated = M.insert name val env in (updated, updated))
            traverse_ (updateComponent updated) (M.findWithDefault [] name liveComponentIndex)
        intializeVariable :: Variable -> IO ()
        intializeVariable var = gPoll 1000 (updateVariable var)

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

data Variable = Variable
    { name :: !Text
    , getValue :: !(IO Text)
    }

timestampVar :: Variable
timestampVar =
    Variable
        { name = "current_time"
        , getValue = tshow <$> getCurrentTime
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
cpuUsage = Variable{name = "cpu_usage", getValue = pure "42"}

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
    { labelWidget :: Gtk.Label
    , labelExpr :: Expression Text
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
