{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Mercury.Runtime (
    RuntimeEnvironment (..),
    MercuryRuntime,
    runMercuryRuntime,
    getUID,
    UID,
    evalExpression,
    addVariable,
    getValue,
    updateValue,
    subscribeToVariable,
    ioa,
    closeWindows,
    startApplication,
) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, ask, asks)
import Control.Monad.State (MonadState, StateT (runStateT), evalStateT, get, gets, modify, put)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.Function
import Data.Functor
import Data.GI.Base (AttrOp (On, (:=)), new, set)
import Data.Hashable
import Data.IORef
import Data.List (nubBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Monoid (Ap)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (traceM)
import qualified Focus as F
import GHC.IO.FD (FD (fdFD))
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import qualified ListT as LT
import Mercury.Runtime.Identified
import Mercury.Runtime.Rendering.Backend
import Mercury.Variable
import Mercury.Widget
import qualified StmContainers.Map as SM
import qualified StmContainers.Set as SS
import UnliftIO (IORef, MonadUnliftIO)

data RuntimeVariable b = RuntimeVariable
    { variableValue :: !Text
    , subscribers :: !(IdentifiedSet (MercuryRuntime b ()))
    }

ioa :: (MonadIO m) => STM a -> m a
ioa = liftIO . atomically

newRV :: MercuryRuntime b (RuntimeVariable b)
newRV = RuntimeVariable "" <$> liftIO SM.newIO

data RuntimeEnvironment b = (RenderingBackend b) =>
    RuntimeEnvironment
    { runtimeVariables :: !(SM.Map Variable (RuntimeVariable b))
    , uidStore :: !UniqueIDStore
    , renderingBackend :: !b
    , applicationInstance :: !(IORef (Maybe (Application b)))
    }

startApplication :: forall b. (RenderingBackend b) => MercuryRuntime b (Application b)
startApplication = do
    app <- liftIO $ createApplication @b "com.example.mercuryapp"
    applicationInstanceRef <- asks applicationInstance
    liftIO $ writeIORef applicationInstanceRef (Just app)
    return app

withApplication :: (Application b -> MercuryRuntime b a) -> MercuryRuntime b (Maybe a)
withApplication action = do
    appM <- liftIO . readIORef =<< asks applicationInstance
    mapM action appM

closeWindows :: forall b. (RenderingBackend b) => MercuryRuntime b ()
closeWindows = void $ withApplication (\(app :: Application b) -> killAllWindows @b app)

withUID :: a -> MercuryRuntime b (Identified a)
withUID a =
    asks uidStore
        >>= ioa . (`identify` a)

addVariable :: Variable -> MercuryRuntime b ()
addVariable v = do
    vars <- asks runtimeVariables
    rv <- newRV
    ioa $ SM.insert rv v vars

newtype UID = UID {getUID :: Int}
    deriving (Eq, Ord, Show, Num, Hashable)

subscribeToVariable :: Variable -> MercuryRuntime b () -> MercuryRuntime b (Identified (MercuryRuntime b ()))
subscribeToVariable var action = do
    idAction <- withUID action
    RuntimeEnvironment{..} <- ask
    ioa $
        -- TODO error management
        SM.lookup var runtimeVariables >>= maybe mempty (insert idAction . subscribers)
    return idAction

updateValue :: Variable -> Text -> MercuryRuntime b ()
updateValue var val = do
    RuntimeEnvironment{..} <- ask
    oldRuntimeVariable <-
        ioa $
            SM.focus (F.lookup <* F.adjust (\rv -> rv{variableValue = val})) var runtimeVariables
    let hook RuntimeVariable{variableValue = oldValue, subscribers} =
            when (oldValue /= val) $ do
                subs <- ioa $ LT.toList $ SM.listT subscribers
                traverse_ snd subs
    maybe mempty hook oldRuntimeVariable

newtype MercuryRuntime b a = MercuryRuntime (ReaderT (RuntimeEnvironment b) IO a)
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadReader (RuntimeEnvironment b), MonadUnliftIO)

instance (Semigroup a) => Semigroup (MercuryRuntime b a) where
    (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (MercuryRuntime b a) where
    mempty = pure mempty

runMercuryRuntime :: MercuryRuntime b a -> RuntimeEnvironment b -> IO a
runMercuryRuntime (MercuryRuntime m) = runReaderT m

getValue :: Variable -> MercuryRuntime b Text
getValue v = do
    vars <- asks runtimeVariables
    ioa $ maybe "" variableValue <$> SM.lookup v vars

evalExpression :: Expression a -> MercuryRuntime b a
evalExpression e = eval e getValue
