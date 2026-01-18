{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import Data.List (nubBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Monoid (Ap)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Focus as F
import GHC.IO.FD (FD (fdFD))
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import qualified ListT as LT
import Mercury.Runtime.Identified
import Mercury.Variable
import Mercury.Widget
import qualified StmContainers.Map as SM
import qualified StmContainers.Set as SS
import UnliftIO (MonadUnliftIO)

data RuntimeVariable = RuntimeVariable
    { variableValue :: !Text
    , subscribers :: !(IdentifiedSet (MercuryRuntime ()))
    }

ioa :: (MonadIO m) => STM a -> m a
ioa = liftIO . atomically

newRV :: MercuryRuntime RuntimeVariable
newRV = RuntimeVariable "" <$> liftIO SM.newIO

data RuntimeEnvironment = RuntimeEnvironment
    { runtimeVariables :: !(SM.Map Variable RuntimeVariable)
    , uidStore :: !UniqueIDStore
    }

withUID :: a -> MercuryRuntime (Identified a)
withUID a =
    asks uidStore
        >>= ioa . (`identify` a)

addVariable :: Variable -> MercuryRuntime ()
addVariable v = do
    vars <- asks runtimeVariables
    rv <- newRV
    ioa $ SM.insert rv v vars

newtype UID = UID {getUID :: Int}
    deriving (Eq, Ord, Show, Num, Hashable)

subscribeToVariable :: Variable -> MercuryRuntime () -> MercuryRuntime (Identified (MercuryRuntime ()))
subscribeToVariable var action = do
    idAction <- withUID action
    RuntimeEnvironment{..} <- ask
    ioa $
        -- TODO error management
        SM.lookup var runtimeVariables >>= maybe mempty (insert idAction . subscribers)
    return idAction

updateValue :: Variable -> Text -> MercuryRuntime ()
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

newtype MercuryRuntime a = MercuryRuntime (ReaderT RuntimeEnvironment IO a)
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadReader RuntimeEnvironment, MonadUnliftIO)

instance (Semigroup a) => Semigroup (MercuryRuntime a) where
    (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (MercuryRuntime a) where
    mempty = pure mempty

runMercuryRuntime :: MercuryRuntime a -> RuntimeEnvironment -> IO a
runMercuryRuntime (MercuryRuntime m) = runReaderT m

getValue :: Variable -> MercuryRuntime Text
getValue v = do
    vars <- asks runtimeVariables
    ioa $ maybe "" variableValue <$> SM.lookup v vars

evalExpression :: Expression a -> MercuryRuntime a
evalExpression Expression{..} = eval getValue
