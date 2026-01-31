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
    updateTypedValue,
    withTypedValue,
    getTypedValue,
    subscribeToVariable,
    ioa,
    closeWindows,
    startApplication,
) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, ask, asks)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Foldable
import Data.Hashable
import Data.IORef
import Data.Text (Text)
import Focus qualified as F
import ListT qualified as LT
import Mercury.Expression
import Mercury.Runtime.Identified
import Mercury.Runtime.Rendering.Backend
import Mercury.Variable
import Mercury.Variable.Typed
import StmContainers.Map qualified as SM
import UnliftIO (MonadUnliftIO)

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

startApplication :: (RenderingBackend b) => MercuryRuntime b (Application b)
startApplication = do
    app <- liftIO $ createApplication "com.example.mercuryapp"
    applicationInstanceRef <- asks applicationInstance
    liftIO $ writeIORef applicationInstanceRef (Just app)
    return app

withApplication :: (Application b -> MercuryRuntime b a) -> MercuryRuntime b (Maybe a)
withApplication action = do
    appM <- liftIO . readIORef =<< asks applicationInstance
    mapM action appM

closeWindows :: (RenderingBackend b) => MercuryRuntime b ()
closeWindows = void $ withApplication killAllWindows

withUID :: a -> MercuryRuntime b (Identified a)
withUID a =
    asks uidStore
        >>= ioa . (`identify` a)

addVariable :: Variable -> MercuryRuntime b ()
addVariable v = do
    vars <- asks runtimeVariables
    rv <- newRV
    let rv' = case runtimeBehavior v of
            Pure val -> rv{variableValue = val}
            _ -> rv
    ioa $ SM.insert rv' v vars

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

updateTypedValue :: TypedVariable a -> a -> MercuryRuntime b ()
updateTypedValue tv val = updateValue (rawVariable tv) (encode tv val)

getTypedValue :: TypedVariable a -> MercuryRuntime b (Maybe a)
getTypedValue tv =
    decode tv <$> getValue (rawVariable tv)

withTypedValue :: TypedVariable a -> (a -> MercuryRuntime b c) -> MercuryRuntime b (Maybe c)
withTypedValue tv act = do
    v <- getTypedValue tv
    mapM act v

evalExpression :: Expression a -> MercuryRuntime b a
evalExpression e = eval e getValue
