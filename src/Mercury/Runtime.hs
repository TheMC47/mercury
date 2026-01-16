{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mercury.Runtime (RuntimeEnvironment (..), MercuryRuntime, runMercuryRuntime) where

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
import Mercury.Variable
import System.IO
import System.Process
import Text.Read

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
