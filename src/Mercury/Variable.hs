module Mercury.Variable (
    Variable (..),
    ScriptAction (..),
    PollingAction (..),
    SubscriptionAction (..),
    RuntimeBehavior (..),
) where

import Data.Function
import Data.Hashable
import Data.Text (Text)

data Variable = Variable
    { name :: !Text
    , runtimeBehavior :: !RuntimeBehavior
    }

instance Eq Variable where
    (==) = (==) `on` name

instance Ord Variable where
    compare = compare `on` name

instance Hashable Variable where
    hashWithSalt salt var = hashWithSalt salt (name var)

data ScriptAction = Script !FilePath ![String]

data PollingAction
    = PollingCustomIO !(IO Text)
    | PollingScriptAction !ScriptAction

data SubscriptionAction
    = SubscriptionScriptAction !ScriptAction

data RuntimeBehavior
    = Polling {intervalMs :: !Int, action :: !PollingAction}
    | Subscription {script :: !SubscriptionAction}
    | Pure {initialValue :: !Text}
