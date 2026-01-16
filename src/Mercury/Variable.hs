module Mercury.Variable where

import qualified Data.Map.Strict as M
import Data.Text (Text)

type VariableEnv = M.Map Text Text

data Variable = Variable
    { name :: !Text
    , runtimeBehavior :: !RuntimeBehavior
    }

data ScriptAction = Script !FilePath ![String]

data PollingAction
    = PollingCustomIO !(IO Text)
    | PollingScriptAction !ScriptAction

data SubscriptionAction
    = SubscriptionScriptAction !ScriptAction

data RuntimeBehavior
    = Polling {intervalMs :: !Int, action :: !PollingAction}
    | Subscription {script :: !SubscriptionAction}
