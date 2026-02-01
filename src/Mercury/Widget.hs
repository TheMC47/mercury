{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Mercury.Widget (
    getAllVars,
    Widget (..),
    Action (..),
    module Mercury.Expression,
) where

import Data.Kind (Type)
import Data.Set qualified as S
import Data.Text (Text)
import Mercury.Expression
import Mercury.Runtime (MercuryRuntime)
import Mercury.Runtime.Rendering.Backend (RenderingBackend)
import Mercury.Variable (Variable)

data Action = Action {runAction :: forall (b :: Type). (RenderingBackend b) => MercuryRuntime b ()}

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
        , onClick :: !Action
        }

getAllVars :: Widget -> S.Set Variable
getAllVars (Label{..}) = dependencies text
getAllVars (Box{..}) =
    S.unions $ map getAllVars children
getAllVars (Button{..}) = getAllVars child
