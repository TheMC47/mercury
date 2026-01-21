{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Mercury.Widget (getAllVars, Widget (..), isStatic, use, (#), Expression (eval, dependencies)) where

import Control.Monad.Identity
import Data.Function (on)
import Data.List (nubBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import Mercury.Variable

getAllVars :: Widget -> S.Set Variable
getAllVars (Label{..}) = dependencies text
getAllVars (Box{..}) =
    S.unions $ map getAllVars children
getAllVars (Button{..}) = getAllVars child

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

data Expression a = Expression
    { dependencies :: !(S.Set Variable)
    , eval :: forall m. (Monad m) => (Variable -> m Text) -> m a
    }

isStatic :: Expression a -> Bool
isStatic e = S.null (dependencies e)

use :: Variable -> Expression Text
use v =
    Expression
        { dependencies = S.singleton v
        , eval = ($ v) -- Just apply it
        }

(#) :: Variable -> Expression Text
(#) = use
infixl 9 #

instance Functor Expression where
    fmap f e = e{eval = fmap f . eval e}

instance Applicative Expression where
    pure x =
        Expression
            { dependencies = S.empty
            , eval = const (pure x)
            }
    (Expression depsF evalF) <*> (Expression depsX evalX) =
        Expression
            { dependencies = depsF `S.union` depsX
            , eval = \l -> evalF l <*> evalX l
            }
