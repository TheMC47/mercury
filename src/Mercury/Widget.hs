{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Mercury.Widget where

import Data.Function (on)
import Data.List (nubBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import Mercury.Variable

getAllVars :: Widget -> [Variable]
getAllVars (Label{..}) = vars text
getAllVars (Box{..}) =
    nubBy ((==) `on` name) $ concatMap getAllVars children
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
    { dependencies :: !(S.Set Text)
    , vars :: ![Variable]
    , eval :: !(VariableEnv -> a)
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

(#) :: Variable -> Expression Text
(#) = use
infixl 9 #

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
