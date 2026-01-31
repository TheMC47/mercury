{-# LANGUAGE OverloadedStrings #-}

module Mercury.Expression (
    Expression (eval, dependencies),
    isStatic,
    useRaw,
    use,
    useOr,
    (#),
) where

import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import Mercury.Variable
import Mercury.Variable.Typed

data Expression a = Expression
    { dependencies :: !(S.Set Variable)
    , eval :: forall m. (Monad m) => (Variable -> m Text) -> m a
    }

isStatic :: Expression a -> Bool
isStatic e = S.null (dependencies e)

useRaw :: Variable -> Expression Text
useRaw v =
    Expression
        { dependencies = S.singleton v
        , eval = ($ v)
        }

use :: TypedVariable a -> Expression (Maybe a)
use tv = decode tv <$> useRaw (rawVariable tv)

useOr :: a -> TypedVariable a -> Expression a
useOr fallback tv = fromMaybe fallback <$> use tv

(#) :: TypedVariable Text -> Expression Text
(#) = useOr ""
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
