{-# LANGUAGE OverloadedStrings #-}

module Mercury.Variable.Typed (
    Codec (..),
    encode,
    decode,
    TypedVariable,
    HasCodec (..),
    typedVar,
    pureVar,
    pollingVar,
    pollingScriptVar,
    subscriptionVar,
    rawVariable,
    typedCodec,
    textCodec,
    intCodec,
    doubleCodec,
    boolCodec,
) where

import Data.Function
import Data.Text (Text)
import qualified Data.Text as T
import Mercury.Variable
import Text.Read (readMaybe)

data Codec a = Codec
    { cEncode :: !(a -> Text)
    , cDecode :: !(Text -> Maybe a)
    }

data TypedVariable a = TypedVariable
    { variable :: !Variable
    , codec :: !(Codec a)
    }

instance Eq (TypedVariable a) where
    (==) = (==) `on` variable

instance Ord (TypedVariable a) where
    compare = compare `on` variable

class HasCodec a where
    varCodec :: Codec a

instance HasCodec Text where
    varCodec = textCodec

instance HasCodec Int where
    varCodec = intCodec

instance HasCodec Double where
    varCodec = doubleCodec

instance HasCodec Bool where
    varCodec = boolCodec

encode :: TypedVariable a -> a -> Text
encode tv = cEncode (codec tv)

decode :: TypedVariable a -> Text -> Maybe a
decode tv = cDecode (codec tv)

textCodec :: Codec Text
textCodec = Codec id Just

intCodec :: Codec Int
intCodec = Codec (T.pack . show) (readMaybe . T.unpack)

doubleCodec :: Codec Double
doubleCodec = Codec (T.pack . show) (readMaybe . T.unpack)

boolCodec :: Codec Bool
boolCodec = Codec boolEncode boolDecode
  where
    boolEncode True = "true"
    boolEncode False = "false"
    boolDecode t = case T.toLower t of
        "true" -> Just True
        "1" -> Just True
        "false" -> Just False
        "0" -> Just False
        _ -> Nothing

typedVar :: Text -> Codec a -> RuntimeBehavior -> TypedVariable a
typedVar n c rb = TypedVariable (Variable n rb) c

pureVar :: (HasCodec a) => Text -> a -> TypedVariable a
pureVar n initial =
    let c = varCodec
     in TypedVariable (Variable n (Pure (cEncode c initial))) c

pollingVar :: (HasCodec a) => Text -> Int -> IO a -> TypedVariable a
pollingVar n interval io =
    let c = varCodec
     in TypedVariable (Variable n (Polling interval (PollingCustomIO (cEncode c <$> io)))) c

pollingScriptVar :: (HasCodec a) => Text -> Int -> ScriptAction -> TypedVariable a
pollingScriptVar n interval act =
    TypedVariable (Variable n (Polling interval (PollingScriptAction act))) varCodec

subscriptionVar :: (HasCodec a) => Text -> SubscriptionAction -> TypedVariable a
subscriptionVar n act = TypedVariable (Variable n (Subscription act)) varCodec

rawVariable :: TypedVariable a -> Variable
rawVariable = variable

typedCodec :: TypedVariable a -> Codec a
typedCodec = codec
