{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Mercury.Runtime.Rendering.Handle where

import Data.Text
import Mercury.Runtime (MercuryRuntime)

data Handle m widgetType boxType labelType buttonType = Handle
    { boxToWidget :: boxType -> m widgetType
    , labelToWidget :: labelType -> m widgetType
    , buttonToWidget :: buttonType -> m widgetType
    , renderBox :: Bool -> [widgetType] -> m boxType
    , renderLabel :: Text -> m labelType
    , renderButton :: widgetType -> m buttonType
    , setLabelText :: labelType -> Text -> m ()
    }
