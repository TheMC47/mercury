module Mercury.Prelude where

import Data.Text qualified as T

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show
