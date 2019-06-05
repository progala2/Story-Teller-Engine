module Game.RealShow where

import           Data.List (intercalate)

class RealShow a where
  rShow :: a -> String

instance RealShow a => RealShow [a] where
  rShow [] = ""
  rShow xs = intercalate ", " $ rShow <$> xs