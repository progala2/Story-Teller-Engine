-- |
-- Module      : RealShow
-- Description : User friendly show
--
-- It is used to convert data to string that is without unnecessary constructots names. Only pure strings for user-friendliness.
module Game.RealShow where

import           Data.List (intercalate)

class RealShow a where
  rShow :: a -> String

-- | Provides default solution for table of 'RealShow' data. It produces following string:
-- >>> "Name, Name2, Name3"
instance RealShow a => RealShow [a] where
  rShow [] = ""
  rShow xs = intercalate ", " $ rShow <$> xs