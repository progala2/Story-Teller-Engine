module Parser.Game.Condition where

import qualified Game.Types as G
import qualified Parser.Text.Tokens as T

condition :: (Int, T.Condition) -> (G.CondId, G.Condition)
condition (k, (T.Condition one iil)) = (G.CondId k, 
  G.Condition (G.ObjectsNotExist (G.Object <$> one)) (G.ItemsInLocation (G.Item <$> iil)))