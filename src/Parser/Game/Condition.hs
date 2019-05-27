module Parser.Game.Condition where

import qualified Game.Types as G
import qualified Parser.Text.Tokens as T
import qualified Parser.Text.Option as T
import qualified Data.Map.Strict as M

condition :: (Int, T.Condition) -> (G.CondId, G.Condition)
condition (k, cond) = (G.CondId k, 
  G.Condition (hp T.CotObjectsNotExist G.Object) (hp T.CotItemsInLocation G.Item) (hp T.CotPlayerItems G.Item))
  where
    hp = T.saGetD cond
conditionsLoader :: T.Conditions -> G.Conditions
conditionsLoader cs = M.fromList (condition <$> M.toList cs)