module Parser.Game.Location.Travel where

import Extensions.Errors
import qualified Game.GameState as G
import qualified Parser.Text.Tokens as T

travel :: String -> (String, T.LocTravel) -> Either String (G.LocName, G.LocCanTravel)
travel n x = case x of
    (k, (T.LocCannotTravel (T.Local idL) s)) -> cantSameLoc k (G.LocName k, G.LocCannotTravel G.CondLocal (G.CondId idL) s);
    (k, (T.LocCannotTravel (T.Global idG) s)) -> cantSameLoc k (G.LocName k, G.LocCannotTravel G.CondGlobal (G.CondId idG) s);
    (k, (T.LocCanTravel)) -> cantSameLoc k (G.LocName k, G.LocCanTravel)
    (_, T.LocCannotTravel T.None _) -> errCant
    where 
      cantSameLoc k r = if (k == n)
        then Left "You can't have on travel list the same place you are in!"
        else Right r 
