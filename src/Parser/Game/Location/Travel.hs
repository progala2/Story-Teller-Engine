module Parser.Game.Location.Travel where

import Extensions.Errors
import qualified Game.Types as G
import qualified Parser.Text.Tokens as T

travel :: (String, T.LocTravel) -> (G.LocName, G.LocCanTravel)
travel x = case x of
    (k, (T.LocCannotTravel (T.Local idL) s)) -> (G.LocName k, G.LocCannotTravel G.CondLocal (G.CondId idL) s);
    (k, (T.LocCannotTravel (T.Global idG) s)) -> (G.LocName k, G.LocCannotTravel G.CondGlobal (G.CondId idG) s);
    (k, (T.LocCanTravel)) -> (G.LocName k, G.LocCanTravel)
    (_, T.LocCannotTravel T.None _) -> errCant
