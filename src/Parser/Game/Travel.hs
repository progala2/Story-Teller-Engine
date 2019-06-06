-- |
-- Module: Parser.Game.Travel
-- Description: Parsing Action token to Game's Action data.
module Parser.Game.Travel where

import           Parser.Errors
import           Extensions.Errors
import qualified Game.GameState as G
import qualified Parser.Text.Tokens as T
import qualified Data.Map as M

-- | Converts '(T.LocNameStr, T.LocTravel)' to '(G.LocName, G.LocCanTravel)'. Get the 'T.LocMap' tokens and the name of the location to performs consistency check.
travel :: T.LocMap -> T.LocNameStr -> (T.LocNameStr, T.LocTravel) -> Either LoaderError (G.LocName, G.LocCanTravel)
travel tLocs n x = case x of
    (k, (T.LocCannotTravel (T.Local idL) s)) -> check k (G.LocName k, G.LocCannotTravel G.CondLocal (G.CondId idL) s);
    (k, (T.LocCannotTravel (T.Global idG) s)) -> check k (G.LocName k, G.LocCannotTravel G.CondGlobal (G.CondId idG) s);
    (k, (T.LocCanTravel)) -> check k (G.LocName k, G.LocCanTravel)
    (_, T.LocCannotTravel T.None _) -> errCant
    where 
      check k r = isThereLoc k r >> cantSameLoc k r
      isThereLoc k r= if M.member k tLocs 
        then Right r
        else Left "There is no such location to travel!"
      cantSameLoc k r = if (k == n)
        then Left "You can't have on travel list the same place you are in!"
        else Right r 
