module Game.Runner(runGame) where

import System.Console.ANSI
import Game.CommandParser
import qualified Control.Monad.State.Lazy as S
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Game.Types
import Data.List
import Data.Functor.Identity (Identity(..))

runGame :: GameStateIO ()
runGame = gameLoop "New Game has been started!"
      where
        currLoc ((PlayerStatus cL _), (_, locs)) = locs M.! cL
        gameLoop str = do 
          S.lift $ clearScreen
          putStrLnL str
          ss@((PlayerStatus (LocName loc) _), _) <- S.get
          putStrLnL $ "Location: " ++ loc
          putStrLnL $ showDescription ss (currLoc ss)
          putStrLnL "What now? : "
          line <- S.lift $ getLine
          res <- raiseStateToT (either (\_ -> return "You wrote something really wrong!") handleCommand (parseCommand line)) 
          gameLoop res
          --rdHp opts k = case opts M.! k of Ge.GameOptionString str -> str; _ -> error "Unexpected error"

handleCommand :: Command -> GameState String
handleCommand (Travel dest) = do
  gs@((PlayerStatus pLocName items), (opts, locations)) <- S.get
  let currLoc = locations M.! pLocName
  case locations M.!? dest of
    Just _ ->
      case lcTravelList currLoc M.!? dest of
        Just LocCanTravel -> travel items opts locations
        Just (LocCannotTravel ct cid str) -> if checkCondition gs currLoc (ct, cid) 
          then travel items opts locations
          else return str
        Nothing -> nothing
      
    Nothing -> nothing
  where
    nothing = return $ "There is no "++ (show dest) ++ "..."
    travel :: ItemSet -> GameOptions -> Locations -> GameState String
    travel items opts lc = do
      S.put (PlayerStatus dest items, (opts, lc))
      return $ "You travel to: " ++ (show dest)
handleCommand (ItemsOnObject items obj) = return $ "Not yet implemented" ++ (show items) ++ (show obj)

showDescription :: GameStatus -> Location -> String
showDescription gs l = foldl' sepByLn [] $ desc <$> M.elems (lcDescList l)
  where
    desc (LocDesc str) = str
    desc (LocDescCond ct cid str) = if checkCondition gs l (ct, cid) then str else ""

checkCondition :: GameStatus -> Location -> (CondType, CondId) -> Bool
checkCondition (_, _) loc (ct, cid) = case ct of
  CondLocal -> condition $ lcCondition loc cid
  CondGlobal -> False
  where 
    condition (Condition objs items) = not (any objCond objs) && all itemCond items
      where 
        objCond obj = Set.member obj (lcObjects loc)
        itemCond item = Set.member item (lcItems loc)

-- printL :: (Show a) => a -> GameStateIO ()
-- printL str = S.lift $ print str
--putStrL :: String -> GameStateIO ()
--putStrL str = S.lift $ putStr str
putStrLnL :: String -> GameStateIO ()
putStrLnL str = S.lift $ putStrLn str
sepByLn :: String -> String -> String
sepByLn = sepBy "\n" (++)

sepBy :: a -> (a -> a -> a) -> a -> a -> a
sepBy s f a b = a `f` s `f` b
raiseStateToT :: Monad m => S.StateT s Identity a -> S.StateT s m a
raiseStateToT = S.mapStateT (\(Identity (s, a)) -> return (s, a))