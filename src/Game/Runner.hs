module Game.Runner(runGame) where

import System.Console.ANSI
import Game.CommandParser
import qualified Control.Monad.State.Lazy as S
import qualified Data.Map.Strict as M
import Game.Types
import Data.List

runGame :: GameStateIO ()
runGame = do
    S.lift $ clearScreen
    ss@((PlayerStatus (LocName loc) _), _) <- S.get
    putStrLnL $ "Location: " ++ loc
    putStrLnL $ showDescription ss (currLoc ss)
    putStrLnL "What now? : "
    line <- S.lift $ getLine
    printL $ S.evalState (either (\_ -> return "ss" ) handleCommand (parseCommand line)) ss
    runGame
      where
        currLoc ((PlayerStatus cL _), (_, locs)) = locs M.! cL 
        --rdHp opts k = case opts M.! k of Ge.GameOptionString str -> str; _ -> error "Unexpected error"

handleCommand :: Command -> GameState String
handleCommand (Travel dest) = do
    (status, (opts, lc)) <- S.get
    case status of
        PlayerStatus _ items ->
            case lc M.!? dest of
                Just _ -> do
                    S.put (PlayerStatus dest items, (opts, lc))
                    return $ "You travel to: " ++ (show dest)
                Nothing -> return $ "There is no "++ (show dest) ++ "..." ++(show $ ((== dest) <$> M.keys lc))
handleCommand (ItemsOnObject items obj) = return $ "Not yet implemented" ++ (show items) ++ (show obj)

showDescription :: GameStatus -> Location -> String
showDescription _ l = foldl' sepByLn [] $ desc <$> M.elems (lcDescList l)
  where
    desc (LocDesc str) = str
    desc (LocDescCond _ _ str) = str

printL :: (Show a) => a -> GameStateIO ()
printL str = S.lift $ print str
--putStrL :: String -> GameStateIO ()
--putStrL str = S.lift $ putStr str
putStrLnL :: String -> GameStateIO ()
putStrLnL str = S.lift $ putStrLn str
sepByLn :: String -> String -> String
sepByLn = sepBy "\n" (++)

sepBy :: a -> (a -> a -> a) -> a -> a -> a
sepBy s f a b = a `f` s `f` b