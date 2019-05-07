module Game.Runner(runGame) where

import System.Console.ANSI
import Game.CommandParser
import qualified Control.Monad.State.Lazy as S
import qualified Data.Map.Strict as M
import Game.Types

runGame :: GameStateIO ()
runGame = do
    S.lift $ clearScreen
    (_, (opts, loc)) <- S.get
    ss <- S.get
    line <- S.lift $ getLine
    printL $ S.evalState (either (\_ -> return "ss" ) handleCommand (parseCommand line)) ss
    runGame
      --where 
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
                Nothing -> return "There is no destination like that..."
printL :: (Show a) => a -> GameStateIO ()
printL str = S.lift $ print str