module Game.Game where

import System.Console.ANSI
import Game.CommandParser
import qualified Control.Monad.State.Lazy as S
import qualified Data.Map.Strict as M
import qualified GameEngine as Ge

type LocationName = String
type Items = [String]
data PlayerStatus = PlayerStatus LocationName Items
type GameStatus = (PlayerStatus, (Ge.GameOptions, Ge.Locations))

type GameStateIO a = S.StateT GameStatus IO a
type GameState a = S.State GameStatus a

runGame :: GameStateIO ()
runGame = do
    S.lift $ clearScreen
    (_, (opts, loc)) <- S.get
    printL $ rdHp opts Ge.GameName
    S.put (PlayerStatus (rdHp opts Ge.StartingLocation) [], (opts, loc))
    ss <- S.get
    line <- S.lift $ getLine
    printL $ S.evalState (either (\_ -> return "ss" ) handleCommand (parseCommand line)) ss
    runGame
      where 
        rdHp opts k = case opts M.! k of Ge.GameOptionString str -> str; _ -> error "Unexpected error"

handleCommand :: Command -> GameState String
handleCommand (Travel dest) = do
    (status, (opts, lc)) <- S.get
    case status of
        PlayerStatus _ items ->
            case lc M.!? dest of
                Just _ -> do
                    S.put (PlayerStatus dest items, (opts, lc))
                    return $ "You travel to: " ++ dest
                Nothing -> return "There is no destination like that..."
printL :: (Show a) => a -> GameStateIO ()
printL str = S.lift $ print str