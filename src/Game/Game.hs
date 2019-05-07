module Game.Game where

import System.Console.ANSI
import Game.CommandParser
import qualified Control.Monad.State.Lazy as S
import qualified Data.Map.Strict as M


data CondType = Local Int | Global Int | None deriving(Show)
data LocDesc = LocDesc CondType String deriving(Show)
data Action = Action [String] String [String] String [String] deriving(Show)
type ObjectsNotExist = [String] 
type ItemsInLocation = [String] 
data Condition = Condition ObjectsNotExist ItemsInLocation deriving(Show)

data LocTravel = LocCannotTravel CondType String | LocCanTravel deriving(Show)
type LocationName = String
data Location = Location {
    lcDescList::(M.Map Int LocDesc), 
    lcTravelList::(M.Map LocationName LocTravel), 
    lcObjects::[String], 
    lcItems::[String], 
    lcActions::[Action],
    lcConditions::(M.Map Int Condition)
    } 
type Locations = M.Map LocationName Location
type Items = [String]
data PlayerStatus = PlayerStatus LocationName Items
data GameOptions = GameOptions {
    goGameName::String, 
    goGameVersion::String, 
    goPlayerCapacity::Int,
    goStartingLocation::String, 
    goEndingLocation::String
    }
type WorldStatus = (GameOptions, Locations)
type GameStatus = (PlayerStatus, WorldStatus)

type GameStateIO a = S.StateT GameStatus IO a
type GameState a = S.State GameStatus a

runGame :: GameStateIO ()
runGame = do
    S.lift $ clearScreen
    (_, (opts, loc)) <- S.get
    printL $ goGameName opts
    S.put (PlayerStatus (goStartingLocation opts) [], (opts, loc))
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
                    return $ "You travel to: " ++ dest
                Nothing -> return "There is no destination like that..."
printL :: (Show a) => a -> GameStateIO ()
printL str = S.lift $ print str