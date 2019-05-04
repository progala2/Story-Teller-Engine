module GameEngine where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

data GameOption = GameName String | GameVersion String | PlayerCapacity Int 
    | StartingLocation String | EndingLocation String deriving(Show)  
type GameOptions = S.Set GameOption

data CondType = Local Int | Global Int | None deriving(Show)

data LocDesc = LocDesc CondType String deriving(Show)
data LocTravel = LocCannotTravel CondType String | LocCanTravel deriving(Show)

data Action = Action [String] String [String] String [String] deriving(Show)
type ObjectsNotExist = [String] 
type ItemsInLocation = [String] 
data Condition = Condition ObjectsNotExist ItemsInLocation deriving(Show)

type LocNameStr = String
data LocationOption = LocDescList (M.Map Int LocDesc) | LocTravelList (M.Map String LocTravel) 
    | LocObjects [String] | LocItems [String] | LocActions [Action] | LocCond (M.Map Int Condition) deriving(Show)

type Location = S.Set LocationOption
type Locations = M.Map LocNameStr Location

instance Ord GameOption where
    GameName _ <= GameName _ = True
    GameVersion _ <= GameVersion _ = True
    PlayerCapacity _ <= PlayerCapacity _ = True
    StartingLocation _ <= StartingLocation _ = True
    EndingLocation _ <= EndingLocation _ = True
    _ <= _ = False

instance Eq GameOption where
    GameName _ == GameName _ = True
    GameVersion _ == GameVersion _ = True
    PlayerCapacity _ == PlayerCapacity _ = True
    StartingLocation _ == StartingLocation _ = True
    EndingLocation _ == EndingLocation _ = True
    _ == _ = False

instance Eq LocationOption where
    LocDescList _ == LocDescList _ = True
    LocTravelList _ == LocTravelList _ = True
    LocObjects _ == LocObjects _  = True
    LocItems _ == LocItems _ = True
    LocActions _ == LocActions _ = True
    _ == _ = False

instance Ord LocationOption where
    LocDescList _ <= LocDescList _ = True
    LocTravelList _ <= LocTravelList _ = True
    LocObjects _ <= LocObjects _  = True
    LocItems _ <= LocItems _ = True
    LocActions _ <= LocActions _ = True
    _ <= _ = False