module GameParser.Tokens where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

data GameOptionType = GameName | GameVersion | PlayerCapacity 
    | StartingLocation | EndingLocation deriving (Show)
data GameOption = GameOptionString String | GameOptionInt Int deriving(Show)  
type GameOptions = M.Map GameOptionType GameOption

data CondType = Local Int | Global Int | None deriving(Show)

data LocDesc = LocDesc CondType String deriving(Show)
data LocTravel = LocCannotTravel CondType String | LocCanTravel deriving(Show)

data Action = Action [String] String [String] String [String] deriving(Show)
type ObjectsNotExist = [String] 
type ItemsInLocation = [String] 
data Condition = Condition ObjectsNotExist ItemsInLocation deriving(Show)

type LocNameStr = String
data LocationOptionType = LocDescList | LocTravelList 
    | LocObjects | LocItems| LocActions | LocCond deriving (Show)
data LocationOption = LocDescListV (M.Map Int LocDesc) | LocTravelListV (M.Map String LocTravel) 
    | LocStrings [String] | LocActionsV [Action] | LocCondV (M.Map Int Condition) deriving(Show)

type Location = M.Map LocationOptionType LocationOption
type Locations = M.Map LocNameStr Location

instance Ord GameOptionType where
    _ <= GameName = True
    GameName <= _ = False
    _ <= GameVersion = True
    GameVersion <= _ = False
    _ <= PlayerCapacity = True
    PlayerCapacity <= _ = False
    _ <= StartingLocation = True
    StartingLocation <= _ = False
    EndingLocation <= EndingLocation = True

instance Eq GameOptionType where
    GameName == GameName = True
    GameVersion == GameVersion = True
    PlayerCapacity == PlayerCapacity = True
    StartingLocation == StartingLocation = True
    EndingLocation == EndingLocation = True
    _ == _ = False

instance Eq LocationOptionType where
    LocDescList == LocDescList = True
    LocTravelList == LocTravelList = True
    LocObjects == LocObjects = True
    LocItems == LocItems = True
    LocActions == LocActions = True
    _ == _ = False

instance Ord LocationOptionType where
    _ <= LocDescList = True
    LocDescList <= _ = False
    _ <= LocTravelList = True
    LocTravelList <= _ = False
    _ <= LocObjects  = True
    LocObjects <= _  = False
    _ <= LocItems = True
    LocItems <= _ = True
    _ <= LocActions = True
    LocActions <= _ = True
    LocCond <= LocCond = True