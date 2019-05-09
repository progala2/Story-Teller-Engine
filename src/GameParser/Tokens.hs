module GameParser.Tokens where

import qualified Data.Map.Strict as M

data GameOptionType = GameName | GameVersion | PlayerCapacity 
    | StartingLocation | EndingLocation deriving (Show, Eq, Ord)
data GameOption = GameOptionString String | GameOptionInt Int deriving(Show)  
type GameOptions = M.Map GameOptionType GameOption

data CondType = Local Int | Global Int | None deriving(Show)

data LocDesc = LocDesc CondType String deriving(Show)
data LocTravel = LocCannotTravel CondType String | LocCanTravel deriving(Show)

type Action = M.Map ActionOptionType ActionOption
data ActionOptionType = AotType | AotUsedItems | AotUsedOn 
    | AotAddItemsToLocation | AotComment | AotObjectsRemove deriving(Show, Eq, Ord)
data ActionOption = AoArrString [String] | AoString String deriving(Show)

type ObjectsNotExist = [String] 
type ItemsInLocation = [String] 
data Condition = Condition ObjectsNotExist ItemsInLocation deriving(Show)

type LocNameStr = String
data LocationOptionType = LocDescList | LocTravelList 
    | LocObjects | LocItems| LocActions | LocCond deriving (Show, Eq, Ord)
data LocationOption = LocDescListV (M.Map Int LocDesc) | LocTravelListV (M.Map String LocTravel) 
    | LocStrings [String] | LocActionsV [Action] | LocCondV (M.Map Int Condition) deriving(Show)

type Location = M.Map LocationOptionType LocationOption
type Locations = M.Map LocNameStr Location

data DataType = IntType | StringType deriving(Show)