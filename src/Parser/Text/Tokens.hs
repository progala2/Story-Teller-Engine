module Parser.Text.Tokens where

import qualified Data.Map.Strict as M
import Parser.Text.Option

type IntroText = String
type OutroText = String

data GameOptionType = GameName | GameVersion | PlayerCapacity 
    | StartingLocation | EndingLocation deriving (Show, Eq, Ord)
data GameOption = GameOptionString String | GameOptionInt Int deriving(Show)  
type GameOptions = M.Map GameOptionType GameOption

data CondType = Local Int | Global Int | None deriving(Show)

data LocDesc = LocDesc CondType String deriving(Show)
data LocTravel = LocCannotTravel CondType String | LocCanTravel deriving(Show)

type Action = M.Map ActionOptionType ActionOption
data ActionOptionType = AotType | AotUsedItems | AotUsedOn 
    | AotAddItemsToLocation | AotComment | AotObjectsRemove 
    | AotCommands deriving(Show, Eq, Ord)
data ActionOption = AoArrString [String] | AoString String deriving(Show)

type Condition = M.Map ConditionOptionType ConditionOption
type Conditions = M.Map Int Condition
data ConditionOptionType = CotObjectsNotExist | CotItemsInLocation | CotPlayerItems deriving(Show, Eq, Ord) 
data ConditionOption = CoArrString [String] deriving(Show)

type LocNameStr = String
data LocationOptionType = LocDescList | LocTravelList 
    | LocObjects | LocItems| LocActions | LocCond deriving (Show, Eq, Ord)
data LocationOption = LocDescListV (M.Map Int LocDesc) | LocTravelListV (M.Map String LocTravel) 
    | LocStrings [String] | LocActionsV [Action] | LocCondV (M.Map Int Condition) deriving(Show)

type Location = M.Map LocationOptionType LocationOption
type LocMap = M.Map LocNameStr Location

data DataType = IntType | StringType deriving(Show)

instance OptionStr GameOption where
    getStrM (GameOptionString str) = Just str
    getStrM _ = Nothing
instance OptionInt GameOption where
    getIntM (GameOptionInt str) = Just str
    getIntM _ = Nothing
    
instance OptionStr ActionOption where
    getStrM (AoString str) = Just str
    getStrM _ = Nothing
instance OptionArrStr ActionOption where
    getArrStrM (AoArrString str) = Just str
    getArrStrM _ = Nothing
    
instance OptionArrStr ConditionOption where
    getArrStrM (CoArrString str) = Just str
    
instance OptionArrStr LocationOption where
    getArrStrM (LocStrings str) = Just str
    getArrStrM _ = Nothing