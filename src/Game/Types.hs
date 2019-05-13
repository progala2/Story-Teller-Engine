module Game.Types where

import qualified Control.Monad.State.Lazy as S
import qualified Data.Map.Strict as M


data CondType = CondLocal | CondGlobal deriving(Show)
data LocDesc = LocDesc String | LocDescCond CondType CondId String deriving(Show)
newtype CondId = CondId Int deriving(Show, Eq, Ord)
newtype DescOrder = DescOrder Int deriving(Show, Eq, Ord)
newtype ObjectsNotExist = ObjectsNotExist [String] deriving(Show)
newtype ItemsInLocation = ItemsInLocation [String] deriving(Show)

type Conditions = (M.Map CondId Condition)
data Condition = Condition ObjectsNotExist ItemsInLocation deriving(Show)

data ActionResult = ArAddLocationItems [Item] | ArRemoveObjects [Object] deriving(Show)
data Action = 
  ActionUseItemsOnObject { 
    auiooItemsUsed::[Item],
    aObject::Object, 
    aComment::String,
    aResults::[ActionResult]
  } 
  | 
  ActionUnique {
    auCommands::[String],
    aObjectM::Maybe Object
    aComment::String,
    aResults::[ActionResult]
  } deriving(Show)
type Actions = [Action]

data LocCanTravel = LocCannotTravel CondType CondId String | LocCanTravel deriving(Show)
newtype LocName = LocName String deriving(Eq, Ord, Show)
data Location = Location {
    lcDescList::(M.Map DescOrder LocDesc), 
    lcTravelList::(M.Map LocName LocCanTravel), 
    lcObjects::Objects, 
    lcItems::Items, 
    lcActions::Actions,
    lcConditions::Conditions
    } 
type Locations = M.Map LocName Location

newtype Item = Item String deriving(Show)
newtype Object = Object String deriving(Show)
type Items = [Item]
type Objects = [Object]

data PlayerStatus = PlayerStatus LocName Items
data GameOptions = GameOptions {
    goGameName::String, 
    goGameVersion::String, 
    goPlayerCapacity::Int,
    goEndingLocation::String
    }
type WorldStatus = (GameOptions, Locations)
type GameStatus = (PlayerStatus, WorldStatus)

type GameStateIO a = S.StateT GameStatus IO a
type GameState a = S.State GameStatus a