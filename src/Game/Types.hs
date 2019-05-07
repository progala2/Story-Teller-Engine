module Game.Types where

import qualified Control.Monad.State.Lazy as S
import qualified Data.Map.Strict as M


data CondType = CondLocal | CondGlobal deriving(Show)
data LocDesc = LocDesc CondType String deriving(Show)
newtype CondId = CondId Int
newtype ObjectsNotExist = ObjectsNotExist [String] deriving(Show)
newtype ItemsInLocation = ItemsInLocation [String] deriving(Show)

type Conditions = (M.Map Int Condition)
data Condition = Condition ObjectsNotExist ItemsInLocation deriving(Show)

data Action = Action [String] String [String] String [String] deriving(Show)
type Actions = [Action]

data LocCanTravel = LocCannotTravel CondType String | LocCanTravel deriving(Show)
newtype LocName = LocName String deriving(Eq, Ord, Show)
data Location = Location {
    lcDescList::(M.Map CondId LocDesc), 
    lcTravelList::(M.Map LocName LocCanTravel), 
    lcObjects::Objects, 
    lcItems::Items, 
    lcActions::Actions,
    lcConditions::Conditions
    } 
type Locations = M.Map LocName Location

newtype Item = Item String
newtype Object = Object String
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