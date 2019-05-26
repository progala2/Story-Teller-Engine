module Game.Types where

import qualified Control.Monad.State.Lazy as S
import qualified Data.Map.Strict as M
import qualified Data.Set as Set


data CondType = CondLocal | CondGlobal deriving(Show)
data LocDesc = LocDesc String | LocDescCond CondType CondId String deriving(Show)
newtype CondId = CondId Int deriving(Show, Eq, Ord)
newtype DescOrder = DescOrder Int deriving(Show, Eq, Ord)
type ObjectsNotExist = [Object]
type ItemsInLocation = [Item]

type Conditions = (M.Map CondId Condition)
data Condition = Condition ObjectsNotExist ItemsInLocation deriving(Show)

data ActionResult = ArAddLocationItems [Item] | ArRemoveObjects [Object] deriving(Show)
data Action = 
  ActionUseItemsOnObject { 
    aItemsUsed::ItemSet,
    aObject::Object, 
    aComment::String,
    aResults::[ActionResult]
  } 
  | 
  ActionUnique {
    auCommands::[String],
    aObjectM::Maybe Object,
    aComment::String,
    aResults::[ActionResult]
  } deriving(Show)
type Actions = [Action]

data LocCanTravel = LocCannotTravel CondType CondId String | LocCanTravel deriving(Show)
newtype LocName = LocName String deriving(Eq, Ord, Show)
data Location = Location {
    lcDescList::(M.Map DescOrder LocDesc), 
    lcTravelList::(M.Map LocName LocCanTravel), 
    lcObjects::ObjectSet, 
    lcItems::ItemSet, 
    lcActions::Actions,
    lcConditions::Conditions
    } deriving(Show)
type LocationP = (LocName, Location)
type Locations = M.Map LocName Location
type DescMap = (M.Map DescOrder LocDesc)
type TravelMap = (M.Map LocName LocCanTravel)

addItemsToLoc :: [Item] -> LocationP -> LocationP
addItemsToLoc is (nm, Location d t o itms a c) = 
  (nm, Location d t o (Set.union (Set.fromList is) itms) a c)
removeObjectsFromLoc :: [Object] -> LocationP -> LocationP
removeObjectsFromLoc os (nm, Location d t objs i a c) = 
  (nm, Location d t (objs Set.\\ (Set.fromList os)) i a c )
removeItemFromLoc :: Item -> LocationP -> LocationP
removeItemFromLoc i (nm, Location d t objs itms a c) = 
  (nm, Location d t objs (Set.delete i itms) a c)
itemExists :: Item -> LocationP -> Bool
itemExists it (_, loc) = it `Set.member` lcItems loc

lcCondition :: Location -> CondId -> Condition
lcCondition l cid = lcConditions l M.! cid

newtype Item = Item String deriving(Eq, Ord, Show)
newtype Object = Object String deriving(Eq, Ord, Show)
type ItemSet = Set.Set Item
type ObjectSet = Set.Set Object

data PlayerStatus = PlayerStatus LocationP ItemSet deriving(Show)
data GameOptions = GameOptions {
    goGameName::String, 
    goGameVersion::String, 
    goPlayerCapacity::Int,
    goEndingLocation::LocName
    } deriving(Show)
type WorldStatus = (GameOptions, Locations)
type GameStatus = (PlayerStatus, WorldStatus)

type GameStateIO a = S.StateT GameStatus IO a
type GameState a = S.State GameStatus a
type GameStateM m a = S.StateT GameStatus m a

getLocations :: GameState (LocationP, Locations)
getLocations = do
  (PlayerStatus currLoc _, (_, locations)) <- S.get
  return (currLoc, locations)