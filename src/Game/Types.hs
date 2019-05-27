module Game.Types where

import qualified Control.Monad.State.Strict as S
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Data.List
import           Data.Foldable (foldlM)

data CondType = CondLocal | CondGlobal deriving(Show, Read)
data LocDesc = LocDesc String | LocDescCond CondType CondId String deriving(Show, Read)
newtype CondId = CondId Int deriving(Show, Eq, Ord, Read)
newtype DescOrder = DescOrder Int deriving(Show, Eq, Ord, Read)
type ObjectsNotExist = [Object]
type ItemsInLocation = [Item]
type PlayerItems = [Item]

type Conditions = (M.Map CondId Condition)
data Condition = Condition ObjectsNotExist ItemsInLocation PlayerItems deriving(Show, Read)

data ActionResult = ArAddLocationItems [Item] | ArRemoveObjects [Object] deriving(Show, Read)
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
  } deriving(Show, Read)
type Actions = [Action]

data LocCanTravel = LocCannotTravel CondType CondId String | LocCanTravel deriving(Show, Read)
newtype LocName = LocName String deriving(Eq, Ord, Show, Read)
data Location = Location {
    lcDescList::(M.Map DescOrder LocDesc), 
    lcTravelList::(M.Map LocName LocCanTravel), 
    lcObjects::ObjectSet, 
    lcItems::ItemSet, 
    lcActions::Actions,
    lcConditions::Conditions
    } deriving(Show, Read)
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
goCondition :: GameOptions -> CondId -> Condition
goCondition l cid = goConditions l M.! cid

newtype Item = Item String deriving(Eq, Ord, Show, Read)
newtype Object = Object String deriving(Eq, Ord, Show, Read)
type ItemSet = Set.Set Item
type ObjectSet = Set.Set Object

data PlayerStatus = PlayerStatus LocationP ItemSet deriving(Show, Read)
data GameOptions = GameOptions {
    goGameName::String, 
    goGameVersion::String, 
    goPlayerCapacity::Int,
    goEndingLocation::LocName,
    goConditions::Conditions,
    goIntro::String,
    goOutro::String
    } deriving(Show, Read)
type WorldStatus = (GameOptions, Locations)
type GameStatus = (PlayerStatus, WorldStatus)

type GameStateIO a = S.StateT GameStatus IO a
type GameState a = S.State GameStatus a
type GameStateM m a = S.StateT GameStatus m a

getLocations :: Monad m => GameStateM m (LocationP, Locations)
getLocations = do
  (PlayerStatus currLoc _, (_, locations)) <- S.get
  return (currLoc, locations)
getPlayerStatus :: Monad m => GameStateM m PlayerStatus
getPlayerStatus = do
  (ps, _) <- S.get
  return ps

itemSetFromList :: [String] -> ItemSet
itemSetFromList = Set.fromList . (Item <$>)

showDescription :: Monad m => GameStateM m String
showDescription = do 
  (PlayerStatus (_, l) _) <- getPlayerStatus 
  descL <- foldlM desc [] $ M.elems (lcDescList l)
  return $ foldl' sepByLn [] descL
  where
    desc ss (LocDesc str) = return $ str:ss
    desc ss (LocDescCond ct cid str) = checkConditionWith (ct, cid) (str:ss) ss


checkConditionWith :: Monad m => (CondType, CondId) -> a -> a -> GameStateM m a
checkConditionWith p t f = checkCondition p >>= (\c -> return $ if c then t else f)

checkConditionWithM :: Monad m => (CondType, CondId) -> GameStateM m a -> GameStateM m a -> GameStateM m a
checkConditionWithM p t f = checkCondition p >>= (\c -> if c then t else f)

checkCondition :: Monad m => (CondType, CondId) -> GameStateM m Bool
checkCondition (ct, cid) = do
  (PlayerStatus (_, loc) plItems, (go, _)) <- S.get
  case ct of
    CondLocal -> condition plItems loc $ lcCondition loc cid
    CondGlobal -> condition plItems loc $ goCondition go cid
    where 
      condition plItems loc (Condition objs items plIts) = return $ not (any objCond objs) && all itemLocCond items && all itemPlCond plIts
        where 
          objCond obj = Set.member obj (lcObjects loc)
          itemLocCond item = Set.member item (lcItems loc)
          itemPlCond item = Set.member item plItems

sepByLn :: String -> String -> String
sepByLn = sepBy "\n" (++)

sepBy :: a -> (a -> a -> a) -> a -> a -> a
sepBy s f a b = a `f` s `f` b