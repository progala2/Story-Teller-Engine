module Game.GameState where

import qualified Control.Monad.State.Strict as S
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Data.List
import           Data.Foldable (foldlM)

-- | 'StateT' 'IO' monad with 'GameStatus'
type GameStateIO a = S.StateT GameStatus IO a
-- | 'StateT' monad with 'GameStatus'
type GameStateM m a = S.StateT GameStatus m a

-- | Helper for taking only locations data ('LocationP', 'LocMap') from 'GameStatus'.
getLocations :: Monad m => GameStateM m (LocationP, LocMap)
getLocations = do
  (PlayerStatus currLoc _, (_, locations)) <- S.get
  return (currLoc, locations)

-- | Helper for taking only 'PlayerStatus' data from 'GameStatus'.
getPlayerStatus :: Monad m => GameStateM m PlayerStatus
getPlayerStatus = do
  (ps, _) <- S.get
  return ps

-- | Provide the whole description of the 'Location' the player is in, taking 'Condition's into consideration if necessary.
-- if 'Condition' for description of the current 'Location' is not met it will not be added to the result.
showDescription :: Monad m => GameStateM m String
showDescription = do 
  (PlayerStatus (_, l) _) <- getPlayerStatus 
  descL <- foldlM desc [] $ M.elems (lcDescList l)
  return $ intercalate "\n" descL
  where
    desc ss (LocDesc str) = return $ str:ss
    desc ss (LocDescCond ct cid str) = checkConditionWith (ct, cid) (str:ss) ss

-- | Check whetever the given 'Condition', indexed by 'CondType' and 'CondId', is true in current state of a game.
checkCondition :: Monad m => CondKey -> GameStateM m Bool
checkCondition (ct, cid) = do
  (ps@(PlayerStatus l _), (go, _)) <- S.get
  case ct of
    CondLocal -> condition ps l $ lcCondition l cid
    CondGlobal -> condition ps l $ goCondition go cid
    where 
      condition ps l (Condition objs items plIts) = return $ objsNotExist && itsExist && itsPsExist
        where 
          objsNotExist = not (any (lcObjExists l) objs)
          itsExist = all (lcItemExists l) items
          itsPsExist = all (psItemExists ps) plIts

-- | The same as with 'checkCondition' but you can provide what values should be returned on result
--
-- > checkConditionWith (cl, id) trueValue falseValue
checkConditionWith :: Monad m => CondKey -> a -> a -> GameStateM m a
checkConditionWith p t f = checkCondition p >>= (\c -> return $ if c then t else f)

-- | The same as with 'checkConditionWith' but in Monad version.
--
-- > checkConditionWithM (cl, id) (Just truValue) Nothing
checkConditionWithM :: Monad m => CondKey -> GameStateM m a -> GameStateM m a -> GameStateM m a
checkConditionWithM p t f = checkCondition p >>= (\c -> if c then t else f)

type GameStatus = (PlayerStatus, WorldStatus)
-- | Information about player
--
-- [@LocationP@]: Current location of the player.
-- [@ItemSet@]: Items carry on by the player.
data PlayerStatus = PlayerStatus LocationP ItemSet deriving(Show, Read)

-- | Check whether the player has the item.
psItemExists :: PlayerStatus -> Item -> Bool
psItemExists (PlayerStatus _ plItems) item = Set.member item plItems

type WorldStatus = (GameOptions, LocMap)

data GameOptions = GameOptions {
    goGameName::String, 
    goGameVersion::String, 
    -- | Define how many items can player can have.
    goPlayerCapacity::Int,
    -- | Define in which location the game will end.
    goEndingLocation::LocName,
    -- | Global conditions. They apply to every location in the game.
    goConditions::Conditions,
    -- | Introduction text when a player starts a new game.
    goIntro::String,
    -- | Outro text when a player finishes a game.
    goOutro::String
    } deriving(Show, Read)

-- | Get a global condition from the game options by its Id.
goCondition :: GameOptions -> CondId -> Condition
goCondition l cid = goConditions l M.! cid

-- | Data providing all necessary information about location.
data Location = Location {
    lcDescList::DescMap, 
    lcTravelList::TravelMap, 
    lcObjects::ObjectSet, 
    lcItems::ItemSet, 
    lcActions::Actions,
    lcConditions::Conditions
    } deriving(Show, Read)

-- | LocName is the key of the 'Location' data
newtype LocName = LocName String deriving(Eq, Ord, Show, Read)
type LocationP = (LocName, Location)
type LocMap = M.Map LocName Location
type DescMap = (M.Map DescOrder LocDesc)
type TravelMap = (M.Map LocName LocCanTravel)

-- | Adding items to the given location.
lcAddItems :: [Item] -> LocationP -> LocationP
lcAddItems is (nm, Location d t o itms a c) = 
  (nm, Location d t o (Set.union (Set.fromList is) itms) a c)
-- | Removing objects from the given location.
lcRemoveObjects :: [Object] -> LocationP -> LocationP
lcRemoveObjects os (nm, Location d t objs i a c) = 
  (nm, Location d t (objs Set.\\ (Set.fromList os)) i a c )
-- | Removing an item from the given location.
lcRemoveItem :: Item -> LocationP -> LocationP
lcRemoveItem i (nm, Location d t objs itms a c) = 
  (nm, Location d t objs (Set.delete i itms) a c)
-- | Check whether the item exists in the given location.
lcItemExists :: LocationP -> Item -> Bool
lcItemExists (_, loc) it = it `Set.member` lcItems loc

-- | Check whether the object exists in the given location.
lcObjExists :: LocationP -> Object -> Bool
lcObjExists (_, loc) obj = Set.member obj (lcObjects loc)

-- | Take a condition from the location by its Id. Throws exception when there is no such condition.
lcCondition :: LocationP -> CondId -> Condition
lcCondition (_, l) cid = lcConditions l M.! cid

-- | Data type that has two constructors: a simple description 'LocDesc' or description displayed only under some condition 'LocDescCond'.
--
-- It is a data type for a 'Location' description.
data LocDesc = LocDesc String | LocDescCond CondType CondId String deriving(Show, Read)
-- | Unique key for ordering the description on the screen.
newtype DescOrder = DescOrder Int deriving(Show, Eq, Ord, Read)

-- | Data type used to determine whether a player can travel to a location availible in a current location.
--
-- 'LocCanTravel' - there is no restraints to travel.
-- 'LocCannotTravel' - there is some condition to be met before you can travel.
data LocCanTravel = LocCannotTravel CondType CondId String | LocCanTravel deriving(Show, Read)

-- | The 'Condition' type stores information about all conditions that need to be satisfied.
--
-- Can have empty parameters. 
data Condition = Condition ObjectsNotExist ItemsInLocation PlayerHasItems deriving(Show, Read)
-- | the Enum data to determine whether condition apply only in a location or it is a global condition that can be applied everywhere during the game.
data CondType = CondLocal | CondGlobal deriving(Show, Read)
-- | An unique Id of a condition. Uniquness works separately for each location and for globals ones; in other words two location can have conditions with the same Ids. 
newtype CondId = CondId Int deriving(Show, Eq, Ord, Read)
type CondKey = (CondType, CondId)
type Conditions = (M.Map CondId Condition)

type ObjectsNotExist = [Object]
type ItemsInLocation = [Item]
type PlayerHasItems = [Item]

-- | Action data. 
data Action = 
  -- | Action of using items on some object.
  ActionUseItemsOnObject { 
    aItemsUsed::ItemSet,
    aObject::Object, 
    aComment::String,
    aResults::[ActionResult]
  } 
  |
  -- | Action of doing unique command on some object or without an object.
  ActionUnique {
    auCommands::[String],
    aObjectM::Maybe Object,
    aComment::String,
    aResults::[ActionResult]
  } deriving(Show, Read)
type Actions = [Action]
-- | Information about results of some Action.
--
-- [@ArAddLocationItems@] - After an action items will be added to a location.
-- [@ArRemoveObjects@] - After an action objects will be removed from a location.
data ActionResult = ArAddLocationItems [Item] | ArRemoveObjects [Object] deriving(Show, Read)

newtype Item = Item String deriving(Eq, Ord, Show, Read)
newtype Object = Object String deriving(Eq, Ord, Show, Read)

type ItemSet = Set.Set Item
type ObjectSet = Set.Set Object

itemSetFromList :: [String] -> ItemSet
itemSetFromList = Set.fromList . (Item <$>)