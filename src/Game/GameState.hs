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

-- | The same as with 'checkCondition' but you can provide what values should be returned on result
--
-- > checkConditionWith (cl, id) trueValue falseValue
checkConditionWith :: Monad m => (CondType, CondId) -> a -> a -> GameStateM m a
checkConditionWith p t f = checkCondition p >>= (\c -> return $ if c then t else f)


-- | The same as with 'checkConditionWith' but in Monad version.
--
-- > checkConditionWithM (cl, id) (Just truValue) Nothing
checkConditionWithM :: Monad m => (CondType, CondId) -> GameStateM m a -> GameStateM m a -> GameStateM m a
checkConditionWithM p t f = checkCondition p >>= (\c -> if c then t else f)

type GameStatus = (PlayerStatus, WorldStatus)
-- | Information about player
--
-- [@LocationP@]: Current location of the player.
-- [@ItemSet@]: Items carry on by the player.
data PlayerStatus = PlayerStatus LocationP ItemSet deriving(Show, Read)
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


data Location = Location {
    lcDescList::DescMap, 
    lcTravelList::TravelMap, 
    lcObjects::ObjectSet, 
    lcItems::ItemSet, 
    lcActions::Actions,
    lcConditions::Conditions
    } deriving(Show, Read)

newtype LocName = LocName String deriving(Eq, Ord, Show, Read)
type LocationP = (LocName, Location)
type LocMap = M.Map LocName Location
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

data LocDesc = LocDesc String | LocDescCond CondType CondId String deriving(Show, Read)
newtype DescOrder = DescOrder Int deriving(Show, Eq, Ord, Read)

data LocCanTravel = LocCannotTravel CondType CondId String | LocCanTravel deriving(Show, Read)

data Condition = Condition ObjectsNotExist ItemsInLocation PlayerHasItems deriving(Show, Read)
data CondType = CondLocal | CondGlobal deriving(Show, Read)
newtype CondId = CondId Int deriving(Show, Eq, Ord, Read)
type Conditions = (M.Map CondId Condition)

type ObjectsNotExist = [Object]
type ItemsInLocation = [Item]
type PlayerHasItems = [Item]

-- | Action data. 
data Action = 
  -- | Action of using items on some object
  ActionUseItemsOnObject { 
    aItemsUsed::ItemSet,
    aObject::Object, 
    aComment::String,
    aResults::[ActionResult]
  } 
  |
  -- | Action of doing unique command on some object or without
  ActionUnique {
    auCommands::[String],
    aObjectM::Maybe Object,
    aComment::String,
    aResults::[ActionResult]
  } deriving(Show, Read)
type Actions = [Action]
data ActionResult = ArAddLocationItems [Item] | ArRemoveObjects [Object] deriving(Show, Read)

newtype Item = Item String deriving(Eq, Ord, Show, Read)
newtype Object = Object String deriving(Eq, Ord, Show, Read)

type ItemSet = Set.Set Item
type ObjectSet = Set.Set Object

itemSetFromList :: [String] -> ItemSet
itemSetFromList = Set.fromList . (Item <$>)