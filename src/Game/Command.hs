module Game.Command(ExitCode(..), handleCommand) where

import           Game.CommandParser
import qualified Control.Monad.State.Strict as S
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Data.Maybe (maybeToList)
import           Game.GameState
import           Data.List
import qualified Data.List.Ordered as LO

data ExitCode = QuitGame | QuitAndSave | PlayerWin deriving(Show)

handleCommand :: Command -> GameStateM (Either ExitCode) String
handleCommand (Travel dest) = do
  str <- downMb nothing travelCommand
  (PlayerStatus (name, _) _, (go, _)) <- S.get
  if name == goEndingLocation go then S.lift $ Left PlayerWin else return str
  where
    nothing = "There is no " ++ (show dest) ++ "..."
    travelCommand :: GameStateM Maybe String
    travelCommand = do
      (currLoc, locations) <- getLocations
      destLoc <- S.lift $ locations M.!? dest
      trvl <- S.lift $ lcTravelList (snd currLoc) M.!? dest
      case trvl of
        LocCanTravel -> travel (dest, destLoc)
        (LocCannotTravel ct cid str) -> checkConditionWithM (ct, cid) (travel (dest, destLoc)) (return str)

handleCommand (ItemsOnObject items obj) = do
  (PlayerStatus (_, currLoc) plItems, _) <- S.get
  case hasNotItems items plItems of
    Right _ ->
      case hasNotItems [obj] (lcObjects currLoc) of
        Right _ -> canApply canApplyItemsOnObject currLoc
        _ -> return "There is no object like that!"
    Left is -> return $ "You don't have these items: " ++ (show $ LO.sort is)
  where 
    canApplyItemsOnObject (ActionUseItemsOnObject aItms aObj _ _) 
      | Set.difference aItms items == Set.empty && aObj == obj = True
      | otherwise = False
    canApplyItemsOnObject _ = False

handleCommand (UniqueCommand comm obj) = do
  (PlayerStatus (_, currLoc) _, _) <- S.get
  case hasNotItems (maybeToList obj) (lcObjects currLoc) of
    Right _ -> canApply canApplyUniqeCommand currLoc
    _ -> return "There is no object like that!"
  where 
    canApplyUniqeCommand(ActionUnique aComm aObj _ _) 
      | elem comm aComm && aObj == obj = True
      | otherwise = False
    canApplyUniqeCommand _ = False

handleCommand CheckBp = S.get >>= (\(PlayerStatus _ items, _) -> return $ intercalate "\n" (show <$> Set.toList items))

handleCommand (PickUpItem it) = do 
  (PlayerStatus currLoc plItems, ws@(go, _)) <- S.get
  if itemExists it currLoc 
    then if Set.size plItems /= (goPlayerCapacity go) 
      then S.put (PlayerStatus (removeItemFromLoc it currLoc) (Set.insert it plItems), ws) >> return "Item taken."
      else return $ "You can't have more than " ++ (show $ goPlayerCapacity go) ++ " items."
    else return "There is no such item in this location..."

handleCommand (ThrowItem it) = do
  (PlayerStatus currLoc plIt, ws) <- S.get
  if Set.member it plIt
    then S.put (PlayerStatus (addItemsToLoc [it] currLoc) (Set.delete it plIt), ws) >> return "Item has been thrown away."
    else return "You don't have that item."

handleCommand ExitGame = S.lift $ Left QuitGame

handleCommand ExitAndSave = S.lift $ Left QuitAndSave

canApply :: Monad m => (Action -> Bool) -> Location -> GameStateM m String
canApply f currLoc = case find f (lcActions currLoc) of
  Just (ActionUnique _ _ com ress) -> 
    mapM_ applyActionResults ress >> return com
  _ -> return "I can't do it."

travel :: Monad m => LocationP -> GameStateM m String
travel destLoc = do
  (PlayerStatus currLoc plItems, (go, locs)) <- S.get
  S.put (PlayerStatus destLoc plItems, (go, insert2 currLoc locs))
  return $ "You travel to: " ++ (show $ fst destLoc)

hasNotItems :: (Foldable t, Ord a) => t a -> Set.Set a -> Either [a] ()
hasNotItems items plItems = foldl' (nCorrectItem) (Right ()) items
  where 
    nCorrectItem (Right _) i = if Set.member i plItems 
      then Right ()
      else Left $ i:[]
    nCorrectItem (Left is) i = if Set.member i plItems
      then Left is
      else Left $ i:is

applyActionResults :: Monad m => ActionResult -> GameStateM m ()
applyActionResults (ArAddLocationItems aItms) = do
  (PlayerStatus currLoc plIt, ws) <- S.get
  S.put (PlayerStatus (addItemsToLoc aItms currLoc) plIt, ws)
applyActionResults (ArRemoveObjects objs) = do
  (PlayerStatus currLoc plIt, ws) <- S.get
  S.put (PlayerStatus (removeObjectsFromLoc objs currLoc) plIt, ws)

downMb :: a -> S.StateT s Maybe a -> S.StateT s (Either e) a
downMb f st = S.get >>= (\ oldSt -> S.mapStateT (mapper oldSt) st)
  where
    mapper oldSt Nothing = Right (f, oldSt)
    mapper _ (Just (a, s)) = Right (a, s)

insert2 ::Ord a => (a, b) -> M.Map a b -> M.Map a b
insert2 (a, b) = M.insert a b