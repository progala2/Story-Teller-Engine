-- |
-- Module      : Command
-- Description : Handling Game Commands
module Game.CommandHandler(ExitCode(..), handleCommand) where

import           Game.Command
import qualified Control.Monad.State.Strict as S
import           Extensions.Monad ((<$$>))
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Data.Maybe (maybeToList)
import           Game.GameState
import           Data.List
import qualified Data.List.Ordered as LO

-- | A data type is used to communicate the result, when exiting the game.
data ExitCode = 
  -- | A player quits game without saving it.
  QuitGame | 
  -- | A player quits game with saving it.
  QuitAndSave |
  -- | A player won the game. 
  PlayerWin deriving(Show)

-- | The core of the game logic. Basing on the received 'Command' from the player it manipulates the GameState.
--
-- If the game has been finished, regardles of the reason, it will end up in the Left state with 'ExitCode'.
--
-- Otherwise it will just return a string as a result message after handling a particular command. 
handleCommand :: Command -> GameStateM (Either ExitCode) String
handleCommand (Travel dest) = do
  str <- mapStateTMb nothing travelCommand
  b <- playerWon
  if b then S.lift $ Left PlayerWin else return str
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
  (PlayerStatus (_, currLoc) plItems) <- getPlayerStatus
  case hasNotItems items plItems of
    Right _ ->
      case hasNotItems [obj] (lcObjects currLoc) of
        Right _ -> canApply (isItemsOnObjectAction items obj)
        _ -> return "There is no object like that!"
    Left is -> return $ "You don't have these items: " ++ (show $ LO.sort is)

handleCommand (UniqueCommand comm obj) = do
  currLoc <- getCurrLocation
  case hasNotItems (maybeToList obj) (lcObjects currLoc) of
    Right _ -> canApply (isUniqeAction comm obj)
    _ -> return "There is no object like that!"

handleCommand CheckBp = S.get >>= (\(PlayerStatus _ items, _) -> return $ intercalate "\n" (show <$> Set.toList items))

handleCommand (PickUpItem it) = do 
  (PlayerStatus currLoc plItems, ws@(go, _)) <- S.get
  if lcItemExists currLoc it 
    then if Set.size plItems /= (goPlayerCapacity go) 
      then S.put (PlayerStatus (lcRemoveItem it currLoc) (Set.insert it plItems), ws) >> return "Item taken."
      else return $ "You can't have more than " ++ (show $ goPlayerCapacity go) ++ " items."
    else return "There is no such item in this location..."

handleCommand (ThrowItem it) = do
  (PlayerStatus currLoc plIt, ws) <- S.get
  if Set.member it plIt
    then S.put (PlayerStatus (lcAddItems [it] currLoc) (Set.delete it plIt), ws) >> return "Item has been thrown away."
    else return "You don't have that item."

handleCommand Help =  intercalate "\n" <$> map (++ " - ???") <$> (intercalate ", " <$$> getCurrLocUniqueActionsNames) >>= 
  return . (++) ("go to\\travel to\\move to\\gt LocationName - travel to the LocationName.\n"
  ++ "use items\\ui 'ItemName1, ItemName2' on ObjectName - use the ItemName1 and ItemName2 on the ObjectName.\n"
  ++ "check bp\\check backpack\\cbp - Check status of your Backpack.\n"
  ++ "pick up\\pup ItemName - take item from the current location.\n"
  ++ "throw item\\hri ItemName - throw the ItemName to the current location.\n"
  ++ ":exit\\:q - exit game without saving.\n" 
  ++ ":exitSave\\:qs - exit game with saving it.\n"
  ++ "Unique Commands - each location can have unique commands. Only old Gods know what they do!\n Some unique commands need object to use on. For example \"cut Tree\"\n")

handleCommand ExitGame = S.lift $ Left QuitGame

handleCommand ExitAndSave = S.lift $ Left QuitAndSave

-- | Check whether a given action is availible and can be done in the current location and state of the game.
--
-- The function parameter is a mean to recognize a searching action among the ones located in the current location.
canApply :: Monad m => (Action -> Bool) -> GameStateM m String
canApply f = getCurrLocation >>= (\currLoc -> case find f (lcActions currLoc) of
    Just act -> 
      mapM_ applyActionResults (aResults act) >> return (aComment act)
    _ -> return "I can't do it.")

-- | Change the current location of a player to the given one to this function. 
-- It doesn't check any conditions, just teleport player to the given location.
travel :: Monad m => LocationP -> GameStateM m String
travel destLoc = do
  (PlayerStatus currLoc plItems, (go, locs)) <- S.get
  S.put (PlayerStatus destLoc plItems, (go, insert2 currLoc locs))
  return $ "You travel to: " ++ (show $ fst destLoc)

-- | Check whether player won the game by being the ending location.
playerWon :: Monad m => GameStateM m Bool
playerWon = S.get >>= (\(PlayerStatus (name, _) _, (go, _)) -> return $ name == goEndingLocation go )

-- | Check whether an array of values is not in the given Set.
-- 
-- Returns @Left [a]@ when there are values in the Set and provides those found values.
-- Otherwise it is just @Right ()@.
hasNotItems :: (Foldable t, Ord a) => t a -> Set.Set a -> Either [a] ()
hasNotItems items plItems = foldl' (nCorrectItem) (Right ()) items
  where 
    nCorrectItem (Right _) i = if Set.member i plItems 
      then Right ()
      else Left $ i:[]
    nCorrectItem (Left is) i = if Set.member i plItems
      then Left is
      else Left $ i:is

-- | Applies the given 'ActionResult' without checking any conditions.
applyActionResults :: Monad m => ActionResult -> GameStateM m ()
applyActionResults (ArAddLocationItems aItms) = do
  (PlayerStatus currLoc plIt, ws) <- S.get
  S.put (PlayerStatus (lcAddItems aItms currLoc) plIt, ws)
applyActionResults (ArRemoveObjects objs) = do
  (PlayerStatus currLoc plIt, ws) <- S.get
  S.put (PlayerStatus (lcRemoveObjects objs currLoc) plIt, ws)

-- | If the inner monad of the input is 'Nothing' function will convert it to the 'Right' with the default value passed on the first argument.
-- 
-- Otherwise it will just rewrite Just to Right. 
mapStateTMb :: a -> S.StateT s Maybe a -> S.StateT s (Either e) a
mapStateTMb f st = S.get >>= (\ oldSt -> S.mapStateT (mapper oldSt) st)
  where
    mapper oldSt Nothing = Right (f, oldSt)
    mapper _ (Just (a, s)) = Right (a, s)

-- | Just a helper for a tuple insert
insert2 ::Ord k => (k, a) -> M.Map k a -> M.Map k a
insert2 = uncurry M.insert