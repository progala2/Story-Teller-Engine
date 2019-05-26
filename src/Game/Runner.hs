module Game.Runner where

import System.Console.ANSI
import Game.CommandParser
import qualified Control.Monad.State.Lazy as S
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Game.Types
import Data.List
import qualified Data.List.Ordered as LO
import Data.Functor.Identity (Identity(..))

runGame :: GameStateIO ()
runGame = gameLoop
      where
        gameLoop = do 
          ((PlayerStatus currLoc _), _) <- S.get
          putStrLnL $ "Location: " ++ (show currLoc)
          putStrLnL $ showDescription $ snd currLoc
          putStrLnL "What now? : "
          line <- S.lift $ getLine
          S.lift $ clearScreen
          res <- liftT (either (\_ -> return "You wrote something really wrong!") handleCommand (parseCommand line)) 
          putStrLnL res
          gameLoop

handleCommand :: Command -> GameState String
handleCommand (Travel dest) =
  downMb nothing travelCommand
  where
    nothing = "There is no " ++ (show dest) ++ "..."
    travelCommand :: GameStateM Maybe String
    travelCommand = do
      (currLoc, locations) <- liftT getLocations
      destLoc <- S.lift $ locations M.!? dest
      trvl <- S.lift $ lcTravelList (snd currLoc) M.!? dest
      case trvl of
        LocCanTravel -> liftT $ travel (dest, destLoc)
        (LocCannotTravel ct cid str) -> 
          if checkCondition (snd currLoc) (ct, cid) 
          then liftT $ travel (dest, destLoc)
          else return str

handleCommand (ItemsOnObject items obj) = do
  ((PlayerStatus currLoc plItems), _) <- S.get
  case hasNotItems items plItems of
    Right _ ->
      case hasNotItems [obj] (lcObjects $ snd currLoc) of
        Right _ ->
          case find canApplyItemsOnObject (lcActions $ snd currLoc) of
            Just (ActionUseItemsOnObject _ _ com ress) -> 
              mapM_ applyActionResults ress >> return com
            _ -> return "I can't do it."
        _ -> return "There is no object like that!"
    Left is -> return $ "You don't have these items: " ++ (show $ LO.sort is)
  where 
    canApplyItemsOnObject (ActionUseItemsOnObject aItms aObj _ _) 
      | Set.difference aItms items == Set.empty && aObj == obj = True
      | otherwise = False
    canApplyItemsOnObject _ = False
handleCommand CheckBp = S.get >>= (\(PlayerStatus _ items, _) -> return $ foldl' sepByLn [] (show <$> Set.toList items))
handleCommand (PickUpItem it) = do 
  (PlayerStatus currLoc plItems, ws) <- S.get
  if itemExists it currLoc 
    then S.put (PlayerStatus (removeItemFromLoc it currLoc) (Set.insert it plItems), ws) >> return "Item taken."
    else return "There is no such item in this location..."
handleCommand (ThrowItem _) = return $ "Not yet implemented"

travel :: LocationP -> GameState String
travel destLoc = do
  (PlayerStatus currLoc plItems, (go, locs)) <- S.get
  S.put (PlayerStatus destLoc plItems, (go, insert2 currLoc locs))
  return $ "You travel to: " ++ (show $ fst destLoc)

showDescription :: Location -> String
showDescription l = foldl' sepByLn [] $ desc <$> M.elems (lcDescList l)
  where
    desc (LocDesc str) = str
    desc (LocDescCond ct cid str) = if checkCondition l (ct, cid) then str else ""

checkCondition :: Location -> (CondType, CondId) -> Bool
checkCondition loc (ct, cid) = case ct of
  CondLocal -> condition $ lcCondition loc cid
  CondGlobal -> False
  where 
    condition (Condition objs items) = not (any objCond objs) && all itemCond items
      where 
        objCond obj = Set.member obj (lcObjects loc)
        itemCond item = Set.member item (lcItems loc)

hasNotItems :: (Foldable t, Ord a) => t a -> Set.Set a -> Either [a] ()
hasNotItems items plItems = foldl' (nCorrectItem) (Right ()) items
  where 
    nCorrectItem (Right _) i = if Set.member i plItems 
      then Right ()
      else Left $ i:[]
    nCorrectItem (Left is) i = if Set.member i plItems
      then Left is
      else Left $ i:is

applyActionResults :: ActionResult -> GameState ()
applyActionResults (ArAddLocationItems aItms) = do
  (PlayerStatus currLoc plIt, ws) <- S.get
  S.put (PlayerStatus (addItemsToLoc aItms currLoc) plIt, ws)
applyActionResults (ArRemoveObjects objs) = do
  (PlayerStatus currLoc plIt, ws) <- S.get
  S.put (PlayerStatus (removeObjectsFromLoc objs currLoc) plIt, ws)

putStrLnL :: String -> GameStateIO ()
putStrLnL str = S.lift $ putStrLn str
sepByLn :: String -> String -> String
sepByLn = sepBy "\n" (++)

sepBy :: a -> (a -> a -> a) -> a -> a -> a
sepBy s f a b = a `f` s `f` b

liftT :: Monad m => S.StateT s Identity a -> S.StateT s m a
liftT = S.mapStateT (\(Identity (a, s)) -> return (a, s))

--downStateFromE :: Monad m => S.StateT s (Either e) a -> S.StateT s Identity e
--downStateFromE f st = S.mapStateT mapper st
--  where
--    mapper (Left e) = lift (f e)
--    mapper (Right (a, s)) = return (a, s)

downMb :: a -> S.StateT s Maybe a -> S.StateT s Identity a
downMb f st = do
  oldSt <- S.get
  S.mapStateT (mapper oldSt) st
  where
    mapper oldSt Nothing = Identity (f, oldSt)
    mapper _ (Just (a, s)) = Identity (a, s)

insert2 ::Ord a => (a, b) -> M.Map a b -> M.Map a b
insert2 (a, b) m = M.insert a b m