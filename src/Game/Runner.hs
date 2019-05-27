module Game.Runner where

import System.Console.ANSI
import Game.CommandParser
import qualified Control.Monad.State.Strict as S
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Game.Types
import Data.List
import qualified Data.List.Ordered as LO
import Data.Functor.Identity (Identity(..))
import Data.Foldable (foldlM)

data ExitCode = QuitGame | QuitAndSave | PlayerWin deriving(Show)

runGame :: GameStateIO ()
runGame = gameLoop
      where
        gameLoop = do 
          putStrLnL "test"
          gs@(PlayerStatus currLoc _, _) <- S.get
          putStrLnL $ "Location: " ++ (show $ fst currLoc)
          showDescription >>= putStrLnL
          putStrLnL "What now? : "
          line <- S.lift $ getLine
          S.lift $ clearScreen
          b <- S.mapStateT (quitGame gs) (handleCommandE line)
          if b then return () else gameLoop
        handleCommandE line = either (\_ -> return "You wrote something wrong!") handleCommand (parseCommand line)
        quitGame _ (Right (a, s)) = putStrLn a >> return (False, s)
        quitGame gs (Left c) = case c of
          QuitGame -> quitGameWo
          QuitAndSave -> quitGameSave
          PlayerWin -> return (True, gs)
          where
            quitGameWo = do
              putStrLn "Are you sure you want to quit this game without saving?(y/n)"
              answer <- getLine
              case answer of
                "y" -> return (True, gs)
                "n" -> return (False, gs)
                _ -> quitGameWo
            quitGameSave = error "Not implemented yet."

handleCommand :: Command -> GameStateM (Either ExitCode) String
handleCommand (Travel dest) =
  downMb nothing travelCommand
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
  (PlayerStatus currLoc plItems, _) <- S.get
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
  (PlayerStatus currLoc plItems, ws@(go, _)) <- S.get
  if itemExists it currLoc 
    then if Set.size plItems == (goPlayerCapacity go) 
      then S.put (PlayerStatus (removeItemFromLoc it currLoc) (Set.insert it plItems), ws) >> return "Item taken."
      else return $ "You can't have more than " ++ (show $ goPlayerCapacity go) ++ "items." 
    else return "There is no such item in this location..."

handleCommand (ThrowItem it) = do
  (PlayerStatus currLoc plIt, ws) <- S.get
  if Set.member it plIt
    then S.put (PlayerStatus (addItemsToLoc [it] currLoc) (Set.delete it plIt), ws) >> return "Item has been thrown away."
    else return "You don't have that item."

handleCommand ExitGame = S.lift $ Left QuitGame


travel :: Monad m => LocationP -> GameStateM m String
travel destLoc = do
  (PlayerStatus currLoc plItems, (go, locs)) <- S.get
  S.put (PlayerStatus destLoc plItems, (go, insert2 currLoc locs))
  return $ "You travel to: " ++ (show $ fst destLoc)

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

putStrLnL :: String -> GameStateIO ()
putStrLnL str = S.lift $ putStrLn str
sepByLn :: String -> String -> String
sepByLn = sepBy "\n" (++)

sepBy :: a -> (a -> a -> a) -> a -> a -> a
sepBy s f a b = a `f` s `f` b

liftT :: Monad m => S.StateT s Identity a -> S.StateT s m a
liftT = S.mapStateT (\(Identity (a, s)) -> return (a, s))

downMb :: a -> S.StateT s Maybe a -> S.StateT s (Either e) a
downMb f st = S.get >>= (\ oldSt -> S.mapStateT (mapper oldSt) st)
  where
    mapper oldSt Nothing = Right (f, oldSt)
    mapper _ (Just (a, s)) = Right (a, s)

insert2 ::Ord a => (a, b) -> M.Map a b -> M.Map a b
insert2 (a, b) = M.insert a b