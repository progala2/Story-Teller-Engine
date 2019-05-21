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
        currLoc ((PlayerStatus cL _), (_, locs)) = locs M.! cL
        gameLoop = do 
          ss@((PlayerStatus (LocName loc) _), _) <- S.get
          putStrLnL $ "Location: " ++ loc
          putStrLnL $ showDescription ss (currLoc ss)
          putStrLnL "What now? : "
          line <- S.lift $ getLine
          S.lift $ clearScreen
          res <- raiseStateToT (either (\_ -> return "You wrote something really wrong!") handleCommand (parseCommand line)) 
          putStrLnL res
          gameLoop

handleCommand :: Command -> GameState String
handleCommand (Travel dest) = do
  gs@((PlayerStatus pLocName items), (opts, locations)) <- S.get
  let currLoc = locations M.! pLocName
  if dest `M.member` locations
    then
      case lcTravelList currLoc M.!? dest of
        Just LocCanTravel -> travel items opts locations
        Just (LocCannotTravel ct cid str) -> if checkCondition gs currLoc (ct, cid) 
          then travel items opts locations
          else return str
        Nothing -> nothing
      
    else 
      nothing
  where
    nothing = return $ "There is no " ++ (show dest) ++ "..."
    travel :: ItemSet -> GameOptions -> Locations -> GameState String
    travel items opts lc = do
      S.put (PlayerStatus dest items, (opts, lc))
      return $ "You travel to: " ++ (show dest)

handleCommand (ItemsOnObject items obj) = do
  ((PlayerStatus pLocName plItems), (_, locations)) <- S.get
  case hasNotItems items plItems of
    Nothing -> do
      let currLoc = locations M.! pLocName 
      let objects = lcObjects currLoc
      case hasNotItems [obj] objects of
        Nothing -> do 
          let actions = lcActions currLoc
          case find canApplyItemsOnObject actions of
            Just _ -> return "Action applied!"
            Nothing -> return "I can't do it."
        _ -> return "There is no object like that!"
    Just is -> return $ "You don't have these items: " ++ (show $ LO.sort is)
  where 
    canApplyItemsOnObject (ActionUseItemsOnObject aItms aObj _ _) 
      | Set.difference aItms items == Set.empty && aObj == obj = True
      | otherwise = False
    canApplyItemsOnObject _ = False
handleCommand CheckBp = S.get >>= (\(PlayerStatus _ items, _) -> return $ foldl' sepByLn [] (show <$> Set.toList items))
handleCommand (PickUpItem _) = return $ "Not yet implemented"
handleCommand (ThrowItem _) = return $ "Not yet implemented"


showDescription :: GameStatus -> Location -> String
showDescription gs l = foldl' sepByLn [] $ desc <$> M.elems (lcDescList l)
  where
    desc (LocDesc str) = str
    desc (LocDescCond ct cid str) = if checkCondition gs l (ct, cid) then str else ""

checkCondition :: GameStatus -> Location -> (CondType, CondId) -> Bool
checkCondition (_, _) loc (ct, cid) = case ct of
  CondLocal -> condition $ lcCondition loc cid
  CondGlobal -> False
  where 
    condition (Condition objs items) = not (any objCond objs) && all itemCond items
      where 
        objCond obj = Set.member obj (lcObjects loc)
        itemCond item = Set.member item (lcItems loc)

hasNotItems :: (Foldable t, Ord a) => t a -> Set.Set a -> Maybe [a]
hasNotItems items plItems = foldl' (nCorrectItem) Nothing items
  where 
    nCorrectItem Nothing i = if Set.member i plItems 
      then Nothing
      else Just $ i:[]
    nCorrectItem (Just is) i = if Set.member i plItems
      then Just is
      else Just $ i:is

putStrLnL :: String -> GameStateIO ()
putStrLnL str = S.lift $ putStrLn str
sepByLn :: String -> String -> String
sepByLn = sepBy "\n" (++)

sepBy :: a -> (a -> a -> a) -> a -> a -> a
sepBy s f a b = a `f` s `f` b
raiseStateToT :: Monad m => S.StateT s Identity a -> S.StateT s m a
raiseStateToT = S.mapStateT (\(Identity (s, a)) -> return (s, a))