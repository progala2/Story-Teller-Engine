module GameParser.Loader(loadGame) where

import Extensions.Maybe
import Extensions.Errors
import qualified Game.Types as G
import qualified Data.Map.Strict as M
import qualified GameParser.Tokens as T
import GameParser.Parser 

type LoaderError = String

loadGame::String -> Either LoaderError G.GameStatus
loadGame = (either (Left . show) tokensToGame) . parseGameFile 
  where 
    tokensToGame (go, locs) = (,) <$> playerStatus <*> worldStatus
      where
        playerStatus = G.PlayerStatus <$> startingLocation <*> Right []
          where
            startingLocation = G.LocName <$> sGet go T.StartingLocation
        
        worldStatus = (,) <$> gameOptions <*> locations
          where
            gameOptions = G.GameOptions 
                <$> gameName
                <*> gameVersion 
                <*> playerCapacity
                <*> endingLocation
              where
                gameName = sGet go T.GameName
                gameVersion = sGet go T.GameVersion
                playerCapacity =  iGet go T.PlayerCapacity
                endingLocation = sGet go T.EndingLocation
            
            locations = M.fromAscList <$> rightsIfAll (location <$> (M.toList locs))
              where
                location (locK, locOpts) = (,) (G.LocName locK) 
                  <$> (G.Location descList 
                    <$> locTravelList 
                    <*> Right locObjects 
                    <*> Right locItems 
                    <*> locActions 
                    <*> Right locCond)
                  where
                    descList = case locOpts M.!? T.LocDescList of Just (T.LocDescListV mp) -> mapDescMap mp; Nothing -> M.empty; _ -> error "This shouldn't happen."
                      where
                        mapDescMap mp = M.fromAscList $ mapDescPair <$> M.toList mp
                        mapDescPair x = case x of 
                          (k, (T.LocDesc (T.Local idL) s)) -> (G.DescOrder k, G.LocDescCond G.CondLocal (G.CondId idL) s);
                          (k, (T.LocDesc (T.Global idG) s)) -> (G.DescOrder k, G.LocDescCond G.CondGlobal (G.CondId idG) s);
                          (k, (T.LocDesc (T.None) s)) -> (G.DescOrder k, G.LocDesc s)
                    
                    locTravelList = case locOpts M.!? T.LocTravelList of Just (T.LocTravelListV mp) -> Right $ mapTravelMap mp; Nothing -> errO T.LocTravelList; _ -> errCant
                      where
                        mapTravelMap mp = M.fromAscList $ mapTravelPair <$> M.toList mp
                        mapTravelPair x = case x of
                          (k, (T.LocCannotTravel (T.Local idL) s)) -> (G.LocName k, G.LocCannotTravel G.CondLocal (G.CondId idL) s);
                          (k, (T.LocCannotTravel (T.Global idG) s)) -> (G.LocName k, G.LocCannotTravel G.CondGlobal (G.CondId idG) s);
                          (k, (T.LocCanTravel)) -> (G.LocName k, G.LocCanTravel)
                          (_, T.LocCannotTravel T.None _) -> errCant

                    locObjects = case locOpts M.!? T.LocObjects of Just (T.LocStrings mp) -> map object mp; Nothing -> []; _ -> errCant
                      where
                        object x = G.Object x
                    locItems = case locOpts M.!? T.LocItems of Just (T.LocStrings mp) -> map item mp; Nothing -> []; _ -> errCant
                      where
                        item x = G.Item x
                    locActions = rightsIfAll $ case locOpts M.!? T.LocActions of Just (T.LocActionsV mp) -> map action mp; Nothing -> []; _ -> errCant--case loc M.!? T.LocActions 
                      where
                        action act = G.ActionUseItemsOnObject 
                          <$> (G.Item <$$> saGet act T.AotUsedItems) 
                          <*> (G.Object <$> sGet act T.AotUsedOn)
                          <*> Right ""
                          <*> Right []
                    locCond = M.empty--case loc M.!? T.LocCond

sGet :: (Ord k, Show k, T.OptionStr o) => M.Map k o -> k -> Either LoaderError String
sGet mp k = case mp M.!? k of Just o -> Right $ T.getStr o; _ -> errO k

saGet :: (Ord k, Show k, T.OptionArrStr o) => M.Map k o -> k -> Either LoaderError [String]
saGet mp k = case mp M.!? k of Just o -> Right $ T.getArrStr o; _ -> errO k

iGet :: (Ord k, Show k, T.OptionInt o) => M.Map k o -> k -> Either LoaderError Int
iGet mp k = case mp M.!? k of Just o -> Right $ T.getInt o; _ -> errO k

errO ::(Show a) => a -> Either LoaderError b
errO str = Left $ "There is no " ++ (show str) ++ " option."
