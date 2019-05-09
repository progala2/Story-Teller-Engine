module GameParser.Loader(loadGame) where

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
                location (locK, locOpts) = (,) (G.LocName locK) <$>  (G.Location descList <$> locTravelList <*> Right locObjects <*> Right locItems <*> Right locActions <*> Right locCond)
                  where
                    descList = case locOpts M.!? T.LocDescList of Just (T.LocDescListV mp) -> mapDescMap mp; Nothing -> M.empty; _ -> error "This shouldn't happen."
                      where
                        mapDescMap mp = M.fromAscList $ mapDescPair <$> M.toList mp
                        mapDescPair x = case x of 
                          (k, (T.LocDesc (T.Local idL) s)) -> (G.DescOrder k, G.LocDescCond G.CondLocal (G.CondId idL) s);
                          (k, (T.LocDesc (T.Global idG) s)) -> (G.DescOrder k, G.LocDescCond G.CondGlobal (G.CondId idG) s);
                          (k, (T.LocDesc (T.None) s)) -> (G.DescOrder k, G.LocDesc s)
                    
                    locTravelList = case locOpts M.!? T.LocTravelList of Just (T.LocTravelListV mp) -> Right $ mapTravelMap mp; Nothing -> errO T.LocTravelList; _ -> error "This shouldn't happen."
                      where
                        mapTravelMap mp = M.fromAscList $ mapTravelPair <$> M.toList mp
                        mapTravelPair x = case x of
                          (k, (T.LocCannotTravel (T.Local idL) s)) -> (G.LocName k, G.LocCannotTravel G.CondLocal (G.CondId idL) s);
                          (k, (T.LocCannotTravel (T.Global idG) s)) -> (G.LocName k, G.LocCannotTravel G.CondGlobal (G.CondId idG) s);
                          (k, (T.LocCanTravel)) -> (G.LocName k, G.LocCanTravel)
                          (_, T.LocCannotTravel T.None _) -> error "This scenatio can't happen."

                    locObjects = []--case loc M.!? T.LocObjects 
                    locItems = []--case loc M.!? T.LocItems 
                    locActions = []--case loc M.!? T.LocActions 
                    locCond = M.empty--case loc M.!? T.LocCond

rightsIfAll :: [Either String b] -> Either String [b]
rightsIfAll [] = Right []
rightsIfAll rs = case foldl fldFunc ("", []) rs of
  ("", table) -> Right table
  (str, _) -> Left str
  where
    fldFunc (l, t) (Left str) = (l ++ "\n" ++ str, t)
    fldFunc ("", t) (Right elm) = ("", elm:t)
    fldFunc (l, _) (Right _) = (l, [])


sGet :: (Ord k, Show k) => M.Map k T.GameOption -> k -> Either LoaderError String
sGet mp k = case mp M.!? k of Just (T.GameOptionString str) -> Right str; _ -> errO k

iGet :: (Ord k, Show k) => M.Map k T.GameOption -> k -> Either LoaderError Int
iGet mp k = case mp M.!? k of Just (T.GameOptionInt str) -> Right str; _ -> errO k

errO ::(Show a) => a -> Either LoaderError b
errO str = Left $ "There is no " ++ (show str) ++ " option."
