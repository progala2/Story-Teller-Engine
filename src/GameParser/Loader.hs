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
            startingLocation = case go M.!? T.StartingLocation of Just(T.GameOptionString str) -> Right $ G.LocName str; _ -> errO T.StartingLocation
        
        worldStatus = (,) <$> gameOptions <*> Right locations
          where
            gameOptions = G.GameOptions 
                <$> gameName
                <*> gameVersion 
                <*> playerCapacity
                <*> endingLocation
              where
                gameName = case go M.!? T.GameName of Just (T.GameOptionString str) -> Right str; _ -> errO T.GameName
                gameVersion = case go M.!? T.GameVersion of Just (T.GameOptionString str) -> Right str; _ -> errO T.GameVersion
                playerCapacity =  case go M.!? T.PlayerCapacity of Just(T.GameOptionInt str) -> Right str; _ -> errO T.PlayerCapacity
                endingLocation = case go M.!? T.EndingLocation of Just(T.GameOptionString str) -> Right str; _ -> errO T.EndingLocation
            
            locations = M.fromAscList $ location <$> (M.toList locs)
              where
                location (locK, locOpts) = (G.LocName locK, G.Location descList locTravelList locObjects locItems locActions locCond)
                  where
                    descList = case locOpts M.!? T.LocDescList of Just (T.LocDescListV mp) -> mapDescMap mp; Nothing -> M.empty; _ -> error "This shouldn't happen"
                      where
                        mapDescMap mp = M.fromAscList $ mapDescPair <$> M.toList mp
                        mapDescPair x = case x of 
                          (k, (T.LocDesc (T.Local idL) s)) -> (G.DescOrder k, G.LocDescCond G.CondLocal (G.CondId idL) s);
                          (k, (T.LocDesc (T.Global idG) s)) -> (G.DescOrder k, G.LocDescCond G.CondGlobal (G.CondId idG) s);
                          (k, (T.LocDesc (T.None) s)) -> (G.DescOrder k, G.LocDesc s)
                    
                    locTravelList = M.empty--case loc M.!? T.LocTravelList 
                    locObjects = []--case loc M.!? T.LocObjects 
                    locItems = []--case loc M.!? T.LocItems 
                    locActions = []--case loc M.!? T.LocActions 
                    locCond = M.empty--case loc M.!? T.LocCond

errO :: T.GameOptionType -> Either LoaderError a
errO str = Left $ "There is no " ++ (show str) ++ " option."
{-
getOpt typ val go = case typ of 
  T.PlayerCapacity -> func3 $ go M.!? typ
  _ -> func2 $ go M.!? typ
  where
    func2 Just (T.GameOptionString str) = Right str
    func2 _ = errO typ
    func3 Just (T.GameOptionInt str) = Right str
    func3 _ = errO typ-}