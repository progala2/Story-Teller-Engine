module Parser.Loader(loadGame) where

import Extensions.Errors
import Extensions.Monad
import qualified Game.Types as G
import qualified Data.Map.Strict as M
import qualified Data.Either as E
import qualified Parser.Text.Tokens as T
import Parser.Text.Parser 
import qualified Parser.Text.Option as T
import Parser.Errors

loadGame::String -> Either LoaderError G.GameStatus
loadGame = (either (Left . show) tokensToGame) . parseGameFile 
  where 
    tokensToGame (go, locs) = (,) <$> playerStatus <*> worldStatus
      where
        playerStatus = G.PlayerStatus <$> startingLocation <*> Right []
          where
            startingLocation = G.LocName <$> T.sGet go T.StartingLocation
        
        worldStatus = (,) <$> gameOptions <*> locations
          where
            gameOptions = G.GameOptions 
                <$> gameName
                <*> gameVersion 
                <*> playerCapacity
                <*> endingLocation
              where
                gameName = T.sGet go T.GameName
                gameVersion = T.sGet go T.GameVersion
                playerCapacity =  T.iGet go T.PlayerCapacity
                endingLocation = T.sGet go T.EndingLocation
            
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
                    descList = E.fromRight M.empty $ T.tGet locOpts T.LocDescList mapDescMap
                      where
                        mapDescMap (T.LocDescListV mp) = M.fromAscList $ mapDescPair <$> M.toList mp
                        mapDescMap _ = errCant
                        mapDescPair x = case x of 
                          (k, (T.LocDesc (T.Local idL) s)) -> (G.DescOrder k, G.LocDescCond G.CondLocal (G.CondId idL) s);
                          (k, (T.LocDesc (T.Global idG) s)) -> (G.DescOrder k, G.LocDescCond G.CondGlobal (G.CondId idG) s);
                          (k, (T.LocDesc (T.None) s)) -> (G.DescOrder k, G.LocDesc s)
                    
                    locTravelList = T.tGet locOpts T.LocTravelList mapTravelMap
                      where
                        mapTravelMap (T.LocTravelListV mp) = M.fromAscList $ mapTravelPair <$> M.toList mp
                        mapTravelMap _ = errCant
                        mapTravelPair x = case x of
                          (k, (T.LocCannotTravel (T.Local idL) s)) -> (G.LocName k, G.LocCannotTravel G.CondLocal (G.CondId idL) s);
                          (k, (T.LocCannotTravel (T.Global idG) s)) -> (G.LocName k, G.LocCannotTravel G.CondGlobal (G.CondId idG) s);
                          (k, (T.LocCanTravel)) -> (G.LocName k, G.LocCanTravel)
                          (_, T.LocCannotTravel T.None _) -> errCant

                    locObjects = G.Object <$> (E.fromRight [] $ T.saGet locOpts T.LocObjects)
                    locItems = G.Item <$> (E.fromRight [] $ T.saGet locOpts T.LocItems)
                    locActions = rightsIfAll $ E.fromRight [] $ T.tGet locOpts T.LocActions actions 
                      where
                        actions (T.LocActionsV act) = map action act 
                        actions _ = errCant
                        action act = G.ActionUseItemsOnObject 
                          <$> (G.Item <$$> T.saGet act T.AotUsedItems) 
                          <*> (G.Object <$> T.sGet act T.AotUsedOn)
                          <*> Right ""
                          <*> Right []
                    locCond = M.empty--case loc M.!? T.LocCond
