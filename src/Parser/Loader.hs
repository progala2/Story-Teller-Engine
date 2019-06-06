module Parser.Loader(loadGame) where

import           Extensions.Monad
import qualified Game.GameState as G
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Parser.Text.Tokens as T
import           Parser.Text.Parser 
import qualified Parser.Text.Option as T
import           Parser.Game.Location
import           Parser.Game.Condition
import           Parser.Errors

loadGame :: String -> Either LoaderError G.GameStatus
loadGame = (either (Left . show) tokensToGame) . parseGameFile 
  where 
    tokensToGame (go, conds, locs, (intro, outro)) = do 
      ws@(_, glocs) <- worldStatus
      (,) <$> playerStatus glocs <.> ws
      where
        playerStatus glocs = G.PlayerStatus <$> (startLocName >>= startLoc) <*> Right Set.empty
          where
            startLocName = G.LocName <$> T.sGet go T.StartingLocation
            startLoc s = case glocs M.!? s of 
              Just a -> Right (s, a)
              _ -> Left "There is no starting location like that!"
        worldStatus = (,) <$> gameOptions <*> locations
          where
            gameOptions = G.GameOptions 
                <$> gameName
                <*> gameVersion 
                <*> playerCapacity
                <*> endingLocation
                <.> conditionsLoader conds
                <.> intro
                <.> outro
              where
                gameName = T.sGet go T.GameName
                gameVersion = T.sGet go T.GameVersion
                playerCapacity =  T.iGet go T.PlayerCapacity
                endingLocation = G.LocName <$> T.sGet go T.EndingLocation
            
            locations = M.fromList <$> rightsIfAll ((location locs) <$> M.toList locs)
