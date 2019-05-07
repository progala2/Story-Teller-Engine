module GameParser.Loader(parseTokensToGame) where

import qualified Game.Types as G
import qualified Data.Map.Strict as M
import qualified GameParser.Tokens as T

type GeneratorError = String
parseTokensToGame::(T.GameOptions, T.Locations) -> Either GeneratorError G.GameStatus
parseTokensToGame (go, loc) = (,) <$> (playerStatus go) <*> (worldStatus go)
    where 
        playerStatus go = G.PlayerStatus <$> (startingLocation go) <*> Right []
        worldStatus go = (,) <$> (gameOptions go) <*> Right M.empty
          where
            gameOptions go = G.GameOptions 
                <$> (gameVersion go) 
                <*> (gameVersion go) 
                <*> (playerCapacity go) 
                <*> (endingLocation go)
            locations loc = map


gameName go = case go M.!? T.GameName of Just (T.GameOptionString str) -> Right str; _ -> errO T.GameName
gameVersion go = case go M.!? T.GameVersion of Just (T.GameOptionString str) -> Right str; _ -> errO T.GameVersion
playerCapacity go =  case go M.!? T.PlayerCapacity of Just(T.GameOptionInt str) -> Right str; _ -> errO T.PlayerCapacity
startingLocation go = case go M.!? T.StartingLocation of Just(T.GameOptionString str) -> Right $ G.LocName str; _ -> errO T.StartingLocation
endingLocation go = case go M.!? T.EndingLocation of Just(T.GameOptionString str) -> Right str; _ -> errO T.EndingLocation

errO str = Left $ "There is no " ++ (show str) ++ " option."