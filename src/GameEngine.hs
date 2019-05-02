module GameEngine where

import qualified Data.Set as S

data GameOption = GameName String | GameVersion String | PlayerCapacity Int | StartingLocation String | EndingLocation String deriving(Show)  
type GameOptions = S.Set GameOption

instance Ord GameOption where
    GameName _ <= GameName _ = True
    GameVersion _ <= GameVersion _ = True
    PlayerCapacity _ <= PlayerCapacity _ = True
    StartingLocation _ <= StartingLocation _ = True
    EndingLocation _ <= EndingLocation _ = True
    _ <= _ = False

instance Eq GameOption where
    GameName _ == GameName _ = True
    GameVersion _ == GameVersion _ = True
    PlayerCapacity _ == PlayerCapacity _ = True
    StartingLocation _ == StartingLocation _ = True
    EndingLocation _ == EndingLocation _ = True
    _ == _ = False