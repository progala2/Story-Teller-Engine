module GameEngine where

data GameOption = GameName String | GameVersion String | PlayerCapacity Int | StartingLocation String | EndingLocation String deriving(Show)  
type GameOptions = [GameOption]