-- |
-- Module: Command
-- Description: Availible commands and parser of them.
--
-- This module provides Commands data and the mean to parse it from a string.
module Game.Command(parseCommand, Command(..)) where

import           Extensions.Parsec
import qualified Game.GameState as G
import           Extensions.Monad
import qualified Data.Set as Set

-- | Available commands in a game.
data Command = 
  Travel G.LocName | 
  ItemsOnObject (Set.Set G.Item) G.Object | 
  UniqueCommand String (Maybe G.Object) | 
  CheckBp | 
  PickUpItem G.Item | 
  ThrowItem G.Item | 
  ExitGame | 
  ExitAndSave | 
  Help deriving(Show)

-- | Parse the given string to a 'Command'.
--
-- @[]@ are used to show alternative commands, @{}@ are used to show what a player can write in.
--
-- 'Travel':
--
-- @
--  [go to\\travel to\\move to\\gt] {locationName}
--  ex. go to Old House
-- @
--
-- 'ItemsOnObject':
--
-- @
--  [use items\\ui] '{item},{item2},...' on {object name}
--  ex. ui 'Axe' on New Door
-- @
--
-- 'UniqueCommand':
--
-- @
--  {command} [on// ] {object name}
--  {command}
--  ex. paint New Door
--  ex. super not useful command
-- @
--
-- 'CheckBp':
--
-- @
--  [check bp//check backpack//cbp]
-- @
--
-- 'ExitGame':
--
-- @
--  [:exit//:q]
-- @
--
-- 'ExitAndSave':
--
-- @
--  [:exitSave//:qs]
-- @
--
-- 'Help':
--
-- @
--  [:help//:h]
-- @
--
-- 'PickUpItem':
--
-- @
--  [pick up//pup] {item name}
--  ex. pup Fantastic Sword
-- @
--
-- 'ThrowItem':
--
-- @
--  [throw item//thri] {item name}
--  ex. thri Fantastic Sword
-- @
parseCommand :: String -> Either ParseError Command
parseCommand str = parse command "command" str

command :: CharParser () Command
command = tryEChoice [
    Travel . G.LocName <$> (gotoS *> stringSpacebs),
    itemsOnObject,
    checkBpS *> return CheckBp,
    PickUpItem . G.Item <$> (pickUpItemS *> stringSpacebs),
    ThrowItem . G.Item <$> (throwItemS *> stringSpacebs),
    exitGameS *> return ExitGame,
    exitAndSaveS *> return ExitAndSave,
    helpS *> return Help,
    UniqueCommand <$> (letters1 <* spacebs1 <* string "on" <* spacebs) <*> (Just . G.Object <$> stringSpacebs),
    UniqueCommand <$> (letters1 <* spacebs1) <*> (Just . G.Object <$> stringSpacebs),
    UniqueCommand <$> letters1 <.> Nothing
    ] <?> "I don't understand..."
  where
    tryEChoice xs = choice (tryE <$> xs)
    tryE f = try (f <* spacebs <* eof)
    itemsOnObject = ItemsOnObject
      <$> (G.itemSetFromList <$> (itemsOnObjectS *> between sQuotaBtwnSpacebs sQuotaBtwnSpacebs (sepEndBy1 stringSpacebs (try (btwnSpacebs $ char ','))) <* (spacebs *> string "on ")))
      <*> (G.Object <$> stringSpacebs)

gotoS :: CharParser () String
gotoS = choiceString ["go to ", "travel to ", "move to ", "gt "]
itemsOnObjectS :: CharParser () String
itemsOnObjectS = choiceString ["use items ", "ui "]
checkBpS :: CharParser () String
checkBpS = choiceString ["check bp", "check backpack", "cbp"]
pickUpItemS :: CharParser () String
pickUpItemS = choiceString ["pick up ", "pup "]
throwItemS :: CharParser () String
throwItemS = choiceString ["throw item ", "thri "]
exitGameS :: CharParser () String
exitGameS = choiceString [":exit", ":q"]
exitAndSaveS :: CharParser () String
exitAndSaveS = choiceString [":exitSave", ":qs"]
helpS :: CharParser () String
helpS = choiceString [":help", ":h"]