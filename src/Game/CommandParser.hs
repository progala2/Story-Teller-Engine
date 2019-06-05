module Game.CommandParser(parseCommand, Command(..)) where

import           Extensions.Parsec
import qualified Game.GameState as G
import           Extensions.Monad
import qualified Data.Set as Set

-- | Available commands in a game.
data Command = 
  --
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
parseCommand :: String -> Either ParseError Command
parseCommand str = parse command "command" str

command :: CharParser () Command
command = tryEChoice [
    Travel . G.LocName <$> (gotoS *> stringSpacebs),
    itemsOnObject,
    checkBpS *> return CheckBp,
    PickUpItem . G.Item <$> (pickUpItemS *> stringSpacebs),
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
      <$> (G.itemSetFromList <$> (itemsOnObjectS *> between sQuotasBtwnSpacebs sQuotasBtwnSpacebs (sepEndBy1 stringSpacebs (try (btwnSpacebs $ char ','))) <* (spacebs *> string "on ")))
      <*> (G.Object <$> stringSpacebs)

gotoS :: CharParser () String
gotoS = choiceString ["go to ", "travel to ", "move to ", "gt "]
itemsOnObjectS :: CharParser () String
itemsOnObjectS = choiceString ["use items ", "ui "]
checkBpS :: CharParser () String
checkBpS = choiceString ["check bp", "check backpack", "cbp"]
pickUpItemS :: CharParser () String
pickUpItemS = choiceString ["pick up ", "pup "]
exitGameS :: CharParser () String
exitGameS = choiceString [":exit", ":q"]
exitAndSaveS :: CharParser () String
exitAndSaveS = choiceString [":exitSave", ":qs"]
helpS :: CharParser () String
helpS = choiceString [":help", ":h"]