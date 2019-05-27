module Game.CommandParser where

import Extensions.Parsec
import qualified Game.Types as G
import Extensions.Monad
import qualified Data.Set as Set

data Command = Travel G.LocName | ItemsOnObject (Set.Set G.Item) G.Object | UniqueCommand String (Maybe G.Object) | CheckBp | PickUpItem G.Item | ThrowItem G.Item 
 | ExitGame | ExitAndSave deriving(Show)

parseCommand :: String -> Either ParseError Command
parseCommand str = parse command "command" str

command :: CharParser () Command
command = choice [
    Travel . G.LocName <$> tryE (gotoS *> stringSpaces),
    uncurry ItemsOnObject <$> tryE itemsOnObject,
    tryE (checkBpS *> return CheckBp),
    PickUpItem . G.Item <$> tryE (pickUpItemS *> stringSpaces),
    tryE (exitGameS *> return ExitGame),
    tryE (exitAndSaveS *> return ExitAndSave),
    uncurry UniqueCommand <$> tryE ((,) <$> (many1 letter <* many1 (char ' ') <* string "on" <* spaces) <*> (Just . G.Object <$> stringSpaces)),
    uncurry UniqueCommand <$> tryE ((,) <$> (many1 letter <* many1 (char ' ')) <*> (Just . G.Object <$> stringSpaces)),
    uncurry UniqueCommand <$> tryE ((,) <$> (many1 letter) <.> Nothing) 
    ] <?> "I don't understand..."
  where
    tryE f = try (f <* endCheck)
    endCheck = spaces *> eof
    itemsOnObject = (,) 
      <$> (G.itemSetFromList <$> (itemsOnObjectS *> between singleQuotaSpace singleQuotaSpace (sepEndBy1 stringSpaces (try (btwnSpaces $ char ','))) <* (spaces *> string "on ")))
      <*> (G.Object <$> stringSpaces)

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

choiceString :: [String] -> CharParser () String
choiceString [] = error "Can't be empty!"
choiceString elems = choice $ (try . string) <$> elems

stringSpaces :: CharParser () String
stringSpaces = many1 letter <> (concat <$> many (try (many1 (char ' ') <> many1 letter) ))

singleQuotaSpace :: CharParser () String
singleQuotaSpace = btwnSpaces $ string "'"