module Game.CommandParser(parseCommand, Command (..)) where

import Extensions.Parsec
import qualified Game.Types as G
import Extensions.Monad

data Command = Travel G.LocName | ItemsOnObject [G.Item] G.Object | CheckBp | PickUpItem G.Item | ThrowItem G.Item deriving(Show)

parseCommand :: String -> Either ParseError Command
parseCommand str = parse command "command" str

command :: CharParser () Command
command = choice [
    Travel . G.LocName <$> tryE (gotoS *> stringSpaces),
    uncurry ItemsOnObject <$> tryE itemsOnObject,
    tryE (checkBpS *> return CheckBp),
    PickUpItem . G.Item <$> tryE (pickUpItemS *> stringSpaces)
    ]
  where
    tryE f = try (f <* endCheck)
    endCheck = spaces *> eof
    itemsOnObject = (,) 
      <$> (G.Item <$$> (itemsOnObjectS *> between (spaces *> string "'" <* spaces) (spaces *> string "'"<* spaces) (sepEndBy1 stringSpaces (try (spaces *> char ',' <* spaces))) <* (spaces *> string "on ")))
      <*> (G.Object <$> stringSpaces)

gotoS :: CharParser () String
gotoS = choiceString ["go to ", "travel to ", "move to "]
itemsOnObjectS :: CharParser () String
itemsOnObjectS = choiceString ["use items "]
checkBpS :: CharParser () String
checkBpS = choiceString ["check bp", "check backpack"]
pickUpItemS :: CharParser () String
pickUpItemS = choiceString ["pick up ", "pup "]

choiceString :: [String] -> CharParser () String
choiceString [] = error "Can't be empty!"
choiceString elems = choice $ (try . string) <$> elems

stringSpaces :: CharParser () String
stringSpaces = many1 letter <> (concat <$> many (try (string " " <> many1 letter) ))