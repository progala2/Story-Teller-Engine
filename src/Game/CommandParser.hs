module Game.CommandParser(parseCommand, Command (..)) where

import Extensions.Parsec
import qualified Game.Types as G
import Extensions.Monad

data Command = Travel G.LocName | ItemsOnObject [G.Item] G.Object

parseCommand :: String -> Either ParseError Command
parseCommand str = parse command "command" str

command :: CharParser () Command
command = choice [
    Travel . G.LocName <$> try (gotoS *> many anyChar),
    uncurry ItemsOnObject <$> try itemsOnObject 
    ]
  where
    itemsOnObject = (,) 
      <$> (G.Item <$$> (itemsOnObjectS *> manyTill1 (many letter <* (string " ")) (string "on ")))
      <*> (G.Object <$> many letter)
gotoS :: CharParser () String
gotoS = choiceString ["go to ", "Go to ", "travel to ", "move to "]
itemsOnObjectS :: CharParser () String
itemsOnObjectS = choiceString ["use items ", "Use items "]

choiceString :: [String] -> CharParser () String
choiceString [] = error "Can't be empty!"
choiceString elems = choice $ (try . string) <$> elems