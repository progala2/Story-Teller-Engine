module Game.CommandParser(parseCommand, Command (..)) where

import Text.ParserCombinators.Parsec
import Game.Types

data Command = Travel LocName

parseCommand :: String -> Either ParseError Command
parseCommand str = parse command "command" str

command :: CharParser () Command
command = Travel . LocName <$> (string "go to " *> many anyChar)