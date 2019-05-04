module Game.CommandParser(parseCommand, Command (..)) where

import Text.ParserCombinators.Parsec

data Command = Travel String

parseCommand :: String -> Either ParseError Command
parseCommand str = parse command "command" str

command :: CharParser () Command
command = Travel <$> (string "go to " *> many anyChar)