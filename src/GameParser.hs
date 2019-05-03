module GameParser
    ( parseGameFile
    ) where

import Text.ParserCombinators.Parsec
import qualified Data.Set as S
import GameEngine

parseGameFile :: String -> Either ParseError GameOptions
parseGameFile str = parse gameFile "(unknown)" str

gameFile :: CharParser () GameOptions
gameFile = do 
    gameBeginToken
    go <- headersSection
    _ <- many anyChar
    return go

headersSection :: CharParser () GameOptions
headersSection = do
    spaces
    string' "<Headers"
    go <- S.fromList <$> (manyTill tryOptions (try (spaces *> string "/>")))
    return go
      where 
        tryOptions = choice [
            GameVersion <$> tryOption "Game Version",
            GameName <$> tryOption "Game Name",
            PlayerCapacity <$> tryOptionInt "Player Capacity",
            StartingLocation <$> tryOption "Starting Location",
            EndingLocation <$> tryOption "Ending Location"
            ] 
        tryOption name = try (readOption name)
        tryOptionInt name = try (readOptionInt name)
        readName name =  
            do 
                spaces
                string' name
                whiteSpaces
                _ <- char '='
                whiteSpaces
        readOption name =
            do 
                readName name
                str <- between (char '"') (char '"') (many (noneOf "\""))
                whiteSpacesAndEol
                return str
        readOptionInt name = 
            do 
                readName name
                str <- read <$> (many (alphaNum))
                whiteSpacesAndEol
                return str

gameBeginToken :: CharParser () ()
gameBeginToken = do
    spaces 
    _ <- string "<Game"
    whiteSpacesAndEol

eol :: CharParser () String
eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

eol' :: CharParser () ()
eol' = eol >> return ()

string' :: String -> CharParser () ()
string' str = string str *> return ()

whiteSpaces :: CharParser () ()
whiteSpaces = many (oneOf " \t\f\v")  *> return ()
whiteSpacesAndEol :: CharParser () ()
whiteSpacesAndEol = whiteSpaces *> eol'