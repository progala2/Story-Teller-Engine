module GameParser
    ( parseGameFile
    ) where

import Text.ParserCombinators.Parsec
import qualified Data.Set as S
import qualified GameEngine as Ge

parseGameFile :: String -> Either ParseError (Ge.GameOptions, Ge.Location)
parseGameFile str = parse gameFile "(unknown)" str

gameFile :: CharParser () (Ge.GameOptions, Ge.Location)
gameFile = do 
    gameBeginToken
    go <- headersSection
    spaces
    location <- locationSection
    _ <- many anyChar
    return (go, location)

headersSection :: CharParser () Ge.GameOptions
headersSection = do
    S.fromList <$> section "Headers" tryOptions
      where 
        tryOptions = choice [
            Ge.GameVersion <$> tryReadOption "Game Version",
            Ge.GameName <$> tryReadOption "Game Name",
            Ge.PlayerCapacity <$> tryOptionInt "Player Capacity",
            Ge.StartingLocation <$> tryReadOption "Starting Location",
            Ge.EndingLocation <$> tryReadOption "Ending Location"
            ] 
        tryOptionInt name = try (readOptionInt name)
        readOptionInt name = do 
            skipOptionName name
            str <- read <$> (many (alphaNum))
            whiteSpacesAndEol
            return str

locationSection :: CharParser () Ge.Location
locationSection = S.fromList <$> section "Location" tryOptions
    where 
        tryOptions = choice [
            Ge.LocName <$> tryReadOption "Name"
            ] 

section :: String -> CharParser () a -> CharParser () [a]
section name parser = do 
    spaces 
    string' ("<" ++ name) 
    (manyTill parser (try (spaces *> string "/>")))

skipOptionName :: String -> CharParser () ()
skipOptionName name = do 
    spaces
    string' name
    whiteSpaces
    _ <- char '='
    whiteSpaces

tryReadOption :: String -> CharParser () String
tryReadOption name = try (readOption name)

readOption :: String -> CharParser () String
readOption name = do 
    skipOptionName name
    str <- between (char '"') (char '"') (many (noneOf "\""))
    whiteSpacesAndEol
    return str

gameBeginToken :: CharParser () ()
gameBeginToken = 
    spaces 
    *> string' "<Game"
    <* whiteSpacesAndEol

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