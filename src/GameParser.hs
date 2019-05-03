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
            Ge.GameVersion <$> tryOption "Game Version",
            Ge.GameName <$> tryOption "Game Name",
            Ge.PlayerCapacity <$> tryOptionInt "Player Capacity",
            Ge.StartingLocation <$> tryOption "Starting Location",
            Ge.EndingLocation <$> tryOption "Ending Location"
            ] 
        tryOption name = try (readOption name)
        tryOptionInt name = try (readOptionInt name)
        readOption name = do 
            skipAssignName name
            str <- between (char '"') (char '"') (many (noneOf "\""))
            whiteSpacesAndEol
            return str
        readOptionInt name = do 
            skipAssignName name
            str <- read <$> (many (alphaNum))
            whiteSpacesAndEol
            return str

locationSection :: CharParser () Ge.Location
locationSection = return $ S.fromList $ [Ge.LocName "dd"]

section :: String -> CharParser () a -> CharParser () [a]
section name parser = do 
    spaces 
    string' ("<" ++ name) 
    (manyTill parser (try (spaces *> string "/>")))

skipAssignName :: String -> CharParser () ()
skipAssignName name = do 
    spaces
    string' name
    whiteSpaces
    _ <- char '='
    whiteSpaces

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