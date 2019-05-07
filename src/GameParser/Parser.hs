module GameParser.Parser
    ( parseGameFile
    ) where

import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as M
import qualified GameParser.Tokens as T
import GameParser.Language
import GameParser.Location

parseGameFile :: String -> Either ParseError (T.GameOptions, T.Locations)
parseGameFile str = parse gameFile "(unknown)" str

gameFile :: CharParser () (T.GameOptions, T.Locations)
gameFile = do 
    gameBeginToken
    go <- headersSection
    spaces
    _ <- section "Intro" (many (noneOf "/>"))
    spaces
    locations <- locationsSection
    spaces
    _ <- many anyChar
    return (go, locations)

headersSection :: CharParser () T.GameOptions
headersSection = do
    M.fromList <$> sectionMany "Headers" tryOptions
      where 
        tryOptions = choice [
            (,) T.GameVersion . T.GameOptionString <$> tryReadOption "Game Version",
            (,) T.GameName . T.GameOptionString <$> tryReadOption "Game Name",
            (,) T.PlayerCapacity . T.GameOptionInt <$> tryOptionInt "Player Capacity",
            (,) T.StartingLocation . T.GameOptionString <$> tryReadOption "Starting Location",
            (,) T.EndingLocation . T.GameOptionString <$> tryReadOption "Ending Location"
            ] 

gameBeginToken :: CharParser () ()
gameBeginToken = 
    spaces 
    *> string' "<Game"
    <* whiteSpacesAndEol