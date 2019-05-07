module GameParser.Parser
    ( parseGameFile
    ) where

import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as M
import qualified GameParser.Tokens as Ge
import GameParser.Language
import GameParser.Location

parseGameFile :: String -> Either ParseError (Ge.GameOptions, Ge.Locations)
parseGameFile str = parse gameFile "(unknown)" str

gameFile :: CharParser () (Ge.GameOptions, Ge.Locations)
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

headersSection :: CharParser () Ge.GameOptions
headersSection = do
    M.fromList <$> sectionMany "Headers" tryOptions
      where 
        tryOptions = choice [
            (,) Ge.GameVersion . Ge.GameOptionString <$> tryReadOption "Game Version",
            (,) Ge.GameName . Ge.GameOptionString <$> tryReadOption "Game Name",
            (,) Ge.PlayerCapacity . Ge.GameOptionInt <$> tryOptionInt "Player Capacity",
            (,) Ge.StartingLocation . Ge.GameOptionString <$> tryReadOption "Starting Location",
            (,) Ge.EndingLocation . Ge.GameOptionString <$> tryReadOption "Ending Location"
            ] 

gameBeginToken :: CharParser () ()
gameBeginToken = 
    spaces 
    *> string' "<Game"
    <* whiteSpacesAndEol