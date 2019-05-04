module GameParser.Parser
    ( parseGameFile
    ) where

import Text.ParserCombinators.Parsec
import qualified Data.Set as S
import qualified GameEngine as Ge
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
    S.fromList <$> sectionMany "Headers" tryOptions
      where 
        tryOptions = choice [
            Ge.GameVersion <$> tryReadOption "Game Version",
            Ge.GameName <$> tryReadOption "Game Name",
            Ge.PlayerCapacity <$> tryOptionInt "Player Capacity",
            Ge.StartingLocation <$> tryReadOption "Starting Location",
            Ge.EndingLocation <$> tryReadOption "Ending Location"
            ] 

gameBeginToken :: CharParser () ()
gameBeginToken = 
    spaces 
    *> string' "<Game"
    <* whiteSpacesAndEol