module Parser.Text.Parser
    ( parseGameFile
    ) where

import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as M
import qualified Parser.Text.Tokens as T
import Parser.Text.Language
import Parser.Text.Location
import Parser.Text.Condition

parseGameFile :: String -> Either ParseError (T.GameOptions, T.Conditions, T.Locations)
parseGameFile str = parse gameFile "(unknown)" str

gameFile :: CharParser () (T.GameOptions, T.Conditions, T.Locations)
gameFile = do 
    gameBeginToken
    go <- headersSection
    spaces
    _ <- section "Intro" (many (noneOf "/>"))
    spaces
    locations <- locationsSection
    spaces
    conds <- readCondsSection
    spaces
    _ <- section "Outro" (many (noneOf "/>"))
    spaces
    return (go, conds, locations)

headersSection :: CharParser () T.GameOptions
headersSection = do
    M.fromList <$> sectionMany "Headers" tryOptions
      where 
        tryOptions = choiceToTV [
             (T.GameVersion, T.GameOptionString <$> readOption "Game Version"),
             (T.GameName, T.GameOptionString <$> readOption "Game Name"),
             (T.PlayerCapacity, T.GameOptionInt <$> readOptionInt "Player Capacity"),
             (T.StartingLocation, T.GameOptionString <$> readOption "Starting Location"),
             (T.EndingLocation, T.GameOptionString <$> readOption "Ending Location")
             ] 

gameBeginToken :: CharParser () ()
gameBeginToken = 
    spaces 
    *> string' "<Game"
    <* whiteSpacesAndEol