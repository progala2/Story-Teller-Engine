module Parser.Text.Parser
    ( parseGameFile
    ) where

import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as M
import qualified Parser.Text.Tokens as T
import Parser.Text.Language
import Parser.Text.Location
import Parser.Text.Condition

type ParsingResult = (T.GameOptions, T.Conditions, T.LocMap, (T.IntroText, T.OutroText))
parseGameFile :: String -> Either ParseError ParsingResult
parseGameFile str = parse gameFile "(unknown)" str

gameFile :: CharParser () ParsingResult
gameFile = do 
    gameBeginToken
    go <- headersSection
    spaces
    intro <- section "Intro" (readOption "Text")
    spaces
    locations <- locationsSection
    spaces
    conds <- readCondsSection
    spaces
    outro <- section "Outro" (readOption "Text")
    spaces
    return (go, conds, locations, (intro, outro))

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
    <* spacebsAndEol'