module GameParser.Parser
    ( parseGameFile
    ) where

import Text.ParserCombinators.Parsec
import qualified Data.Set as S
import qualified GameEngine as Ge
import qualified Data.Map.Strict as M
import GameParser.Language

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

locationsSection :: CharParser () Ge.Locations
locationsSection = M.fromList <$> sectionMany "Locations" (try locationSection)

locationSection :: CharParser () (Ge.LocNameStr, Ge.Location)
locationSection = do 
    (name, set) <- sectionManyWithKey "Location" tryOptions (readOption "Name")
    return (name, S.fromList set)
    where 
        tryOptions = choice [
            Ge.LocDescList <$> tryReadDescList
            ] 
        tryReadDescList = M.fromList <$> sectionMany "Descriptions" locDescsParser
        where
            locDescsParser = sectionWithKey "Description" locDescParser (readOptionInt "Order")
            locDescParser = do 
                text <- readOption "Text" 
                condType <- option Ge.None (try readCond) 
                return $ Ge.LocDesc condType text

readCond :: CharParser () Ge.CondType
readCond = do 
    skipOptionName "Condition"
    str <- choice [string "Local", string "Global"]
    whiteSpaces
    i <- readInt
    whiteSpacesAndEol
    return $ (if str == "Local" then Ge.Local else Ge.Global) i

gameBeginToken :: CharParser () ()
gameBeginToken = 
    spaces 
    *> string' "<Game"
    <* whiteSpacesAndEol