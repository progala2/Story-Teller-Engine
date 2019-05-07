module GameParser.Condition where

import Text.ParserCombinators.Parsec
import qualified GameParser.Tokens as Ge
import GameParser.Language

readCond :: CharParser () Ge.CondType
readCond = readCondN "Condition"

readCondN :: String -> CharParser () Ge.CondType
readCondN name = do 
    skipOptionName name
    str <- choice [string "Local", string "Global"]
    whiteSpaces
    i <- readInt
    whiteSpacesAndEol
    return $ (if str == "Local" then Ge.Local else Ge.Global) i