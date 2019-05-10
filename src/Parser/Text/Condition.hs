module Parser.Text.Condition where

import Text.ParserCombinators.Parsec
import qualified Parser.Text.Tokens as Ge
import Parser.Text.Language

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