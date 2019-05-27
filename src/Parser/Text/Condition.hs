module Parser.Text.Condition where

import           Text.ParserCombinators.Parsec
import qualified Parser.Text.Tokens as Ge
import           Parser.Text.Language
import qualified Data.Map.Strict as M

readCond :: CharParser () Ge.CondType
readCond = readCondN "Condition" 

readCondN :: String -> CharParser () Ge.CondType
readCondN name = do 
    skipOptionName name
    str <- choice [string "Local", string "Global"] <?> "Wrong condition type."
    whiteSpaces
    i <- readInt
    whiteSpacesAndEol
    return $ (if str == "Local" then Ge.Local else Ge.Global) i

readCondsSection :: CharParser () Ge.Conditions
readCondsSection = M.fromList <$> sectionMany "Conditions" condsParser
  where
    condsParser = sectionManyWithKey "Condition" condParser (readOptionInt "Id") >>= (\ (k, a) -> return (k, M.fromList a))
    condParser = choiceToTV [
        (Ge.CotItemsInLocation, Ge.CoArrString <$> inlineSection "Items Location"),
        (Ge.CotObjectsNotExist, Ge.CoArrString <$> inlineSection "Objects Not Exist"),
        (Ge.CotPlayerItems, Ge.CoArrString <$> inlineSection "Player Items")
        ] <?> "Unknown condition option."