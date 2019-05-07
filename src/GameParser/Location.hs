module GameParser.Location ( locationsSection ) where

import Text.ParserCombinators.Parsec
import qualified Data.Set as S
import qualified GameParser.Tokens as Ge
import qualified Data.Map.Strict as M
import GameParser.Language
import GameParser.Condition

locationsSection :: CharParser () Ge.Locations
locationsSection = M.fromList <$> sectionMany "Locations" (try locationSection)

locationSection :: CharParser () (Ge.LocNameStr, Ge.Location)
locationSection = do 
    (name, set) <- sectionManyWithKey "Location" tryOptions (readOption "Name")
    return (name, S.fromList set)
    where 
        tryOptions = choice [
            Ge.LocDescList <$> try readDescList,
            Ge.LocTravelList <$> try readLocTravelList,
            Ge.LocObjects <$> try (inlineSection "Objects"),
            Ge.LocItems <$> try (inlineSection "Items"),
            Ge.LocActions <$> try (readLocActionList),
            Ge.LocCond <$> try (readLocCondList)
            ] 
        readLocTravelList = M.fromList <$> choice [try (sectionMany "Travel Locations" locTravelsParser),
            ( (`tupple` Ge.LocCanTravel) <$> ) <$> inlineSection "Travel Locations"]
          where
            locTravelsParser = choice [try (sectionWithKey "Travel" locTravelParser (readOption "Dest")),
                (`tupple` Ge.LocCanTravel) <$> readValueName]
            locTravelParser = do 
                condType <- option Ge.None (try (readCondN "Travel Condition")) 
                text <- readOption "Cannot Travel Comment"
                return $ Ge.LocCannotTravel condType text
        
        readDescList = M.fromList <$> choice [try (sectionMany "Descriptions" locDescsParser),
            ((tupple 0).(Ge.LocDesc Ge.None) <$>) <$> inlineSection "Description"]
          where
            locDescsParser = sectionWithKey "Description" locDescParser (readOptionInt "Order")
            locDescParser = do 
                text <- readOption "Text" 
                condType <- option Ge.None (try readCond) 
                return $ Ge.LocDesc condType text
        
        readLocActionList = sectionMany "Actions" locActionsParser
          where
            locActionsParser = section "Action" locActionParser
            locActionParser = do
                used <- inlineSection "Used Items"
                usedOn <- readOption "Used On"
                addItems <- inlineSection "Add Items To Location"
                comment <- readOption "Comment"
                objRem <- inlineSection "Objects Remove"
                return $ Ge.Action used usedOn addItems comment objRem
        
        readLocCondList = M.fromList <$> sectionMany "Conditions" locCondsParser
          where
            locCondsParser = sectionWithKey "Condition" locCondParser (readOptionInt "Id")
            locCondParser = do
                items <- option [] (try $ inlineSection "Items Location")
                objRem <- option [] (try $ inlineSection "Objects Not Exist")
                return $ Ge.Condition objRem items
