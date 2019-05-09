module GameParser.Location ( locationsSection ) where

import Text.ParserCombinators.Parsec
import qualified GameParser.Tokens as Ge
import qualified Data.Map.Strict as M
import GameParser.Language
import GameParser.Condition

locationsSection :: CharParser () Ge.Locations
locationsSection = M.fromList <$> sectionMany "Locations" (try locationSection)

locationSection :: CharParser () (Ge.LocNameStr, Ge.Location)
locationSection = do 
    (name, set) <- sectionManyWithKey "Location" tryOptions (readOption "Name")
    return (name, M.fromList set)
    where 
        tryOptions = choice [
            tryToTV Ge.LocDescList Ge.LocDescListV readDescList,
            tryToTV Ge.LocTravelList Ge.LocTravelListV readLocTravelList,
            tryToTV Ge.LocItems Ge.LocStrings (inlineSection "Items"),
            tryToTV Ge.LocObjects Ge.LocStrings (inlineSection "Objects"),
            tryToTV Ge.LocActions Ge.LocActionsV (readLocActionList),
            tryToTV Ge.LocCond Ge.LocCondV (readLocCondList)
            ]  <?> "Not acceptable header option."
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
            locActionsParser = M.fromList <$> sectionMany "Action" locActionParser
            locActionParser = choice [
              tryToTV Ge.AotType Ge.AoString (readOption "Type"),
              tryToTV Ge.AotUsedItems Ge.AoArrString (inlineSection "Used Items"),
              tryToTV Ge.AotUsedOn Ge.AoString $ readOption "Used On",
              tryToTV Ge.AotAddItemsToLocation Ge.AoArrString $ inlineSection "Add Items To Location",
              tryToTV Ge.AotComment Ge.AoString $ readOption "Comment",
              tryToTV Ge.AotObjectsRemove Ge.AoArrString $ inlineSection "Objects Remove"
              ] <?> "Not acceptable action option."
        
        readLocCondList = M.fromList <$> sectionMany "Conditions" locCondsParser
          where
            locCondsParser = sectionWithKey "Condition" locCondParser (readOptionInt "Id")
            locCondParser = do
                items <- option [] (try $ inlineSection "Items Location")
                objRem <- option [] (try $ inlineSection "Objects Not Exist")
                return $ Ge.Condition objRem items

tryToTV :: a -> (b -> c) -> CharParser () b -> CharParser () (a, c)
tryToTV t v r = (,)t . v <$> try r
