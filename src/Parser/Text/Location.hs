module Parser.Text.Location ( locationsSection ) where

import           Extensions.Parsec
import           Extensions.Monad ((<$$>))
import qualified Parser.Text.Tokens as Ge
import qualified Data.Map.Strict as M
import           Parser.Text.Language
import           Parser.Text.Condition
import           Data.Tuple(swap)

locationsSection :: CharParser () Ge.LocMap
locationsSection = M.fromList <$> sectionMany "Locations" (try locationSection)

locationSection :: CharParser () (Ge.LocNameStr, Ge.Location)
locationSection = do 
    (name, set) <- sectionManyWithKey "Location" tryOptions (readOption "Name")
    return (name, M.fromList set)
    where 
        tryOptions = choiceToTV [
            (Ge.LocDescList, Ge.LocDescListV <$> readDescList),
            (Ge.LocTravelList, Ge.LocTravelListV <$> readLocTravelList),
            (Ge.LocItems, Ge.LocStrings <$> (inlineSection "Items")),
            (Ge.LocObjects, Ge.LocStrings <$> (inlineSection "Objects")),
            (Ge.LocActions, Ge.LocActionsV <$> readLocActionList),
            (Ge.LocCond, Ge.LocCondV <$> readCondsSection)
            ]  <?> "Not acceptable location option."
        readLocTravelList = M.fromList <$> choice [try (sectionMany "Travel Locations" locTravelsParser),
            (swap . (,) Ge.LocCanTravel) <$$> inlineSection "Travel Locations"]
          where
            locTravelsParser = choice [try (sectionWithKey "Travel" locTravelParser (readOption "Dest")),
                (swap . (,) Ge.LocCanTravel) <$> readValueName]
            locTravelParser = do 
                condType <- option Ge.None (try (readCondN "Travel Condition")) 
                text <- readOption "Cannot Travel Comment"
                return $ Ge.LocCannotTravel condType text
        
        readDescList = M.fromList <$> choice [try (sectionMany "Descriptions" locDescsParser),
            ((,) 0) . (Ge.LocDesc Ge.None) <$$> inlineSection "Description"]
          where
            locDescsParser = sectionWithKey "Description" locDescParser (readOptionInt "Order")
            locDescParser = do 
                text <- readOption "Text" 
                condType <- option Ge.None (try readCond) 
                return $ Ge.LocDesc condType text
        
        readLocActionList = sectionMany "Actions" locActionsParser
          where
            locActionsParser = M.fromList <$> sectionMany "Action" locActionParser
            locActionParser = choiceToTV [
              (Ge.AotType, Ge.AoString <$> (readOption "Type")),
              (Ge.AotUsedItems, Ge.AoArrString <$> (inlineSection "Used Items")),
              (Ge.AotUsedOn, Ge.AoString <$> (readOption "Used On")),
              (Ge.AotAddItemsToLocation, Ge.AoArrString <$> (inlineSection "Add Items To Location")),
              (Ge.AotComment, Ge.AoString <$> (readOption "Comment")),
              (Ge.AotObjectsRemove, Ge.AoArrString <$> (inlineSection "Objects Remove")),
              (Ge.AotCommands, Ge.AoArrString <$> (inlineSection "Commands"))
              ] <?> "Not acceptable action option."