{-# LANGUAGE TemplateHaskell #-}
module Game.CommandParserTests(commandParserTestsList) where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Game.CommandParser
import Game.Types
import Fake

commandParserTestsList = TestList [
    TestLabel "UseItemsCommandWithOneItem" (useItemsCommandWithOneItem "item" "object")]

useItemsCommandWithOneItem item object = TestCase (assertEqual "" (parseCommand $ "use items '"++item++"' on "++object) (Right $ ItemsOnObject [Item item] (Object object)))
useItemsCommandWithManyItems item object = TestCase (assertEqual "" (parseCommand $ "use items '"++item++"' on "++object) (Right $ ItemsOnObject [Item item] (Object object)))

instance Eq Command where
  ItemsOnObject items object == ItemsOnObject items2 object2 = items == items2 && object == object
  _ == _ = False 