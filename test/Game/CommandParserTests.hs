{-# LANGUAGE TemplateHaskell #-}
module Game.CommandParserTests where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified TestBase as Gen
import qualified Game.CommandParser as C
import Game.Types
import Data.List (intercalate, any)

tests :: IO Bool
tests = checkParallel $$(discover)

prop_useItemsCommandWithManyItemsWithMinimalSpaces = property $ do
  item <- forAll $ Gen.list (Range.linear 1 100) (
    Gen.alphaStringNE <> Gen.randSpace <> Gen.alphaStringNE )
  object <-forAll $ Gen.list (Range.linear 1 100) Gen.alpha
  (C.parseCommand $ "use items '" ++ (intercalate "," item) ++"' on "++object) 
    === 
      (Right $ C.ItemsOnObject (Item <$> item) (Object object))

instance Eq C.Command where
  C.ItemsOnObject items object == C.ItemsOnObject items2 object2 = items == items2 && object == object
  _ == _ = False