{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Game.CommandParserTests where

import           Data.Text (strip, pack, unpack)
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
  item <- forAll $ Gen.list (Range.linear 1 100) (Gen.spaces (0, 10) <> Gen.sentence (1, 15) (1, 4) <> Gen.spaces (0, 10))
  object <-forAll $ Gen.alphaStringNE <> Gen.randSpace <> Gen.alphaStringNE
  (C.parseCommand $ "use items '" ++ (intercalate "," item) ++ "' on " ++ object) 
    === 
      (Right $ C.ItemsOnObject (Item . unpack . strip . pack <$> item) (Object object))

instance Eq C.Command where
  C.ItemsOnObject items object == C.ItemsOnObject items2 object2 = items == items2 && object == object
  _ == _ = False