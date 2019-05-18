{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Game.CommandParserTests where

import           Data.Text (strip, pack, unpack)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified TestBase as Gen
import qualified Game.CommandParser as C
import qualified Game.Types as G
import qualified Extensions.Parsec as C
import qualified Data.Either as E
import Game.Types
import Data.List (intercalate, any, takeWhile)

tests :: IO Bool
tests = checkParallel $$(discover)

prop_useItemsCommandWithManyItemsWithMinimalSpaces = property $ do
  item <- forAll $ Gen.list (Range.linear 1 100) (Gen.spaces (0, 10) <> Gen.sentence (1, 15) (1, 4) <> Gen.spaces (0, 10))
  object <-forAll $ Gen.alphaStringNE <> Gen.randSpace <> Gen.alphaStringNE
  (C.parseCommand $ "use items '" ++ (intercalate "," item) ++ "' on " ++ object) 
    === 
      (Right $ C.ItemsOnObject (Item . unpack . strip . pack <$> item) (Object object))

prop_travelCommandReadsTravelPlace = property $ do
  item <- forAll $ (Gen.string (Range.linear 1 50) Gen.alpha)
  (C.parseCommand $ "go to " ++ item) 
    === 
      (Right $ C.Travel (G.LocName item))

prop_parseCommandNotReadsReturnsError = property $ do
  item <- forAll $ (Gen.sentenceSp (1, 10) (1, 10) (1, 10) )
  (E.isLeft $ C.parseCommand $ item) 
    === True

prop_stringSpacesReadsSentencesWithSingleSpace = property $ do
  item <- forAll $ Gen.sentence (1, 30) (1, 15)
  (C.parse C.stringSpaces "" item) === (Right item)

prop_stringSpacesReadsSentencesWithManySpace = property $ do
  item <- forAll $ Gen.sentenceSp (2, 5) (1, 30) (1, 3)
  (C.parse C.stringSpaces "" item) === (Right $ item)

instance Eq C.Command where
  C.ItemsOnObject items object == C.ItemsOnObject items2 object2 = items == items2 && object == object
  C.Travel item == C.Travel item2 = item == item2
  _ == _ = False