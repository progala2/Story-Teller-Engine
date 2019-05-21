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
import qualified Data.Set as Set
import qualified Data.Either as E
import Game.Types
import Data.List (intercalate)

tests :: IO Bool
tests = checkParallel $$(discover)

prop_useItemsCommand :: Property
prop_useItemsCommand = property $ do
  item <- forAll $ Gen.list (Range.linear 1 20) (Gen.spaces (0, 10) <> Gen.sentence (1, 15) (1, 4) <> Gen.spaces (0, 10))
  let itemsDouble = item ++ item 
  object <-forAll $ Gen.alphaStringNE <> Gen.randSpace <> Gen.alphaStringNE
  (C.parseCommand $ "use items '" ++ (intercalate "," itemsDouble) ++ "' on " ++ object) 
    `eqComm` 
      (C.ItemsOnObject (Set.fromList $ Item . unpack . strip . pack <$> item) (Object object))

prop_travelCommandReadsTravelPlace :: Property
prop_travelCommandReadsTravelPlace = property $ do
  item <- forAll $ (Gen.string (Range.linear 1 20) Gen.alpha)
  (C.parseCommand $ "go to " ++ item) 
    `eqComm` 
      (C.Travel (G.LocName item))

prop_parseCommandNotReadsReturnsError :: Property
prop_parseCommandNotReadsReturnsError = property $ do
  item <- forAll $ (Gen.sentenceSp (1, 10) (1, 10) (1, 10) )
  (E.isLeft $ C.parseCommand $ item) 
    === True

prop_stringSpacesReadsSentencesWithSingleSpace :: Property
prop_stringSpacesReadsSentencesWithSingleSpace = property $ do
  item <- forAll $ Gen.sentence (1, 30) (1, 15)
  (C.parse C.stringSpaces "" item) === (Right item)

prop_stringSpacesReadsSentencesWithManySpace :: Property
prop_stringSpacesReadsSentencesWithManySpace = property $ do
  item <- forAll $ Gen.sentenceSp (2, 5) (1, 30) (1, 3)
  (C.parse C.stringSpaces "" item) === (Right $ item)

newtype EqCommand = EqCommand C.Command deriving (Show)

instance Eq EqCommand where
  EqCommand (C.ItemsOnObject items object) == EqCommand (C.ItemsOnObject items2 object2) = items == items2 && object == object2
  EqCommand (C.Travel item) == EqCommand (C.Travel item2) = item == item2
  _ == _ = False

eqComm :: (MonadTest m) => Either C.ParseError C.Command -> C.Command -> m ()
eqComm (Right a1) a2 = EqCommand a1 === EqCommand a2
eqComm _ _ = True === False