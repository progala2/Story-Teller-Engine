{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Game.RunnerTests where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as R
import qualified TestBase as Gen
import qualified Game.Runner as G
import qualified Game.CommandParser as C
import qualified Game.Types as G
import qualified Control.Monad.State.Lazy as S
import qualified Data.Set as Set
import qualified Data.Map.Strict as M
import Extensions.Monad

tests :: IO Bool
tests = checkParallel $$(discover)

prop_handleCommandItemsOnObjectWithSomeItemsMissing :: Property
prop_handleCommandItemsOnObjectWithSomeItemsMissing = property $ do
  itms <- forAll $ itemsS (R.linear 1 5) Gen.lower
  itemsMiss <- forAll $ itemsS (R.linear 1 5) Gen.upper
  allItems <- forAll $ Gen.shuffle (itms ++ itemsMiss)
  obj <- forAll $ object
  gs <- forAll $ gameStatus itms
  (S.evalState (G.handleCommand (C.ItemsOnObject (reverse allItems) obj)) gs)
    ===
      ("You don't have these items: " ++ (show itemsMiss))

--prop_hasNoItems = property $ do

prop_handleCommandItemsOnObjectNoItemsMissing :: Property
prop_handleCommandItemsOnObjectNoItemsMissing = property $ do
  itms <- forAll $ items (R.linear 1 5)
  gs <- forAll $ gameStatus itms
  obj <- forAll $ object
  (S.evalState (G.handleCommand (C.ItemsOnObject itms obj)) gs)
    ===
      ("You don't have these items: ")

gameOptions :: MonadGen m => m G.GameOptions
gameOptions = G.GameOptions 
  <$> Gen.alphaStringNE 
  <*> Gen.alphaStringNE 
  <*> Gen.int (R.linear 1 5) 
  <*> (G.LocName <$> Gen.alphaStringNE)

items :: MonadGen m => R.Range Int -> m [G.Item]
items r = G.Item <$$> (Gen.strings r (R.linear 10 20) Gen.alpha)

itemsS :: MonadGen m => R.Range Int -> m Char -> m [G.Item]
itemsS r g = G.Item <$$> (Gen.strings r (R.linear 10 20) g)
object :: MonadGen m => m G.Object
object = G.Object <$> (Gen.string (R.linear 10 20) Gen.alpha)

gameStatus :: MonadGen m => [G.Item] -> m G.GameStatus
gameStatus itms = do
    go <- gameOptions
    return (G.PlayerStatus (G.LocName "") (Set.fromList itms), (go, M.empty))