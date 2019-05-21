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
import qualified Data.List.Ordered as LO

tests :: IO Bool
tests = checkParallel $$(discover)

prop_handleCommandItemsOnObjectWithSomeItemsMissing :: Property
prop_handleCommandItemsOnObjectWithSomeItemsMissing = property $ do
  itms <- forAll $ itemsS (R.linear 1 5) Gen.lower
  itemsMiss <- forAll $ itemsS (R.linear 1 5) Gen.upper
  allItems <- forAll $ Set.fromList <$> Gen.shuffle (itms ++ itemsMiss)
  obj <- forAll $ object
  gs <- forAll $ gameStatus (Set.fromList itms) obj
  (S.evalState (G.handleCommand (C.ItemsOnObject allItems obj)) gs)
    ===
      ("You don't have these items: " ++ (show $ LO.sort itemsMiss))

prop_handleCommandItemsOnObjectNoAction :: Property
prop_handleCommandItemsOnObjectNoAction = property $ do
  itms <- forAll $ Set.fromList <$> items (R.linear 1 5)
  obj <- forAll $ object
  gs <- forAll $ gameStatus itms obj
  (S.evalState (G.handleCommand (C.ItemsOnObject itms obj)) gs)
    ===
      ("I can't do it.")

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

gameStatus :: MonadGen m => Set.Set G.Item -> G.Object -> m G.GameStatus
gameStatus itms obj = do
    go <- gameOptions
    return (G.PlayerStatus (G.LocName "a") itms, (go, M.fromList [(G.LocName "a", G.Location M.empty M.empty (Set.fromList [obj]) Set.empty [] M.empty )]))