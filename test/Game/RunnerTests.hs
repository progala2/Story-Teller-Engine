{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Game.RunnerTests where

import           Hedgehog
import qualified TestBase as Gen
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as R
import qualified Game.Runner as G
import qualified Game.Types as G
import qualified Game.CommandParser as C
import qualified Control.Monad.State.Lazy as S
import qualified Data.Set as Set
import Game.Builders
import qualified Data.List.Ordered as LO

tests :: IO Bool
tests = checkParallel $$(discover)

prop_handleCommandItemsOnObjectWithSomeItemsMissing :: Property
prop_handleCommandItemsOnObjectWithSomeItemsMissing = property $ do
  itms <- forAll $ itemsS (R.linear 1 5) Gen.lower
  itemsMiss <- forAll $ itemsS (R.linear 1 5) Gen.upper
  allItems <- forAll $ Set.fromList <$> Gen.shuffle (itms ++ itemsMiss)
  obj <- forAll $ object
  gs <- forAll $ gameStatus (Set.fromList itms) (locations (R.linear 1 1) Nothing Nothing (justMonSet [obj]) Nothing Nothing Nothing Nothing)
  (S.evalState (G.handleCommand (C.ItemsOnObject allItems obj)) gs)
    ===
      ("You don't have these items: " ++ (show $ LO.sort itemsMiss))

prop_handleCommandItemsOnObjectNoAction :: Property
prop_handleCommandItemsOnObjectNoAction = property $ do
  itms <- forAll $ Set.fromList <$> items (R.linear 1 5)
  obj <- forAll $ object
  gs <- forAll $ gameStatus itms (locations (R.linear 1 1) Nothing Nothing (justMonSet [obj]) Nothing Nothing Nothing Nothing)
  (S.evalState (G.handleCommand (C.ItemsOnObject itms obj)) gs)
    ===
      ("I can't do it.")

prop_handleCommandItemsOnObjectActionApplied :: Property
prop_handleCommandItemsOnObjectActionApplied = property $ do
  itms <- forAll $ Set.fromList <$> items (R.linear 1 5)
  obj <- forAll $ object
  act <- forAll $ actionUseItemsOnObject (Just $ Gen.shuffleS itms) (justMon obj) Nothing (justMon [])
  gs <- forAll $ gameStatus itms (locations (R.linear 1 1) Nothing Nothing (justMonSet [obj]) Nothing (justMon [act]) Nothing Nothing)
  (S.evalState (G.handleCommand (C.ItemsOnObject itms obj)) gs)
    ===
      G.aComment act