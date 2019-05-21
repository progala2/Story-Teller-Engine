{-# LANGUAGE TemplateHaskell, OverloadedStrings, ViewPatterns #-}
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
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid, mempty)
import Control.Monad (Monad)
import qualified Data.List.Ordered as LO

tests :: IO Bool
tests = checkParallel $$(discover)

prop_handleCommandItemsOnObjectWithSomeItemsMissing :: Property
prop_handleCommandItemsOnObjectWithSomeItemsMissing = property $ do
  itms <- forAll $ itemsS (R.linear 1 5) Gen.lower
  itemsMiss <- forAll $ itemsS (R.linear 1 5) Gen.upper
  allItems <- forAll $ Set.fromList <$> Gen.shuffle (itms ++ itemsMiss)
  obj <- forAll $ object
  gs <- forAll $ gameStatus (Set.fromList itms) (locations (R.linear 1 1) Nothing Nothing (Just $ return $ Set.fromList [obj]) Nothing Nothing Nothing Nothing)
  (S.evalState (G.handleCommand (C.ItemsOnObject allItems obj)) gs)
    ===
      ("You don't have these items: " ++ (show $ LO.sort itemsMiss))

prop_handleCommandItemsOnObjectNoAction :: Property
prop_handleCommandItemsOnObjectNoAction = property $ do
  itms <- forAll $ Set.fromList <$> items (R.linear 1 5)
  obj <- forAll $ object
  gs <- forAll $ gameStatus itms (locations (R.linear 1 1) Nothing Nothing (Just $ return $ Set.fromList [obj]) Nothing Nothing Nothing Nothing)
  (S.evalState (G.handleCommand (C.ItemsOnObject itms obj)) gs)
    ===
      ("I can't do it.")

prop_handleCommandItemsOnObjectActionApplied :: Property
prop_handleCommandItemsOnObjectActionApplied = property $ do
  itms <- forAll $ Set.fromList <$> items (R.linear 1 5)
  obj <- forAll $ object
  gs <- forAll $ gameStatus itms (locations (R.linear 1 1) Nothing Nothing (Just $ return $ Set.fromList [obj]) Nothing Nothing Nothing Nothing)
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

locations :: Monad m =>
            R.Range Int ->
            Maybe (GenT m G.DescMap) -> 
            Maybe (GenT m G.TravelMap) -> 
            Maybe (GenT m G.ObjectSet) -> 
            Maybe (GenT m G.ItemSet) ->
            Maybe (GenT m G.Actions) ->
            Maybe (GenT m G.Conditions) ->
            Maybe (GenT m G.LocName) ->
            GenT m G.Locations
locations r 
          (defMM -> descList) 
          (defMM -> travelList) 
          (defMM -> objects) 
          (defMM -> itms)
          (defMM -> actions)
          (defMM -> conditions) 
          (def (G.LocName <$> Gen.alphaStringNE) -> locName) =
            M.fromList <$> Gen.list r ((,) <$> locName <*> (G.Location 
            <$> descList 
            <*> travelList
            <*> objects
            <*> itms
            <*> actions
            <*> conditions))

gameStatus :: MonadGen m => Set.Set G.Item -> m G.Locations -> m G.GameStatus
gameStatus itms loc = loc >>= (\l ->
     (,) (G.PlayerStatus (fst (M.elemAt 0 l)) itms) 
      <$> ((,) <$> gameOptions <.> l) )

defM :: Monad m => a -> Maybe (m a) -> m a
defM a m = fromMaybe (return a) m

def :: a -> Maybe a -> a
def a m = fromMaybe a m

defMM :: (Monoid a, Monad m, Monoid (m a)) => Maybe (m a) -> m a
defMM = fromMaybe mempty