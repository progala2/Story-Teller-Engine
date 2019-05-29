{-# LANGUAGE ViewPatterns #-}
module Game.Builders where

import           Hedgehog
import qualified TestBase as Gen
import qualified Game.GameState as G
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as R
import qualified Data.Set as Set
import qualified Data.Map.Strict as M
import Extensions.Monad
import Data.Maybe (fromMaybe)

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

type MaybeGenT m a = Maybe (GenT m a)
locations :: Monad m =>
            R.Range Int ->
            MaybeGenT m G.DescMap -> 
            MaybeGenT m G.TravelMap -> 
            MaybeGenT m G.ObjectSet -> 
            MaybeGenT m G.ItemSet ->
            MaybeGenT m G.Actions ->
            MaybeGenT m G.Conditions ->
            MaybeGenT m G.LocName ->
            GenT m G.LocMap
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

actionUseItemsOnObject :: Monad m =>
            MaybeGenT m G.ItemSet -> 
            MaybeGenT m G.Object -> 
            MaybeGenT m String -> 
            MaybeGenT m [G.ActionResult] ->
            GenT m G.Action
actionUseItemsOnObject
    (def (Set.fromList <$> items (R.linear 3 3)) -> itms) 
    (def object -> obj) 
    (def Gen.alphaStringNE -> comm) 
    (defMM -> ress) =
    G.ActionUseItemsOnObject 
        <$> itms 
        <*> obj
        <*> comm
        <*> ress

gameStatus :: MonadGen m => Set.Set G.Item -> m G.LocMap -> m G.GameStatus
gameStatus itms loc = loc >>= (\l ->
     (,) (G.PlayerStatus (snd (M.elemAt 0 l)) itms) 
      <$> ((,) <$> gameOptions <.> l) )


defM :: Monad m => a -> Maybe (m a) -> m a
defM a m = fromMaybe (return a) m

def :: a -> Maybe a -> a
def a m = fromMaybe a m

defMM :: (Monoid a, Monad m, Monoid (m a)) => Maybe (m a) -> m a
defMM = fromMaybe mempty

justMonSet :: (Ord a, Monad m) => [a] -> Maybe (m (Set.Set a))
justMonSet = justMon . Set.fromList

justMon :: (Monad m) => a -> Maybe (m a)
justMon = Just . return