-- |
-- Module: Parser.Game.Location
-- Description: Parsing Location token to Game's Location data.
module Parser.Game.Location where

import           Parser.Errors
import           Extensions.Errors
import           Extensions.Monad (rightsIfAll, (<.>))
import qualified Data.Either as E
import qualified Data.Set as Set
import qualified Data.Map.Strict as M
import qualified Game.GameState as G
import qualified Parser.Text.Tokens as T
import qualified Parser.Text.Option as T
import           Parser.Game.Action
import           Parser.Game.Travel
import           Parser.Game.Condition

-- | Converts from '(T.LocNameStr, T.Location)' to the '(G.LocName, G.Location)'. Get the 'T.LocMap' to perform consistency checks.
location :: T.LocMap -> (T.LocNameStr, T.Location) -> Either LoaderError (G.LocName, G.Location)
location tLocs (locK, locOpts) = (,) (G.LocName locK) 
  <$> (G.Location descList 
  <$> locTravelList 
  <.> locObjects 
  <.> locItems 
  <*> locActions 
  <.> E.fromRight M.empty (T.tGet locOpts T.LocCond (\ (T.LocCondV a) -> conditionsLoader a)))
  where
    descList = E.fromRight M.empty $ T.tGet locOpts T.LocDescList mapDescMap
      where
      mapDescMap (T.LocDescListV mp) = M.fromList $ mapDescPair <$> M.toList mp
      mapDescMap _ = errCant
      mapDescPair x = case x of 
        (k, (T.LocDesc (T.Local idL) s)) -> (G.DescOrder k, G.LocDescCond G.CondLocal (G.CondId idL) s);
        (k, (T.LocDesc (T.Global idG) s)) -> (G.DescOrder k, G.LocDescCond G.CondGlobal (G.CondId idG) s);
        (k, (T.LocDesc (T.None) s)) -> (G.DescOrder k, G.LocDesc s)
    
    locTravelList = E.fromRight (Right M.empty) $ T.tGet locOpts T.LocTravelList mapTravelMap
      where
      mapTravelMap (T.LocTravelListV mp) = M.fromList <$> rightsIfAll ((travel tLocs locK) <$> M.toList mp)
      mapTravelMap _ = errCant
    
    locObjects = Set.fromList $ T.saGetD locOpts T.LocObjects G.Object
    locItems = Set.fromList $ T.saGetD locOpts T.LocItems G.Item
    locActions = rightsIfAll $ E.fromRight [] $ T.tGet locOpts T.LocActions actions 
      where
      actions (T.LocActionsV act) = map action act 
      actions _ = errCant