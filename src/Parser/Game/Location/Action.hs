module Parser.Game.Location.Action where

import Parser.Errors
import Extensions.Monad
import qualified Game.Types as G
import qualified Parser.Text.Tokens as T
import qualified Parser.Text.Option as T

action :: T.Action -> Either LoaderError G.Action
action act = case T.sGet act T.AotType of
    Right "ItemsOnObject" -> useItemsOnObject
    Right a -> Left $ "Not known action type: " ++ a
    Left str -> Left str
    where
      useItemsOnObject = G.ActionUseItemsOnObject 
        <$> (G.Item <$$> T.saGet act T.AotUsedItems) 
        <*> (G.Object <$> T.sGet act T.AotUsedOn)
        <*> T.sGet act T.AotComment
        <*> results
        where
          results = case list of
            [] -> Left "You must have at least one result of the action!"
            t -> Right t
            where
            list = 
                (G.ArAddLocationItems (T.saGetD act T.AotAddItemsToLocation G.Item))
                : (G.ArRemoveObjects (T.saGetD act T.AotObjectsRemove G.Object))
                :[]