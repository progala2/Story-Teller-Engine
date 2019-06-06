-- |
-- Module: Parser.Game.Action
-- Description: Parsing Action token to Game's Action data.
module Parser.Game.Action where

import Parser.Errors
import Data.Either
import qualified Game.GameState as G
import qualified Parser.Text.Tokens as T
import qualified Parser.Text.Option as T

-- | Converts 'T.Action' to 'G.Action'. Performs simple check whether the given token is correct. If isn't 'LoaderError' is returned.
action :: T.Action -> Either LoaderError G.Action
action act = case T.sGet act T.AotType of
    Right "ItemsOnObject" -> useItemsOnObject
    Right "Unique" -> unique
    Right a -> Left $ "Not known action type: " ++ a
    Left str -> Left str
    where
      useItemsOnObject = G.ActionUseItemsOnObject 
        <$> (G.itemSetFromList <$> T.saGet act T.AotUsedItems) 
        <*> (G.Object <$> T.sGet act T.AotUsedOn)
        <*> T.sGet act T.AotComment
        <*> Right results
      unique = G.ActionUnique
        <$> (T.saGet act T.AotCommands)
        <*> Right (fromRight Nothing (Just . G.Object <$> T.sGet act T.AotUsedOn))
        <*> T.sGet act T.AotComment
        <*> Right results
      results = 
        (G.ArAddLocationItems (T.saGetD act T.AotAddItemsToLocation G.Item))
        : (G.ArRemoveObjects (T.saGetD act T.AotObjectsRemove G.Object))
        :[]