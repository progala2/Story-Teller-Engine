module Parser.Game.Location.Action where

import Parser.Errors
import Extensions.Monad
import qualified Game.Types as G
import qualified Parser.Text.Tokens as T
import qualified Parser.Text.Option as T

action :: T.Action -> Either LoaderError G.Action
action act = G.ActionUseItemsOnObject 
    <$> (G.Item <$$> T.saGet act T.AotUsedItems) 
    <*> (G.Object <$> T.sGet act T.AotUsedOn)
    <*> T.sGet act T.AotComment
    <*> Right []