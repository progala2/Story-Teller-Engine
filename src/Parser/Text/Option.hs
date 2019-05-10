module Parser.Text.Option where

import Parser.Errors
import Data.Map.Strict
import Extensions.Monad

class OptionInt a where
    getIntM :: a -> Maybe Int
    getInt :: a -> Int
    getInt = takeJust . getIntM
    
class OptionStr a where
    getStrM :: a -> Maybe String
    getStr :: a -> String
    getStr = takeJust . getStrM

class OptionArrStr a where
    getArrStrM :: a -> Maybe [String]
    getArrStr :: a -> [String]
    getArrStr = takeJust . getArrStrM


sGet :: (Ord k, Show k, OptionStr o) => Map k o -> k -> Either LoaderError String
sGet mp k = tGet mp k getStr

saGet :: (Ord k, Show k, OptionArrStr o) => Map k o -> k -> Either LoaderError [String]
saGet mp k = tGet mp k getArrStr

iGet :: (Ord k, Show k, OptionInt o) => Map k o -> k -> Either LoaderError Int
iGet mp k = tGet mp k getInt

tGet :: (Ord k, Show k) => Map k o -> k -> (o -> b) -> Either LoaderError b
tGet mp k func = case mp !? k of Just o -> Right $ func o; _ -> errO k

errO ::(Show a) => a -> Either LoaderError b
errO str = Left $ "There is no " ++ (show str) ++ " option."