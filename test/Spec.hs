{-# LANGUAGE OverloadedStrings, TemplateHaskell  #-}
module Main where
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Game.CommandParserTests

main :: IO ()
main = tests >> return ()