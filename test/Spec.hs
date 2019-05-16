module Main where
import Test.HUnit
import Game.CommandParserTests

main :: IO ()
main = (runTestTT $ commandParserTestsList) >> return ()