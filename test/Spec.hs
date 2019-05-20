{-# LANGUAGE TemplateHaskell  #-}
module Main where
import qualified Game.CommandParserTests as GCP
import qualified Game.RunnerTests as GR

main :: IO ()
main = GCP.tests >> GR.tests >> return ()