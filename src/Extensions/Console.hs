module Extensions.Console(clearScreen, module Extensions.Console) where

import           System.Console.ANSI
import           Control.Monad (forM_)

-- | Clear a console and write the provided string in red color.
printError :: String -> IO ()
printError str = do 
    clearScreen 
    setSGR [SetColor Foreground Vivid Red]
    putStrLn $ str
    setSGR [Reset]

-- | Print a message to the console and wait for input from an user.
-- It is the same as
-- 
-- > putStrLn str >> getLine 
askForLine :: String -> IO String
askForLine str = putStrLn str >> getLine 

-- | Print a multiline message to the console and wait for input from an user. 
askMlForLine :: [String] -> IO String
askMlForLine str = forM_ str putStrLn >> getLine 