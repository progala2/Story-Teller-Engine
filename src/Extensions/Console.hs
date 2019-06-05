module Extensions.Console(clearScreen, module Extensions.Console) where

import           System.Console.ANSI
import           Control.Monad (forM_)
import           Data.Foldable (foldlM)
import           Control.Concurrent (threadDelay)
import           System.IO

-- | Clear a console and write the provided string in red color.
printError :: String -> IO ()
printError = printColored Red
    
-- | Clear a console and write the provided string in blue color.
printMessage :: String -> IO ()
printMessage = printColored Blue

printColored :: Color -> String -> IO ()
printColored c str = do 
  clearScreen 
  setSGR [SetColor Foreground Vivid c]
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

-- | Works as the 'putStrLn' function but text is writen one char per given miliseconds.
putStrLnLazy :: Int -> String -> IO ()
putStrLnLazy t str = hSetBuffering stdout NoBuffering >> foldlM putCharLazy () str >> hSetBuffering stdout LineBuffering >> putStrLn "\n"
  where 
    putCharLazy _ c = putChar c >> threadDelay (t * 1000)