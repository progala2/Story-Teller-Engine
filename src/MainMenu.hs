module MainMenu
    ( mainMenu
    ) 
where
import qualified System.IO.Error as Er
import           System.IO
import qualified Control.Monad.State.Strict as S
import           Game.Runner
import           Game.Types (GameStatus)
import           Parser.Loader
import           Text.Read (readMaybe)
import           System.Console.ANSI

mainMenu :: IO ()
mainMenu = do 
    putStrLn "Welcome to the Story teller engine!"
    putStrLn "Chose number: "
    putStrLn "1. new game"
    putStrLn "2. load game"
    putStrLn "3. exit game"
    menuOption <- getLine
    case menuOption of
        "1" -> clearScreen >> newGame >> mainMenu
        "2" -> clearScreen >> savedGame >> mainMenu
        "3" -> putStrLn "Farewell!"
        _ -> mainMenu
    where
      newGame = templGame "Give the game name:" ".game.ste" newGameParse
        where
          newGameParse hh = do 
            str <- hGetContents hh
            case loadGame str of
              Right s -> do 
                hClose hh
                S.evalStateT (runGame True) s
              Left s -> hClose hh >> printError s
      savedGame = templGame "Give the save name:" ".save.ste" savedGameParse
        where
          savedGameParse hh = do 
            str <- hGetContents hh
            case readMaybe str::(Maybe GameStatus) of
              Just s -> do 
                hClose hh
                S.evalStateT (runGame False) s
              Nothing -> hClose hh >> printError "Save game is broken."
      printError str = do 
        clearScreen 
        setSGR [SetColor Foreground Vivid Red]
        putStrLn $ str
        setSGR [Reset]
      templGame str ext r = do
        putStrLn str
        gameName <- getLine
        h <- Er.tryIOError (openFile (gameName ++ ext) ReadMode)
        case h of
          Right hh -> r hh
          Left e -> printError $ Er.ioeGetErrorString e 