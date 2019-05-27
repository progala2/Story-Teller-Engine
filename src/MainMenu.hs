module MainMenu
    ( mainMenu
    ) 
where
import qualified System.IO.Error as Er
import           System.IO
import qualified Control.Monad.State.Strict as S
import           Game.Runner
import           Parser.Loader
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
      newGame = do
        putStrLn "Give the game name:"
        gameName <- getLine
        h <- Er.tryIOError (openFile (gameName ++ ".game.ste") ReadMode)
        case h of
          Right hh -> newGameParse hh
          Left e -> printError $ Er.ioeGetErrorString e
        where
          newGameParse hh = do 
            str <- hGetContents hh
            case loadGame str of
              Right s -> do 
                hClose hh
                S.evalStateT runGame s
              Left s -> hClose hh >> printError s
      printError str = do 
        clearScreen 
        setSGR [SetColor Foreground Vivid Red]
        putStrLn $ str
        setSGR [Reset]
      savedGame = error "Not Implemented yet!"