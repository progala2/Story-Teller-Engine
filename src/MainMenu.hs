-- |
-- Module      : MainMenu
-- Description : Handling Game Main Menu
-- 
-- It does what it says - interactive menu with an user.
-- You can start new game or load a saved one.
-- Saved game has extension @.save.ste@. 
-- 
-- New game has extension @.game.ste@
module MainMenu
    ( mainMenu
    ) 
where
import qualified System.IO.Error as Er
import           System.IO
import qualified Control.Monad.State.Strict as S
import           Game.Runner
import           Game.GameState (GameStatus)
import           Parser.Loader
import           Text.Read (readMaybe)
import           Extensions.Console


mainMenu :: IO ()
mainMenu = do 
    menuOption <- askMlForLine ["Welcome to the Story teller engine!",
          "Chose number:",
          "1. New game",
          "2. Load game",
          "3. Exit game"]
    case menuOption of
        "1" -> clearScreen >> newGame >> mainMenu
        "2" -> clearScreen >> savedGame >> mainMenu
        "3" -> putStrLn "Farewell!"
        _ -> mainMenu
    where
      newGame = templGame "Give the game name:" ".game.ste" newGameParse
        where
          newGameParse hh str = do
            res <- return $ loadGame str
            hClose hh
            case res of
              Right s -> S.evalStateT (runGame True) s
              Left s -> printError s
      savedGame = templGame "Give the save name:" ".save.ste" savedGameParse
        where
          savedGameParse hh str = do
            res <- return (readMaybe str::(Maybe GameStatus))
            hClose hh 
            case res of
              Just s -> S.evalStateT (runGame False) s
              Nothing -> printError "Saved game is broken."
      -- | Handle file opening and invoking provided function on the content.
      templGame str ext r = do
        gameName <- askForLine str
        h <- Er.tryIOError (openFile (gameName ++ ext) ReadMode)
        case h of
          Right hh -> hGetContents hh >>= r hh
          Left e -> printError $ Er.ioeGetErrorString e