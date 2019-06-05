module Game.Runner where

import           Game.Command
import           Game.RealShow
import           Extensions.Console
import           Game.CommandHandler
import qualified Control.Monad.State.Strict as S
import           Game.GameState
import           Extensions.Monad(ifdrM)

runGame :: Bool -> GameStateIO ()
runGame b = do
  (_, (go, _)) <- S.get
  if b then S.lift $ introLn go else return ()
  gameLoop
    where
      introLn go = do
        clearScreen
        putStrLn $ "Welcome to " ++ goGameName go ++ " ver. " ++ goGameVersion go ++ "\n"
        putStrLnLazy 40 $ goIntro go
      gameLoop = do 
        gs@(PlayerStatus currLoc _, _) <- S.get
        putStrLnL $ "Location: " ++ (rShow $ fst currLoc)
        showDescription >>= putStrLnL
        putStrLnL "What now? : "
        line <- S.lift $ getLine
        S.lift $ clearScreen
        S.mapStateT (quitGame gs) (handleCommandE line) >>= ifdrM gameLoop
      handleCommandE line = either (\_ -> return "You wrote something wrong!") handleCommand (parseCommand line)
      quitGame _ (Right (a, s)) = printMessage a >> putStrLn "" >> return (False, s)
      quitGame gs@(_, (go, _)) (Left c) = case c of
        QuitGame -> quitGameWo
        QuitAndSave -> quitGameSave
        PlayerWin -> (putStrLnLazy 40 $ goOutro go) >> return (True, gs)
        where
          quitGameWo = do
            putStrLn "Are you sure you want to quit this game without saving?(y/n)"
            answer <- getLine
            case answer of
              "y" -> return (True, gs)
              "n" -> return (False, gs)
              _ -> quitGameWo
          quitGameSave = do 
            putStrLn "Give a name of the save:"
            answer <- getLine
            writeFile (answer ++ ".save.ste") (show gs)
            return (True, gs)

putStrLnL :: String -> GameStateIO ()
putStrLnL str = S.lift $ putStrLn str