-- |
-- Module      : Runner
-- Description : Orchestrate the game; the game loop.
-- 
-- Here we can find the game loop logic. 
module Game.Runner(runGame, gameLoop) where

import           Game.Command
import           Game.RealShow
import           Extensions.Console
import           Game.CommandHandler
import qualified Control.Monad.State.Strict as S
import           Game.GameState
import           Extensions.Monad(ifdrM, ifdM)

-- | Run the game. If true it will show the game's introduction text. When false it will go straight to the 'gameLoop'
runGame :: Bool -> GameStateIO ()
runGame b = (S.lift . introLn <$> getGo) >>= (`ifdM` b) >> gameLoop
  where
    introLn go = do
      clearScreen
      putStrLn $ "Welcome to " ++ goGameName go ++ " ver. " ++ goGameVersion go ++ "\n"
      putStrLnLazy 40 $ goIntro go

-- | Game loop handle interactions between the player and the game.
--
-- Provides logic for handling quitting the current game, by handling the 'ExitCode' from the 'handleCommand''s 'Left' result.
gameLoop :: GameStateIO ()
gameLoop = do 
  gs@(PlayerStatus currLoc _, _) <- S.get
  putStrLnL $ "Location: " ++ (rShow $ fst currLoc)
  showDescription >>= putStrLnL
  putStrLnL "What now? : "
  line <- S.lift $ getLine
  S.lift $ clearScreen
  S.mapStateT (quitGame gs) (handleCommandE line) >>= ifdrM gameLoop
  where
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

-- | Shortcut for putStrLn in GameStateIO.
putStrLnL :: String -> GameStateIO ()
putStrLnL str = S.lift $ putStrLn str