module Game.Runner where

import           Game.Command
import           System.Console.ANSI
import           Game.CommandParser
import           System.IO
import qualified Control.Monad.State.Strict as S
import           Game.GameState
import           Data.Foldable (foldlM)
import           Control.Concurrent (threadDelay)
import           Data.Functor.Identity (Identity(..))

runGame :: Bool -> GameStateIO ()
runGame b = do
  (_, (go, _)) <- S.get
  if b then S.lift $ introLn go else return ()
  gameLoop
    where
      introLn go = do
        clearScreen
        putStrLn $ "Welcome to " ++ goGameName go ++ " ver. " ++ goGameVersion go ++ "\n"
        putStrLnLazy $ goIntro go
      gameLoop = do 
        gs@(PlayerStatus currLoc _, _) <- S.get
        putStrLnL $ "Location: " ++ (show $ fst currLoc)
        showDescription >>= putStrLnL
        putStrLnL "What now? : "
        line <- S.lift $ getLine
        S.lift $ clearScreen
        b <- S.mapStateT (quitGame gs) (handleCommandE line)
        if b then return () else gameLoop
      handleCommandE line = either (\_ -> return "You wrote something wrong!") handleCommand (parseCommand line)
      quitGame _ (Right (a, s)) = putStrLn a >> return (False, s)
      quitGame gs@(_, (go, _)) (Left c) = case c of
        QuitGame -> quitGameWo
        QuitAndSave -> quitGameSave
        PlayerWin -> (putStrLnLazy $ goOutro go) >> return (True, gs)
        where
          quitGameWo = do
            putStrLn "Are you sure you want to quit this game without saving?(y/n)"
            answer <- getLine
            case answer of
              "y" -> return (True, gs)
              "n" -> return (False, gs)
              _ -> quitGameWo
          quitGameSave = do 
            putStrLn "Give save name:"
            answer <- getLine
            writeFile (answer ++ ".save.ste") (show gs)
            return (True, gs)

putStrLnL :: String -> GameStateIO ()
putStrLnL str = S.lift $ putStrLn str

putStrLnLazy :: String -> IO ()
putStrLnLazy str = hSetBuffering stdout NoBuffering >> foldlM putCharLazy () str >> hSetBuffering stdout LineBuffering >> putStrLn "\n"
  where 
    putCharLazy _ c = putChar c >> threadDelay 40000

liftT :: Monad m => S.StateT s Identity a -> S.StateT s m a
liftT = S.mapStateT (\(Identity (a, s)) -> return (a, s))