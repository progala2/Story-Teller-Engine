module MainMenu
    ( mainMenu
    ) 
where
--import System.IO.Error
import System.IO
import qualified Control.Monad.State.Lazy as S
import Game.Runner
import Parser.Loader
import Parser.Text.Parser
--import Data.Either

mainMenu :: IO ()
mainMenu = do 
    putStrLn "Give the game name:"
    gameName <- getLine
    h <- openFile (gameName ++ ".game.ste") ReadMode
    str <- hGetContents h
    print $ parseGameFile str
    print $ loadGame str
    either (print) (S.evalStateT runGame) (loadGame str)
    hClose h
      ----  where
      --    withFile' fp = withFile fp ReadMode (\h -> Just <$> hGetContents h)
      --    errHandler _ = return Nothing