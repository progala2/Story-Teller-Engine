module MainMenu
    ( mainMenu
    ) 
where
--import System.IO.Error
import System.IO
import GameParser.Parser
import qualified Control.Monad.State.Lazy as S
import Game.Game
--import Data.Either

mainMenu :: IO ()
mainMenu = do 
    putStrLn "Give the game name:"
    gameName <- getLine
    h <- openFile (gameName ++ ".game.ste") ReadMode
    str <- hGetContents h
    print $ parseGameFile str
 --   either (print) (S.evalStateT runGame . (,)(PlayerStatus "" [])) (parseGameFile str)
    hClose h
      ----  where
      --    withFile' fp = withFile fp ReadMode (\h -> Just <$> hGetContents h)
      --    errHandler _ = return Nothing