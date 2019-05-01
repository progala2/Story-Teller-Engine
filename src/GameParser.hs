module GameParser
    ( parseGameFile
    ) where

import Text.ParserCombinators.Parsec
import GameEngine

parseGameFile :: String -> Either ParseError GameOptions
parseGameFile str = parse gameFile "(unknown)" str

gameFile :: CharParser () GameOptions
gameFile = do 
    gameBeginToken
    go <- headersSection
    _ <- manyTill anyChar (try gameEndToken)
    return go

headersSection :: CharParser () GameOptions
headersSection = do
    whiteSpacesAndNl
    _ <- string "Headers Begin"
    go <- (:[]) <$> GameName <$> tryOptions "Game Name"
    _ <- manyTill anyChar (try $ string "End")
    return go
      where 
        tryOptions name = 
            do 
                whiteSpacesAndNl
                _ <- string name
                whiteSpaces
                char '='
                whiteSpaces
                str <- between (char '"') (char '"') (many (noneOf "\""))
                whiteSpaces
                eol'
                return str

gameBeginToken :: CharParser () ()
gameBeginToken = do
    whiteSpacesAndNl 
    _ <- string "Game Begin"
    whiteSpaces
    eol'
    return ()

gameEndToken :: CharParser () ()
gameEndToken = string "Game End" >> return ()

eol :: CharParser () String
eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

eol' :: CharParser () ()
eol' = eol >> return ()

whiteSpacesAndNl :: CharParser () ()
whiteSpacesAndNl = many (oneOf " \t\r\n") >> return ()

whiteSpaces :: CharParser () ()
whiteSpaces = many (oneOf " \t")  >> return ()