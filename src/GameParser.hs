module GameParser
    ( parseGameFile
    ) where

import Text.ParserCombinators.Parsec

parseGameFile :: String -> Either ParseError String
parseGameFile str = parse gameFile "(unknown)" str

gameFile :: CharParser () String
gameFile = do 
    gameBeginToken
    str <- manyTill anyChar (try gameEndToken)
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