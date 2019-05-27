{-# LANGUAGE FlexibleContexts #-}
module Extensions.Parsec (module Text.ParserCombinators.Parsec, module Extensions.Parsec) where

import Text.Parsec hiding (try)
import Text.ParserCombinators.Parsec

manyTill1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyTill1 el end = (:) <$> el <*> manyTill el end

whiteSpaces :: Stream s m Char => ParsecT s u m ()
whiteSpaces = many (oneOf " \t\f\v")  *> return ()

whiteSpacesAndEol :: CharParser () ()
whiteSpacesAndEol = whiteSpaces *> eol'

eol :: CharParser () String
eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

eol' :: CharParser () ()
eol' = eol >> return ()

btwnSpaces :: CharParser () a -> CharParser () a
btwnSpaces f = spaces *> f <* spaces