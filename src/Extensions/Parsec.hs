{-# LANGUAGE FlexibleContexts #-}
module Extensions.Parsec (module Text.ParserCombinators.Parsec, module Extensions.Parsec) where

import Text.Parsec hiding (try)
import Text.ParserCombinators.Parsec

manyTill1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyTill1 el end = (:) <$> el <*> manyTill el end

spaceb :: CharParser () Char
spaceb = (oneOf " \t\f\v")

spacebs :: CharParser () String
spacebs = many spaceb

spacebs' :: CharParser () ()
spacebs' = spacebs *> return ()

spacebsAndEol' :: CharParser () ()
spacebsAndEol' = spacebs' *> eol'

eol :: CharParser () String
eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

eol' :: CharParser () ()
eol' = eol >> return ()

btwnSpacebs :: CharParser () a -> CharParser () a
btwnSpacebs f = spacebs *> f <* spacebs

letters1 :: CharParser () String
letters1 = many1 letter

spacebs1 :: CharParser () String
spacebs1 = many1 spaceb 