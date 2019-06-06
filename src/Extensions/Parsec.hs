-- | 
-- Module: Parsec
-- Description: Additional parsec functions.
module Extensions.Parsec (module Text.ParserCombinators.Parsec, module Extensions.Parsec) where

import Text.Parsec hiding (try)
import Text.ParserCombinators.Parsec

-- | The same as 'manyTill' but at least one @a@ is parsed.
manyTill1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyTill1 el end = (:) <$> el <*> manyTill el end

-- | Very similar to 'space' but without new lines charactes.
spaceb :: CharParser () Char
spaceb = (oneOf " \t\f\v")

-- | Very similar to 'spaces' but without new lines charactes.
spacebs :: CharParser () String
spacebs = many spaceb

-- | The same as 'spacebs' but discards any results.
spacebs' :: CharParser () ()
spacebs' = spacebs *> return ()

-- | Read the end of line. 
eol :: CharParser () String
eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

-- | As 'eol' but discards any results.
eol' :: CharParser () ()
eol' = eol >> return ()

-- | Parse all spacesbs until the end of line that it also read.
spacebsAndEol' :: CharParser () ()
spacebsAndEol' = spacebs' *> eol'

-- | Parse between spacebs. Spacebs are discarded and only the result of the given parser is returned.
btwnSpacebs :: CharParser () a -> CharParser () a
btwnSpacebs f = spacebs *> f <* spacebs

-- | 
-- >>> 'many1' 'letter'
letters1 :: CharParser () String
letters1 = many1 letter

-- | 
-- >>> 'many1' 'spaceb'
spacebs1 :: CharParser () String
spacebs1 = many1 spaceb 

-- | Parsing with trying to use one of the given strings. Very similar to 'choice'.
choiceString :: [String] -> CharParser () String
choiceString [] = error "Can't be empty!"
choiceString elems = choice $ (try . string) <$> elems

-- | Parse a string with spacebs between. End parsing when there is no more letters after spacebs or the charactes is different than letter.
stringSpacebs :: CharParser () String
stringSpacebs = letters1 <> (concat <$> many (try (spacebs1 <> letters1) ))

-- | Parse @'@ and discards spacebs around it.
sQuotaBtwnSpacebs :: CharParser () String
sQuotaBtwnSpacebs = btwnSpacebs $ string "'"