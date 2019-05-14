module Parser.Text.Language (module Parser.Text.Language, module Extensions.Parsec) where

import Extensions.Parsec

sectionMany :: String -> CharParser () a -> CharParser () [a]
sectionMany name parser = beginToken name *> parser `manyTill` tryEndToken

sectionManyWithKey :: String -> CharParser () a -> CharParser () key -> CharParser () (key, [a])
sectionManyWithKey name prsr prsrKey = (,) <$> (beginToken name *> prsrKey) <*> (prsr `manyTill` tryEndToken)

sectionWithKey :: String -> CharParser () a -> CharParser () key -> CharParser () (key, a)
sectionWithKey name parser parserKey = (,) <$> (beginToken name *> parserKey) <*> (parser <* endToken)

section :: String -> CharParser () a -> CharParser () a
section name parser = beginToken name *> parser <* endToken

inlineSection :: String -> CharParser () [String]
inlineSection name = skipOptionName name *> sepBy (quotaString) (char ' ') <* whiteSpacesAndEol

beginToken :: String -> CharParser () ()
beginToken name = spaces *> string' ("<" ++ name) *> whiteSpacesAndEol

tryEndToken :: CharParser () String
tryEndToken = try endToken
endToken :: CharParser () String
endToken = spaces *> string "/>"

skipOptionName :: String -> CharParser () ()
skipOptionName name = do 
    spaces
    string' name
    whiteSpaces
    _ <- char '='
    whiteSpaces

tryReadOption :: String -> CharParser () String
tryReadOption name = try (readOption name)

readOption :: String -> CharParser () String
readOption name = skipOptionName name *> quotaString <* whiteSpacesAndEol

quotaString :: CharParser () String
quotaString = between (char '"') (char '"') (many (noneOf "\""))

readValueName ::CharParser () String
readValueName = spaces *> quotaString <* whiteSpacesAndEol

tryOptionInt :: String -> CharParser () Int
tryOptionInt name = try (readOptionInt name)

readOptionInt :: String -> CharParser () Int
readOptionInt name = skipOptionName name *> readInt <* whiteSpacesAndEol

readInt :: CharParser () Int
readInt = read <$> many alphaNum

string' :: String -> CharParser () ()
string' str = string str *> return ()

tupple :: a -> b -> (a, b)
tupple a b = (a, b)

tryToTV :: (b -> c) -> a -> CharParser () b -> CharParser () (a, c)
tryToTV v t r = (,)t . v <$> try r

choiceToTV :: [(a, CharParser () c)] -> CharParser () (a, c)
choiceToTV [] = error "Can't be empty!"
choiceToTV xs = choice $ (uncurry (tryToTV id)) <$> xs