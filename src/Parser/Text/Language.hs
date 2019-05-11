module Parser.Text.Language where

import Text.ParserCombinators.Parsec

sectionMany :: String -> CharParser () a -> CharParser () [a]
sectionMany name parser = beginToken name *> parser `manyTill` tryEndToken

sectionManyWithKey :: String -> CharParser () a -> CharParser () key -> CharParser () (key, [a])
sectionManyWithKey name parser parserKey = do 
    beginToken name
    key <- parserKey
    set <- parser `manyTill` tryEndToken
    return (key, set)
sectionWithKey :: String -> CharParser () a -> CharParser () key -> CharParser () (key, a)
sectionWithKey name parser parserKey = do 
    beginToken name
    key <- parserKey
    set <- parser
    _ <- endToken
    return (key, set)

section :: String -> CharParser () a -> CharParser () a
section name parser = beginToken name *> parser <* endToken

inlineSection :: String -> CharParser () [String]
inlineSection name = do 
    skipOptionName name
    str <- sepBy (between (char '"') (char '"') (many (noneOf "\""))) (char ' ')
    whiteSpacesAndEol
    return str

beginToken :: String -> CharParser () ()
beginToken name = do
    spaces 
    string' ("<" ++ name)
    whiteSpacesAndEol

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
readOption name = do 
    skipOptionName name
    str <- between (char '"') (char '"') (many (noneOf "\""))
    whiteSpacesAndEol
    return str

readValueName ::CharParser () String
readValueName = do 
    spaces
    str <- between (char '"') (char '"') (many (noneOf "\""))
    whiteSpacesAndEol
    return str

tryOptionInt :: String -> CharParser () Int
tryOptionInt name = try (readOptionInt name)

readOptionInt :: String -> CharParser () Int
readOptionInt name = do 
    skipOptionName name
    str <- readInt
    whiteSpacesAndEol
    return str

readInt :: CharParser () Int
readInt = read <$> (many (alphaNum))

eol :: CharParser () String
eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

eol' :: CharParser () ()
eol' = eol >> return ()

string' :: String -> CharParser () ()
string' str = string str *> return ()

whiteSpaces :: CharParser () ()
whiteSpaces = many (oneOf " \t\f\v")  *> return ()
whiteSpacesAndEol :: CharParser () ()
whiteSpacesAndEol = whiteSpaces *> eol'

tupple :: a -> b -> (a, b)
tupple a b = (a, b)

tryToTV :: (b -> c) -> a -> CharParser () b -> CharParser () (a, c)
tryToTV v t r = (,)t . v <$> try r

choiceToTV :: [(a, CharParser () c)] -> CharParser () (a, c)
choiceToTV [] = error "Can't be empty!"
choiceToTV xs = choice $ (uncurry (tryToTV id)) <$> xs